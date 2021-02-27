
import { BoltModifiers, BoltPattern, BoltVariableDeclaration, isBoltAssignStatement, isBoltBlockExpression, isBoltExpression, isBoltFunctionDeclaration, isBoltFunctionExpression, isBoltSourceFile, isBoltVariableDeclaration, kindToString, SourceFile, Syntax, SyntaxKind } from "./ast";
import { getAllReturnStatementsInFunctionBody, getSymbolText } from "./common";
import { CompileError, Diagnostic, E_DECLARATION_NOT_FOUND, E_PARAM_COUNT_MISMATCH, E_THIS_NODE_CAUSED_INVALID_TYPE, E_TYPE_DECLARATION_NOT_FOUND, E_TYPE_UNIFICATION_FAILURE, E_UNBOUND_FREE_VARIABLE, E_UNINITIALIZED_BINDING } from "./diagnostics";
import { assert, FastStringMap, prettyPrintTag } from "./util";

enum TypeKind {
  TypeVar,
  PrimType,
  ArrowType,
  TupleType,
}

type Type
  = TypeVar
  | PrimType
  | ArrowType
  | TupleType

interface TypeBase {
  kind: TypeKind;
}

function createGenerator(defaultPrefix: string) {
  const counts = Object.create(null);
  return function (prefix = defaultPrefix) {
    const count = counts[prefix];
    if (count !== undefined) {
      counts[prefix]++;
      return prefix + count;
    }
    counts[prefix] = 1;
    return prefix + '0';
  }
}

const generateTypeVarId = createGenerator('a');

class TypeVar implements TypeBase {

  public readonly kind: TypeKind.TypeVar = TypeKind.TypeVar;

  public varId: string;

  constructor(hint?: string) {
    this.varId = generateTypeVarId(hint);
  }

  public hasTypeVariable(typeVar: TypeVar): boolean {
    return typeVar.varId === this.varId;
  }

  public applySubstitution(substitution: TypeVarSubstitution): Type {
    if (substitution.has(this)) {
      return substitution.get(this);
    } else {
      return this;
    }
  }

  public [prettyPrintTag]() {
    return this.varId;
  }

}

class TupleType implements TypeBase {

  public readonly kind = TypeKind.TupleType;

  constructor(public elements: Type[]) {

  }

  public hasTypeVariable(typeVar: TypeVar): boolean {
    return this.elements.some(type => type.hasTypeVariable(typeVar))
  }

  public applySubstitution(substitution: TypeVarSubstitution): Type {
    return new TupleType(
      this.elements.map(type => type.applySubstitution(substitution))
    )
  }

  public [prettyPrintTag](): string {
    return '(' + this.elements
      .map(type => type[prettyPrintTag]())
      .join(', ') + ')';
  }

}

class PrimType implements TypeBase {

  public readonly kind = TypeKind.PrimType;

  constructor(
    public primId: number,
    public displayName: string,
  ) {

  }

  public hasTypeVariable(typeVar: TypeVar): boolean {
    return false;
  }

  public applySubstitution(substitution: TypeVarSubstitution) {
    return this;
  }

  public [prettyPrintTag]() {
    return this.displayName;
  }

}

class ArrowType implements TypeBase {

  public readonly kind: TypeKind.ArrowType = TypeKind.ArrowType;

  constructor(
    public paramTypes: Type[],
    public returnType: Type
  ) {

  }

  public hasTypeVariable(typeVar: TypeVar): boolean {
    return this.paramTypes.some(paramType => paramType.hasTypeVariable(typeVar))
        || this.returnType.hasTypeVariable(typeVar);
  }

  public applySubstitution(substitution: TypeVarSubstitution): Type {
    return new ArrowType(
      this.paramTypes.map(type => type.applySubstitution(substitution)),
      this.returnType.applySubstitution(substitution)
    )
  }

  public [prettyPrintTag](): string {
    return `(${this.paramTypes.map(type => type[prettyPrintTag]()).join(', ')}) -> ${this.returnType[prettyPrintTag]()}`
  }

}

function getFreeVariablesOfType(type: Type): TypeVarSet {

  const freeVariables = new TypeVarSet();

  const visit = (type: Type) => {

    switch (type.kind) {

      case TypeKind.TypeVar:
        freeVariables.add(type);
        break;

      case TypeKind.PrimType:
        break;

      case TypeKind.ArrowType:
        for (const paramType of type.paramTypes) {
          visit(paramType);
        }
        visit(type.returnType);
        break;

      default:
        throw new Error(`Could not get the free type variables: unknown type kind`);

    }

  }

  visit(type);

  return freeVariables;

}

class TypeVarSubstitution {

  private mapping = new FastStringMap<string, [TypeVar, Type]>();

  public add(source: TypeVar, target: Type) {
    if (this.mapping.has(source.varId)) {
      throw new Error(`Could not add type variable to substitution: variable ${source.varId} already exists.`)
    }
    this.mapping.set(source.varId, [source, target]);
  }

  public has(source: TypeVar): boolean {
    return this.mapping.has(source.varId);
  }

  public delete(source: TypeVar): void {
    this.mapping.delete(source.varId);
  }

  public composeWith(other: TypeVarSubstitution): TypeVarSubstitution {
    const newSubstitution = new TypeVarSubstitution();
    for (const [typeVar, type] of other) {
      newSubstitution.add(typeVar, type.applySubstitution(this));
    }
    for (const [typeVar, type] of this) {
      newSubstitution.add(typeVar, type);
    }
    return newSubstitution;
  }

  public applyComposition(other: TypeVarSubstitution): void {
    for (const [typeVar, type] of other) {
      // if (this.has(typeVar)) {
      //   this.delete(typeVar);
      // }
      this.add(typeVar, type.applySubstitution(this));
    }
  }

  public defaults(other: TypeVarSubstitution): void{ 
    for (const [name, entry] of other.mapping) {
      if (!this.mapping.has(name)) {
        this.mapping.set(name, entry);
      }
    }
  }

  public get(source: TypeVar): Type {
    return this.mapping.get(source.varId)[1];
  }

  public *[Symbol.iterator](): Iterator<[TypeVar, Type]> {
    for (const [source, target] of this.mapping.values()) {
      yield [source, target];
    }
  }

}

const emptyTypeVarSubstitution = new TypeVarSubstitution();

class TypeVarSet {

  private typeVarIds = new FastStringMap<string, TypeVar>();

  constructor(iterable: Iterable<TypeVar> = []) {
    for (const typeVar of iterable) {
      this.add(typeVar);
    }
  }

  public add(typeVar: TypeVar): void {
    this.typeVarIds.set(typeVar.varId, typeVar);
  }

  public has(typeVar: TypeVar): boolean {
    return this.typeVarIds.has(typeVar.varId);
  }

  public delete(typeVar: TypeVar): void {
    this.typeVarIds.delete(typeVar.varId);
  }

  public [Symbol.iterator]() {
    return this.typeVarIds.values();
  }

}

class ForallScheme {

  constructor(
    public typeVars: TypeVarSet,
    public type: Type,
  ) {

  }

  public getFreeVariables(): TypeVarSet {
    const freeVariables = getFreeVariablesOfType(this.type);
    for (const typeVar of this.typeVars) {
      if (freeVariables.has(typeVar)) {
        freeVariables.delete(typeVar);
      }
    }
    return freeVariables;
  }

  public applySubstitution(substitution: TypeVarSubstitution): Scheme {
    const newSubstitution = new TypeVarSubstitution();
    for (const [typeVar, mappedType] of substitution) {
      if (!this.typeVars.has(typeVar)) {
        newSubstitution.add(typeVar, mappedType);
      }
    }
    return new ForallScheme(
      this.typeVars,
      this.type.applySubstitution(newSubstitution)
    );
  }

}

type Scheme
  = ForallScheme

type Constraint = [Type, Type]

export class TypeEnv {

  private mapping = new FastStringMap<string, Scheme>();

  constructor(public parentTypeEnv: TypeEnv | null = null) {

  }

  public set(name: string, scheme: Scheme) {
    this.mapping.set(name, scheme)
  }

  public remove(name: string): void {
    this.mapping.delete(name)
  }

  public lookup(name: string): Type | null {

    let scheme;
    let currEnv: TypeEnv | null = this;

    while (true) { 

      // Check if the binding exists. It's the caller's responsibility to so
      // something sensible if the binding was not found.
      if (currEnv!.mapping.has(name)) {
        scheme = currEnv.mapping.get(name);
        break;
      }

      currEnv = currEnv.parentTypeEnv;

      // If we went through every parent environment and still did not found a
      // valid type, then we return nothing.
      if (currEnv === null) {
        return null;
      }

    }

    // Now we instantiate the type inside the scheme with some fresh variables
    // that are guaranteed to not occur anywhere else.
    const freshVars = new TypeVarSubstitution();
    for (const typeVar of scheme.typeVars) {
      freshVars.add(typeVar, new TypeVar())
    }
    return scheme.type.applySubstitution(freshVars);
  }

  public has(typeVar: TypeVar): boolean {
    let currEnv: TypeEnv | null = this;
    while (true) { 
      if (currEnv!.mapping.has(typeVar.varId)) {
        return true;
      }
      currEnv = currEnv.parentTypeEnv;
      if (currEnv === null) {
        return false;
      }
    }
  }

  public clone(): TypeEnv {
    const result = new TypeEnv(this.parentTypeEnv);
    for (const [name, scheme] of this.mapping) {
      result.set(name, scheme);
    }
    return result;
  }

  public getFreeVariables(): TypeVarSet {
    const freeVariables = new TypeVarSet();
    let currEnv: TypeEnv | null = this;
    do {
      for (const scheme of currEnv.mapping.values()) {
        for (const typeVar of scheme.getFreeVariables()) {
          freeVariables.add(typeVar);
        }
      }
      currEnv = currEnv!.parentTypeEnv;
    } while (currEnv !== null);
    return freeVariables;
  }

}

function generalizeType(type: Type, typeEnv: TypeEnv): Scheme {
  const freeVariables = getFreeVariablesOfType(type);
  for (const typeVar of typeEnv.getFreeVariables()) {
    if (freeVariables.has(typeVar)) {
      freeVariables.delete(typeVar)
    }
  }
  return new ForallScheme(freeVariables, type);
}

function bindTypeVar(typeVar: TypeVar, type: Type): TypeVarSubstitution {

  // Binding a type variable to itself means that we don't have to substitute
  // anything.
  if (type.kind === TypeKind.TypeVar && type.varId === typeVar.varId) {
    return emptyTypeVarSubstitution;
  }

  // This 'occurs check' ensures that a type variable is actually used. If it
  // isn't used, bindTypeVar would do nothing and we are at risk of having an
  // infinite loop in the unifier.
  if (type.hasTypeVariable(typeVar)) {
    throw new UnboundFreeVariableError(type, typeVar);
  }

  const substitution = new TypeVarSubstitution();
  substitution.add(typeVar, type);
  return substitution
}

function getNodeIntroducingScope(node: Syntax) {
  let currNode: Syntax | null = node;
  while (true) {
    if (isBoltSourceFile(currNode)
    || isBoltBlockExpression(currNode)
    || isBoltFunctionDeclaration(currNode)
    || (isBoltFunctionExpression(currNode) && currNode.body !== null)) {
      return currNode;
    }
    if (currNode!.parentNode === null) {
      return currNode;
    }
    currNode = currNode!.parentNode;
  }
}

export class TypeCheckError extends CompileError {

}

export class UnificationError extends TypeCheckError {

  constructor(public left: Type, public right: Type) {
    super({
      message: E_TYPE_UNIFICATION_FAILURE,
      args: {
        left,
        right,
      },
      severity: 'error',
    });
  }
}

export class TypeNotFoundError extends TypeCheckError {

  constructor(public node: Syntax, public typeName: string) {
    super({
      node: node,
      message: E_TYPE_DECLARATION_NOT_FOUND,
      args: {
        name: typeName,
      },
      severity: 'error'
    });
  }

}

export class ParamCountMismatchError extends TypeCheckError {

  constructor(public left: ArrowType, public right: ArrowType) {
    super({
      message: E_PARAM_COUNT_MISMATCH,
      args: {
        left,
        right,
        leftCount: left.paramTypes.length,
        rightCount: right.paramTypes.length,
      },
      severity: 'error'
    });
  }

}

export class UnboundFreeVariableError extends TypeCheckError {

  constructor(public type: Type, public typeVar: TypeVar) {
    super({
      message: E_UNBOUND_FREE_VARIABLE,
      args: {
        type,
        typeVar
      },
      severity: 'error',
    })
  }

}

export class BindingNotFoundError extends TypeCheckError {
  constructor(public node: Syntax, public name: string) {
    super({
      node,
      message: E_DECLARATION_NOT_FOUND,
      args: {
        name
      },
      severity: 'error'
    });
  }
}

export class UninitializedBindingError extends TypeCheckError {
  constructor(public node: BoltVariableDeclaration) {
    super({
      node,
      message: E_UNINITIALIZED_BINDING,
      severity: 'error'
    });
  }
}

export class TypeChecker {

  private nextPrimTypeId = 1;

  private intType = this.createPrimType('int');
  private stringType = this.createPrimType('String');
  private boolType = this.createPrimType('bool');
  private voidType = new TupleType([]);

  private nodeToType = new FastStringMap<number, Type>();
  private nodeToTypeEnv = new FastStringMap<number, TypeEnv>();
  private sourceFileToSubstitution = new FastStringMap<number, TypeVarSubstitution>();

  public isIntType(type: Type) {
    return type === this.intType;
  }

  public isStringType(type: Type) {
    return type === this.stringType;
  }

  public isBoolType(type: Type) {
    return type === this.boolType;
  }

  private builtinTypes = new FastStringMap<string, Type>([
    ['int', this.intType],
    ['String', this.stringType],
    ['bool', this.boolType]
  ]);

  private createPrimType(name: string): PrimType {
    return new PrimType(this.nextPrimTypeId++, name);
  }

  public registerSourceFile(sourceFile: SourceFile): void {
    const newTypeEnv = new TypeEnv();
    this.nodeToTypeEnv.set(sourceFile.id, newTypeEnv)
    const constraints: Constraint[] = [];
    this.forwardDeclare(sourceFile, newTypeEnv);
    this.checkNode(sourceFile, newTypeEnv, constraints);
    this.sourceFileToSubstitution.set(sourceFile.id, this.solveConstraints(constraints));
  }

  private inferBinding(
    node: BoltPattern,
    valueType: Type,
    typeEnv: TypeEnv,
    constraints: Constraint[],
    shouldGeneralize: boolean
  ) {
    switch (node.kind) {
      case SyntaxKind.BoltExpressionPattern:
        {
          constraints.push([
            valueType,
            this.inferNode(node.expression, typeEnv, constraints)
          ])
          break;
        }
      case SyntaxKind.BoltBindPattern:
        {
          const varName = node.name.text;
          typeEnv.set(varName, shouldGeneralize ? generalizeType(valueType, typeEnv) : new ForallScheme(new TypeVarSet(), valueType));
          break;
        }
      default:
        throw new Error(`Could not infer constraints from pattern in binding: unknown node type`);
    }
  }

  private inferAssignment(node: BoltPattern, valueType: Type, typeEnv: TypeEnv, constraints: Constraint[]) {
    switch (node.kind) {
      case SyntaxKind.BoltBindPattern:
      {
        const varName = node.name.text;
        const varType = typeEnv.lookup(varName);
        if (varType === null) {
          throw new BindingNotFoundError(node, varName);
        }
        constraints.push([
          varType,
          valueType,
        ])
        break;
      }
      default:
        throw new Error(`Could not infer constraints from pattern in assignment: unknown node type`);
    }
  }

  private forwardDeclare(node: Syntax, typeEnv: TypeEnv) {

    switch (node.kind) {

      case SyntaxKind.BoltSourceFile:
        {
          for (const element of node.elements) {
            this.forwardDeclare(element, typeEnv);
          }
          break;
        }

      case SyntaxKind.BoltReferenceExpression:
      case SyntaxKind.BoltConstantExpression:
        break;

      case SyntaxKind.BoltCallExpression:
        {
          this.forwardDeclare(node.operator, typeEnv);
          for (const operand of node.operands) {
            this.forwardDeclare(operand, typeEnv);
          }
          break;
        }

      case SyntaxKind.BoltFunctionExpression:
        {
          // TODO
          break;
        }

      case SyntaxKind.BoltExpressionStatement:
        {
          this.forwardDeclare(node.expression, typeEnv);
          break;
        }

      case SyntaxKind.BoltVariableDeclaration:
        break;

      case SyntaxKind.BoltAssignStatement:
        {
          this.forwardDeclare(node.rhs, typeEnv);
          break;
        }

      case SyntaxKind.BoltFunctionDeclaration:
        {

          const name = getSymbolText(node.name);

          // Right now, we don't know much about the function and just want to
          // make sure that whatever we learn about it is linked into this type variable.
          const fnType = new TypeVar();

          this.nodeToType.set(node.id, fnType);

          // We get/create a type environment that is NOT cloned, because that
          // allows us complete access to all locally declared identifiers inside
          // the function.
          const innerTypeEnv = this.getTypeEnvForNode(node, typeEnv, false);

          // Register this function in the type environment it was presumably
          // defined in. This will also define it in the function's body since
          // the two environments should be linked.
          typeEnv.set(name, new ForallScheme(new TypeVarSet(), fnType));

          break;
        }

      default:
        throw new Error(`Could not forward declare node: unkown node type`);

    }

  }

  private inferNode(node: Syntax, typeEnv: TypeEnv, constraints: Constraint[]): Type {

    switch (node.kind) {

      case SyntaxKind.BoltMatchExpression:
        {

          // We get/create a type environment that is NOT cloned, because that
          // allows us complete access to all locally declared identifiers inside
          // the function.
          const resultType = new TypeVar();

          for (const matchArm of node.arms) {
            const type = this.inferNode(matchArm.body, typeEnv, constraints);
            // FIXME Should let-bindings generated in a match arm be generalised?
            this.inferBinding(matchArm.pattern, type, typeEnv, constraints, false);
            constraints.push([
              type,
              resultType
            ])
          }

          return resultType;
        }

      case SyntaxKind.BoltReferenceTypeExpression:
        {
          assert(node.name.modulePath.length === 0);
          const typeName = getSymbolText(node.name.name);
          if (!this.builtinTypes.has(typeName)) {
            throw new TypeNotFoundError(node, typeName);
          }
          return this.builtinTypes.get(typeName);
        }

      case SyntaxKind.BoltFunctionDeclaration:
        {

          // By now, the forward declaration has made sure that the function
          // body's environment already exists. It should be a fork of the
          // parent environment and not simply a clone.
          const innerTypeEnv = this.getTypeEnvForNode(node, typeEnv, false)

          // Get our type that was forward-declared.
          const fnType = this.nodeToType.get(node.id);

          const paramTypes: Type[] = [];
          for (const param of node.params) {
            const typeVar = new TypeVar();
            this.inferBinding(param.bindings, typeVar, innerTypeEnv, constraints, false);
            paramTypes.push(typeVar);
          }

          const returnType = new TypeVar();
          constraints.push([
            fnType,
            new ArrowType(paramTypes, returnType)
          ]);

          if (node.returnTypeExpr !== null) {
            constraints.push([
              returnType,
              this.inferNode(node.returnTypeExpr, typeEnv, constraints)
            ]);
          }

          if (node.body !== null) {
            for (const returnStmt of getAllReturnStatementsInFunctionBody(node.body)) {
              constraints.push([
                returnType,
                returnStmt.value === null
                  ? this.voidType
                  : this.inferNode(returnStmt.value, innerTypeEnv, constraints)
              ]);
            }
          }

          return fnType;
        }

      case SyntaxKind.BoltVariableDeclaration:
        {

          let resultType: Type;

          if (node.modifiers & BoltModifiers.IsMutable) {

            // We will connect all relevant types to this type variable, so that
            // the unifier can solve them.
            const typeVar = new TypeVar();

            // A type assertion should just add the required constraints to the
            // list and bind whatever type that comes out of it with our type
            // variable.
            if (node.typeExpr !== null) {
              const typeExprType = this.inferNode(node.typeExpr, typeEnv, constraints);
              constraints.push([ typeVar, typeExprType ]);
            }

            // Similar to the type assertion above, but using the value
            // expression if there was any.
            if (node.value !== null) {
              const valueType = this.inferNode(node.value, typeEnv, constraints);
              constraints.push([ typeVar, valueType ])
            }

            // We don't generalize a mutable variable declaration. It causes
            // problems in the case we assign an expression of a different type
            // to the variable. The assignment would be accepted, while it really
            // shouldn't.
            // See also https://cstheory.stackexchange.com/a/42557
            this.inferBinding(node.bindings, typeVar, typeEnv, constraints, false);

            resultType = typeVar;

          } else {

            // It does not make sense to declare a read-only variable without a
            // value associated with it.
            if (node.value === null) {
              throw new UninitializedBindingError(node);
            }

            resultType = this.inferNode(node.value!, typeEnv, constraints);

            this.inferBinding(node.bindings, resultType, typeEnv, constraints, true);
          }

          return resultType;
        }

      case SyntaxKind.BoltConstantExpression:
        {
          if (typeof(node.value) === 'bigint') {
            return this.intType;
          } else if (typeof(node.value === 'string')) {
            return this.stringType;
          } else if (typeof(node.value) === 'boolean') {
            return this.boolType;
          } else {
            throw new Error(`Could not infer type of BoltConstantExpression`)
          }
        }

      case SyntaxKind.BoltReferenceExpression:
        {
          const varName = getSymbolText(node.name.name);
          if (varName === 'true') {
            return this.boolType;
          }
          if (varName === 'false') {
            return this.boolType;
          }
          if (varName === '+') {
            return new ArrowType([ this.intType, this.intType ], this.intType);
          }
          if (varName === '-') {
            return new ArrowType([ this.intType, this.intType ], this.intType);
          }
          if (varName === '/') {
            return new ArrowType([ this.intType, this.intType ], this.intType);
          }
          if (varName === '*') {
            return new ArrowType([ this.intType, this.intType ], this.intType);
          }
          const type = typeEnv.lookup(varName);
          if (type === null) {
            throw new BindingNotFoundError(node, varName);
          }
          return type!;
        }

      case SyntaxKind.BoltCallExpression:
        {
          // This type variable will contain the type that the call expression resolves to.
          const returnType = new TypeVar();

          // First we build a type of the function that has been called. This
          // type should eventually resolve to an ArrowType.
          const operatorType = this.inferNode(node.operator, typeEnv, constraints)

          // Build a list of types that correspond to the parameters of the
          // function being called.
          const operandTypes = [];
          for (const operand of node.operands) {
            const operandType = this.inferNode(operand, typeEnv, constraints);
            operandTypes.push(operandType)
          }

          // Now make sure that the signature built out of these parameter types
          // actually match the function being called. They will be solved later
          // by the unifier.
          constraints.push([
            operatorType,
            new ArrowType(operandTypes, returnType)
          ]);

          return returnType;
        }

      case SyntaxKind.BoltFunctionExpression:
        {

          // Introduce a new scope by starting with a fresh environment.
          const newEnv = typeEnv.clone();

          // Build the function's signature by going through its parameter types.
          const paramTypes = [];
          for (const param of node.params) {
            const typeVar = new TypeVar();
            paramTypes.push(typeVar);
            this.inferBinding(param.bindings, typeVar, newEnv, constraints, false);
          }

          // Whatever the function returns is eventually determined by its body.
          let returnType: Type;
          if (node.expression !== null) {
           returnType = this.inferNode(node.expression!, newEnv, constraints); 
          } else {
            throw new Error(`Not supported yet.`)
          }

          return new ArrowType(
            paramTypes,
            returnType
          );

        }

      case SyntaxKind.BoltAssignStatement:
        {
          this.inferAssignment(
            node.lhs,
            this.inferNode(node.rhs, typeEnv, constraints),
            typeEnv,
            constraints
          );
          return this.voidType;
        }

      default:
        throw new Error(`Could not infer type of node ${kindToString(node.kind)}`)

    }

  }

  public checkNode(node: Syntax, typeEnv: TypeEnv, constraints: Constraint[]): void {
    if (isBoltExpression(node) || isBoltFunctionDeclaration(node) || isBoltAssignStatement(node) || isBoltVariableDeclaration(node)) {
      const type = this.inferNode(node, typeEnv, constraints);
      this.nodeToType.set(node.id, type);
      return;
    }
    switch (node.kind) {
      case SyntaxKind.BoltSourceFile:
        for (const element of node.elements) {
          this.checkNode(element, typeEnv, constraints);
        }
        break;
      case SyntaxKind.BoltExpressionStatement:
        this.checkNode(node.expression, typeEnv, constraints);
        break;
      default:
        throw new Error(`Could not typecheck node: unknown node type ${kindToString(node.kind)}`);
    }
  }

  /**
   * Solve all given constraints by running an unification algorithm on them.
   *
   * Be aware that the constraints might be mutated during this process. If you
   * plan to process them after this method completed, make sure to clone them.
   * 
   * @param constraints A collection of constraints that will be consumed.
   * 
   */
  private solveConstraints(constraints: Constraint[]): TypeVarSubstitution {
    let substitution = new TypeVarSubstitution();
    while (true) {
      if (constraints.length === 0) {
        return substitution;
      }
      const [a, b] = constraints.pop()!;
      const newSubstitution = this.unifies(a, b);
      for (let i = 0; i < constraints.length; i++) {
        const constraint = constraints[i];
        constraint[0] = constraint[0].applySubstitution(newSubstitution)
        constraint[1] = constraint[1].applySubstitution(newSubstitution)
      }
      substitution = newSubstitution.composeWith(substitution);
    }
  }

  private areTypesEqual(a: Type, b: Type): boolean {

    // This is an early check that is not strictly necessary but might speed up
    // things a bit.
    if (a === b) {
      return true;
    }

    // We are checking if two types are equal syntactically, so if they are of
    // a different kind we're already done.
    if (a.kind !== b.kind) {
      return false;
    }

    if (a.kind === TypeKind.PrimType && b.kind === TypeKind.PrimType) {
      return a.primId === b.primId;
    }

    if (a.kind === TypeKind.ArrowType && b.kind === TypeKind.ArrowType) {
      if (a.paramTypes.length !== b.paramTypes.length
          || !this.areTypesEqual(a.returnType, b.returnType)) {
        return false;
      }
      for (let i = 0; i < a.paramTypes.length; i++) {
        if (!this.areTypesEqual(a.paramTypes[i], b.paramTypes[i])) {
          return false;
        }
      }
      return true;
    }

    if (a.kind === TypeKind.TypeVar && b.kind === TypeKind.TypeVar) {
      return a.varId === b.varId;
    }

    throw new Error(`Unexpected combination of types while checking equality`)
  }

  private unifyMany(a: Type[], b: Type[]): TypeVarSubstitution {
    let substitution = new TypeVarSubstitution();
    for (let i = 0; i < a.length; i++) {
      const localSubstitution = this.unifies(a[i], b[i]);
      for (let k = i; k < a.length; k++) {
        a[k] = a[k].applySubstitution(localSubstitution);
        b[k] = b[k].applySubstitution(localSubstitution);
      }
      substitution = localSubstitution.composeWith(substitution);
    }
    return substitution;
  }

  private unifies(a: Type, b: Type): TypeVarSubstitution {

    // Two types that have the same structure can be unified as-is without
    // requiring any substitution to take place.
    if (this.areTypesEqual(a, b)) {
      return new TypeVarSubstitution();
    }

    // The following cases are quite straightforward. We perform some checks
    // and return an empty substitution if both a and b point to the same type
    // variable. Otherwise, we create a simple substitution for the type
    // variable.

    if (a.kind === TypeKind.TypeVar) {
      return bindTypeVar(a, b);
    }
    if (b.kind === TypeKind.TypeVar) {
      return bindTypeVar(b, a);
    }

    if (a.kind === TypeKind.ArrowType && b.kind === TypeKind.ArrowType) {

      // Right now, two arrow types must have the exact same amount of
      // parameters or else an error occurs. In the future, this might changed
      // in order to support default arguments.
      if (a.paramTypes.length !== b.paramTypes.length) {
        throw new ParamCountMismatchError(a, b);
      }

      // Becuase the return type depends on the parameter types, and because a
      // parameter type may eventually depend on a previous parameter (due to
      // default expressions), we use a unifier that simply runs from left to
      // right.
      return this.unifyMany(
        [...a.paramTypes, a.returnType],
        [...b.paramTypes, b.returnType]
      );
    }

    // If we got here then none of our unification rules matched, so the
    // combination of types must be invalid.
    throw new UnificationError(a, b);
  }

  private getTypeEnvForNode(node: Syntax, parentTypeEnv: TypeEnv, shouldClone: boolean): TypeEnv {
    const scopeNode = getNodeIntroducingScope(node)
    if (this.nodeToTypeEnv.has(scopeNode.id)) {
      return this.nodeToTypeEnv.get(scopeNode.id)
    }
    const newTypeEnv = shouldClone
      ? parentTypeEnv.clone()
      : new TypeEnv(parentTypeEnv);
    this.nodeToTypeEnv.set(scopeNode.id, newTypeEnv)
    return newTypeEnv;
  }

  public getTypeOfNode(node: Syntax): Type {;
    const type = this.nodeToType.get(node.id);
    const substitution = this.sourceFileToSubstitution.get(node.getSourceFile().id);
    return type.applySubstitution(substitution);
  }

  public isBuiltinType(name: string) {
    return this.builtinTypes.has(name);
  }

}
