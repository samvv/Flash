
import {
  Modifiers,
  Pattern,
  isAssignStatement,
  isBlockExpression,
  isExpression,
  isFunctionDeclaration,
  isFunctionExpression,
  isSourceFile,
  isVariableDeclaration,
  kindToString,
  NodeFlags,
  SourceFile,
  Syntax,
  SyntaxKind
} from "./ast";
import { getSymbolText } from "./common";
import {
  TypeNotFoundError,
  UninitializedBindingError,
  BindingNotFoundError,
} from "./errors";
import { Program } from "./program";
import { assert, FastStringMap } from "./util";
import {
  Type,
  TypeKind,
  TypeVar,
  TypeVarSubstitution,
  TypeVarSet,
  ArrowType,
  PrimType,
  TupleType,
  intType,
  boolType,
  stringType,
  unifies
} from "./types"

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

class ForallScheme {

  public typeVars: TypeVarSet;

  constructor(
    typeVars: TypeVarSet | Iterable<TypeVar> = [],
    public type: Type,
  ) {
    if (typeVars instanceof TypeVarSet) {
      this.typeVars = typeVars;
    } else if (Array.isArray(typeVars)) {
      this.typeVars = new TypeVarSet;
      for (const typeVar of typeVars) {
        this.typeVars.add(typeVar)
      }
    } else {
      throw new Error(`Could not create a type variable set out of the given argument`);
    }
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
    this.mapping.add(name, scheme)
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
      freshVars.add(typeVar, new TypeVar(typeVar.node))
    }
    return scheme.type.applySubstitution(freshVars);
  }

  public has(name: string): boolean {
    let currEnv: TypeEnv | null = this;
    while (true) { 
      if (currEnv!.mapping.has(name)) {
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
function getNodeIntroducingScope(node: Syntax) {
  let currNode: Syntax | null = node;
  while (true) {
    if (isSourceFile(currNode)
    || isBlockExpression(currNode)
    || isFunctionDeclaration(currNode)
    || (isFunctionExpression(currNode) && currNode.body !== null)) {
      return currNode;
    }
    if (currNode!.parentNode === null) {
      return currNode;
    }
    currNode = currNode!.parentNode;
  }
}

// function listOriginatingTypes(types: Type[]) {
//   const diagnostics = [];
//   for (let i = 0; i < types.length; i++) {
//     const type = types[i];
//     if (type !== null) {
//       let message = E_TYPE_ORIGINATED_FROM_HERE ;
//       if (types.length >= 2) {
//         if (i === 0) {
//           message = E_FIRST_TYPE_ORIGINATED_FROM_HERE;
//         } else if (i === 1) {
//           message = E_SECOND_TYPE_ORIGINATED_FROM_HERE;
//         }
//       }
//       diagnostics.push({
//         node: type.node,
//         message,
//         severity: 'info'
//       });
//     }
//   }
//   return diagnostics;
// }

export class TypeChecker {

  private nextPrimTypeId = 1;

  private nodeToType = new FastStringMap<number, Type>();
  private nodeToTypeEnv = new FastStringMap<number, TypeEnv>();
  private sourceFileToSubstitution = new FastStringMap<number, TypeVarSubstitution>();

  private builtinTypeEnv = new TypeEnv();

  constructor(public program: Program) {
    this.builtinTypeEnv.set('Int', new ForallScheme([], intType))
    this.builtinTypeEnv.set('String', new ForallScheme([], stringType))
    this.builtinTypeEnv.set('Bool', new ForallScheme([], boolType))
    this.builtinTypeEnv.set('false', new ForallScheme([], boolType))
    this.builtinTypeEnv.set('true', new ForallScheme([], boolType))
    this.builtinTypeEnv.set('+', new ForallScheme([], new ArrowType([ intType, intType ], intType)))
    this.builtinTypeEnv.set('-', new ForallScheme([], new ArrowType([ intType, intType ], intType)))
    this.builtinTypeEnv.set('/', new ForallScheme([], new ArrowType([ intType, intType ], intType)))
    this.builtinTypeEnv.set('*', new ForallScheme([], new ArrowType([ intType, intType ], intType)))
    this.builtinTypeEnv.set('==', new ForallScheme([], new ArrowType([ intType, intType ], boolType)))
    this.builtinTypeEnv.set('!=', new ForallScheme([], new ArrowType([ intType, intType ], boolType)))
  }

  public registerSourceFile(sourceFile: SourceFile): void {

    // This type environment will contain all top-level bindings that are
    // declared inside the source file.
    const newTypeEnv = new TypeEnv(this.builtinTypeEnv);

    //for (const type of this.builtinTypes) {
    //  newTypeEnv.set(type.displayName, new ForallScheme(new TypeVarSet, type));
    //}

    this.nodeToTypeEnv.add(sourceFile.id, newTypeEnv)

    // All contraints that are generated inside this source file and all
    // imported source files are stored in this array.
    const constraints: Constraint[] = [];

    // We want to be lazy and only import those source files that are actually
    // used. The auto-imported source files should be scanned anyways because
    // their bindings might be used without a corresponding import-statement.
    for (const importedSourceFile of this.program.getAllGloballyDeclaredSourceFiles()) {
      this.forwardDeclare(importedSourceFile, newTypeEnv);
      this.checkNode(importedSourceFile, newTypeEnv, constraints);
    }

    // If the source file wasn't auto-imported it can't possibly be processed
    // yet because the previous loop only took the auto-imported source files.
    if ((sourceFile.flags & NodeFlags.AutoImported) === 0) {
      this.forwardDeclare(sourceFile, newTypeEnv);
      this.checkNode(sourceFile, newTypeEnv, constraints);
    }

    // Save the substitution for future reference.
    this.sourceFileToSubstitution.add(sourceFile.id, this.solveConstraints(constraints));

  }

  private inferBinding(
    node: Pattern,
    valueType: Type,
    typeEnv: TypeEnv,
    constraints: Constraint[],
    shouldGeneralize: boolean
  ) {
    switch (node.kind) {
      case SyntaxKind.ExpressionPattern:
        {
          constraints.push([
            valueType,
            this.inferNode(node.expression, typeEnv, constraints)
          ])
          break;
        }
      case SyntaxKind.BindPattern:
        {
          const varName = node.name.text;
          const localValueType = valueType.shallowClone();
          localValueType.node = node;
          typeEnv.set(varName, shouldGeneralize ? generalizeType(localValueType, typeEnv) : new ForallScheme([], localValueType));
          break;
        }
      default:
        throw new Error(`Could not infer constraints from pattern in binding: unknown node type`);
    }
  }

  private inferAssignment(node: Pattern, valueType: Type, typeEnv: TypeEnv, constraints: Constraint[]) {
    switch (node.kind) {
      case SyntaxKind.BindPattern:
      {
        const varName = node.name.text;
        const varType = typeEnv.lookup(varName);
        if (varType === null) {
          throw new BindingNotFoundError(node, varName);
        }
        const localVarType = varType.shallowClone();
        localVarType.node = node;
        constraints.push([
          localVarType,
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

      case SyntaxKind.SourceFile:
        {
          for (const element of node.elements) {
            this.forwardDeclare(element, typeEnv);
          }
          break;
        }

      case SyntaxKind.ReferenceExpression:
      case SyntaxKind.MatchExpression:
      case SyntaxKind.ConstantExpression:
        break;

      case SyntaxKind.RecordDeclaration:
      {
        typeEnv.set(node.name.text, new ForallScheme([], new PrimType(node.name.text, undefined, node)));
        break;
      }

      case SyntaxKind.EnumDeclaration:
      {
        const enumType = new PrimType(node.name.text, undefined, node);
        typeEnv.set(node.name.text, new ForallScheme([], enumType));
        for (const member of node.members) {
          typeEnv.set(node.name.text + '::' + member.text, new ForallScheme([], enumType));
        }
        break;
      }

      case SyntaxKind.CallExpression:
        {
          this.forwardDeclare(node.operator, typeEnv);
          for (const operand of node.operands) {
            this.forwardDeclare(operand, typeEnv);
          }
          break;
        }

      case SyntaxKind.FunctionExpression:
        {
          // TODO
          break;
        }

      case SyntaxKind.ExpressionStatement:
        {
          this.forwardDeclare(node.expression, typeEnv);
          break;
        }

      case SyntaxKind.VariableDeclaration:
        break;

      case SyntaxKind.AssignStatement:
        {
          this.forwardDeclare(node.rhs, typeEnv);
          break;
        }

      case SyntaxKind.FunctionDeclaration:
        {

          const name = getSymbolText(node.name);

          // Right now, we don't know much about the function and just want to
          // make sure that whatever we learn about it is linked into this type variable.
          const fnType = new TypeVar(node);

          this.nodeToType.add(node.id, fnType);

          // We get/create a type environment that is NOT cloned, because that
          // allows us complete access to all locally declared identifiers inside
          // the function.
          const innerTypeEnv = this.getTypeEnvForNode(node, typeEnv, false);

          // Register this function in the type environment it was presumably
          // defined in. This will also define it in the function's body since
          // the two environments should be linked.
          // FIXME Should this type be generalised?
          typeEnv.set(name, new ForallScheme([], fnType));

          break;
        }

      default:
        throw new Error(`Could not forward declare node: unkown node type ${kindToString(node.kind)}`);

    }

  }

  private inferNode(
    node: Syntax,
    typeEnv: TypeEnv,
    constraints: Constraint[],
    returnType: Type | null = null
  ): Type {

    switch (node.kind) {

      case SyntaxKind.ReturnStatement:
      {
        assert(returnType !== null);
        const valueType = node.value === null
            ? new TupleType([], node)
            : this.inferNode(node.value, typeEnv, constraints)
        constraints.push([
          returnType!,
          valueType,
        ]);
        return valueType;
      }

      case SyntaxKind.CaseStatement:
        {
          for (const caseNode of node.cases) {
            if (caseNode.test !== null) {
              constraints.push([
                this.inferNode(caseNode.test, typeEnv, constraints, returnType),
                // FIXME Should be looked up instead of created.
                boolType
              ]);
            }
            for (const element of caseNode.body) {
              this.inferNode(element, typeEnv, constraints, returnType);
            }
          }
          return new TupleType([], node);
        }

      case SyntaxKind.MatchExpression:
        {

          const valueType = this.inferNode(node.value, typeEnv, constraints);
          const resultType = new TypeVar(node);

          for (const matchArm of node.arms) {
            const innerTypeEnv = new TypeEnv(typeEnv);
            // FIXME Should let-bindings generated in a match arm be generalised?
            this.inferBinding(matchArm.pattern, valueType, innerTypeEnv, constraints, false);
            const type = this.inferNode(matchArm.body, innerTypeEnv, constraints);
            constraints.push([
              resultType,
              type,
            ])
          }

          return resultType;
        }

      case SyntaxKind.ReferenceTypeExpression:
        {
          assert(node.name.modulePath.length === 0);
          const typeName = getSymbolText(node.name.name);
          const type = typeEnv.lookup(typeName);
          if (type === null) {
            throw new TypeNotFoundError(node, typeName);
          }
          const localType = type.shallowClone();
          localType.node = node;
          return localType!;
        }

      case SyntaxKind.FunctionDeclaration:
        {

          // By now, the forward declaration has made sure that the function
          // body's environment already exists. It should be a fork of the
          // parent environment and not simply a clone.
          const innerTypeEnv = this.getTypeEnvForNode(node, typeEnv, false)

          // Get our type that was forward-declared.
          const fnType = this.nodeToType.get(node.id);

          const paramTypes: Type[] = [];
          for (const param of node.params) {
            let paramType
            if (param.typeExpr !== null) {
              paramType = this.inferNode(param.typeExpr, typeEnv, constraints);
            } else {
              paramType = new TypeVar(param); 
            }
            this.inferBinding(param.bindings, paramType, innerTypeEnv, constraints, false);
            paramTypes.push(paramType);
          }

          const returnType = new TypeVar(node);
          constraints.push([
            fnType,
            new ArrowType(paramTypes, returnType, node)
          ]);

          if (node.returnTypeExpr !== null) {
            constraints.push([
              returnType,
              this.inferNode(node.returnTypeExpr, typeEnv, constraints)
            ]);
          }

          if (node.body !== null) {
            for (const element of node.body) {
              this.inferNode(element, innerTypeEnv, constraints, returnType);
            }
          }

          return fnType;
        }

      case SyntaxKind.VariableDeclaration:
        {

          let resultType: Type;

          if (node.modifiers & Modifiers.IsMutable) {

            // We will connect all relevant types to this type variable, so that
            // the unifier can solve them.
            const typeVar = new TypeVar(node);

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

      case SyntaxKind.ConstantExpression:
        {
          let typePath;
          if (typeof(node.value) === 'bigint') {
            typePath = 'Int';
          } else if (typeof(node.value === 'string')) {
            typePath = 'String';
          } else if (typeof(node.value) === 'boolean') {
            typePath = 'Bool';
          } else {
            throw new Error(`Could not infer type of ConstantExpression`);
          }
          const declaredType = typeEnv.lookup(typePath);
          if (declaredType === null) {
            throw new TypeNotFoundError(node, typePath);
          }
          const localType = declaredType.shallowClone()
          localType.node = node;
          return localType;
        }

      case SyntaxKind.ReferenceExpression:
        {
          // Right now we don't support resolving absolute module paths.
          assert(!node.name.isAbsolute);

          // We build an unique identifier for this reference expression based
          // on the module path and the identifier.
          let varName = '';
          for (const element of node.name.modulePath) {
            varName += element.text + '::';
          }
          varName += getSymbolText(node.name.name);

          const type = typeEnv.lookup(varName);
          if (type === null) {
            throw new BindingNotFoundError(node, varName);
          }
          const localType = type.shallowClone();
          localType.node = node;
          return localType;
        }

      case SyntaxKind.CallExpression:
        {
          // This type variable will contain the type that the call expression resolves to.
          const returnType = new TypeVar(node);

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
            new ArrowType(operandTypes, returnType, node)
          ]);

          return returnType;
        }

      case SyntaxKind.FunctionExpression:
        {

          // Introduce a new scope by starting with a fresh environment.
          const newEnv = typeEnv.clone();

          // Build the function's signature by going through its parameter types.
          const paramTypes = [];
          for (const param of node.params) {
            const typeVar = new TypeVar(param);
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
            returnType,
            node
          );

        }

      case SyntaxKind.AssignStatement:
        {
          this.inferAssignment(
            node.lhs,
            this.inferNode(node.rhs, typeEnv, constraints),
            typeEnv,
            constraints
          );
          return new TupleType([], node);
        }

      default:
        throw new Error(`Could not infer type of node ${kindToString(node.kind)}`)

    }

  }

  public checkNode(node: Syntax, typeEnv: TypeEnv, constraints: Constraint[]): void {
    if (isExpression(node)
        || isFunctionDeclaration(node)
        || isAssignStatement(node)
        || isVariableDeclaration(node)) {
      const type = this.inferNode(node, typeEnv, constraints);
      if (!this.nodeToType.has(node.id)) {
        this.nodeToType.add(node.id, type);
      }
      return;
    }
    switch (node.kind) {
      case SyntaxKind.SourceFile:
        for (const element of node.elements) {
          this.checkNode(element, typeEnv, constraints);
        }
        break;
      case SyntaxKind.ExpressionStatement:
        this.checkNode(node.expression, typeEnv, constraints);
        break;
      case SyntaxKind.EnumDeclaration:
      case SyntaxKind.RecordDeclaration:
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
      const newSubstitution = unifies(a, b);
      for (let i = 0; i < constraints.length; i++) {
        const constraint = constraints[i];
        constraint[0] = constraint[0].applySubstitution(newSubstitution)
        constraint[1] = constraint[1].applySubstitution(newSubstitution)
      }
      substitution = newSubstitution.composeWith(substitution);
    }
  }


  private getTypeEnvForNode(node: Syntax, parentTypeEnv: TypeEnv, shouldClone: boolean): TypeEnv {
    const scopeNode = getNodeIntroducingScope(node)
    if (this.nodeToTypeEnv.has(scopeNode.id)) {
      return this.nodeToTypeEnv.get(scopeNode.id)
    }
    const newTypeEnv = shouldClone
      ? parentTypeEnv.clone()
      : new TypeEnv(parentTypeEnv);
    this.nodeToTypeEnv.add(scopeNode.id, newTypeEnv)
    return newTypeEnv;
  }

  public getTypeOfNode(node: Syntax): Type {;
    const type = this.nodeToType.get(node.id);
    const substitution = this.sourceFileToSubstitution.get(node.getSourceFile().id);
    return type.applySubstitution(substitution);
  }

}
