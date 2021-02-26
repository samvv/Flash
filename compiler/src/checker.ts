
import { BoltBindPattern, kindToString, SourceFile, Syntax, SyntaxKind } from "./ast";
import { getSymbolText } from "./common";
import { assert, FastStringMap } from "./util";

enum TypeKind {
  TypeVar,
  PrimType,
  ArrowType,
}

type Type
  = TypeVar
  | PrimType
  | ArrowType

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

  constructor(public hint?: string) {
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

  public format() {
    return this.varId;
  }

}

class PrimType implements TypeBase {

  public readonly kind: TypeKind.PrimType = TypeKind.PrimType;

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

  public format() {
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

  public format(): string {
    return `(${this.paramTypes.map(type => type.format()).join(', ')}) -> ${this.returnType.format()}`
  }

}

function getFreeTypeVariablesOfType(type: Type) {

  const freeVariables = new Set<string>();

  const visit = (type: Type) => {

    switch (type.kind) {

      case TypeKind.TypeVar:
        freeVariables.add(type.varId);
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

  public compose(other: TypeVarSubstitution): TypeVarSubstitution {
    const newSubstitution = new TypeVarSubstitution();
    for (const [typeVar, type] of other) {
      newSubstitution.add(typeVar, type.applySubstitution(this));
    }
    for (const [typeVar, type] of this) {
      newSubstitution.add(typeVar, type);
    }
    return newSubstitution;
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

class ForallScheme {

  private typeVarIds = new Set<string>();

  constructor(
    public typeVars: TypeVar[],
    public type: Type,
  ) {
    this.typeVarIds = new Set(typeVars.map(tv => tv.varId));
  }

  public applySubstitution(substitution: TypeVarSubstitution): Scheme {
    const newSubstitution = new TypeVarSubstitution();
    for (const [typeVar, mappedType] of substitution) {
      if (!this.typeVarIds.has(typeVar.varId)) {
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

  public set(name: string, scheme: Scheme) {
    this.mapping.set(name, scheme)
  }

  public remove(name: string): void {
    this.mapping.delete(name)
  }

  public lookup(name: string): Type | null {
    if (!this.mapping.has(name)) {
      return null;
    }
    const scheme = this.mapping.get(name);
    const freshVars = new TypeVarSubstitution();
    for (const typeVar of scheme.typeVars) {
      freshVars.add(typeVar, new TypeVar())
    }
    return scheme.type.applySubstitution(freshVars);
  }

  public has(typeVar: TypeVar): boolean {
    return this.mapping.has(typeVar.varId)
  }

  public clone(): TypeEnv {
    const result = new TypeEnv();
    for (const [name, scheme] of this.mapping) {
      result.set(name, scheme);
    }
    return result;
  }

}

function bindTypeVar(typeVar: TypeVar, type: Type): TypeVarSubstitution {
  if (type.kind === TypeKind.TypeVar && type.varId === typeVar.varId) {
    return emptyTypeVarSubstitution;
  }
  if (type.hasTypeVariable(typeVar)) {
    throw new Error(`Type ${type.format()} has ${typeVar.format()} as an unbound free type variable.`);
  }
  const substitution = new TypeVarSubstitution();
  substitution.add(typeVar, type);
  return substitution
}

export class TypeChecker {

  private nextVarId = 1;
  private nextPrimTypeId = 1;

  private intType = this.createPrimType('int');
  private stringType = this.createPrimType('String');

  public isIntType(type: Type) {
    return type === this.intType;
  }

  public isStringType(type: Type) {
    return type === this.stringType;
  }

  private builtinTypes = new FastStringMap<string, Type>([
    ['int', this.intType],
    ['String', this.stringType]
  ]);

  private createPrimType(name: string): PrimType {
    return new PrimType(this.nextPrimTypeId++, name);
  }

  public registerSourceFile(sourceFile: SourceFile): void {
    
  }

  private applySubstitutionToConstraints(constraints: Constraint[], substitution: TypeVarSubstitution): void {
    for (let i = 0; i < constraints.length; i++) {
      const constraint = constraints[i];
      constraint[0] = constraint[0].applySubstitution(substitution)
      constraint[1] = constraint[1].applySubstitution(substitution)
    }
  }

  private inferNode(node: Syntax, env: TypeEnv, constraints: Constraint[]): Type {

    switch (node.kind) {

      case SyntaxKind.BoltConstantExpression:
      {
        if (typeof(node.value) === 'bigint') {
          return this.intType;
        } else if (typeof(node.value === 'string')) {
          return this.stringType;
        } else {
          throw new Error(`Could not infer type of BoltConstantExpression`)
        }
      }

      case SyntaxKind.BoltReferenceExpression:
      {
        const text = getSymbolText(node.name.name);
        if (text === '+') {
          return new ArrowType([ this.intType, this.intType ], this.intType);
        }
        const type = env.lookup(text)
        assert(type !== null);
        return type!;
      }

      case SyntaxKind.BoltCallExpression:
      {
        const operatorType = this.inferNode(node.operator, env, constraints)
        const operandTypes = [];
        for (const operand of node.operands) {
          const operandType = this.inferNode(operand, env, constraints);
          operandTypes.push(operandType)
        }
        const returnType = new TypeVar();
        constraints.push([
          operatorType,
          new ArrowType(operandTypes, returnType)
        ])
        return returnType;
      }

      case SyntaxKind.BoltFunctionExpression:
      {
        const tvs = [];
        const newEnv = env.clone();
        for (const param of node.params) {
          const tv = new TypeVar();
          tvs.push(tv)
          const x = (param.bindings as BoltBindPattern).name.text;
          newEnv.set(x, new ForallScheme([], tv))
        }
        const returnType = this.inferNode(node.expression!, newEnv, constraints);
        return new ArrowType(
          tvs,
          returnType
        );
      }

      default:
        throw new Error(`Could not infer type of node ${kindToString(node.kind)}`)

    }

  }

  public checkNode(node: Syntax): void {

  }

  private solveConstraints(constraints: Constraint[]) {
    let substitution = new TypeVarSubstitution();
    while (true) {
      if (constraints.length === 0) {
        return substitution;
      }
      const [a, b] = constraints.pop()!;
      const newSubstitution = this.unifies(a, b);
      substitution = newSubstitution.compose(substitution);
      this.applySubstitutionToConstraints(constraints, newSubstitution);
    }
  }

  private areTypesEqual(a: Type, b: Type): boolean {
    if (a === b) { 
      return true;
    }
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

  private unifies(a: Type, b: Type): TypeVarSubstitution {
    if (this.areTypesEqual(a, b)) {
      return new TypeVarSubstitution();
    }
    if (a.kind === TypeKind.TypeVar) {
      return bindTypeVar(a, b);
    }
    if (b.kind === TypeKind.TypeVar) {
      return bindTypeVar(b, a);
    }
    if (a.kind === TypeKind.ArrowType && b.kind === TypeKind.ArrowType) {
      if (a.paramTypes.length !== b.paramTypes.length) {
        throw new Error(`Parameter count does not match.`)
      }
      let substitution = new TypeVarSubstitution();
      let returnA = a.returnType;
      let returnB = b.returnType;
      for (let i = 0; i < a.paramTypes.length; i++) {
        const paramSubstitution = this.unifies(a.paramTypes[i], b.paramTypes[i]);
        returnA = returnA.applySubstitution(paramSubstitution);
        returnB = returnB.applySubstitution(paramSubstitution);
        substitution = paramSubstitution.compose(substitution);
      }
      const returnSubstitution = this.unifies(returnA, returnB);
      return returnSubstitution.compose(substitution);
    }
    throw new Error(`Types ${a.format()} and ${b.format()} could not be unified`)
  }

  public getTypeOfNode(node: Syntax, env: TypeEnv = new TypeEnv()): Type {
    const constraints: Constraint[] = [];
    const type = this.inferNode(node, env, constraints);
    const substitution = this.solveConstraints(constraints);
    return type.applySubstitution(substitution);
  }

  public isBuiltinType(name: string) {
    return this.builtinTypes.has(name);
  }

}
