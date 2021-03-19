import { Syntax } from "./ast";
import { CompileError, ParamCountMismatchError, UnboundFreeVariableError, UnificationError } from "./errors";
import { FastStringMap, prettyPrintTag } from "./util";

export enum TypeKind {
  TypeVar,
  PrimType,
  ArrowType,
  TupleType,
}

export type Type
  = TypeVar
  | PrimType
  | ArrowType
  | TupleType

export abstract class TypeBase {

  public abstract kind: TypeKind;

  constructor(public node: Syntax | null) {

  }

  public abstract applySubstitution(substitution: TypeVarSubstitution): Type;

  public abstract hasTypeVariable(typeVar: TypeVar): boolean;

  public abstract deepClone(): Type;

  public abstract [prettyPrintTag](): string;

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

export class TypeVar extends TypeBase {

  public readonly kind = TypeKind.TypeVar;

  constructor(
    node: Syntax | null = null,
    public varId = generateTypeVarId()
  ) {
    super(node);
  }

  public hasTypeVariable(typeVar: TypeVar): boolean {
    return typeVar.varId === this.varId;
  }

  public deepClone(): TypeVar {
    return new TypeVar(this.node, this.varId);
  }

  public applySubstitution(substitution: TypeVarSubstitution): Type {
    if (substitution.has(this)) {
      // FIXME How can we guarantee that a shallow clone is enough when the
      //       original algorithm performs a deep clone?
      const type = substitution.get(this).deepClone();
      return type;
    } else {
      return this;
    }
  }

  public [prettyPrintTag]() {
    return this.varId;
  }

}

export class TupleType extends TypeBase {

  public readonly kind = TypeKind.TupleType;

  constructor(
    public elements: Type[],
    node: Syntax | null = null
  ) {
    super(node);
  }

  public hasTypeVariable(typeVar: TypeVar): boolean {
    return this.elements.some(type => type.hasTypeVariable(typeVar))
  }

  public deepClone(): TupleType {
    return new TupleType(this.elements.map(element => element.deepClone()), this.node);
  }

  public applySubstitution(substitution: TypeVarSubstitution): Type {
    return new TupleType(
      this.elements.map(type => type.applySubstitution(substitution)),
      this.node
    )
  }

  public [prettyPrintTag](): string {
    return '(' + this.elements
      .map(type => type[prettyPrintTag]())
      .join(', ') + ')';
  }

}

let nextPrimTypeId = 0;

export class PrimType extends TypeBase {

  public readonly kind = TypeKind.PrimType;

  public primId: number;

  constructor(
    public displayName: string,
    primId?: number,
    node: Syntax | null = null,
  ) {
    super(node);
    this.primId = primId ?? nextPrimTypeId++;
  }

  public deepClone(): PrimType {
    return new PrimType(this.displayName, this.primId, this.node);
  }

  public hasTypeVariable(_typeVar: TypeVar): boolean {
    return false;
  }

  public applySubstitution(_substitution: TypeVarSubstitution) {
    return this;
  }

  public [prettyPrintTag](): string {
    return this.displayName;
  }

}

export class ArrowType extends TypeBase {

  public readonly kind = TypeKind.ArrowType;

  constructor(
    public paramTypes: Type[],
    public returnType: Type,
    node: Syntax | null = null
  ) {
    super(node);
  }

  public deepClone(): ArrowType {
    return new ArrowType(
      this.paramTypes.map(paramType => paramType.deepClone()),
      this.returnType.deepClone(),
      this.node
    )
  }

  public hasTypeVariable(typeVar: TypeVar): boolean {
    return this.paramTypes.some(paramType => paramType.hasTypeVariable(typeVar))
        || this.returnType.hasTypeVariable(typeVar);
  }

  public applySubstitution(substitution: TypeVarSubstitution): Type {
    return new ArrowType(
      this.paramTypes.map(type => type.applySubstitution(substitution)),
      this.returnType.applySubstitution(substitution),
      this.node
    )
  }

  public [prettyPrintTag](): string {
    return `(${this.paramTypes.map(type => type[prettyPrintTag]()).join(', ')}) -> ${this.returnType[prettyPrintTag]()}`
  }

}

function compareToDiff<T>(lessThan: (a: T, b: T) => boolean) {
  return function (a: T, b: T) {
    if (lessThan(a, b)) {
      return -1;
    } else if (lessThan(b, a)) {
      return 1;
    } else {
      return 0;
    }
  }
}

export function isType(value: any): value is Type {
  return Object(value) instanceof TypeBase;
}

export const intType = new PrimType('Int');
export const stringType = new PrimType('String');
export const boolType = new PrimType('Bool');
export const voidType = new PrimType('void');

export class TypeVarSubstitution {

  private mapping = new FastStringMap<string, [TypeVar, Type]>();

  public add(source: TypeVar, target: Type) {
    if (this.mapping.has(source.varId)) {
      throw new Error(`Could not add type variable to substitution: variable ${source.varId} already exists.`)
    }
    this.mapping.add(source.varId, [source, target]);
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
        this.mapping.add(name, entry);
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

export class TypeVarSet {

  private typeVarIds = new FastStringMap<string, TypeVar>();

  constructor(iterable: Iterable<TypeVar> = []) {
    for (const typeVar of iterable) {
      this.add(typeVar);
    }
  }

  public add(typeVar: TypeVar): void {
    if (!this.typeVarIds.has(typeVar.varId)) {
      this.typeVarIds.add(typeVar.varId, typeVar);
    }
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

const emptyTypeVarSubstitution = new TypeVarSubstitution();

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

function unifyMany(leftTypes: Type[], rightTypes: Type[]): TypeVarSubstitution {
  let substitution = new TypeVarSubstitution();
  for (let i = 0; i < leftTypes.length; i++) {
    const localSubstitution = unifies(leftTypes[i], rightTypes[i]);
    for (let k = i; k < leftTypes.length; k++) {
      leftTypes[k] = leftTypes[k].applySubstitution(localSubstitution);
      rightTypes[k] = rightTypes[k].applySubstitution(localSubstitution);
    }
    substitution = localSubstitution.composeWith(substitution);
  }
  return substitution;
}

export function unifies(a: Type, b: Type): TypeVarSubstitution {

  // Two types that have the same structure can be unified as-is without
  // requiring any substitution to take place.
  if (areTypesEqual(a, b)) {
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
    return unifyMany(
      [...a.paramTypes, a.returnType],
      [...b.paramTypes, b.returnType]
    );
  }

  // If we got here then none of our unification rules matched, so the
  // combination of types must be invalid.
  throw new UnificationError(a, b);
}

function areTypesEqual(a: Type, b: Type): boolean {

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
        || !areTypesEqual(a.returnType, b.returnType)) {
      return false;
    }
    for (let i = 0; i < a.paramTypes.length; i++) {
      if (!areTypesEqual(a.paramTypes[i], b.paramTypes[i])) {
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

export function isTypeAssignableTo(a: Type, b: Type): boolean {
  try {
    unifies(a, b);
  } catch (error) {
    if (!(error instanceof CompileError)) {
      throw error;
    }
    return false;
  }
  return true;
}

