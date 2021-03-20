
import { Syntax } from "./ast";
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

  public get solved(): Type {
    return this as unknown as Type;
  }

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

  private solvedType?: Type;

  constructor(
    node: Syntax | null = null,
    public varId = generateTypeVarId()
  ) {
    super(node);
  }

  public get solved(): Type {
    if (this.solvedType === undefined) {
      return this;
    }
    let currType: Type = this;
    do {
      currType = currType.solvedType!;
    } while (currType.kind === TypeKind.TypeVar && currType.solvedType !== undefined);
    return this.solvedType = currType;
  }

  public set solved(newType: Type) {
    this.solvedType = newType;
  }

  public resolve(): Type {
    return this.solved;
  }

  public hasTypeVariable(typeVar: TypeVar): boolean {
    return typeVar.varId === this.varId;
  }

  public deepClone(): TypeVar {
    return new TypeVar(this.node, this.varId);
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

  public resolve(): Type {
    return new TupleType(
      this.elements.map(type => type.resolve())
    );
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

  public resolve(): Type {
    return this;
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

  public resolve(): Type {
    return new ArrowType(
      this.paramTypes.map(type => type.resolve()),
      this.returnType.resolve()
    );
  }

  public [prettyPrintTag](): string {
    return `(${this.paramTypes
        .map(paramTypeRef => paramTypeRef)
        .map(paramType => paramType[prettyPrintTag]())
        .join(', ')}) -> ${this.returnType[prettyPrintTag]()}`
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

  //public composeWith(other: TypeVarSubstitution): TypeVarSubstitution {
  //  const newSubstitution = new TypeVarSubstitution();
  //  for (const [typeVar, type] of other) {
  //    newSubstitution.add(typeVar, type.applySubstitution(this));
  //  }
  //  for (const [typeVar, type] of this) {
  //    newSubstitution.add(typeVar, type);
  //  }
  //  return newSubstitution;
  //}

  //public applyComposition(other: TypeVarSubstitution): void {
  //  for (const [typeVar, type] of other) {
  //    // if (this.has(typeVar)) {
  //    //   this.delete(typeVar);
  //    // }
  //    this.add(typeVar, type.applySubstitution(this));
  //  }
  //}

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

export function areTypesEqual(a: Type, b: Type): boolean {

  // We need to take in account any type variables that already have been
  // substituted. The 'solved' property contains the type with as much
  // top-level type variables replaced as possible.
  a = a.solved;
  b = b.solved;

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
  // Provided we have already unified the both `a` and `b` in the past,
  // checking for assignability is just the same as checking the types we get
  // after resolving any type variables are syntactically equivalent.
  return areTypesEqual(a, b);
}

