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

export interface TypeBase {
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

const classTag = Symbol('class tag');
const typeClass = Symbol('type class');
const nodeClass = Symbol('node class');

export class TypeVar implements TypeBase {

  public readonly [classTag] = typeClass;

  public readonly kind: TypeKind.TypeVar = TypeKind.TypeVar;

  constructor(
    public node: Syntax,
    public varId = generateTypeVarId()
  ) {

  }

  public hasTypeVariable(typeVar: TypeVar): boolean {
    return typeVar.varId === this.varId;
  }

  public shallowClone(): TypeVar {
    return new TypeVar(this.node, this.varId);
  }

  public applySubstitution(substitution: TypeVarSubstitution): Type {
    if (substitution.has(this)) {
      const type = substitution.get(this).shallowClone();
      // type.node = this.node;
      return type;
    } else {
      return this;
    }
  }

  public [prettyPrintTag]() {
    return this.varId;
  }

}

export class TupleType implements TypeBase {

  public readonly [classTag] = typeClass;

  public readonly kind = TypeKind.TupleType;

  constructor(
    public elements: Type[],
    public node: Syntax
  ) {

  }

  public hasTypeVariable(typeVar: TypeVar): boolean {
    return this.elements.some(type => type.hasTypeVariable(typeVar))
  }

  public shallowClone() {
    return new TupleType(this.elements, this.node);
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

export class PrimType implements TypeBase {

  public readonly [classTag] = typeClass;

  public readonly kind = TypeKind.PrimType;

  constructor(
    public primId: number,
    public displayName: string,
    public node: Syntax,
  ) {

  }

  public shallowClone() {
    return new PrimType(this.primId, this.displayName, this.node);
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

export class ArrowType implements TypeBase {

  public readonly [classTag] = typeClass;

  public readonly kind: TypeKind.ArrowType = TypeKind.ArrowType;

  constructor(
    public paramTypes: Type[],
    public returnType: Type,
    public node: Syntax
  ) {

  }

  public shallowClone() {
    return new ArrowType(this.paramTypes, this.returnType, this.node)
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

export function isType(value: any): value is Type {
  return value !== null
      && typeof(value) === 'object'
      && value[classTag] === typeClass;
}

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
    this.typeVarIds.add(typeVar.varId, typeVar);
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
