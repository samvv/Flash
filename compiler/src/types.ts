
import { isRecordDeclaration, RecordDeclaration, Syntax } from "./ast";
import { FastStringMap, prettyPrintTag } from "./util";

export enum TypeKind {
  TypeVar,
  PrimType,
  ArrowType,
  TupleType,
  RecordFieldType,
  RecordType,
}

export type Type
  = TypeVar
  | PrimType
  | ArrowType
  | TupleType
  | RecordFieldType
  | RecordType

export abstract class TypeBase {

  public abstract kind: TypeKind;

  private solvedType?: Type;

  constructor(public node: Syntax | null) {

  }

  public get solved(): Type {
    if (this.solvedType === undefined) {
      return this as unknown as Type;
    }
    let currType: Type = this as unknown as Type;
    do {
      currType = currType.solvedType!;
    } while (currType.solvedType !== undefined);
    return this.solvedType = currType;
  }

  public set solved(newType: Type) {
    this.solvedType = newType;
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

export class RecordType extends TypeBase {

  public readonly kind = TypeKind.RecordType;

  private fieldTypeMap: Map<string, Type>;

  constructor(
    fieldTypes: Iterable<[string, Type]>,
    public declaration: RecordDeclaration,
    node: Syntax | null = null
  ) {
    super(node);
    this.fieldTypeMap = new Map(fieldTypes);
  }

  public [Symbol.iterator]() {
    return this.fieldTypeMap[Symbol.iterator]();
  }

  public hasField(fieldName: string): boolean {
    return this.fieldTypeMap.has(fieldName);
  }

  public getFieldType(fieldName: string): Type {
    if (!this.fieldTypeMap.has(fieldName)) {
      throw new Error(`Trying to get a field named ${fieldName} on a record type that does not have it.`);
    }
    return this.fieldTypeMap.get(fieldName)!;
  }

  public applySubstitution(substitution: TypeVarSubstitution): RecordType {
    return new RecordType(
      [...this.fieldTypeMap].map(([fieldName, fieldType]) => [fieldName, fieldType.applySubstitution(substitution)]),
      this.node
    );
  }

  public hasTypeVariable(typeVar: TypeVar): boolean {
    for (const [_fieldName, fieldType] of this.fieldTypeMap) {
      if (fieldType.hasTypeVariable(typeVar)) {
        return true;
      }
    }
    return false;
  }

  public deepClone(): RecordType {
    return new RecordType(
      [...this.fieldTypeMap].map(([fieldName, fieldType]) => [fieldName, fieldType.deepClone()]),
      this.node,
    );
  }

  public [prettyPrintTag](): string {
    if (isRecordDeclaration(this.node)) {
      return this.node.name.text;
    }
    return '{ ' + [...this.fieldTypeMap].map(([fieldName, fieldType]) => `${fieldName}: ${fieldType[prettyPrintTag]()}`).join(', ') + ' }';
  }

  public resolve(): RecordType {
    return new RecordType(
      [...this.fieldTypeMap].map(([fieldName, fieldType]) => [fieldName, fieldType.resolve()]),
      this.node,
    );
  }

}

export class RecordFieldType extends TypeBase {

  public readonly kind = TypeKind.RecordFieldType;

  constructor(
    public recordType: Type,
    public fieldName: string,
    node: Syntax | null = null
  ) {
    super(node);
  }

  public [prettyPrintTag](): string {
    return this.recordType[prettyPrintTag]() + '.' + this.fieldName;
  }

  public hasTypeVariable(typeVar: TypeVar): boolean {
    return this.recordType.hasTypeVariable(typeVar);
  }

  public applySubstitution(substitution: TypeVarSubstitution): RecordFieldType {
    return new RecordFieldType(
      this.recordType.applySubstitution(substitution),
      this.fieldName,
      this.node,
    );
  }

  public resolve(): Type {
    return this.solved;
  }

  public deepClone(): RecordFieldType {
    return new RecordFieldType(
      this.recordType.deepClone(),
      this.fieldName,
      this.node,
    );
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

  if (a.kind === TypeKind.RecordType && b.kind === TypeKind.RecordType) {
    return a.declaration === b.declaration;
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

