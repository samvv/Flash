
import { isRecordDeclaration, RecordDeclaration, Syntax } from "./ast";
import { assert, FastMultiMap, FastStringMap, prettyPrintTag } from "./util";

export enum TypeKind {
  TypeVar,
  PrimType,
  ArrowType,
  TupleType,
  MemberType,
  RecordType,
  OverloadType,
  NeverType,
}

export type Type
  = TypeVar
  | PrimType
  | ArrowType
  | TupleType
  | MemberType
  | RecordType
  | OverloadType
  | NeverType

export type Signature = Type[] | null;

export abstract class TypeBase {

  public abstract kind: TypeKind;

  private solvedType?: Type;

  constructor(public node: Syntax | null) {

  }

  public get solved(): Type {
    if (this.solvedType === undefined) {
      return this as unknown as Type;
    }
    let currType: Type = this.solvedType;
    while (currType.solvedType !== undefined) {
      currType = currType.solvedType;
    }
    return this.solvedType = currType;
  }

  public set solved(newType: Type) {
    this.solvedType = newType;
  }

  public abstract applySubstitution(substitution: TypeVarSubstitution): Type;

  public abstract getFreeTypeVars(): Iterable<TypeVar>;

  public hasTypeVariable(typeVar: TypeVar): boolean {
    for (const otherTypeVar of this.getFreeTypeVars()) {
      if (otherTypeVar.varId === typeVar.varId) {
        return true;
      }
    }
    return false;
  }

  public abstract shallowClone(): Type;

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

  public *getFreeTypeVars() {
    yield this;
  }

  public shallowClone(): TypeVar {
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

export class NeverType extends TypeBase {

  public readonly kind = TypeKind.NeverType;

  public *getFreeTypeVars(): Iterable<TypeVar> {

  }

  public shallowClone(): NeverType {
    return new NeverType(this.node);
  }

  public resolve() {
    return this;
  }

  public applySubstitution(_substitution: TypeVarSubstitution): Type {
    return this;
  }

  public [prettyPrintTag](): string {
    return 'never';
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

  public *getFreeTypeVars(): Iterable<TypeVar> {
    for (const element of this.elements) {
      yield* element.getFreeTypeVars();
    }
  }

  public shallowClone(): TupleType {
    return new TupleType(this.elements, this.node);
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

  public *getFreeTypeVars(): Iterable<TypeVar> {

  }

  public shallowClone(): PrimType {
    return new PrimType(this.displayName, this.primId, this.node);
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
    public overloaded: boolean,
    node: Syntax | null = null
  ) {
    super(node);
  }

  public shallowClone(): ArrowType {
    return new ArrowType(
      this.paramTypes,
      this.returnType,
      this.overloaded,
      this.node
    )
  }

  public *getFreeTypeVars(): Iterable<TypeVar> {
    for (const paramType of this.paramTypes) {
      yield* paramType.getFreeTypeVars();
    }
    yield* this.returnType.getFreeTypeVars();
  }

  public applySubstitution(substitution: TypeVarSubstitution): Type {
    return new ArrowType(
      this.paramTypes.map(type => type.applySubstitution(substitution)),
      this.returnType.applySubstitution(substitution),
      this.overloaded,
      this.node
    )
  }

  public resolve(): Type {
    return new ArrowType(
      this.paramTypes.map(type => type.resolve()),
      this.returnType.resolve(),
      this.overloaded,
      this.node,
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
      this.declaration,
      this.node
    );
  }

  public *getFreeTypeVars(): Iterable<TypeVar> {
    for (const fieldType of this.fieldTypeMap.values()) {
      yield* fieldType.getFreeTypeVars();
    }
  }

  public hasTypeVariable(typeVar: TypeVar): boolean {
    for (const [_fieldName, fieldType] of this.fieldTypeMap) {
      if (fieldType.hasTypeVariable(typeVar)) {
        return true;
      }
    }
    return false;
  }

  public shallowClone(): RecordType {
    return new RecordType(
      this.fieldTypeMap,
      this.declaration,
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
      this.declaration,
      this.node,
    );
  }

}

//export class OverloadType extends TypeBase {

//  public readonly kind = TypeKind.OverloadType;

//  private memberTypeMap: FastStringMap<string, Type>;

//  constructor(
//    memberTypes: Iterable<[string, Type]>,
//    public declaration: RecordDeclaration,
//    node: Syntax | null = null
//  ) {
//    super(node);
//    this.memberTypeMap = new FastStringMap(memberTypes);
//  }

//  public [Symbol.iterator]() {
//    return this.memberTypeMap[Symbol.iterator]();
//  }

//  public hasMember(fieldName: string): boolean {
//    return this.memberTypeMap.has(fieldName);
//  }

//  public getMemberTypes(fieldName: string): Type {
//    if (!this.memberTypeMap.has(fieldName)) {
//      throw new Error(`Trying to get a field named ${fieldName} on a record type that does not have it.`);
//    }
//    return this.memberTypeMap.get(fieldName)!;
//  }

//  public applySubstitution(substitution: TypeVarSubstitution): OverloadType {
//    return new OverloadType(
//      [...this.memberTypeMap].map(([fieldName, fieldType]) => [fieldName, fieldType.applySubstitution(substitution)]),
//      this.declaration,
//      this.node
//    );
//  }

//  public *getTypeVars(): Iterable<TypeVar> {
//    for (const memberType of this.memberTypeMap.values()) {
//      yield* memberType.getTypeVars();
//    }
//  }

//  public deepClone(): OverloadType {
//    return new OverloadType(
//      [...this.memberTypeMap].map(([fieldName, fieldType]) => [fieldName, fieldType.deepClone()]),
//      this.declaration,
//      this.node,
//    );
//  }

//  public [prettyPrintTag](): string {
//    if (isRecordDeclaration(this.node)) {
//      return this.node.name.text;
//    }
//    return '{ ' + [...this.memberTypeMap].map(([fieldName, fieldType]) => `${fieldName}: ${fieldType[prettyPrintTag]()}`).join(', ') + ' }';
//  }

//  public resolve(): OverloadType {
//    return new OverloadType(
//      [...this.memberTypeMap].map(([fieldName, fieldType]) => [fieldName, fieldType.resolve()]),
//      this.declaration,
//      this.node,
//    );
//  }

//}

export class OverloadType extends TypeBase {

  public readonly kind = TypeKind.OverloadType;

  constructor(public typeEnv: TypeEnv, public types: Type[]) {
    super(null);
  }

  public *getFreeTypeVars(): Iterable<TypeVar> {
    for (const type of this.types) {
      yield* type.getFreeTypeVars();
    }
  }

  public applySubstitution(substitution: TypeVarSubstitution): Type {
    return new OverloadType(
      this.typeEnv,
      this.types.map(type => type.applySubstitution(substitution))
    );
  }

  public resolve(): Type {
    throw new Error(`Cannot resolve an overloaded type to a single type. The type should have been resolved.`);
  }

  public shallowClone(): OverloadType {
    return new OverloadType(this.typeEnv, this.types);
  }

  public [prettyPrintTag](): string {
    return this.types.map(type => type[prettyPrintTag]()).join(' | ');
  }

}

export class MemberType extends TypeBase {

  public readonly kind = TypeKind.MemberType;

  constructor(
    public sourceType: Type,
    public fieldName: string,
    public signature: Signature,
    public scope: TypeEnv,
    node: Syntax | null = null
  ) {
    super(node);
  }

  public [prettyPrintTag](): string {
    return this.sourceType[prettyPrintTag]() + '.' + this.fieldName;
  }

  public hasTypeVariable(typeVar: TypeVar): boolean {
    return this.sourceType.hasTypeVariable(typeVar);
  }

  public applySubstitution(substitution: TypeVarSubstitution): MemberType {
    return new MemberType(
      this.sourceType.applySubstitution(substitution),
      this.fieldName,
      this.signature,
      this.scope,
      this.node,
    );
  }

  public resolve(): Type {
    assert(this.solved !== this);
    return this.solved.resolve();
  }

  public shallowClone(): MemberType {
    return new MemberType(
      this.sourceType,
      this.fieldName,
      this.signature,
      this.scope,
      this.node,
    );
  }

  public *getFreeTypeVars(): Iterable<TypeVar> {
    yield* this.sourceType.getFreeTypeVars();
    if (this.signature !== null) {
      for (const paramType of this.signature) {
        yield* paramType.getFreeTypeVars();
      }
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

export type Scheme
  = ForallScheme

export class ForallScheme {

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
    const freeVariables = new TypeVarSet(this.type.getFreeTypeVars());
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

/**
 * Instantiate the type inside the scheme with some fresh variables that are
 * guaranteed to not occur anywhere else.
 */
function instantiate(scheme: Scheme): Type {
  const freshVars = new TypeVarSubstitution();
  for (const typeVar of scheme.typeVars) {
    freshVars.add(typeVar, new TypeVar(typeVar.node))
  }
  return scheme.type.applySubstitution(freshVars);
}

export class TypeEnv {

  private mapping = new FastMultiMap<string, Scheme>();

  constructor(public parentTypeEnv: TypeEnv | null = null) {

  }

  public set(name: string, scheme: Scheme) {
    this.mapping.add(name, scheme)
  }

  public *getSchemes(name: string): Iterable<Scheme> {
    let currEnv: TypeEnv | null = this;
    while (true) {
      yield* currEnv!.mapping.get(name);
      currEnv = currEnv.parentTypeEnv;
      if (currEnv === null) {
        break;
      }
    }
  }

  public lookup(name: string): Type | null {
    const { done, value } = this.getSchemes(name)[Symbol.iterator]().next();
    if (done) {
      return null;
    }
    return instantiate(value);
  }

  //public lookup(name: string): Type {
  //  const types = [];
  //  for (const scheme of this.getSchemes(name)) {
  //    types.push(instantiate(scheme));
  //  }
  //  return new OverloadType(this, types);
  //}

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

  public [Symbol.iterator]() {
    return this.mapping[Symbol.iterator]();
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

