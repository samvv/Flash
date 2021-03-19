
import { SyntaxKind, kindToString, Syntax, isStatement } from "./ast"
import { FastStringMap, assert } from "./util"
import { boolType, intType, isTypeAssignableTo, PrimType, stringType, Type, voidType } from "./types";

export enum ValueType {
  Int,
  Bool,
  String,
  Record,
  Void,
}

export class VoidValue {

  public readonly type = ValueType.Void;

  public getType() {
    return voidType;
  }

}

export class IntValue {

  public readonly type = ValueType.Int;

  constructor(public data: bigint) {

  }

  public getType() {
    return intType;
  }

}

export class StringValue {

  public readonly type = ValueType.String;

  constructor(public data: string) {

  }

  public getType() {
    return stringType;
  }

}

export class BoolValue {

  public readonly type = ValueType.Bool;

  constructor(public data: boolean) {

  }

  public getType() {
    return boolType;
  }

}

export class RecordValue {

  public readonly type = ValueType.Record;

  private fields: Map<string, Value>;

  constructor(
    public primType: PrimType,
    fields: Iterable<[string, Value]>,
  ) {
    this.fields = new Map(fields);
  }

  public getFields(): IterableIterator<[string, Value]> {
    return this.fields[Symbol.iterator]();
  }

  public clone(): RecordValue {
    return new RecordValue(this.primType, this.fields);
  }

  public getFieldValue(name: string): Value {
    if (!this.fields.has(name)) {
      throw new Error(`Trying to access non-existent field ${name} of a record.`);
    }
    return this.fields.get(name)!;
  }

  public addField(name: string, value: Value): void {
    this.fields.set(name, value);
  }

  public deleteField(name: string): void {
    this.fields.delete(name);
  }

  public clear(): void {
    this.fields.clear();
  }

  public getType(): Type {
    throw new Error(`Method not implemented yet`);
  }

}

export type Value
  = IntValue
  | StringValue
  | BoolValue
  | RecordValue
  | VoidValue

class Environment {

  private symbols = new FastStringMap<string, Value>();

  constructor(public parentEnv: Environment | null = null) {

  }

  public setValue(name: string, value: Value) {
    if (name in this.symbols) {
      throw new Error(`A variable with the name '${name}' already exists.`);
    }
    this.symbols.add(name, value);
  }

  public updateValue(name: string, newValue: Value) {
    if (!this.symbols.has(name)) {
      throw new Error(`Trying to update a variable '${name}' that has not been declared.`);
    }
    this.symbols.delete(name);
    this.symbols.add(name, newValue); 
  }

  public lookup(name: string) {
    let curr = this as Environment;
    while (true) {
      if (this.symbols.has(name)) {
        return curr.symbols.get(name);
      }
      if (curr.parentEnv === null) {
        break;
      }
      curr = curr.parentEnv;
    }
    throw new Error(`A variable named '${name}' was not found.`);
  }

}

function mangle(node: Syntax) {
  switch (node.kind) {
    case SyntaxKind.Identifier:
      return node.text;
    default:
      throw new Error(`Could not mangle ${kindToString(node.kind)} to a symbol name.`)
  }
}

class EvaluationError extends Error {

}

export class Evaluator {

  constructor() {

  }

  private performPatternMatch(value: Value, node: Syntax, env: Environment): boolean {

    switch (node.kind) {

      case SyntaxKind.BindPattern:
      {
        env.setValue(node.name.text, value);
        return true;
      }

      case SyntaxKind.RecordPattern:
      {
        if (!(value instanceof RecordValue)) {
          throw new EvaluationError(`A deconstructing record pattern received a value that is not a record.`);
        }
        const record = value.clone();
        for (const fieldPatt of node.fields) {
          if (fieldPatt.isRest) {
            if (fieldPatt.name !== null) {
              env.setValue(fieldPatt.name.text, record.clone());
            }
            record.clear();
          } else {
            assert(fieldPatt.name !== null);
            let isMatch = true;
            if (fieldPatt.pattern !== null) {
              isMatch = this.performPatternMatch(value.getFieldValue(fieldPatt.name!.text), fieldPatt.pattern, env);
            }
            if (!isMatch) {
              return false;
            }
            record.deleteField(fieldPatt.name!.text);
          }
        }
        return true;
      }

      case SyntaxKind.TypePattern:
      {
        const expectedType = node.typeExpr.getType();
        if (!isTypeAssignableTo(expectedType, value.getType())) {
          return false;
        }
        return false;
      }

      default:
        throw new Error(`I did not know how to match on pattern ${kindToString(node.kind)}`)

    }

  }

  public eval(node: Syntax, env: Environment = new Environment()): Value { 

    switch (node.kind) {

      case SyntaxKind.SourceFile:
      case SyntaxKind.Module:
        for (const element of node.elements) {
          if (isStatement(element)) {
            this.eval(element, env);
          }
        }
        return new VoidValue();

      case SyntaxKind.ReferenceExpression:
        return env.lookup(mangle(node.name));

      case SyntaxKind.MatchExpression:
        const value = this.eval(node.value, env);
        for (const matchArm of node.arms) {
          const matchArmEnv = new Environment(env);
          const isMatch = this.performPatternMatch(value, matchArm.pattern, matchArmEnv);
          if (isMatch) {
            return this.eval(matchArm.body, env)
          }
        }
        throw new Error(`Not a single match arm matched and no default arm provided.`);

      case SyntaxKind.ConstantExpression:
        switch (typeof(node.value)) {
          case 'boolean':
            return new BoolValue(node.value);
          case 'string':
            return new StringValue(node.value);
          case 'bigint':
            return new IntValue(node.value);
          default:
            throw new Error(`Could evaluate ConstantExpression: the expression's value was not recognised.`);
        }

      default:
        throw new Error(`Could not evaluate node ${kindToString(node.kind)}: unrecognised AST node type`)

    }

  }

}

