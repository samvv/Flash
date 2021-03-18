
import { TextSpan } from "./text"
import { Package } from "./package"
import { CompileError } from "./errors";
import { serializeTag, inspectTag, indent, assert } from "./util";
import { InspectOptionsStylized, inspect } from "util";
import { Type } from "./types";
import { TypeChecker } from "./checker";

let nextNodeId = 1;

export enum NodeFlags {
  None = 0,
  HasTypeError = 1,
  AutoImported = 2,
}

declare function isSourceFile(node: Syntax): node is SourceFile;

export abstract class Syntax {

  public id: number;

  public flags = NodeFlags.None;

  public errors: CompileError[] = [];

  public hasTypeError() {
    return (this.flags & NodeFlags.HasTypeError) > 0;
  }

  public markAsHavingTypeError() {
    this.flags |= NodeFlags.HasTypeError;
  }

  constructor(
    public span: TextSpan | null = null,
    public parentNode: Syntax | null = null,
  ) {
    this.id = nextNodeId++;
  }

  public [inspectTag](depth: number | null, options: InspectOptionsStylized) {
    const proto = Object.getPrototypeOf(this);
    if (depth !== null && depth < 0) {
      return options.stylize(`[${proto.constructor.name}]`, 'special')
    }
    const newOptions = {
      ...options,
      depth: options.depth === null ? null : options.depth!-1,
    }
    let out = `${proto.constructor.name} {\n`;
    for (const key of Object.keys(this)) {
      if (key === 'kind' || key === 'impls' || key === 'parentNode' || key === 'errors' || key === 'type' || key === 'id') {
        continue;
      }
      out += indent(`${key}: ${inspect((this as any)[key], newOptions)},\n`);
    }
    out += '}\n';
    return out;
  }

  public [serializeTag]() {
    const result: any[] = [];
    for (const key of Object.keys(this)) {
      if (key === 'kind' || key === 'impls' || key === 'span' || key === 'parentNode' || key === 'errors' || key === 'type' || key === 'id') {
        continue;
      }
      result.push((this as any)[key]);
    }
    result.push(this.span);
    return result;
  }

  public getSourceFile(): SourceFile {
    let currNode: Syntax | null = this as any;
    do {
      if (isSourceFile(currNode!)) {
        return currNode;
      }
      currNode = currNode!.parentNode;
    } while (currNode !== null);
    throw new Error(`Could not get the source file an AST object belonged to.`);
  }

  public getType(): Type {
    const sourceFile = this.getSourceFile();
    assert(sourceFile.checker !== undefined);
    return sourceFile.checker.getTypeOfNode(this as any);
  }

}

export interface Token extends Syntax {}

export interface EndOfFile extends Token {}

export interface StringLiteral extends Token {
  value: string,
}

export interface IntegerLiteral extends Token {
  value: bigint,
}

export interface Identifier extends Token {
  text: string,
}

export type Symbol
  = Identifier
  | OperatorLike

export type OperatorLike 
  = GtSign
  | LtSign
  | ExMark
  | VBar
  | Operator

export interface Operator extends Token {
  text: string,
}

export interface Assignment extends Token {
  operator: string | null,
}

export interface Comma      extends Token {}
export interface Semi       extends Token {}
export interface Colon      extends Token {}
export interface ColonColon extends Token {}
export interface Dot        extends Token {}
export interface DotDot     extends Token {}
export interface RArrow     extends Token {}
export interface RArrowAlt  extends Token {}
export interface LArrow     extends Token {}
export interface EqSign     extends Token {}

export interface GtSign    extends Token {}
export interface ExMark    extends Token {}
export interface LtSign    extends Token {}
export interface VBar      extends Token {}

export interface Keyword {}

export interface ElseKeyword    extends Token {}
export interface IfKeyword      extends Token {}
export interface WhereKeyword   extends Token {}
export interface QuoteKeyword   extends Token {}
export interface FnKeyword      extends Token {}
export interface ForeignKeyword extends Token {}
export interface ForKeyword     extends Token {}
export interface LetKeyword     extends Token {}
export interface ReturnKeyword  extends Token {}
export interface LoopKeyword    extends Token {}
export interface YieldKeyword   extends Token {}
export interface MatchKeyword   extends Token {}
export interface ImportKeyword  extends Token {}
export interface ExportKeyword  extends Token {}
export interface PubKeyword     extends Token {}
export interface ModKeyword     extends Token {}
export interface MutKeyword     extends Token {}
export interface EnumKeyword    extends Token {}
export interface StructKeyword  extends Token {}
export interface TypeKeyword    extends Token {}
export interface TraitKeyword   extends Token {}
export interface ImplKeyword    extends Token {}

export interface Punctuated extends Token {
  text: string,
}

export interface Parenthesized extends Punctuated {}
export interface Braced extends Punctuated {}
export interface Bracketed extends Punctuated {}

export class SourceFile extends Syntax {

  public checker?: TypeChecker;

  constructor(
    public elements: SourceElement[],
    public pkg: Package,
    span: TextSpan | null = null,
    parentNode: Syntax | null = null,
  ) {
    super(span, parentNode);
  }

}

export interface QualName extends Syntax {
  isAbsolute: boolean,
  modulePath: Identifier[],
  name: Symbol,
}

export interface TypeExpression extends Syntax {}

export interface TypeOfExpression extends TypeExpression {
  expression: Expression,
}

export interface ReferenceTypeExpression extends TypeExpression {
  name: QualName,
  typeArgs: TypeExpression[] | null,
}

export interface FunctionTypeExpression extends TypeExpression {
  params: Parameter[],
  returnTypeExpr: TypeExpression | null,
}

export interface LiftedTypeExpression extends TypeExpression {
  expression: Expression, 
}

export interface TypeParameter extends Syntax {
  index: number,
  name: Identifier,
  typeExpr: TypeExpression | null,
  defaultType: TypeExpression | null,
}

export interface Pattern extends Syntax {}

export interface BindPattern extends Pattern {
  name: Identifier,
}

export interface TypePattern extends Pattern {
  typeExpr: TypeExpression,
  nestedPattern: Pattern,
}

export interface ExpressionPattern extends Pattern {
  expression: Expression,
}

export interface TuplePatternElement extends Syntax {
  index: number,
  pattern: Pattern,
}

export interface TuplePattern extends Pattern {
  elements: TuplePatternElement[],
}

export interface RecordFieldPattern extends Syntax {
  isRest: boolean,
  name: Identifier | null,
  pattern: Pattern | null,
}

export interface RecordPattern extends Pattern {
  name: TypeExpression,
  fields: RecordFieldPattern[],
}

export interface Expression extends Syntax {}

export interface RecordExpression extends Expression {
    typeRef: ReferenceTypeExpression;
    fields: RecordExpressionElement[];
}

export interface RecordExpressionElement extends Syntax {}

export interface RecordFieldValue extends RecordExpressionElement {
  name: Identifier;
  value: Expression | null;
}

export interface QuoteExpression extends Expression {
  tokens: Array<Token | Expression>,
}

export interface TupleExpression extends Expression {
  elements: Expression[],
}

export interface ReferenceExpression extends Expression {
  name: QualName,
}

export interface MemberExpression extends Expression {
  expression: Expression,
  path: Identifier[],
}

export interface FunctionExpression extends Expression {
  params: Parameter[],
  returnTypeExpr: TypeExpression | null,
  body: FunctionBodyElement[] | null,
  expression: Expression | null;
}

export interface CallExpression extends Expression {
  operator: Expression,
  operands: Expression[],
}

export interface YieldExpression extends Expression {
  value: Expression,
}

export interface MatchArm extends Syntax {
  pattern: Pattern,
  body: Expression,
}

export interface MatchExpression extends Expression {
  value: Expression,
  arms: MatchArm[],
}

export interface Statement extends Syntax {}

export interface CaseStatementCase extends Syntax {
  test: Expression | null,
  body: FunctionBodyElement[],
}

export interface CaseStatement extends Statement {
  cases: CaseStatementCase[],
}

export interface BlockExpression extends Expression {
  elements: FunctionBodyElement[],
}

export interface ConstantExpression extends Expression {
  value: string | number | bigint | boolean,
}

export interface ReturnStatement extends Statement {
  value: Expression | null,
}

export interface ConditionalCase extends Syntax {
  test: Expression | null,
  body: FunctionBodyElement[],
}

export interface ConditionalStatement extends Statement {
  cases: ConditionalCase[],
}

export interface ResumeStatement extends Statement {
  value: Expression,
}

export interface ExpressionStatement extends Statement {
  expression: Expression,
}

export interface AssignStatement extends Statement {
  lhs: Pattern;
  rhs: Expression;
}

export interface LoopStatement extends Statement {
  elements: FunctionBodyElement[],
}

export interface Parameter extends Syntax {
  index: number,
  bindings: Pattern,
  typeExpr: TypeExpression | null,
  defaultValue: Expression | null,
}

export enum Modifiers {
  IsMutable   = 0x1,
  IsPublic    = 0x2,
}

export interface Module extends Syntax {
  modifiers: Modifiers,
  name: Identifier[],
  elements: SourceElement[],
}

export interface DeclarationLike extends Syntax {}

export interface FunctionDeclaration extends DeclarationLike {
  modifiers: Modifiers,
  target: string,
  name: Symbol,
  params: Parameter[],
  returnTypeExpr: TypeExpression | null,
  typeParams: TypeParameter[] | null,
  body: FunctionBodyElement[] | null,
}

export type FunctionBody = Array<FunctionBodyElement>;

export type FunctionBodyElement
  = Statement
  | MacroCall
  | Declaration

export interface VariableDeclaration extends DeclarationLike {
  modifiers: Modifiers,
  bindings: Pattern,
  typeExpr: TypeExpression | null,
  value: Expression | null,
}

export interface ImportSymbol extends Syntax {}

export interface PlainImportSymbol extends ImportSymbol {
  remote: QualName,
  local: Symbol,
}

export interface ImportDeclaration extends Syntax {
  modifiers: Modifiers,
  file: StringLiteral,
  symbols: ImportSymbol[] | null,
}

export interface ExportSymbol extends Syntax {}

export interface PlainExportSymbol extends ExportSymbol {
  local: QualName,
  remote: Symbol,
}

export interface ExportDeclaration extends Syntax {
  file: string,
  symbols: ExportSymbol[] | null,
}

export interface TraitOrImplElement {}

export interface TraitDeclaration extends DeclarationLike {
  modifiers: Modifiers,
  typeParams: TypeParameter[] | null,
  name: Identifier,
  typeBoundExpr: TypeExpression | null,
  elements: TraitOrImplElement[] | null,
}

export interface ImplDeclaration extends DeclarationLike {
  modifiers: Modifiers,
  typeParams: TypeParameter[] | null,
  traitTypeExpr: ReferenceTypeExpression,
  typeExpr: ReferenceTypeExpression,
  elements: TraitOrImplElement[],
}

export interface TypeAliasDeclaration extends DeclarationLike {
  modifiers: Modifiers,
  name: Identifier,
  typeParams: TypeParameter[] | null,
  typeExpr: TypeExpression,
}

export type EnumDeclarationElement
  = Identifier

export interface EnumDeclaration extends DeclarationLike {
  modifiers: Modifiers,
  name: Identifier;
  typeParams: TypeParameter[] | null;
  members: EnumDeclarationElement[];
}

export type RecordDeclartionElement
  = MacroCall
  | RecordDeclarationField

export interface RecordDeclarationField extends Syntax {
  name: Identifier,
  typeExpr: TypeExpression,
}

export interface RecordDeclaration extends DeclarationLike {
  modifiers: Modifiers,
  name: Identifier,
  typeParms: TypeParameter[] | null,
  members: RecordDeclartionElement[] | null,
}

export interface MacroCall extends Syntax {
  name: Identifier,
  text: string,
}

export type Declaration
  = FunctionDeclaration
  | VariableDeclaration

export type TypeDeclaration
  = RecordDeclaration
  | EnumDeclaration
  | TypeAliasDeclaration

export type SourceElement
  = DeclarationLike
  | Statement
  | Module
  | MacroCall
  | ImportDeclaration
  | ExportDeclaration

