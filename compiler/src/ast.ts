export function isSyntax(value: any): value is Syntax { return typeof value === "object" && value !== null && value instanceof SyntaxBase; }

import { TextSpan } from "./text";

import { Package } from "./package";

import { CompileError } from "./errors";

import { serializeTag, inspectTag, indent, assert } from "./util";

import { InspectOptionsStylized, inspect } from "util";

import { Type } from "./types";

import { TypeChecker } from "./checker";

let nextNodeId = 1;

export enum NodeFlags {
    None = 0,
    HasTypeError = 1,
    AutoImported = 2
}

export abstract class SyntaxBase {
    public id: number;
    public flags = NodeFlags.None;
    public errors: CompileError[] = [];
    public hasTypeError() {
        return (this.flags & NodeFlags.HasTypeError) > 0;
    }
    public markAsHavingTypeError() {
        this.flags |= NodeFlags.HasTypeError;
    }
    constructor(public span: TextSpan | null = null, public parentNode: Syntax | null = null) {
        this.id = nextNodeId++;
    }
    public [inspectTag](depth: number | null, options: InspectOptionsStylized) {
        const proto = Object.getPrototypeOf(this);
        if (depth !== null && depth < 0) {
            return options.stylize(`[${proto.constructor.name}]`, 'special');
        }
        const newOptions = {
            ...options,
            depth: options.depth === null ? null : options.depth! - 1,
        };
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
    getParentOfKind<K extends SyntaxKind>(kind: K): (Syntax & {
        kind: K;
    }) | null { var currNode = this.parentNode; while (currNode !== null) {
        if (currNode.kind === kind)
            return <any>currNode;
        currNode = currNode.parentNode;
    } return null; }
}

export class TokenBase extends SyntaxBase {
}

export type Token = EndOfFile | StringLiteral | IntegerLiteral | Identifier | Operator | Assignment | Comma | Semi | Colon | ColonColon | Dot | DotDot | RArrow | RArrowAlt | LArrow | EqSign | GtSign | ExMark | LtSign | VBar | ElseKeyword | IfKeyword | WhereKeyword | QuoteKeyword | FnKeyword | ForeignKeyword | ForKeyword | LetKeyword | ReturnKeyword | LoopKeyword | YieldKeyword | MatchKeyword | ImportKeyword | ExportKeyword | PubKeyword | ModKeyword | MutKeyword | EnumKeyword | StructKeyword | TypeKeyword | TraitKeyword | ImplKeyword | Parenthesized | Braced | Bracketed;

export function isToken(value: Syntax): value is Token { return value.kind === SyntaxKind.EndOfFile || value.kind === SyntaxKind.StringLiteral || value.kind === SyntaxKind.IntegerLiteral || value.kind === SyntaxKind.Identifier || value.kind === SyntaxKind.Operator || value.kind === SyntaxKind.Assignment || value.kind === SyntaxKind.Comma || value.kind === SyntaxKind.Semi || value.kind === SyntaxKind.Colon || value.kind === SyntaxKind.ColonColon || value.kind === SyntaxKind.Dot || value.kind === SyntaxKind.DotDot || value.kind === SyntaxKind.RArrow || value.kind === SyntaxKind.RArrowAlt || value.kind === SyntaxKind.LArrow || value.kind === SyntaxKind.EqSign || value.kind === SyntaxKind.GtSign || value.kind === SyntaxKind.ExMark || value.kind === SyntaxKind.LtSign || value.kind === SyntaxKind.VBar || value.kind === SyntaxKind.ElseKeyword || value.kind === SyntaxKind.IfKeyword || value.kind === SyntaxKind.WhereKeyword || value.kind === SyntaxKind.QuoteKeyword || value.kind === SyntaxKind.FnKeyword || value.kind === SyntaxKind.ForeignKeyword || value.kind === SyntaxKind.ForKeyword || value.kind === SyntaxKind.LetKeyword || value.kind === SyntaxKind.ReturnKeyword || value.kind === SyntaxKind.LoopKeyword || value.kind === SyntaxKind.YieldKeyword || value.kind === SyntaxKind.MatchKeyword || value.kind === SyntaxKind.ImportKeyword || value.kind === SyntaxKind.ExportKeyword || value.kind === SyntaxKind.PubKeyword || value.kind === SyntaxKind.ModKeyword || value.kind === SyntaxKind.MutKeyword || value.kind === SyntaxKind.EnumKeyword || value.kind === SyntaxKind.StructKeyword || value.kind === SyntaxKind.TypeKeyword || value.kind === SyntaxKind.TraitKeyword || value.kind === SyntaxKind.ImplKeyword || value.kind === SyntaxKind.Parenthesized || value.kind === SyntaxKind.Braced || value.kind === SyntaxKind.Bracketed; }

export class EndOfFile extends TokenBase {
    readonly kind = SyntaxKind.EndOfFile;
    parentNode: null | EndOfFileParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<EndOfFileChild> { }
}

export type EndOfFileParent = QuoteExpression | never;

export type EndOfFileChild = never;

export function isEndOfFile(value: any): value is EndOfFile { return value.kind === SyntaxKind.EndOfFile; }

export function createEndOfFile(span: TextSpan | null = null, parentNode: Syntax | null = null): EndOfFile { return new EndOfFile(span, parentNode); }

export class StringLiteral extends TokenBase {
    readonly kind = SyntaxKind.StringLiteral;
    parentNode: null | StringLiteralParent = null;
    constructor(public value: string, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<StringLiteralChild> { }
}

export type StringLiteralParent = QuoteExpression | ImportDeclaration | never;

export type StringLiteralChild = never;

export function isStringLiteral(value: any): value is StringLiteral { return value.kind === SyntaxKind.StringLiteral; }

export function createStringLiteral(value: string, span: TextSpan | null = null, parentNode: Syntax | null = null): StringLiteral { return new StringLiteral(value, span, parentNode); }

export class IntegerLiteral extends TokenBase {
    readonly kind = SyntaxKind.IntegerLiteral;
    parentNode: null | IntegerLiteralParent = null;
    constructor(public value: bigint, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<IntegerLiteralChild> { }
}

export type IntegerLiteralParent = QuoteExpression | never;

export type IntegerLiteralChild = never;

export function isIntegerLiteral(value: any): value is IntegerLiteral { return value.kind === SyntaxKind.IntegerLiteral; }

export function createIntegerLiteral(value: bigint, span: TextSpan | null = null, parentNode: Syntax | null = null): IntegerLiteral { return new IntegerLiteral(value, span, parentNode); }

export class Identifier extends TokenBase {
    readonly kind = SyntaxKind.Identifier;
    parentNode: null | IdentifierParent = null;
    constructor(public text: string, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<IdentifierChild> { }
}

export type IdentifierParent = QualName | TypeParameter | BindPattern | RecordPatternField | RecordFieldValue | QuoteExpression | MemberExpression | Module | TraitDeclaration | TypeAliasDeclaration | EnumDeclaration | RecordDeclarationField | RecordDeclaration | MacroCall | never;

export type IdentifierChild = never;

export function isIdentifier(value: any): value is Identifier { return value.kind === SyntaxKind.Identifier; }

export function createIdentifier(text: string, span: TextSpan | null = null, parentNode: Syntax | null = null): Identifier { return new Identifier(text, span, parentNode); }

export type Symbol = Identifier | OperatorLike;

export function isSymbol(value: Syntax): value is Symbol { return value.kind === SyntaxKind.Identifier || value.kind === SyntaxKind.GtSign || value.kind === SyntaxKind.LtSign || value.kind === SyntaxKind.ExMark || value.kind === SyntaxKind.VBar || value.kind === SyntaxKind.Operator; }

export type OperatorLike = GtSign | LtSign | ExMark | VBar | Operator;

export function isOperatorLike(value: Syntax): value is OperatorLike { return value.kind === SyntaxKind.GtSign || value.kind === SyntaxKind.LtSign || value.kind === SyntaxKind.ExMark || value.kind === SyntaxKind.VBar || value.kind === SyntaxKind.Operator; }

export class Operator extends TokenBase {
    readonly kind = SyntaxKind.Operator;
    parentNode: null | OperatorParent = null;
    constructor(public text: string, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<OperatorChild> { }
}

export type OperatorParent = QuoteExpression | never;

export type OperatorChild = never;

export function isOperator(value: any): value is Operator { return value.kind === SyntaxKind.Operator; }

export function createOperator(text: string, span: TextSpan | null = null, parentNode: Syntax | null = null): Operator { return new Operator(text, span, parentNode); }

export class Assignment extends TokenBase {
    readonly kind = SyntaxKind.Assignment;
    parentNode: null | AssignmentParent = null;
    constructor(public operator: string | null, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<AssignmentChild> { }
}

export type AssignmentParent = QuoteExpression | never;

export type AssignmentChild = never;

export function isAssignment(value: any): value is Assignment { return value.kind === SyntaxKind.Assignment; }

export function createAssignment(operator: string | null, span: TextSpan | null = null, parentNode: Syntax | null = null): Assignment { return new Assignment(operator, span, parentNode); }

export class Comma extends TokenBase {
    readonly kind = SyntaxKind.Comma;
    parentNode: null | CommaParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<CommaChild> { }
}

export type CommaParent = QuoteExpression | never;

export type CommaChild = never;

export function isComma(value: any): value is Comma { return value.kind === SyntaxKind.Comma; }

export function createComma(span: TextSpan | null = null, parentNode: Syntax | null = null): Comma { return new Comma(span, parentNode); }

export class Semi extends TokenBase {
    readonly kind = SyntaxKind.Semi;
    parentNode: null | SemiParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<SemiChild> { }
}

export type SemiParent = QuoteExpression | never;

export type SemiChild = never;

export function isSemi(value: any): value is Semi { return value.kind === SyntaxKind.Semi; }

export function createSemi(span: TextSpan | null = null, parentNode: Syntax | null = null): Semi { return new Semi(span, parentNode); }

export class Colon extends TokenBase {
    readonly kind = SyntaxKind.Colon;
    parentNode: null | ColonParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ColonChild> { }
}

export type ColonParent = QuoteExpression | never;

export type ColonChild = never;

export function isColon(value: any): value is Colon { return value.kind === SyntaxKind.Colon; }

export function createColon(span: TextSpan | null = null, parentNode: Syntax | null = null): Colon { return new Colon(span, parentNode); }

export class ColonColon extends TokenBase {
    readonly kind = SyntaxKind.ColonColon;
    parentNode: null | ColonColonParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ColonColonChild> { }
}

export type ColonColonParent = QuoteExpression | never;

export type ColonColonChild = never;

export function isColonColon(value: any): value is ColonColon { return value.kind === SyntaxKind.ColonColon; }

export function createColonColon(span: TextSpan | null = null, parentNode: Syntax | null = null): ColonColon { return new ColonColon(span, parentNode); }

export class Dot extends TokenBase {
    readonly kind = SyntaxKind.Dot;
    parentNode: null | DotParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<DotChild> { }
}

export type DotParent = QuoteExpression | never;

export type DotChild = never;

export function isDot(value: any): value is Dot { return value.kind === SyntaxKind.Dot; }

export function createDot(span: TextSpan | null = null, parentNode: Syntax | null = null): Dot { return new Dot(span, parentNode); }

export class DotDot extends TokenBase {
    readonly kind = SyntaxKind.DotDot;
    parentNode: null | DotDotParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<DotDotChild> { }
}

export type DotDotParent = QuoteExpression | never;

export type DotDotChild = never;

export function isDotDot(value: any): value is DotDot { return value.kind === SyntaxKind.DotDot; }

export function createDotDot(span: TextSpan | null = null, parentNode: Syntax | null = null): DotDot { return new DotDot(span, parentNode); }

export class RArrow extends TokenBase {
    readonly kind = SyntaxKind.RArrow;
    parentNode: null | RArrowParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<RArrowChild> { }
}

export type RArrowParent = QuoteExpression | never;

export type RArrowChild = never;

export function isRArrow(value: any): value is RArrow { return value.kind === SyntaxKind.RArrow; }

export function createRArrow(span: TextSpan | null = null, parentNode: Syntax | null = null): RArrow { return new RArrow(span, parentNode); }

export class RArrowAlt extends TokenBase {
    readonly kind = SyntaxKind.RArrowAlt;
    parentNode: null | RArrowAltParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<RArrowAltChild> { }
}

export type RArrowAltParent = QuoteExpression | never;

export type RArrowAltChild = never;

export function isRArrowAlt(value: any): value is RArrowAlt { return value.kind === SyntaxKind.RArrowAlt; }

export function createRArrowAlt(span: TextSpan | null = null, parentNode: Syntax | null = null): RArrowAlt { return new RArrowAlt(span, parentNode); }

export class LArrow extends TokenBase {
    readonly kind = SyntaxKind.LArrow;
    parentNode: null | LArrowParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<LArrowChild> { }
}

export type LArrowParent = QuoteExpression | never;

export type LArrowChild = never;

export function isLArrow(value: any): value is LArrow { return value.kind === SyntaxKind.LArrow; }

export function createLArrow(span: TextSpan | null = null, parentNode: Syntax | null = null): LArrow { return new LArrow(span, parentNode); }

export class EqSign extends TokenBase {
    readonly kind = SyntaxKind.EqSign;
    parentNode: null | EqSignParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<EqSignChild> { }
}

export type EqSignParent = QuoteExpression | never;

export type EqSignChild = never;

export function isEqSign(value: any): value is EqSign { return value.kind === SyntaxKind.EqSign; }

export function createEqSign(span: TextSpan | null = null, parentNode: Syntax | null = null): EqSign { return new EqSign(span, parentNode); }

export class GtSign extends TokenBase {
    readonly kind = SyntaxKind.GtSign;
    parentNode: null | GtSignParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<GtSignChild> { }
}

export type GtSignParent = QuoteExpression | never;

export type GtSignChild = never;

export function isGtSign(value: any): value is GtSign { return value.kind === SyntaxKind.GtSign; }

export function createGtSign(span: TextSpan | null = null, parentNode: Syntax | null = null): GtSign { return new GtSign(span, parentNode); }

export class ExMark extends TokenBase {
    readonly kind = SyntaxKind.ExMark;
    parentNode: null | ExMarkParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ExMarkChild> { }
}

export type ExMarkParent = QuoteExpression | never;

export type ExMarkChild = never;

export function isExMark(value: any): value is ExMark { return value.kind === SyntaxKind.ExMark; }

export function createExMark(span: TextSpan | null = null, parentNode: Syntax | null = null): ExMark { return new ExMark(span, parentNode); }

export class LtSign extends TokenBase {
    readonly kind = SyntaxKind.LtSign;
    parentNode: null | LtSignParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<LtSignChild> { }
}

export type LtSignParent = QuoteExpression | never;

export type LtSignChild = never;

export function isLtSign(value: any): value is LtSign { return value.kind === SyntaxKind.LtSign; }

export function createLtSign(span: TextSpan | null = null, parentNode: Syntax | null = null): LtSign { return new LtSign(span, parentNode); }

export class VBar extends TokenBase {
    readonly kind = SyntaxKind.VBar;
    parentNode: null | VBarParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<VBarChild> { }
}

export type VBarParent = QuoteExpression | never;

export type VBarChild = never;

export function isVBar(value: any): value is VBar { return value.kind === SyntaxKind.VBar; }

export function createVBar(span: TextSpan | null = null, parentNode: Syntax | null = null): VBar { return new VBar(span, parentNode); }

export interface Keyword {
}

export class ElseKeyword extends TokenBase {
    readonly kind = SyntaxKind.ElseKeyword;
    parentNode: null | ElseKeywordParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ElseKeywordChild> { }
}

export type ElseKeywordParent = QuoteExpression | never;

export type ElseKeywordChild = never;

export function isElseKeyword(value: any): value is ElseKeyword { return value.kind === SyntaxKind.ElseKeyword; }

export function createElseKeyword(span: TextSpan | null = null, parentNode: Syntax | null = null): ElseKeyword { return new ElseKeyword(span, parentNode); }

export class IfKeyword extends TokenBase {
    readonly kind = SyntaxKind.IfKeyword;
    parentNode: null | IfKeywordParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<IfKeywordChild> { }
}

export type IfKeywordParent = QuoteExpression | never;

export type IfKeywordChild = never;

export function isIfKeyword(value: any): value is IfKeyword { return value.kind === SyntaxKind.IfKeyword; }

export function createIfKeyword(span: TextSpan | null = null, parentNode: Syntax | null = null): IfKeyword { return new IfKeyword(span, parentNode); }

export class WhereKeyword extends TokenBase {
    readonly kind = SyntaxKind.WhereKeyword;
    parentNode: null | WhereKeywordParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<WhereKeywordChild> { }
}

export type WhereKeywordParent = QuoteExpression | never;

export type WhereKeywordChild = never;

export function isWhereKeyword(value: any): value is WhereKeyword { return value.kind === SyntaxKind.WhereKeyword; }

export function createWhereKeyword(span: TextSpan | null = null, parentNode: Syntax | null = null): WhereKeyword { return new WhereKeyword(span, parentNode); }

export class QuoteKeyword extends TokenBase {
    readonly kind = SyntaxKind.QuoteKeyword;
    parentNode: null | QuoteKeywordParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<QuoteKeywordChild> { }
}

export type QuoteKeywordParent = QuoteExpression | never;

export type QuoteKeywordChild = never;

export function isQuoteKeyword(value: any): value is QuoteKeyword { return value.kind === SyntaxKind.QuoteKeyword; }

export function createQuoteKeyword(span: TextSpan | null = null, parentNode: Syntax | null = null): QuoteKeyword { return new QuoteKeyword(span, parentNode); }

export class FnKeyword extends TokenBase {
    readonly kind = SyntaxKind.FnKeyword;
    parentNode: null | FnKeywordParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<FnKeywordChild> { }
}

export type FnKeywordParent = QuoteExpression | never;

export type FnKeywordChild = never;

export function isFnKeyword(value: any): value is FnKeyword { return value.kind === SyntaxKind.FnKeyword; }

export function createFnKeyword(span: TextSpan | null = null, parentNode: Syntax | null = null): FnKeyword { return new FnKeyword(span, parentNode); }

export class ForeignKeyword extends TokenBase {
    readonly kind = SyntaxKind.ForeignKeyword;
    parentNode: null | ForeignKeywordParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ForeignKeywordChild> { }
}

export type ForeignKeywordParent = QuoteExpression | never;

export type ForeignKeywordChild = never;

export function isForeignKeyword(value: any): value is ForeignKeyword { return value.kind === SyntaxKind.ForeignKeyword; }

export function createForeignKeyword(span: TextSpan | null = null, parentNode: Syntax | null = null): ForeignKeyword { return new ForeignKeyword(span, parentNode); }

export class ForKeyword extends TokenBase {
    readonly kind = SyntaxKind.ForKeyword;
    parentNode: null | ForKeywordParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ForKeywordChild> { }
}

export type ForKeywordParent = QuoteExpression | never;

export type ForKeywordChild = never;

export function isForKeyword(value: any): value is ForKeyword { return value.kind === SyntaxKind.ForKeyword; }

export function createForKeyword(span: TextSpan | null = null, parentNode: Syntax | null = null): ForKeyword { return new ForKeyword(span, parentNode); }

export class LetKeyword extends TokenBase {
    readonly kind = SyntaxKind.LetKeyword;
    parentNode: null | LetKeywordParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<LetKeywordChild> { }
}

export type LetKeywordParent = QuoteExpression | never;

export type LetKeywordChild = never;

export function isLetKeyword(value: any): value is LetKeyword { return value.kind === SyntaxKind.LetKeyword; }

export function createLetKeyword(span: TextSpan | null = null, parentNode: Syntax | null = null): LetKeyword { return new LetKeyword(span, parentNode); }

export class ReturnKeyword extends TokenBase {
    readonly kind = SyntaxKind.ReturnKeyword;
    parentNode: null | ReturnKeywordParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ReturnKeywordChild> { }
}

export type ReturnKeywordParent = QuoteExpression | never;

export type ReturnKeywordChild = never;

export function isReturnKeyword(value: any): value is ReturnKeyword { return value.kind === SyntaxKind.ReturnKeyword; }

export function createReturnKeyword(span: TextSpan | null = null, parentNode: Syntax | null = null): ReturnKeyword { return new ReturnKeyword(span, parentNode); }

export class LoopKeyword extends TokenBase {
    readonly kind = SyntaxKind.LoopKeyword;
    parentNode: null | LoopKeywordParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<LoopKeywordChild> { }
}

export type LoopKeywordParent = QuoteExpression | never;

export type LoopKeywordChild = never;

export function isLoopKeyword(value: any): value is LoopKeyword { return value.kind === SyntaxKind.LoopKeyword; }

export function createLoopKeyword(span: TextSpan | null = null, parentNode: Syntax | null = null): LoopKeyword { return new LoopKeyword(span, parentNode); }

export class YieldKeyword extends TokenBase {
    readonly kind = SyntaxKind.YieldKeyword;
    parentNode: null | YieldKeywordParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<YieldKeywordChild> { }
}

export type YieldKeywordParent = QuoteExpression | never;

export type YieldKeywordChild = never;

export function isYieldKeyword(value: any): value is YieldKeyword { return value.kind === SyntaxKind.YieldKeyword; }

export function createYieldKeyword(span: TextSpan | null = null, parentNode: Syntax | null = null): YieldKeyword { return new YieldKeyword(span, parentNode); }

export class MatchKeyword extends TokenBase {
    readonly kind = SyntaxKind.MatchKeyword;
    parentNode: null | MatchKeywordParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<MatchKeywordChild> { }
}

export type MatchKeywordParent = QuoteExpression | never;

export type MatchKeywordChild = never;

export function isMatchKeyword(value: any): value is MatchKeyword { return value.kind === SyntaxKind.MatchKeyword; }

export function createMatchKeyword(span: TextSpan | null = null, parentNode: Syntax | null = null): MatchKeyword { return new MatchKeyword(span, parentNode); }

export class ImportKeyword extends TokenBase {
    readonly kind = SyntaxKind.ImportKeyword;
    parentNode: null | ImportKeywordParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ImportKeywordChild> { }
}

export type ImportKeywordParent = QuoteExpression | never;

export type ImportKeywordChild = never;

export function isImportKeyword(value: any): value is ImportKeyword { return value.kind === SyntaxKind.ImportKeyword; }

export function createImportKeyword(span: TextSpan | null = null, parentNode: Syntax | null = null): ImportKeyword { return new ImportKeyword(span, parentNode); }

export class ExportKeyword extends TokenBase {
    readonly kind = SyntaxKind.ExportKeyword;
    parentNode: null | ExportKeywordParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ExportKeywordChild> { }
}

export type ExportKeywordParent = QuoteExpression | never;

export type ExportKeywordChild = never;

export function isExportKeyword(value: any): value is ExportKeyword { return value.kind === SyntaxKind.ExportKeyword; }

export function createExportKeyword(span: TextSpan | null = null, parentNode: Syntax | null = null): ExportKeyword { return new ExportKeyword(span, parentNode); }

export class PubKeyword extends TokenBase {
    readonly kind = SyntaxKind.PubKeyword;
    parentNode: null | PubKeywordParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<PubKeywordChild> { }
}

export type PubKeywordParent = QuoteExpression | never;

export type PubKeywordChild = never;

export function isPubKeyword(value: any): value is PubKeyword { return value.kind === SyntaxKind.PubKeyword; }

export function createPubKeyword(span: TextSpan | null = null, parentNode: Syntax | null = null): PubKeyword { return new PubKeyword(span, parentNode); }

export class ModKeyword extends TokenBase {
    readonly kind = SyntaxKind.ModKeyword;
    parentNode: null | ModKeywordParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ModKeywordChild> { }
}

export type ModKeywordParent = QuoteExpression | never;

export type ModKeywordChild = never;

export function isModKeyword(value: any): value is ModKeyword { return value.kind === SyntaxKind.ModKeyword; }

export function createModKeyword(span: TextSpan | null = null, parentNode: Syntax | null = null): ModKeyword { return new ModKeyword(span, parentNode); }

export class MutKeyword extends TokenBase {
    readonly kind = SyntaxKind.MutKeyword;
    parentNode: null | MutKeywordParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<MutKeywordChild> { }
}

export type MutKeywordParent = QuoteExpression | never;

export type MutKeywordChild = never;

export function isMutKeyword(value: any): value is MutKeyword { return value.kind === SyntaxKind.MutKeyword; }

export function createMutKeyword(span: TextSpan | null = null, parentNode: Syntax | null = null): MutKeyword { return new MutKeyword(span, parentNode); }

export class EnumKeyword extends TokenBase {
    readonly kind = SyntaxKind.EnumKeyword;
    parentNode: null | EnumKeywordParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<EnumKeywordChild> { }
}

export type EnumKeywordParent = QuoteExpression | never;

export type EnumKeywordChild = never;

export function isEnumKeyword(value: any): value is EnumKeyword { return value.kind === SyntaxKind.EnumKeyword; }

export function createEnumKeyword(span: TextSpan | null = null, parentNode: Syntax | null = null): EnumKeyword { return new EnumKeyword(span, parentNode); }

export class StructKeyword extends TokenBase {
    readonly kind = SyntaxKind.StructKeyword;
    parentNode: null | StructKeywordParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<StructKeywordChild> { }
}

export type StructKeywordParent = QuoteExpression | never;

export type StructKeywordChild = never;

export function isStructKeyword(value: any): value is StructKeyword { return value.kind === SyntaxKind.StructKeyword; }

export function createStructKeyword(span: TextSpan | null = null, parentNode: Syntax | null = null): StructKeyword { return new StructKeyword(span, parentNode); }

export class TypeKeyword extends TokenBase {
    readonly kind = SyntaxKind.TypeKeyword;
    parentNode: null | TypeKeywordParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<TypeKeywordChild> { }
}

export type TypeKeywordParent = QuoteExpression | never;

export type TypeKeywordChild = never;

export function isTypeKeyword(value: any): value is TypeKeyword { return value.kind === SyntaxKind.TypeKeyword; }

export function createTypeKeyword(span: TextSpan | null = null, parentNode: Syntax | null = null): TypeKeyword { return new TypeKeyword(span, parentNode); }

export class TraitKeyword extends TokenBase {
    readonly kind = SyntaxKind.TraitKeyword;
    parentNode: null | TraitKeywordParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<TraitKeywordChild> { }
}

export type TraitKeywordParent = QuoteExpression | never;

export type TraitKeywordChild = never;

export function isTraitKeyword(value: any): value is TraitKeyword { return value.kind === SyntaxKind.TraitKeyword; }

export function createTraitKeyword(span: TextSpan | null = null, parentNode: Syntax | null = null): TraitKeyword { return new TraitKeyword(span, parentNode); }

export class ImplKeyword extends TokenBase {
    readonly kind = SyntaxKind.ImplKeyword;
    parentNode: null | ImplKeywordParent = null;
    constructor(span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ImplKeywordChild> { }
}

export type ImplKeywordParent = QuoteExpression | never;

export type ImplKeywordChild = never;

export function isImplKeyword(value: any): value is ImplKeyword { return value.kind === SyntaxKind.ImplKeyword; }

export function createImplKeyword(span: TextSpan | null = null, parentNode: Syntax | null = null): ImplKeyword { return new ImplKeyword(span, parentNode); }

export class PunctuatedBase extends TokenBase {
}

export type Punctuated = Parenthesized | Braced | Bracketed;

export function isPunctuated(value: Syntax): value is Punctuated { return value.kind === SyntaxKind.Parenthesized || value.kind === SyntaxKind.Braced || value.kind === SyntaxKind.Bracketed; }

export class Parenthesized extends PunctuatedBase {
    readonly kind = SyntaxKind.Parenthesized;
    parentNode: null | ParenthesizedParent = null;
    constructor(public text: string, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ParenthesizedChild> { }
}

export type ParenthesizedParent = QuoteExpression | never;

export type ParenthesizedChild = never;

export function isParenthesized(value: any): value is Parenthesized { return value.kind === SyntaxKind.Parenthesized; }

export function createParenthesized(text: string, span: TextSpan | null = null, parentNode: Syntax | null = null): Parenthesized { return new Parenthesized(text, span, parentNode); }

export class Braced extends PunctuatedBase {
    readonly kind = SyntaxKind.Braced;
    parentNode: null | BracedParent = null;
    constructor(public text: string, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<BracedChild> { }
}

export type BracedParent = QuoteExpression | never;

export type BracedChild = never;

export function isBraced(value: any): value is Braced { return value.kind === SyntaxKind.Braced; }

export function createBraced(text: string, span: TextSpan | null = null, parentNode: Syntax | null = null): Braced { return new Braced(text, span, parentNode); }

export class Bracketed extends PunctuatedBase {
    readonly kind = SyntaxKind.Bracketed;
    parentNode: null | BracketedParent = null;
    constructor(public text: string, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<BracketedChild> { }
}

export type BracketedParent = QuoteExpression | never;

export type BracketedChild = never;

export function isBracketed(value: any): value is Bracketed { return value.kind === SyntaxKind.Bracketed; }

export function createBracketed(text: string, span: TextSpan | null = null, parentNode: Syntax | null = null): Bracketed { return new Bracketed(text, span, parentNode); }

export class SourceFile extends SyntaxBase {
    readonly kind = SyntaxKind.SourceFile;
    parentNode: null | SourceFileParent = null;
    *getChildNodes(): Iterable<SourceFileChild> { for (let element of this.elements)
        yield element; }
    public checker?: TypeChecker;
    constructor(public elements: SourceElement[], public pkg: Package, span: TextSpan | null = null, parentNode: Syntax | null = null) {
        super(span, parentNode);
    }
}

export type SourceFileParent = never;

export type SourceFileChild = SourceElement | never;

export function isSourceFile(value: any): value is SourceFile { return value.kind === SyntaxKind.SourceFile; }

export function createSourceFile(elements: SourceElement[], pkg: Package, span: TextSpan | null = null, parentNode: Syntax | null = null): SourceFile { return new SourceFile(elements, pkg, span, parentNode); }

export class QualName extends SyntaxBase {
    readonly kind = SyntaxKind.QualName;
    parentNode: null | QualNameParent = null;
    constructor(public isAbsolute: boolean, public modulePath: Identifier[], public name: Symbol, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<QualNameChild> { for (let element of this.modulePath)
        yield element; yield this.name; }
}

export type QualNameParent = ReferenceTypeExpression | ReferenceExpression | PlainImportSymbol | PlainExportSymbol | never;

export type QualNameChild = Identifier | Symbol | never;

export function isQualName(value: any): value is QualName { return value.kind === SyntaxKind.QualName; }

export function createQualName(isAbsolute: boolean, modulePath: Identifier[], name: Symbol, span: TextSpan | null = null, parentNode: Syntax | null = null): QualName { return new QualName(isAbsolute, modulePath, name, span, parentNode); }

export class TypeExpressionBase extends SyntaxBase {
}

export type TypeExpression = TypeOfExpression | ReferenceTypeExpression | FunctionTypeExpression | LiftedTypeExpression;

export function isTypeExpression(value: Syntax): value is TypeExpression { return value.kind === SyntaxKind.TypeOfExpression || value.kind === SyntaxKind.ReferenceTypeExpression || value.kind === SyntaxKind.FunctionTypeExpression || value.kind === SyntaxKind.LiftedTypeExpression; }

export class TypeOfExpression extends TypeExpressionBase {
    readonly kind = SyntaxKind.TypeOfExpression;
    parentNode: null | TypeOfExpressionParent = null;
    constructor(public expression: Expression, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<TypeOfExpressionChild> { yield this.expression; }
}

export type TypeOfExpressionParent = ReferenceTypeExpression | FunctionTypeExpression | TypeParameter | TypePattern | RecordPattern | FunctionExpression | Parameter | FunctionDeclaration | VariableDeclaration | TraitDeclaration | TypeAliasDeclaration | RecordDeclarationField | never;

export type TypeOfExpressionChild = Expression | never;

export function isTypeOfExpression(value: any): value is TypeOfExpression { return value.kind === SyntaxKind.TypeOfExpression; }

export function createTypeOfExpression(expression: Expression, span: TextSpan | null = null, parentNode: Syntax | null = null): TypeOfExpression { return new TypeOfExpression(expression, span, parentNode); }

export class ReferenceTypeExpression extends TypeExpressionBase {
    readonly kind = SyntaxKind.ReferenceTypeExpression;
    parentNode: null | ReferenceTypeExpressionParent = null;
    constructor(public name: QualName, public typeArgs: TypeExpression[] | null, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ReferenceTypeExpressionChild> { yield this.name; if (this.typeArgs !== null)
        for (let element of this.typeArgs)
            yield element; }
}

export type ReferenceTypeExpressionParent = ReferenceTypeExpression | FunctionTypeExpression | TypeParameter | TypePattern | RecordPattern | RecordExpression | FunctionExpression | Parameter | FunctionDeclaration | VariableDeclaration | TraitDeclaration | ImplDeclaration | TypeAliasDeclaration | RecordDeclarationField | never;

export type ReferenceTypeExpressionChild = QualName | TypeExpression | never;

export function isReferenceTypeExpression(value: any): value is ReferenceTypeExpression { return value.kind === SyntaxKind.ReferenceTypeExpression; }

export function createReferenceTypeExpression(name: QualName, typeArgs: TypeExpression[] | null, span: TextSpan | null = null, parentNode: Syntax | null = null): ReferenceTypeExpression { return new ReferenceTypeExpression(name, typeArgs, span, parentNode); }

export class FunctionTypeExpression extends TypeExpressionBase {
    readonly kind = SyntaxKind.FunctionTypeExpression;
    parentNode: null | FunctionTypeExpressionParent = null;
    constructor(public params: Parameter[], public returnTypeExpr: TypeExpression | null, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<FunctionTypeExpressionChild> { for (let element of this.params)
        yield element; if (this.returnTypeExpr !== null)
        yield this.returnTypeExpr; }
}

export type FunctionTypeExpressionParent = ReferenceTypeExpression | FunctionTypeExpression | TypeParameter | TypePattern | RecordPattern | FunctionExpression | Parameter | FunctionDeclaration | VariableDeclaration | TraitDeclaration | TypeAliasDeclaration | RecordDeclarationField | never;

export type FunctionTypeExpressionChild = Parameter | TypeExpression | never;

export function isFunctionTypeExpression(value: any): value is FunctionTypeExpression { return value.kind === SyntaxKind.FunctionTypeExpression; }

export function createFunctionTypeExpression(params: Parameter[], returnTypeExpr: TypeExpression | null, span: TextSpan | null = null, parentNode: Syntax | null = null): FunctionTypeExpression { return new FunctionTypeExpression(params, returnTypeExpr, span, parentNode); }

export class LiftedTypeExpression extends TypeExpressionBase {
    readonly kind = SyntaxKind.LiftedTypeExpression;
    parentNode: null | LiftedTypeExpressionParent = null;
    constructor(public expression: Expression, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<LiftedTypeExpressionChild> { yield this.expression; }
}

export type LiftedTypeExpressionParent = ReferenceTypeExpression | FunctionTypeExpression | TypeParameter | TypePattern | RecordPattern | FunctionExpression | Parameter | FunctionDeclaration | VariableDeclaration | TraitDeclaration | TypeAliasDeclaration | RecordDeclarationField | never;

export type LiftedTypeExpressionChild = Expression | never;

export function isLiftedTypeExpression(value: any): value is LiftedTypeExpression { return value.kind === SyntaxKind.LiftedTypeExpression; }

export function createLiftedTypeExpression(expression: Expression, span: TextSpan | null = null, parentNode: Syntax | null = null): LiftedTypeExpression { return new LiftedTypeExpression(expression, span, parentNode); }

export class TypeParameter extends SyntaxBase {
    readonly kind = SyntaxKind.TypeParameter;
    parentNode: null | TypeParameterParent = null;
    constructor(public index: number, public name: Identifier, public typeExpr: TypeExpression | null, public defaultType: TypeExpression | null, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<TypeParameterChild> { yield this.name; if (this.typeExpr !== null)
        yield this.typeExpr; if (this.defaultType !== null)
        yield this.defaultType; }
}

export type TypeParameterParent = FunctionDeclaration | TraitDeclaration | ImplDeclaration | TypeAliasDeclaration | EnumDeclaration | RecordDeclaration | never;

export type TypeParameterChild = Identifier | TypeExpression | never;

export function isTypeParameter(value: any): value is TypeParameter { return value.kind === SyntaxKind.TypeParameter; }

export function createTypeParameter(index: number, name: Identifier, typeExpr: TypeExpression | null, defaultType: TypeExpression | null, span: TextSpan | null = null, parentNode: Syntax | null = null): TypeParameter { return new TypeParameter(index, name, typeExpr, defaultType, span, parentNode); }

export class PatternBase extends SyntaxBase {
}

export type Pattern = BindPattern | TypePattern | ExpressionPattern | TuplePattern | RecordPattern;

export function isPattern(value: Syntax): value is Pattern { return value.kind === SyntaxKind.BindPattern || value.kind === SyntaxKind.TypePattern || value.kind === SyntaxKind.ExpressionPattern || value.kind === SyntaxKind.TuplePattern || value.kind === SyntaxKind.RecordPattern; }

export class BindPattern extends PatternBase {
    readonly kind = SyntaxKind.BindPattern;
    parentNode: null | BindPatternParent = null;
    constructor(public name: Identifier, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<BindPatternChild> { yield this.name; }
}

export type BindPatternParent = TypePattern | TuplePatternElement | RecordPatternField | MatchArm | AssignStatement | Parameter | VariableDeclaration | never;

export type BindPatternChild = Identifier | never;

export function isBindPattern(value: any): value is BindPattern { return value.kind === SyntaxKind.BindPattern; }

export function createBindPattern(name: Identifier, span: TextSpan | null = null, parentNode: Syntax | null = null): BindPattern { return new BindPattern(name, span, parentNode); }

export class TypePattern extends PatternBase {
    readonly kind = SyntaxKind.TypePattern;
    parentNode: null | TypePatternParent = null;
    constructor(public typeExpr: TypeExpression, public nestedPattern: Pattern, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<TypePatternChild> { yield this.typeExpr; yield this.nestedPattern; }
}

export type TypePatternParent = TypePattern | TuplePatternElement | RecordPatternField | MatchArm | AssignStatement | Parameter | VariableDeclaration | never;

export type TypePatternChild = TypeExpression | Pattern | never;

export function isTypePattern(value: any): value is TypePattern { return value.kind === SyntaxKind.TypePattern; }

export function createTypePattern(typeExpr: TypeExpression, nestedPattern: Pattern, span: TextSpan | null = null, parentNode: Syntax | null = null): TypePattern { return new TypePattern(typeExpr, nestedPattern, span, parentNode); }

export class ExpressionPattern extends PatternBase {
    readonly kind = SyntaxKind.ExpressionPattern;
    parentNode: null | ExpressionPatternParent = null;
    constructor(public expression: Expression, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ExpressionPatternChild> { yield this.expression; }
}

export type ExpressionPatternParent = TypePattern | TuplePatternElement | RecordPatternField | MatchArm | AssignStatement | Parameter | VariableDeclaration | never;

export type ExpressionPatternChild = Expression | never;

export function isExpressionPattern(value: any): value is ExpressionPattern { return value.kind === SyntaxKind.ExpressionPattern; }

export function createExpressionPattern(expression: Expression, span: TextSpan | null = null, parentNode: Syntax | null = null): ExpressionPattern { return new ExpressionPattern(expression, span, parentNode); }

export class TuplePatternElement extends SyntaxBase {
    readonly kind = SyntaxKind.TuplePatternElement;
    parentNode: null | TuplePatternElementParent = null;
    constructor(public index: number, public pattern: Pattern, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<TuplePatternElementChild> { yield this.pattern; }
}

export type TuplePatternElementParent = TuplePattern | never;

export type TuplePatternElementChild = Pattern | never;

export function isTuplePatternElement(value: any): value is TuplePatternElement { return value.kind === SyntaxKind.TuplePatternElement; }

export function createTuplePatternElement(index: number, pattern: Pattern, span: TextSpan | null = null, parentNode: Syntax | null = null): TuplePatternElement { return new TuplePatternElement(index, pattern, span, parentNode); }

export class TuplePattern extends PatternBase {
    readonly kind = SyntaxKind.TuplePattern;
    parentNode: null | TuplePatternParent = null;
    constructor(public elements: TuplePatternElement[], span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<TuplePatternChild> { for (let element of this.elements)
        yield element; }
}

export type TuplePatternParent = TypePattern | TuplePatternElement | RecordPatternField | MatchArm | AssignStatement | Parameter | VariableDeclaration | never;

export type TuplePatternChild = TuplePatternElement | never;

export function isTuplePattern(value: any): value is TuplePattern { return value.kind === SyntaxKind.TuplePattern; }

export function createTuplePattern(elements: TuplePatternElement[], span: TextSpan | null = null, parentNode: Syntax | null = null): TuplePattern { return new TuplePattern(elements, span, parentNode); }

export class RecordPatternField extends SyntaxBase {
    readonly kind = SyntaxKind.RecordPatternField;
    parentNode: null | RecordPatternFieldParent = null;
    constructor(public isRest: boolean, public name: Identifier | null, public pattern: Pattern | null, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<RecordPatternFieldChild> { if (this.name !== null)
        yield this.name; if (this.pattern !== null)
        yield this.pattern; }
}

export type RecordPatternFieldParent = RecordPattern | never;

export type RecordPatternFieldChild = Identifier | Pattern | never;

export function isRecordPatternField(value: any): value is RecordPatternField { return value.kind === SyntaxKind.RecordPatternField; }

export function createRecordPatternField(isRest: boolean, name: Identifier | null, pattern: Pattern | null, span: TextSpan | null = null, parentNode: Syntax | null = null): RecordPatternField { return new RecordPatternField(isRest, name, pattern, span, parentNode); }

export class RecordPattern extends PatternBase {
    readonly kind = SyntaxKind.RecordPattern;
    parentNode: null | RecordPatternParent = null;
    constructor(public name: TypeExpression, public fields: RecordPatternField[], span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<RecordPatternChild> { yield this.name; for (let element of this.fields)
        yield element; }
}

export type RecordPatternParent = TypePattern | TuplePatternElement | RecordPatternField | MatchArm | AssignStatement | Parameter | VariableDeclaration | never;

export type RecordPatternChild = TypeExpression | RecordPatternField | never;

export function isRecordPattern(value: any): value is RecordPattern { return value.kind === SyntaxKind.RecordPattern; }

export function createRecordPattern(name: TypeExpression, fields: RecordPatternField[], span: TextSpan | null = null, parentNode: Syntax | null = null): RecordPattern { return new RecordPattern(name, fields, span, parentNode); }

export class ExpressionBase extends SyntaxBase {
}

export type Expression = RecordExpression | QuoteExpression | TupleExpression | ReferenceExpression | MemberExpression | FunctionExpression | CallExpression | YieldExpression | MatchExpression | BlockExpression | ConstantExpression;

export function isExpression(value: Syntax): value is Expression { return value.kind === SyntaxKind.RecordExpression || value.kind === SyntaxKind.QuoteExpression || value.kind === SyntaxKind.TupleExpression || value.kind === SyntaxKind.ReferenceExpression || value.kind === SyntaxKind.MemberExpression || value.kind === SyntaxKind.FunctionExpression || value.kind === SyntaxKind.CallExpression || value.kind === SyntaxKind.YieldExpression || value.kind === SyntaxKind.MatchExpression || value.kind === SyntaxKind.BlockExpression || value.kind === SyntaxKind.ConstantExpression; }

export class RecordExpression extends ExpressionBase {
    readonly kind = SyntaxKind.RecordExpression;
    parentNode: null | RecordExpressionParent = null;
    constructor(public typeRef: ReferenceTypeExpression, public fields: RecordExpressionElement[], span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<RecordExpressionChild> { yield this.typeRef; for (let element of this.fields)
        yield element; }
}

export type RecordExpressionParent = TypeOfExpression | LiftedTypeExpression | ExpressionPattern | RecordFieldValue | QuoteExpression | TupleExpression | MemberExpression | FunctionExpression | CallExpression | YieldExpression | MatchArm | MatchExpression | CaseStatementCase | ReturnStatement | ConditionalCase | ResumeStatement | ExpressionStatement | AssignStatement | Parameter | VariableDeclaration | never;

export type RecordExpressionChild = ReferenceTypeExpression | RecordExpressionElement | never;

export function isRecordExpression(value: any): value is RecordExpression { return value.kind === SyntaxKind.RecordExpression; }

export function createRecordExpression(typeRef: ReferenceTypeExpression, fields: RecordExpressionElement[], span: TextSpan | null = null, parentNode: Syntax | null = null): RecordExpression { return new RecordExpression(typeRef, fields, span, parentNode); }

export class RecordExpressionElementBase extends SyntaxBase {
}

export type RecordExpressionElement = RecordFieldValue;

export function isRecordExpressionElement(value: Syntax): value is RecordExpressionElement { return value.kind === SyntaxKind.RecordFieldValue; }

export class RecordFieldValue extends RecordExpressionElementBase {
    readonly kind = SyntaxKind.RecordFieldValue;
    parentNode: null | RecordFieldValueParent = null;
    constructor(public name: Identifier, public value: Expression | null, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<RecordFieldValueChild> { yield this.name; if (this.value !== null)
        yield this.value; }
}

export type RecordFieldValueParent = RecordExpression | never;

export type RecordFieldValueChild = Identifier | Expression | never;

export function isRecordFieldValue(value: any): value is RecordFieldValue { return value.kind === SyntaxKind.RecordFieldValue; }

export function createRecordFieldValue(name: Identifier, value: Expression | null, span: TextSpan | null = null, parentNode: Syntax | null = null): RecordFieldValue { return new RecordFieldValue(name, value, span, parentNode); }

export class QuoteExpression extends ExpressionBase {
    readonly kind = SyntaxKind.QuoteExpression;
    parentNode: null | QuoteExpressionParent = null;
    constructor(public tokens: Array<Token | Expression>, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<QuoteExpressionChild> { for (let element of this.tokens) {
        if (isToken(element))
            yield element;
        if (isExpression(element))
            yield element;
    } }
}

export type QuoteExpressionParent = TypeOfExpression | LiftedTypeExpression | ExpressionPattern | RecordFieldValue | QuoteExpression | TupleExpression | MemberExpression | FunctionExpression | CallExpression | YieldExpression | MatchArm | MatchExpression | CaseStatementCase | ReturnStatement | ConditionalCase | ResumeStatement | ExpressionStatement | AssignStatement | Parameter | VariableDeclaration | never;

export type QuoteExpressionChild = Token | Expression | never;

export function isQuoteExpression(value: any): value is QuoteExpression { return value.kind === SyntaxKind.QuoteExpression; }

export function createQuoteExpression(tokens: Array<Token | Expression>, span: TextSpan | null = null, parentNode: Syntax | null = null): QuoteExpression { return new QuoteExpression(tokens, span, parentNode); }

export class TupleExpression extends ExpressionBase {
    readonly kind = SyntaxKind.TupleExpression;
    parentNode: null | TupleExpressionParent = null;
    constructor(public elements: Expression[], span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<TupleExpressionChild> { for (let element of this.elements)
        yield element; }
}

export type TupleExpressionParent = TypeOfExpression | LiftedTypeExpression | ExpressionPattern | RecordFieldValue | QuoteExpression | TupleExpression | MemberExpression | FunctionExpression | CallExpression | YieldExpression | MatchArm | MatchExpression | CaseStatementCase | ReturnStatement | ConditionalCase | ResumeStatement | ExpressionStatement | AssignStatement | Parameter | VariableDeclaration | never;

export type TupleExpressionChild = Expression | never;

export function isTupleExpression(value: any): value is TupleExpression { return value.kind === SyntaxKind.TupleExpression; }

export function createTupleExpression(elements: Expression[], span: TextSpan | null = null, parentNode: Syntax | null = null): TupleExpression { return new TupleExpression(elements, span, parentNode); }

export class ReferenceExpression extends ExpressionBase {
    readonly kind = SyntaxKind.ReferenceExpression;
    parentNode: null | ReferenceExpressionParent = null;
    constructor(public name: QualName, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ReferenceExpressionChild> { yield this.name; }
}

export type ReferenceExpressionParent = TypeOfExpression | LiftedTypeExpression | ExpressionPattern | RecordFieldValue | QuoteExpression | TupleExpression | MemberExpression | FunctionExpression | CallExpression | YieldExpression | MatchArm | MatchExpression | CaseStatementCase | ReturnStatement | ConditionalCase | ResumeStatement | ExpressionStatement | AssignStatement | Parameter | VariableDeclaration | never;

export type ReferenceExpressionChild = QualName | never;

export function isReferenceExpression(value: any): value is ReferenceExpression { return value.kind === SyntaxKind.ReferenceExpression; }

export function createReferenceExpression(name: QualName, span: TextSpan | null = null, parentNode: Syntax | null = null): ReferenceExpression { return new ReferenceExpression(name, span, parentNode); }

export class MemberExpression extends ExpressionBase {
    readonly kind = SyntaxKind.MemberExpression;
    parentNode: null | MemberExpressionParent = null;
    constructor(public expression: Expression, public path: Identifier[], span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<MemberExpressionChild> { yield this.expression; for (let element of this.path)
        yield element; }
}

export type MemberExpressionParent = TypeOfExpression | LiftedTypeExpression | ExpressionPattern | RecordFieldValue | QuoteExpression | TupleExpression | MemberExpression | FunctionExpression | CallExpression | YieldExpression | MatchArm | MatchExpression | CaseStatementCase | ReturnStatement | ConditionalCase | ResumeStatement | ExpressionStatement | AssignStatement | Parameter | VariableDeclaration | never;

export type MemberExpressionChild = Expression | Identifier | never;

export function isMemberExpression(value: any): value is MemberExpression { return value.kind === SyntaxKind.MemberExpression; }

export function createMemberExpression(expression: Expression, path: Identifier[], span: TextSpan | null = null, parentNode: Syntax | null = null): MemberExpression { return new MemberExpression(expression, path, span, parentNode); }

export class FunctionExpression extends ExpressionBase {
    readonly kind = SyntaxKind.FunctionExpression;
    parentNode: null | FunctionExpressionParent = null;
    constructor(public params: Parameter[], public returnTypeExpr: TypeExpression | null, public body: FunctionBodyElement[] | null, public expression: Expression | null, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<FunctionExpressionChild> { for (let element of this.params)
        yield element; if (this.returnTypeExpr !== null)
        yield this.returnTypeExpr; if (this.body !== null)
        for (let element of this.body)
            yield element; if (this.expression !== null)
        yield this.expression; }
}

export type FunctionExpressionParent = TypeOfExpression | LiftedTypeExpression | ExpressionPattern | RecordFieldValue | QuoteExpression | TupleExpression | MemberExpression | FunctionExpression | CallExpression | YieldExpression | MatchArm | MatchExpression | CaseStatementCase | ReturnStatement | ConditionalCase | ResumeStatement | ExpressionStatement | AssignStatement | Parameter | VariableDeclaration | never;

export type FunctionExpressionChild = Parameter | TypeExpression | FunctionBodyElement | Expression | never;

export function isFunctionExpression(value: any): value is FunctionExpression { return value.kind === SyntaxKind.FunctionExpression; }

export function createFunctionExpression(params: Parameter[], returnTypeExpr: TypeExpression | null, body: FunctionBodyElement[] | null, expression: Expression | null, span: TextSpan | null = null, parentNode: Syntax | null = null): FunctionExpression { return new FunctionExpression(params, returnTypeExpr, body, expression, span, parentNode); }

export class CallExpression extends ExpressionBase {
    readonly kind = SyntaxKind.CallExpression;
    parentNode: null | CallExpressionParent = null;
    constructor(public operator: Expression, public operands: Expression[], span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<CallExpressionChild> { yield this.operator; for (let element of this.operands)
        yield element; }
}

export type CallExpressionParent = TypeOfExpression | LiftedTypeExpression | ExpressionPattern | RecordFieldValue | QuoteExpression | TupleExpression | MemberExpression | FunctionExpression | CallExpression | YieldExpression | MatchArm | MatchExpression | CaseStatementCase | ReturnStatement | ConditionalCase | ResumeStatement | ExpressionStatement | AssignStatement | Parameter | VariableDeclaration | never;

export type CallExpressionChild = Expression | never;

export function isCallExpression(value: any): value is CallExpression { return value.kind === SyntaxKind.CallExpression; }

export function createCallExpression(operator: Expression, operands: Expression[], span: TextSpan | null = null, parentNode: Syntax | null = null): CallExpression { return new CallExpression(operator, operands, span, parentNode); }

export class YieldExpression extends ExpressionBase {
    readonly kind = SyntaxKind.YieldExpression;
    parentNode: null | YieldExpressionParent = null;
    constructor(public value: Expression, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<YieldExpressionChild> { yield this.value; }
}

export type YieldExpressionParent = TypeOfExpression | LiftedTypeExpression | ExpressionPattern | RecordFieldValue | QuoteExpression | TupleExpression | MemberExpression | FunctionExpression | CallExpression | YieldExpression | MatchArm | MatchExpression | CaseStatementCase | ReturnStatement | ConditionalCase | ResumeStatement | ExpressionStatement | AssignStatement | Parameter | VariableDeclaration | never;

export type YieldExpressionChild = Expression | never;

export function isYieldExpression(value: any): value is YieldExpression { return value.kind === SyntaxKind.YieldExpression; }

export function createYieldExpression(value: Expression, span: TextSpan | null = null, parentNode: Syntax | null = null): YieldExpression { return new YieldExpression(value, span, parentNode); }

export class MatchArm extends SyntaxBase {
    readonly kind = SyntaxKind.MatchArm;
    parentNode: null | MatchArmParent = null;
    constructor(public pattern: Pattern, public body: Expression, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<MatchArmChild> { yield this.pattern; yield this.body; }
}

export type MatchArmParent = MatchExpression | never;

export type MatchArmChild = Pattern | Expression | never;

export function isMatchArm(value: any): value is MatchArm { return value.kind === SyntaxKind.MatchArm; }

export function createMatchArm(pattern: Pattern, body: Expression, span: TextSpan | null = null, parentNode: Syntax | null = null): MatchArm { return new MatchArm(pattern, body, span, parentNode); }

export class MatchExpression extends ExpressionBase {
    readonly kind = SyntaxKind.MatchExpression;
    parentNode: null | MatchExpressionParent = null;
    constructor(public value: Expression, public arms: MatchArm[], span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<MatchExpressionChild> { yield this.value; for (let element of this.arms)
        yield element; }
}

export type MatchExpressionParent = TypeOfExpression | LiftedTypeExpression | ExpressionPattern | RecordFieldValue | QuoteExpression | TupleExpression | MemberExpression | FunctionExpression | CallExpression | YieldExpression | MatchArm | MatchExpression | CaseStatementCase | ReturnStatement | ConditionalCase | ResumeStatement | ExpressionStatement | AssignStatement | Parameter | VariableDeclaration | never;

export type MatchExpressionChild = Expression | MatchArm | never;

export function isMatchExpression(value: any): value is MatchExpression { return value.kind === SyntaxKind.MatchExpression; }

export function createMatchExpression(value: Expression, arms: MatchArm[], span: TextSpan | null = null, parentNode: Syntax | null = null): MatchExpression { return new MatchExpression(value, arms, span, parentNode); }

export class StatementBase extends SyntaxBase {
}

export type Statement = CaseStatement | ReturnStatement | ConditionalStatement | ResumeStatement | ExpressionStatement | AssignStatement | LoopStatement;

export function isStatement(value: Syntax): value is Statement { return value.kind === SyntaxKind.CaseStatement || value.kind === SyntaxKind.ReturnStatement || value.kind === SyntaxKind.ConditionalStatement || value.kind === SyntaxKind.ResumeStatement || value.kind === SyntaxKind.ExpressionStatement || value.kind === SyntaxKind.AssignStatement || value.kind === SyntaxKind.LoopStatement; }

export class CaseStatementCase extends SyntaxBase {
    readonly kind = SyntaxKind.CaseStatementCase;
    parentNode: null | CaseStatementCaseParent = null;
    constructor(public test: Expression | null, public body: FunctionBodyElement[], span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<CaseStatementCaseChild> { if (this.test !== null)
        yield this.test; for (let element of this.body)
        yield element; }
}

export type CaseStatementCaseParent = CaseStatement | never;

export type CaseStatementCaseChild = Expression | FunctionBodyElement | never;

export function isCaseStatementCase(value: any): value is CaseStatementCase { return value.kind === SyntaxKind.CaseStatementCase; }

export function createCaseStatementCase(test: Expression | null, body: FunctionBodyElement[], span: TextSpan | null = null, parentNode: Syntax | null = null): CaseStatementCase { return new CaseStatementCase(test, body, span, parentNode); }

export class CaseStatement extends StatementBase {
    readonly kind = SyntaxKind.CaseStatement;
    parentNode: null | CaseStatementParent = null;
    constructor(public cases: CaseStatementCase[], span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<CaseStatementChild> { for (let element of this.cases)
        yield element; }
}

export type CaseStatementParent = never;

export type CaseStatementChild = CaseStatementCase | never;

export function isCaseStatement(value: any): value is CaseStatement { return value.kind === SyntaxKind.CaseStatement; }

export function createCaseStatement(cases: CaseStatementCase[], span: TextSpan | null = null, parentNode: Syntax | null = null): CaseStatement { return new CaseStatement(cases, span, parentNode); }

export class BlockExpression extends ExpressionBase {
    readonly kind = SyntaxKind.BlockExpression;
    parentNode: null | BlockExpressionParent = null;
    constructor(public elements: FunctionBodyElement[], span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<BlockExpressionChild> { for (let element of this.elements)
        yield element; }
}

export type BlockExpressionParent = TypeOfExpression | LiftedTypeExpression | ExpressionPattern | RecordFieldValue | QuoteExpression | TupleExpression | MemberExpression | FunctionExpression | CallExpression | YieldExpression | MatchArm | MatchExpression | CaseStatementCase | ReturnStatement | ConditionalCase | ResumeStatement | ExpressionStatement | AssignStatement | Parameter | VariableDeclaration | never;

export type BlockExpressionChild = FunctionBodyElement | never;

export function isBlockExpression(value: any): value is BlockExpression { return value.kind === SyntaxKind.BlockExpression; }

export function createBlockExpression(elements: FunctionBodyElement[], span: TextSpan | null = null, parentNode: Syntax | null = null): BlockExpression { return new BlockExpression(elements, span, parentNode); }

export class ConstantExpression extends ExpressionBase {
    readonly kind = SyntaxKind.ConstantExpression;
    parentNode: null | ConstantExpressionParent = null;
    constructor(public value: string | number | bigint | boolean, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ConstantExpressionChild> { }
}

export type ConstantExpressionParent = TypeOfExpression | LiftedTypeExpression | ExpressionPattern | RecordFieldValue | QuoteExpression | TupleExpression | MemberExpression | FunctionExpression | CallExpression | YieldExpression | MatchArm | MatchExpression | CaseStatementCase | ReturnStatement | ConditionalCase | ResumeStatement | ExpressionStatement | AssignStatement | Parameter | VariableDeclaration | never;

export type ConstantExpressionChild = never;

export function isConstantExpression(value: any): value is ConstantExpression { return value.kind === SyntaxKind.ConstantExpression; }

export function createConstantExpression(value: string | number | bigint | boolean, span: TextSpan | null = null, parentNode: Syntax | null = null): ConstantExpression { return new ConstantExpression(value, span, parentNode); }

export class ReturnStatement extends StatementBase {
    readonly kind = SyntaxKind.ReturnStatement;
    parentNode: null | ReturnStatementParent = null;
    constructor(public value: Expression | null, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ReturnStatementChild> { if (this.value !== null)
        yield this.value; }
}

export type ReturnStatementParent = never;

export type ReturnStatementChild = Expression | never;

export function isReturnStatement(value: any): value is ReturnStatement { return value.kind === SyntaxKind.ReturnStatement; }

export function createReturnStatement(value: Expression | null, span: TextSpan | null = null, parentNode: Syntax | null = null): ReturnStatement { return new ReturnStatement(value, span, parentNode); }

export class ConditionalCase extends SyntaxBase {
    readonly kind = SyntaxKind.ConditionalCase;
    parentNode: null | ConditionalCaseParent = null;
    constructor(public test: Expression | null, public body: FunctionBodyElement[], span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ConditionalCaseChild> { if (this.test !== null)
        yield this.test; for (let element of this.body)
        yield element; }
}

export type ConditionalCaseParent = ConditionalStatement | never;

export type ConditionalCaseChild = Expression | FunctionBodyElement | never;

export function isConditionalCase(value: any): value is ConditionalCase { return value.kind === SyntaxKind.ConditionalCase; }

export function createConditionalCase(test: Expression | null, body: FunctionBodyElement[], span: TextSpan | null = null, parentNode: Syntax | null = null): ConditionalCase { return new ConditionalCase(test, body, span, parentNode); }

export class ConditionalStatement extends StatementBase {
    readonly kind = SyntaxKind.ConditionalStatement;
    parentNode: null | ConditionalStatementParent = null;
    constructor(public cases: ConditionalCase[], span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ConditionalStatementChild> { for (let element of this.cases)
        yield element; }
}

export type ConditionalStatementParent = never;

export type ConditionalStatementChild = ConditionalCase | never;

export function isConditionalStatement(value: any): value is ConditionalStatement { return value.kind === SyntaxKind.ConditionalStatement; }

export function createConditionalStatement(cases: ConditionalCase[], span: TextSpan | null = null, parentNode: Syntax | null = null): ConditionalStatement { return new ConditionalStatement(cases, span, parentNode); }

export class ResumeStatement extends StatementBase {
    readonly kind = SyntaxKind.ResumeStatement;
    parentNode: null | ResumeStatementParent = null;
    constructor(public value: Expression, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ResumeStatementChild> { yield this.value; }
}

export type ResumeStatementParent = never;

export type ResumeStatementChild = Expression | never;

export function isResumeStatement(value: any): value is ResumeStatement { return value.kind === SyntaxKind.ResumeStatement; }

export function createResumeStatement(value: Expression, span: TextSpan | null = null, parentNode: Syntax | null = null): ResumeStatement { return new ResumeStatement(value, span, parentNode); }

export class ExpressionStatement extends StatementBase {
    readonly kind = SyntaxKind.ExpressionStatement;
    parentNode: null | ExpressionStatementParent = null;
    constructor(public expression: Expression, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ExpressionStatementChild> { yield this.expression; }
}

export type ExpressionStatementParent = never;

export type ExpressionStatementChild = Expression | never;

export function isExpressionStatement(value: any): value is ExpressionStatement { return value.kind === SyntaxKind.ExpressionStatement; }

export function createExpressionStatement(expression: Expression, span: TextSpan | null = null, parentNode: Syntax | null = null): ExpressionStatement { return new ExpressionStatement(expression, span, parentNode); }

export class AssignStatement extends StatementBase {
    readonly kind = SyntaxKind.AssignStatement;
    parentNode: null | AssignStatementParent = null;
    constructor(public lhs: Pattern, public rhs: Expression, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<AssignStatementChild> { yield this.lhs; yield this.rhs; }
}

export type AssignStatementParent = never;

export type AssignStatementChild = Pattern | Expression | never;

export function isAssignStatement(value: any): value is AssignStatement { return value.kind === SyntaxKind.AssignStatement; }

export function createAssignStatement(lhs: Pattern, rhs: Expression, span: TextSpan | null = null, parentNode: Syntax | null = null): AssignStatement { return new AssignStatement(lhs, rhs, span, parentNode); }

export class LoopStatement extends StatementBase {
    readonly kind = SyntaxKind.LoopStatement;
    parentNode: null | LoopStatementParent = null;
    constructor(public elements: FunctionBodyElement[], span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<LoopStatementChild> { for (let element of this.elements)
        yield element; }
}

export type LoopStatementParent = never;

export type LoopStatementChild = FunctionBodyElement | never;

export function isLoopStatement(value: any): value is LoopStatement { return value.kind === SyntaxKind.LoopStatement; }

export function createLoopStatement(elements: FunctionBodyElement[], span: TextSpan | null = null, parentNode: Syntax | null = null): LoopStatement { return new LoopStatement(elements, span, parentNode); }

export class Parameter extends SyntaxBase {
    readonly kind = SyntaxKind.Parameter;
    parentNode: null | ParameterParent = null;
    constructor(public index: number, public bindings: Pattern, public typeExpr: TypeExpression | null, public defaultValue: Expression | null, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ParameterChild> { yield this.bindings; if (this.typeExpr !== null)
        yield this.typeExpr; if (this.defaultValue !== null)
        yield this.defaultValue; }
}

export type ParameterParent = FunctionTypeExpression | FunctionExpression | FunctionDeclaration | never;

export type ParameterChild = Pattern | TypeExpression | Expression | never;

export function isParameter(value: any): value is Parameter { return value.kind === SyntaxKind.Parameter; }

export function createParameter(index: number, bindings: Pattern, typeExpr: TypeExpression | null, defaultValue: Expression | null, span: TextSpan | null = null, parentNode: Syntax | null = null): Parameter { return new Parameter(index, bindings, typeExpr, defaultValue, span, parentNode); }

export enum Modifiers {
    IsMutable = 0x1,
    IsPublic = 0x2
}

export class Module extends SyntaxBase {
    readonly kind = SyntaxKind.Module;
    parentNode: null | ModuleParent = null;
    constructor(public modifiers: Modifiers, public name: Identifier[], public elements: SourceElement[], span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ModuleChild> { for (let element of this.name)
        yield element; for (let element of this.elements)
        yield element; }
}

export type ModuleParent = never;

export type ModuleChild = Identifier | SourceElement | never;

export function isModule(value: any): value is Module { return value.kind === SyntaxKind.Module; }

export function createModule(modifiers: Modifiers, name: Identifier[], elements: SourceElement[], span: TextSpan | null = null, parentNode: Syntax | null = null): Module { return new Module(modifiers, name, elements, span, parentNode); }

export class DeclarationLikeBase extends SyntaxBase {
}

export type DeclarationLike = FunctionDeclaration | VariableDeclaration | TraitDeclaration | ImplDeclaration | TypeAliasDeclaration | EnumDeclaration | RecordDeclaration;

export function isDeclarationLike(value: Syntax): value is DeclarationLike { return value.kind === SyntaxKind.FunctionDeclaration || value.kind === SyntaxKind.VariableDeclaration || value.kind === SyntaxKind.TraitDeclaration || value.kind === SyntaxKind.ImplDeclaration || value.kind === SyntaxKind.TypeAliasDeclaration || value.kind === SyntaxKind.EnumDeclaration || value.kind === SyntaxKind.RecordDeclaration; }

export class FunctionDeclaration extends DeclarationLikeBase {
    readonly kind = SyntaxKind.FunctionDeclaration;
    parentNode: null | FunctionDeclarationParent = null;
    constructor(public modifiers: Modifiers, public target: string, public name: Symbol, public params: Parameter[], public returnTypeExpr: TypeExpression | null, public typeParams: TypeParameter[] | null, public body: FunctionBodyElement[] | null, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<FunctionDeclarationChild> { yield this.name; for (let element of this.params)
        yield element; if (this.returnTypeExpr !== null)
        yield this.returnTypeExpr; if (this.typeParams !== null)
        for (let element of this.typeParams)
            yield element; if (this.body !== null)
        for (let element of this.body)
            yield element; }
}

export type FunctionDeclarationParent = never;

export type FunctionDeclarationChild = Symbol | Parameter | TypeExpression | TypeParameter | FunctionBodyElement | never;

export function isFunctionDeclaration(value: any): value is FunctionDeclaration { return value.kind === SyntaxKind.FunctionDeclaration; }

export function createFunctionDeclaration(modifiers: Modifiers, target: string, name: Symbol, params: Parameter[], returnTypeExpr: TypeExpression | null, typeParams: TypeParameter[] | null, body: FunctionBodyElement[] | null, span: TextSpan | null = null, parentNode: Syntax | null = null): FunctionDeclaration { return new FunctionDeclaration(modifiers, target, name, params, returnTypeExpr, typeParams, body, span, parentNode); }

export type FunctionBody = Array<FunctionBodyElement>;

export type FunctionBodyElement = Statement | MacroCall | Declaration;

export function isFunctionBodyElement(value: Syntax): value is FunctionBodyElement { return value.kind === SyntaxKind.CaseStatement || value.kind === SyntaxKind.ReturnStatement || value.kind === SyntaxKind.ConditionalStatement || value.kind === SyntaxKind.ResumeStatement || value.kind === SyntaxKind.ExpressionStatement || value.kind === SyntaxKind.AssignStatement || value.kind === SyntaxKind.LoopStatement || value.kind === SyntaxKind.MacroCall || value.kind === SyntaxKind.FunctionDeclaration || value.kind === SyntaxKind.VariableDeclaration; }

export class VariableDeclaration extends DeclarationLikeBase {
    readonly kind = SyntaxKind.VariableDeclaration;
    parentNode: null | VariableDeclarationParent = null;
    constructor(public modifiers: Modifiers, public bindings: Pattern, public typeExpr: TypeExpression | null, public value: Expression | null, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<VariableDeclarationChild> { yield this.bindings; if (this.typeExpr !== null)
        yield this.typeExpr; if (this.value !== null)
        yield this.value; }
}

export type VariableDeclarationParent = never;

export type VariableDeclarationChild = Pattern | TypeExpression | Expression | never;

export function isVariableDeclaration(value: any): value is VariableDeclaration { return value.kind === SyntaxKind.VariableDeclaration; }

export function createVariableDeclaration(modifiers: Modifiers, bindings: Pattern, typeExpr: TypeExpression | null, value: Expression | null, span: TextSpan | null = null, parentNode: Syntax | null = null): VariableDeclaration { return new VariableDeclaration(modifiers, bindings, typeExpr, value, span, parentNode); }

export class ImportSymbolBase extends SyntaxBase {
}

export type ImportSymbol = PlainImportSymbol;

export function isImportSymbol(value: Syntax): value is ImportSymbol { return value.kind === SyntaxKind.PlainImportSymbol; }

export class PlainImportSymbol extends ImportSymbolBase {
    readonly kind = SyntaxKind.PlainImportSymbol;
    parentNode: null | PlainImportSymbolParent = null;
    constructor(public remote: QualName, public local: Symbol, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<PlainImportSymbolChild> { yield this.remote; yield this.local; }
}

export type PlainImportSymbolParent = ImportDeclaration | never;

export type PlainImportSymbolChild = QualName | Symbol | never;

export function isPlainImportSymbol(value: any): value is PlainImportSymbol { return value.kind === SyntaxKind.PlainImportSymbol; }

export function createPlainImportSymbol(remote: QualName, local: Symbol, span: TextSpan | null = null, parentNode: Syntax | null = null): PlainImportSymbol { return new PlainImportSymbol(remote, local, span, parentNode); }

export class ImportDeclaration extends SyntaxBase {
    readonly kind = SyntaxKind.ImportDeclaration;
    parentNode: null | ImportDeclarationParent = null;
    constructor(public modifiers: Modifiers, public file: StringLiteral, public symbols: ImportSymbol[] | null, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ImportDeclarationChild> { yield this.file; if (this.symbols !== null)
        for (let element of this.symbols)
            yield element; }
}

export type ImportDeclarationParent = never;

export type ImportDeclarationChild = StringLiteral | ImportSymbol | never;

export function isImportDeclaration(value: any): value is ImportDeclaration { return value.kind === SyntaxKind.ImportDeclaration; }

export function createImportDeclaration(modifiers: Modifiers, file: StringLiteral, symbols: ImportSymbol[] | null, span: TextSpan | null = null, parentNode: Syntax | null = null): ImportDeclaration { return new ImportDeclaration(modifiers, file, symbols, span, parentNode); }

export class ExportSymbolBase extends SyntaxBase {
}

export type ExportSymbol = PlainExportSymbol;

export function isExportSymbol(value: Syntax): value is ExportSymbol { return value.kind === SyntaxKind.PlainExportSymbol; }

export class PlainExportSymbol extends ExportSymbolBase {
    readonly kind = SyntaxKind.PlainExportSymbol;
    parentNode: null | PlainExportSymbolParent = null;
    constructor(public local: QualName, public remote: Symbol, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<PlainExportSymbolChild> { yield this.local; yield this.remote; }
}

export type PlainExportSymbolParent = ExportDeclaration | never;

export type PlainExportSymbolChild = QualName | Symbol | never;

export function isPlainExportSymbol(value: any): value is PlainExportSymbol { return value.kind === SyntaxKind.PlainExportSymbol; }

export function createPlainExportSymbol(local: QualName, remote: Symbol, span: TextSpan | null = null, parentNode: Syntax | null = null): PlainExportSymbol { return new PlainExportSymbol(local, remote, span, parentNode); }

export class ExportDeclaration extends SyntaxBase {
    readonly kind = SyntaxKind.ExportDeclaration;
    parentNode: null | ExportDeclarationParent = null;
    constructor(public file: string, public symbols: ExportSymbol[] | null, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ExportDeclarationChild> { if (this.symbols !== null)
        for (let element of this.symbols)
            yield element; }
}

export type ExportDeclarationParent = never;

export type ExportDeclarationChild = ExportSymbol | never;

export function isExportDeclaration(value: any): value is ExportDeclaration { return value.kind === SyntaxKind.ExportDeclaration; }

export function createExportDeclaration(file: string, symbols: ExportSymbol[] | null, span: TextSpan | null = null, parentNode: Syntax | null = null): ExportDeclaration { return new ExportDeclaration(file, symbols, span, parentNode); }

export interface TraitOrImplElement {
}

export class TraitDeclaration extends DeclarationLikeBase {
    readonly kind = SyntaxKind.TraitDeclaration;
    parentNode: null | TraitDeclarationParent = null;
    constructor(public modifiers: Modifiers, public typeParams: TypeParameter[] | null, public name: Identifier, public typeBoundExpr: TypeExpression | null, public elements: TraitOrImplElement[] | null, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<TraitDeclarationChild> { if (this.typeParams !== null)
        for (let element of this.typeParams)
            yield element; yield this.name; if (this.typeBoundExpr !== null)
        yield this.typeBoundExpr; }
}

export type TraitDeclarationParent = never;

export type TraitDeclarationChild = TypeParameter | Identifier | TypeExpression | never;

export function isTraitDeclaration(value: any): value is TraitDeclaration { return value.kind === SyntaxKind.TraitDeclaration; }

export function createTraitDeclaration(modifiers: Modifiers, typeParams: TypeParameter[] | null, name: Identifier, typeBoundExpr: TypeExpression | null, elements: TraitOrImplElement[] | null, span: TextSpan | null = null, parentNode: Syntax | null = null): TraitDeclaration { return new TraitDeclaration(modifiers, typeParams, name, typeBoundExpr, elements, span, parentNode); }

export class ImplDeclaration extends DeclarationLikeBase {
    readonly kind = SyntaxKind.ImplDeclaration;
    parentNode: null | ImplDeclarationParent = null;
    constructor(public modifiers: Modifiers, public typeParams: TypeParameter[] | null, public traitTypeExpr: ReferenceTypeExpression, public typeExpr: ReferenceTypeExpression, public elements: TraitOrImplElement[], span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<ImplDeclarationChild> { if (this.typeParams !== null)
        for (let element of this.typeParams)
            yield element; yield this.traitTypeExpr; yield this.typeExpr; }
}

export type ImplDeclarationParent = never;

export type ImplDeclarationChild = TypeParameter | ReferenceTypeExpression | never;

export function isImplDeclaration(value: any): value is ImplDeclaration { return value.kind === SyntaxKind.ImplDeclaration; }

export function createImplDeclaration(modifiers: Modifiers, typeParams: TypeParameter[] | null, traitTypeExpr: ReferenceTypeExpression, typeExpr: ReferenceTypeExpression, elements: TraitOrImplElement[], span: TextSpan | null = null, parentNode: Syntax | null = null): ImplDeclaration { return new ImplDeclaration(modifiers, typeParams, traitTypeExpr, typeExpr, elements, span, parentNode); }

export class TypeAliasDeclaration extends DeclarationLikeBase {
    readonly kind = SyntaxKind.TypeAliasDeclaration;
    parentNode: null | TypeAliasDeclarationParent = null;
    constructor(public modifiers: Modifiers, public name: Identifier, public typeParams: TypeParameter[] | null, public typeExpr: TypeExpression, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<TypeAliasDeclarationChild> { yield this.name; if (this.typeParams !== null)
        for (let element of this.typeParams)
            yield element; yield this.typeExpr; }
}

export type TypeAliasDeclarationParent = never;

export type TypeAliasDeclarationChild = Identifier | TypeParameter | TypeExpression | never;

export function isTypeAliasDeclaration(value: any): value is TypeAliasDeclaration { return value.kind === SyntaxKind.TypeAliasDeclaration; }

export function createTypeAliasDeclaration(modifiers: Modifiers, name: Identifier, typeParams: TypeParameter[] | null, typeExpr: TypeExpression, span: TextSpan | null = null, parentNode: Syntax | null = null): TypeAliasDeclaration { return new TypeAliasDeclaration(modifiers, name, typeParams, typeExpr, span, parentNode); }

export type EnumDeclarationElement = Identifier;

export function isEnumDeclarationElement(value: Syntax): value is EnumDeclarationElement { return value.kind === SyntaxKind.Identifier; }

export class EnumDeclaration extends DeclarationLikeBase {
    readonly kind = SyntaxKind.EnumDeclaration;
    parentNode: null | EnumDeclarationParent = null;
    constructor(public modifiers: Modifiers, public name: Identifier, public typeParams: TypeParameter[] | null, public members: EnumDeclarationElement[], span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<EnumDeclarationChild> { yield this.name; if (this.typeParams !== null)
        for (let element of this.typeParams)
            yield element; for (let element of this.members)
        yield element; }
}

export type EnumDeclarationParent = never;

export type EnumDeclarationChild = Identifier | TypeParameter | EnumDeclarationElement | never;

export function isEnumDeclaration(value: any): value is EnumDeclaration { return value.kind === SyntaxKind.EnumDeclaration; }

export function createEnumDeclaration(modifiers: Modifiers, name: Identifier, typeParams: TypeParameter[] | null, members: EnumDeclarationElement[], span: TextSpan | null = null, parentNode: Syntax | null = null): EnumDeclaration { return new EnumDeclaration(modifiers, name, typeParams, members, span, parentNode); }

export type RecordDeclartionElement = MacroCall | RecordDeclarationField;

export function isRecordDeclartionElement(value: Syntax): value is RecordDeclartionElement { return value.kind === SyntaxKind.MacroCall || value.kind === SyntaxKind.RecordDeclarationField; }

export class RecordDeclarationField extends SyntaxBase {
    readonly kind = SyntaxKind.RecordDeclarationField;
    parentNode: null | RecordDeclarationFieldParent = null;
    constructor(public name: Identifier, public typeExpr: TypeExpression, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<RecordDeclarationFieldChild> { yield this.name; yield this.typeExpr; }
}

export type RecordDeclarationFieldParent = never;

export type RecordDeclarationFieldChild = Identifier | TypeExpression | never;

export function isRecordDeclarationField(value: any): value is RecordDeclarationField { return value.kind === SyntaxKind.RecordDeclarationField; }

export function createRecordDeclarationField(name: Identifier, typeExpr: TypeExpression, span: TextSpan | null = null, parentNode: Syntax | null = null): RecordDeclarationField { return new RecordDeclarationField(name, typeExpr, span, parentNode); }

export class RecordDeclaration extends DeclarationLikeBase {
    readonly kind = SyntaxKind.RecordDeclaration;
    parentNode: null | RecordDeclarationParent = null;
    constructor(public modifiers: Modifiers, public name: Identifier, public typeParms: TypeParameter[] | null, public members: RecordDeclartionElement[] | null, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<RecordDeclarationChild> { yield this.name; if (this.typeParms !== null)
        for (let element of this.typeParms)
            yield element; if (this.members !== null)
        for (let element of this.members)
            yield element; }
}

export type RecordDeclarationParent = never;

export type RecordDeclarationChild = Identifier | TypeParameter | RecordDeclartionElement | never;

export function isRecordDeclaration(value: any): value is RecordDeclaration { return value.kind === SyntaxKind.RecordDeclaration; }

export function createRecordDeclaration(modifiers: Modifiers, name: Identifier, typeParms: TypeParameter[] | null, members: RecordDeclartionElement[] | null, span: TextSpan | null = null, parentNode: Syntax | null = null): RecordDeclaration { return new RecordDeclaration(modifiers, name, typeParms, members, span, parentNode); }

export class MacroCall extends SyntaxBase {
    readonly kind = SyntaxKind.MacroCall;
    parentNode: null | MacroCallParent = null;
    constructor(public name: Identifier, public text: string, span: TextSpan | null = null, parentNode: Syntax | null = null) { super(span, parentNode); }
    *getChildNodes(): Iterable<MacroCallChild> { yield this.name; }
}

export type MacroCallParent = never;

export type MacroCallChild = Identifier | never;

export function isMacroCall(value: any): value is MacroCall { return value.kind === SyntaxKind.MacroCall; }

export function createMacroCall(name: Identifier, text: string, span: TextSpan | null = null, parentNode: Syntax | null = null): MacroCall { return new MacroCall(name, text, span, parentNode); }

export type Declaration = FunctionDeclaration | VariableDeclaration;

export function isDeclaration(value: Syntax): value is Declaration { return value.kind === SyntaxKind.FunctionDeclaration || value.kind === SyntaxKind.VariableDeclaration; }

export type TypeDeclaration = RecordDeclaration | EnumDeclaration | TypeAliasDeclaration;

export function isTypeDeclaration(value: Syntax): value is TypeDeclaration { return value.kind === SyntaxKind.RecordDeclaration || value.kind === SyntaxKind.EnumDeclaration || value.kind === SyntaxKind.TypeAliasDeclaration; }

export type SourceElement = DeclarationLike | Statement | Module | MacroCall | ImportDeclaration | ExportDeclaration;

export function isSourceElement(value: Syntax): value is SourceElement { return value.kind === SyntaxKind.FunctionDeclaration || value.kind === SyntaxKind.VariableDeclaration || value.kind === SyntaxKind.TraitDeclaration || value.kind === SyntaxKind.ImplDeclaration || value.kind === SyntaxKind.TypeAliasDeclaration || value.kind === SyntaxKind.EnumDeclaration || value.kind === SyntaxKind.RecordDeclaration || value.kind === SyntaxKind.CaseStatement || value.kind === SyntaxKind.ReturnStatement || value.kind === SyntaxKind.ConditionalStatement || value.kind === SyntaxKind.ResumeStatement || value.kind === SyntaxKind.ExpressionStatement || value.kind === SyntaxKind.AssignStatement || value.kind === SyntaxKind.LoopStatement || value.kind === SyntaxKind.Module || value.kind === SyntaxKind.MacroCall || value.kind === SyntaxKind.ImportDeclaration || value.kind === SyntaxKind.ExportDeclaration; }

export function kindToString(kind: SyntaxKind): string { if (SyntaxKind[kind] === undefined)
    throw new Error("The SyntaxKind value that was passed in is not valid."); return SyntaxKind[kind]; }

export type Syntax = EndOfFile | StringLiteral | IntegerLiteral | Identifier | Operator | Assignment | Comma | Semi | Colon | ColonColon | Dot | DotDot | RArrow | RArrowAlt | LArrow | EqSign | GtSign | ExMark | LtSign | VBar | ElseKeyword | IfKeyword | WhereKeyword | QuoteKeyword | FnKeyword | ForeignKeyword | ForKeyword | LetKeyword | ReturnKeyword | LoopKeyword | YieldKeyword | MatchKeyword | ImportKeyword | ExportKeyword | PubKeyword | ModKeyword | MutKeyword | EnumKeyword | StructKeyword | TypeKeyword | TraitKeyword | ImplKeyword | Parenthesized | Braced | Bracketed | SourceFile | QualName | TypeOfExpression | ReferenceTypeExpression | FunctionTypeExpression | LiftedTypeExpression | TypeParameter | BindPattern | TypePattern | ExpressionPattern | TuplePatternElement | TuplePattern | RecordPatternField | RecordPattern | RecordExpression | RecordFieldValue | QuoteExpression | TupleExpression | ReferenceExpression | MemberExpression | FunctionExpression | CallExpression | YieldExpression | MatchArm | MatchExpression | CaseStatementCase | CaseStatement | BlockExpression | ConstantExpression | ReturnStatement | ConditionalCase | ConditionalStatement | ResumeStatement | ExpressionStatement | AssignStatement | LoopStatement | Parameter | Module | FunctionDeclaration | VariableDeclaration | PlainImportSymbol | ImportDeclaration | PlainExportSymbol | ExportDeclaration | TraitDeclaration | ImplDeclaration | TypeAliasDeclaration | EnumDeclaration | RecordDeclarationField | RecordDeclaration | MacroCall;

export const NODE_TYPES = { EndOfFile, StringLiteral, IntegerLiteral, Identifier, Operator, Assignment, Comma, Semi, Colon, ColonColon, Dot, DotDot, RArrow, RArrowAlt, LArrow, EqSign, GtSign, ExMark, LtSign, VBar, ElseKeyword, IfKeyword, WhereKeyword, QuoteKeyword, FnKeyword, ForeignKeyword, ForKeyword, LetKeyword, ReturnKeyword, LoopKeyword, YieldKeyword, MatchKeyword, ImportKeyword, ExportKeyword, PubKeyword, ModKeyword, MutKeyword, EnumKeyword, StructKeyword, TypeKeyword, TraitKeyword, ImplKeyword, Parenthesized, Braced, Bracketed, SourceFile, QualName, TypeOfExpression, ReferenceTypeExpression, FunctionTypeExpression, LiftedTypeExpression, TypeParameter, BindPattern, TypePattern, ExpressionPattern, TuplePatternElement, TuplePattern, RecordPatternField, RecordPattern, RecordExpression, RecordFieldValue, QuoteExpression, TupleExpression, ReferenceExpression, MemberExpression, FunctionExpression, CallExpression, YieldExpression, MatchArm, MatchExpression, CaseStatementCase, CaseStatement, BlockExpression, ConstantExpression, ReturnStatement, ConditionalCase, ConditionalStatement, ResumeStatement, ExpressionStatement, AssignStatement, LoopStatement, Parameter, Module, FunctionDeclaration, VariableDeclaration, PlainImportSymbol, ImportDeclaration, PlainExportSymbol, ExportDeclaration, TraitDeclaration, ImplDeclaration, TypeAliasDeclaration, EnumDeclaration, RecordDeclarationField, RecordDeclaration, MacroCall };

export enum SyntaxKind {
    EndOfFile,
    StringLiteral,
    IntegerLiteral,
    Identifier,
    Operator,
    Assignment,
    Comma,
    Semi,
    Colon,
    ColonColon,
    Dot,
    DotDot,
    RArrow,
    RArrowAlt,
    LArrow,
    EqSign,
    GtSign,
    ExMark,
    LtSign,
    VBar,
    ElseKeyword,
    IfKeyword,
    WhereKeyword,
    QuoteKeyword,
    FnKeyword,
    ForeignKeyword,
    ForKeyword,
    LetKeyword,
    ReturnKeyword,
    LoopKeyword,
    YieldKeyword,
    MatchKeyword,
    ImportKeyword,
    ExportKeyword,
    PubKeyword,
    ModKeyword,
    MutKeyword,
    EnumKeyword,
    StructKeyword,
    TypeKeyword,
    TraitKeyword,
    ImplKeyword,
    Parenthesized,
    Braced,
    Bracketed,
    SourceFile,
    QualName,
    TypeOfExpression,
    ReferenceTypeExpression,
    FunctionTypeExpression,
    LiftedTypeExpression,
    TypeParameter,
    BindPattern,
    TypePattern,
    ExpressionPattern,
    TuplePatternElement,
    TuplePattern,
    RecordPatternField,
    RecordPattern,
    RecordExpression,
    RecordFieldValue,
    QuoteExpression,
    TupleExpression,
    ReferenceExpression,
    MemberExpression,
    FunctionExpression,
    CallExpression,
    YieldExpression,
    MatchArm,
    MatchExpression,
    CaseStatementCase,
    CaseStatement,
    BlockExpression,
    ConstantExpression,
    ReturnStatement,
    ConditionalCase,
    ConditionalStatement,
    ResumeStatement,
    ExpressionStatement,
    AssignStatement,
    LoopStatement,
    Parameter,
    Module,
    FunctionDeclaration,
    VariableDeclaration,
    PlainImportSymbol,
    ImportDeclaration,
    PlainExportSymbol,
    ExportDeclaration,
    TraitDeclaration,
    ImplDeclaration,
    TypeAliasDeclaration,
    EnumDeclaration,
    RecordDeclarationField,
    RecordDeclaration,
    MacroCall
}


export function setParents(node: Syntax, parentNode: Syntax | null = null): void {
  // We cast to any here because parentNode is strongly typed and not generic
  // enough to accept arbitrary AST nodes
  (node as any).parentNode = parentNode;
  for (const childNode of node.getChildNodes()) {
    setParents(childNode, node);
  }
}
