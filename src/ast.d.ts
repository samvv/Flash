
export const enum SyntaxKind {
  EndOfFile = 2,
  FunctionBody = 4,
  BoltStringLiteral = 6,
  BoltIntegerLiteral = 7,
  BoltIdentifier = 9,
  BoltOperator = 10,
  BoltAssignment = 11,
  BoltComma = 12,
  BoltSemi = 13,
  BoltColon = 14,
  BoltDot = 15,
  BoltDotDot = 16,
  BoltRArrow = 17,
  BoltLArrow = 18,
  BoltEqSign = 19,
  BoltGtSign = 20,
  BoltLtSign = 21,
  BoltFnKeyword = 23,
  BoltForeignKeyword = 24,
  BoltForKeyword = 25,
  BoltLetKeyword = 26,
  BoltReturnKeyword = 27,
  BoltLoopKeyword = 28,
  BoltYieldKeyword = 29,
  BoltMatchKeyword = 30,
  BoltImportKeyword = 31,
  BoltPubKeyword = 32,
  BoltModKeyword = 33,
  BoltMutKeyword = 34,
  BoltEnumKeyword = 35,
  BoltStructKeyword = 36,
  BoltTypeKeyword = 37,
  BoltTraitKeyword = 38,
  BoltImplKeyword = 39,
  BoltParenthesized = 41,
  BoltBraced = 42,
  BoltBracketed = 43,
  BoltSourceFile = 44,
  BoltQualName = 45,
  BoltSentence = 46,
  BoltReferenceTypeExpression = 48,
  BoltTypeParameter = 49,
  BoltBindPattern = 51,
  BoltTypePattern = 52,
  BoltExpressionPattern = 53,
  BoltTuplePatternElement = 54,
  BoltTuplePattern = 55,
  BoltRecordPatternField = 56,
  BoltRecordPattern = 57,
  BoltReferenceExpression = 59,
  BoltCallExpression = 60,
  BoltYieldExpression = 61,
  BoltMatchArm = 62,
  BoltMatchExpression = 63,
  BoltCase = 64,
  BoltCaseExpression = 65,
  BoltBlockExpression = 66,
  BoltConstantExpression = 67,
  BoltReturnStatement = 69,
  BoltResumeStatement = 70,
  BoltExpressionStatement = 71,
  BoltParameter = 72,
  BoltModule = 75,
  BoltFunctionDeclaration = 76,
  BoltVariableDeclaration = 77,
  BoltPlainImportSymbol = 79,
  BoltImportDeclaration = 80,
  BoltTraitDeclaration = 81,
  BoltImplDeclaration = 82,
  BoltTypeAliasDeclaration = 83,
  BoltRecordDeclarationField = 84,
  BoltRecordDeclaration = 85,
  JSOperator = 89,
  JSIdentifier = 90,
  JSString = 91,
  JSInteger = 92,
  JSFromKeyword = 93,
  JSReturnKeyword = 94,
  JSTryKeyword = 95,
  JSFinallyKeyword = 96,
  JSCatchKeyword = 97,
  JSImportKeyword = 98,
  JSAsKeyword = 99,
  JSConstKeyword = 100,
  JSLetKeyword = 101,
  JSExportKeyword = 102,
  JSFunctionKeyword = 103,
  JSWhileKeyword = 104,
  JSForKeyword = 105,
  JSCloseBrace = 106,
  JSCloseBracket = 107,
  JSCloseParen = 108,
  JSOpenBrace = 109,
  JSOpenBracket = 110,
  JSOpenParen = 111,
  JSSemi = 112,
  JSComma = 113,
  JSDot = 114,
  JSDotDotDot = 115,
  JSMulOp = 116,
  JSAddOp = 117,
  JSDivOp = 118,
  JSSubOp = 119,
  JSLtOp = 120,
  JSGtOp = 121,
  JSBOrOp = 122,
  JSBXorOp = 123,
  JSBAndOp = 124,
  JSBNotOp = 125,
  JSNotOp = 126,
  JSBindPattern = 128,
  JSConstantExpression = 130,
  JSMemberExpression = 131,
  JSCallExpression = 132,
  JSBinaryExpression = 133,
  JSUnaryExpression = 134,
  JSNewExpression = 135,
  JSSequenceExpression = 136,
  JSConditionalExpression = 137,
  JSLiteralExpression = 139,
  JSReferenceExpression = 140,
  JSCatchBlock = 143,
  JSTryCatchStatement = 144,
  JSExpressionStatement = 145,
  JSConditionalStatement = 146,
  JSReturnStatement = 147,
  JSParameter = 148,
  JSImportStarBinding = 152,
  JSImportAsBinding = 153,
  JSImportDeclaration = 154,
  JSFunctionDeclaration = 155,
  JSArrowFunctionDeclaration = 156,
  JSLetDeclaration = 157,
  JSSourceFile = 158,
}



import { TextSpan } from "./text"

export function setParents(node: Syntax): void;

export type SyntaxRange = [Syntax, Syntax];

interface SyntaxBase {
  kind: SyntaxKind;
  parentNode: Syntax | null;
  span: TextSpan | null;
}
export interface EndOfFile extends SyntaxBase {
  kind: SyntaxKind.EndOfFile;
}

export interface FunctionBody extends SyntaxBase {
  kind: SyntaxKind.FunctionBody;
}

export type BoltToken
  = EndOfFile
  | BoltStringLiteral
  | BoltIntegerLiteral
  | BoltIdentifier
  | BoltOperator
  | BoltAssignment
  | BoltComma
  | BoltSemi
  | BoltColon
  | BoltDot
  | BoltDotDot
  | BoltRArrow
  | BoltLArrow
  | BoltEqSign
  | BoltGtSign
  | BoltLtSign
  | BoltFnKeyword
  | BoltForeignKeyword
  | BoltForKeyword
  | BoltLetKeyword
  | BoltReturnKeyword
  | BoltLoopKeyword
  | BoltYieldKeyword
  | BoltMatchKeyword
  | BoltImportKeyword
  | BoltPubKeyword
  | BoltModKeyword
  | BoltMutKeyword
  | BoltEnumKeyword
  | BoltStructKeyword
  | BoltTypeKeyword
  | BoltTraitKeyword
  | BoltImplKeyword
  | BoltParenthesized
  | BoltBraced
  | BoltBracketed


export interface BoltStringLiteral extends SyntaxBase {
  kind: SyntaxKind.BoltStringLiteral;
  value: string;
}

export interface BoltIntegerLiteral extends SyntaxBase {
  kind: SyntaxKind.BoltIntegerLiteral;
  value: bigint;
}

export type BoltSymbol
  = BoltIdentifier
  | BoltOperator


export interface BoltIdentifier extends SyntaxBase {
  kind: SyntaxKind.BoltIdentifier;
  text: string;
}

export interface BoltOperator extends SyntaxBase {
  kind: SyntaxKind.BoltOperator;
  text: string;
}

export interface BoltAssignment extends SyntaxBase {
  kind: SyntaxKind.BoltAssignment;
  operator: string | null;
}

export interface BoltComma extends SyntaxBase {
  kind: SyntaxKind.BoltComma;
}

export interface BoltSemi extends SyntaxBase {
  kind: SyntaxKind.BoltSemi;
}

export interface BoltColon extends SyntaxBase {
  kind: SyntaxKind.BoltColon;
}

export interface BoltDot extends SyntaxBase {
  kind: SyntaxKind.BoltDot;
}

export interface BoltDotDot extends SyntaxBase {
  kind: SyntaxKind.BoltDotDot;
}

export interface BoltRArrow extends SyntaxBase {
  kind: SyntaxKind.BoltRArrow;
}

export interface BoltLArrow extends SyntaxBase {
  kind: SyntaxKind.BoltLArrow;
}

export interface BoltEqSign extends SyntaxBase {
  kind: SyntaxKind.BoltEqSign;
}

export interface BoltGtSign extends SyntaxBase {
  kind: SyntaxKind.BoltGtSign;
}

export interface BoltLtSign extends SyntaxBase {
  kind: SyntaxKind.BoltLtSign;
}

export type BoltKeyword
  = BoltFnKeyword
  | BoltForeignKeyword
  | BoltForKeyword
  | BoltLetKeyword
  | BoltReturnKeyword
  | BoltLoopKeyword
  | BoltYieldKeyword
  | BoltMatchKeyword
  | BoltImportKeyword
  | BoltPubKeyword
  | BoltModKeyword
  | BoltMutKeyword
  | BoltEnumKeyword
  | BoltStructKeyword
  | BoltTypeKeyword
  | BoltTraitKeyword
  | BoltImplKeyword


export interface BoltFnKeyword extends SyntaxBase {
  kind: SyntaxKind.BoltFnKeyword;
}

export interface BoltForeignKeyword extends SyntaxBase {
  kind: SyntaxKind.BoltForeignKeyword;
}

export interface BoltForKeyword extends SyntaxBase {
  kind: SyntaxKind.BoltForKeyword;
}

export interface BoltLetKeyword extends SyntaxBase {
  kind: SyntaxKind.BoltLetKeyword;
}

export interface BoltReturnKeyword extends SyntaxBase {
  kind: SyntaxKind.BoltReturnKeyword;
}

export interface BoltLoopKeyword extends SyntaxBase {
  kind: SyntaxKind.BoltLoopKeyword;
}

export interface BoltYieldKeyword extends SyntaxBase {
  kind: SyntaxKind.BoltYieldKeyword;
}

export interface BoltMatchKeyword extends SyntaxBase {
  kind: SyntaxKind.BoltMatchKeyword;
}

export interface BoltImportKeyword extends SyntaxBase {
  kind: SyntaxKind.BoltImportKeyword;
}

export interface BoltPubKeyword extends SyntaxBase {
  kind: SyntaxKind.BoltPubKeyword;
}

export interface BoltModKeyword extends SyntaxBase {
  kind: SyntaxKind.BoltModKeyword;
}

export interface BoltMutKeyword extends SyntaxBase {
  kind: SyntaxKind.BoltMutKeyword;
}

export interface BoltEnumKeyword extends SyntaxBase {
  kind: SyntaxKind.BoltEnumKeyword;
}

export interface BoltStructKeyword extends SyntaxBase {
  kind: SyntaxKind.BoltStructKeyword;
}

export interface BoltTypeKeyword extends SyntaxBase {
  kind: SyntaxKind.BoltTypeKeyword;
}

export interface BoltTraitKeyword extends SyntaxBase {
  kind: SyntaxKind.BoltTraitKeyword;
}

export interface BoltImplKeyword extends SyntaxBase {
  kind: SyntaxKind.BoltImplKeyword;
}

export type BoltPunctuated
  = BoltParenthesized
  | BoltBraced
  | BoltBracketed


export interface BoltParenthesized extends SyntaxBase {
  kind: SyntaxKind.BoltParenthesized;
  text: string;
}

export interface BoltBraced extends SyntaxBase {
  kind: SyntaxKind.BoltBraced;
  text: string;
}

export interface BoltBracketed extends SyntaxBase {
  kind: SyntaxKind.BoltBracketed;
  text: string;
}

export interface BoltSourceFile extends SyntaxBase {
  kind: SyntaxKind.BoltSourceFile;
  elements: BoltSourceElement[];
}

export interface BoltQualName extends SyntaxBase {
  kind: SyntaxKind.BoltQualName;
  modulePath: BoltIdentifier[];
  name: BoltSymbol;
}

export interface BoltSentence extends SyntaxBase {
  kind: SyntaxKind.BoltSentence;
  tokens: BoltToken[];
}

export type BoltTypeExpression
  = BoltReferenceTypeExpression


export interface BoltReferenceTypeExpression extends SyntaxBase {
  kind: SyntaxKind.BoltReferenceTypeExpression;
  name: BoltQualName;
  arguments: BoltTypeExpression[] | null;
}

export interface BoltTypeParameter extends SyntaxBase {
  kind: SyntaxKind.BoltTypeParameter;
  index: number;
  name: BoltIdentifier;
  defaultType: BoltTypeExpression | null;
}

export type BoltPattern
  = BoltBindPattern
  | BoltTypePattern
  | BoltExpressionPattern
  | BoltTuplePattern
  | BoltRecordPattern


export interface BoltBindPattern extends SyntaxBase {
  kind: SyntaxKind.BoltBindPattern;
  name: BoltIdentifier;
}

export interface BoltTypePattern extends SyntaxBase {
  kind: SyntaxKind.BoltTypePattern;
  type: BoltTypeExpression;
  nestedPattern: BoltPattern;
}

export interface BoltExpressionPattern extends SyntaxBase {
  kind: SyntaxKind.BoltExpressionPattern;
  expression: BoltExpression;
}

export interface BoltTuplePatternElement extends SyntaxBase {
  kind: SyntaxKind.BoltTuplePatternElement;
  index: number;
  pattern: BoltPattern;
}

export interface BoltTuplePattern extends SyntaxBase {
  kind: SyntaxKind.BoltTuplePattern;
  elements: BoltTuplePatternElement[];
}

export interface BoltRecordPatternField extends SyntaxBase {
  kind: SyntaxKind.BoltRecordPatternField;
  name: BoltIdentifier;
  pattern: BoltPattern;
}

export interface BoltRecordPattern extends SyntaxBase {
  kind: SyntaxKind.BoltRecordPattern;
  name: BoltTypeExpression;
  fields: BoltRecordPatternField[];
}

export type BoltExpression
  = BoltReferenceExpression
  | BoltCallExpression
  | BoltYieldExpression
  | BoltMatchExpression
  | BoltCaseExpression
  | BoltBlockExpression
  | BoltConstantExpression


export interface BoltReferenceExpression extends SyntaxBase {
  kind: SyntaxKind.BoltReferenceExpression;
  name: BoltQualName;
}

export interface BoltCallExpression extends SyntaxBase {
  kind: SyntaxKind.BoltCallExpression;
  operator: BoltExpression;
  operands: BoltExpression[];
}

export interface BoltYieldExpression extends SyntaxBase {
  kind: SyntaxKind.BoltYieldExpression;
  value: BoltExpression;
}

export interface BoltMatchArm extends SyntaxBase {
  kind: SyntaxKind.BoltMatchArm;
  pattern: BoltPattern;
  body: BoltExpression;
}

export interface BoltMatchExpression extends SyntaxBase {
  kind: SyntaxKind.BoltMatchExpression;
  value: BoltExpression;
  arms: BoltMatchArm[];
}

export interface BoltCase extends SyntaxBase {
  kind: SyntaxKind.BoltCase;
  test: BoltExpression;
  result: BoltExpression;
}

export interface BoltCaseExpression extends SyntaxBase {
  kind: SyntaxKind.BoltCaseExpression;
  cases: BoltCase[];
}

export interface BoltBlockExpression extends SyntaxBase {
  kind: SyntaxKind.BoltBlockExpression;
  statements: BoltStatement[];
}

export interface BoltConstantExpression extends SyntaxBase {
  kind: SyntaxKind.BoltConstantExpression;
  value: BoltValue;
}

export type BoltStatement
  = BoltReturnStatement
  | BoltResumeStatement
  | BoltExpressionStatement


export interface BoltReturnStatement extends SyntaxBase {
  kind: SyntaxKind.BoltReturnStatement;
  value: BoltExpression | null;
}

export interface BoltResumeStatement extends SyntaxBase {
  kind: SyntaxKind.BoltResumeStatement;
  value: BoltExpression;
}

export interface BoltExpressionStatement extends SyntaxBase {
  kind: SyntaxKind.BoltExpressionStatement;
  expression: BoltExpression;
}

export interface BoltParameter extends SyntaxBase {
  kind: SyntaxKind.BoltParameter;
  index: number;
  bindings: BoltPattern;
  type: BoltTypeExpression | null;
  defaultValue: BoltExpression | null;
}

export type BoltDeclaration
  = BoltModule
  | BoltFunctionDeclaration
  | BoltVariableDeclaration
  | BoltImportDeclaration
  | BoltTraitDeclaration
  | BoltImplDeclaration
  | BoltTypeAliasDeclaration
  | BoltRecordDeclaration


export const enum BoltDeclarationModifiers {
  Mutable = 1,Public = 2,IsType = 4,IsForeign = 8,}

export interface BoltModule extends SyntaxBase {
  kind: SyntaxKind.BoltModule;
  modifiers: BoltDeclarationModifiers;
  name: BoltQualName;
  elements: BoltSourceElement[];
}

export interface BoltFunctionDeclaration extends SyntaxBase {
  kind: SyntaxKind.BoltFunctionDeclaration;
  modifiers: BoltDeclarationModifiers;
  target: string;
  name: BoltSymbol;
  params: BoltParameter[];
  returnType: BoltTypeExpression | null;
  body: BoltStatement[];
}

export interface BoltVariableDeclaration extends SyntaxBase {
  kind: SyntaxKind.BoltVariableDeclaration;
  modifiers: BoltDeclarationModifiers;
  bindings: BoltPattern;
  type: BoltTypeExpression | null;
  value: BoltExpression | null;
}

export type BoltImportSymbol
  = BoltPlainImportSymbol


export interface BoltPlainImportSymbol extends SyntaxBase {
  kind: SyntaxKind.BoltPlainImportSymbol;
  name: BoltQualName;
}

export interface BoltImportDeclaration extends SyntaxBase {
  kind: SyntaxKind.BoltImportDeclaration;
  file: string;
  symbols: BoltImportSymbol[];
}

export interface BoltTraitDeclaration extends SyntaxBase {
  kind: SyntaxKind.BoltTraitDeclaration;
  modifiers: BoltDeclarationModifiers;
  name: BoltIdentifier;
  typeParams: BoltTypeParameter[] | null;
  elements: BoltDeclaration[];
}

export interface BoltImplDeclaration extends SyntaxBase {
  kind: SyntaxKind.BoltImplDeclaration;
  modifiers: BoltDeclarationModifiers;
  name: BoltIdentifier;
  trait: BoltTypeExpression;
  typeParams: BoltTypeParameter[] | null;
  elements: BoltDeclaration[];
}

export interface BoltTypeAliasDeclaration extends SyntaxBase {
  kind: SyntaxKind.BoltTypeAliasDeclaration;
  modifiers: BoltDeclarationModifiers;
  name: BoltIdentifier;
  typeParams: BoltTypeParameter[] | null;
  typeExpr: BoltTypeExpression;
}

export interface BoltRecordDeclarationField extends SyntaxBase {
  kind: SyntaxKind.BoltRecordDeclarationField;
  name: BoltIdentifier;
  type: BoltTypeExpression;
}

export interface BoltRecordDeclaration extends SyntaxBase {
  kind: SyntaxKind.BoltRecordDeclaration;
  modifiers: BoltDeclarationModifiers;
  name: BoltQualName;
  typeParms: BoltTypeParameter[] | null;
  fields: BoltRecordDeclarationField[];
}

export type BoltSourceElement
  = BoltSentence
  | BoltReturnStatement
  | BoltResumeStatement
  | BoltExpressionStatement
  | BoltModule
  | BoltFunctionDeclaration
  | BoltVariableDeclaration
  | BoltImportDeclaration
  | BoltTraitDeclaration
  | BoltImplDeclaration
  | BoltTypeAliasDeclaration
  | BoltRecordDeclaration


export type JSToken
  = EndOfFile
  | JSOperator
  | JSIdentifier
  | JSString
  | JSInteger
  | JSFromKeyword
  | JSReturnKeyword
  | JSTryKeyword
  | JSFinallyKeyword
  | JSCatchKeyword
  | JSImportKeyword
  | JSAsKeyword
  | JSConstKeyword
  | JSLetKeyword
  | JSExportKeyword
  | JSFunctionKeyword
  | JSWhileKeyword
  | JSForKeyword
  | JSCloseBrace
  | JSCloseBracket
  | JSCloseParen
  | JSOpenBrace
  | JSOpenBracket
  | JSOpenParen
  | JSSemi
  | JSComma
  | JSDot
  | JSDotDotDot
  | JSMulOp
  | JSAddOp
  | JSDivOp
  | JSSubOp
  | JSLtOp
  | JSGtOp
  | JSBOrOp
  | JSBXorOp
  | JSBAndOp
  | JSBNotOp
  | JSNotOp


export interface JSOperator extends SyntaxBase {
  kind: SyntaxKind.JSOperator;
  text: string;
}

export interface JSIdentifier extends SyntaxBase {
  kind: SyntaxKind.JSIdentifier;
  text: string;
}

export interface JSString extends SyntaxBase {
  kind: SyntaxKind.JSString;
  value: string;
}

export interface JSInteger extends SyntaxBase {
  kind: SyntaxKind.JSInteger;
  value: bigint;
}

export interface JSFromKeyword extends SyntaxBase {
  kind: SyntaxKind.JSFromKeyword;
}

export interface JSReturnKeyword extends SyntaxBase {
  kind: SyntaxKind.JSReturnKeyword;
}

export interface JSTryKeyword extends SyntaxBase {
  kind: SyntaxKind.JSTryKeyword;
}

export interface JSFinallyKeyword extends SyntaxBase {
  kind: SyntaxKind.JSFinallyKeyword;
}

export interface JSCatchKeyword extends SyntaxBase {
  kind: SyntaxKind.JSCatchKeyword;
}

export interface JSImportKeyword extends SyntaxBase {
  kind: SyntaxKind.JSImportKeyword;
}

export interface JSAsKeyword extends SyntaxBase {
  kind: SyntaxKind.JSAsKeyword;
}

export interface JSConstKeyword extends SyntaxBase {
  kind: SyntaxKind.JSConstKeyword;
}

export interface JSLetKeyword extends SyntaxBase {
  kind: SyntaxKind.JSLetKeyword;
}

export interface JSExportKeyword extends SyntaxBase {
  kind: SyntaxKind.JSExportKeyword;
}

export interface JSFunctionKeyword extends SyntaxBase {
  kind: SyntaxKind.JSFunctionKeyword;
}

export interface JSWhileKeyword extends SyntaxBase {
  kind: SyntaxKind.JSWhileKeyword;
}

export interface JSForKeyword extends SyntaxBase {
  kind: SyntaxKind.JSForKeyword;
}

export interface JSCloseBrace extends SyntaxBase {
  kind: SyntaxKind.JSCloseBrace;
}

export interface JSCloseBracket extends SyntaxBase {
  kind: SyntaxKind.JSCloseBracket;
}

export interface JSCloseParen extends SyntaxBase {
  kind: SyntaxKind.JSCloseParen;
}

export interface JSOpenBrace extends SyntaxBase {
  kind: SyntaxKind.JSOpenBrace;
}

export interface JSOpenBracket extends SyntaxBase {
  kind: SyntaxKind.JSOpenBracket;
}

export interface JSOpenParen extends SyntaxBase {
  kind: SyntaxKind.JSOpenParen;
}

export interface JSSemi extends SyntaxBase {
  kind: SyntaxKind.JSSemi;
}

export interface JSComma extends SyntaxBase {
  kind: SyntaxKind.JSComma;
}

export interface JSDot extends SyntaxBase {
  kind: SyntaxKind.JSDot;
}

export interface JSDotDotDot extends SyntaxBase {
  kind: SyntaxKind.JSDotDotDot;
}

export interface JSMulOp extends SyntaxBase {
  kind: SyntaxKind.JSMulOp;
}

export interface JSAddOp extends SyntaxBase {
  kind: SyntaxKind.JSAddOp;
}

export interface JSDivOp extends SyntaxBase {
  kind: SyntaxKind.JSDivOp;
}

export interface JSSubOp extends SyntaxBase {
  kind: SyntaxKind.JSSubOp;
}

export interface JSLtOp extends SyntaxBase {
  kind: SyntaxKind.JSLtOp;
}

export interface JSGtOp extends SyntaxBase {
  kind: SyntaxKind.JSGtOp;
}

export interface JSBOrOp extends SyntaxBase {
  kind: SyntaxKind.JSBOrOp;
}

export interface JSBXorOp extends SyntaxBase {
  kind: SyntaxKind.JSBXorOp;
}

export interface JSBAndOp extends SyntaxBase {
  kind: SyntaxKind.JSBAndOp;
}

export interface JSBNotOp extends SyntaxBase {
  kind: SyntaxKind.JSBNotOp;
}

export interface JSNotOp extends SyntaxBase {
  kind: SyntaxKind.JSNotOp;
}

export type JSPattern
  = JSBindPattern


export interface JSBindPattern extends SyntaxBase {
  kind: SyntaxKind.JSBindPattern;
  name: JSIdentifier;
}

export type JSExpression
  = JSConstantExpression
  | JSMemberExpression
  | JSCallExpression
  | JSBinaryExpression
  | JSUnaryExpression
  | JSNewExpression
  | JSSequenceExpression
  | JSConditionalExpression
  | JSLiteralExpression
  | JSReferenceExpression


export interface JSConstantExpression extends SyntaxBase {
  kind: SyntaxKind.JSConstantExpression;
  value: BoltValue;
}

export interface JSMemberExpression extends SyntaxBase {
  kind: SyntaxKind.JSMemberExpression;
  value: JSExpression;
  property: JSIdentifier;
}

export interface JSCallExpression extends SyntaxBase {
  kind: SyntaxKind.JSCallExpression;
  operator: JSExpression;
  operands: JSExpression[];
}

export interface JSBinaryExpression extends SyntaxBase {
  kind: SyntaxKind.JSBinaryExpression;
  left: JSExpression;
  operator: JSOperator;
  right: JSExpression;
}

export interface JSUnaryExpression extends SyntaxBase {
  kind: SyntaxKind.JSUnaryExpression;
  operator: JSOperator;
  operand: JSExpression;
}

export interface JSNewExpression extends SyntaxBase {
  kind: SyntaxKind.JSNewExpression;
  target: JSExpression;
  arguments: JSExpression[];
}

export interface JSSequenceExpression extends SyntaxBase {
  kind: SyntaxKind.JSSequenceExpression;
  expressions: JSExpression[];
}

export interface JSConditionalExpression extends SyntaxBase {
  kind: SyntaxKind.JSConditionalExpression;
  test: JSExpression;
  consequent: JSExpression;
  alternate: JSExpression;
}

export interface JSLiteralExpression extends SyntaxBase {
  kind: SyntaxKind.JSLiteralExpression;
  value: JSValue;
}

export interface JSReferenceExpression extends SyntaxBase {
  kind: SyntaxKind.JSReferenceExpression;
  name: string;
}

export type JSSourceElement
  = JSExpressionStatement
  | JSConditionalStatement
  | JSReturnStatement
  | JSImportDeclaration
  | JSFunctionDeclaration
  | JSArrowFunctionDeclaration
  | JSLetDeclaration


export type JSStatement
  = JSExpressionStatement
  | JSConditionalStatement
  | JSReturnStatement


export interface JSCatchBlock extends SyntaxBase {
  kind: SyntaxKind.JSCatchBlock;
  bindings: JSPattern | null;
  elements: JSSourceElement[];
}

export interface JSTryCatchStatement extends SyntaxBase {
  kind: SyntaxKind.JSTryCatchStatement;
  tryBlock: JSSourceElement[];
  catchBlock: JSCatchBlock | null;
  finalBlock: JSSourceElement[] | null;
}

export interface JSExpressionStatement extends SyntaxBase {
  kind: SyntaxKind.JSExpressionStatement;
  expression: JSExpression;
}

export interface JSConditionalStatement extends SyntaxBase {
  kind: SyntaxKind.JSConditionalStatement;
  test: JSExpression;
  consequent: JSStatement[];
  alternate: JSStatement[];
}

export interface JSReturnStatement extends SyntaxBase {
  kind: SyntaxKind.JSReturnStatement;
  value: JSExpression | null;
}

export interface JSParameter extends SyntaxBase {
  kind: SyntaxKind.JSParameter;
  index: number;
  bindings: JSPattern;
  defaultValue: JSExpression | null;
}

export type JSDeclaration
  = JSImportDeclaration
  | JSFunctionDeclaration
  | JSArrowFunctionDeclaration
  | JSLetDeclaration


export const enum JSDeclarationModifiers {
  IsExported = 1,}

export type JSImportBinding
  = JSImportStarBinding
  | JSImportAsBinding


export interface JSImportStarBinding extends SyntaxBase {
  kind: SyntaxKind.JSImportStarBinding;
  local: JSIdentifier;
}

export interface JSImportAsBinding extends SyntaxBase {
  kind: SyntaxKind.JSImportAsBinding;
  remote: JSIdentifier;
  local: JSIdentifier | null;
}

export interface JSImportDeclaration extends SyntaxBase {
  kind: SyntaxKind.JSImportDeclaration;
  bindings: JSImportBinding[];
  filename: JSString;
}

export interface JSFunctionDeclaration extends SyntaxBase {
  kind: SyntaxKind.JSFunctionDeclaration;
  modifiers: JSDeclarationModifiers;
  name: JSIdentifier;
  params: JSParameter[];
  body: JSStatement[];
}

export interface JSArrowFunctionDeclaration extends SyntaxBase {
  kind: SyntaxKind.JSArrowFunctionDeclaration;
  name: JSIdentifier;
  params: JSParameter[];
  body: JSExpression;
}

export interface JSLetDeclaration extends SyntaxBase {
  kind: SyntaxKind.JSLetDeclaration;
  bindings: JSPattern;
  value: JSExpression | null;
}

export interface JSSourceFile extends SyntaxBase {
  kind: SyntaxKind.JSSourceFile;
  elements: JSSourceElement[];
}

export type BoltSyntax
  = BoltStringLiteral
  | BoltIntegerLiteral
  | BoltIdentifier
  | BoltOperator
  | BoltAssignment
  | BoltComma
  | BoltSemi
  | BoltColon
  | BoltDot
  | BoltDotDot
  | BoltRArrow
  | BoltLArrow
  | BoltEqSign
  | BoltGtSign
  | BoltLtSign
  | BoltFnKeyword
  | BoltForeignKeyword
  | BoltForKeyword
  | BoltLetKeyword
  | BoltReturnKeyword
  | BoltLoopKeyword
  | BoltYieldKeyword
  | BoltMatchKeyword
  | BoltImportKeyword
  | BoltPubKeyword
  | BoltModKeyword
  | BoltMutKeyword
  | BoltEnumKeyword
  | BoltStructKeyword
  | BoltTypeKeyword
  | BoltTraitKeyword
  | BoltImplKeyword
  | BoltParenthesized
  | BoltBraced
  | BoltBracketed
  | BoltSourceFile
  | BoltQualName
  | BoltSentence
  | BoltReferenceTypeExpression
  | BoltTypeParameter
  | BoltBindPattern
  | BoltTypePattern
  | BoltExpressionPattern
  | BoltTuplePatternElement
  | BoltTuplePattern
  | BoltRecordPatternField
  | BoltRecordPattern
  | BoltReferenceExpression
  | BoltCallExpression
  | BoltYieldExpression
  | BoltMatchArm
  | BoltMatchExpression
  | BoltCase
  | BoltCaseExpression
  | BoltBlockExpression
  | BoltConstantExpression
  | BoltReturnStatement
  | BoltResumeStatement
  | BoltExpressionStatement
  | BoltParameter
  | BoltModule
  | BoltFunctionDeclaration
  | BoltVariableDeclaration
  | BoltPlainImportSymbol
  | BoltImportDeclaration
  | BoltTraitDeclaration
  | BoltImplDeclaration
  | BoltTypeAliasDeclaration
  | BoltRecordDeclarationField
  | BoltRecordDeclaration


export type JSSyntax
  = JSOperator
  | JSIdentifier
  | JSString
  | JSInteger
  | JSFromKeyword
  | JSReturnKeyword
  | JSTryKeyword
  | JSFinallyKeyword
  | JSCatchKeyword
  | JSImportKeyword
  | JSAsKeyword
  | JSConstKeyword
  | JSLetKeyword
  | JSExportKeyword
  | JSFunctionKeyword
  | JSWhileKeyword
  | JSForKeyword
  | JSCloseBrace
  | JSCloseBracket
  | JSCloseParen
  | JSOpenBrace
  | JSOpenBracket
  | JSOpenParen
  | JSSemi
  | JSComma
  | JSDot
  | JSDotDotDot
  | JSMulOp
  | JSAddOp
  | JSDivOp
  | JSSubOp
  | JSLtOp
  | JSGtOp
  | JSBOrOp
  | JSBXorOp
  | JSBAndOp
  | JSBNotOp
  | JSNotOp
  | JSBindPattern
  | JSConstantExpression
  | JSMemberExpression
  | JSCallExpression
  | JSBinaryExpression
  | JSUnaryExpression
  | JSNewExpression
  | JSSequenceExpression
  | JSConditionalExpression
  | JSLiteralExpression
  | JSReferenceExpression
  | JSCatchBlock
  | JSTryCatchStatement
  | JSExpressionStatement
  | JSConditionalStatement
  | JSReturnStatement
  | JSParameter
  | JSImportStarBinding
  | JSImportAsBinding
  | JSImportDeclaration
  | JSFunctionDeclaration
  | JSArrowFunctionDeclaration
  | JSLetDeclaration
  | JSSourceFile


export type Syntax
  = EndOfFile
  | FunctionBody
  | BoltStringLiteral
  | BoltIntegerLiteral
  | BoltIdentifier
  | BoltOperator
  | BoltAssignment
  | BoltComma
  | BoltSemi
  | BoltColon
  | BoltDot
  | BoltDotDot
  | BoltRArrow
  | BoltLArrow
  | BoltEqSign
  | BoltGtSign
  | BoltLtSign
  | BoltFnKeyword
  | BoltForeignKeyword
  | BoltForKeyword
  | BoltLetKeyword
  | BoltReturnKeyword
  | BoltLoopKeyword
  | BoltYieldKeyword
  | BoltMatchKeyword
  | BoltImportKeyword
  | BoltPubKeyword
  | BoltModKeyword
  | BoltMutKeyword
  | BoltEnumKeyword
  | BoltStructKeyword
  | BoltTypeKeyword
  | BoltTraitKeyword
  | BoltImplKeyword
  | BoltParenthesized
  | BoltBraced
  | BoltBracketed
  | BoltSourceFile
  | BoltQualName
  | BoltSentence
  | BoltReferenceTypeExpression
  | BoltTypeParameter
  | BoltBindPattern
  | BoltTypePattern
  | BoltExpressionPattern
  | BoltTuplePatternElement
  | BoltTuplePattern
  | BoltRecordPatternField
  | BoltRecordPattern
  | BoltReferenceExpression
  | BoltCallExpression
  | BoltYieldExpression
  | BoltMatchArm
  | BoltMatchExpression
  | BoltCase
  | BoltCaseExpression
  | BoltBlockExpression
  | BoltConstantExpression
  | BoltReturnStatement
  | BoltResumeStatement
  | BoltExpressionStatement
  | BoltParameter
  | BoltModule
  | BoltFunctionDeclaration
  | BoltVariableDeclaration
  | BoltPlainImportSymbol
  | BoltImportDeclaration
  | BoltTraitDeclaration
  | BoltImplDeclaration
  | BoltTypeAliasDeclaration
  | BoltRecordDeclarationField
  | BoltRecordDeclaration
  | JSOperator
  | JSIdentifier
  | JSString
  | JSInteger
  | JSFromKeyword
  | JSReturnKeyword
  | JSTryKeyword
  | JSFinallyKeyword
  | JSCatchKeyword
  | JSImportKeyword
  | JSAsKeyword
  | JSConstKeyword
  | JSLetKeyword
  | JSExportKeyword
  | JSFunctionKeyword
  | JSWhileKeyword
  | JSForKeyword
  | JSCloseBrace
  | JSCloseBracket
  | JSCloseParen
  | JSOpenBrace
  | JSOpenBracket
  | JSOpenParen
  | JSSemi
  | JSComma
  | JSDot
  | JSDotDotDot
  | JSMulOp
  | JSAddOp
  | JSDivOp
  | JSSubOp
  | JSLtOp
  | JSGtOp
  | JSBOrOp
  | JSBXorOp
  | JSBAndOp
  | JSBNotOp
  | JSNotOp
  | JSBindPattern
  | JSConstantExpression
  | JSMemberExpression
  | JSCallExpression
  | JSBinaryExpression
  | JSUnaryExpression
  | JSNewExpression
  | JSSequenceExpression
  | JSConditionalExpression
  | JSLiteralExpression
  | JSReferenceExpression
  | JSCatchBlock
  | JSTryCatchStatement
  | JSExpressionStatement
  | JSConditionalStatement
  | JSReturnStatement
  | JSParameter
  | JSImportStarBinding
  | JSImportAsBinding
  | JSImportDeclaration
  | JSFunctionDeclaration
  | JSArrowFunctionDeclaration
  | JSLetDeclaration
  | JSSourceFile


export function kindToString(kind: SyntaxKind): string;

export function createEndOfFile(span?: TextSpan | null): EndOfFile;
export function createFunctionBody(span?: TextSpan | null): FunctionBody;
export function createBoltStringLiteral(value: string, span?: TextSpan | null): BoltStringLiteral;
export function createBoltIntegerLiteral(value: bigint, span?: TextSpan | null): BoltIntegerLiteral;
export function createBoltIdentifier(text: string, span?: TextSpan | null): BoltIdentifier;
export function createBoltOperator(text: string, span?: TextSpan | null): BoltOperator;
export function createBoltAssignment(operator: string | null, span?: TextSpan | null): BoltAssignment;
export function createBoltComma(span?: TextSpan | null): BoltComma;
export function createBoltSemi(span?: TextSpan | null): BoltSemi;
export function createBoltColon(span?: TextSpan | null): BoltColon;
export function createBoltDot(span?: TextSpan | null): BoltDot;
export function createBoltDotDot(span?: TextSpan | null): BoltDotDot;
export function createBoltRArrow(span?: TextSpan | null): BoltRArrow;
export function createBoltLArrow(span?: TextSpan | null): BoltLArrow;
export function createBoltEqSign(span?: TextSpan | null): BoltEqSign;
export function createBoltGtSign(span?: TextSpan | null): BoltGtSign;
export function createBoltLtSign(span?: TextSpan | null): BoltLtSign;
export function createBoltFnKeyword(span?: TextSpan | null): BoltFnKeyword;
export function createBoltForeignKeyword(span?: TextSpan | null): BoltForeignKeyword;
export function createBoltForKeyword(span?: TextSpan | null): BoltForKeyword;
export function createBoltLetKeyword(span?: TextSpan | null): BoltLetKeyword;
export function createBoltReturnKeyword(span?: TextSpan | null): BoltReturnKeyword;
export function createBoltLoopKeyword(span?: TextSpan | null): BoltLoopKeyword;
export function createBoltYieldKeyword(span?: TextSpan | null): BoltYieldKeyword;
export function createBoltMatchKeyword(span?: TextSpan | null): BoltMatchKeyword;
export function createBoltImportKeyword(span?: TextSpan | null): BoltImportKeyword;
export function createBoltPubKeyword(span?: TextSpan | null): BoltPubKeyword;
export function createBoltModKeyword(span?: TextSpan | null): BoltModKeyword;
export function createBoltMutKeyword(span?: TextSpan | null): BoltMutKeyword;
export function createBoltEnumKeyword(span?: TextSpan | null): BoltEnumKeyword;
export function createBoltStructKeyword(span?: TextSpan | null): BoltStructKeyword;
export function createBoltTypeKeyword(span?: TextSpan | null): BoltTypeKeyword;
export function createBoltTraitKeyword(span?: TextSpan | null): BoltTraitKeyword;
export function createBoltImplKeyword(span?: TextSpan | null): BoltImplKeyword;
export function createBoltParenthesized(text: string, span?: TextSpan | null): BoltParenthesized;
export function createBoltBraced(text: string, span?: TextSpan | null): BoltBraced;
export function createBoltBracketed(text: string, span?: TextSpan | null): BoltBracketed;
export function createBoltSourceFile(elements: BoltSourceElement[], span?: TextSpan | null): BoltSourceFile;
export function createBoltQualName(modulePath: BoltIdentifier[], name: BoltSymbol, span?: TextSpan | null): BoltQualName;
export function createBoltSentence(tokens: BoltToken[], span?: TextSpan | null): BoltSentence;
export function createBoltReferenceTypeExpression(name: BoltQualName, arguments: BoltTypeExpression[] | null, span?: TextSpan | null): BoltReferenceTypeExpression;
export function createBoltTypeParameter(index: number, name: BoltIdentifier, defaultType: BoltTypeExpression | null, span?: TextSpan | null): BoltTypeParameter;
export function createBoltBindPattern(name: BoltIdentifier, span?: TextSpan | null): BoltBindPattern;
export function createBoltTypePattern(type: BoltTypeExpression, nestedPattern: BoltPattern, span?: TextSpan | null): BoltTypePattern;
export function createBoltExpressionPattern(expression: BoltExpression, span?: TextSpan | null): BoltExpressionPattern;
export function createBoltTuplePatternElement(index: number, pattern: BoltPattern, span?: TextSpan | null): BoltTuplePatternElement;
export function createBoltTuplePattern(elements: BoltTuplePatternElement[], span?: TextSpan | null): BoltTuplePattern;
export function createBoltRecordPatternField(name: BoltIdentifier, pattern: BoltPattern, span?: TextSpan | null): BoltRecordPatternField;
export function createBoltRecordPattern(name: BoltTypeExpression, fields: BoltRecordPatternField[], span?: TextSpan | null): BoltRecordPattern;
export function createBoltReferenceExpression(name: BoltQualName, span?: TextSpan | null): BoltReferenceExpression;
export function createBoltCallExpression(operator: BoltExpression, operands: BoltExpression[], span?: TextSpan | null): BoltCallExpression;
export function createBoltYieldExpression(value: BoltExpression, span?: TextSpan | null): BoltYieldExpression;
export function createBoltMatchArm(pattern: BoltPattern, body: BoltExpression, span?: TextSpan | null): BoltMatchArm;
export function createBoltMatchExpression(value: BoltExpression, arms: BoltMatchArm[], span?: TextSpan | null): BoltMatchExpression;
export function createBoltCase(test: BoltExpression, result: BoltExpression, span?: TextSpan | null): BoltCase;
export function createBoltCaseExpression(cases: BoltCase[], span?: TextSpan | null): BoltCaseExpression;
export function createBoltBlockExpression(statements: BoltStatement[], span?: TextSpan | null): BoltBlockExpression;
export function createBoltConstantExpression(value: BoltValue, span?: TextSpan | null): BoltConstantExpression;
export function createBoltReturnStatement(value: BoltExpression | null, span?: TextSpan | null): BoltReturnStatement;
export function createBoltResumeStatement(value: BoltExpression, span?: TextSpan | null): BoltResumeStatement;
export function createBoltExpressionStatement(expression: BoltExpression, span?: TextSpan | null): BoltExpressionStatement;
export function createBoltParameter(index: number, bindings: BoltPattern, type: BoltTypeExpression | null, defaultValue: BoltExpression | null, span?: TextSpan | null): BoltParameter;
export function createBoltModule(modifiers: BoltDeclarationModifiers, name: BoltQualName, elements: BoltSourceElement[], span?: TextSpan | null): BoltModule;
export function createBoltFunctionDeclaration(modifiers: BoltDeclarationModifiers, target: string, name: BoltSymbol, params: BoltParameter[], returnType: BoltTypeExpression | null, body: BoltStatement[], span?: TextSpan | null): BoltFunctionDeclaration;
export function createBoltVariableDeclaration(modifiers: BoltDeclarationModifiers, bindings: BoltPattern, type: BoltTypeExpression | null, value: BoltExpression | null, span?: TextSpan | null): BoltVariableDeclaration;
export function createBoltPlainImportSymbol(name: BoltQualName, span?: TextSpan | null): BoltPlainImportSymbol;
export function createBoltImportDeclaration(file: string, symbols: BoltImportSymbol[], span?: TextSpan | null): BoltImportDeclaration;
export function createBoltTraitDeclaration(modifiers: BoltDeclarationModifiers, name: BoltIdentifier, typeParams: BoltTypeParameter[] | null, elements: BoltDeclaration[], span?: TextSpan | null): BoltTraitDeclaration;
export function createBoltImplDeclaration(modifiers: BoltDeclarationModifiers, name: BoltIdentifier, trait: BoltTypeExpression, typeParams: BoltTypeParameter[] | null, elements: BoltDeclaration[], span?: TextSpan | null): BoltImplDeclaration;
export function createBoltTypeAliasDeclaration(modifiers: BoltDeclarationModifiers, name: BoltIdentifier, typeParams: BoltTypeParameter[] | null, typeExpr: BoltTypeExpression, span?: TextSpan | null): BoltTypeAliasDeclaration;
export function createBoltRecordDeclarationField(name: BoltIdentifier, type: BoltTypeExpression, span?: TextSpan | null): BoltRecordDeclarationField;
export function createBoltRecordDeclaration(modifiers: BoltDeclarationModifiers, name: BoltQualName, typeParms: BoltTypeParameter[] | null, fields: BoltRecordDeclarationField[], span?: TextSpan | null): BoltRecordDeclaration;
export function createJSOperator(text: string, span?: TextSpan | null): JSOperator;
export function createJSIdentifier(text: string, span?: TextSpan | null): JSIdentifier;
export function createJSString(value: string, span?: TextSpan | null): JSString;
export function createJSInteger(value: bigint, span?: TextSpan | null): JSInteger;
export function createJSFromKeyword(span?: TextSpan | null): JSFromKeyword;
export function createJSReturnKeyword(span?: TextSpan | null): JSReturnKeyword;
export function createJSTryKeyword(span?: TextSpan | null): JSTryKeyword;
export function createJSFinallyKeyword(span?: TextSpan | null): JSFinallyKeyword;
export function createJSCatchKeyword(span?: TextSpan | null): JSCatchKeyword;
export function createJSImportKeyword(span?: TextSpan | null): JSImportKeyword;
export function createJSAsKeyword(span?: TextSpan | null): JSAsKeyword;
export function createJSConstKeyword(span?: TextSpan | null): JSConstKeyword;
export function createJSLetKeyword(span?: TextSpan | null): JSLetKeyword;
export function createJSExportKeyword(span?: TextSpan | null): JSExportKeyword;
export function createJSFunctionKeyword(span?: TextSpan | null): JSFunctionKeyword;
export function createJSWhileKeyword(span?: TextSpan | null): JSWhileKeyword;
export function createJSForKeyword(span?: TextSpan | null): JSForKeyword;
export function createJSCloseBrace(span?: TextSpan | null): JSCloseBrace;
export function createJSCloseBracket(span?: TextSpan | null): JSCloseBracket;
export function createJSCloseParen(span?: TextSpan | null): JSCloseParen;
export function createJSOpenBrace(span?: TextSpan | null): JSOpenBrace;
export function createJSOpenBracket(span?: TextSpan | null): JSOpenBracket;
export function createJSOpenParen(span?: TextSpan | null): JSOpenParen;
export function createJSSemi(span?: TextSpan | null): JSSemi;
export function createJSComma(span?: TextSpan | null): JSComma;
export function createJSDot(span?: TextSpan | null): JSDot;
export function createJSDotDotDot(span?: TextSpan | null): JSDotDotDot;
export function createJSMulOp(span?: TextSpan | null): JSMulOp;
export function createJSAddOp(span?: TextSpan | null): JSAddOp;
export function createJSDivOp(span?: TextSpan | null): JSDivOp;
export function createJSSubOp(span?: TextSpan | null): JSSubOp;
export function createJSLtOp(span?: TextSpan | null): JSLtOp;
export function createJSGtOp(span?: TextSpan | null): JSGtOp;
export function createJSBOrOp(span?: TextSpan | null): JSBOrOp;
export function createJSBXorOp(span?: TextSpan | null): JSBXorOp;
export function createJSBAndOp(span?: TextSpan | null): JSBAndOp;
export function createJSBNotOp(span?: TextSpan | null): JSBNotOp;
export function createJSNotOp(span?: TextSpan | null): JSNotOp;
export function createJSBindPattern(name: JSIdentifier, span?: TextSpan | null): JSBindPattern;
export function createJSConstantExpression(value: BoltValue, span?: TextSpan | null): JSConstantExpression;
export function createJSMemberExpression(value: JSExpression, property: JSIdentifier, span?: TextSpan | null): JSMemberExpression;
export function createJSCallExpression(operator: JSExpression, operands: JSExpression[], span?: TextSpan | null): JSCallExpression;
export function createJSBinaryExpression(left: JSExpression, operator: JSOperator, right: JSExpression, span?: TextSpan | null): JSBinaryExpression;
export function createJSUnaryExpression(operator: JSOperator, operand: JSExpression, span?: TextSpan | null): JSUnaryExpression;
export function createJSNewExpression(target: JSExpression, arguments: JSExpression[], span?: TextSpan | null): JSNewExpression;
export function createJSSequenceExpression(expressions: JSExpression[], span?: TextSpan | null): JSSequenceExpression;
export function createJSConditionalExpression(test: JSExpression, consequent: JSExpression, alternate: JSExpression, span?: TextSpan | null): JSConditionalExpression;
export function createJSLiteralExpression(value: JSValue, span?: TextSpan | null): JSLiteralExpression;
export function createJSReferenceExpression(name: string, span?: TextSpan | null): JSReferenceExpression;
export function createJSCatchBlock(bindings: JSPattern | null, elements: JSSourceElement[], span?: TextSpan | null): JSCatchBlock;
export function createJSTryCatchStatement(tryBlock: JSSourceElement[], catchBlock: JSCatchBlock | null, finalBlock: JSSourceElement[] | null, span?: TextSpan | null): JSTryCatchStatement;
export function createJSExpressionStatement(expression: JSExpression, span?: TextSpan | null): JSExpressionStatement;
export function createJSConditionalStatement(test: JSExpression, consequent: JSStatement[], alternate: JSStatement[], span?: TextSpan | null): JSConditionalStatement;
export function createJSReturnStatement(value: JSExpression | null, span?: TextSpan | null): JSReturnStatement;
export function createJSParameter(index: number, bindings: JSPattern, defaultValue: JSExpression | null, span?: TextSpan | null): JSParameter;
export function createJSImportStarBinding(local: JSIdentifier, span?: TextSpan | null): JSImportStarBinding;
export function createJSImportAsBinding(remote: JSIdentifier, local: JSIdentifier | null, span?: TextSpan | null): JSImportAsBinding;
export function createJSImportDeclaration(bindings: JSImportBinding[], filename: JSString, span?: TextSpan | null): JSImportDeclaration;
export function createJSFunctionDeclaration(modifiers: JSDeclarationModifiers, name: JSIdentifier, params: JSParameter[], body: JSStatement[], span?: TextSpan | null): JSFunctionDeclaration;
export function createJSArrowFunctionDeclaration(name: JSIdentifier, params: JSParameter[], body: JSExpression, span?: TextSpan | null): JSArrowFunctionDeclaration;
export function createJSLetDeclaration(bindings: JSPattern, value: JSExpression | null, span?: TextSpan | null): JSLetDeclaration;
export function createJSSourceFile(elements: JSSourceElement[], span?: TextSpan | null): JSSourceFile;

export function isEndOfFile(value: any): value is EndOfFile;
export function isFunctionBody(value: any): value is FunctionBody;
export function isBoltToken(value: any): value is BoltToken;
export function isBoltStringLiteral(value: any): value is BoltStringLiteral;
export function isBoltIntegerLiteral(value: any): value is BoltIntegerLiteral;
export function isBoltSymbol(value: any): value is BoltSymbol;
export function isBoltIdentifier(value: any): value is BoltIdentifier;
export function isBoltOperator(value: any): value is BoltOperator;
export function isBoltAssignment(value: any): value is BoltAssignment;
export function isBoltComma(value: any): value is BoltComma;
export function isBoltSemi(value: any): value is BoltSemi;
export function isBoltColon(value: any): value is BoltColon;
export function isBoltDot(value: any): value is BoltDot;
export function isBoltDotDot(value: any): value is BoltDotDot;
export function isBoltRArrow(value: any): value is BoltRArrow;
export function isBoltLArrow(value: any): value is BoltLArrow;
export function isBoltEqSign(value: any): value is BoltEqSign;
export function isBoltGtSign(value: any): value is BoltGtSign;
export function isBoltLtSign(value: any): value is BoltLtSign;
export function isBoltKeyword(value: any): value is BoltKeyword;
export function isBoltFnKeyword(value: any): value is BoltFnKeyword;
export function isBoltForeignKeyword(value: any): value is BoltForeignKeyword;
export function isBoltForKeyword(value: any): value is BoltForKeyword;
export function isBoltLetKeyword(value: any): value is BoltLetKeyword;
export function isBoltReturnKeyword(value: any): value is BoltReturnKeyword;
export function isBoltLoopKeyword(value: any): value is BoltLoopKeyword;
export function isBoltYieldKeyword(value: any): value is BoltYieldKeyword;
export function isBoltMatchKeyword(value: any): value is BoltMatchKeyword;
export function isBoltImportKeyword(value: any): value is BoltImportKeyword;
export function isBoltPubKeyword(value: any): value is BoltPubKeyword;
export function isBoltModKeyword(value: any): value is BoltModKeyword;
export function isBoltMutKeyword(value: any): value is BoltMutKeyword;
export function isBoltEnumKeyword(value: any): value is BoltEnumKeyword;
export function isBoltStructKeyword(value: any): value is BoltStructKeyword;
export function isBoltTypeKeyword(value: any): value is BoltTypeKeyword;
export function isBoltTraitKeyword(value: any): value is BoltTraitKeyword;
export function isBoltImplKeyword(value: any): value is BoltImplKeyword;
export function isBoltPunctuated(value: any): value is BoltPunctuated;
export function isBoltParenthesized(value: any): value is BoltParenthesized;
export function isBoltBraced(value: any): value is BoltBraced;
export function isBoltBracketed(value: any): value is BoltBracketed;
export function isBoltSourceFile(value: any): value is BoltSourceFile;
export function isBoltQualName(value: any): value is BoltQualName;
export function isBoltSentence(value: any): value is BoltSentence;
export function isBoltTypeExpression(value: any): value is BoltTypeExpression;
export function isBoltReferenceTypeExpression(value: any): value is BoltReferenceTypeExpression;
export function isBoltTypeParameter(value: any): value is BoltTypeParameter;
export function isBoltPattern(value: any): value is BoltPattern;
export function isBoltBindPattern(value: any): value is BoltBindPattern;
export function isBoltTypePattern(value: any): value is BoltTypePattern;
export function isBoltExpressionPattern(value: any): value is BoltExpressionPattern;
export function isBoltTuplePatternElement(value: any): value is BoltTuplePatternElement;
export function isBoltTuplePattern(value: any): value is BoltTuplePattern;
export function isBoltRecordPatternField(value: any): value is BoltRecordPatternField;
export function isBoltRecordPattern(value: any): value is BoltRecordPattern;
export function isBoltExpression(value: any): value is BoltExpression;
export function isBoltReferenceExpression(value: any): value is BoltReferenceExpression;
export function isBoltCallExpression(value: any): value is BoltCallExpression;
export function isBoltYieldExpression(value: any): value is BoltYieldExpression;
export function isBoltMatchArm(value: any): value is BoltMatchArm;
export function isBoltMatchExpression(value: any): value is BoltMatchExpression;
export function isBoltCase(value: any): value is BoltCase;
export function isBoltCaseExpression(value: any): value is BoltCaseExpression;
export function isBoltBlockExpression(value: any): value is BoltBlockExpression;
export function isBoltConstantExpression(value: any): value is BoltConstantExpression;
export function isBoltStatement(value: any): value is BoltStatement;
export function isBoltReturnStatement(value: any): value is BoltReturnStatement;
export function isBoltResumeStatement(value: any): value is BoltResumeStatement;
export function isBoltExpressionStatement(value: any): value is BoltExpressionStatement;
export function isBoltParameter(value: any): value is BoltParameter;
export function isBoltDeclaration(value: any): value is BoltDeclaration;
export function isBoltModule(value: any): value is BoltModule;
export function isBoltFunctionDeclaration(value: any): value is BoltFunctionDeclaration;
export function isBoltVariableDeclaration(value: any): value is BoltVariableDeclaration;
export function isBoltImportSymbol(value: any): value is BoltImportSymbol;
export function isBoltPlainImportSymbol(value: any): value is BoltPlainImportSymbol;
export function isBoltImportDeclaration(value: any): value is BoltImportDeclaration;
export function isBoltTraitDeclaration(value: any): value is BoltTraitDeclaration;
export function isBoltImplDeclaration(value: any): value is BoltImplDeclaration;
export function isBoltTypeAliasDeclaration(value: any): value is BoltTypeAliasDeclaration;
export function isBoltRecordDeclarationField(value: any): value is BoltRecordDeclarationField;
export function isBoltRecordDeclaration(value: any): value is BoltRecordDeclaration;
export function isBoltSourceElement(value: any): value is BoltSourceElement;
export function isJSToken(value: any): value is JSToken;
export function isJSOperator(value: any): value is JSOperator;
export function isJSIdentifier(value: any): value is JSIdentifier;
export function isJSString(value: any): value is JSString;
export function isJSInteger(value: any): value is JSInteger;
export function isJSFromKeyword(value: any): value is JSFromKeyword;
export function isJSReturnKeyword(value: any): value is JSReturnKeyword;
export function isJSTryKeyword(value: any): value is JSTryKeyword;
export function isJSFinallyKeyword(value: any): value is JSFinallyKeyword;
export function isJSCatchKeyword(value: any): value is JSCatchKeyword;
export function isJSImportKeyword(value: any): value is JSImportKeyword;
export function isJSAsKeyword(value: any): value is JSAsKeyword;
export function isJSConstKeyword(value: any): value is JSConstKeyword;
export function isJSLetKeyword(value: any): value is JSLetKeyword;
export function isJSExportKeyword(value: any): value is JSExportKeyword;
export function isJSFunctionKeyword(value: any): value is JSFunctionKeyword;
export function isJSWhileKeyword(value: any): value is JSWhileKeyword;
export function isJSForKeyword(value: any): value is JSForKeyword;
export function isJSCloseBrace(value: any): value is JSCloseBrace;
export function isJSCloseBracket(value: any): value is JSCloseBracket;
export function isJSCloseParen(value: any): value is JSCloseParen;
export function isJSOpenBrace(value: any): value is JSOpenBrace;
export function isJSOpenBracket(value: any): value is JSOpenBracket;
export function isJSOpenParen(value: any): value is JSOpenParen;
export function isJSSemi(value: any): value is JSSemi;
export function isJSComma(value: any): value is JSComma;
export function isJSDot(value: any): value is JSDot;
export function isJSDotDotDot(value: any): value is JSDotDotDot;
export function isJSMulOp(value: any): value is JSMulOp;
export function isJSAddOp(value: any): value is JSAddOp;
export function isJSDivOp(value: any): value is JSDivOp;
export function isJSSubOp(value: any): value is JSSubOp;
export function isJSLtOp(value: any): value is JSLtOp;
export function isJSGtOp(value: any): value is JSGtOp;
export function isJSBOrOp(value: any): value is JSBOrOp;
export function isJSBXorOp(value: any): value is JSBXorOp;
export function isJSBAndOp(value: any): value is JSBAndOp;
export function isJSBNotOp(value: any): value is JSBNotOp;
export function isJSNotOp(value: any): value is JSNotOp;
export function isJSPattern(value: any): value is JSPattern;
export function isJSBindPattern(value: any): value is JSBindPattern;
export function isJSExpression(value: any): value is JSExpression;
export function isJSConstantExpression(value: any): value is JSConstantExpression;
export function isJSMemberExpression(value: any): value is JSMemberExpression;
export function isJSCallExpression(value: any): value is JSCallExpression;
export function isJSBinaryExpression(value: any): value is JSBinaryExpression;
export function isJSUnaryExpression(value: any): value is JSUnaryExpression;
export function isJSNewExpression(value: any): value is JSNewExpression;
export function isJSSequenceExpression(value: any): value is JSSequenceExpression;
export function isJSConditionalExpression(value: any): value is JSConditionalExpression;
export function isJSLiteralExpression(value: any): value is JSLiteralExpression;
export function isJSReferenceExpression(value: any): value is JSReferenceExpression;
export function isJSSourceElement(value: any): value is JSSourceElement;
export function isJSStatement(value: any): value is JSStatement;
export function isJSCatchBlock(value: any): value is JSCatchBlock;
export function isJSTryCatchStatement(value: any): value is JSTryCatchStatement;
export function isJSExpressionStatement(value: any): value is JSExpressionStatement;
export function isJSConditionalStatement(value: any): value is JSConditionalStatement;
export function isJSReturnStatement(value: any): value is JSReturnStatement;
export function isJSParameter(value: any): value is JSParameter;
export function isJSDeclaration(value: any): value is JSDeclaration;
export function isJSImportBinding(value: any): value is JSImportBinding;
export function isJSImportStarBinding(value: any): value is JSImportStarBinding;
export function isJSImportAsBinding(value: any): value is JSImportAsBinding;
export function isJSImportDeclaration(value: any): value is JSImportDeclaration;
export function isJSFunctionDeclaration(value: any): value is JSFunctionDeclaration;
export function isJSArrowFunctionDeclaration(value: any): value is JSArrowFunctionDeclaration;
export function isJSLetDeclaration(value: any): value is JSLetDeclaration;
export function isJSSourceFile(value: any): value is JSSourceFile;