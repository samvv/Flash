
// FIXME 'a || b' is not correctly parsed in a pattern expression due to the cheap check of KIND_NO_EXPRESSION

import {
  SyntaxKind,
  kindToString,
  Identifier,
  ConstantExpression,
  ReferenceExpression,
  Expression,
  RecordDeclaration,
  Statement,
  Parameter,
  SourceElement,
  createQualName,
  Pattern,
  createBindPattern,
  ImportDeclaration,
  TypeExpression,
  createReferenceTypeExpression,
  createConstantExpression,
  createReferenceExpression,
  createParameter,
  BindPattern,
  createRecordDeclaration,
  createImportDeclaration,
  Modifiers,
  StringLiteral,
  ExpressionStatement,
  createExpressionStatement,
  VariableDeclaration,
  Syntax,
  createVariableDeclaration,
  ReturnStatement,
  createReturnStatement,
  Module,
  createModule,
  TypeAliasDeclaration,
  createTypeAliasDeclaration,
  FunctionDeclaration,
  createFunctionDeclaration,
  createCallExpression,
  Symbol,
  TypeParameter,
  createTypeParameter,
  TraitDeclaration,
  createTraitDeclaration,
  createImplDeclaration,
  ImplDeclaration,
  SourceFile,
  FunctionBodyElement,
  createSourceFile,
  MatchExpression,
  createMatchArm,
  MatchArm,
  createMatchExpression,
  createExpressionPattern,
  FunctionTypeExpression,
  ReferenceTypeExpression,
  createFunctionTypeExpression,
  RecordPattern,
  createRecordPattern,
  isPunctuated,
  Token,
  createQuoteExpression,
  QuoteExpression,
  BlockExpression,
  createBlockExpression,
  isOperatorLike,
  FunctionExpression,
  createFunctionExpression,
  MacroCall,
  createMacroCall,
  createMemberExpression,
  DeclarationLike,
  TraitOrImplElement,
  QualName,
  LoopStatement,
  createLoopStatement,
  createRecordDeclarationField,
  createRecordExpression,
  RecordExpressionElement,
  createRecordFieldValue,
  RecordExpression,
  AssignStatement,
  createAssignStatement,
  createCaseStatementCase,
  createCaseStatement,
  CaseStatement,
  createEnumDeclaration,
  EnumDeclarationElement,
  EnumDeclaration,
  createRecordPatternField,
} from "./ast"

import {
  OperatorKind,
  OperatorTable,
  assertToken,
  setOrigNodeRange,
  createTokenStream,
} from "./common"
import { HardParseError, ParseError } from "./errors";
import { Stream, uniq } from "./util"

import { Scanner } from "./scanner"
import { TextSpan, TextPos } from "./text"
import { Package } from "./package"

export type TokenStream = Stream<Token>;

const KIND_NO_EXPRESSION = [
  SyntaxKind.EndOfFile,
  SyntaxKind.Semi,
  SyntaxKind.EqSign,
  SyntaxKind.Braced,
  SyntaxKind.Parenthesized,
  SyntaxKind.Bracketed,
  SyntaxKind.LtSign,
  SyntaxKind.GtSign,
  SyntaxKind.RArrow,
  SyntaxKind.RArrowAlt,
  SyntaxKind.LArrow,
  SyntaxKind.Colon,
  SyntaxKind.VBar,
  SyntaxKind.Comma,
];

function isModifierKeyword(kind: SyntaxKind) {
  return kind === SyntaxKind.PubKeyword
      || kind === SyntaxKind.ForeignKeyword;
}

function assertNoTokens(tokens: TokenStream) {
  const t0 = tokens.peek(1);
  if (t0.kind !== SyntaxKind.EndOfFile) {
    throw new ParseError(t0, [SyntaxKind.EndOfFile]);
  }
}

const KIND_OPERATOR = [
  SyntaxKind.Operator,
  SyntaxKind.VBar,
  SyntaxKind.LtSign,
  SyntaxKind.GtSign,
];

const KIND_EXPRESSION_T0 = uniq([
  SyntaxKind.StringLiteral,
  SyntaxKind.IntegerLiteral,
  SyntaxKind.Operator,
  SyntaxKind.MatchKeyword,
  SyntaxKind.QuoteKeyword,
  SyntaxKind.YieldKeyword,
  SyntaxKind.Identifier,
  SyntaxKind.Parenthesized,
])

const KIND_STATEMENT_T0 = uniq([
  SyntaxKind.ReturnKeyword,
  SyntaxKind.IfKeyword,
  ...KIND_EXPRESSION_T0,
])

const KIND_DECLARATION_KEYWORD = [
  SyntaxKind.ImplKeyword,
  SyntaxKind.TraitKeyword,
  SyntaxKind.FnKeyword,
  SyntaxKind.EnumKeyword,
  SyntaxKind.LetKeyword,
  SyntaxKind.ModKeyword,
  SyntaxKind.StructKeyword,
  SyntaxKind.TypeKeyword,
]

const KIND_DECLARATION_T0 = uniq([
  SyntaxKind.PubKeyword,
  SyntaxKind.ForeignKeyword,
  ...KIND_DECLARATION_KEYWORD,
])

const KIND_SOURCEELEMENT_T0 = uniq([
  SyntaxKind.ModKeyword,
  SyntaxKind.ImportKeyword,
  ...KIND_EXPRESSION_T0,
  ...KIND_STATEMENT_T0,
  ...KIND_DECLARATION_T0,
])

function isRightAssoc(kind: OperatorKind): boolean {
  return kind === OperatorKind.InfixR;
}

export class Parser {

  private exprOperatorTable = new OperatorTable([
    [
      [OperatorKind.InfixL, 2, '==']
    ],
    [
      [OperatorKind.InfixL, 2, '&&'],
      [OperatorKind.InfixL, 2, '||']
    ],
    [
      [OperatorKind.InfixL, 2, '<'],
      [OperatorKind.InfixL, 2, '>'],
      [OperatorKind.InfixL, 2, '<='],
      [OperatorKind.InfixL, 2, '>=']
    ],
    [
      [OperatorKind.InfixL, 2, '>>'],
      [OperatorKind.InfixL, 2, '<<']
    ],
    [
      [OperatorKind.InfixL, 2, '+'],
      [OperatorKind.InfixL, 2, '-'],
    ],
    [
      [OperatorKind.InfixL, 2, '/'],
      [OperatorKind.InfixL, 2, '*'],
      [OperatorKind.InfixL, 2, '%'],
    ],
    [
      [OperatorKind.Prefix, 1, '!']
    ],
  ]);

  private typeOperatorTable = new OperatorTable([
    [
      [OperatorKind.InfixL, 2, '|'],
    ]
  ]);

  public parse(kind: SyntaxKind, tokens: TokenStream): Syntax {
    return (this as any)['parse' + kindToString(kind).substring(''.length)](tokens);
  }

  // private parseModulePath(tokens: TokenStream): ModulePath | null {

  //   let firstToken = tokens.peek();;
  //   let lastToken: Token;
  //   let isAbsolute = false;
  //   let elements = [];

  //   const t0 = tokens.peek();
  //   if (t0.kind === SyntaxKind.ColonColon) {
  //     isAbsolute = true;
  //     tokens.get();
  //     lastToken = t0;
  //   }

  //   if (tokens.peek(2).kind === SyntaxKind.ColonColon) {
  //     while (true) {
  //       const t1 = tokens.get();
  //       assertToken(t1, SyntaxKind.Identifier);
  //       elements.push(t1 as Identifier)
  //       const t2 = tokens.get();
  //       if (tokens.peek(2).kind !== SyntaxKind.ColonColon) {
  //         lastToken = t2;
  //         break;
  //       }
  //     }
  //   }

  //   if (!isAbsolute && elements.length === 0) {
  //     return null;
  //   }
  //   const result = createModulePath(isAbsolute, elements);
  //   setOrigNodeRange(result, firstToken, lastToken!);
  //   return result;
  // }

  public parseQualName(tokens: TokenStream): QualName {

    const firstToken = tokens.peek();
    let isAbsolute = false;
    let modulePath = [];
    let name;
    if (tokens.peek().kind === SyntaxKind.ColonColon) {
      isAbsolute = true;
      tokens.get();
    }

    while (true) {
      const t1 = tokens.get();
      if (tokens.peek().kind === SyntaxKind.ColonColon) {
        assertToken(t1, SyntaxKind.Identifier);
        modulePath.push(t1 as Identifier);
        tokens.get();
      } else {
        if (t1.kind === SyntaxKind.Parenthesized) {
          const innerTokens = createTokenStream(t1);
          const t2 = innerTokens.get();
          if (!isOperatorLike(t2)) {
            throw new ParseError(t2, KIND_OPERATOR);
          }
          assertNoTokens(innerTokens);
          name = t2;
        } else if (t1.kind === SyntaxKind.Identifier) {
          name = t1;
        } else {
          throw new ParseError(t1, [SyntaxKind.Parenthesized, SyntaxKind.Identifier]);
        }
        break;
      }
    }

    const node = createQualName(isAbsolute, modulePath, name as Identifier, null);
    setOrigNodeRange(node, firstToken, name);
    return node;
  }

  public parseBindPattern(tokens: TokenStream): BindPattern {
    const t0 = tokens.get();
    assertToken(t0, SyntaxKind.Identifier);
    const node = createBindPattern(t0 as Identifier);
    setOrigNodeRange(node, t0, t0);
    return node;
  }

  public parseRecordPattern(tokens: TokenStream): RecordPattern {

    const name = this.parseTypeExpression(tokens);
    const t1 = tokens.get();
    assertToken(t1, SyntaxKind.Braced);

    const innerTokens = createTokenStream(t1);
    const members = [];

    while (true) {

      let t0 = innerTokens.get();
      const firstToken = t0;

      if (t0.kind === SyntaxKind.EndOfFile) {
        break;
      }

      let isRest = false;
      let name = null;
      let pattern = null;
      if (t0.kind === SyntaxKind.DotDot) {
        isRest = true;
        t0 = innerTokens.peek();
      }
      if (t0.kind === SyntaxKind.Identifier) {
        name = t0;
        t0 = innerTokens.peek();
      } else if (!isRest) {
        throw new ParseError(t0, [SyntaxKind.Identifier]);
      }
      if (t0.kind === SyntaxKind.EqSign) {
        pattern = this.parsePattern(innerTokens);
      }
      let member = createRecordPatternField(isRest, name, pattern);
      setOrigNodeRange(member, firstToken, t0);
      members.push(member);

      if (t0.kind === SyntaxKind.EndOfFile) {
        break;
      }
      assertToken(t0, SyntaxKind.Comma);

    }

    const result = createRecordPattern(name, members);
    setOrigNodeRange(result, name, t1);
    return result;
  }

  public parsePattern(tokens: TokenStream): Pattern {
    const t0 = tokens.peek(1);
    const t1 = tokens.peek(2);
    if (t0.kind === SyntaxKind.Identifier && t1.kind === SyntaxKind.Braced) {
      return this.parseRecordPattern(tokens);
    } else if (t0.kind === SyntaxKind.Identifier && KIND_NO_EXPRESSION.indexOf(t1.kind) !== -1) {
      return this.parseBindPattern(tokens);
    } else if (t0.kind === SyntaxKind.Operator && t0.text === '^') {
      tokens.get();
      const refExpr = this.parseReferenceExpression(tokens);
      const result = createExpressionPattern(refExpr);
      setOrigNodeRange(result, t0, refExpr);
      return result;
    } else if (KIND_EXPRESSION_T0.indexOf(t0.kind) !== -1) {
      const expr = this.parseExpression(tokens);
      const result = createExpressionPattern(expr);
      setOrigNodeRange(result, expr, expr);
      return result;
    } else {
      const expected = KIND_EXPRESSION_T0.slice();
      expected.push(SyntaxKind.Operator);
      throw new ParseError(t0, expected)
    }
  }

  public parseImportDeclaration(tokens: TokenStream): ImportDeclaration {

    let modifiers = 0;
    if (tokens.peek().kind === SyntaxKind.PubKeyword) {
      tokens.get();
      modifiers |= Modifiers.IsPublic;
    }

    const t0 = tokens.get();
    assertToken(t0, SyntaxKind.ImportKeyword);

    const t1 = tokens.get();
    assertToken(t1, SyntaxKind.StringLiteral);

    const symbols = null;
    const t2 = tokens.peek();
    if (t2.kind === SyntaxKind.Parenthesized) {
      // TODO implement grammar and parsing logic for symbols
    }

    const node = createImportDeclaration(modifiers, t1 as StringLiteral, symbols);
    setOrigNodeRange(node, t0, t1);
    return node;
  }

  public parseFunctionTypeExpression(tokens: TokenStream): FunctionTypeExpression {
    const t0 = tokens.get();
    assertToken(t0, SyntaxKind.FnKeyword);
    const t1 = tokens.get();
    assertToken(t1, SyntaxKind.Parenthesized);
    const innerTokens = createTokenStream(t1);
    let i = 0;
    const params: Parameter[] = [];
    while (true) {
      const t2 = innerTokens.peek();
      if (t2.kind === SyntaxKind.EndOfFile) {
        break;
      }
      const param = this.parseParameter(innerTokens, i++)
      params.push(param);
      const t3 = innerTokens.peek()
      if (t3.kind === SyntaxKind.EndOfFile) {
        break;
      }
      const t4 = innerTokens.get();
      assertToken(t4, SyntaxKind.Comma);
    }
    const t3 = tokens.peek();
    let returnType = null;
    if (t3.kind === SyntaxKind.RArrow) {
      tokens.get();
      returnType = this.parseTypeExpression(tokens);
    }
    const result = createFunctionTypeExpression(params, returnType)
    setOrigNodeRange(result, t0, returnType !== null ? returnType : t1);
    return result;
  }

  public parseReferenceTypeExpression(tokens: TokenStream): ReferenceTypeExpression {

    const firstToken = tokens.peek();
    let isAbsolute = false;
    let modulePath = [];
    let name;

    if (tokens.peek().kind === SyntaxKind.ColonColon) {
      isAbsolute = true;
      tokens.get();
    }

    while (true) {
      const t1 = tokens.get();
      if (tokens.peek().kind === SyntaxKind.ColonColon) {
        assertToken(t1, SyntaxKind.Identifier);
        modulePath.push(t1 as Identifier);
        tokens.get();
      } else {
        assertToken(t1, SyntaxKind.Identifier);
        name = t1 as Identifier;
        break;
      }
    }

    let lastToken: Token = name;
    let typeArgs: TypeExpression[] | null = null;

    if (tokens.peek().kind === SyntaxKind.LtSign) {
      typeArgs = [];
      tokens.get();
      let first = true;
      while (true) {
        const t2 = tokens.peek();
        if (t2.kind === SyntaxKind.GtSign) { 
          break;
        }
        if (first) {
          first = false;
        } else {
          assertToken(t2, SyntaxKind.Comma);
          tokens.get();
        }
        typeArgs!.push(this.parseTypeExpression(tokens));
      }
      const t4 = tokens.get();
      assertToken(t4, SyntaxKind.GtSign);
      lastToken = t4;
    }

    const qualName = createQualName(isAbsolute, modulePath, name);
    setOrigNodeRange(qualName, firstToken, modulePath.length > 0 ? modulePath[modulePath.length-1] : firstToken)
    const node = createReferenceTypeExpression(qualName, typeArgs);
    setOrigNodeRange(node, firstToken, lastToken);
    return node;
  }

  private parsePrimTypeExpression(tokens: TokenStream): TypeExpression {
    const t0 = tokens.peek();
    if (t0.kind === SyntaxKind.FnKeyword) {
      return this.parseFunctionTypeExpression(tokens);
    } else if (t0.kind === SyntaxKind.Identifier) {
      return this.parseReferenceTypeExpression(tokens);
    } else {
      throw new ParseError(t0, [SyntaxKind.Identifier]);
    }
  }

  //private parseTypeExpressionOperators(tokens: TokenStream, lhs: TypeExpression, minPrecedence: number): TypeExpression {
  //  while (true) {
  //    const t0 = tokens.peek();
  //    if (!isOperatorLike(t0)) {
  //      break;
  //    }
  //    let desc0 = this.typeOperatorTable.lookup(emitNode(t0));
  //    if (desc0 === null || desc0.arity !== 2 || desc0.precedence < minPrecedence) {
  //      break;
  //    }
  //    tokens.get();
  //    let rhs = this.parsePrimTypeExpression(tokens);
  //    while (true) {
  //      const t1 = tokens.peek()
  //      if (!isOperatorLike(t1.kind)) {
  //        break;
  //      }
  //      const desc1 = this.typeOperatorTable.lookup(emitNode(t1))
  //      if (desc1 === null || desc1.arity !== 2 || desc1.precedence < desc0.precedence || !isRightAssoc(desc1.kind)) {
  //        break;
  //      }
  //      rhs = this.parseTypeExpressionOperators(tokens, rhs, desc1.precedence);
  //    }
  //    const name = emitNode(t0);
  //    switch (name) {
  //      case '|':
  //        return createFunctionTypeExpression();
  //        )
  //    }
  //    lhs = createReferenceTypeExpression(null, t0, [lhs, rhs]);
  //    setOrigNodeRange(lhs, t0, rhs);
  //  }
  //  return lhs
  //}

  public parseTypeExpression(tokens: TokenStream) {
    //const lhs = this.parsePrimTypeExpression(tokens);
    //return this.parseTypeExpressionOperators(tokens, lhs, 0);
    return this.parsePrimTypeExpression(tokens);
  }

  public parseConstantExpression(tokens: TokenStream): ConstantExpression {
    const t0 = tokens.get();
    let value: boolean | string | bigint;
    if (t0.kind === SyntaxKind.StringLiteral) {
      value = t0.value;
    } else if (t0.kind === SyntaxKind.IntegerLiteral) {
      value = t0.value;
    } else {
      throw new ParseError(t0, [SyntaxKind.StringLiteral, SyntaxKind.IntegerLiteral]);
    }
    const node = createConstantExpression(value);
    setOrigNodeRange(node, t0, t0);
    return node;
  }

  public parseFunctionExpression(tokens: TokenStream): FunctionExpression {
    const t0 = tokens.get();
    assertToken(t0, SyntaxKind.VBar);
    let i = 0;
    const params: Parameter[] = [];
    while (true) {
      const t1 = tokens.peek();
      if (t1.kind === SyntaxKind.VBar) {
        tokens.get();
        break;
      }
      const param = this.parseParameter(tokens, i++);
      params.push(param);
      const t2 = tokens.peek();
      if (t2.kind === SyntaxKind.VBar) {
        tokens.get();
        break;
      }
      const t3 = tokens.get();
      assertToken(t3, SyntaxKind.Comma);
    }
    let returnType = null;
    let t4 = tokens.peek();
    if (t4.kind === SyntaxKind.RArrow) {
      tokens.get();
      returnType = this.parseTypeExpression(tokens);
      t4 = tokens.peek();
    }
    let body = null;
    let expr = null;
    if (t4.kind === SyntaxKind.Braced) {
      assertToken(t4, SyntaxKind.Braced);
      const innerTokens = createTokenStream(t4);
      body = this.parseFunctionBodyElements(innerTokens);
    } else {
      expr = this.parseExpression(tokens);
    }
    const result = createFunctionExpression(params, returnType, body, expr);
    setOrigNodeRange(result, t0, expr !== null ? expr : t4);
    return result;
  }

  public parseReferenceExpression(tokens: TokenStream): ReferenceExpression {
    const firstToken = tokens.peek();
    const name = this.parseQualName(tokens);
    const node = createReferenceExpression(name);
    setOrigNodeRange(node, firstToken, name);
    return node;
  }

  public parseMatchExpression(tokens: TokenStream): MatchExpression {
    const t0 = tokens.get();
    assertToken(t0, SyntaxKind.MatchKeyword);
    const expr = this.parseExpression(tokens);
    const t1 = tokens.get();
    assertToken(t1, SyntaxKind.Braced);
    const innerTokens = createTokenStream(t1);
    const matchArms: MatchArm[] = [];
    while (true) {
      const t2 = innerTokens.peek();
      if (t2.kind === SyntaxKind.EndOfFile) {
        break;
      }
      const pattern = this.parsePattern(innerTokens);
      const t3 = innerTokens.get();
      assertToken(t3, SyntaxKind.RArrowAlt);
      const expression = this.parseExpression(innerTokens);
      const arm = createMatchArm(pattern, expression);
      setOrigNodeRange(arm, pattern, expression);
      matchArms.push(arm);
      const t4 = innerTokens.peek();
      if (t4.kind === SyntaxKind.EndOfFile) {
        break;
      }
      assertToken(t4, SyntaxKind.Comma);
      innerTokens.get();
    }
    const result = createMatchExpression(expr, matchArms);
    setOrigNodeRange(result, t0, t1);
    return result;
  }

  public parseQuoteExpression(tokens: TokenStream): QuoteExpression {
    const t0 = tokens.get();
    assertToken(t0, SyntaxKind.QuoteKeyword);
    let t1 = tokens.get();
    let target = "";
    if (t1.kind === SyntaxKind.StringLiteral) {
      target = t1.value;
      t1 = tokens.get();
    }
    if (!isPunctuated(t1)) {
      throw new ParseError(t1, [SyntaxKind.Braced, SyntaxKind.Parenthesized, SyntaxKind.Bracketed]);
    }
    let scanner;
    switch (target) {
      case "":
        scanner = new Scanner(t1.span!.file, t1.text, t1.span!.start.clone());
        break;
      //case "JS":
      //  scanner = new JSScanner(t1.span!.file, t1.text, t1.span!.start.clone());
      //  break;
      default:
        throw new Error(`Unrecognised language.`);
    }
    const scanned: Token[] = [];
    while (true) {
      const t2 = scanner.scan();
      if (t2.kind === SyntaxKind.EndOfFile) {
        break;
      }
      scanned.push(t2);
    }
    const result = createQuoteExpression(scanned as Token[]);
    setOrigNodeRange(result, t0, t1);
    return result;
  }

  public parseBlockExpression(tokens: TokenStream): BlockExpression {
    const t0 = tokens.get();
    const innerTokens = createTokenStream(t0);
    const elements = this.parseFunctionBodyElements(innerTokens)
    const result = createBlockExpression(elements);
    setOrigNodeRange(result, t0, t0);
    return result;
  }

  private parseRecordExpression(tokens: TokenStream): RecordExpression {
    const typeRef = this.parseReferenceTypeExpression(tokens);
    const t1 = tokens.get();
    assertToken(t1, SyntaxKind.Braced);
    const innerTokens = createTokenStream(t1);
    const fields: RecordExpressionElement[] = [];
    while (true) {
      let name;
      let value = null;
      const t1 = innerTokens.get();
      if (t1.kind === SyntaxKind.EndOfFile) {
        break;
      }
      assertToken(t1, SyntaxKind.Identifier);
      name = t1 as Identifier
      const t2 = innerTokens.peek();
      if (t2.kind === SyntaxKind.Colon) {
        innerTokens.get();
        value = this.parseExpression(innerTokens);
      }
      const t3 = innerTokens.peek();
      if (t3.kind === SyntaxKind.Comma) {
        innerTokens.get();
      } else {
        assertToken(t3, SyntaxKind.EndOfFile)
      }
      const element = createRecordFieldValue(name, value); 
      fields.push(element);
    }
    const result = createRecordExpression(
      typeRef,
      fields
    );
    setOrigNodeRange(result, typeRef, t1)
    return result;
  }

  private getTokenAfterTypeRef(tokens: TokenStream, i = 1): number {

    // Peek actual qualified name

    let t0 = tokens.peek(i);
    if (t0.kind === SyntaxKind.ColonColon) {
      i++;
      t0 = tokens.peek(i);
    }
    if (t0.kind !== SyntaxKind.Identifier) {
      return -1;
    }
    i++;
    while (tokens.peek(i).kind === SyntaxKind.ColonColon) {
      i++;
      if (tokens.peek(i).kind !== SyntaxKind.Identifier) {
        return -1;
      }
    }

    // Peek anything that comes between '<' and '>'

    const t2 = tokens.peek(i);
    if (t2.kind === SyntaxKind.LtSign) {
      let count = 1;
      i++;
      while (count > 0) {
        const t3 = tokens.peek(i++);
        if (t3.kind === SyntaxKind.LtSign) {
          count++;
        }
        if (t3.kind === SyntaxKind.GtSign) {
          count--;
        }
      }
    }

    // Return wherever position we landed

    return i;

  }

  public parseExpression(tokens: TokenStream) {
    return this.parseBinaryExpression(tokens, this.parseUnaryExpression(tokens), 0);
  }

  private parseUnaryExpression(tokens: TokenStream) {
    return this.parseExpressionPrimitive(tokens);
  }

  private parseBinaryExpression(tokens: TokenStream, lhs: Expression, minPrecedence: number) {
    let lookahead = tokens.peek(1);
    while (true) {
      if (lookahead.kind !== SyntaxKind.Operator) {
        break;
      }
      const lookaheadDesc = this.exprOperatorTable.lookup(2, lookahead.text);
      if (lookaheadDesc === null || lookaheadDesc.precedence < minPrecedence) {
        break;
      }
      const op = lookahead;
      const opDesc = this.exprOperatorTable.lookup(2, op.text);
      if (opDesc === null) {
        break;
      }
      tokens.get();
      let rhs = this.parseUnaryExpression(tokens)
      lookahead = tokens.peek()
      while (lookaheadDesc.arity === 2 
          && ((lookaheadDesc.precedence > opDesc.precedence)
            || lookaheadDesc.kind === OperatorKind.InfixR && lookaheadDesc.precedence === opDesc.precedence)) {
          rhs = this.parseBinaryExpression(tokens, rhs, lookaheadDesc.precedence)
          lookahead = tokens.peek();
      }
      lookahead = tokens.peek();
      const qualName = createQualName(false, [], op);
      setOrigNodeRange(qualName, op, op);
      const refExpr = createReferenceExpression(qualName);
      setOrigNodeRange(refExpr, op, op);
      const binExpr = createCallExpression(refExpr, [lhs, rhs]);
      setOrigNodeRange(binExpr, lhs, rhs);
      lhs = binExpr;
    }
    return lhs
  }

  private parseExpressionPrimitive(tokens: TokenStream): Expression {

    try {
      const forked = tokens.fork();
      const recordExpr = this.parseRecordExpression(forked);
      tokens.join(forked);
      return recordExpr;
    } catch (e) {
      if (!(e instanceof ParseError) || (e instanceof HardParseError)) {
        throw e;
      }
    }

    const t0 = tokens.peek();

    let result;
    if (t0.kind === SyntaxKind.VBar) {
      result = this.parseFunctionExpression(tokens);
    } else if (t0.kind === SyntaxKind.Parenthesized) {
      tokens.get();
      const innerTokens = createTokenStream(t0);
      result = this.parseExpression(innerTokens);
      setOrigNodeRange(result, t0, t0);
    } else if (t0.kind === SyntaxKind.Braced) {
      result = this.parseBlockExpression(tokens);
    } else if (t0.kind === SyntaxKind.QuoteKeyword) {
      result = this.parseQuoteExpression(tokens);
    } else if (t0.kind === SyntaxKind.MatchKeyword) {
      result = this.parseMatchExpression(tokens);
    } else if (t0.kind === SyntaxKind.IntegerLiteral || t0.kind === SyntaxKind.StringLiteral) {
      result = this.parseConstantExpression(tokens);
    } else if (t0.kind === SyntaxKind.Identifier) {
      result = this.parseReferenceExpression(tokens);
    } else {
      throw new ParseError(t0, [SyntaxKind.StringLiteral, SyntaxKind.Identifier]);
    }

    while (true) {

      // FIXME The following expression is incorrectly parsed: 0..fac()

      let t2 = tokens.peek();
      const firstToken = t2;

      // Parse all path elements of the member expression: a.foo.bar

      const path: Identifier[] = [];

      while (t2.kind === SyntaxKind.Dot) {
        tokens.get();
        const t3 = tokens.get();
        assertToken(t3, SyntaxKind.Identifier);
        path.push(t3 as Identifier);
        t2 = tokens.peek();
      }

      if (path.length > 0) {
        const node = createMemberExpression(result, path);
        setOrigNodeRange(node, firstToken, path[path.length-1]);
        result = node;
      }

      if (t2.kind !== SyntaxKind.Parenthesized) {
        break;
      }

      tokens.get();

      const args: Expression[] = []
      const innerTokens = createTokenStream(t2);

      while (true) {

        const t3 = innerTokens.peek();
        if (t3.kind === SyntaxKind.EndOfFile) {
          break; 
        }
        args.push(this.parseExpression(innerTokens))
        const t4 = innerTokens.get();
        if (t4.kind === SyntaxKind.EndOfFile) {
          break
        }
        if (t4.kind !== SyntaxKind.Comma) {
          throw new ParseError(t4, [SyntaxKind.Comma])
        }
      }

      const node = createCallExpression(result, args, null)
      setOrigNodeRange(node, result, t2);
      result = node;

    }

    return result;

  }

  public parseParameter(tokens: TokenStream, index: number): Parameter {

    let defaultValue = null;
    let typeDecl = null;

    const pattern = this.parsePattern(tokens)

    let t0 = tokens.peek();
    let endNode: Syntax = pattern;
    if (t0.kind === SyntaxKind.Colon) {
      tokens.get();
      typeDecl = this.parseTypeExpression(tokens);
      endNode = typeDecl;
      t0 = tokens.peek();
    }
    if (t0.kind === SyntaxKind.EqSign) {
      tokens.get();
      defaultValue = this.parseExpression(tokens);
      endNode = defaultValue;
    }

    const node = createParameter(index, pattern, typeDecl, defaultValue)
    setOrigNodeRange(node, pattern, endNode);
    return node;

  }

  public parseVariableDeclaration(tokens: TokenStream): VariableDeclaration {

    let modifiers = 0;
    let typeDecl = null;
    let value = null;

    let t0 = tokens.get();
    if (t0.kind === SyntaxKind.PubKeyword) {
      modifiers |= Modifiers.IsPublic;
      t0 = tokens.get();
    }
    assertToken(t0, SyntaxKind.LetKeyword);

    const t1 = tokens.peek();
    if (t1.kind === SyntaxKind.MutKeyword) {
      tokens.get();
      modifiers |= Modifiers.IsMutable;
    }

    const bindings = this.parsePattern(tokens)

    let t2 = tokens.peek();
    let lastNode: Syntax = bindings;

    if (t2.kind === SyntaxKind.Colon) {
      tokens.get();
      lastNode = typeDecl = this.parseTypeExpression(tokens);
      t2 = tokens.peek();
    }

    if (t2.kind === SyntaxKind.EqSign) {
      tokens.get();
      lastNode = value = this.parseExpression(tokens);
    }

    const node = createVariableDeclaration(modifiers, bindings, typeDecl, value)
    setOrigNodeRange(node, t0, lastNode);
    return node;
  }

  public parseReturnStatement(tokens: TokenStream): ReturnStatement {

    const t0 = tokens.get();
    assertToken(t0, SyntaxKind.ReturnKeyword);

    let expr = null;

    const t1 = tokens.peek();
    if (t1.kind !== SyntaxKind.EndOfFile) { 
      expr = this.parseExpression(tokens)
    }

    const node = createReturnStatement(expr);
    setOrigNodeRange(node, t0, expr !== null ? expr : t0);
    return node;
  }

  protected isUnaryOperator(name: string) {
    // TODO
    return false;
  }

  protected lookaheadHasExpression(tokens: TokenStream, i = 1): boolean {
    const t0 = tokens.peek(i);
    if (t0.kind === SyntaxKind.Parenthesized) {
      return this.lookaheadHasExpression(tokens, i+1);
    }
    return t0.kind === SyntaxKind.Identifier
        || t0.kind === SyntaxKind.StringLiteral
        || t0.kind === SyntaxKind.IntegerLiteral
        || (t0.kind === SyntaxKind.Operator && this.isUnaryOperator(t0.text));
  }

  public parseExpressionStatement(tokens: TokenStream): ExpressionStatement {
    const expression = this.parseExpression(tokens)
    const node = createExpressionStatement(expression)
    setOrigNodeRange(node, expression, expression);
    return node;
  }

  public parseAssignStatement(tokens: TokenStream): AssignStatement {
    const pattern = this.parsePattern(tokens);
    const t2 = tokens.get();
    assertToken(t2, SyntaxKind.EqSign);
    const expr = this.parseExpression(tokens);
    const result = createAssignStatement(pattern, expr);
    setOrigNodeRange(result, pattern, expr);
    return result;
  }

  public parseLoopStatement(tokens: TokenStream): LoopStatement {
    const t0 = tokens.get();
    assertToken(t0, SyntaxKind.LoopKeyword);
    const t1 = tokens.get();
    assertToken(t1, SyntaxKind.Braced);
    const innerTokens = createTokenStream(t1);
    const elements = this.parseFunctionBodyElements(innerTokens);
    const result = createLoopStatement(elements);
    setOrigNodeRange(result, t0, t1);
    return result;
  }

  public parseStatement(tokens: TokenStream): Statement {

    try {
      const forked = tokens.fork();
      const result = this.parseAssignStatement(forked);
      tokens.join(forked);
      return result;
    } catch (e) {
      if (!(e instanceof ParseError) || (e instanceof HardParseError)) {
        throw e;
      }
    }

    // if (this.lookaheadIsMacroCall(tokens)) {
    //   return this.parseMacroCall(tokens);
    // }
    const t0 = tokens.peek();
    if (KIND_EXPRESSION_T0.indexOf(t0.kind) !== -1) {
      return this.parseExpressionStatement(tokens);
    } else if (t0.kind === SyntaxKind.ReturnKeyword) {
      return this.parseReturnStatement(tokens);
    } else if (t0.kind === SyntaxKind.LoopKeyword) {
      return this.parseLoopStatement(tokens);
    } else if (t0.kind === SyntaxKind.IfKeyword) {
      return this.parseIfStatement(tokens);
    } else {
      throw new ParseError(t0, KIND_STATEMENT_T0);
    }
  }

  public parseIfStatement(tokens: TokenStream): CaseStatement {

    const t0 = tokens.get();
    assertToken(t0, SyntaxKind.IfKeyword);

    const test = this.parseExpression(tokens);
    const t1 = tokens.get();
    assertToken(t1, SyntaxKind.Braced);
    const bodyTokens = createTokenStream(t1);
    const body = this.parseFunctionBodyElements(bodyTokens);
    const firstCase = createCaseStatementCase(test, body)
    setOrigNodeRange(firstCase, t0, t1);
    const cases = [ firstCase ];

    let lastToken = t1;

    while (true) {
      const t2 = tokens.peek(1);
      const t3 = tokens.peek(2);
      if (t2.kind !== SyntaxKind.ElseKeyword || t3.kind !== SyntaxKind.IfKeyword) {
        break;
      }
      tokens.get();
      tokens.get();
      const test = this.parseExpression(tokens);
      const t4 = tokens.get();
      assertToken(t4, SyntaxKind.Braced);
      const bodyTokens = createTokenStream(t4);
      const body = this.parseFunctionBodyElements(bodyTokens);
      const altCase = createCaseStatementCase(test, body);
      setOrigNodeRange(altCase, t2, t4);
      cases.push(altCase);
      lastToken = t4;
    }

    const t4 = tokens.peek();
    if (t4.kind === SyntaxKind.ElseKeyword) {
      tokens.get();
      const t5 = tokens.get();
      assertToken(t5, SyntaxKind.Braced);
      const alternativeTokens = createTokenStream(t5);
      const body = this.parseFunctionBodyElements(alternativeTokens)
      const alternative = createCaseStatementCase(null, body);
      setOrigNodeRange(alternative, t4, t5);
      cases.push(alternative);
      lastToken = t5;
    }

    const result = createCaseStatement(cases);
    setOrigNodeRange(result, t0, lastToken);
    return result;
  }

  public parseGenericTypeParameter(tokens: TokenStream): TypeParameter {
    const t0 = tokens.peek();
    tokens.get();
    assertToken(t0, SyntaxKind.Identifier);
    let typeBound = null;
    const t1 = tokens.peek();
    if (t1.kind === SyntaxKind.Colon) {
      tokens.get();
      typeBound = this.parseTypeExpression(tokens);
    }
    const node = createTypeParameter(0, t0 as Identifier, typeBound, null)
    setOrigNodeRange(node, t0, t0);
    return node;
  }

  private parseGenericTypeParameters(tokens: TokenStream): TypeParameter[] {
    let typeParams: TypeParameter[] = [];
    while (true) {
      let t1 = tokens.peek();
      if (t1.kind !== SyntaxKind.Identifier) {
        break;
      }
      typeParams.push(this.parseGenericTypeParameter(tokens));
      const t2 = tokens.peek();
      if (t2.kind !== SyntaxKind.Comma) {
        break;
      }
      tokens.get();
    }
    return typeParams;
  }

  public parseEnumDeclaration(tokens: TokenStream): EnumDeclaration {

    let modifiers = 0;
    let typeParams = null;
    let members: EnumDeclarationElement[] = [];

    let t0 = tokens.get();
    const firstToken = t0;
    if (t0.kind === SyntaxKind.PubKeyword) {
      modifiers |= Modifiers.IsPublic;
      t0 = tokens.get();
    }

    assertToken(t0, SyntaxKind.EnumKeyword);

    const t1 = tokens.get();
    assertToken(t1, SyntaxKind.Identifier);
    const name = t1 as Identifier;

    const t2 = tokens.get();
    assertToken(t2, SyntaxKind.Braced);
    const memberTokens = createTokenStream(t2);
    while (true) {
      const t3 = memberTokens.get();
      if (t3.kind === SyntaxKind.EndOfFile) {
        break;
      }
      assertToken(t3, SyntaxKind.Identifier);
      members.push(t3 as Identifier);
      const t4 = memberTokens.get();
      if (t4.kind === SyntaxKind.EndOfFile) {
        break;
      }
      assertToken(t4, SyntaxKind.Comma);
    }

    const node = createEnumDeclaration(modifiers, name, typeParams, members)
    setOrigNodeRange(node, firstToken, t2);
    return node;
  }

  public parseRecordDeclaration(tokens: TokenStream): RecordDeclaration {

    let modifiers = 0;
    let typeParams = null;

    let t0 = tokens.get();
    const firstToken = t0;
    if (t0.kind === SyntaxKind.PubKeyword) {
      modifiers |= Modifiers.IsPublic;
      t0 = tokens.get();
    }

    assertToken(t0, SyntaxKind.StructKeyword);

    const t1 = tokens.get();
    assertToken(t1, SyntaxKind.Identifier);
    const name = t1 as Identifier;

    let t2 = tokens.peek();

    // FIXME What is this doing here?
    if (t2.kind === SyntaxKind.EndOfFile) {
      const node = createRecordDeclaration(modifiers, name, null, []);
      setOrigNodeRange(node, firstToken, t2);
      return node;
    }

    if (t2.kind === SyntaxKind.LtSign) {
      tokens.get();
      typeParams = this.parseGenericTypeParameters(tokens);
      const t3 = tokens.get();
      assertToken(t3, SyntaxKind.GtSign);
      t2 = tokens.peek();
    }

    let members = null;

    if (t2.kind !== SyntaxKind.Semi) {

      if (t2.kind !== SyntaxKind.Braced) {
        throw new ParseError(t2, [SyntaxKind.Braced])
      }

      members = [];

      tokens.get();
      const innerTokens = createTokenStream(t2);

      while (true) {
        const t3 = innerTokens.peek();
        if (t3.kind === SyntaxKind.EndOfFile) {
          break;
        }
        assertToken(t3, SyntaxKind.Identifier);
        innerTokens.get();
        const name = t3 as Identifier;
        const t4 = innerTokens.get();
        assertToken(t4, SyntaxKind.Colon);
        const type = this.parseTypeExpression(innerTokens);
        const field = createRecordDeclarationField(name as Identifier, type);
        setOrigNodeRange(field, name, type);
        members.push(field);
        const t5 = innerTokens.get();
        if (t5.kind === SyntaxKind.EndOfFile) {
          break;
        }
        assertToken(t5, SyntaxKind.Comma);
      }


    }

    const node = createRecordDeclaration(modifiers, name, typeParams, members);
    setOrigNodeRange(node, firstToken, t2);
    return node;
  }

  public parseStatements(tokens: TokenStream): Statement[] {
    const statements: Statement[] = [];
    while (true) {
      const t0 = tokens.peek();
      if (t0.kind === SyntaxKind.EndOfFile) {
        break;
      }
      if (t0.kind === SyntaxKind.Semi) {
        tokens.get();
        continue;
      }
      const statement = this.parseStatement(tokens);
      statements.push(statement);
    }
    return statements;
  }

  public parseModuleDeclaration(tokens: TokenStream): Module {

    let modifiers = 0;
    let pathElements = [];

    let t0 = tokens.get();
    const firstToken = t0;
    if (t0.kind === SyntaxKind.PubKeyword) {
      modifiers |= Modifiers.IsPublic;
      t0 = tokens.get();
    }

    if (t0.kind !== SyntaxKind.ModKeyword) {
      throw new ParseError(t0, [SyntaxKind.ModKeyword])
    }

    while (true) {
      const t1 = tokens.get();
      assertToken(t1, SyntaxKind.Identifier)
      pathElements.push(t1 as Identifier)
      const t2 = tokens.peek();
      if (t2.kind !== SyntaxKind.ColonColon) {
        break;
      }
      tokens.get();
    }

    const t1 = tokens.get();
    if (t1.kind !== SyntaxKind.Braced) {
      throw new ParseError(t1, [SyntaxKind.Braced])
    }
    const elements = this.parseSourceElements(createTokenStream(t1));

    const node = createModule(modifiers, pathElements, elements);
    setOrigNodeRange(node, firstToken, t1);
    return node;
  }


  public parseTypeAliasDeclaration(tokens: TokenStream): TypeAliasDeclaration {

    let modifiers = 0;
    let typeParams = null;

    let t0 = tokens.get();
    const firstToken = t0;

    if (t0.kind === SyntaxKind.PubKeyword) {
      modifiers |= Modifiers.IsPublic;
      t0 = tokens.get();
    }

    assertToken(t0, SyntaxKind.TypeKeyword);

    const name = tokens.get();
    if (name.kind !== SyntaxKind.Identifier) {
      throw new ParseError(name, [SyntaxKind.Identifier])
    }

    const t2 = tokens.peek();
    if (t2.kind === SyntaxKind.LtSign) {
      tokens.get();
      typeParams = this.parseGenericTypeParameters(tokens);
      const t3 = tokens.get();
      assertToken(t3, SyntaxKind.GtSign);
    }

    const t3 = tokens.get();
    assertToken(t3, SyntaxKind.EqSign);

    const typeExpr = this.parseTypeExpression(tokens);

    const node = createTypeAliasDeclaration(modifiers, name, typeParams, typeExpr)
    setOrigNodeRange(node, firstToken, typeExpr);
    return node;
  }

  public parseFunctionDeclaration(tokens: TokenStream): FunctionDeclaration {

    let target = "";
    let modifiers = 0;

    let k0 = tokens.peek();
    const firstToken = k0;

    if (k0.kind === SyntaxKind.PubKeyword) {
      tokens.get();
      modifiers |= Modifiers.IsPublic;
      k0 = tokens.peek();
    }

    if (k0.kind === SyntaxKind.ForeignKeyword) {
      tokens.get();
      const l1 = tokens.get();
      if (l1.kind !== SyntaxKind.StringLiteral) {
        throw new ParseError(l1, [SyntaxKind.StringLiteral])
      }
      target = l1.value;
      k0 = tokens.peek();
    }

    if (k0.kind !== SyntaxKind.FnKeyword) {
      throw new ParseError(k0, [SyntaxKind.FnKeyword])
    }

    tokens.get();

    let name: Symbol;
    let returnType = null;
    let body: any = null; // FIXME type-checking should not be disabled
    let params: Parameter[] = [];
    let typeParams = null;

    // Parse parameters

    let i = 0;

    const t0 = tokens.peek(1);
    const t1 = tokens.peek(2);

    const isParamLike = (token: Token) =>
        token.kind === SyntaxKind.Identifier || token.kind === SyntaxKind.Parenthesized;

    const parseParamLike = (tokens: TokenStream) => {
      const t0 = tokens.peek(1);
      if (t0.kind === SyntaxKind.Identifier) {
        tokens.get();
        const bindings = createBindPattern(t0 as Identifier);
        setOrigNodeRange(bindings, t0, t0);
        const param = createParameter(i++, bindings, null, null);
        setOrigNodeRange(param, t0, t0);
        return param;
      } else if (t0.kind === SyntaxKind.Parenthesized) {
        tokens.get();
        const innerTokens = createTokenStream(t0);
        const param = this.parseParameter(innerTokens, i++)
        assertNoTokens(innerTokens);
        return param
      } else {
        throw new ParseError(t0, [SyntaxKind.Identifier, SyntaxKind.Parenthesized])
      }
    }

    if (t0.kind === SyntaxKind.Operator) {

      name = t0;
      tokens.get();
      params.push(parseParamLike(tokens))

    } else if (isParamLike(t0) && t1.kind == SyntaxKind.Operator) {

      params.push(parseParamLike(tokens));
      name = t1;
      while (true) {
        const t2 = tokens.peek();
        if (t2.kind !== SyntaxKind.Operator) {
          break;
        }
        if (t2.text !== t1.text) {
          throw new Error(`Operators have to match when defining or declaring an n-ary operator.`);
        }
        tokens.get();
        params.push(parseParamLike(tokens))
      }

    } else if (t0.kind === SyntaxKind.Identifier) {

      name = t0;
      tokens.get();
      const t2 = tokens.get();
      if (t2.kind === SyntaxKind.Parenthesized) {
        const innerTokens = createTokenStream(t2);
        while (true) {
          const t3 = innerTokens.peek();
          if (t3.kind === SyntaxKind.EndOfFile) {
            break;
          }
          params.push(this.parseParameter(innerTokens, i++))
          const t4 = innerTokens.get();
          if (t4.kind === SyntaxKind.Comma) {
            continue;
          } else if (t4.kind === SyntaxKind.EndOfFile) {
            break;
          } else {
            throw new ParseError(t4, [SyntaxKind.Comma, SyntaxKind.EndOfFile])
          }
        }
      }

    } else {

      throw new ParseError(t0, [SyntaxKind.Identifier, SyntaxKind.Operator, SyntaxKind.Parenthesized])

    }

    // Parse return type

    const t2 = tokens.peek();
    if (t2.kind === SyntaxKind.RArrow) {
      tokens.get();
      returnType = this.parseTypeExpression(tokens);
    }

    // Parse second possible version of generic type parameters

    const t4 = tokens.peek();
    if (t4.kind === SyntaxKind.WhereKeyword) {
      tokens.get();
      typeParams = this.parseGenericTypeParameters(tokens);
    }

    // Parse function body

    const t3 = tokens.peek();
    if (t3.kind === SyntaxKind.Braced) {
      tokens.get();
      switch (target) {
        case "":
          body = this.parseFunctionBodyElements(createTokenStream(t3));
          break;
        //default:
        //  body = parseForeignLanguage(target, t3.text, t3.span!.file, t3.span!.start);
        //  break;
        default:
          throw new Error(`Parsing function bodies in a foreign language is not supported yet.`);
      }
    } else if (t3.kind !== SyntaxKind.Semi) {
      const expected = [ SyntaxKind.Braced, SyntaxKind.Semi ];
      if (returnType === null) {
        expected.push(SyntaxKind.RArrow);
      }
      throw new ParseError(t3, expected);
    }

    const result = createFunctionDeclaration(
      modifiers,
      target,
      name,
      params,
      returnType,
      typeParams,
      body
    );
    setOrigNodeRange(result, firstToken, t3);
    return result;

  }

  public parseTraitDeclaration(tokens: TokenStream): TraitDeclaration {

    let modifiers = 0;
    let typeParams = null;
    let name;
    let typeBoundExpr = null;
    let elements = null;

    // Parse the 'pub' keyword, if present
    let t0 = tokens.get();
    const firstToken = t0;
    if (t0.kind === SyntaxKind.PubKeyword) {
      modifiers |= Modifiers.IsPublic;
      t0 = tokens.get();
    }

    // By now, we should encounter the 'trait' keyword'
    assertToken(t0, SyntaxKind.TraitKeyword);

    // Type parameters are introduced by '<' and end with '>'
    const t2 = tokens.peek();
    if (t2.kind === SyntaxKind.LtSign) {
      tokens.get();
      typeParams = this.parseGenericTypeParameters(tokens);
      const t2 = tokens.get();
      assertToken(t2, SyntaxKind.GtSign);
    }

    // A trait must be named by an identifier
    const t1 = tokens.get();
    assertToken(t1, SyntaxKind.Identifier);
    name = t1 as Identifier;

    let lastToken = t1;

    if (tokens.peek().kind === SyntaxKind.Colon) {
      tokens.get();
      typeBoundExpr = this.parseTypeExpression(tokens);
    }

    // The trait may optionally have 'fn ...' and 'type ..' elements wrapped in braces.
    const t3 = tokens.peek();
    if (t3.kind === SyntaxKind.Braced) {
      const t3 = tokens.get();
      lastToken = t3;
      const innerTokens = createTokenStream(t3);
      elements = this.parseTraitOrImplElements(innerTokens);
      assertNoTokens(innerTokens);
    }

    // Create and return the resulting AST node
    const result = createTraitDeclaration(modifiers, typeParams, name, typeBoundExpr, elements);
    setOrigNodeRange(result, firstToken, lastToken);
    return result;
  }

  public parseImplDeclaration(tokens: TokenStream): ImplDeclaration {

    let modifiers = 0;
    let typeParams = null;
    let traitTypeExpr;
    let typeExpr = null;

    // Parse the 'pub' keyword
    let t0 = tokens.get();
    const firstToken = t0;
    if (t0.kind === SyntaxKind.PubKeyword) {
      modifiers |= Modifiers.IsPublic;
      t0 = tokens.get();
    }

    // By now, we should encounter the 'impl' keyword
    assertToken(t0, SyntaxKind.ImplKeyword);

    // Type parameters are introduced by '<' and end with '>'
    const t1 = tokens.peek();
    if (t1.kind === SyntaxKind.LtSign) {
      typeParams = this.parseGenericTypeParameters(tokens);
      const t2 = tokens.get();
      assertToken(t2, SyntaxKind.GtSign);
    }

    // Check for the 'for' keyword occuring before '{' .. '}'
    let i = 2;
    let foundForKeyword = false;
    while (true) {
      const tn = tokens.peek(i++);
      if (tn.kind === SyntaxKind.Braced || tn.kind === SyntaxKind.EndOfFile) {
        break;
      }
      if (tn.kind === SyntaxKind.ForKeyword) {
        foundForKeyword = true;
      }
    }

    if (foundForKeyword) {

      // Parse the type expression that references the trait the user wants to implement
      traitTypeExpr = this.parseReferenceTypeExpression(tokens);

      // Skip the 'for' keyword itself
      assertToken(tokens.get(), SyntaxKind.ForKeyword);

      // Parse the type that this implementation is for
      typeExpr = this.parseReferenceTypeExpression(tokens);

    } else {

      // Just parse the trait the user wants to implement and leave the rest as is
      const resultTypeExpr = this.parseReferenceTypeExpression(tokens);

      // We cheat a bit by assigning the referenced trait to both fields
      // NOTE Assigning the same node by reference to different fields should be done with great care.
      typeExpr =  resultTypeExpr;
      traitTypeExpr = resultTypeExpr;

    }

    // Parse all 'fn ...' and 'type ...' elements
    const t5 = tokens.get();
    assertToken(t5, SyntaxKind.Braced);
    const elements = this.parseTraitOrImplElements(createTokenStream(t5));

    // Create and return the result
    const result = createImplDeclaration(modifiers, typeParams, traitTypeExpr, typeExpr, elements);
    setOrigNodeRange(result, firstToken, t5);
    return result;
  }

  public parseDeclarationLike(tokens: TokenStream): DeclarationLike {
    let t0 = tokens.peek(1);
    let i = 1;
    if (t0.kind === SyntaxKind.PubKeyword) {
      t0 = tokens.peek(++i);
      if (t0.kind !== SyntaxKind.ForeignKeyword) {
        if (KIND_DECLARATION_KEYWORD.indexOf(t0.kind) === -1) {
          throw new ParseError(t0, KIND_DECLARATION_KEYWORD);
        }
      }
    }
    if (t0.kind === SyntaxKind.ForeignKeyword) {
      i += 2;
      t0 = tokens.peek(i);
      if (KIND_DECLARATION_KEYWORD.indexOf(t0.kind) === -1) {
        throw new ParseError(t0, KIND_DECLARATION_KEYWORD);
      }
    }
    switch (t0.kind) {
      case SyntaxKind.ImplKeyword:
        return this.parseImplDeclaration(tokens);
      case SyntaxKind.TraitKeyword:
        return this.parseTraitDeclaration(tokens);
      case SyntaxKind.TypeKeyword:
        return this.parseTypeAliasDeclaration(tokens);
      case SyntaxKind.FnKeyword:
        return this.parseFunctionDeclaration(tokens);
      case SyntaxKind.LetKeyword:
        return this.parseVariableDeclaration(tokens);
      case SyntaxKind.StructKeyword:
        return this.parseRecordDeclaration(tokens);
      case SyntaxKind.EnumKeyword:
        return this.parseEnumDeclaration(tokens);
      default:
        throw new ParseError(t0, KIND_DECLARATION_T0);
      }
  }

  private getFirstTokenAfterModifiers(tokens: TokenStream): Token {
    let mustBeDeclOrImport = false;
    let mustBeFunctionOrVariable = false;
    let i = 1;
    let t0 = tokens.peek(i);
    if (t0.kind === SyntaxKind.PubKeyword) {
      mustBeDeclOrImport = true;
      t0 = tokens.peek(++i);
    }
    if (t0.kind === SyntaxKind.ForeignKeyword) {
      mustBeFunctionOrVariable = true;
      i += 2;
      t0 = tokens.peek(i);
    }
    if (mustBeFunctionOrVariable
      && t0.kind !== SyntaxKind.StructKeyword
      && t0.kind !== SyntaxKind.FnKeyword) {
      throw new ParseError(t0, [SyntaxKind.StructKeyword, SyntaxKind.FnKeyword]);
    }
    if (mustBeDeclOrImport && KIND_DECLARATION_T0.indexOf(t0.kind) === -1 && t0.kind !== SyntaxKind.ImportKeyword) {
      throw new ParseError(t0, KIND_DECLARATION_KEYWORD);
    }
    return t0;
  }

  private lookaheadIsMacroCall(tokens: TokenStream): boolean {
    return tokens.peek(1).kind === SyntaxKind.Identifier
        && tokens.peek(2).kind === SyntaxKind.ExMark;
  }

  public parseSourceElement(tokens: TokenStream): SourceElement {
    if (this.lookaheadIsMacroCall(tokens)) {
      return this.parseMacroCall(tokens);
    }
    const t0 = tokens.peek();
    const t1 = this.getFirstTokenAfterModifiers(tokens);
    if (t1.kind ===  SyntaxKind.ImportKeyword) {
      return this.parseImportDeclaration(tokens);
    } else if (t1.kind === SyntaxKind.ModKeyword) {
      return this.parseModuleDeclaration(tokens);
    } else if (KIND_STATEMENT_T0.indexOf(t1.kind) !== -1) {
      return this.parseStatement(tokens);
    } else if (KIND_DECLARATION_KEYWORD.indexOf(t1.kind) !== -1) {
      return this.parseDeclarationLike(tokens);
    } else {
      throw new ParseError(t0, KIND_SOURCEELEMENT_T0);
    }
  }

  public parseFunctionBodyElement(tokens: TokenStream): FunctionBodyElement {
    if (this.lookaheadIsMacroCall(tokens)) {
      return this.parseMacroCall(tokens);
    }
    const t0 = this.getFirstTokenAfterModifiers(tokens);
    if (KIND_STATEMENT_T0.indexOf(t0.kind) !== -1) {
      return this.parseStatement(tokens);
    } else if (t0.kind === SyntaxKind.LetKeyword) {
      return this.parseVariableDeclaration(tokens); 
    } else if (t0.kind === SyntaxKind.FnKeyword) {
      return this.parseFunctionDeclaration(tokens);
    } else {
      throw new ParseError(t0, [...KIND_STATEMENT_T0, SyntaxKind.LetKeyword, SyntaxKind.FnKeyword]);
    }
  }

  public parseMacroCall(tokens: TokenStream): MacroCall {
    const t0 = tokens.get();
    assertToken(t0, SyntaxKind.Identifier);
    const t1 = tokens.get();
    assertToken(t1, SyntaxKind.ExMark);
    const t2 = tokens.get();
    if (!isPunctuated(t2)) {
      throw new ParseError(t2, [SyntaxKind.Braced, SyntaxKind.Parenthesized, SyntaxKind.Bracketed]);
    }
    const result = createMacroCall(t0 as Identifier, t2.text);
    setOrigNodeRange(result, t0, t2);
    return result;
  }

  public parseFunctionBodyElements(tokens: TokenStream): FunctionBodyElement[] {
    const elements: FunctionBodyElement[] = []
    while (true) {
      const t0 = tokens.peek();
      if (t0.kind === SyntaxKind.EndOfFile) {
        break;
      }
      if (t0.kind === SyntaxKind.Semi) {
        tokens.get();
        continue;
      }
      elements.push(this.parseFunctionBodyElement(tokens));
    }
    return elements
  }

  public parseSourceElements(tokens: TokenStream): SourceElement[] {
    const elements: SourceElement[] = []
    while (true) {
      const t0 = tokens.peek();
      if (t0.kind === SyntaxKind.EndOfFile) {
        break;
      }
      if (t0.kind === SyntaxKind.Semi) {
        tokens.get();
        continue;
      }
      elements.push(this.parseSourceElement(tokens));
    }
    return elements
  }

  public parseSourceFile(tokens: TokenStream, pkg: Package): SourceFile {
    const elements = this.parseSourceElements(tokens);
    const t1 = tokens.peek();
    assertToken(t1, SyntaxKind.EndOfFile);
    return createSourceFile(
      elements,
      pkg,
      new TextSpan(t1.span!.file, new TextPos(0,1,1), t1.span!.end.clone())
    );
  }

  public parseTraitOrImplElements(tokens: TokenStream, allowPub = false): TraitOrImplElement[] {
    const elements: TraitOrImplElement[] = [];
    while (true) {
      const t0 = tokens.peek();
      if (t0.kind === SyntaxKind.EndOfFile) {
        break;
      }
      if (t0.kind === SyntaxKind.Semi) {
        tokens.get();
        continue;
      }
      elements.push(this.parseTraitOrImplElement(tokens))
    }
    return elements;
  }

  public parseTraitOrImplElement(tokens: TokenStream): TraitOrImplElement {
    if (this.lookaheadIsMacroCall(tokens)) {
      return this.parseMacroCall(tokens);
    }
    let i = 1;
    let t0 = tokens.peek(i);
    if (t0.kind === SyntaxKind.PubKeyword) {
      i += 1;
      t0 = tokens.peek(i);
    }
    if (t0.kind === SyntaxKind.ForeignKeyword) {
      i += 2;
      t0 = tokens.peek(i);
    }
    switch (t0.kind) {
      case SyntaxKind.FnKeyword:
        return this.parseFunctionDeclaration(tokens);
      case SyntaxKind.TypeKeyword:
        return this.parseTypeAliasDeclaration(tokens);
      default:
        throw new ParseError(t0, [SyntaxKind.FnKeyword, SyntaxKind.TypeAliasDeclaration, SyntaxKind.MacroCall])
    }
  }

  private canParseExpression(tokens: TokenStream): boolean {
    // TODO
    return false;
  }

  private canParseReturnStatement(tokens: TokenStream): boolean {
    const t0 = tokens.get();
    if (t0.kind !== SyntaxKind.ReturnKeyword) {
      return false;
    }
    const t1 = tokens.peek();
    if (t1.kind === SyntaxKind.EndOfFile) {
      return true;
    }
    return this.canParseExpression(tokens);
  }

  private canParseLoopStatement(tokens: TokenStream): boolean {
    if (tokens.peek(1).kind !== SyntaxKind.LoopKeyword) {
      return false;
    }
    return true;
  }

  private canParseStatement(tokens: TokenStream): boolean {
    const t0 = tokens.peek();
    switch (t0.kind) {
      case SyntaxKind.ReturnKeyword:
        return this.canParseReturnStatement(tokens);
      case SyntaxKind.LoopKeyword:
        return this.canParseLoopStatement(tokens);
      default:
        return this.canParseExpression(tokens)
    }
  }

  private canParseFunctionDeclaration(tokens: TokenStream): boolean {
    let t0 = tokens.peek();
    if (t0.kind === SyntaxKind.PubKeyword) {
      tokens.get();
      t0 = tokens.peek();
    }
    if (t0.kind === SyntaxKind.ForeignKeyword) {
      tokens.get();
      const t1 = tokens.get();
      if (t1.kind !== SyntaxKind.StringLiteral) {
        return false;
      }
      t0 = tokens.peek();
    }
    // TODO
    return true;
  }

  private canParseRecordDeclaration(tokens: TokenStream): boolean {
    // TODO
    return true;
  }

  private canParseVariableDeclaration(tokens: TokenStream): boolean {
    // TODO
    return true;
  }

  private canParseDeclaration(tokens: TokenStream): boolean {
    let i = 0;
    let t0 = tokens.peek(i);
    while (isModifierKeyword(t0.kind)) {
      t0 = tokens.peek(++i);
    }
    switch (t0.kind) {
      case SyntaxKind.FnKeyword:
        return this.canParseFunctionDeclaration(tokens);
      case SyntaxKind.StructKeyword:
        return this.canParseRecordDeclaration(tokens);
      default:
        return false;
    }
  }

  private canParseSourceElement(tokens: TokenStream): boolean {
    return this.canParseStatement(tokens)
        || this.canParseDeclaration(tokens)
  }

  private canParseRecordMember(tokens: TokenStream): boolean {
    // TODO
    return true;
  }

  private canParseFunctionBodyElement(tokens: TokenStream): boolean {
    return this.canParseFunctionDeclaration(tokens)
        || this.canParseStatement(tokens)
        || this.canParseVariableDeclaration(tokens);
  }

  private canParseRecordMembers(tokens: TokenStream): boolean {
    while (true) {
      const t0 = tokens.peek();
      if (t0.kind === SyntaxKind.EndOfFile) {
        break;
      }
      if (!this.canParseRecordMember(tokens)) {
        return false;
      }
    }
    return true;
  }

  private canParseSourceElements(tokens: TokenStream): boolean {
    while (true) {
      const t0 = tokens.peek();
      if (t0.kind === SyntaxKind.EndOfFile) {
        break;
      }
      if (!this.canParseSourceElement(tokens)) {
        return false;
      }
    }
    return true;
  }

  private canParseFunctionBodyElements(tokens: TokenStream): boolean {
    while (true) {
      const t0 = tokens.peek();
      if (t0.kind === SyntaxKind.EndOfFile) {
        break;
      }
      if (!this.canParseFunctionBodyElement(tokens)) {
        return false;
      }
    }
    return true;
  }

}
