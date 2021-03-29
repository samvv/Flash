import {
  ReturnStatement,
  SyntaxKind,
  Expression,
  kindToString,
  Syntax,
  isPunctuated,
  SourceFile,
  Modifiers,
  FunctionBodyElement,
  Token,
  Symbol,
  FunctionBody
} from "./ast";

import { BOLT_SUPPORTED_LANGUAGES } from "./constants"
import { assert, registerClass, GeneratorStream, FastMultiMap } from "./util";
import { TextSpan, TextPos, TextFile } from "./text";
import { Scanner } from "./scanner";
import { HardParseError, ParseError } from "./errors";
import { NODE_TYPES } from "./ast"
import { Package } from "./package";

for (const key of Object.keys(NODE_TYPES)) {
  registerClass((NODE_TYPES as any)[key]);
}

export function getPackage(node: SourceFile): Package {
  const sourceFile = node.getSourceFile() as SourceFile;
  assert(sourceFile.kind === SyntaxKind.SourceFile)
  assert(sourceFile.pkg !== null);
  return sourceFile.pkg!;
}

export function getNodeLanguage(node: Syntax): string {
  const kindStr = kindToString(node.kind);
  for (const prefix of BOLT_SUPPORTED_LANGUAGES) {
    if (kindStr.startsWith(prefix)) {
      return prefix;
    }
  }
  throw new Error(`Could not determine the language of ${kindStr}`);
}

export function createTokenStream(value: any) {
  if (value instanceof Scanner) {
    return new GeneratorStream<Token>(() => value.scan());
  } else if (typeof(value) === 'string') {
    const scanner = new Scanner(new TextFile('#<anonymous>'), value);
    return new GeneratorStream<Token>(() => scanner.scan());
  } else if (isPunctuated(value)) {
    const origPos = value.span!.start;
    const startPos = new TextPos(origPos.offset+1, origPos.line, origPos.column+1);
    const scanner = new Scanner(value.span!.file, value.text, startPos);
    return new GeneratorStream<Token>(() => scanner.scan());
  } else {
    throw new Error(`Could not convert ${kindToString(value.kind)} to a token stream.`);
  }
}

export const EOF = ''

export function cloneSpan(span: TextSpan | null) {
  if (span === null) {
    return null;
  }
  return span.clone();
}

export function setOrigNodeRange(node: Syntax, startNode: Syntax, endNode: Syntax): void {
  node.span = new TextSpan(startNode.span!.file, startNode.span!.start.clone(), endNode.span!.end.clone());
}

export function getReturnStatementsInFunctionBody(body: FunctionBody): ReturnStatement[] {

  const results: ReturnStatement[] = [];

  for (const element of body) {
    visit(element);
  }

  return results;

  function visit(node: FunctionBodyElement) {
    switch (node.kind) {
      case SyntaxKind.ReturnStatement:
        results.push(node);
        break;
      case SyntaxKind.ExpressionStatement:
        visitExpression(node.expression);
        break;
    }
  }

  function visitExpression(node: Expression) {
    switch (node.kind) {
      case SyntaxKind.BlockExpression:
        for (const element of node.elements) {
          visit(element);
        }
        break;
      case SyntaxKind.MatchExpression:
        for (const arm of node.arms) {
          visitExpression(arm.body);
        }
        break;
      case SyntaxKind.CallExpression:
        visitExpression(node.operator);
        for (const operand of node.operands) {
          visitExpression(operand);
        }
        break;
    }
  }

}

export enum OperatorKind {
  Prefix,
  InfixL,
  InfixR,
  Suffix,
}

export function isRightAssoc(kind: OperatorKind) {
  return kind === OperatorKind.InfixR;
}

export function getSymbolText(node: Symbol): string {
  switch (node.kind) {
    case SyntaxKind.Identifier:
      return node.text;
    case SyntaxKind.Operator:
      return node.text;
    case SyntaxKind.GtSign:
      return '>';
    case SyntaxKind.ExMark:
      return '!';
    case SyntaxKind.LtSign:
      return '<';
    case SyntaxKind.VBar:
      return '|';
    default:
      throw new Error(`Could not convert the node ${kindToString((node as any).kind)} to the name of an operator`);
  }
}

export interface OperatorInfo {
  kind: OperatorKind;
  arity: number;
  name: string;
  precedence: number;
}

export function forceToken(node: Token, kind: SyntaxKind) {
  if (node.kind !== kind) {
    throw new HardParseError(node, [ kind ]);
  }
}

export function assertToken(node: Token, kind: SyntaxKind) {
  if (node.kind !== kind) {
    throw new ParseError(node, [ kind ]);
  }
}

type OperatorTableList = [OperatorKind, number, string][][];

export class OperatorTable {

  private operatorsByName = new FastMultiMap<string, OperatorInfo>();

  constructor(definitions: OperatorTableList) {
    let i = 0;
    for (const group of definitions) {
      for (const [kind, arity, name] of group) {
        const info = { kind, arity, name, precedence: i }
        this.operatorsByName.add(name, info);
      }
      i++;
    }
  }

  public lookup(arity: number, name: string): OperatorInfo | null {
    if (!this.operatorsByName.has(name)) {
      return null;
    }
    for (const operatorInfo of this.operatorsByName.get(name)) {
      if (operatorInfo.arity === arity) {
        return operatorInfo;
      }
    }
    return null;
  }

}

export function getModulePathToNode(node: Syntax): string[] {
  let elements = [];
  while (true) {
    if (node.kind === SyntaxKind.Module) {
      for (const element of node.name) {
        elements.unshift(element.text);
      }
    }
    if (node.parentNode === null) {
      break;
    }
    node = node.parentNode;
  }
  return elements;
}

export function isExported(node: Syntax) { 
  switch (node.kind) {
    case SyntaxKind.VariableDeclaration:
    case SyntaxKind.FunctionDeclaration:
    case SyntaxKind.Module:
    case SyntaxKind.RecordDeclaration:
    case SyntaxKind.TypeAliasDeclaration:
    case SyntaxKind.TraitDeclaration:
    case SyntaxKind.ImplDeclaration:
      return (node.modifiers & Modifiers.IsPublic) > 0;
    default:
      return false;
  }
}

//export function getFullyQualifiedPathToNode(node: Syntax): SymbolPath {
//  const symbolPath = convertNodeToSymbolPath(node);
//  while (true) {
//    const parentNode = node.parentNode;
//    if (parentNode === null) {
//      break;
//    }
//    node = parentNode;
//    if (node.kind === SyntaxKind.Module) {
//      for (const element of node.name) {
//        symbolPath.modulePath.unshift(element.text);
//      }
//    }
//  }
//  return symbolPath;
//}

export function describeKind(kind: SyntaxKind): string {
  switch (kind) {
    case SyntaxKind.ImportKeyword:
      return "'import'";
    case SyntaxKind.ExportKeyword:
      return "'export'";
    case SyntaxKind.ExMark:
      return "'!'";
    case SyntaxKind.Identifier:
      return "an identifier"
    case SyntaxKind.Operator:
      return "an operator"
    case SyntaxKind.StringLiteral:
      return "a string"
    case SyntaxKind.IntegerLiteral:
      return "an integer"
    case SyntaxKind.FnKeyword:
      return "'fn'"
    case SyntaxKind.WhereKeyword:
      return "'where'";
    case SyntaxKind.QuoteKeyword:
      return "'quote'";
    case SyntaxKind.ModKeyword:
      return "'mod'";
    case SyntaxKind.ForeignKeyword:
      return "'foreign'"
    case SyntaxKind.MatchKeyword:
      return "'match'";
    case SyntaxKind.YieldKeyword:
      return "'yield'";
    case SyntaxKind.ReturnKeyword:
      return "'return'";
    case SyntaxKind.PubKeyword:
      return "'pub'"
    case SyntaxKind.LetKeyword:
      return "'let'"
    case SyntaxKind.Semi:
      return "';'"
    case SyntaxKind.Colon:
      return "':'"
    case SyntaxKind.ColonColon:
      return "'::'";
    case SyntaxKind.Dot:
      return "'.'"
    case SyntaxKind.RArrow:
      return "'->'"
    case SyntaxKind.VBar:
      return "'|'";
    case SyntaxKind.Comma:
      return "','"
    case SyntaxKind.ModKeyword:
      return "'mod'"
    case SyntaxKind.StructKeyword:
      return "'struct'"
    case SyntaxKind.EnumKeyword:
      return "'enum'"
    case SyntaxKind.TypeKeyword:
      return "'type'";
    case SyntaxKind.Braced:
      return "'{' .. '}'"
    case SyntaxKind.Bracketed:
      return "'[' .. ']'"
    case SyntaxKind.Parenthesized:
      return "'(' .. ')'"
    case SyntaxKind.EndOfFile:
      return "'}', ')', ']' or end-of-file"
    case SyntaxKind.LtSign:
      return "'<'";
    case SyntaxKind.GtSign:
      return "'<'";
    case SyntaxKind.EqSign:
      return "'='";
    case SyntaxKind.TraitKeyword:
      return "'trait'";
    case SyntaxKind.TraitKeyword:
      return "'impl'";
    case SyntaxKind.ImplKeyword:
      return "'impl'";
    case SyntaxKind.ForKeyword:
      return "'for'";
    case SyntaxKind.RArrowAlt:
      return "'=>'";
    case SyntaxKind.Braced:
      return "'{ ... }'";
    case SyntaxKind.IfKeyword:
      return "'if'";
    case SyntaxKind.ElseKeyword:
      return "'else'";
    case SyntaxKind.TypeAliasDeclaration:
      return "a type alias";
    case SyntaxKind.MacroCall:
      return "a macro call";
    default:
      throw new Error(`failed to describe ${kindToString(kind)}`)
  }
}

export function *getAllReturnStatementsInFunctionBody(body: FunctionBodyElement[]): Generator<ReturnStatement> {
  for (const element of body) {
    switch (element.kind) {
      case SyntaxKind.ReturnStatement:
      {
        yield element;
        break;
      }
      case SyntaxKind.CaseStatement:
      {
        for (const caseNode of element.cases) {
          yield* getAllReturnStatementsInFunctionBody(caseNode.body);
        }
        break;
      }
      case SyntaxKind.ConditionalStatement:
      {
        for (const caseNode of element.cases) {
          yield* getAllReturnStatementsInFunctionBody(caseNode.body);
        }
        break;
      }
      case SyntaxKind.ExpressionStatement:
        // TODO Find return statements in expression blocks
        break;
      default:
        throw new Error(`I did not know how to find return statements in ${kindToString(element.kind)}`);
    }
  }
}
