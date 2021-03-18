
import { Syntax, SyntaxKind, kindToString } from "./ast"

export class Emitter {

  public emit(node: Syntax) {

    let out = '';

    switch (node.kind) {

      case SyntaxKind.QualName:
        if (node.modulePath !== null) {
          if (node.isAbsolute) {
            out += '::'
          }
          for (const element of node.modulePath) {
            out += element.text + '::';
          }
        }
        out += this.emit(node.name);
        break;

      case SyntaxKind.Identifier:
      case SyntaxKind.Operator:
        out += node.text;
        break;

      case SyntaxKind.GtSign:
        out += '>';
        break;

      case SyntaxKind.LtSign:
        out += '<';
        break;

      case SyntaxKind.EqSign:
        out += '=';
        break;

      case SyntaxKind.VBar:
        out += '|';
        break;

      case SyntaxKind.ExMark:
        out += '!';
        break;

      default:
        throw new Error(`Could not emit source code for ${kindToString(node.kind)}`)

    }

    return out;

  }

}

/**
 * A wrapper around `Emitter` for quick emission of AST nodes with sane defaults.
 */
export function emitNode(node: Syntax) {
  const emitter = new Emitter();
  return emitter.emit(node);
}

