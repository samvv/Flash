
import chalk from "chalk";

import { VariableDeclaration, Syntax, SyntaxKind } from "./ast";
import { format, MapLike, FormatArg, assert, escapeChar, countDigits } from "./util";
import { TextPos, TextFile, TextSpan } from "./text";
import { describeKind, EOF } from "./common";
import { isType } from "./types";
import { ArrowType, Type, TypeVar } from "./types";
import {BOLT_DIAG_NUM_EXTRA_LINES} from "./constants";

export const E_TYPE_ORIGINATED_FROM_HERE = "Type originated from here."
export const E_FIRST_TYPE_ORIGINATED_FROM_HERE = "First type originated from here."
export const E_SECOND_TYPE_ORIGINATED_FROM_HERE = "Second type originated from here."
export const E_UNINITIALIZED_BINDING = "This variable is read-only and has not been initialized."
export const E_UNBOUND_FREE_VARIABLE = "Type {type} has {typeVar} as an unbound free type variable."
export const E_PARAM_COUNT_MISMATCH = "Type {left} accepts {leftCount} arguments while {right} accepts {rightCount}"
export const E_ASSIGN_TUPLE_LENGTH_MISMATCH = "Trying to assign a tuple of length {rhsLength} to a tuple of length {lhsLength}"
export const E_MAY_NOT_RETURN_BECAUSE_TYPE_RESOLVES_TO_VOID = "May not return a value because the function's return type resolves to '()'"
export const E_MUST_RETURN_BECAUSE_TYPE_DOES_NOT_RESOLVE_TO_VOID = "Must return a value because the function's return type does not resolve to '()'"
export const E_INVALID_TEST_COMPARE = "The given test results cannot be compared because they use different specifications."
export const E_TESTS_DO_NOT_COMPARE = "This test does not compare with its expected output."
export const E_NO_BOLTFILE_FOUND_IN_PATH_OR_PARENT_DIRS = 'No Boltfile found in {path} or any of its parent directories.'
export const E_SCAN_ERROR = "Got an unexpected {char}"
export const E_STDLIB_NOT_FOUND = "Package 'stdlib' is required to build the current source set but it was not found. Use --no-std if you know what you are doing."
export const E_PARSE_ERROR = "Expected {expected:enum} but got {actual}"
export const E_MAY_NOT_RETURN_A_VALUE = "Returning a value inside a function that does not return values."
export const E_FILE_NOT_FOUND = "A file named {filename} was not found.";
export const E_FIELD_HAS_INVALID_VERSION_NUMBER = "Field '{name}' contains an invalid version nunmber."
export const E_FIELD_MUST_BE_STRING = "Field '{name}' must be a string."
export const E_FIELD_NOT_PRESENT = "Field '{name}' is not present."
export const E_FIELD_MUST_BE_BOOLEAN = "Field '{name}' must be a either 'true' or 'false'."
export const E_TYPE_DECLARATION_NOT_FOUND = "A type declaration named '{name}' was not found."
export const E_DECLARATION_NOT_FOUND = "Reference to an undefined declaration '{name}'.";
export const E_TYPE_UNIFICATION_FAILURE = "Types '{left}' and '{right}' could not be unified.";
export const E_THIS_NODE_CAUSED_INVALID_TYPE = "This expression resolved to the type {type}, which is incompatible with {origType}."
export const E_TOO_FEW_ARGUMENTS_FOR_FUNCTION_CALL = "Too few arguments for function call. Expected {expected} but got {actual}.";
export const E_TOO_MANY_ARGUMENTS_FOR_FUNCTION_CALL = "Too many arguments for function call. Expected {expected} but got {actual}.";
export const E_NOT_CALLABLE = "The result of this expression is not callable."
export const E_CANDIDATE_FUNCTION_REQUIRES_THIS_PARAMETER = "Candidate function requires this parameter."
export const E_ARGUMENT_HAS_NO_CORRESPONDING_PARAMETER = "This argument is missing a corresponding parameter."
export const E_INVALID_ARGUMENTS = "Invalid arguments passed to function '{name}'"
export const E_RECORD_FIELD_NOT_FOUND = "Record {name} does not have a member declaration named {fieldName}"
export const E_TYPE_NEVER_MATCHES = "Type '{type}' never matches anything."
export const E_TYPES_MISSING_MEMBER = "Not all types resolve to a record with the a member named '{name}'."
export const E_NODE_DOES_NOT_CONTAIN_MEMBER = "This node does not contain the the member '{name}'."
export const E_ARGUMENT_TYPE_NOT_ASSIGNABLE = "This argument's type '{argType}' is not assignable to the function's parameter type '{paramType}'."
export const E_PARAMETER_DECLARED_HERE = "The parameter was declared here with type {type}."
export const E_BUILTIN_TYPE_MISSING = "A built-in type named '{name}' was not found in the prelude."
export const E_BOLTFILE_INVALID = "The Boltfile in {path} is not valid."

export abstract class CompileError extends Error {

  public abstract readonly severity: string;

  public node?: Syntax;
  public position?: TextPos;
  public file?: TextFile;

  constructor(private messageTemplate: string) {
    super();
  }

  public get message(): string {
    let message = '';
    if (this.node) {
      assert(this.node.span !== null);
      message += `${this.node.span!.file.origPath}:${this.node.span!.start.line}:${this.node.span!.start.column}: `
    } else if (this.position) {
      assert(this.file !== undefined);
      message += `${this.file!.origPath}:${this.position.line}:${this.position.column}: `
    }
    message += this.messageText;
    return message;
  }

  public get messageText(): string {
    const args: MapLike<FormatArg> = {};
    for (const key of Object.keys(this)) {
      args[key] = (this as any)[key];
    }
    return format(this.messageTemplate, args);
  }

  public print({ indentation = ' ' } = {}) {

    let out = indentation;

    switch (this.severity) {
      case 'error':
        out += chalk.bold.red('error: ');
        break;
      case 'fatal':
        out += chalk.bold.red('fatal: ') 
        break;
      case 'warning':
        out += chalk.bold.red('warning: ');
        break;
      case 'info':
        out += chalk.bold.yellow('info: ')
        break;
      default:
        throw new Error(`Unkown severity for this message.`);
    }

    let span = null;
    if (this.node !== undefined) {
      span = this.node.span!;
    } else if (this.position !== undefined) {
      assert(this.file !== undefined);
      span = new TextSpan(this.file!, this.position, this.position)
    }

    if (span !== null) {
      out += chalk.bold.yellow(`${span.file.origPath}:${span.start.line}:${span.start.column}: `);
    }

    out += this.messageText + '\n\n';

    if (span !== null) {
      out += printExcerpt(span, { indentation });
      out += '\n'
    }

    for (const key of Object.keys(this)) {
      const arg = (this as any)[key];
      if (isType(arg) && arg.node !== null) {
        out += printExcerpt(arg.node.span!)
        out += '\n';
      }
    }

    process.stderr.write(out);

  }

}

export class FileNotFoundError extends CompileError {

  public readonly severity = 'fatal';

  constructor(public filename: string) {
    super(E_FILE_NOT_FOUND);
  }

}

export class InvalidBoltfileError extends CompileError {

  public readonly severity = 'fatal';

  constructor(public path: string, public errors: any) {
    super(E_BOLTFILE_INVALID);
  }

}

export class RecordFieldNotFoundError extends CompileError {

  public readonly severity = 'error';

  constructor(public fieldName: string) {
    super(E_RECORD_FIELD_NOT_FOUND);
  }

}

export class BoltfileNotFoundError extends CompileError {

  public readonly severity = 'fatal';

  constructor(public path: string) {
    super(E_NO_BOLTFILE_FOUND_IN_PATH_OR_PARENT_DIRS);
  }

}

export class ScanError extends CompileError {

  public readonly severity = 'fatal';

  public char: string;

  constructor(public file: TextFile, public position: TextPos, char: string) {
    super(E_SCAN_ERROR);
    this.char = char === EOF ? 'end-of-file' : `'${escapeChar(char)}'`;
  }

}

export class ParseError extends CompileError {

  public severity = 'fatal';

  public actual: string;
  public expected: string[];

  constructor(actual: Syntax, expected: SyntaxKind[]) {
    super(E_PARSE_ERROR);
    this.actual = describeKind(actual.kind);
    this.expected = expected.map(describeKind);
  }

}

export class UnificationError extends CompileError {

  public readonly severity = 'error';

  constructor(public left: Type, public right: Type) {
    super(E_TYPE_UNIFICATION_FAILURE);
  }
}

export class TypeNotFoundError extends CompileError {

  public readonly severity = 'error';

  constructor(public node: Syntax, public name: string) {
    super(E_TYPE_DECLARATION_NOT_FOUND);
  }

}

export class ParamCountMismatchError extends CompileError {

  public readonly severity = 'error';

  public leftCount: number;
  public rightCount: number;

  constructor(public left: ArrowType, public right: ArrowType) {
    super(E_PARAM_COUNT_MISMATCH);
    this.leftCount = left.paramTypes.length;
    this.rightCount = right.paramTypes.length;
  }

}

export class UnboundFreeVariableError extends CompileError {

  public readonly severity = 'error'

  constructor(public type: Type, public typeVar: TypeVar) {
    super(E_UNBOUND_FREE_VARIABLE);
  }

}

export class BindingNotFoundError extends CompileError {

  public readonly severity = 'error'

  constructor(public node: Syntax, public name: string) {
    super(E_DECLARATION_NOT_FOUND);
  }

}

export class UninitializedBindingError extends CompileError {

  public readonly severity = 'error'

  constructor(public node: VariableDeclaration) {
    super(E_UNINITIALIZED_BINDING);
  }

}

function printExcerpt(span: TextSpan, { indentation = '  ' } = {}) {
  let out = '';
  const content = span.file.getText();
  const startLine = Math.max(0, span.start.line-1-BOLT_DIAG_NUM_EXTRA_LINES)
  const lines = content.split('\n')
  const endLine = Math.min(lines.length, (span.end !== undefined ? span.end.line : startLine)+BOLT_DIAG_NUM_EXTRA_LINES)
  const gutterWidth = Math.max(2, countDigits(endLine+1))
  for (let i = startLine; i < endLine; i++) {
    const line = lines[i];
    let j = firstIndexOfNonEmpty(line);
    out +=  indentation + '  '+chalk.bgWhite.black(' '.repeat(gutterWidth-countDigits(i+1))+(i+1).toString())+' '+line+'\n'
    const gutter = indentation + '  '+chalk.bgWhite.black(' '.repeat(gutterWidth))+' '
    let mark: number;
    let skip: number;
    if (i === span.start.line-1 && i === span.end.line-1) {
      skip = span.start.column-1;
      mark = span.end.column-span.start.column;
    } else if (i === span.start.line-1) {
      skip = span.start.column-1;
      mark = line.length-span.start.column+1;
    } else if (i === span.end.line-1) {
      skip = 0;
      mark = span.end.column-1;
    } else if (i > span.start.line-1 && i < span.end.line-1) {
      skip = 0;
      mark = line.length;
    } else {
      continue;
    }
    if (j <= skip) {
      j = 0;
    }
    out += gutter+' '.repeat(j+skip)+chalk.red('~'.repeat(mark-j)) + '\n'
  }
  return out;
}

function firstIndexOfNonEmpty(str: string) {
  let j = 0;
  for (; j < str.length; j++) {
    const ch = str[j];
    if (ch !== ' ' && ch !== '\t') {
      break;
    }
  }
  return j
}

