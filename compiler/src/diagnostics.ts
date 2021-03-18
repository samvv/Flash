
import { CompileError } from "./errors";

const BOLT_HARD_ERRORS = process.env['BOLT_HARD_ERRORS'] ?? false;

export interface Diagnostics {
  add(diagnostic: CompileError): void;
}

export class DiagnosticIndex {

  private diagnostics = new Array<CompileError>();

  public add(diagnostic: CompileError) {
    this.diagnostics.push(diagnostic);
  }

  public getAllDiagnostics(): IterableIterator<CompileError> {
    return this.diagnostics[Symbol.iterator]();
  }

}

export class DiagnosticPrinter {

  public hasErrors = false;
  public hasFatal = false;

  public add(diagnostic: CompileError): void {

    if (diagnostic.severity === 'error') {
      this.hasErrors = true;
    }
    if (diagnostic.severity === 'fatal') {
      this.hasFatal = true;
    }

    if (BOLT_HARD_ERRORS &&
        (diagnostic.severity === 'error'
          || diagnostic.severity === 'fatal')) {
      throw diagnostic;
    }

    diagnostic.print();

  }

}

