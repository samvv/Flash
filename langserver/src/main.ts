
import net from "net"
import minimist from "minimist"
import {
  Connection,
  createConnection,
  Diagnostic,
  DiagnosticSeverity,
  DidChangeConfigurationNotification,
  InitializeParams,
  InitializeResult,
  ProposedFeatures,
  TextDocuments,
  TextDocumentSyncKind
} from "vscode-languageserver";
import IntervalTree from "@flatten-js/interval-tree"
import { TextDocument } from "vscode-languageserver-textdocument";

import {
  Parser,
  Scanner,
  TextFile,
  TextSpan,
  createTokenStream,
  ParseError,
  ScanError,
  assert,
  FastStringMap,
  TypeChecker,
  isBoltReferenceExpression,
  isBoltToken,
  SourceFile,
  Syntax,
  prettyPrint
} from "@boltlang/compiler";

interface TextPositionLike {
  line: number;
  column: number;
}

interface TextSpanLike {
  start: TextPositionLike;
  end: TextPositionLike;
}

function isTextSpanBetween(start: TextPositionLike, target: TextSpanLike, end: TextPositionLike): boolean {
  return ((start.line === target.start.line && start.column >= target.start.column) || start.line > target.start.line)
      && (((end.line === target.end.line) && end.column <= target.end.column) || end.line < target.end.line);
}

const args = minimist(process.argv.slice(2))

if (args.socket) {
  if (typeof(args.socket) !== 'number') {
    console.error(`The flag --socket=PORT must contain a valid port number when provided.`)
    process.exit(1);
  }
  const server = net.createServer(socket => {
    const connection = createConnection(ProposedFeatures.all, socket, socket);
    configureConnection(connection)
  });
  server.listen(args.socket);
} else {
  const connection = createConnection(ProposedFeatures.all);
  configureConnection(connection);
}

interface BoltWorkspaceFolder {
  name: string;
  uri: string;
}

class SourceFileHandle {

  private offsetToNode = new IntervalTree()
  private lineToOffset = [ 0 ];

  constructor(
    public sourceText: string,
    public sourceFile: SourceFile
  ) {
    for (const node of sourceFile.preorder()) {
      if (!isBoltReferenceExpression(node)) {
        this.offsetToNode.insert([node.span!.start.offset, node.span!.end.offset], node)
      }
    }
    let currOffset = 0;
    while (currOffset < this.sourceText.length) {
      const ch = this.sourceText[currOffset]
      currOffset++;
      if (ch === '\n') {
        this.lineToOffset.push(currOffset);
      }
    }
  }

  public getOffset(position: TextPositionLike): number | null {
    if ((position.line-1) >= this.lineToOffset.length) {
      return null;
    }
    return this.lineToOffset[position.line] + position.column;
  }

  public getNodeAtOffset(offset: number): Syntax | null {
    const matches = this.offsetToNode.search([offset, offset]);
    if (matches.length === 0) {
      return null;
    }
    assert(matches.length === 1);
    return matches[0];
  }

}

function configureConnection(connection: Connection) {

  const workspaceFolders = new FastStringMap<string, BoltWorkspaceFolder>()

  const sourceFiles = new FastStringMap<string, SourceFileHandle>();
  const checker = new TypeChecker();

  const documents = new TextDocuments(TextDocument);

  let hasConfigurationCapability = false;
  let hasWorkspaceFolderCapability = false;
  let hasDiagnosticRelatedInformationCapability = false;

  connection.onInitialize((params: InitializeParams) => {

    const capabilities = params.capabilities

    hasConfigurationCapability = !!(capabilities.workspace && capabilities.workspace.configuration);
    hasWorkspaceFolderCapability = !!(capabilities.workspace && capabilities.workspace.workspaceFolders);
    hasDiagnosticRelatedInformationCapability = !!(capabilities.textDocument
      && capabilities.textDocument.publishDiagnostics
      && capabilities.textDocument.publishDiagnostics.relatedInformation);

    const result: InitializeResult = {
      capabilities: {
        hoverProvider: {
          workDoneProgress: false
        },
        textDocumentSync: TextDocumentSyncKind.Incremental,
      }
    }

    if (hasWorkspaceFolderCapability) {
      result.capabilities.workspace = {
        workspaceFolders: { supported: true }
      }
    }

    return result
  });

  connection.onInitialized(() => {
    if (hasConfigurationCapability) {
      connection.client.register(DidChangeConfigurationNotification.type, undefined);
    }
    if (hasWorkspaceFolderCapability) {
      connection.workspace.onDidChangeWorkspaceFolders(event => {
        for (const removed of event.removed) {
          workspaceFolders.delete(removed.uri)
        }
        for (const added of event.added) {
          workspaceFolders.add(added.uri, { name: added.name, uri: added.uri })
        }
      })
    }
  });

  documents.onDidChangeContent(event => {
    validateSourceFile(event.document)
  })

  function validateSourceFile(document: TextDocument) {
    const file = new TextFile(document.uri)
    const scanner = new Scanner(file, document.getText());
    const tokens = createTokenStream(scanner);
    const parser = new Parser()
    const diagnostics: Diagnostic[] = [];
    let sourceFile;
    try {
      sourceFile = parser.parseSourceFile(tokens)
    } catch (e) {
      if (e instanceof ParseError) {
        const span = e.actual.span!;
        diagnostics.push({
          severity: DiagnosticSeverity.Error,
          range: {
            start: { line: span.start.line-1, character: span.start.column-1 },
            end: { line: span.end.line-1, character: span.end.column-1 }
          },
          message: e.errorText
        });
        connection.sendDiagnostics({ uri: document.uri, diagnostics })
        return;
      } else if (e instanceof ScanError) {
        diagnostics.push({
          severity: DiagnosticSeverity.Error,
          range: {
            start: { line: e.position.line-1, character: e.position.column-1 },
            end: { line: e.position.line-1, character: e.position.column-1 },
          },
          message: e.diagnostic.formattedMessage
        });
        connection.sendDiagnostics({ uri: document.uri, diagnostics })
        return;
      } else {
        throw e;
      }
    }
    sourceFiles.add(document.uri, new SourceFileHandle(document.getText(), sourceFile));
    connection.sendDiagnostics({ uri: document.uri, diagnostics })
  }

  connection.onHover(params => {

    if (!sourceFiles.has(params.textDocument.uri)) {
      return null;
    }

    const sourceFileHandle = sourceFiles.get(params.textDocument.uri);
    const offset = sourceFileHandle.getOffset({
      line: params.position.line,
      column: params.position.character
    })

    if (offset === null) {
      return null;
    }

    const node = sourceFileHandle.getNodeAtOffset(offset);

    if (node === null) {
      return null;
    }

    const nodeType = checker.getTypeOfNode(node);

    if (nodeType === null) {
      return null;
    }

    return {
      contents: prettyPrint(nodeType),
      range: {
        start: { line: node.span!.start.line-1, character: node.span!.start.column-1  },
        end: { line: node.span!.end.line-1, character: node.span!.end.column-1 }
      },
    }

  })

  documents.listen(connection)

  connection.listen()

}
