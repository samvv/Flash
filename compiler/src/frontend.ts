
import { EventEmitter } from "events"
import * as fs from "fs-extra"
import { sync as globSync } from "glob"
import { now } from "moment"
import * as path from "path"

import { Token, NodeFlags, setParents, SourceFile } from "./ast"
import { FileNotFoundError, BoltfileNotFoundError } from "./errors"
import { DiagnosticPrinter } from "./diagnostics";
import { Evaluator } from "./evaluator"
import { Container } from "./ioc"
import { loadPackageMetadata, Package } from "./package"
import { Parser } from "./parser"
import { Program } from "./program"
import { Scanner } from "./scanner"
import { TextFile } from "./text"
import { GeneratorStream, getFileStem, MapLike, toArray, upsearchSync, verbose } from "./util"

const BOLTFILE_FILENAMES = [
  'Boltfile.yaml',
  'Boltfile.yml',
  'Boltfile',
]

function isDirectory(filePath: string): boolean {
  let stats;
  try {
    stats = fs.statSync(filePath);
  } catch (error) {
    if (error.code === 'ENOENT') {
      return false;
    }
    throw error;
  }
  return stats.isDirectory();
}

interface TimingInfo {
  timestamp: number;
  refCount: number;
}

function findCommonBasePath(filePaths: string[]): string {
  if (filePaths.length === 0) {
    return '.'
  }
  const firstFilePath = filePaths[0];
  let k;
  for (k = 0; k < firstFilePath.length; ) {
    let sepIndex = firstFilePath.indexOf(path.sep, k);
    if (sepIndex === -1) {
      sepIndex = firstFilePath.length;
    }
    const chunk = firstFilePath.substr(k, sepIndex);
    for (let j = 1; j < filePaths.length; j++) {
      const filePath = filePaths[j];
      let sepIndex = filePath.indexOf(path.sep, k);
      if (sepIndex === -1) {
        sepIndex = filePath.length;
      }
      if (filePath.substr(k, sepIndex) !== chunk) {
        break;
      }
    }
    k += chunk.length;
  }
  return firstFilePath.substr(0, k);
}

class Timing extends EventEmitter {

  private runningTasks: MapLike<TimingInfo> = Object.create(null);

  public start(name: string) {
    if (this.runningTasks[name] !== undefined) {
      this.runningTasks[name].refCount++;
      return;
    }
    this.runningTasks[name] = { timestamp: now(), refCount: 1 };
    this.emit(`start ${name}`);
  }

  public end(name: string) {
    if (this.runningTasks[name] === undefined) {
      throw new Error(`Task '${name}' was never started.`);
    }
    const info = this.runningTasks[name];
    info.refCount--;
    if (info.refCount === 0) {
      const usecs = now() - info.timestamp;
      verbose(`Task '${name}' completed after ${usecs} microseconds.`);
      this.emit(`end ${name}`);
    }
  }

}

export class Frontend {

  //public resolver = new SymbolResolver();
  //public evaluator = new Evaluator(this.resolver);
  public diagnostics: DiagnosticPrinter;
  public timing: Timing;

  constructor() {
    this.diagnostics = new DiagnosticPrinter();
    this.timing = new Timing();
  }

  //public compile(program: Program, target: string) {

  //  const container = new Container();
  //  const resolver = new SymbolResolver(program, new SymbolResolutionStrategy);
  //  for (const sourceFile of program.getAllSourceFiles()) {
  //    resolver.registerSourceFile(sourceFile as SourceFile);
  //  }
  //  const transforms = new TransformManager(container);
  //  container.bindSelf(transforms);
  //  container.bindSelf(program);
  //  container.bindSelf(resolver);

  //  switch (target) {

  //    case "JS":
  //      transforms.register(ExpandTransform);
  //      transforms.register(CompileToJSTransform);
  //      transforms.register(ConstFoldTransform);
  //      transforms.apply(program);
  //      break;

  //    default:
  //      throw new Error(`"${target}" is an invalid compile target.`);

  //  }

  //  for (const sourceFile of program.getAllSourceFiles()) {
  //    fs.mkdirp('.bolt-work');
  //    fs.writeFileSync(this.mapToTargetFile(sourceFile), emitNode(sourceFile), 'utf8');
  //  }

  //}

  //private mapToTargetFile(node: SourceFile) {
  //  return path.join('.bolt-work', getFileStem(node.span!.file.fullPath) + getDefaultFileExtension(getNodeLanguage(node)));
  //}

  public eval(program: Program) {

    // Type-check the program before proceeding
    program.check();

    const container = new Container();
    container.bindSelf(program);

    const evaluator = container.createInstance(Evaluator)

    for (const sourceFile of program.getAllSourceFiles()) {
      evaluator.eval(sourceFile)
    }

  }

  private parseSourceFile(filepath: string, pkg: Package): SourceFile {

    const file = new TextFile(filepath);
    const contents = fs.readFileSync(file.origPath, 'utf8');
    const scanner = new Scanner(file, contents)
    const tokens = new GeneratorStream<Token>(() => scanner.scan());
    const parser = new Parser();

    let sourceFile = parser.parseSourceFile(tokens, pkg);

    setParents(sourceFile);

    return sourceFile;
  }

  private getfilePath(packagePath: string): string {
    if (isDirectory(packagePath)) {
      for (const fileName of BOLTFILE_FILENAMES) {
        const fullPath = path.resolve(packagePath, fileName);
        if (fs.existsSync(fullPath)) {
          return fullPath;
        }
      }
      throw new FileNotFoundError(path.join(packagePath, BOLTFILE_FILENAMES[0]));
    } else {
      return packagePath;
    }
  }

  public* loadPackagesFromPath(packagePath: string, isDependency: boolean): Iterable<Package> {
    const boltfilePath = this.getfilePath(packagePath);
    const rootDir = path.dirname(boltfilePath);
    const data = toArray(loadPackageMetadata(boltfilePath));
    for (const packageSpec of data) {
      const pkg = new Package(rootDir, packageSpec.name, packageSpec.version);
      const sourceFiles: SourceFile[] = [];
      for (let sourceSpec of packageSpec.files ?? [ 'src/**/*.bolt' ]) {
        if (typeof(sourceSpec) === 'string') {
          sourceSpec = { path: sourceSpec }
        }
        for (const filePath of globSync(path.join(rootDir, sourceSpec.path))) {
          const sourceFile = this.parseSourceFile(filePath, pkg);
          if (sourceSpec["auto-import"]) {
            sourceFile.flags |= NodeFlags.AutoImported;
          }
          pkg.addSourceFile(sourceFile);
        }
      }
      pkg.basePath = findCommonBasePath(sourceFiles.map(sourceFile => sourceFile.span!.file.fullPath));
      yield pkg;
    }
  }

  public loadProgramFromFileList(filenames: string[], cwd = '.'): Program | null {

    cwd = path.resolve(cwd);

    if (filenames.length === 0) {
      const metadataPath = upsearchSync(BOLTFILE_FILENAMES, cwd);
      if (metadataPath === null) {
        this.diagnostics.add(new BoltfileNotFoundError(cwd));
        return null;
      }
      filenames.push(metadataPath);
    }

    const anonPkg = new Package(cwd, null, null);

    const pkgs = [ anonPkg ];

    for (const filename of filenames) {

      if (fs.statSync(filename).isDirectory()
          || BOLTFILE_FILENAMES.indexOf(path.basename(filename)) !== -1) {
        pkgs.push(...this.loadPackagesFromPath(filename, false));
      } else {
        const sourceFile = this.parseSourceFile(filename, anonPkg);
        if (sourceFile !== null) {
          anonPkg.addSourceFile(sourceFile);
        }
      }
    }

    return new Program(pkgs, anonPkg);
  }

}

