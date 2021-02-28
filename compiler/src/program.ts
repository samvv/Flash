
import * as path from "path"
import { getPackage } from "./common"
import { Package } from "./package"
import { BoltSourceFile, NodeFlags, SourceFile, Syntax, SyntaxKind } from "./ast"
import { FastStringMap, assert, isInsideDirectory, stripExtensions } from "./util";

function isRelativePath(filePath: string): boolean {
  return filePath.startsWith('.');
}

export class Program {

  private packagesByName = new FastStringMap<string, Package>();
  private packagesToCompile = new Set<Package>();

  private sourceFilesByFilePath = new FastStringMap<string, SourceFile>();
  private sourceFilesToAutoImport = new Set<SourceFile>();

  constructor(
    private pkgs: Package[],
    private target: Package,
  ) {
    for (const pkg of pkgs) {
      if (pkg.name !== null) {
        this.packagesByName.add(pkg.name, pkg);
      }
      for (const sourceFile of pkg.getAllSourceFiles()) {
        this.sourceFilesByFilePath.add(stripExtensions(sourceFile.span!.file.fullPath), sourceFile);
        if (sourceFile.flags & NodeFlags.AutoImported) {
          this.sourceFilesToAutoImport.add(sourceFile);
        }
      }
    }
  }

  public getAllSourceFiles() {
    return this.sourceFilesByFilePath.values();
  }

  public getSourceFile(filepath: string): SourceFile | null {
    assert(path.isAbsolute(filepath));
    if (!this.sourceFilesByFilePath.has(filepath)) {
      return null;
    }
    return this.sourceFilesByFilePath.get(filepath);
  }

  public getAllPackages(): Iterable<Package> {
    return this.pkgs;
  }

  public getAllGloballyDeclaredSourceFiles(): Iterable<SourceFile> {
    return this.sourceFilesToAutoImport;
  }

  public getPackageNamed(name: string): Package {
    return this.packagesByName.get(name);
  }

  public resolveToSourceFile(importPath: string, fromNode: Syntax): SourceFile | null {

    // This will contain the resulting path to the source file after resolution
    // has taken place.
    let resolvedFilePath: string;

    if (isRelativePath(importPath)) {

      // If the import is relative, we need to get the package the import was
      // defined in and start resolving from there.

      const sourceFile = fromNode.getSourceFile() as BoltSourceFile;
      assert(sourceFile.kind === SyntaxKind.BoltSourceFile)
      resolvedFilePath = path.join(sourceFile.pkg!.rootDir, importPath.substring(2));
      assert(isInsideDirectory(resolvedFilePath, sourceFile.pkg!.rootDir));

    } else {

      // If the import is absolute, the first component denotes the package
      // name and the rest the path to the source file.
      // Example: yaml/parser

      const pathElements = importPath.split('/');

      const pkg = this.getPackageNamed(pathElements[0]);

      let filename: string;

      if (pathElements.length === 1) {

        // If the import path just referenced the package and not a specific
        // file then we need to resolve to the default 'lib.bolt' source.
        filename = 'lib.bolt';

      } else {

        // If the import path referenced a specific file we need to extract the
        // filename from the full path.
        filename = pathElements.slice(1).join(path.sep);

      }

      resolvedFilePath = path.join(pkg.basePath, filename);

      // This check is here for security reasons. It should probably be moved
      // to a separate validation layer.
      assert(isInsideDirectory(resolvedFilePath, pkg.rootDir));
    }

    return this.getSourceFile(resolvedFilePath);
  }

  public updateSourceFile(oldSourceFile: SourceFile, newSourceFile: SourceFile): void {
    if (!this.sourceFilesByFilePath.has(oldSourceFile.span!.file.fullPath)) {
      throw new Error(`Could not update ${oldSourceFile.span!.file.origPath} because it was not found in this program.`);
    }
    this.sourceFilesByFilePath.delete(oldSourceFile.span!.file.fullPath);
    this.sourceFilesByFilePath.add(newSourceFile.span!.file.fullPath, newSourceFile);
    this.sourceFilesToAutoImport.delete(oldSourceFile);
    if (newSourceFile.flags & NodeFlags.AutoImported) {
      this.sourceFilesToAutoImport.add(newSourceFile);
    }
  }

}

