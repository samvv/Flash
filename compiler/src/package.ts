
import * as path from "path"
import * as fs from "fs"

import AJV from "ajv"
import yaml from "js-yaml"
import semver from "semver"

import { FileNotFoundError, InvalidBoltfileError} from "./errors";
import { FastStringMap } from "./util";
import { SourceFile } from "./ast";
import { Boltfile } from "./boltfile"

let nextPackageId = 1;

export class Package {

  public id = nextPackageId++;

  private sourceFilesByPath = new FastStringMap<string, SourceFile>();

  constructor(
    public rootDir: string,
    public name: string | null,
    public version: string | null,
    public basePath: string = '.',
    sourceFiles: SourceFile[] = [],
  ) {
    for (const sourceFile of sourceFiles) {
      this.addSourceFile(sourceFile);
    }
  }

  public getAllSourceFiles(): IterableIterator<SourceFile> {
    return this.sourceFilesByPath.values();
  }

  public getMainLibrarySourceFile(): SourceFile | null {
    const fullPath = path.resolve(this.rootDir, this.basePath, 'lib.bolt');
    if (!this.sourceFilesByPath.has(fullPath)) {
      return null;
    }
    return this.sourceFilesByPath.get(fullPath)
  }

  public addSourceFile(sourceFile: SourceFile) {
    this.sourceFilesByPath.add(sourceFile.span!.file.fullPath, sourceFile);
  }

}

const ajv = new AJV();
const schema = require('../boltfile.schema.json');
const validate = ajv.compile(schema);

export function loadPackageMetadata(filepath: string): Boltfile {

  if (!fs.existsSync(filepath)) {
    throw new FileNotFoundError(filepath);
  }

  const data = yaml.load(fs.readFileSync(filepath, 'utf8'));

  if (!validate(data)) {
    for (const error of validate.errors!) {
      throw new InvalidBoltfileError(filepath, validate.errors);
    }
  }

  return data as Boltfile;

}
