#!/usr/bin/env node

import "reflect-metadata"
import "source-map-support/register"

import path from "path";

import yargs from "yargs"

import { Frontend, expandPath } from "@boltlang/compiler"
import { CompileError } from "@boltlang/compiler/lib/diagnostics";

process.on('uncaughtException', error => {
  if (error instanceof CompileError) {
    error.print();
    if (error.severity === 'fatal' || error.severity === 'error') {
      process.exit(1);
    }
  } else {
    throw error;
  }
})

const BOLT_HOME = expandPath(process.env['BOLT_HOME'] ?? '~/.bolt-compiler');

function toArray<T>(value: T | T[]): T[] {
  if (Array.isArray(value)) {
    return value;
  }
  return value === null || value === undefined ? [] : [value]
}

function error(message: string) {
  console.error(`Error: ${message}`);
}

yargs

  .command(

    'link [name]',
    'Link projects with each other',

    yargs => yargs,

    args => {

      console.log(args.name)

    }

  )

  .command(
    'check [files..]',
    'Check the given files/packages for mistakes.',
    yargs => yargs
      .string('work-dir')
      .describe('work-dir', 'The working directory where files will be resolved against.')
      .default('work-dir', '.')
      .boolean('no-std')
      .describe('no-std', 'Do not build using the standard library.'),
    args => {
      const useStd = args['std'] as boolean ?? true;
      const cwd = path.resolve(args['work-dir']);
      const files = toArray(args.files as string[] | string);
      const frontend = new Frontend();
      const program = frontend.loadProgramFromFileList(files, cwd, useStd);
      if (!frontend.diagnostics.hasFatal && program !== null) {
        frontend.check(program);
      }
      if (frontend.diagnostics.hasErrors || frontend.diagnostics.hasFatal) {
        process.exit(1);
      }
    }
  )

  .command(

    'bundle [files..]',
    'Compile and optimise a set of Bolt packages/scripts', 

    yargs => yargs
      .string('work-dir')
      .describe('work-dir', 'The working directory where files will be resolved against.')
      .default('work-dir', '.')
      .string('target')
      .describe('target', 'The target language to compile to.')
      .default('target', 'JS')
      .boolean('force')
      .describe('force', 'Ignore as much errors as possible.')
      .default('force', false)

    , args => {

      const force = args.force as boolean;
      const useStd = args['std'] as boolean ?? true;
      const cwd = process.cwd();
      const files = toArray(args.files as string[] | string);

      const frontend = new Frontend();

      parsePackageResolverFlags(frontend, toArray(args.pkg as string | string[]));

      const program = frontend.loadProgramFromFileList(files, cwd, useStd);

      if (program === null) {
        process.exit(force ? 0 : 1);
      }

      frontend.check(program);
      if (frontend.diagnostics.hasErrors && !force) {
        process.exit(1);
      }
      frontend.compile(program, args.target);

    })

  .command(

    'exec [files..]',
    'Run the specified Bolt packages/scripts',

    yargs => yargs
      .string('work-dir')
      .describe('work-dir', 'The working directory where files will be resolved against.')
      .default('work-dir', '.')
      .boolean('skip-type-checks')
      .describe('skip-type-checks', 'Do not check the program for common mistakes before evaluating.')
      .default('skip-type-checks', true)
      .boolean('force')
      .describe('force', 'Ignore as much errors as possible.')
      .default('force', false)

    , args => {

      const runTypeChecker = !(args["skip-type-checks"] as boolean);
      const force = args.force as boolean;
      const useStd = args['std'] as boolean ?? true;
      const cwd = process.cwd();
      const files = toArray(args.files as string[] | string);

      const frontend = new Frontend();

      parsePackageResolverFlags(frontend, toArray(args.pkg as string | string[]));

      const program = frontend.loadProgramFromFileList(files, cwd, useStd);

      if (program === null && !force) {
        process.exit(1);
      }

      if (program !== null) {
        if (runTypeChecker) {
          frontend.check(program);
        }
        if (frontend.diagnostics.hasErrors && !force) {
          process.exit(1);
        }
        frontend.eval(program);
      }


    }

  )

  .demandCommand()
  .argv

