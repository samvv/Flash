
import test from "ava";
import { Type } from "js-yaml";
import { BoltExpressionStatement, createBoltConstantExpression, setParents } from "../ast";

import { BindingNotFoundError, TypeChecker, TypeCheckError, UnificationError, UninitializedBindingError } from "../checker"
import { createTokenStream } from "../common";
import { Parser } from "../parser";

function parseExpr(input: string) {
  const tokens = createTokenStream(input);
  const parser = new Parser()
  const node = parser.parseExpression(tokens);
  setParents(node);
  return node;
}

function parseSourceFile(input: string) {
  const tokens = createTokenStream(input);
  const parser = new Parser();
  const node = parser.parseSourceFile(tokens);
  setParents(node);
  return node;
}

test('an integer literal should resolve to the integer type', t => {
  const expr = createBoltConstantExpression(BigInt(1));
  const checker = new TypeChecker();
  const exprType = checker.getTypeOfNode(expr);
  t.assert(checker.isIntType(exprType))
});

test('a string literal should resolve to the string type', t => {
  const expr = createBoltConstantExpression('foo');
  const checker = new TypeChecker();
  const exprType = checker.getTypeOfNode(expr);
  t.assert(checker.isStringType(exprType));
})

test('an addition is strongly typed', t => {
  const expr = parseExpr('1 + 1');
  const checker = new TypeChecker();
  const exprType = checker.getTypeOfNode(expr)
  t.assert(checker.isIntType(exprType));
})

test('an application of the identity function with an integer should resolve to the integer type', t => {
  const expr = parseExpr('(|x| x)(1)')
  const checker = new TypeChecker();
  const exprType = checker.getTypeOfNode(expr);
  t.assert(checker.isIntType(exprType))
});

test('an application of an anonymous function returning an integer should resolve to the integer type', t => {
  const expr = parseExpr('(| | 1)()')
  const checker = new TypeChecker();
  const exprType = checker.getTypeOfNode(expr);
  t.assert(checker.isIntType(exprType))
});

test('an application of an anonymous function returning an integer should resolve to the integer type and ignore its arguments', t => {
  const expr = parseExpr('(|x| 1)("foo")')
  const checker = new TypeChecker();
  const exprType = checker.getTypeOfNode(expr);
  t.assert(checker.isIntType(exprType))
});

test('an application of a function using builtins should work', t => {
  const expr = parseExpr('(|x, y| x + y)(1, 2)')
  const checker = new TypeChecker();
  const exprType = checker.getTypeOfNode(expr);
  t.assert(checker.isIntType(exprType))
});

test('a reference to a variable declaration containing an integer is correctly typed', t => {
  const sourceFile = parseSourceFile(`
let a = 1;
a;
`)
  const checker = new TypeChecker();
  checker.registerSourceFile(sourceFile);
  const type = checker.getTypeOfNode((sourceFile.elements[1] as BoltExpressionStatement).expression);
  t.assert(checker.isIntType(type));
});

test('repeated uses of the same variable works without error', t => {
  const sourceFile = parseSourceFile(`
let a = 1;
a;
a + 2;
3 + a;
a + a;
`)
  const checker = new TypeChecker();
  checker.registerSourceFile(sourceFile);
  const type1 = checker.getTypeOfNode((sourceFile.elements[1] as BoltExpressionStatement).expression);
  t.assert(checker.isIntType(type1));
  const type2 = checker.getTypeOfNode((sourceFile.elements[2] as BoltExpressionStatement).expression);
  t.assert(checker.isIntType(type2));
  const type3 = checker.getTypeOfNode((sourceFile.elements[3] as BoltExpressionStatement).expression);
  t.assert(checker.isIntType(type3));
  const type4 = checker.getTypeOfNode((sourceFile.elements[4] as BoltExpressionStatement).expression);
  t.assert(checker.isIntType(type4));
});

test('the identity function is polymorphic in its parameter', t => {
  const sourceFile = parseSourceFile(`
let id = |x| x;
id(1);
id("foo");
`);
  const checker = new TypeChecker();
  checker.registerSourceFile(sourceFile);
  const type1 = checker.getTypeOfNode((sourceFile.elements[1] as BoltExpressionStatement).expression);
  t.assert(checker.isIntType(type1));
  const type2 = checker.getTypeOfNode((sourceFile.elements[2] as BoltExpressionStatement).expression);
  t.assert(checker.isStringType(type2));
});

test('a reference to a binding that does not exist is caught as an error', t => {
  const sourceFile = parseSourceFile(`x;`)
  const checker = new TypeChecker();
  const error = t.throws(() => {
    checker.registerSourceFile(sourceFile);
  }) as BindingNotFoundError;
  t.assert(error instanceof BindingNotFoundError);
  t.assert(error.varName === 'x');
})

test('a variable is correctly checked for invalid assignments', t => {
  const sourceFile = parseSourceFile(`
let mut a: int;
a = "foo";
`)
  const checker = new TypeChecker();
  const error = t.throws(() => {
    checker.registerSourceFile(sourceFile)
  }) as UnificationError;
  t.assert(error instanceof UnificationError);
  t.assert(checker.isStringType(error.left));
  t.assert(checker.isIntType(error.right));
});

test('a variable that is not declared mutable cannot be assigned', t => {
const sourceFile = parseSourceFile(`
let a;
a = 1;
`);
  const checker = new TypeChecker();
  const error = t.throws(() => {
    checker.registerSourceFile(sourceFile)
  });
  t.assert(error instanceof UninitializedBindingError);
});

test('an untyped variable will take the first assignment as its type', t => {
  const sourceFile = parseSourceFile(`
let mut a;
a = 1;
a = "foo";
`)
  const checker = new TypeChecker();
  const error = t.throws(() => {
    checker.registerSourceFile(sourceFile)
  }) as UnificationError;
  t.assert(error instanceof UnificationError);
  t.assert(checker.isStringType(error.left));
  t.assert(checker.isIntType(error.right));
});
