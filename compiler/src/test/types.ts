
import test from "ava";
import { BoltExpressionStatement, createBoltConstantExpression, setParents } from "../ast";

import { TypeChecker } from "../checker"
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

test('repeated uses of the same variable does not change its type', t => {
  const sourceFile = parseSourceFile(`
let a = 1;
a;
a + a;
`)
  const checker = new TypeChecker();
  checker.registerSourceFile(sourceFile);
  const type1 = checker.getTypeOfNode((sourceFile.elements[1] as BoltExpressionStatement).expression);
  t.assert(checker.isIntType(type1));
  const type2 = checker.getTypeOfNode((sourceFile.elements[2] as BoltExpressionStatement).expression);
  t.assert(checker.isIntType(type2));
});
