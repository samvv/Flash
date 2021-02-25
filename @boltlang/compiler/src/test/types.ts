
import test from "ava";
import { createBoltConstantExpression, createBoltIdentifier, createBoltQualName, createBoltReferenceExpression } from "../ast";

import { TypeChecker } from "../checker"
import { createTokenStream } from "../common";
import { Parser } from "../parser";

function parse(input: string, filePath = '#<anonymous>') {
  const tokens = createTokenStream(input);
  const parser = new Parser()
  return parser.parseExpression(tokens);
}

function createSimpleBoltReferenceExpression(name: string) {
  return createBoltReferenceExpression(
    createBoltQualName(false, [], createBoltIdentifier(name)),
  )
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
  const expr = parse('1 + 1');
  const checker = new TypeChecker();
  const exprType = checker.getTypeOfNode(expr)
  t.assert(checker.isIntType(exprType));
})

test('an application of the identity function with an integer should resolve to the integer type', t => {
  const expr = parse('(|x| x)(1)')
  const checker = new TypeChecker();
  const exprType = checker.getTypeOfNode(expr);
  t.assert(checker.isIntType(exprType))
});

test('an application of an anonymous function returning an integer should resolve to the integer type', t => {
  const expr = parse('(| | 1)()')
  const checker = new TypeChecker();
  const exprType = checker.getTypeOfNode(expr);
  t.assert(checker.isIntType(exprType))
});

test('an application of an anonymous function returning an integer should resolve to the integer type and ignore its arguments', t => {
  const expr = parse('(|x| 1)("foo")')
  const checker = new TypeChecker();
  const exprType = checker.getTypeOfNode(expr);
  t.assert(checker.isIntType(exprType))
});

test('an application of a function using builtins should work', t => {
  const expr = parse('(|x, y| x + y)(1, 2)')
  const checker = new TypeChecker();
  const exprType = checker.getTypeOfNode(expr);
  t.assert(checker.isIntType(exprType))
});