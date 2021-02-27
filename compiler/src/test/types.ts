
import test from "ava";
import { BoltExpressionStatement, setParents } from "../ast";

import { BindingNotFoundError, TypeChecker, UnificationError, UninitializedBindingError } from "../checker"
import { createTokenStream } from "../common";
import { Parser } from "../parser";

function getTypeOfExpr(input: string) {
  const sourceFile = parseSourceFile(input + ';');
  const checker = new TypeChecker();
  checker.registerSourceFile(sourceFile);
  return checker.getTypeOfNode((sourceFile.elements[0] as BoltExpressionStatement).expression);
}

function parseSourceFile(input: string) {
  const tokens = createTokenStream(input);
  const parser = new Parser();
  const node = parser.parseSourceFile(tokens);
  setParents(node);
  return node;
}

test('an integer literal should resolve to the integer type', t => {
  const sourceFile = parseSourceFile('1;');
  const checker = new TypeChecker();
  checker.registerSourceFile(sourceFile);
  const exprType = checker.getTypeOfNode((sourceFile.elements[0] as BoltExpressionStatement).expression);
  t.assert(checker.isIntType(exprType))
});

test('a string literal should resolve to the string type', t => {
  const sourceFile = parseSourceFile('"foo"');
  const checker = new TypeChecker();
  checker.registerSourceFile(sourceFile);
  const exprType = checker.getTypeOfNode((sourceFile.elements[0] as BoltExpressionStatement).expression);
  t.assert(checker.isStringType(exprType));
})

test('an addition is strongly typed', t => {
  const sourceFile = parseSourceFile('1 + 1;');
  const checker = new TypeChecker();
  checker.registerSourceFile(sourceFile);
  const exprType = checker.getTypeOfNode((sourceFile.elements[0] as BoltExpressionStatement).expression);
  t.assert(checker.isIntType(exprType));
})

test('an application of the identity function with an integer should resolve to the integer type', t => {
  const sourceFile = parseSourceFile('(|x| x)(1);');
  const checker = new TypeChecker();
  checker.registerSourceFile(sourceFile);
  const exprType = checker.getTypeOfNode((sourceFile.elements[0] as BoltExpressionStatement).expression);
  t.assert(checker.isIntType(exprType))
});

test('an application of an anonymous function returning an integer should resolve to the integer type', t => {
  const sourceFile = parseSourceFile('(| | 1)()')
  const checker = new TypeChecker();
  checker.registerSourceFile(sourceFile);
  const exprType = checker.getTypeOfNode((sourceFile.elements[0] as BoltExpressionStatement).expression);
  t.assert(checker.isIntType(exprType))
});

test('an application of an anonymous function returning an integer should resolve to the integer type and ignore its arguments', t => {
  const sourceFile = parseSourceFile('(|x| 1)("foo")')
  const checker = new TypeChecker();
  checker.registerSourceFile(sourceFile);
  const exprType = checker.getTypeOfNode((sourceFile.elements[0] as BoltExpressionStatement).expression);
  t.assert(checker.isIntType(exprType))
});

test('an application of a function using builtins should work', t => {
  const sourceFile = parseSourceFile('(|x, y| x + y)(1, 2)')
  const checker = new TypeChecker();
  checker.registerSourceFile(sourceFile);
  const exprType = checker.getTypeOfNode((sourceFile.elements[0] as BoltExpressionStatement).expression);
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
  t.assert(error.name === 'x');
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


test('a variable may be reassigned different times with a value of the same type', t => {
  const sourceFile = parseSourceFile(`
let mut a;
a = "foo";
a = "bar";
a = "baz";
`)
  const checker = new TypeChecker();
  checker.registerSourceFile(sourceFile)
  const exprType = checker.getTypeOfNode(sourceFile.elements[0]);
  t.assert(checker.isStringType(exprType));
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

test('a recursive function calculating the factorial can be defined', t => {
  const sourceFile = parseSourceFile(`
fn fac(n: int) -> int {
  return match n {
    0 => 1,
    n => n * fac(n-1),
  }
}

fac(1);
`)
  const checker = new TypeChecker();
  checker.registerSourceFile(sourceFile);
  const type = checker.getTypeOfNode((sourceFile.elements[1] as BoltExpressionStatement).expression);
  t.assert(checker.isIntType(type));
})

test('a plain variable can only be used after it has been declared', t => {
  const sourceFile = parseSourceFile(`
let a = b;
let b = 1;
`)
  const checker = new TypeChecker();
  const error = t.throws(() => {
    checker.registerSourceFile(sourceFile)
  }) as BindingNotFoundError;
  t.assert(error instanceof BindingNotFoundError);
  t.assert(error.name === 'b');
});

test('an untyped expression will derive its return type from the function body', t => {
  const sourceFile = parseSourceFile(`
fn bar(i) {
  return i;
}
fn foo(i) {
  return bar(i + 1);
}
foo(1);
`);
  const checker = new TypeChecker();
  checker.registerSourceFile(sourceFile);
  const exprType = checker.getTypeOfNode((sourceFile.elements[2] as BoltExpressionStatement).expression);
  t.assert(checker.isIntType(exprType));
});


test('two fully typed functions that are mutually recursive can be defined', t => {
  const sourceFile = parseSourceFile(`
fn a(i: int) -> int {
  return b(i / 2);
}
fn b(i: int) -> int {
  return a(i + 1);
}
a(1);
`)
  const checker = new TypeChecker();
  checker.registerSourceFile(sourceFile);
  const exprType = checker.getTypeOfNode((sourceFile.elements[2] as BoltExpressionStatement).expression);
  t.assert(checker.isIntType(exprType));

  const sourceFile2 = parseSourceFile(`
fn a(i: int) -> int {
  return b("foo");
}
fn b(i: int) -> int {
  return a(i + 1);
}
a(1);
`)
  const checker2 = new TypeChecker();
  const error2 = t.throws(() => {
    checker2.registerSourceFile(sourceFile2);
  });
  t.assert(error2 instanceof UnificationError);

  const sourceFile3 = parseSourceFile(`
fn a(i: int) -> int {
  return b(i / 2);
}
fn b(i: int) -> int {
  return a("foo");
}
a(1);
`)
  const checker3 = new TypeChecker();
  const error3 = t.throws(() => {
    checker3.registerSourceFile(sourceFile3);
  })
  t.assert(error3 instanceof UnificationError);
});

test('two untyped functions that are mutually recursive can be defined', t => {
  const sourceFile = parseSourceFile(`
fn is_even(i) {
  if (i == 0) {
    return true;
  } else {
    return is_odd(i-1);
  }
}
fn is_odd(i) {
  if i == 1 {
    return true;
  } else {
    return is_even(i-1);
  }
}
is_even(1);
is_odd(2);
`)
  const checker = new TypeChecker();
  checker.registerSourceFile(sourceFile);
  const exprType1 = checker.getTypeOfNode((sourceFile.elements[2] as BoltExpressionStatement).expression);
  t.assert(checker.isBoolType(exprType1));
  const exprType2 = checker.getTypeOfNode((sourceFile.elements[3] as BoltExpressionStatement).expression);
  t.assert(checker.isBoolType(exprType2));
});
