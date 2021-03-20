
import test from "ava";

import { BindPattern, ExpressionStatement, setParents } from "../ast";
import { BindingNotFoundError, UnificationError, UninitializedBindingError } from "../errors";
import { createTokenStream } from "../common";
import { Parser } from "../parser";
import { Program } from "../program";
import { Package } from "../package";
import {boolType, intType, stringType, Type, TypeKind} from "../types";

function getTypeOfExpr(input: string) {
  const sourceFile = loadSourceFile(input + ';');
  return (sourceFile.elements[0] as ExpressionStatement).expression.getType();
}

function isIntType(type: Type) {
  return type.kind === TypeKind.PrimType
      && type.primId === intType.primId;
}

function isStringType(type: Type) {
  return type.kind === TypeKind.PrimType
      && type.primId === stringType.primId
}

function isBoolType(type: Type) {
  return type.kind === TypeKind.PrimType
      && type.primId === boolType.primId
}

function loadSourceFile(input: string) {
  const tokens = createTokenStream(input);
  const parser = new Parser();
  const pkg = new Package('.', null, null, undefined);
  const sourceFile = parser.parseSourceFile(tokens, pkg);
  pkg.addSourceFile(sourceFile);
  setParents(sourceFile);
  const program = new Program([ pkg ], pkg);
  program.check();
  return sourceFile;
}

test('an integer literal should resolve to the integer type', t => {
  const exprType = getTypeOfExpr('1');
  t.assert(isIntType(exprType))
});

test('a string literal should resolve to the string type', t => {
  const exprType = getTypeOfExpr('"foo"');
  t.assert(isStringType(exprType));
})

test('an addition is strongly typed', t => {
  const exprType = getTypeOfExpr('1 + 1');
  t.assert(isIntType(exprType));
})

test('an application of the identity function with an integer should resolve to the integer type', t => {
  const exprType = getTypeOfExpr('(|x| x)(1)');
  t.assert(isIntType(exprType))
});

test('an application of an anonymous function returning an integer should resolve to the integer type', t => {
  const exprType = getTypeOfExpr('(| | 1)()')
  t.assert(isIntType(exprType))
});

test('an application of an anonymous function returning an integer should resolve to the integer type and ignore its arguments', t => {
  const exprType = getTypeOfExpr('(|x| 1)("foo")')
  t.assert(isIntType(exprType))
});

test('an application of a function using builtins should work', t => {
  const exprType = getTypeOfExpr('(|x, y| x + y)(1, 2)')
  t.assert(isIntType(exprType))
});

test('a reference to a variable declaration containing an integer is correctly typed', t => {
  const sourceFile = loadSourceFile(`
let a = 1;
a;
`)
  const type = (sourceFile.elements[1] as ExpressionStatement).expression.getType();
  t.assert(isIntType(type));
});

test('repeated uses of the same variable works without error', t => {
  const sourceFile = loadSourceFile(`
let a = 1;
a;
a + 2;
3 + a;
a + a;
`)
  const type1 = (sourceFile.elements[1] as ExpressionStatement).expression.getType();
  t.assert(isIntType(type1));
  const type2 = (sourceFile.elements[2] as ExpressionStatement).expression.getType();
  t.assert(isIntType(type2));
  const type3 = (sourceFile.elements[3] as ExpressionStatement).expression.getType();
  t.assert(isIntType(type3));
  const type4 = (sourceFile.elements[4] as ExpressionStatement).expression.getType();
  t.assert(isIntType(type4));
});

test('the identity function is polymorphic in its parameter', t => {
  const sourceFile = loadSourceFile(`
let id = |x| x;
id(1);
id("foo");
`);
  const type1 = (sourceFile.elements[1] as ExpressionStatement).expression.getType();
  t.assert(isIntType(type1));
  const type2 = (sourceFile.elements[2] as ExpressionStatement).expression.getType();
  t.assert(isStringType(type2));
});

test('a reference to a binding that does not exist is caught as an error', t => {
  const error = t.throws(() => { getTypeOfExpr(`x;`) }) as BindingNotFoundError;
  t.assert(error instanceof BindingNotFoundError);
  t.assert(error.name === 'x');
})

test('a variable is correctly checked for invalid assignments', t => {
  const error = t.throws(() => {
    loadSourceFile(`
let mut a: Int;
a = "foo";
`);
  }) as UnificationError;
  t.assert(error instanceof UnificationError);
  t.assert(isStringType(error.right));
  t.assert(isIntType(error.left));
});

test('a variable may be reassigned different times with a value of the same type', t => {
  const sourceFile = loadSourceFile(`
let mut a;
a = "foo";
a = "bar";
a = "baz";
`)
  const exprType = sourceFile.elements[0].getType();
  t.assert(isStringType(exprType));
});

test('a variable that is not declared mutable cannot be assigned', t => {
const error = t.throws(() => {
  loadSourceFile(`
let a;
a = 1;
`);
    }) as UninitializedBindingError;
  t.assert(error instanceof UninitializedBindingError);
  t.assert((error.node.bindings as BindPattern).name.text === 'a');
});

test('an untyped variable will take the first assignment as its type', t => {
  const error = t.throws(() => {
  loadSourceFile(`
let mut a;
a = 1;
a = "foo";
`)
  }) as UnificationError;
  t.assert(error instanceof UnificationError);
  t.assert(isStringType(error.right));
  t.assert(isIntType(error.left));
});

test('a recursive function calculating the factorial can be defined', t => {
  const sourceFile = loadSourceFile(`
fn fac(n: Int) -> Int {
  return match n {
    0 => 1,
    n => n * fac(n-1),
  }
}

fac(1);
`)
  const type = (sourceFile.elements[1] as ExpressionStatement).expression.getType();
  t.assert(isIntType(type));
})

test('a plain variable can only be used after it has been declared', t => {
  const error = t.throws(() => {
    loadSourceFile(`
let a = b;
let b = 1;
`)
  }) as BindingNotFoundError;
  t.assert(error instanceof BindingNotFoundError);
  t.assert(error.name === 'b');
});

test('an untyped expression will derive its return type from the function body', t => {
  const sourceFile = loadSourceFile(`
fn bar(i) {
  return i;
}
fn foo(i) {
  return bar(i + 1);
}
foo(1);
`);
  const exprType = (sourceFile.elements[2] as ExpressionStatement).expression.getType();
  t.assert(isIntType(exprType));
});


test('two fully typed functions that are mutually recursive can be defined', t => {
  const sourceFile = loadSourceFile(`
fn a(i: Int) -> Int {
  return b(i / 2);
}
fn b(i: Int) -> Int {
  return a(i + 1);
}
a(1);
`)
  const exprType = (sourceFile.elements[2] as ExpressionStatement).expression.getType();
  t.assert(isIntType(exprType));

  const error2 = t.throws(() => {
    loadSourceFile(`
fn a(i: Int) -> Int {
  return b("foo");
}
fn b(i: Int) -> Int {
  return a(i + 1);
}
a(1);
`);
  }) as UnificationError;
  t.assert(error2 instanceof UnificationError);

  const error3 = t.throws(() => {
    loadSourceFile(`
fn a(i: Int) -> Int {
  return b(i / 2);
}
fn b(i: Int) -> Int {
  return a("foo");
}
a(1);
`)
  }) as UnificationError;
  t.assert(error3 instanceof UnificationError);
});

test('two untyped functions that are mutually recursive can be defined', t => {
  const sourceFile = loadSourceFile(`
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
  const exprType1 = (sourceFile.elements[2] as ExpressionStatement).expression.getType();
  t.assert(isBoolType(exprType1));
  const exprType2 = (sourceFile.elements[3] as ExpressionStatement).expression.getType();
  t.assert(isBoolType(exprType2));
});

test('access to a record field is correctly typed', t => {
  const sourceFile = loadSourceFile(`
struct Foo {
  a: String,
  b: Int,
}
let foo = Foo { a: "Hello, world!", b: 1 }
foo.a;
foo.b;
`)
  const aType = (sourceFile.elements[2] as ExpressionStatement).expression.getType();
  t.assert(isStringType(aType));
  const bType = (sourceFile.elements[3] as ExpressionStatement).expression.getType();
  t.assert(isIntType(bType));
});

test('constructing a record with a wrongly typed field fails', t => {
  const error1 = t.throws(() => loadSourceFile(`
struct Foo {
  a: String,
  b: Int,
}
let foo = Foo { a: 42, b: 1 }
foo.a;
foo.b;
`)) as UnificationError;
  t.assert(error1 instanceof UnificationError);
  t.assert(isStringType(error1.left));
  t.assert(isIntType(error1.right));

  const error2 = t.throws(() => loadSourceFile(`
struct Foo {
  a: String,
  b: Int,
}
let foo = Foo { a: "Some text", b: "Hello, world!" }
foo.a;
foo.b;
`)) as UnificationError;
  t.assert(error2 instanceof UnificationError);
  t.assert(isIntType(error2.left));
  t.assert(isStringType(error2.right));
});

//test('an instance of a record can be overloaded with any matching function', t => {
//  const sourceFile = loadSourceFile(`
//struct Wrapper {
//  value: Int,
//}
//fn get(wrapper: Wrapper) {
//  return wrapper.value;
//}
//let foo = Wrapper { value: 1 }
//foo.get();
//`)
//  t.assert(isIntType((sourceFile.elements[3] as ExpressionStatement).expression.getType()));
//});

