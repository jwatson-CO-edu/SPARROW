#[
    Lesson AK: Macros
    https://nim-lang.org/docs/tut3.html
    nim c -d:release --hints:off --run -o:exec/ak_macros -r ak_macros.nim
]#

#[### Macros #####
    A macro is a function that is executed at compile-time and transforms a Nim syntax tree into a different tree.
    A piece of good advice is to use them as little as possible, but as much as necessary. Macros can change the semantics of expressions, 
making the code incomprehensible for anybody who does not know exactly what the macro does with it. So whenever a macro is not necessary 
and the same logic can be implemented using templates or generics, it is probably better not to use a macro. And when a macro is used for something, 
the macro should better have a well-written documentation.
    Since macros are evaluated in the compiler in the NimVM, macros share all the limitations of the NimVM. They have to be implemented 
in pure Nim code. Macros can start external processes on the shell, but they cannot call C functions except those that are built in the compiler.

### Arguments ###
    The types of macro arguments have two faces. One face is used for the overload resolution 
and the other face is used within the macro body. For example, if macro `foo(arg: int)` is called in an expression `foo(x)`, 
`x` has to be of a type compatible to `int`, but within the macro's body `arg` has the type `NimNode`, not `int`! 
    There are two ways to pass arguments to a macro, an argument can be either `typed` or `untyped`.

## Untyped Arguments ##
    Untyped macro arguments are passed to the macro before they are semantically checked. This means the syntax tree that is passed down 
to the macro does not need to make sense for Nim yet, the only limitation is that it needs to be parsable. Usually, the macro does not 
check the argument either but uses it in the transformation's result somehow. The result of a macro expansion is always checked by the compiler, 
so apart from weird error messages, nothing bad can happen.
    The downside for an untyped argument is that these do not play well with Nim's overloading resolution.
    The upside for untyped arguments is that the syntax tree is quite predictable and less complex compared to its typed counterpart.

## Typed Arguments ##
    For typed arguments, the semantic checker runs on the argument and does transformations on it, before it is passed to the macro. 
Here identifier nodes are resolved as symbols, implicit type conversions are visible in the tree as calls, templates are expanded, 
and probably most importantly, nodes have type information. Typed arguments can have the type typed in the arguments list. 
But all other types, such as `int`, `float` or `MyObjectType` are typed arguments as well, and they are passed to the macro as a syntax tree.

## Static Arguments ##
    Static arguments are a way to pass values as values and not as syntax tree nodes to a macro. For example for macro 
`foo(arg: static[int])` in the expression `foo(x)`, `x` needs to be an integer constant, but in the macro body arg is just like a normal parameter 
of type `int`.  ]#

import std/macros

macro myMacro(arg: static[int]): untyped =
  echo arg # just an int (7), not `NimNode`

myMacro(1 + 2 * 3)


#[ Code Block Arguments ##
    It is possible to pass the last argument of a call expression in a separate code block with indentation.  This way of calling is very useful; 
syntax trees of arbitrary complexity can be passed to macros with this notation. For example, 
the following code example is a valid (but not a recommended) way to call `echo`: ]#

echo "Hello ":
  let a = "Wor"
  let b = "ld!"
  a & b

#[### The Syntax Tree #####
    In order to build a Nim syntax tree one needs to know how Nim source code is represented as a syntax tree, and how such a tree needs to 
look like so that the Nim compiler will understand it. The nodes of the Nim syntax tree are documented in the `macros` module. But a more 
interactive way to explore the Nim syntax tree is with `macros.treeRepr`, it converts a syntax tree into a multi-line string for printing 
on the console. It can be used to explore how the argument expressions are represented in tree form and for debug printing of generated syntax tree. 
`dumpTree` is a predefined macro that just prints its argument in a tree representation, but does nothing else. 

Here is an example of such a tree representation: ]#

dumpTree:
  var mt: MyType = MyType(a:123.456, b:"abcdef")

# output:
#   StmtList
#     VarSection
#       IdentDefs
#         Ident "mt"
#         Ident "MyType"
#         ObjConstr
#           Ident "MyType"
#           ExprColonExpr
#             Ident "a"
#             FloatLit 123.456
#           ExprColonExpr
#             Ident "b"
#             StrLit "abcdef"


#[### Custom Semantic Checking #####
    The first thing that a macro should do with its arguments is to check if the argument is in the correct form. Not every type of wrong input 
needs to be caught here, but anything that could cause a crash during macro evaluation should be caught and create a nice error message. 
`macros.expectKind` and `macros.expectLen` are a good start. If the checks need to be more complex, arbitrary error messages can be created 
with the `macros.error` proc.  ]#

macro myAssert(arg: untyped): untyped =
  arg.expectKind nnkInfix


#[### Generating Code #####
    There are two ways to generate the code. Either by creating the syntax tree with expressions that contain a lot of calls to 
`newTree` and `newLit`, or with `quote do:` expressions. The first option offers the best low-level control for the syntax tree generation, 
but the second option is much less verbose. If you choose to create the syntax tree with calls to `newTree` and `newLit` the macro 
`macros.dumpAstGen` can help you with the verbosity.
    `quote do:` allows you to write the code that you want to generate literally. Backticks are used to insert code from `NimNode` symbols 
into the generated expression. The injected symbol needs accent quoted when it resolves to a symbol. 
    Make sure to inject only symbols of type `NimNode` into the generated syntax tree. You can use `newLit` to convert arbitrary values 
into expressions trees of type NimNode so that it is safe to inject them into the tree.  ]#

type
  MyType = object
    a: float
    b: string

macro myMacro(arg: untyped): untyped =
  var mt: MyType = MyType(a:123.456, b:"abcdef")
  
  # ...
  
  let mtLit = newLit(mt)
  
  result = quote do:
    echo `arg`
    echo `mtLit`

myMacro("Hallo") #[ The call to myMacro will generate the following code:

echo "Hallo"
echo MyType(a: 123.456'f64, b: "abcdef")  ]#


#[### Your First Macro #####
    To give a starting point to writing macros we will show now how to implement the `myDebug` macro. The first thing to do is to 
build a simple example of the macro usage, and then just print the argument. This way it is possible to get an idea of what a correct argument 
should look like. 
    To debug what the macro actually generated, the statement echo result.repr can be used, in the last line of the macro. ]#

# import std/macros

macro myAssert2(arg: untyped): untyped =
  echo arg.treeRepr

let c = 1
let d = 2

myAssert2(c != d)
#[ Infix
    Ident "!="
    Ident "c"
    Ident "d" 
    
From the output, it is possible to see that the argument is an infix operator (node kind is "Infix"), 
as well as that the two operands are at index 1 and 2. With this information, the actual macro can be written.  ]#

# import std/macros

macro myAssert3(arg: untyped): untyped =
  # all node kind identifiers are prefixed with "nnk"
  arg.expectKind nnkInfix
  arg.expectLen 3
  # operator as string literal
  let op  = newLit(" " & arg[0].repr & " ")
  let lhs = arg[1]
  let rhs = arg[2]
  
  result = quote do:
    if not `arg`:
      raise newException(AssertionDefect,$`lhs` & `op` & $`rhs`)

let e = 1
let f = 2

myAssert3(e != f)
# myAssert3(e == f) # Error: unhandled exception: 1 == 2 [AssertionDefect]