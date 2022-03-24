#[
    Lesson AJ: Generics
    https://nim-lang.org/docs/tut1.html
    nim c -d:release --hints:off --run -o:exec/aj_generics -r aj_generics.nim
]#

#[### Generics #####
    Generic programming is a style of computer programming in which algorithms are written in terms of types to-be-specified-later 
that are then instantiated when needed for specific types provided as parameters. This approach permits writing common functions or types 
that differ only in the set of types on which they operate when used, thus reducing duplication.
    Generics are Nim's means to parametrize procs, iterators or types with type parameters. Generic parameters are written within 
square brackets, for example Foo[T]. 

They are most useful for efficient type safe containers:  ]#

type
  BinaryTree*[T] = ref object # BinaryTree is a generic type with
                              # generic param `T`
    le, ri: BinaryTree[T]     # left and right subtrees; may be nil
    data: T                   # the data stored in a node

proc newNode*[T](data: T): BinaryTree[T] =
  # constructor for a node
  new(result)
  result.data = data

proc add*[T](root: var BinaryTree[T], n: BinaryTree[T]) =
  # insert a node into the tree
  if root == nil:
    root = n
  else:
    var it = root
    while it != nil:
      # compare the data items; uses the generic `cmp` proc
      # that works for any type that has a `==` and `<` operator
      var c = cmp(it.data, n.data)
      if c < 0:
        if it.le == nil:
          it.le = n
          return
        it = it.le
      else:
        if it.ri == nil:
          it.ri = n
          return
        it = it.ri

proc add*[T](root: var BinaryTree[T], data: T) =
  # convenience proc:
  add(root, newNode(data))

iterator preorder*[T](root: BinaryTree[T]): T =
  # Preorder traversal of a binary tree.
  # This uses an explicit stack (which is more efficient than
  # a recursive iterator factory).
  var stack: seq[BinaryTree[T]] = @[root]
  while stack.len > 0:
    var n = stack.pop()
    while n != nil:
      yield n.data
      add(stack, n.ri)  # push right subtree onto the stack
      n = n.le          # and follow the left pointer

var
  root: BinaryTree[string] # instantiate a BinaryTree with `string`
add(root, newNode("hello")) # instantiates `newNode` and `add`
add(root, "world")          # instantiates the second `add` proc
for str in preorder(root):
  stdout.writeLine(str)

proc foo[T](i: T) =
  discard

var i: int

# i.foo[int]() # Error: expression 'foo(i)' has no type (or is ambiguous)

i.foo[:int]() # Success



#[### Templates #####
    Templates are a simple substitution mechanism that operates on Nim's abstract syntax trees. 
Templates are processed in the semantic pass of the compiler. To invoke a template, call it like a procedure.
    The !=, >, >=, in, notin, isnot operators are in fact templates: this has the benefit that if you overload the == operator, 
the != operator is available automatically and does the right thing. (Except for IEEE floating point numbers - NaN breaks basic boolean logic.)
    The parameters' types can be ordinary types or the meta types untyped, typed, or type. type suggests that only a type symbol may be given 
as an argument, and untyped means symbol lookups and type resolution is not performed before the expression is passed to the template.
    If the template has no explicit return type, void is used for consistency with procs and methods.

Example:  ]#

template `!=` (a, b: untyped): untyped =
  # this definition exists in the System module
  not (a == b)

assert(5 != 6) # the compiler rewrites that to: assert(not (5 == 6))


# To pass a block of statements to a template, use untyped for the last parameter:

template withFile(f: untyped, filename: string, mode: FileMode,
                  body: untyped) =
  let fn = filename
  var f: File
  if open(f, fn, mode):
    try:
      body
    finally:
      close(f)
  else:
    quit("cannot open: " & fn)

withFile(txt, "ttempl3.txt", fmWrite):
  txt.writeLine("line 1")
  txt.writeLine("line 2")