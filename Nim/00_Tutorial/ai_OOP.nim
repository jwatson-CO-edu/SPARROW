#[
    Lesson AI: Object-Oriented Programming
    https://nim-lang.org/docs/tut2.html#object-oriented-programming
    nim c -d:release --hints:off --run -o:exec/ai_OOP -r ai_OOP.nim
]#

#[### Inheritance ##### 
    Inheritance in Nim is entirely optional. To enable inheritance with runtime type information the object needs to inherit from `RootObj`.

    Usually types with inheritance are also marked as ref types even though this isn't strictly enforced. 
To check at runtime if an object is of a certain type, the of operator can be used.

    Inheritance is done with the `object of` syntax. Multiple inheritance is currently not supported. 
If an object type has no suitable ancestor, `RootObj` can be used as its ancestor, but this is only a convention. 
Objects that have no ancestor are implicitly final. You can use the inheritable pragma to introduce new object roots apart from system.RootObj.

    Ref objects should be used whenever inheritance is used. It isn't strictly necessary, but with non-ref objects, 
assignments such as `let person: Person = Student(id: 123)` will truncate subclass fields.

    Note: Composition (has-a relation) is often preferable to inheritance (is-a relation) for simple code reuse. 
Since objects are value types in Nim, composition is as efficient as inheritance.
]#

type
  Person = ref object of RootObj
    name*: string  # the * means that `name` is accessible from other modules
    age: int       # no * means that the field is hidden from other modules
  
  Student = ref object of Person # Student inherits from Person
    id: int                      # with an id field

var
  student: Student
  person: Person
assert(student of Student) # is always true
# object construction:
person  = Person(name: "Gerbort", age: 500)
student = Student(name: "Anton", age: 5, id: 2)
echo person[]
echo student[]

#[# Mutually Recursive Types ###
    In Nim these types can only be declared within a single type section. 
    (Anything else would require arbitrary symbol lookahead which slows down compilation.)  ]#

type
  Node = ref object  # a reference to an object with the following field:
    le, ri: Node     # left and right subtrees
    sym: ref Sym     # leaves contain a reference to a Sym
  
  Sym = object       # a symbol
    name: string     # the symbol's name
    line: int        # the line the symbol was declared in
    code: Node       # the symbol's abstract syntax tree


#[# Type Conversions ### 
    Nim distinguishes between type casts and type conversions. Casts are done with the `cast` operator and force the compiler 
to interpret a bit pattern to be of another type.
    Type conversions are a much more polite way to convert a type into another: They preserve the abstract value, 
not necessarily the bit-pattern. If a type conversion is not possible, the compiler complains or an exception is raised.
    The syntax for type conversions is `<destination_type>(<expression_to_convert>)`  ]#

proc getID(x: Person): int = # The `InvalidObjectConversionDefect` exception is raised if `x` is not a Student.
  Student(x).id


#[### Object Variants #####
    Often an object hierarchy is overkill in certain situations where simple variant types are needed.  
    
    As can been seen from the example below, an advantage to an object hierarchy is that 
no conversion between different object types is needed. Yet, access to invalid object fields raises an exception.  ]#

# This is an example how an abstract syntax tree could be modelled in Nim
type
  NodeKind = enum  # different node types
    nkInt,         # leaf with an integer value
    nkFloat,       # leaf with a float value
    nkString,      # leaf with a string value
    nkAdd,         # an addition
    nkSub,         # a subtraction
    nkIf           # an if statement
  AST_Node = ref object
    case kind: NodeKind  # the `kind` field is the discriminator
    of nkInt: intVal: int
    of nkFloat: floatVal: float
    of nkString: strVal: string
    of nkAdd, nkSub:
      leftOp, rightOp: AST_Node
    of nkIf:
      condition, thenPart, elsePart: AST_Node

var n = AST_Node(kind: nkFloat, floatVal: 1.0)
# the following statement raises an `FieldDefect` exception, because
# n.kind's value does not fit:
# n.strVal = ""


#[### Member Method Call Syntax #####
    There is a syntactic sugar for calling routines: The syntax `obj.methodName(args)` can be used instead of `methodName(obj, args)`. 
If there are no remaining arguments, the parentheses can be omitted: obj.len (instead of len(obj)).

    This method call syntax is not restricted to objects, it can be used for any type:
]#

import std/[strutils, sequtils]

echo "abc".len # is the same as echo len("abc")
echo "abc".toUpperAscii()
echo({'a', 'b', 'c'}.card)
stdout.writeLine("Hallo") # the same as writeLine(stdout, "Hallo")

stdout.writeLine("Give a list of numbers (separated by spaces): ")
stdout.write(stdin.readLine.splitWhitespace.map(parseInt).max.`$`)
stdout.writeLine(" is the maximum!")


#[### Getters and Setters #####
    Ordinary get-procedures that are called with the method are as you would expect. 
But setting a value is different; for this a special setter syntax is needed:  ]#

type
  Socket* = ref object of RootObj
    h: int # cannot be accessed from the outside of the module due to missing star

proc `host=`*(s: var Socket, value: int) {.inline.} =
  ## setter of host address
  s.h = value

proc host*(s: Socket): int {.inline.} =
  ## getter of host address
  s.h

var s: Socket
new s
`host=`(s, 34)


# The [] array access operator can be overloaded to provide array properties:

type
  Vector* = object
    x, y, z: float

proc `[]=`* (v: var Vector, i: int, value: float) =
  # setter
  case i
  of 0: v.x = value
  of 1: v.y = value
  of 2: v.z = value
  else: assert(false)

proc `[]`* (v: Vector, i: int): float =
  # getter
  case i
  of 0: result = v.x
  of 1: result = v.y
  of 2: result = v.z
  else: assert(false)


#[### Dynamic Dispatch #####
    Dynamic dispatch is the process of selecting which implementation of a polymorphic operation (method or function) 
to call at run time. It is commonly employed in, and considered a prime characteristic of, object-oriented programming (OOP) languages 
and systems.  Polymorphism is the phenomenon wherein somewhat interchangeable objects each expose an operation of the same name,
but possibly differing in behavior. 
    Dynamic dispatch contrasts with static dispatch, in which the implementation of a polymorphic operation is selected at compile time. 
The purpose of dynamic dispatch is to defer the selection of an appropriate implementation until the run time type of a parameter 
(or multiple parameters) is known.
    A language may be implemented with different dynamic dispatch mechanisms. The choices of the dynamic dispatch mechanism offered 
by a language to a large extent alter the programming paradigms that are available or are most natural to use within a given language.
Normally, in a typed language, the dispatch mechanism will be performed based on the type of the arguments 
(most commonly based on the type of the receiver of a message). Languages with weak or no typing systems often carry a dispatch table as part 
of the object data for each object. This allows instance behaviour as each instance may map a given message to a separate method.  Some 
languages offer a hybrid approach.  Dynamic dispatch will always incur an overhead so some languages offer static dispatch for 
particular methods.
    Procedures always use static dispatch. For dynamic dispatch replace the `proc` keyword by `method`:
]#

type
  Expression = ref object of RootObj ## abstract base class for an expression
  Literal = ref object of Expression
    x: int
  PlusExpr = ref object of Expression
    a, b: Expression

# watch out: 'eval' relies on dynamic binding
method eval(e: Expression): int {.base.} =
  # override this base method
  quit "to override!"

method eval(e: Literal): int = e.x
method eval(e: PlusExpr): int = eval(e.a) + eval(e.b)

proc newLit(x: int): Literal = Literal(x: x)
proc newPlus(a, b: Expression): PlusExpr = PlusExpr(a: a, b: b)

echo eval(newPlus(newPlus(newLit(1), newLit(2)), newLit(4)))
#[ Note that in the example the constructors newLit and newPlus are procs because it makes more sense for them to use static binding, 
but eval is a method because it requires dynamic binding. ]#


#[### Multiple Dispatch #####
    Multiple dispatch or multimethods is a feature of some programming languages in which a function or method can be dynamically dispatched 
based on the run-time (dynamic) type or, in the more general case, some other attribute of more than one of its arguments.  
This is a generalization of single-dispatch polymorphism where a function or method call is dynamically dispatched based on the derived type 
of the object on which the method has been called. Multiple dispatch routes the dynamic dispatch to the implementing function or method 
using the combined characteristics of one or more arguments.

    In a multi-method all parameters that have an object type are used for the dispatching.  As the example below demonstrates, 
invocation of a multi-method cannot be ambiguous:  ]#

type
  Thing = ref object of RootObj
  Unit = ref object of Thing
    x: int

#[ Error: ambiguous call; both ai_OOP.collide(a: Thing, b: Unit) [method declared in /home/jwatson/za_Other/FINCH/Nim/ai_OOP.nim(222, 8)] 
and ai_OOP.collide(a: Unit, b: Thing) [method declared in /home/jwatson/za_Other/FINCH/Nim/ai_OOP.nim(225, 8)] match for: (Unit, Unit) ]#

method collide(a, b: Thing) {.inline.} =
  quit "to override!"

# method collide(a: Thing, b: Unit) {.inline.} = # ERROR
#   echo "1"

method collide(a: Unit, b: Thing) {.inline.} =
  echo "2"

var a, b: Unit
new a
new b
collide(a, b) # output: 2

#[ Performance note: Nim does not produce a virtual method table, but generates dispatch trees. 
This avoids the expensive indirect branch for method calls and enables inlining. 
However, other optimizations like compile time evaluation or dead code elimination do not work with methods. ]#



