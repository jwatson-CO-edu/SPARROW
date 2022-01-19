#[
    Lesson AB: Variables
    https://nim-lang.org/docs/tut1.html
    nim c -d:release --hints:off --run -o:exec/ab_vars -r ab_vars.nim
]#

#[
  This is a multi-
  line comment.
]#

# One var of one type
var q: float

# Many vars of one type
var x, y: int # declares x and y to have the type ``int``

# Many vars of many types
var
  z, h: int
  # a comment can occur here too
  a, b, c: string


## Constants: Evaluated at compile time, Protected from change at compile time ##

# Single Constant
const e = "abc" # Note that the type was inferred here

# Multiple Constant
const 
  f = 1 # Note that the type was inferred here
  g = 2
  i = 3


## Single-Assignment Variables: Evaluated at run time, Protected from change at compile time ##
let j = "xyz" # Note that the type was inferred here
# j = "abc" # ERROR, Protected from change at compile time