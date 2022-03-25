#[
    Lesson AC: Control Flow
    https://nim-lang.org/docs/tut1.html
    nim c -d:release --hints:off --run -o:exec/ac_ctrl_flow -r ac_ctrl_flow.nim
]#

from std/strutils import parseInt  # Import parsing string to integer from string utils lib

# Read text from standard input and solidify into variable
echo "Enter Name 1"
let name = readLine( stdin )

# If-Elif-Else #
if name == "":
    echo "Poor soul, you lost your name?"
elif name == "name":
    echo "Very funny, your name is `name`"
else:
    echo "Hi, ", name, "!"

## Case ##
echo "Enter Name 2"
let nam2 = readLine( stdin )
case nam2
    of "":
        echo "Poor soul, you lost your name?"
    of "name":
        echo "Very funny, your name is `name`"
    of "James", "Krystal": # You can list multiple accepted values to activate the same case
        echo "Cool name!"
    else: # Default Case
        echo "Hi, ", nam2, "!"

# Case with Number Ranges #
echo "Enter an integer"
let n = parseInt( readLine( stdin ) )
case n # When doing a case with number var, all possible values must be covered by cases
    of 0..2, 4..7: # Integer ranges
        echo "The number is in the set: {0, 1, 2, 4, 5, 6, 7}"
    of 3, 8:
        echo "The number is 3 or 8"
    else:
        discard # pass, NO-OP


## While Loop ##
echo "Enter any string but a blank one:"
var a = readLine( stdin )
while a == "":
    echo "I'm sorry, but I asked for a string..."
    a = readLine( stdin )


## For Loop ##
# With `countup` #
echo "`countup` is `range`"
for i in countup( 1, 10): # Unlike range
    stdout.write( i )
    stdout.write( ", " )
echo ""
# With range operator `..` #
for i in 1..10:
    stdout.write( i )
    stdout.write( ", " )
echo ""
# Exactly like `range` #
for i in 0 ..< 10:
    stdout.write( i )
    stdout.write( ", " )
echo ""
# String Index Iteration #
var s = "some string"
for i in 0 ..< s.len:
    stdout.write( i )
    stdout.write( ", " )
echo ""

# Enumerate #
for i, c in s[0 .. ^1]:
    stdout.write( i )
    stdout.write( ":" )
    stdout.write( c )
    stdout.write( ", " )
echo ""


## Blocks ##

# Declare block
block myBlock:  # Block label is optional
    var x = "hi"
    echo x
# echo x  # ERROR: Out of scope

# Break #
block blok1:
    echo "Entering block ..."
    while true:
        echo "\tLooping ..."
        break  # Leaves the loop, but not the block
    echo "... Still in the block"
echo "Outside of the block"

block bloc2:
    echo "Entering block ..."
    while true:
        echo "\tLooping ..."
        break bloc2 # Leaves the loop, but not the block
    echo "... Still in the block"  # Not printed!
echo "Outside of the block"


## Continue ##
for i in 1..5:
    if i <= 3:  
        continue
    echo i # Only prints when i > 3


## When ##
# + Good for platform-specific code
#[ The `when` statement is almost identical to the `if` statement, but with these differences:
    * Each condition must be a constant expression since it is evaluated by the compiler.
    * The statements within a branch do not open a new scope.
    * The compiler checks the semantics and produces code only for 
      the statements that belong to the first condition that evaluates to true. ]#
when system.hostOS == "windows":
  echo "running on Windows!"
elif system.hostOS == "linux":
  echo "running on Linux!"
elif system.hostOS == "macosx":
  echo "running on Mac OS X!"
else:
  echo "unknown operating system"



#[### Exceptions #####
    In Nim exceptions are objects. By convention, exception types are suffixed with 'Error'. The system module defines an exception hierarchy 
that you might want to stick to. Exceptions derive from `system.Exception`, which provides the common interface.
    Exceptions have to be allocated on the heap because their lifetime is unknown. The compiler will prevent you from raising an exception 
created on the stack. All raised exceptions should at least specify the reason for being raised in the msg field.
    A convention is that exceptions should be raised in exceptional cases, they should not be used as an alternative method of control flow.

Raising an exception is done with the raise statement: ]#

var
  e: ref OSError
new(e)
e.msg = "the request to the OS failed"
# raise e # Error: unhandled exception: the request to the OS failed [OSError]

#[ If the raise keyword is not followed by an expression, the last exception is re-raised. For the purpose of avoiding repeating this 
common code pattern, the template newException in the system module can be used: ]#
# raise newException(OSError, "the request to the OS failed")  # Error: unhandled exception: the request to the OS failed [OSError]


#[# Try-Except ### 
    The try statement handles exceptions.
    The exception is consumed in an except part. If an exception is not handled, it is propagated through the call stack. 
This means that often the rest of the procedure - that is not within a finally clause - is not executed (if an exception occurs). ]#

from std/strutils import parseInt

# read the first two lines of a text file that should contain numbers
# and tries to add them
var
  f: File
if open(f, "numbers.txt"):
  try:
    let a = readLine(f)
    let b = readLine(f)
    echo "sum: ", parseInt(a) + parseInt(b)
  except OverflowDefect:
    echo "overflow!"
  except ValueError:
    echo "could not convert string to integer"
  except IOError:
    echo "IO error!"
  except: #[ The empty except part is executed if there is an exception that is not explicitly listed. 
             It is similar to an else part in if statements. ]# 
    echo "Unknown exception!"
    # reraise the unknown exception:
    raise
  finally: # If there is a finally part, it is always executed after the exception handlers.
    close(f)

#[ If you need to access the actual exception object or message inside an except branch you can use the 
`getCurrentException()` and `getCurrentExceptionMsg()` procs from the system module. Example: ]#

try:
    let foo = 6/0
    echo foo
except:
  let
    e = getCurrentException()
    msg = getCurrentExceptionMsg()
  echo "Got exception ", repr(e), " with message ", msg