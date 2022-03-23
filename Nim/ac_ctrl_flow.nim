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
        echo "Hi, ", name, "!"

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
