#[
    Lesson AC: Control Flow
    https://nim-lang.org/docs/tut1.html
    nim c -d:release --hints:off --run -o:exec/ac_ctrl_flow -r ac_ctrl_flow.nim
]#

from std/strutils import parseInt  # Import parsing string to integer from string utils lib

# Read text from standard input and solidify into variable
let name = readLine( stdin )

# If-Elif-Else #
echo "Enter Name 1"
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
var # FIXME START HERE