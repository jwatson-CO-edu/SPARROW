#[
    Lesson AD: Procedures
    https://nim-lang.org/docs/tut1.html#procedures
    nim c -d:release --hints:off --run -o:exec/ad_proc -r ad_proc.nim
]#

##### Procedure Definition #####
proc yes( question:string ): bool =
    echo question, " (y/n)"
    while true:
        case readLine( stdin )
            of "y", "Y", "yes", "YES", "Yes":
                return true
            of "n", "N", "no", "NO", "No":
                return false
            else:
                echo "Please be clear: yes or no"

if yes("Should I delete all your important files?"):
    echo "I'm sorry Dave, I'm afraid I can't do that."
else:
    echo "I think you know what the problem is just as well as I do."


# Return Variable #
#[ A procedure that returns a value has an implicit `result` variable declared 
  that represents the return value. A `return` statement with no expression 
  is shorthand for `return result`. The result value is always returned automatically 
  at the end of a procedure if there is no return statement at the exit. ]#
proc sumTillNegative( x:varargs[int] ): int = 
    for num in x:
        if num < 0:
            return
        result += num

echo sumTillNegative() # ------------ echoes  0
echo sumTillNegative(3, 4, 5) # ----- echoes 12
echo sumTillNegative(3, 4 , -1 , 6) # echoes  7
#[ The result variable is already implicitly declared at the start of the function, 
   so declaring it again with 'var result', for example, would shadow it 
   with a normal variable of the same name. The result variable is also already initialized 
   with the type's default value. 
   
   A procedure that does not have any return statement and does not use the special result variable 
   returns the value of its last expression. ]#
proc helloWorld(): string =
    "Hello, World!"

echo helloWorld()



##### Parameters #####
#[ Parameters are immutable in the procedure body. If a mutable variable is needed 
   inside the procedure, it has to be declared with var in the procedure body. ]#
proc printSeq( s:seq, nPrinted:int = -1 ) = # void return type
    # shadowing the parameter name is possible
    var nPrinted = if nPrinted == -1: s.len else: min( nPrinted, s.len ) # Ternary
    for i in 0 ..< nPrinted:
        echo s[i]

printSeq( 
    @[1,2,3,4], # Sequence literal
    3 # --------- prints @[1,2,3] only w/ each elem on separate lines
)

# Reference Parameter #
#[ In the example, `res` and `remainder` are `var` parameters. 
   Var parameters can be modified by the procedure and the changes are visible to the caller. ]#
proc divmod( a,b:int; res,remainder: var int ) = 
    res       = a div b # integer division
    remainder = a mod b # integer modulo operation

var x,y:int
divmod(8, 5, x, y) # modifies x and y
echo x, " ", y


# Discard Statement #
#[ To call a procedure that returns a value just for its side effects and ignoring its return value, 
   a `discard` statement *must* be used. Nim does *not* allow silently throwing away a return value ]#
discard yes("May I ask a pointless question?")

#[ The return value can be ignored implicitly if the called proc/iterator 
  has been declared with the `discardable` pragma ]#
proc p( x,y:int ): int {.discardable.} =
    return x + y
p(3,4) # No error, return value vanishes


##### Named Arguments #####
proc createWindow1( x, y, width, height: int; title: string; show: bool ): int =
    echo title
    return 1

var w = createWindow1( 0, 0, title = "My Application", height = 600, width = 800, true )


##### Default Argument Values #####
proc createWindow2(x = 0, y = 0, width = 500, height = 700, title = "unknown", show = true): int =
    echo title
    return 1

var a = createWindow2( 0, 0, title = "Your Application" )


##### Procedure Overloading, and Ternary Operator #####
proc toString(x: int): string =
  result =
    if x < 0: "negative"
    elif x > 0: "positive"
    else: "zero"

proc toString(x: bool): string =
  result =
    if x: "yep"
    else: "nope"

assert toString(13) == "positive" # calls the toString(x: int) proc
assert toString(true) == "yep"    # calls the toString(x: bool) proc


##### Operator Overloading #####
# proc `$` (x: myDataType): string = ...
# now the $ operator also works with myDataType, overloading resolution
# ensures that $ works for built-in types just like before

## Call operator w/ backtick ##
if `==`( `+`(3, 4), 7): echo "true"


##### Forward Declaration #####
proc even(n: int): bool # Signature only

proc odd(n: int): bool =
  assert(n >= 0) # makes sure we don't run into negative recursion
  if n == 0: false
  else:
    n == 1 or even(n-1)

proc even(n: int): bool =
  assert(n >= 0) # makes sure we don't run into negative recursion
  if n == 1: false
  else:
    n == 0 or odd(n-1)


##### Iterators #####
echo "Counting to ten: "
for i in countup(1, 10):
    echo i

### `yield` ###
iterator count_alt(a, b: int): int =
    var res = a
    while res <= b:
        yield res
        inc( res )

for i in count_alt(1, 10):
    echo i