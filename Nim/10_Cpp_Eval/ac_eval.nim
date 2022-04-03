#[ * Text:      [L]ISP [I]n [S]mall [P]ieces by Christian Quinnec 
   * Chapter:   1
   * Project:   SPARROW, [S]cheme [P]rogram [A]llowing [R]easonable [R]eckoning [O]f [W]ork
   * Component: Micro-evaluator, written in Nim instead of Scheme 

   nim c -d:release --hints:off --run -o:exec/ac_eval -r ac_eval.nim
   
   https://github.com/jwatson-CO-edu/FINCH/blob/main/Cpp/LISP/01_eval.cpp
   James Watson, 2022-03 ]#


########## INIT ###################################################################################

import std/strutils # strip



########## ATOMS ##################################################################################

type
    F_Error = enum # https://nim-by-example.github.io/types/enums/
    # All basic error codes
        OKAY    =  0, # No error code applicable
        NOVALUE = 10, # There is no value held in this atom

type
    F_Type = enum # https://nim-by-example.github.io/types/enums/
    # This micro-language has the following types, all packaged into the every Atom
        CONS, # Cons pair
        STRN, # String/Symbol
        NMBR, # Number
        NULL, # Null
        EROR, # Error object
    Atom = ref object
    # The most basic and interchangeable unit of this LISP, Atom variants defined here
        case kind: F_Type # the `kind` field is the discriminator
        of CONS:
            car : Atom # Pair left
            cdr : Atom # Pair right
        of STRN:
            str : string # String data
        of NMBR:
            num : float # Numeric data: All Nim floats are "double" 64bit precision
        of NULL:
            nul : byte # ??? Cannot leave this empty ???
        of EROR:
            code : F_Error # Error code
            info : string #- Detailed error info


##### Printing ##################################

proc `$`( item: Atom ): string = 
    # Return the string representation of the `item`
    # Null Symbol: https://www.fileformat.info/info/unicode/char/29c4/index.htm
    case item.kind:
    #[Null   ]# of NULL:  result = "\xE2\xA7\x84"
    #[Str/Sym]# of STRN:  result = "\"" & item.str & "\""
    #[Num    ]# of NMBR:  result = $item.num
    #[Pair   ]# of CONS:  result = "(" & $item.car & ", " & $item.cdr & ")"  
    #[Error  ]# of EROR:  result = "(ERROR: " & $item.code & ", Code: " & item.info & ")"


##### Constructors ###############################

proc empty_atom*(): Atom =
    # Allocate and return an a `NOVALUE` error
    return Atom( kind: EROR, code: NOVALUE, info: "No Data" )

echo $empty_atom()


proc make_null*(): Atom =
    # Make a NULL
    return Atom( kind: NULL, nul: 0 )

echo $make_null()


proc make_cons*( car: Atom = nil, cdr: Atom = nil ): Atom =
    # Make a pair
    var op1, op2: Atom
    if isNil( car ):
        op1 = make_null()
    else:
        op1 = car
    if isNil( cdr ):
        op2 = make_null()
    else:
        op2 = cdr
    return Atom( kind: CONS, car: op1, cdr: op2 )

echo $make_cons()


proc make_string*( str: string ): Atom =
    # Make a string
    return Atom( kind: STRN, str: str )

echo $make_string( "Hello World!" )


proc make_number*( nmbr: float ): Atom =
    # Make a number
    return Atom( kind: NMBR, num: nmbr )

echo $make_number( 42.0 )



########## LIST PROCESSING ########################################################################


##### Type Tests ################################

proc p_Null*( op: Atom ): bool =  return (op.kind == NULL) # Return T if this atom is `Null`, Otherwise return F
proc p_cons*( op: Atom ): bool =  return (op.kind == CONS) # Return T if this atom is a pair, Otherwise return F

var A00, A01: Atom
A00 = make_null()
A01 = make_cons()
echo "Is ", $A00, " a Null?: ", p_Null( A00 )
echo "Is ", $A00, " a cons?: ", p_cons( A00 )
echo "Is ", $A01, " a Null?: ", p_Null( A01 )
echo "Is ", $A01, " a cons?: ", p_cons( A01 )


##### Accessing & Constructing ##################
proc set_car_B*( cons: Atom , valu: Atom ): void =   cons.car = valu # Set the left  pair item
proc set_cdr_B*( cons: Atom , valu: Atom ): void =   cons.cdr = valu # Set the right pair item

echo "Set car and cdr of ", $A01
set_car_B( A01, make_string( "The Answer" ) )
set_cdr_B( A01, make_number( 42 ) )
echo $A01

proc consify_atom*( atm: Atom ): Atom = 
    # Wrap the `atm` in a cons, with `atm` as 'car'
    return make_cons( atm, make_null() )

echo "consify_atom: ", $consify_atom( make_number(2) )


proc find_terminus*( list: Atom ): Atom = 
    # Iterate to the ending cons of the list and return a pointer to it
    # 0. Set the argument equal to our pointer
    var curr = list
    # 1. If this is a cons structure, then we must find the end of it
    if list.kind == CONS:
        # 2. Iterate pointer to next `cdr` until we reach a pair that contains the terminating null, return pair
        while not p_Null( curr.cdr ):
            curr = curr.cdr
        return curr
    # Else atom was literal, it is its own terminus
    else:
        return list

var A02: Atom = make_cons( make_number(1), make_cons( make_number(2), make_cons( make_number(3) ) ) )
echo "What is the terminus of", $A02, "? --> ", $find_terminus( A02 )

# 2022-04-02: All tests pass!