#[ * Text:      [L]ISP [I]n [S]mall [P]ieces by Christian Quinnec 
   * Chapter:   1
   * Project:   SPARROW, [S]cheme [P]rogram [A]llowing [R]easonable [R]eckoning [O]f [W]ork
   * Component: Micro-evaluator, written in C++ instead of Scheme 

   nim c -d:release --hints:off --run -o:exec/ab_eval -r ab_eval.nim
   
   https://github.com/jwatson-CO-edu/FINCH/blob/main/Cpp/LISP/01_eval.cpp
   James Watson, 2022-03 ]#


########## HELPER FUNCTIONS #######################################################################

# string str_to_upper( const string& inputStr )
# https://nim-lang.org/docs/strutils.html#capitalizeAscii%2Cstring



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



proc empty_atom*(): Atom =
    # Allocate and return an a `NOVALUE` error
    return Atom( kind: EROR, code: NOVALUE, info: "Atom was created without data" )


# FIXME: Atom* make_null(){ , 

