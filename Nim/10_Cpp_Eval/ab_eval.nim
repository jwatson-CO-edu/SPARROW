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
            car : ptr Atom # Pair left
            cdr : ptr Atom # Pair right
        of STRN:
            str : string # String data
        of NMBR:
            num : float # Numeric data: All Nim floats are "double" 64bit precision
        of NULL:
            nul : byte # ??? Cannot leave this empty ???
        of EROR:
            code : F_Error # Error code
            info : string #- Detailed error info

type pt_Atom = ptr Atom

proc empty_atom*(): pt_Atom =
    # Allocate and return an a `NOVALUE` error
    result   = create Atom
    result[] = Atom( kind: EROR, code: NOVALUE, info: "No Data" )

# 2022-03-25: Compiles!

proc make_null*(): pt_Atom =
    # Make a NULL
    result   = create Atom
    result[] =  Atom( kind: NULL, nul: 0 )


proc make_cons*( car: pt_Atom, cdr: pt_Atom ): pt_Atom =
    # Make a pair
    result   = create Atom
    result[] = Atom( kind: CONS, car: car, cdr: cdr )


proc make_string*( str: string ): pt_Atom =
    # Make a string
    result   = create Atom
    result[] = Atom( kind: STRN, str: str )


proc make_number*( nmbr: float ): pt_Atom =
    # Make a number
    result   = create Atom
    result[] = Atom( kind: NMBR, num: nmbr )



########## LIST PROCESSING ########################################################################


##### Type Tests ################################

proc p_Null*( op: pt_Atom ): bool =  return (op.kind == NULL) # Return T if this atom is `Null`, Otherwise return F
proc p_cons*( op: pt_Atom ): bool =  return (op.kind == CONS) # Return T if this atom is a pair, Otherwise return F


##### Accessing & Constructing ##################

proc set_car_B*( cons: pt_Atom , valu: pt_Atom ): void =   cons.car = valu # Set the left  pair item
proc set_cdr_B*( cons: pt_Atom , valu: pt_Atom ): void =   cons.cdr = valu # Set the right pair item

proc consify_atom*( atm: pt_Atom ): pt_Atom = 
    # Wrap the `atm` in a cons, with `atm` as 'car'
    result = make_cons( atm, make_null() )

# 2022-03-25: Compiles!


proc find_terminus*( list: pt_Atom ): pt_Atom = 
    # Iterate to the ending cons of the list and return a pointer to it
    # 0. Set the argument equal to our pointer
    var curr = list
    # 1. If this is a cons structure, then we must find the end of it
    if list.kind == CONS:
        # 2. Iterate pointer to next `cdr` until we reach a pair that contains the terminating null, return pair
        while not p_Null( curr.cdr ):
            curr = addr(curr.cdr[])
        return curr
    # Else atom was literal, it is its own terminus
    else:
        return list


proc append*( list: pt_Atom, atom: pt_Atom = nil ): pt_Atom = 
    # Append an atom to the end of a conslist, Create a conslist if none exists, return pointer to list head
    var 
        rtnLst = list
        endCns: pt_Atom = nil
    #  1. If the given list is a cons list, it is either an empty cons or the head of a LISP list
    if list.kind == CONS:
        # 2. If we were given an atom to append, it either belongs in the `car` of the empty cons,
        #    or in the `car` of a new terminal cons
        if atom != nil: 
            if p_Null( list.car ):
                set_car_B( list, atom )
            else:
                endCns = find_terminus( list )
                set_cdr_B( endCns, consify_atom( atom ) )
    # 3. Else we either have one or two non-cons atoms
    else:
        rtnLst = consify_atom( list ) # -------------------------- ( `list` , [X] )
        if atom != nil:
            set_cdr_B( rtnLst, consify_atom( atom ) ) # ( `list` , ( `atom` , [X] ) )
    return rtnLst


##### Printing ##################################

proc str( item: pt_Atom ): string = 
    # Return the string representation of the `item`
    # Null Symbol: https://www.fileformat.info/info/unicode/char/29c4/index.htm
    var rtnStr = ""
    case item.kind:
    #[Null   ]# of NULL:  rtnStr = "\xE2\xA7\x84"
    #[Str/Sym]# of STRN:  rtnStr = item.str
    #[Num    ]# of NMBR:  rtnStr = $item.num
    #[Pair   ]# of CONS:  rtnStr = "( "&str(item.car)&", "&str(item.cdr)&" )"  
    #[Error  ]# of EROR:  rtnStr = "( ERROR: " & $item.code & ", " & item.info & " )"
    return rtnStr



########## PARSING ################################################################################

import std/tables
let RESERVED* = {
    "(": "open_parn", # Open  paren
    ")": "clos_parn", # Close paren
}.newTable


proc find_reserved*( token: string ): string =
    # Return the name of the reserved symbol, or an empty string if not found
    if not RESERVED.hasKey( token ):  return ""  # If the search failed, return an empty string
    else:  return RESERVED[ token ] # ------------ Else return the string name of the reserved name


# 2022-03-26: Compiles!