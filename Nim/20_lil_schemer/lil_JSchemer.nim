#[ * Text:      [L]ISP [I]n [S]mall [P]ieces by Christian Quinnec 
   * Chapter:   1
   * Project:   SPARROW, [S]cheme [P]rogram [A]llowing [R]easonable [R]eckoning [O]f [W]ork
   * Component: Micro-evaluator, written in Nim instead of Scheme 

   nim c -d:release --hints:off --run -o:exec/lil_JSchemer -r lil_JSchemer.nim
   
   https://github.com/jwatson-CO-edu/FINCH/blob/main/JS/JS_Scheme_f_rename.js
   James Watson, 2022-04 ]#


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


proc make_null*(): Atom =
    # Make a NULL
    return Atom( kind: NULL, nul: 0 )


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


proc make_string*( str: string ): Atom =
    # Make a string
    return Atom( kind: STRN, str: str )


proc make_number*( nmbr: float ): Atom =
    # Make a number
    return Atom( kind: NMBR, num: nmbr )



########## LIST PROCESSING ########################################################################


##### Type Tests ################################

proc p_Null*( op: Atom ): bool =  return (op.kind == NULL) # Return T if this atom is `Null`, Otherwise return F
proc p_cons*( op: Atom ): bool =  return (op.kind == CONS) # Return T if this atom is a pair, Otherwise return F


##### Accessing & Constructing ##################
proc get_car*( cons: Atom ): Atom =  return cons.car # --------------- Get the left  pair item
proc get_cdr*( cons: Atom ): Atom =  return cons.car # --------------- Get the right pair item
proc set_car_B*( cons: Atom , valu: Atom ): void =   cons.car = valu # Set the left  pair item
proc set_cdr_B*( cons: Atom , valu: Atom ): void =   cons.cdr = valu # Set the right pair item
# FIXME: START HERE, rename: `build_cons`
# function build(s1, s2){ return cons(s1, cons(s2, null)); } // return a two-item list with 's1' as the first item and 's2' as the second item


proc consify_atom*( atm: Atom ): Atom = 
    # Wrap the `atm` in a cons, with `atm` as 'car'
    return make_cons( atm, make_null() )


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

# 2022-04-02: All tests pass!

proc append*( list: var Atom, atom: Atom = nil ): Atom {.discardable.} = 
    # Append an atom to the end of a conslist, Create a conslist if none exists, return pointer to list head
    result = list
    var endCns: Atom
    #  1. If the given list is a cons list, it is either an empty cons or the head of a LISP list
    if list.kind == CONS:
        # 2. If we were given an atom to append, it either belongs in the `car` of the empty cons,
        #    or in the `car` of a new terminal cons
        if atom != nil: 
            if p_Null( list.car ):
                list = make_cons( atom, make_null() )
            else:
                endCns = find_terminus( list )
                set_cdr_B( endCns, consify_atom( atom ) )
    # 3. Else we either have one or two non-cons atoms
    else:
        result = consify_atom( list ) # -------------------------- ( `list` , [X] )
        if atom != nil:
            set_cdr_B( result, consify_atom( atom ) ) # ( `list` , ( `atom` , [X] ) )



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


proc tokenize*( expStr: var string, sepChar: string = " "  ): seq[string] = 
    # Parse an expression string into an s-expression
    let 
        expLen = expStr.len() # --- Length of the given string
    var 
        token  = "" # -------------- Current token in progress
        c:char = ' ' # ------------- Current character
        tokens: seq[string] = @[] # Vector of string tokens

    proc stow_token(): void =
        # LAMBDA: Add the current token to the vector and reset current token to empty
        tokens.add( token )
        token = ""

    proc stow_char(): void = 
        # LAMBDA: Add the current character to the vector and reset token to empty
        tokens.add( $c )

    proc cache_char(): void = 
        # LAMBDA: Add the current character to the current token
        token.add( c )

    # 0. Apply the postfix hack
    expStr.add( ' ' )
    # 1. For every character in the string
    for i in 0 .. (expLen-1):
        # 2. Fetch character
        c = expStr[i]
        # 3. Either add char to present token or create a new one
        
        # Case Open Paren
        if find_reserved( $c ) == "open_parn":  stow_char()
        # Case Close Paren
        elif find_reserved( $c ) == "clos_parn":
            if token.len() > 0:  stow_token()
            stow_char()
        # Case separator
        elif $c == sepChar:  stow_token()
        # Case any other char
        else:  cache_char()
    # N. Return the vector of tokens
    return tokens


proc p_float_string*( inputStr: string ): bool = 
    # Return T if the string is appropriate for float conversion, otherwise return F
    # Adapted from Original Author: Bill the Lizard,  https://stackoverflow.com/a/447307
    var slimStr = inputStr.strip()
    try:
        discard parseFloat( slimStr )
        return true
    except ValueError:
        return false


proc p_null_string*( inputStr: string ): bool =
    # Return T if the string is appropriate for Null conversion, otherwise return F
    return (toUpper( inputStr ) == "NULL") # 2022-03-28: For now, do not consider empty string null


proc atomize_string*( token: string ): Atom =
    # Convert a string into a non-cons atom
    if p_float_string( token ):  return make_number( parseFloat( token ) )
    if p_null_string( token ) :  return make_null()
   #[ else assume string -----]# return make_string( token )


proc consify_tokens*( tokens: seq[string], i: var int ): Atom =
    # Render tokens as a cons structure
    var
        token:   string # --------------- Current token from the vector
        tkLen:   int     = tokens.len() # Number of tokens in the given vector
        rtnTree: Atom  # -------- Cons structure to return

    #  1. If there are one or more strings to process, then attempt to construct a token tree
    if (tokens.len()-i) > 1:
        #  2. Start off by creating a cons list, if needed
        rtnTree = make_cons()

        #  3. For each token in the vector
        while i < tkLen:
            #  4. Fetch token at this index
            token = tokens[i]
            i += 1

            #  5. Case Open Paren
            if find_reserved( token ) == "open_parn":
                # If there is a new level of depth to the structure, recur
                if i>1:  append(  rtnTree , consify_tokens( tokens, i )  )
                # else this is the first level
                else:    rtnTree = consify_tokens( tokens, i )
            else:
                #  6. Case Close Paren
                if find_reserved( token ) == "clos_parn":  return rtnTree
                #  7. Case Null
                elif p_null_string( token ):  append( rtnTree , make_null() )
                #  8. Case Literal
                else:  
                    append( rtnTree , atomize_string( token ) )

        #  9. Return the constructed tree
        return rtnTree
    # 10. else there were no tokens, return Null
    else:  return make_null()


proc expression_from_string*( expStr: var string, sepChar: string = " " ): Atom =
    # Tokenize the `expStr`, express it as a conslist, and return
    var
        i:      int         = 0
        tokens: seq[string] = tokenize( expStr, sepChar )
    return consify_tokens( tokens, i )



########## ENVIRONMENT ############################################################################

type
    Env* = ref object
        parent:    Env # --------------- Pointer to the environment that contains this one
        freeVars:  seq[Atom] # --------- Free  variables, without binding
        boundVars: Table[string, Atom] # Bound variables, have names given to them by statements


proc newEnv(): Env =
    # Create a new, empty environment
    new(result)
    result.parent    = nil
    result.freeVars  = @[]
    result.boundVars = initTable[string, Atom]()


proc p_binding_exists*( env: Env, name: string ): bool =
    # Return T if the binding exists in `boundVars`, otherwise return F
    return env.boundVars.hasKey( name )


proc get_bound_atom*( env: Env, name: string ): Atom =
    #  Return the atom bound to `name`, if it exists, Otherwise return `nil`
    if p_binding_exists( env, name ):
        return env.boundVars[ name ]
    return nil


proc bind_atom*( env: Env, name: string, atom: Atom ): void =
    # Bind an `atom` to a `name` by adding it to the mapping, If the name already exists, it will be updated
    env.boundVars[ name ] = atom


# 2022-04-02: All tests pass!

