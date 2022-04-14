#[ * Text:      [L]ISP [I]n [S]mall [P]ieces by Christian Quinnec 
   * Chapter:   1
   * Project:   SPARROW, [S]cheme [P]rogram [A]llowing [R]easonable [R]eckoning [O]f [W]ork
   * Component: Micro-evaluator, written in Nim instead of Scheme 

   nim c -d:release --hints:off --run -o:exec/lil_JSchemer -r lil_JSchemer.nim
   
   https://github.com/jwatson-CO-edu/FINCH/blob/main/JS/JS_Scheme_f_rename.js
   James Watson, 2022-04 ]#


########## INIT ###################################################################################

import std/strutils # strip
import std/tables # - hashes
import std/math # --- classify



########## ATOMS ##################################################################################

type
    F_Error = enum # https://nim-by-example.github.io/types/enums/
    # All basic error codes
        OKAY    =  0, # No error code applicable
        NOVALUE = 10, # There is no value held in this atom
        UNBOUND = 20, # Requested symbol is unbound, or a binding could not be made

type
    F_Type = enum # https://nim-by-example.github.io/types/enums/
    # This micro-language has the following types, all packaged into the every Atom
        CONS, # Cons pair
        STRN, # String/Symbol
        NMBR, # Number
        BOOL, # Boolean
        NULL, # Null
        EROR, # Error object
        FUNC, # Function
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
        of BOOL:
            bul : bool  # True or False
        of NULL:
            nul : byte # ??? Cannot leave this empty ???
        of EROR:
            code : F_Error # Error code
            info : string #- Detailed error info
        of FUNC:
            name: string # --------------------- Function name
            args: OrderedTable[string, F_Type] # Input  arguments and types
            rtns: OrderedTable[string, F_Type] # Output returns   and types
            sorc: Atom # ----------------------- Actual code of the function


##### Printing ##################################

proc `$`( item: Atom ): string = 
    # Return the string representation of the `item`
    # Null Symbol: https://www.fileformat.info/info/unicode/char/29c4/index.htm
    case item.kind:
    #[Null   ]# of NULL:  result = "\xE2\xA7\x84"
    #[Str/Sym]# of STRN:  result = "\"" & item.str & "\""
    #[Num    ]# of NMBR:  result = $item.num
    #[Bool   ]# of BOOL:  result = $item.bul
    #[Pair   ]# of CONS:  result = "(" & $item.car & ", " & $item.cdr & ")"  
    #[Error  ]# of EROR:  result = "(ERROR: " & $item.code & ", Code: " & item.info & ")"
    #[Func   ]# of FUNC:  result = "(" & $item.name & ")"  # FIXME: LIST ALL KEYS INPUT # FIXME: LIST ALL KEYS OUTPUT



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


proc make_bool*( buul: bool ): Atom =
    # Make a boolean
    return Atom( kind: BOOL, bul: buul )


proc make_error*( errCode: F_Error, errInfo: string ): Atom =
    # Allocate and return an error
    return Atom( kind: EROR, code: errCode, info: errInfo )
        

proc make_function*( fName: string, fArgs: OrderedTable[string, F_Type], fReturnVals: OrderedTable[string, F_Type], source: Atom ): Atom =
    # Allocate and return a function
    return Atom( kind: FUNC, name: fName, args: fArgs, rtns: fReturnVals, sorc: source  )
    

proc empty_function*( funcName: string ): Atom =
    # Return a Function in Name Only
    result = Atom( kind: FUNC ) # Partial instantiation: https://forum.nim-lang.org/t/9093#59260
    result.name = funcName

########## LIST PROCESSING ########################################################################


##### Type Tests ################################

proc p_Null*( op: Atom ): bool =  return (op.kind == NULL) # Return T if this atom is `Null`, Otherwise return F
proc p_cons*( op: Atom ): bool =  return (op.kind == CONS) # Return T if this atom is a pair, Otherwise return F


##### Accessing & Constructing ##################
proc get_car*( cons: Atom ): Atom =  return cons.car # --------------- Get the left  pair item
proc get_cdr*( cons: Atom ): Atom =  return cons.cdr # --------------- Get the right pair item
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
        parent:    Env # --------------- Smart pointer to the environment that contains this one
        freeVars:  seq[Atom] # --------- Free  variables, without binding
        boundVars: Table[string, Atom] # Bound variables, have names given to them by statements


proc newEnv*(): Env =
    # Create a new, empty environment
    new(result)
    result.parent    = nil
    result.freeVars  = @[]
    result.boundVars = initTable[string, Atom]()


proc p_binding_exists*( env: Env, name: string ): bool =
    # Return T if the binding exists in `boundVars`, otherwise return F
    return env.boundVars.hasKey( name )


proc p_binding_missing*( env: Env, name: string ): bool =
    # Return F if the binding exists in `boundVars`, otherwise return T
    return not p_binding_exists( env, name )


proc get_bound_atom*( env: Env, name: string ): Atom =
    #  Return the atom bound to `name`, if it exists, Otherwise return `nil`
    if p_binding_exists( env, name ):
        return env.boundVars[ name ]
    return nil


proc bind_atom*( env: Env, name: string, atom: Atom ): void =
    # Bind an `atom` to a `name` by adding it to the mapping, If the name already exists, it will be updated
    env.boundVars[ name ] = atom



########## LITTLE JAVASCRIPTER #####################################################################

##### Construction #####
proc make_list_of_2*( op1: Atom, op2: Atom ): Atom =
    # return a two-item list with 's1' as the first item and 's2' as the second item
    return make_cons( op1, make_cons( op2, make_null() ) )
    
# TEST #
var A00: Atom = make_list_of_2( make_number(1), make_number(2) ) 
append( A00, make_number(3) )
echo A00

##### Access #####
proc second*( l: Atom ): Atom =  return get_car(get_cdr(l)) # -------- Return the second item in a list, (cadr l)
proc third*( l: Atom ): Atom  =  return get_car(get_cdr(get_cdr(l))) # return the third item of 'l', (caddr l)

# TEST #
echo second( A00 )
echo third( A00 )

##### Predicates #####
proc p_literal*( a: Atom ): bool =  return (a.kind == STRN) or (a.kind == NMBR) or (a.kind == BOOL) # 'a' is any of String, Number, or Boolean

# TEST #
var A01: Atom = make_number(42)
echo p_literal( A01 )

proc p_eq*( s: Atom, t: Atom ): bool =
    if s.kind != t.kind: # If not the same kind, then don't bother checking values
        return false
    case s.kind: # - Otherwise, there is more work to do ...
    of NULL:  return true # - Two Nulls are always the same
    of STRN:  return s.str == t.str # - Rely on Nim string comparison
    of NMBR:  return s.num == t.num # - Numbers must be exactly the same
    of BOOL:  return s.bul == t.bul # - Boolean values must be the same
    of EROR:  return (s.code == t.code) # -------------------------- Don't care if the message is the same
    of CONS:  return p_eq( s.car, t.car ) and p_eq( s.cdr, t.cdr ) # Recursively determine if a structure is equal
    of FUNC:  return false # FIXME: COMPARE TABLES

# TEST #
var
    Astr1 = make_string("foo")
    Astr2 = make_string("foo")
    Anum1 = make_number( 2 )
    Anum2 = make_number( 3 )
    Abul1 = make_bool( true )
    Abul2 = make_bool( true )
    Aerr1 = make_error( NOVALUE, "This message does not matter" )
    Aerr2 = make_error( NOVALUE, "Neither does this one"        )
    Acns1 = make_cons( make_cons( make_number( 1 ), make_number( 2 ) ), make_cons( make_number( 3 ), make_number( 4 ) ) )
    Acns2 = make_cons( make_cons( make_number( 1 ), make_number( 2 ) ), make_cons( make_number( 3 ), make_number( 4 ) ) )
    Acns3 = make_cons( make_cons( make_number( 1 ), make_number( 2 ) ), make_cons( make_number( 3 ), make_number( 5 ) ) )
echo p_eq( Astr1, Astr2 ) , ' ' , p_eq( Astr1, Anum2 ) , ' ' , p_eq( Anum1, Anum2 ) , ' ' , p_eq( Abul1, Abul2 )
echo p_eq( Aerr1, Aerr2 ) , ' ' , p_eq( Acns1, Acns2 ) , ' ' , p_eq( Acns1, Acns3 ) , ' ' , p_eq( Acns2, Acns3 )

# 2022-04-06: All tests pass!


proc p_number*( a: Atom ): bool =
    # Return true only if the value held by `a` is a `NMBR`, is not infinite, and is not NaN
    # Based on work by Alex Craft, https://stackoverflow.com/a/66063769
    if a.kind != NMBR:
        return false
    else:
        let ntype = a.num.classify()
        return (ntype == fc_normal) or (ntype == fc_zero) or (ntype == fc_neg_zero)

# TEST #
var
    Anum3 = make_number( Nan )
    Anum4 = make_number( Inf )
echo p_number( Anum1 ) , ' ' , p_number( Anum2 ) , ' ' , p_number( Anum3 ) , ' ' , p_number( Anum4 )

# 2022-04-06: All tests pass!


proc p_boolean*( a: Atom ): bool =  return a.kind == BOOL  # Return true if `a` is a BOOL atom

# TEST #
echo p_boolean( Anum1 ) , ' ' , p_boolean( Abul2 )


proc p_zero*( a: Atom ): bool =  
    # `a` is strictly equivalent to 0.0
    if a.kind != NMBR:
        return false
    else:
        return a.num == 0.0

    
# TEST #
var Anum5 = make_number( 0 )
echo p_zero( Anum1 ), ' ' , p_zero( Anum3 ) , ' ' , p_zero( Anum4 ) , ' ' , p_zero( Anum5 )


proc p_function*( a: Atom ): bool = return a.kind == FUNC  # Return true if `a` is a FUNC atom
    
# TEST #
var Afnc1: Atom 
Afnc1 = empty_function( "NoName" )
echo "Function Test: ", p_function( Anum1 ), ' ' , p_function( Afnc1 )

# 2022-04-08: All tests pass!



##### Mathematics #####
# 2022-04-10: For now following Little JavaScripter convention of base math functions working in `float`s instead of `Atom`s

proc add1*( n: float = NaN ): float =
    # Return the first argument plus  1, returns  1 if no argument given
    if isNaN( n ):
        return 1
    else:
        return n + 1


proc sub1*( n: float = NaN ): float =
    # Return the first argument minus 1, returns -1 if no argument given
    if isNaN( n ):
        return -1
    else:
        return n - 1

# TEST #
echo Anum1.num, ' ', add1( Anum1.num ) 
echo Anum2.num, ' ', sub1( Anum2.num ) 


proc plus*( ops: varargs[float] ): float =
    # Sums an arbitrary number of arguments, returns 0 if no args given
    result = 0.0
    for op in ops:
        result += op

# TEST #
echo Anum1.num, " + ", Anum2.num, " + ", Anum5.num, " = ", plus( Anum1.num, Anum2.num , Anum5.num )

# 2022-04-10: All tests pass!

proc minus*( ops: varargs[float] ): float =
    # Return the difference between the first arg and all subsequent args, returns 0 if no args given
    if ops.len() == 0:
        result = 0.0
    else:
        result = ops[0]
    for op in ops[1..ops.len-1]:
        result = result - op

# TEST #
echo Anum2.num, " - ", Anum1.num, " - ", Anum1.num, " = ", minus( Anum2.num, Anum1.num , Anum1.num )


proc multiply*( ops: varargs[float] ): float =
    # Returns the product of an arbitrary number of arguments, returns 1 if no args given
    result = 1.0
    for op in ops:
        result *= op

# TEST #
echo Anum2.num, " * ", Anum1.num, " * ", Anum1.num, " = ", multiply( Anum2.num, Anum1.num , Anum1.num )


proc divide*( ops: varargs[float] ): float =
    # Return the difference between the first arg and all subsequent args, returns 0 if no args given
    if ops.len() == 0:
        result = 1.0
    else:
        result = ops[0]
    for op in ops[1..ops.len-1]:
        result = result / op

# TEST #
echo Anum2.num, " / ", Anum1.num, " / ", Anum1.num, " = ", divide( Anum2.num, Anum1.num , Anum1.num )


proc make_list_comparator*( pairCompare: proc(op1: float, op2:float):bool ): proc( ops: varargs[float] ): bool =
    # Return a function that returns true only if the comparison of on arg to the next is true monotonically
    let helper = pairCompare
    return proc( ops: varargs[float] ): bool =
        var 
            opA = ops[0]
        result = false # also returns false if less than two arguments given
        if len( ops ) >= 2:
            result = true
            for opB in ops[1..(ops.len()-1)]:
                if not helper( opA, opB ):
                    result = false
                else:
                    opA = opB

proc lt_help(op1: float, op2: float): bool =  return op1 <  op2  # Less    Than
proc gt_help(op1: float, op2: float): bool =  return op1 >  op2  # Greater Than
proc le_help(op1: float, op2: float): bool =  return op1 <= op2  # Less    Than or Equal To
proc ge_help(op1: float, op2: float): bool =  return op1 >= op2  # Greater Than or Equal To
            

let lt* = make_list_comparator(lt_help)  # list comparator for "<" Less Than
let gt* = make_list_comparator(gt_help)  # list comparator for ">" Greater Than
let le* = make_list_comparator(le_help)  # list comparator for "<=" Less Than or Equal To
let ge* = make_list_comparator(ge_help)  # list comparator for ">=" Greater Than or Equal To

# TEST #
echo lt( 1.0, 2.0, 3.0, 4.0 )
echo lt( 1.0, 2.0, 4.0, 3.0 )
echo gt( 4.0, 3.0, 2.0, 1.0 )
echo gt( 4.0, 3.0, 1.0, 2.0 )
echo le( 1.0, 2.0, 4.0, 4.0 )
echo le( 1.0, 2.0, 4.0, 3.0 )
echo ge( 4.0, 3.0, 2.0, 2.0 )
echo ge( 4.0, 3.0, 1.0, 2.0 )

# 2022-04-13: All tests pass!



##### Environment #####

# FIXME: RENAME --> `lookupInContextHelp`
proc lookupInEntryHelp*( env: Env, ident: string, DNEcallback: proc( en: Env, id: string ): Atom ): Atom =
    # Does the work of searching for a bound name in this and all containing contexts
    # 1: List of names null, invoke the contingency func
    if len( env.boundVars ) == 0:
        return DNEcallback( env, ident )
    # 2. Item name match, return item value
    if p_binding_exists( env, ident ):
        return get_bound_atom( env, ident )
    # 3. Search in this Env failed, look at the containing context
    if env.parent != nil:
        return lookupInEntryHelp( env, ident, DNEcallback )
    else:
        return make_error( UNBOUND, "lookupInEntryHelp: Could NOT create a binding for \"" & ident & "\"!" )

# FIXME: RENAME --> `lookupInContext`
proc lookupInEntry*(env: Env, ident: Atom, DNEcallback: proc( en: Env, id: string ): Atom ): Atom =
    # Return value associated with name if entry, otherwise (entry-f name)
    return lookupInEntryHelp( env, ident.str, DNEcallback )
    
# TEST #
var 
    eBuiltin  : Env = newEnv()
    eFunction : Env = newEnv()
    eAnonBlock: Env = newEnv()
eAnonBlock.parent = eFunction
eFunction.parent  = eBuiltin
bind_atom( eBuiltin,  "foo", make_number(1.0) )
bind_atom( eFunction, "bar", make_number(2.0) )
bind_atom( eFunction, "baz", make_number(3.0) )

proc TEST_CALLBACK( en: Env, id: string ): Atom =
    # FIXME, START HERE: BIND THE ATOM, ECHO IT, RETURN IT

echo $1.0