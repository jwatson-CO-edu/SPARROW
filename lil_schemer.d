module lil_schemer;

/* * Text:      The Lil' Javascripter, Originally by Douglas Crockford
   * Chapter:   source, Modified by James Watson
   * Project:   SPARROW, [S]cheme [P]rogram [A]llowing [R]easonable [R]eckoning [O]f [W]ork
   * Component: Scheme mini-implementation, written in Dlang instead of Javascript

   rdmd lil_schemer.d
   
   https://github.com/jwatson-CO-edu/FINCH/blob/main/JS/JS_Scheme_f_rename.js
   James Watson, 2022-09 */

/*
///// DEV THOUGHTS & PLANS /////

/// Comparison of SPARROW and FINCH ///
Feature/Structure | SPARROW      | FINCH
-------------------------------------------
Structure           Cons           Object   // Can an object be versatile, lightweight, and fast?  Needs study
Syntax Unit         Atom           Atom     // Would like to have a lightweight atom for FINCH
Eval Unit           ExprInContext  Fragment // `ExprInContext` was a happy accident in partially modelling a Fragment
Context             Scoped         Flow     // Is flow based programming relevant outside of an event loop?

*/

////////// INIT ////////////////////////////////////////////////////////////////////////////////////

///// Imports /////
import std.string; // ----------- `string` type
import std.stdio; // ------------ `writeln`
import std.conv; // ------------- string conversions
import std.uni; // -------------- `strip`
import std.math.operations; // -- `NaN`
import std.typecons; // ---------- Tuple
import std.ascii; // ------------- Whitespace test
import std.algorithm.searching; // `canFind``
alias  is_white = std.ascii.isWhite; 

///// Env Vars /////
bool _DEBUG_VERBOSE = false; // Set true for debug prints


////////// ATOMS ///////////////////////////////////////////////////////////////////////////////////

enum F_Error{ 
    OKAY    = "OKAY", // -- No error code applicable
    NOVALUE = "NOVALUE", // There is no value held in this atom
    NAN     = "NAN", // --- Not A Number
    DNE     = "DNE", // --- Does Not Exist
    SYNTAX  = "SYNTAX", //- Syntax error
    LEXER   = "LEXER", // - Eval machinery failed
}


enum F_Type{ 
    CONS, // Cons pair
    STRN, // String/Symbol
    NMBR, // Number
    EROR, // Error object
    BOOL, // Boolean value
    FUNC, // Function
    // NULL, // Null // 2022-09-03: Trying it w/o NULL
}


struct Atom{
    F_Type  kind; // ---------------- What kind of atom this is
    Atom*   car; // ----------------- Left  `Atom` Pointer
    Atom*   cdr; // ----------------- Right `Atom` Pointer
    double  num; // ----------------- Number value
    string  str; // ----------------- String value, D-string underlies
    bool    bul; // ----------------- Boolean value
    F_Error err = F_Error.NOVALUE; /* Error code, 2022-09-03: Any atom can have an error code
                                      Instead of NULL, we can ask the Atom if it has a fault code assigned to it */
}


Atom* empty_atom(){
    // Allocate and return an a `NOVALUE` error
    return new Atom(
        F_Type.EROR,
        null,
        null,
        double.nan,
        "NO VALUE",
        false,
        F_Error.NOVALUE
    );
}


Atom* make_string( string str ){
    // Make a string
    return new Atom(
        F_Type.STRN,
        null,
        null,
        double.nan,
        str,
        (str.length > 0),
        F_Error.OKAY
    );
}


Atom* make_number( double nmbr ){
    // Make a number
    return new Atom(
        F_Type.NMBR,
        null,
        null,
        nmbr,
        "",
        (nmbr > 0.0),
        F_Error.OKAY
    );
}

Atom* make_error( F_Error code, string msg ){
    // Make an error
    return new Atom(
        F_Type.EROR,
        null,
        null,
        double.nan,
        msg,
        false,
        code
    );
}

Atom* make_bool( bool val ){
    // Make a Boolean value
    return new Atom(
        F_Type.BOOL,
        null,
        null,
        double.nan,
        "",
        val,
        F_Error.OKAY
    );
}


///// Cons /////

Atom* make_cons( Atom* car = null, Atom* cdr = null, ){
    // Make a pair
    return new Atom(
        F_Type.CONS,
        car,
        cdr,
        double.nan,
        "",
        true,
        F_Error.OKAY
    );
}


///// Function /////

Atom* make_function( Atom* parameters = null, Atom* definition = null ){
    // Make a function
    return new Atom(
        F_Type.FUNC, // Function
        parameters, //- Cons list of parameter symbols
        definition, //- Cons structure containing code
        double.nan, //- No number interpretation
        "function", //- Name as a string
        true, // ------ Assume truthiness
        F_Error.OKAY // Assume okay
    );
}

///// Getters and Setters ////////////////////////

// Basic Getters //
Atom* get_car( Atom* atm ){  return atm.car;  } // ---------------------- Get left  pair item
Atom* get_cdr( Atom* atm ){  return atm.cdr;  } // ---------------------- Get right pair item
Atom* first(   Atom* atm ){  return get_car(atm);  } // ----------------- Return the first item of an LS pair
Atom* second(  Atom* atm ){  return get_car(get_cdr(atm));  } // -------- Return the second item in a list, (cadr l)
Atom* third(   Atom* atm ){  return get_car(get_cdr(get_cdr(atm)));  } // Return the third item of 'l', (caddr l)

// Aliased Getters //
Atom* condLinesOf( Atom* atm ){  return get_cdr( atm );  }
Atom* formLinesOf( Atom* atm ){  return get_cdr( atm );  }
Atom* argsOf(      Atom* atm ){  return get_cdr( atm );  }
// Atom* paramsOf(    Atom* atm ){  return get_cdr( atm );  }
// Atom* tableOf(     Atom* atm ){  return first( atm );    }
Atom* nameOf(      Atom* atm ){  return first( atm );    }
Atom* questionOf(  Atom* atm ){  return first( atm );    }
Atom* textOf(      Atom* atm ){  return second( atm );   }
Atom* formalsOf(   Atom* atm ){  return first( atm );    }
Atom* answerOf(    Atom* atm ){  return second( atm );   }
Atom* bodyOf(      Atom* atm ){  return second( atm );   }


// Basic Setters //

bool set_car_B( Atom* atm, Atom* carAtm ){  
    // Set left  pair item
    atm.car = carAtm;  
    return true;  
} 


bool set_cdr_B( Atom* atm, Atom* cdrAtm ){  
    // Set right pair item
    atm.cdr = cdrAtm;  
    return true;  
} 


////////// PREDICATES //////////////////////////////////////////////////////////////////////////////

bool p_empty( Atom* atm ){  
    //- Atom either has the `NOVALUE` code or is null
    return (atm == null) || (atm.err == F_Error.NOVALUE);  
} 
bool p_has_error( Atom* atm ){  return (atm.err != F_Error.OKAY);  } // Atom has any code other than `OKAY`
bool p_cons( Atom* atm ){  return (atm.kind == F_Type.CONS);  } // ---- Return true if Atom is a pair
bool p_literal( Atom* atm ){  return (atm.kind == F_Type.NMBR) || (atm.kind == F_Type.STRN) || (atm.kind == F_Type.BOOL);  }
bool p_number( Atom* atm ){  return (atm.kind == F_Type.NMBR);  }
bool p_string( Atom* atm ){  return (atm.kind == F_Type.STRN);  }
bool p_bool( Atom* atm ){  return (atm.kind == F_Type.BOOL);  }
bool p_zero( Atom* atm ){  return (atm.kind == F_Type.NMBR) && (atm.num == 0.0);  }


////////// MATHEMATIC PRIMITIVE HELPERS ////////////////////////////////////////////////////////////

///// Dlang Math /////

double add1( double n ){  return n + 1.0;  } // Increment
double sub1( double n ){  return n - 1.0;  } // Decrement


double add( double[] args ){ 
    // sums an arbitrary number of arguments, returns 0 if no args given
    // typesafe variadic function: https://dlang.org/spec/function.html#typesafe_variadic_functions
    double sum = 0.0;
    foreach (double x; args)
        sum += x;
    return sum;
}


double minus( double[] args ){ 
    // returns the difference between the first arg and all subsequent args, returns NaN if no args given
    if( args.length == 0 ){
        return NaN(0);
    }else if( args.length == 1 ){
        return -args[0];
    }else{
        double total = args[0];
        foreach (double x; args[1..$])
            total -= x;
        return total;
    }
    return NaN(0);
}


double multiply( double[] args ){ 
    // returns the product of an arbitrary number of arguments, returns 1 if no args given
    // typesafe variadic function: https://dlang.org/spec/function.html#typesafe_variadic_functions
    double prod = 1.0;
    foreach (double x; args)
        prod *= x;
    return prod;
}


double divide( double[] args ){ 
    // returns the quotient of the first agument divided by every subsequent argument, returns 1 if no args given
    // typesafe variadic function: https://dlang.org/spec/function.html#typesafe_variadic_functions
    if( args.length == 0 ){
        return NaN(0);
    }else if( args.length == 1 ){
        return 1.0/args[0];
    }else{
        double total = args[0];
        foreach (double x; args[1..$])
            total /= x;
        return total;
    }
    return NaN(0);
}


bool lt( double[] args ){
    // Less Than, 2 or more arguments, If insufficient arguments, then return False 
    if( args.length < 2 ){  return false;  }
    else{
        double last = args[0];
        foreach (double x; args[1..$]){
            if( last >= x ){  return false;  }
            last = x;
        }
        return true;
    }
}


bool gt( double[] args ){
    // Greater Than, 2 or more arguments, If insufficient arguments, then return False 
    if( args.length < 2 ){  return false;  }
    else{
        double last = args[0];
        foreach (double x; args[1..$]){
            if( last <= x ){  return false;  }
            last = x;
        }
        return true;
    }
}


bool le( double[] args ){
    // Less Than Or Equal To, 2 or more arguments, If insufficient arguments, then return False 
    if( args.length < 2 ){  return false;  }
    else{
        double last = args[0];
        foreach (double x; args[1..$]){
            if( last > x ){  return false;  }
            last = x;
        }
        return true;
    }
}


bool ge( double[] args ){
    // Greater Than Or Equal To, 2 or more arguments, If insufficient arguments, then return False 
    if( args.length < 2 ){  return false;  }
    else{
        double last = args[0];
        foreach (double x; args[1..$]){
            if( last < x ){  return false;  }
            last = x;
        }
        return true;
    }
}


////////// LIST PROCESSING /////////////////////////////////////////////////////////////////////////


///// Accessing & Constructing ///////////////////

Atom* consify_atom( Atom* carAtm ){
    // Wrap the `atm` in a cons, with `carAtm` as 'car'
    return make_cons( carAtm, empty_atom() );
}


Atom* find_terminus( Atom* list ){
    // Iterate to the ending cons of the list and return a pointer to it
    // 0. Set the argument equal to our pointer
    Atom* curr = list;
    // 1. If this is a cons structure, then we must find the end of it
    if( list.kind == F_Type.CONS ){
        // 2. Iterate pointer to next `cdr` until we reach a pair that contains the terminating null, return pair
        while( !p_empty( curr.cdr ) ){  curr = curr.cdr;  }
        // while( !p_empty( curr ) ){  curr = curr.cdr;  }
        return curr;
    }else{ // Else atom was literal, it is its own terminus
        return list;
    }
}


Atom* append( Atom* list, Atom* atm = null ){
    // Append an atom to the end of a conslist, Create a conslist if none exists, return pointer to list head
    Atom* rtnLst = null;
    Atom* endCns = null;
    // 1. If the given list is a cons list, it is either an empty cons or the head of a LISP list
    if( list.kind == F_Type.CONS ){
        /* 2. If we were given an atom to append, it either belongs in the `car` of the empty cons,
              or in the `car` of a new terminal cons */
        if( p_empty( list.car ) ){
            set_car_B( list, atm );
        }else{
            endCns = find_terminus( list );
            set_cdr_B( endCns, consify_atom( atm ) );
        }
        rtnLst = list;
    // 3. Else we either have one or two non-cons atoms
    }else{ 
        rtnLst = consify_atom( list ); // --------------- ( `list` , [X] )
        if( !p_empty( atm ) ){
            set_cdr_B( rtnLst, consify_atom( atm ) ); // ( `list` , ( `atom` , [X] ) )
        }
    }
    return rtnLst;
}


Atom* make_list_of_2( Atom* atm1, Atom* atm2 ){
    // return a two-item list with 's1' as the first item and 's2' as the second item
    return make_cons( atm1, make_cons( atm2, empty_atom() ) );
}



///// Printing ///////////////////////////////////

string str( Atom* item ){
    // Return the string representation of the `item`
    // Null Symbol: https://www.fileformat.info/info/unicode/char/29c4/index.htm
    string rtnStr = "";
    if( p_empty( item ) ){  rtnStr = "\xE2\xA7\x84";  }
    else{
        switch( item.kind ){
            case F_Type.STRN:
                rtnStr = item.str;
                break;
            case F_Type.BOOL:
                if( item.bul ){  rtnStr = "T";  }
                else{            rtnStr = "F";  }
                break;
            case F_Type.NMBR:
                rtnStr = item.num.to!string();
                break;
            case F_Type.CONS:
                rtnStr = "( "~str(item.car)~", "~str(item.cdr)~" )";
                break;
            case F_Type.EROR:
                rtnStr = "( ERROR: " ~ item.err ~ ", " ~ item.str ~ " )";
                break;
            default: break;
        }
    }
    return rtnStr;
}

void prnt( Atom* atm ){  writeln( str( atm ) );  } // Print a cons structure



////////// LEXING //////////////////////////////////////////////////////////////////////////////////

string[string] RESERVED;


void init_reserved(){
    RESERVED["("] = "open_parn"; // Open  paren
    RESERVED[")"] = "clos_parn"; // Close paren
}


string find_reserved( string token ){
    // Return the name of the reserved symbol, or an empty string if not found
    string* res = token in RESERVED;
    if( res !is null ){  return *res;  } // If key in dict, then return the string name of the reserved token
    else{  return "";  } // --------------- Else the search failed, return an empty string
}


string[] tokenize( string expStr, dchar sepChar = ' ' ){
    // Separate an expression string into tokens
    string[] tokens;
    dchar    c      = ' ';
    string   token  = "";
    bool     testWhite = false;
    if( sepChar == ' ' ) testWhite = true;


    // Helpers //
    void stow_token(){  tokens ~= token;  token = "";  }
    void stow_char(){  tokens ~= to!string( c );  }
    void cache_char(){  token ~= c;  }

    // 0. Apply the postfix hack
    expStr ~= sepChar;
    // 1. For every character in the string
    foreach( i, ch_i; expStr ){
        // 2. Fetch character
        c = ch_i;
        // 3. Either add char to present token or create a new one
        // A. Case Open Paren
        if ( find_reserved( c.to!string() ) == "open_parn" ){  stow_char();  }  
        // B. Case Close Paren
        else if( find_reserved( c.to!string() ) == "clos_parn" ){  
            if( token.length > 0 ){  stow_token();  }
            stow_char();  
        }
        // C. Case separator
        else if( (testWhite && is_white(c)) || (c == sepChar)){
            if(token.length > 0){  stow_token();  }
        // D. Case any other char
        }else{  cache_char();  } 
    }
    // N. Return the vector of tokens
    return tokens;
}
    


////////// ENVIRONMENT /////////////////////////////////////////////////////////////////////////////

struct Env{
    Env* /*----*/ parent = null; // Pointer to the environment that contains this one
    Atom*[] /*-*/ freeVars; // ---- Free  variables, without binding
    Atom*[string] boundVars; // --- Bound variables, have names given to them by statements
}


void bind_atom( Env* env, string name, Atom* atom ){
    // Bind an `atom` to a `name` by adding it to the mapping, If the name already exists, it will be updated
    env.boundVars[ name ] = atom;
}


bool p_binding_exists( Env* env, string name ){
    // Return T if the binding exists in the `boundVars` of `env`, otherwise return F
    if((name in env.boundVars) is null){
        if( env.parent == null )
            return false;
        else
            return p_binding_exists( env.parent, name );
    }else
        return true;
}


Atom* get_bound_atom( Env* env, string name ){
    // Return the atom bound to `name`, if it exists, Otherwise return an empty atom
    if((name in env.boundVars) is null){
        if( env.parent == null )
            return empty_atom();  
        else
            return get_bound_atom( env.parent, name );
    }else
        return env.boundVars[ name ];
}


Env* enclose( Env* parent, Atom* names, Atom* values ){
    // Create a child `Env` of `parent`, then bind `values` to `names` in the child context
    Env* rtnEnv   = new Env();
    rtnEnv.parent = parent;
    string[] nams = flatten_string_list( names  );
    Atom*[]  vals = flatten_atom_list(   values );
    if( nams.length == vals.length ){
        foreach( i, nam; nams ){
            bind_atom( rtnEnv, nam, vals[i] );
        }
    }
    return rtnEnv;
}


Env* baseEnv; // Global context


void init_env(){
    // Create the base environment that is parent of all contexts in the interpreter
    baseEnv = new Env();
}



////////// INTERPRETATION && EXECUTION /////////////////////////////////////////////////////////////

///// Scheme --to-> D ////////////////////////////

double[] flatten_double_list( Atom* dbblList ){
    // Take a LISP list of numbers and convert to a Dlang dyn. array
    Atom*    currCons = dbblList;
    double[] rtnArr;
    while( !p_empty( currCons ) ){
        if( p_number( currCons.car ) ){  rtnArr ~= currCons.car.num;  }
        currCons = currCons.cdr;
    }
    return rtnArr;
}


string[] flatten_string_list( Atom* strnList ){
    // Take a LISP list of strings and convert to a Dlang dyn. array
    Atom*    currCons = strnList;
    string[] rtnArr;
    while( !p_empty( currCons ) ){
        if( p_string( currCons.car ) ){  rtnArr ~= currCons.car.str;  }
        currCons = currCons.cdr;
    }
    return rtnArr;
}


Atom*[] flatten_atom_list( Atom* atomList ){
    // Take a LISP list of Atoms and convert to a Dlang dyn. array
    Atom*   currCons = atomList;
    Atom*[] rtnArr;
    while( !p_empty( currCons ) ){
        rtnArr ~= currCons.car;
        currCons = currCons.cdr;
    }
    return rtnArr;
}


Atom*[] flatten_cons_list( Atom* atomList ){
    // Take a LISP list of Atoms and convert to a Dlang dyn. array
    Atom*   currCons = atomList;
    Atom*[] rtnArr;
    ulong   depth = 0;
    while( !p_empty( currCons ) ){
        rtnArr ~= currCons.car;
        currCons = currCons.cdr;
    }
    return rtnArr;
}


///// Primitives /////

Atom* function( Atom* )[string] primitiveFunctions; // Dictionary of foundational operations, implemented in Dlang
Atom* function()[string] /*--*/ primitiveSymbols; // - Dictionary of text aliases of important symbols


bool p_primitve_symbol( string token ){    return (token in primitiveSymbols) !is null;  } // - In the primitive sym dict?
bool p_primitve_function( string token ){  return (token in primitiveFunctions) !is null;  } // In the primitive func dict?


void init_primitives(){

    /// Zero Arguments ///

    primitiveSymbols["true"]  = function Atom*(){  return make_bool(true);   }; // Boolean True
    primitiveSymbols["#t"]    = function Atom*(){  return make_bool(true);   }; // Boolean True
    primitiveSymbols["false"] = function Atom*(){  return make_bool(false);  }; // Boolean False
    primitiveSymbols["#f"]    = function Atom*(){  return make_bool(false);  }; // Boolean False
    
    /// One Argument ///

    primitiveFunctions["atom?"] = function Atom*( Atom* args ){
        // Predicate: Is this atom a literal?
        // FIXME: CONVERT THIS FUNCTION TO HANDLE MANY ARGUMENTS WHERE ALL ATOMS TESTED
        if( p_literal( first( args ) ) ){  return make_bool(true);  }
        else{  return make_bool(false);  }
    };

    /// Many Arguments ///

    primitiveFunctions["eq?"] = function Atom*( Atom* args ){
        // Predicate: Are these atoms of the same type and value?
        Atom*[] atoms = flatten_atom_list( args );
        if( atoms.length > 1 ){
            F_Type typ0 = atoms[0].kind;
            foreach(Atom* atm; atoms[1..$]){  if(atm.kind != typ0){  return make_bool(false);  }  }
            // NOTE: WOULD BE NICE TO USE A `Variant` HERE? (loops) (Algebraic?)
            switch( typ0 ){
                case F_Type.STRN:
                    string val0 = atoms[0].str;
                    foreach(Atom* atm; atoms[1..$]){  if(atm.str != val0){  return make_bool(false);  }  }
                    break;
                case F_Type.NMBR:
                    double val0 = atoms[0].num;
                    foreach(Atom* atm; atoms[1..$]){  if(atm.num != val0){  return make_bool(false);  }  }
                    break;
                case F_Type.BOOL:
                    bool val0 = atoms[0].bul;
                    foreach(Atom* atm; atoms[1..$]){  if(atm.bul != val0){  return make_bool(false);  }  }
                    break;
                default: 
                    return make_bool(false);
            }
            return make_bool(true);       
        }else{  return make_bool(false);  }
    };


    primitiveFunctions["empty?"] = function Atom*( Atom* args ){
        // Predicate: Is this an empty atom?
        Atom*[] atoms = flatten_atom_list( args );
        foreach(Atom* atm; atoms){  if( !p_empty( atm ) ){  return make_bool(false);  }  }
        return make_bool(true);
    };


    primitiveFunctions["zero?"] = function Atom*( Atom* args ){
        // Predicate: Is this a number atom wtih a zero value?
        Atom*[] atoms = flatten_atom_list( args );
        foreach(Atom* atm; atoms){  if( !p_zero( atm ) ){  return make_bool(false);  }  }
        return make_bool(true);
    };
    

    primitiveFunctions["number?"] = function Atom*( Atom* args ){
        // Predicate: Is this atom of number type?
        Atom*[] atoms = flatten_atom_list( args );
        foreach(Atom* atm; atoms){  if( !p_zero( atm ) ){  return make_bool(false);  }  }
        return make_bool(true);
    };


    primitiveFunctions["+"] = function Atom*( Atom* args ){
        // Add 1 or more number atoms
        double[] ops = flatten_double_list( args );
        return make_number( add( ops ) );
    };


    primitiveFunctions["-"] = function Atom*( Atom* args ){
        // Negate 1 or subtract more number atoms
        double[] ops = flatten_double_list( args );
        return make_number( minus( ops ) );
    };


    primitiveFunctions["*"] = function Atom*( Atom* args ){
        // Multiply 1 or more number atoms
        double[] ops = flatten_double_list( args );
        return make_number( multiply( ops ) );
    };


    primitiveFunctions["/"] = function Atom*( Atom* args ){
        // Inverse 1 or divide more number atoms
        double[] ops = flatten_double_list( args );
        return make_number( divide( ops ) );
    };


    primitiveFunctions["1+"] = function Atom*( Atom* args ){
        // Increment 1 or more number atoms by 1
        Atom*[] ops = flatten_atom_list( args );
        Atom* rtnLst;
        if( ops.length > 1 ){
            rtnLst = make_cons();
            foreach(Atom* atm; ops){  if( p_number( atm ) ){  rtnLst = append( rtnLst, make_number( add1( atm.num ) ) );  }  }
        }else if( ops.length == 1 )
            rtnLst = make_number( add1( ops[0].num ) );
        return rtnLst;
    };


    primitiveFunctions["1-"] = function Atom*( Atom* args ){
        // Decrement 1 or more number atoms by 1
        Atom*[] ops = flatten_atom_list( args );
        Atom* rtnLst;
        if( ops.length > 1 ){
            rtnLst = make_cons();
            foreach(Atom* atm; ops){  if( p_number( atm ) ){  rtnLst = append( rtnLst, make_number( sub1( atm.num ) ) );  }  }
        }else if( ops.length == 1 )
            rtnLst = make_number( sub1( ops[0].num ) );
        return rtnLst;
    };


    primitiveFunctions["<"] = function Atom*( Atom* args ){
        // Less Than, for 2 or more Number atoms
        double[] ops = flatten_double_list( args );
        return make_bool( lt( ops ) );
    };


    primitiveFunctions[">"] = function Atom*( Atom* args ){
        // Greater Than, for 2 or more Number atoms
        double[] ops = flatten_double_list( args );
        return make_bool( gt( ops ) );
    };


    primitiveFunctions["<="] = function Atom*( Atom* args ){
        // Less Than Or Equal To, for 2 or more Number atoms
        double[] ops = flatten_double_list( args );
        return make_bool( le( ops ) );
    };


    primitiveFunctions[">="] = function Atom*( Atom* args ){
        // Greater Than Or Equal To, for 2 or more Number atoms
        double[] ops = flatten_double_list( args );
        return make_bool( ge( ops ) );
    };


    primitiveFunctions["cons"] = function Atom*( Atom* args ){
        // Cons up to 2 atoms together into a pair. Missing params filled with `empty`
        Atom*[] atoms = flatten_atom_list( args );
        if( atoms.length >= 2 ) return make_cons( atoms[0], atoms[1] ); // Only takes 1st two args
        if( atoms.length == 1 ) return make_cons( atoms[0] ); // --------- One arg is accepted, other empty
        else /*--------------*/ return make_cons(); // ------------------- Two empty accepted
    };
}



////////// PARSING /////////////////////////////////////////////////////////////////////////////////

///// Predicates /////

bool p_float_string( string inputStr ){
    // Predicate: Is this string suitable to be converted to a (double) number?
    string slimStr = strip( inputStr );
    try{
        slimStr.to!double();
        return true;
    }catch( ConvException e ){
        if(_DEBUG_VERBOSE){  writeln( "CONVERSION ERROR: Cannot convert \"", slimStr, "\"" );  }
    }
    return false;
}


bool p_empty_atom_string( string inputStr ){
    // Return T if the string is appropriate for empty atom conversion, otherwise return F
    if( inputStr == null ){ /*------*/ return true;  }
    if( inputStr == "" ){ /*--------*/ return true;  }
    if( inputStr == "\xE2\xA7\x84" ){  return true;  }
    if( inputStr == "[/]" ){ /*-----*/ return true;  }
    return false;
}

///// Text Processing /////

Atom* atomize_string( string token ){
    // Convert a string token into a non-cons atom
    if( p_float_string( token ) ){       return make_number( token.to!double() );  }  
    if( p_empty_atom_string( token ) ){  return empty_atom();  }  
    if( p_primitve_symbol( token ) ){    return primitiveSymbols[ token ]();  }
   /* else assume string -------------*/ return make_string( token );
}


bool p_open_paren( string token ){  return find_reserved( token ) == "open_parn";  } // Is an open  paren?
bool p_clos_paren( string token ){  return find_reserved( token ) == "clos_parn";  } // Is an close paren?


bool p_balanced_parens( string[] tokens ){
    // Return true if parens open and close in a quantity and order that returns to root depth, Otherwise return false
    int depth = 0;
    foreach( string token; tokens ){
        if( p_open_paren( token ) ) depth++; 
        else
        if( p_clos_paren( token ) ) depth--;
    }
    return (depth == 0);
}


bool p_bounded_parens( string[] tokens ){
    // Return true if the token sequence begins and ends with open and close parens, respectively
    if( p_open_paren( tokens[0] ) ){
        if( p_clos_paren( tokens[$-1] ) )  return true;  else  return false;
    }else return false;
}


bool p_parent_parens( string[] tokens ){
    // Return true if the outermost parens define a simple list
    int   depth  = 0;
    ulong i      = 0;
    ulong seqLen = tokens.length;
    if( p_bounded_parens( tokens ) ){
        foreach( string token; tokens ){
            i++;
            if( p_open_paren( token ) ) depth++; 
            else
            if( p_clos_paren( token ) ) depth--;
            if( (depth == 0) && (i<seqLen) ) return false;
        }
        return true;
    }else return false;
}


Atom* consify_token_sequence( string[] tokens ){
    // Recursively render tokens as a cons structure
    // 2022-11: Rewritten
    ulong    seqLen  = tokens.length;
    ulong    bgn;
    ulong    end;
    int /**/ depth   = 0;
    ulong    index;
    string   token;
    string[] carPart;
    string[] cdrPart;
    Atom*    lstRoot = null;

    if( _DEBUG_VERBOSE )  writeln( "Input: " ~ tokens );

    // Base Case: There were no tokens, return Empty
    if( seqLen == 0 ){  return empty_atom();  }
    
    // Base Case: There was one token, Return it
    if( seqLen == 1 ){  return atomize_string( tokens[0] );  }

    // Recursive Case: Multiple tokens, is at least a list
    if( seqLen > 1 ){

        // Establish bounds
        bgn = 0;
        end = seqLen-1;

        // Are the parens correct?
        if( p_balanced_parens( tokens ) ){
            
            // Are we building a list?
            if( p_parent_parens( tokens ) ){
                bgn++;
                end--;
                // Begin list
                index   = bgn;
                lstRoot = make_cons();
                // While we are in the list bounds
                while( index <= end ){
                    
                    // Find an element
                    carPart = [];
                    
                    // If element is a list
                    if( p_open_paren( tokens[index] ) ){
                        do{
                            token = tokens[index];
                            if( p_open_paren( token ) ) depth++; 
                            else
                            if( p_clos_paren( token ) ) depth--;
                            carPart ~= token;
                            index++;
                        }while( (depth > 0) && (index <= end) );

                    // else element was atom
                    }else{
                        carPart ~= tokens[index];
                        index++;
                    }

                    // Append element to list
                    lstRoot = append( 
                        lstRoot,
                        consify_token_sequence( carPart )
                    );
                }
                return lstRoot;
            }else{
                return make_error( F_Error.SYNTAX, "BAD LIST CASE" );
            }
        }else{
            return make_error( F_Error.SYNTAX, "PARENTHESES MISMATCH" );
        }
    }
    return make_error( F_Error.LEXER, "LEXER FAILED, This branch was ?UNREACHABLE?" );
}


Atom* expression_from_string( string expStr, dchar sepChar = ' ' ){
    // Tokenize the `expStr`, express it as a nested cons struct, and return
    string[] tokens = tokenize( expStr, sepChar );
    // writeln( tokens );
    return consify_token_sequence( tokens );
}



////////// SPECIAL FORMS ///////////////////////////////////////////////////////////////////////////

struct ExprInContext{
    // Container struct for an expression and its context, Used to simultaneously return expression and context
    Atom*  expr;
    Env*   context;
    string tag;
}

ExprInContext function( ExprInContext )[string] specialForms; // Dictionary of forms other than func applications, implemented in Dlang


bool truthiness( Atom* atm = null ){
    // Determine the truth value of an atom
    if( !(atm is null) ){
        switch( atm.kind ){
            case F_Type.STRN:
                // A string is true if it has length
                return (atm.str.length > 0);
            case F_Type.BOOL:
                // A Boolean is already a truth value
                return atm.bul;
            case F_Type.NMBR:
                // A number is true if it above zero
                return (atm.num > 0.0);
            case F_Type.CONS:
                // A cons is true if either side is non-empty
                return (!p_empty(atm.car)) || (!p_empty(atm.cdr));
            case F_Type.EROR:
                // An error is always false
                return false;
            case F_Type.FUNC:
                // A function is always true
                return true;
            default: 
                // This should not happen, return false
                return false;
        }
    }
    // No arg or null arg, returh false
    return false;
}


void init_specials(){
    // Assign functions to handle special forms

    specialForms["quote"] = function ExprInContext( ExprInContext eINc ){  
        // Passthru for expression args 
        return ExprInContext(
            textOf( eINc.expr ),
            eINc.context,
            "quote"
        );
    };
    

    specialForms["lambda"] = function ExprInContext( ExprInContext eINc ){  
        // Package anonymous function for eval
        Atom* forms = get_cdr( eINc.expr );
        return ExprInContext(
            make_function( first( forms ), get_cdr( forms ) ),
            eINc.context,
            "non-primitive"
        );
    };


    specialForms["cond"] = function ExprInContext( ExprInContext eINc ){  
        // Package cond for eval
        return evcon( ExprInContext(
            condLinesOf(eINc.expr),
            eINc.context,
            "cond"
        ) );
    };


    specialForms["define"] = function ExprInContext( ExprInContext eINc ){  
        // Bind expression result to a name

        if( _DEBUG_VERBOSE ) writeln( "`define`: " ~ str( eINc.expr )  );
        
        // 1. Bind name
        bind_atom( 
            eINc.context, // ---------- Context to bind to
            second( eINc.expr ).str, // Name to bind
            meaning( ExprInContext( //- Expression result to bind
                third( eINc.expr ), // Eval this
                eINc.context, // ----- Context for eval
                "meaning" // --------- Eval tag
            ) ).expr 
        );

        // 2. Return modified context
        return ExprInContext(
            second( eINc.expr ), // Name that was bound
            eINc.context, // ------ Modified context
            "define" // ----------- Binding tag
        );
    };


    specialForms["and"] = function ExprInContext( ExprInContext eINc ){  
        // Return true only if all the forms evaluate to true, otherwise return false
        Atom*   forms   = formLinesOf( eINc.expr );
        Atom*[] formLst = flatten_atom_list( forms );

        foreach (Atom* form; formLst){
            if( !truthiness(
                meaning( ExprInContext( //- Expression result to bind
                    form, // Eval this
                    eINc.context, // ------- Context for eval
                    str( form ) // --------- Eval tag
                )).expr
            ) ){
                return ExprInContext(
                    make_bool( false ), // Truthiness of the element
                    eINc.context, //- Original context
                    "truthiness" // ---- Tag
                );
            }
        }

        return ExprInContext(
            make_bool( true ), // Truthiness of the element
            eINc.context, // Original context
            "truthiness" // --- Tag
        );
    };


    specialForms["or"] = function ExprInContext( ExprInContext eINc ){  
        // Return true if any of the forms evaluate to true, otherwise return false
        Atom*   forms   = formLinesOf( eINc.expr );
        Atom*[] formLst = flatten_atom_list( forms );

        if( _DEBUG_VERBOSE ) writeln( "OR" );

        foreach (Atom* form; formLst){
            if( truthiness(
                meaning( ExprInContext( //- Expression result to bind
                    form, // Eval this
                    eINc.context, // ------- Context for eval
                    str( form ) // --------- Eval tag
                )).expr
            ) ){
                return ExprInContext(
                    make_bool( true ), // Truthiness of the element
                    eINc.context, //- Original context
                    "truthiness" // ---- Tag
                );
            }
        }

        return ExprInContext(
            make_bool( false ), // Truthiness of the element
            eINc.context, // Original context
            "truthiness" // --- Tag
        );
    };

    // specialForms["load"] = function ExprInContext( ExprInContext eINc ){ /* LOAD FILE */ } // FIXME: LOAD FILE
}



////////// EVALUATION //////////////////////////////////////////////////////////////////////////////
// This is the real living mechanism of the SPARROW interpreted language
// 2022-09-13: `atomize_string` will fetch primitive symbols, these were together w/ primitve functions in Little JS

bool p_eq( Atom* op1, Atom* op2 ){  return primitiveFunctions["eq?"]( make_list_of_2( op1, op2 ) ).bul;  }
bool p_else( Atom* x ){  return p_literal( x ) && p_eq( x, make_string( "else" ) );  } // is the arg an 'else symbol?


ExprInContext evcon( ExprInContext eINc ){
    // evaluate cond form by form, this is the guts of cond
    Atom* /*---*/ forms     = eINc.expr; // ------------------------------ Fetch cond lines from the expression
    Atom* /*---*/ condition = null;
    Atom* /*---*/ answer    = null;
    ExprInContext result; // ------------------------------------------- Evaluation result

    if( _DEBUG_VERBOSE ) writeln( "\t`evcon`: received the following forms: " ~ str( forms ) );

    Atom*[] formLst = flatten_atom_list( forms );

    foreach (Atom* form; formLst){
        if( _DEBUG_VERBOSE ) writeln( "\tevcon: " ~ "Working on form - " ~ str(form) );
        condition = questionOf( form );
        answer    = answerOf( form );
        if( _DEBUG_VERBOSE ) writeln( "\tevcon: " ~ "evaluate ..." );
        if( p_else( condition ) ){
            if( _DEBUG_VERBOSE ) writeln( "\tevcon: " ~ "Default to else" );
            return meaning( ExprInContext( 
                answer,
                eINc.context,
                str( answer )
            ) );
        }else{
            if( _DEBUG_VERBOSE ) writeln( "\tevcon: " ~ "Regular condition" );
            result = meaning( ExprInContext( 
                condition,
                eINc.context,
                str( condition )
            ) );
            if( truthiness( result.expr ) ){

                if( _DEBUG_VERBOSE ) writeln( "\tevcon: " ~ "Expression was TRUE" );

                return meaning( ExprInContext( 
                    answer,
                    eINc.context,
                    str( answer )
                ) );
            }
        }
    }

    return meaning( ExprInContext( 
        make_bool( false ),
        baseEnv,
        "No Cond True"
    ) );
}


ExprInContext apply_primitive_function( ExprInContext eINc ){
    // Invocation of primitive function in a context

    if( _DEBUG_VERBOSE ) writeln( "`apply_primitive_function`" );

    string /*--*/ name /*-*/ = get_car( eINc.expr ).str;
    Atom* /*---*/ interpArgs = null;
    ExprInContext result;

    if( p_primitve_function( name ) ){

        interpArgs = meaning( ExprInContext(
            get_cdr( eINc.expr ),
            eINc.context,
            "primitive arguments"
        )).expr;
        // interpArgs = get_cdr( eINc.expr );

        if( _DEBUG_VERBOSE ) writeln( 
            "\t`apply_primitive_function`: " ~ "About to call " ~ name ~ " with the following args -> " ~ str( interpArgs ) 
        );

        result = ExprInContext(
            primitiveFunctions[ name ]( 
                interpArgs
            ), // Balance of arguments
            eINc.context, // ---- Original context
            "primitive: " ~ name // ------- Tag
        );

        if( _DEBUG_VERBOSE ) writeln( 
            "\t`apply_primitive_function`: " ~ str( eINc.expr ) ~ "--(" ~ name ~ ")-> " ~ str( result.expr )
        );

        return result;
    }else{

        if( _DEBUG_VERBOSE ) writeln( 
            "\t`apply_primitive_function`: Search for primitive function " ~ name ~ " has no result!" 
        );

        return ExprInContext(
            make_error(
                F_Error.DNE,
                "Oops, \"" ~ get_car( eINc.expr ).str ~ "\" is NOT a primitive function in SPARROW!"
            ),
            eINc.context, // ---- Original context
            "ERROR"
        );
    }
}


bool p_func( Atom* atm ){  return atm.kind == F_Type.FUNC;  }


bool p_user_def_function( Env* env, string funcName ){
    // Return true if this is the name of a user-defined function that exists in the environment under given name
    if(  p_binding_exists( env, funcName )  ){
        return p_func( get_bound_atom( env, funcName ) );
    }else{  return false;  }
}


ExprInContext apply_closure( ExprInContext input ){ 
    // apply a non-primitive function

    if( _DEBUG_VERBOSE )  writeln( "`apply_closure`" );

    Env* /*----*/ nuEnv = null;
    Atom* /*---*/ func  = null;
    ExprInContext argMeaning;

    // 0. Determine if the function exists, then construct a new context
    if(  p_user_def_function( input.context, nameOf( input.expr ).str )  ){
        // 1. Create a new context with the arguments given values as a child of the containing context

        func  = get_bound_atom( input.context, nameOf( input.expr ).str );

        argMeaning = meaning(ExprInContext(
        // argMeaning = ExprInContext(
            argsOf( input.expr ), // arguments
            input.context,
            "args meaning"
        ));
        // );

        nuEnv = enclose( 
            input.context, // ------- parent 
            formalsOf( func ), // --- parameters
            argMeaning.expr, 
        );

        if( _DEBUG_VERBOSE ){  
            writeln( "\t`apply_closure`: " ~ "Found function " ~ nameOf( input.expr ).str );
            writeln( "\t`apply_closure`: " ~ "Parameters - " ~ str(formalsOf( func ) ));
            writeln( "\t`apply_closure`: " ~ "Arguments  - " ~ str( argMeaning.expr ) );
            writeln( "\t`apply_closure`: " ~ "Body       - " ~ str( bodyOf( func ) ) );
        }

        // 2. Evaluate the function within the new context
        return meaning(
            ExprInContext(
                bodyOf( func ),
                nuEnv,
                "closure"
            )
        );
    }

    if( _DEBUG_VERBOSE )  writeln( "`apply_closure`: " ~ "NO function named " ~ nameOf( input.expr ).str );

    // 2. Evaluate the function within the new context
    return ExprInContext(
        make_error( F_Error.NOVALUE, "There is no function with the name " ~ nameOf( input.expr ).str ),
        nuEnv,
        "closure"
    );
    
}


bool p_special_form( string name ){
    // Return true if there is a special form with `name`
    return (name in specialForms) !is null;
}


bool p_bound_function( Env* env, string name ){
    // Return true if there is a bound function with this name
    if( p_binding_exists( env, name ) ){
        return p_func(  get_bound_atom( env, name )  );
    }else{  return false;  }
}


bool first_only( Atom* atm ){
    if( p_cons( atm ) ){
        return !p_empty( get_cdr( atm ) );
    }else return false;
}


ExprInContext meaning( ExprInContext eINc ){ 
    // Handle expressions more complex than literals

    if( _DEBUG_VERBOSE ) writeln( "`meaning`" );
    
    Atom* /*---*/ e /*-*/  = eINc.expr;
    Atom* /*---*/ balance  = get_cdr( e );
    Atom* /*---*/ rtnRoot  = null;
    Atom* /*---*/ inputPtr = null;
    string /*--*/ name     = "";
    ExprInContext rntResult;

    if( p_string( e ) ){  name = e.str;  }

    /// Base Cases ///

    // Base Case: Primitive Symbol
    if( p_primitve_symbol( name ) ){

        if( _DEBUG_VERBOSE ) writeln( "\t`meaning`: " ~ "Primitive Symbol" );

        rntResult = ExprInContext(
            primitiveSymbols[ name ](), // Balance of arguments
            eINc.context, // ---- Original context
            "primitive: " ~ name // ------- Tag
        );

    // Base Case: Bound Symbol
    }else if( p_binding_exists( eINc.context, name ) && (!p_bound_function( eINc.context, name )) ){

        if( _DEBUG_VERBOSE ){ 
            writeln( "\t`meaning`: " ~ "Bound Symbol, " ~ name );
            writeln( "\t`meaning`: " ~ name ~ " = " ~ str( get_bound_atom( eINc.context, name ) ) );
            writeln( eINc.context );
        }

        rntResult = ExprInContext(
            get_bound_atom( eINc.context, name ), // Balance of arguments
            eINc.context, // ---- Original context
            "Variable: " ~ name // ------- Tag
        );

    // Base Case Literal -OR- Base Case Empty: Pass thru
    }else if( p_literal(e) || p_empty(e) ){
        if( _DEBUG_VERBOSE ) writeln( "\tmeaning: " ~ "Was atom, " ~ str(e) ~ " is a " ~ e.kind.to!string );
        rntResult = eINc;

    /// Recursive Cases ///
    }else{

        if( p_cons( e ) ){  name = get_car( e ).str;  }
        
        // Case Primitive Function
        if( p_primitve_function( name ) ){

            if( _DEBUG_VERBOSE ){
                writeln( "\t`meaning`: " ~ "Primitive Function" );
                writeln( "\t`meaning`: " ~ "Args - ", str( balance ) );
            }

            rntResult = apply_primitive_function( eINc );

        // Case Special Form
        }else if( p_special_form( name ) ){

            if( _DEBUG_VERBOSE ) writeln( "`meaning`: " ~ "Special Form" );

            rntResult = specialForms[ name ]( eINc );

        // Case Function Application
        }else if( p_bound_function( eINc.context, name ) ){

            if( _DEBUG_VERBOSE ) writeln( "`meaning`: " ~ "Bound Function (Closure)" );

            rntResult = apply_closure( eINc );

        // Case Cons Structure
        }else{

            if( _DEBUG_VERBOSE ){ 
                writeln( "\t`meaning`: " ~ "Cons Structure" );
                writeln( "\t`meaning`: " ~ str( e ) );
            }

            inputPtr = e;
            rtnRoot  = make_cons();

            do{
                rtnRoot = append( rtnRoot,  
                    meaning( ExprInContext(
                        first( inputPtr ), // Balance of arguments
                        eINc.context, // ---- Original context
                        "suppress cons" // ------- Tag
                    ) ).expr
                );
                inputPtr = get_cdr( inputPtr );
            }while( !p_empty( inputPtr ) );

            rntResult = ExprInContext(
                rtnRoot,
                eINc.context, // ---- Original context
                str( rtnRoot ) // ------- Tag
            );            
        }
    } 
    if( _DEBUG_VERBOSE ) writeln( "\t`meaning`: " ~ "Final ->" ~ str( rntResult.expr ) );
    return rntResult;
}


Atom* value( Atom* expression ){
    // call `meaning`` on expression in the '$global' context
    ExprInContext result = meaning(
        ExprInContext(
            expression, // ----- Expression to be evaluated
            baseEnv, // -------- Global context
            str( expression ) // String representation of the original expression
        )
    );
    return result.expr;
}


void init_SPARROW(){
    // Populate necessary global structures
    init_reserved(); // - Reserved symbols
    init_env(); // ------ Global context
    init_primitives(); // Special atoms and Primitive Functions
    init_specials(); // - Special forms
}



////////// READ, EVALUATE, PRINT, LOOP /////////////////////////////////////////////////////////////

void read_eval_prnt_loop(){
    // Terminal interaction with SPARROW
    bool   quit   = false; // Use request to quit
    // char[] inBuf;
    string input; // -------- User input
    Atom*  expr   = null; //- S-Expression
    Atom*  output = null; //- Eval result
    
    // Preamble //
    // writeln( "\n##### SPARROW Interpreter, Version 2022-11 #####" );
    writeln( "\n##### \"Little Scheme\" Interpreter, Version 2022-11 #####" );
    writeln( "Interpreter expects complete, well-formed s-expressions,\n" ~
             "and does not currently support multi-line input.\n" ~
             "Input validation is unsupported, your mistakes will crash the program. ;P" );
    writeln( "Type \"exit\" and the [Enter] key to end program." );
    writeln( "=========================================================================\n" );

    // REPL //
    while( !quit ){
        
        // 0. Emit prompt
        write( "S> " ); 

        // 1. Read
        // readf("%s", &input);
        // readf!" %s"( input );
        // stdin.read( input );
        input = readln(); // Note no newline handling is needed as the parser will strip it automatically

        // 1.5. Handle quit
        if( canFind( input, "exit" ) )  break;

        // 2. Evaluate
        expr   = expression_from_string( input ); // FIXME: INPUT VALIDATION
        output = value( expr ); // FIXME: CATCH INTERPRETER ERRORS

        // 3. Print
        prnt( output );
        write( "\n" );

        // 4. Loop if we did not quit ___,^
    }

    // End //
    writeln( "\nProgram exit by user request. ~ Goodbye!\n" );
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN //
////////////////////////////////////////////////////////////////////////////////////////////////////

void main(){

    // Populate necessary interpreter components
    init_SPARROW();
    
    // Begin REPL
    read_eval_prnt_loop();
}
    