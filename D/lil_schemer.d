/* * Text:      The Lil' Javascripter, Originally by Douglas Crockford
   * Chapter:   source, Modified by James Watson
   * Project:   SPARROW, [S]cheme [P]rogram [A]llowing [R]easonable [R]eckoning [O]f [W]ork
   * Component: Scheme mini-implementation, written in Dlang instead of Javascript

   rdmd lil_schemer.d
   
   https://github.com/jwatson-CO-edu/FINCH/blob/main/JS/JS_Scheme_f_rename.js
   James Watson, 2022-09 */

////////// INIT ////////////////////////////////////////////////////////////////////////////////////

import std.string; // `string` type
import std.stdio; //- `writeln`
import std.conv; // - string conversions
import std.uni; // -- `strip`
import std.math.operations; // `NaN`

///// Env Vars /////
bool _DEBUG_VERBOSE = true;


////////// ATOMS ///////////////////////////////////////////////////////////////////////////////////

enum F_Error{ 
    OKAY    = "OKAY",    // No error code applicable
    NOVALUE = "NOVALUE", // There is no value held in this atom
    NAN     = "NAN",     // Not A Number
}

enum F_Type{ 
    CONS, // Cons pair
    STRN, // String/Symbol
    NMBR, // Number
    EROR, // Error object
    BOOL, // Boolean value
    // NULL, // Null // 2022-09-03: Trying it w/o NULL
}

// 2022-09-05: Structs cannot hold references, switch to address passing

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
    // Make an error
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
    F_Error mtCode;
    // if( (car != null) || (cdr != null) ){  mtCode = F_Error.OKAY; }
    // else{  mtCode = F_Error.NOVALUE;  }
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

///// Getters and Setters ////////////////////////

// Basic Getters //
Atom* get_car( Atom* atm ){  return atm.car;  } // ---------------------- Get left  pair item
Atom* get_cdr( Atom* atm ){  return atm.cdr;  } // ---------------------- Get right pair item
Atom* first(   Atom* atm ){  return get_car(atm);  } // ----------------- Return the first item of an LS pair
Atom* second(  Atom* atm ){  return get_car(get_cdr(atm));  } // -------- Return the second item in a list, (cadr l)
Atom* third(   Atom* atm ){  return get_car(get_cdr(get_cdr(atm)));  } // Return the third item of 'l', (caddr l)


// Aliased Getters //
Atom* condLinesOf( Atom* atm ){  return get_cdr( atm );   }
Atom* tableOf(     Atom* atm ){  return first( atm );     }
Atom* questionOf(  Atom* atm ){  return first( atm );     }
Atom* textOf(      Atom* atm ){  return second( atm );    }
Atom* formalsOf(   Atom* atm ){  return second( atm );    }
Atom* answerOf(    Atom* atm ){  return second( atm );    }
Atom* bodyOf(      Atom* atm ){  return third( atm );     }


// Basic Setters //
bool set_car_B( Atom* atm, Atom* carAtm ){  
    // Set left  pair item
    // if( carAtm != null ){  atm.err = F_Error.OKAY;  }
    atm.car = carAtm;  
    return true;  
} 


bool set_cdr_B( Atom* atm, Atom* cdrAtm ){  
    // Set right pair item
    // if( cdrAtm != null ){  atm.err = F_Error.OKAY;  }
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


////////// MATHEMATICS /////////////////////////////////////////////////////////////////////////////

///// Dlang Math /////
double add1( double n ){  return n + 1.0;  }
double sub1( double n ){  return n - 1.0;  }

double add(double[] args){ 
    // sums an arbitrary number of arguments, returns 0 if no args given
    // typesafe variadic function: https://dlang.org/spec/function.html#typesafe_variadic_functions
    double sum = 0.0;
    foreach (double x; args)
        sum += x;
    return sum;
}

double minus(double[] args){ 
    // returns the difference between the first arg and all subsequent args, returns NaN if no args given
    if( args.length == 0 ){
        return NaN(0);
    }else if( args.length == 1 ){
        return args[0];
    }else{
        double total = args[0];
        foreach (double x; args[1..$])
            total -= x;
        return total;
    }
    return NaN(0);
}


double multiply(double[] args){ 
    // returns the product of an arbitrary number of arguments, returns 1 if no args given
    // typesafe variadic function: https://dlang.org/spec/function.html#typesafe_variadic_functions
    double prod = 1.0;
    foreach (double x; args)
        prod *= x;
    return prod;
}


double divide(double[] args){ 
    // returns the quotient of the first agument divided by every subsequent argument, returns 1 if no args given
    // typesafe variadic function: https://dlang.org/spec/function.html#typesafe_variadic_functions
    double quot = 1.0;
    foreach (double x; args)
        quot /= x;
    return quot;
}


bool lt(double[] args){
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


bool gt(double[] args){
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


bool le(double[] args){
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


bool ge(double[] args){
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
        return curr;
    }else{ // Else atom was literal, it is its own terminus
        return list;
    }
}


Atom* append( Atom* list, Atom* atm = null ){
    // Append an atom to the end of a conslist, Create a conslist if none exists, return pointer to list head
    Atom* rtnLst = list;
    Atom* endCns = null;
    // 1. If the given list is a cons list, it is either an empty cons or the head of a LISP list
    if( rtnLst.kind == F_Type.CONS ){
        /* 2. If we were given an atom to append, it either belongs in the `car` of the empty cons,
              or in the `car` of a new terminal cons */
        if( !p_empty( atm ) ){
             if( p_empty( rtnLst.car ) ){
                rtnLst = make_cons( atm, empty_atom() );
             }else{
                endCns = find_terminus( rtnLst );
                set_cdr_B( endCns, consify_atom( atm ) );
             }
        }
    }else{ // 3. Else we either have one or two non-cons atoms
        rtnLst = consify_atom( rtnLst ); // --------------- ( `list` , [X] )
        if( !p_empty( atm ) ){
            set_cdr_B( rtnLst, consify_atom( atm ) ); // ( `list` , ( `atom` , [X] ) )
        }
    }
    return rtnLst;
}

Atom* make_list_of_2( Atom* atm1, Atom* atm2 ){
    // return a two-item list with 's1' as the first item and 's2' as the second item
    return make_cons( atm1, make_cons(atm2, empty_atom));
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
    ulong    expLen = expStr.length;

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
        else if( (c == sepChar) && (token.length > 0) ){  stow_token();  }
        // D. Case any other char
        else{  cache_char();  }
    }
    // N. Return the vector of tokens
    return tokens;
}
    


////////// PARSING /////////////////////////////////////////////////////////////////////////////////

///// Predicates /////

bool p_float_string( string inputStr ){
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
   /* else assume string -------------*/ return make_string( token );
}


// Atom* consify_token_sequence( string[] tokens, ulong i ){
Atom* consify_token_sequence( string[] tokens, ulong i = 0 ){
    // Recursively render tokens as a cons structure
    string token;
    ulong  seqLen  = tokens.length;
    writeln( "Got a sequence of ", seqLen, " tokens, at index ", i, " ", (seqLen-i) > 1 ) ;
    Atom*  rtnTree = null;
    // 1. If there are one or more strings to process, then attempt to construct a token tree
    if( (seqLen-i) > 1 ){
        writeln( "There are ", seqLen-i, " tokens to process." );
        // 2. Start off by creating a cons list, if needed
        rtnTree = make_cons();
        // 3. For each token in the vector
        while( i < seqLen ){
            // 4. Fetch token at this index and update counter
            token = tokens[i];
            i += 1;
            // 5. Case Open Paren
            if( find_reserved( token ) == "open_parn" ){
                writeln( "Open Paren" );
                // If there is a new level of depth to the existing structure, recur
                if( i>1 ){  rtnTree = append(  rtnTree , consify_token_sequence( tokens, i )  );  }  
                // else this is the first level, no action
            }else{
                // 6. Case Close Paren, ascend one level
                if( find_reserved( token ) == "clos_parn" ){  return rtnTree;  }  
                // 7. Case Empty, is a list terminator
                else if( p_empty_atom_string( token ) ){  rtnTree = append( rtnTree , empty_atom() );  }  
                // 8. Case literal
                else{  rtnTree = append( rtnTree , atomize_string( token ) );  }
            }
        }
        // 9. Return the constructed tree
        return rtnTree;
    }else{ // 10. else there were no tokens, return Empty
        return empty_atom();
    }
}


Atom* expression_from_string( string expStr, dchar sepChar = ' ' ){
    // Tokenize the `expStr`, express it as a nested cons struct, and return
    string[] tokens = tokenize( expStr, sepChar );
    writeln( tokens );
    return consify_token_sequence( tokens );
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
    return (name in env.boundVars) !is null;
}


Atom* get_bound_atom( Env* env, string name ){
    // Return the atom bound to `name`, if it exists, Otherwise return an empty atom
    if( p_binding_exists( env, name ) ){  return env.boundVars[ name ];  }
    else{  return empty_atom();  }
}


Env* baseEnv;


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
    Atom*   currCons = strnList;
    Atom*[] rtnArr;
    while( !p_empty( currCons ) ){
        if( p_string( currCons.car ) ){  rtnArr ~= currCons.car;  }
        currCons = currCons.cdr;
    }
    return rtnArr;
}

///// D --to-> Scheme ////////////////////////////

struct Payload{ // https://forum.dlang.org/post/tcnpomjpodsveowbzgdd@forum.dlang.org
    // Container struct of interpreted data
    double[] dbbls;
    string[] strns;
    Atom*[]  atoms;
    // Probably others ...
}

///// Primitive Helpers /////

// FIXME: START HERE
// FIXME: all_atoms_same_type( Atom*[] atoms ){...}

///// Primitives /////

Atom* function( Payload )[string] primitives;
Payload function( Atom* )[string] gatherers;

void init_primitives(){
    primitives["true"]  = function Atom*( Payload args ){  return make_bool(true);   };
    primitives["#t"]    = function Atom*( Payload args ){  return make_bool(true);   };
    primitives["false"] = function Atom*( Payload args ){  return make_bool(false);  };
    primitives["#f"]    = function Atom*( Payload args ){  return make_bool(false);  };
    
    primitives["atom?"] = function Atom*( Payload args ){
        // Test if atom is a literal
        if( p_literal( args.atoms[0] ) ){  return make_bool(true);  }
        else{  return make_bool(false);  }
    };

    primitives["eq?"] = function Atom*( Payload args ){
        if( args.atoms.length > 1 ){
            F_Type typ0 = args.atoms
        }else{  return make_bool(false);  }
    }


}


////////// MAIN ////////////////////////////////////////////////////////////////////////////////////

void main(){
    // SPARROW Init //
    init_reserved();
    init_env();
    
    // Structure Tests //
    Atom* list1 = make_list_of_2( make_number(2), make_number(3) );
    writeln( str( list1 ) );
    append( list1, make_number(4) );
    append( list1, make_number(5) );
    writeln( str( list1 ) );
    prnt( first( list1 ) );
    prnt( second( list1 ) );
    prnt( third( list1 ) );

    // Math Tests //
    writeln( add1(2) ); // 3
    writeln( sub1(2) ); // 1
    writeln( add([2.0,3.0,4.0]) ); // 9
    writeln( minus([2,3,4]) ); // -5
    writeln( lt([2,3,4]) ); // true
    writeln( lt([2,3,3]) ); // false
    writeln( gt([4,3,2]) ); // true
    writeln( gt([4,3,3]) ); // false
    writeln( le([2,3,3]) ); // true
    writeln( le([2,3,1]) ); // false
    writeln( ge([4,3,3]) ); // true
    writeln( ge([4,3,5]) ); // false

    // Interpreter Tests //
    writeln( flatten_double_list( list1 ) ); // [2, 3, 4, 5]
    Atom* list2 = make_list_of_2( make_string( "foo" ), make_string( "bar" ) );
    append( list2, make_string( "baz" ) );
    append( list2, make_string( "xur" ) );
    append( list2, make_string( "tef" ) );
    writeln( flatten_string_list( list2 ) ); // ["foo", "bar", "baz", "xur", "tef"]


}