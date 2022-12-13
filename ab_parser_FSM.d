module ab_parser_FSM;

/* SPARROW, [S]cheme [P]rogram [A]llowing [R]easonable [R]eckoning [O]f [W]ork
   This is an outgrowth of an implementation of "Little Scheme".  
   The concept is to make small changes to it until it becomes my own language.
   This is for entertainment only and comes with no warrantee whatsoever.

   rdmd ab_parser_FSM.d
   dmd ab_parser_FSM.d -of=sparrow.app
   
   James Watson, 2022-11 */

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

////////// INIT //////////////////////////////////

///// Imports /////
import std.string; // ----------- `string` type
import std.stdio; // ------------ `writeln`
import std.conv; // ------------- string conversions
import std.uni; // -------------- `strip`
import std.math.operations; // -- `NaN`
import std.typecons; // ---------- Tuple
import std.ascii; // ------------- Whitespace test
import std.algorithm.searching; // `canFind`
import std.range.primitives; // `popBack`
// import std.file; // -------------- `readText`
alias  p_whitespace = std.ascii.isWhite; 

///// Env Vars /////
bool _DEBUG_VERBOSE  =  true; // Set true for debug prints
bool _TEST_ALL_PARTS =  true; // Set true to run all unit tests


////////// ATOMS /////////////////////////////////

enum F_Error{ 
    OKAY, // -- No error code applicable
    NOVALUE, // There is no value held in this atom
    NAN, // --- Not A Number
    DNE, // --- Does Not Exist
    SYNTAX, //- Syntax error
    LEXER, // - Tokenization failed
    PARSER, //- Translation failed
}


enum F_Type{ 
    CONS, // Cons pair
    STRN, // String/Symbol
    NMBR, // Number
    EROR, // Error object
    BOOL, // Boolean value
    FUNC, // Function
    BLOK, // Code block
}


struct Atom{
    F_Type  kind; // What kind of atom this is
    union{
        double  num; // NMBR: Number value
        string  str; // STRN: String value, D-string 
        bool    bul; // BOOL: Boolean value
        Atom*[] blk; // BLOK: A sequence of statements
        struct{ // ---- CONS: pair
            Atom* car; // Left  `Atom` Pointer
            Atom* cdr; // Right `Atom` Pointer
        }
        struct{ // ---- EROR: Code + Message
            F_Error err; // Error code
            string  msg; // Detailed desc
        }
    }
    // https://forum.dlang.org/post/omsbr8$7do$1@digitalmars.com
    this( double n ){ kind = F_Type.NMBR; num = n; } // make number
    this( string s ){ kind = F_Type.STRN; str = s; } // make string
    this( bool   b ){ kind = F_Type.BOOL; bul = b; } // make bool
    this( Atom* a, Atom* d ){ kind = F_Type.CONS; car = a; cdr = d; } // make cons
    this( Atom* a ){ kind = F_Type.CONS; car = a; cdr = null; } // make left-handed cons
    this( F_Error e, string m ){ kind = F_Type.EROR; err = e; msg = m; } // make error
    this( Atom*[] blockStatements ){ kind = F_Type.BLOK; blk = blockStatements; } // make block
}

///// Empty / Terminator /////

Atom* empty_atom(){
    // Allocate and return an a `NOVALUE` error
    return new Atom( F_Error.NOVALUE, "NO VALUE" );
}

Atom* empty_cons(){
    // Cons without contents
    Atom* rtnAtm = new Atom( empty_atom(), empty_atom() );
    rtnAtm.car   = null; 
    rtnAtm.cdr   = null;
    return rtnAtm;
}


///// Function /////

Atom* make_function( Atom* parameters = null, Atom* definition = null ){
    // Make a function
    Atom* rtnAtm = new Atom( parameters, definition );
    rtnAtm.kind = F_Type.FUNC;
    return rtnAtm;
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
    return (atm == null) || ((atm.kind == F_Type.EROR) && (atm.err == F_Error.NOVALUE));  
} 
bool p_has_error( Atom* atm ){  return (atm.kind == F_Type.EROR);  } // Atom has any code other than `OKAY`
bool p_cons( Atom* atm ){  return (atm.kind == F_Type.CONS);  } // ---- Return true if Atom is a pair
bool p_literal( Atom* atm ){  return (atm.kind == F_Type.NMBR) || (atm.kind == F_Type.STRN) || (atm.kind == F_Type.BOOL);  }
bool p_number( Atom* atm ){  return (atm.kind == F_Type.NMBR);  }
bool p_string( Atom* atm ){  return (atm.kind == F_Type.STRN);  }
bool p_bool( Atom* atm ){  return (atm.kind == F_Type.BOOL);  }
bool p_zero( Atom* atm ){  return (atm.kind == F_Type.NMBR) && (atm.num == 0.0);  }
bool p_block( Atom* atm ){  return (atm.kind == F_Type.BLOK);  }


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
    return new Atom( carAtm, empty_atom() );
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
    Atom* second = new Atom( atm2, empty_atom() );
    return new Atom( atm1, second );
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
                rtnStr = "( ERROR: " ~ item.err.to!string ~ ", " ~ item.str ~ " )";
                break;
            case F_Type.BLOK:
                rtnStr = "{<CODE BLOCK>}";
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
    RESERVED[";"] = "semicolon"; // Semicolon
    RESERVED["{"] = "open_crly"; // Open  curly brace
    RESERVED["}"] = "clos_crly"; // Close curly brace
}


string find_reserved( string token ){
    // Return the name of the reserved symbol, or an empty string if not found
    string* res = token in RESERVED;
    if( res !is null ){  return *res;  } // If key in dict, then return the string name of the reserved token
    else{  return "";  } // --------------- Else the search failed, return an empty string
}

bool p_open_paren( string token ){  return find_reserved( token ) == "open_parn";  } // Is an open  paren?
bool p_clos_paren( string token ){  return find_reserved( token ) == "clos_parn";  } // Is an close paren?
bool p_semicolon(  string token ){  return find_reserved( token ) == "semicolon";  } // Is a semicolon?
bool p_open_curly( string token ){  return find_reserved( token ) == "open_crly";  } // Is an open  curly?
bool p_clos_curly( string token ){  return find_reserved( token ) == "clos_crly";  } // Is an close curly?


string[] tokenize( string expStr, dchar sepChar = ' ' ){
    // Separate an expression string into tokens
    string[] tokens;
    dchar    c      = ' ';
    string   cStr;
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
        c    = ch_i;
        cStr = c.to!string();
        // 3. Either add char to present token or create a new one
        // A. Case Open Paren
        if ( p_open_paren( cStr ) || p_open_curly( cStr ) ){  stow_char();  }  
        // B. Case end of expression or list
        else if(  p_clos_paren( cStr )  ||  p_clos_curly( cStr )  ||  p_semicolon( cStr )  ){  
            if( token.length > 0 ){  stow_token();  }
            stow_char();  
        }
        // C. Case separator
        else if( (testWhite && p_whitespace(c)) || (c == sepChar)){
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

    primitiveSymbols["true"]  = function Atom*(){  return new Atom(true);   }; // Boolean True
    primitiveSymbols["#t"]    = function Atom*(){  return new Atom(true);   }; // Boolean True
    primitiveSymbols["false"] = function Atom*(){  return new Atom(false);  }; // Boolean False
    primitiveSymbols["#f"]    = function Atom*(){  return new Atom(false);  }; // Boolean False

    /// One Argument ///

    primitiveFunctions["atom?"] = function Atom*( Atom* args ){
        // Predicate: Is this atom a literal?
        // FIXME: CONVERT THIS FUNCTION TO HANDLE MANY ARGUMENTS WHERE ALL ATOMS TESTED
        if( p_literal( first( args ) ) ){  return new Atom(true);  }
        else{  return new Atom(false);  }
    };

    /// Many Arguments ///

    primitiveFunctions["eq?"] = function Atom*( Atom* args ){
        // Predicate: Are these atoms of the same type and value?
        Atom*[] atoms = flatten_atom_list( args );
        if( atoms.length > 1 ){
            F_Type typ0 = atoms[0].kind;
            foreach(Atom* atm; atoms[1..$]){  if(atm.kind != typ0){  return new Atom(false);  }  }
            // NOTE: WOULD BE NICE TO USE A `Variant` HERE? (loops) (Algebraic?)
            switch( typ0 ){
                case F_Type.STRN:
                    string val0 = atoms[0].str;
                    foreach(Atom* atm; atoms[1..$]){  if(atm.str != val0){  return new Atom(false);  }  }
                    break;
                case F_Type.NMBR:
                    double val0 = atoms[0].num;
                    foreach(Atom* atm; atoms[1..$]){  if(atm.num != val0){  return new Atom(false);  }  }
                    break;
                case F_Type.BOOL:
                    bool val0 = atoms[0].bul;
                    foreach(Atom* atm; atoms[1..$]){  if(atm.bul != val0){  return new Atom(false);  }  }
                    break;
                default: 
                    return new Atom(false);
            }
            return new Atom(true);       
        }else{  return new Atom(false);  }
    };


    primitiveFunctions["empty?"] = function Atom*( Atom* args ){
        // Predicate: Is this an empty atom?
        Atom*[] atoms = flatten_atom_list( args );
        foreach(Atom* atm; atoms){  if( !p_empty( atm ) ){  return new Atom(false);  }  }
        return new Atom(true);
    };


    primitiveFunctions["zero?"] = function Atom*( Atom* args ){
        // Predicate: Is this a number atom wtih a zero value?
        Atom*[] atoms = flatten_atom_list( args );
        foreach(Atom* atm; atoms){  if( !p_zero( atm ) ){  return new Atom(false);  }  }
        return new Atom(true);
    };


    primitiveFunctions["number?"] = function Atom*( Atom* args ){
        // Predicate: Is this atom of number type?
        Atom*[] atoms = flatten_atom_list( args );
        foreach(Atom* atm; atoms){  if( !p_zero( atm ) ){  return new Atom(false);  }  }
        return new Atom(true);
    };


    primitiveFunctions["+"] = function Atom*( Atom* args ){
        // Add 1 or more number atoms
        double[] ops = flatten_double_list( args );
        return new Atom( add( ops ) );
    };


    primitiveFunctions["-"] = function Atom*( Atom* args ){
        // Negate 1 or subtract more number atoms
        double[] ops = flatten_double_list( args );
        return new Atom( minus( ops ) );
    };


    primitiveFunctions["*"] = function Atom*( Atom* args ){
        // Multiply 1 or more number atoms
        double[] ops = flatten_double_list( args );
        return new Atom( multiply( ops ) );
    };


    primitiveFunctions["/"] = function Atom*( Atom* args ){
        // Inverse 1 or divide more number atoms
        double[] ops = flatten_double_list( args );
        return new Atom( divide( ops ) );
    };


    primitiveFunctions["1+"] = function Atom*( Atom* args ){
        // Increment 1 or more number atoms by 1
        Atom*[] ops = flatten_atom_list( args );
        Atom* rtnLst;
        if( ops.length > 1 ){
            rtnLst = empty_cons();
            foreach(Atom* atm; ops){  if( p_number( atm ) ){  rtnLst = append( rtnLst, new Atom( add1( atm.num ) ) );  }  }
        }else if( ops.length == 1 )
            rtnLst = new Atom( add1( ops[0].num ) );
        return rtnLst;
    };


    primitiveFunctions["1-"] = function Atom*( Atom* args ){
        // Decrement 1 or more number atoms by 1
        Atom*[] ops = flatten_atom_list( args );
        Atom* rtnLst;
        if( ops.length > 1 ){
            rtnLst = empty_cons();
            foreach(Atom* atm; ops){  if( p_number( atm ) ){  rtnLst = append( rtnLst, new Atom( sub1( atm.num ) ) );  }  }
        }else if( ops.length == 1 )
            rtnLst = new Atom( sub1( ops[0].num ) );
        return rtnLst;
    };


    primitiveFunctions["<"] = function Atom*( Atom* args ){
        // Less Than, for 2 or more Number atoms
        double[] ops = flatten_double_list( args );
        return new Atom( lt( ops ) );
    };


    primitiveFunctions[">"] = function Atom*( Atom* args ){
        // Greater Than, for 2 or more Number atoms
        double[] ops = flatten_double_list( args );
        return new Atom( gt( ops ) );
    };


    primitiveFunctions["<="] = function Atom*( Atom* args ){
        // Less Than Or Equal To, for 2 or more Number atoms
        double[] ops = flatten_double_list( args );
        return new Atom( le( ops ) );
    };


    primitiveFunctions[">="] = function Atom*( Atom* args ){
        // Greater Than Or Equal To, for 2 or more Number atoms
        double[] ops = flatten_double_list( args );
        return new Atom( ge( ops ) );
    };


    primitiveFunctions["cons"] = function Atom*( Atom* args ){
        // Cons up to 2 atoms together into a pair. Missing params filled with `empty`
        Atom*[] atoms = flatten_atom_list( args );
        if( atoms.length >= 2 ) return new Atom( atoms[0], atoms[1] ); // Only takes 1st two args
        if( atoms.length == 1 ) return new Atom( atoms[0] ); // --------- One arg is accepted, other empty
        else /*--------------*/ return empty_cons(); // ----------------- Two empty accepted
    };
}



////////// PARSING /////////////////////////////////////////////////////////////////////////////////

///// Predicates /////

bool p_float_string( string inputStr ){
    // Predicate: Is this string suitable to be converted to a (double) number?
    string slimStr = strip( inputStr );
    try{
        slimStr.to!double();
        if(_DEBUG_VERBOSE){  writeln( "Converted \"", slimStr, "\" to a number" );  }
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
    if( p_float_string( token ) ){       return new Atom( token.to!double() );  }  
    if( p_empty_atom_string( token ) ){  return empty_atom();  }  
    if( p_primitve_symbol( token ) ){    return primitiveSymbols[ token ]();  }
   /* else assume string -------------*/ return new Atom( token );
}


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


bool p_balanced_curlies( string[] tokens ){
    // Return true if parens open and close in a quantity and order that returns to root depth, Otherwise return false
    int depth = 0;
    foreach( string token; tokens ){
        if( p_open_curly( token ) ) depth++; 
        else
        if( p_clos_curly( token ) ) depth--;
    }
    return (depth == 0);
}


bool p_bounded_parens( string[] tokens ){
    // Return true if the token sequence begins and ends with open and close parens, respectively
    if( p_open_paren( tokens[0] ) ){
        if( p_clos_paren( tokens[$-1] ) )  return true;  else  return false;
    }else return false;
}


bool p_bounded_curlies( string[] tokens ){
    // Return true if the token sequence begins and ends with open and close curly braces, respectively
    if( p_open_curly( tokens[0] ) ){
        if( p_clos_curly( tokens[$-1] ) )  return true;  else  return false;
    }else return false;
}


bool p_parent_parens( string[] tokens ){
    // Return true if the outermost parens define a single root list, possibly with nested lists
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


bool p_parent_curlies( string[] tokens ){
    // Return true if the outermost curlies define a single root block, possibly with nested blocks
    int   depth  = 0;
    ulong i      = 0;
    ulong seqLen = tokens.length;
    if( p_bounded_curlies( tokens ) ){
        foreach( string token; tokens ){
            i++;
            if( p_open_curly( token ) ) depth++; 
            else
            if( p_clos_curly( token ) ) depth--;
            if( (depth == 0) && (i<seqLen) ) return false;
        }
        return true;
    }else return false;
}


bool p_parent_semi( string[] tokens ){
    // Return true if the outermost level of the statement defines a list punctuated with a semicolon
    ulong seqLen = tokens.length;
    if( seqLen > 1 ){
        if(  p_semicolon( tokens[$-1] )  &&  !p_semicolon( tokens[0] )  ){
            foreach( string token; tokens[1..$-2] ){
                if( p_semicolon( token ) )  return false;
            }
            return true;
        }else  return false;
    }else  return false;
}


Atom* consify_token_sequence( string[] tokens ){
    // Recursively render tokens as a cons structure
    // 2022-11-XX: Rewritten
    // 2022-11-30: EZ List CANNOT begin with a LISP list! AMBIGUOUS!, Acceptable bug until LISP lists abandoned
    ulong    seqLen  = tokens.length;
    ulong    bgn;
    ulong    end;
    int /**/ depth   = 0;
    ulong    index;
    string   token;
    string[] carPart;
    Atom*    lstRoot = null;
    Atom*[]  blockStatements;

    if( _DEBUG_VERBOSE ){
        writeln("`consify_token_sequence`");
        writeln( "\tInput: " ~ tokens );
    }  

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
            
            // Are we building a block?
            if(  p_parent_curlies( tokens )  ){
                // WARNING: This will also scoop up the last INCOMPLETE statement in the block!

                blockStatements = [];
                bgn++;
                end--;

                // Begin block
                index = bgn;

                // While we are in the block bounds, find and append statements
                while( index <= end ){

                    // Find an statement
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

                    // else scan for an EZ list
                    }else{
                        do{
                            token = tokens[index];
                            carPart ~= token;
                            index++;
                            if( p_semicolon( token ) )  break;
                        }while( (index <= end) );
                    }

                    // Collect statement
                    blockStatements ~= consify_token_sequence( carPart );
                }

                // Return a block atom
                return new Atom( blockStatements );

            // Are we building a list?
            }else if(  p_parent_parens( tokens )  ||  p_parent_semi( tokens )  ){
                end--;
                if( p_parent_parens( tokens ) )  bgn++;
                
                // Begin list
                index   = bgn;
                lstRoot = empty_cons();
                if( _DEBUG_VERBOSE ){
                    writeln( "\tInput: " ~ tokens.to!string );
                    writeln( "\tRoot: " ~ lstRoot.kind.to!string ~ ", " ~ str( lstRoot ) );
                }  

                // While we are in the list bounds, find and append items
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

                    // else element is an atom
                    }else{

                        // index = lastDex;
                        carPart ~= tokens[index];
                        index++;
                    }

                    if( _DEBUG_VERBOSE ){
                        writeln( "\tappend list elem: " ~ carPart.to!string );
                    }

                    // Append element to list
                    lstRoot = append( 
                        lstRoot,
                        consify_token_sequence( carPart )
                    );
                }
                return lstRoot;
            }else{
                return new Atom( F_Error.SYNTAX, "BAD LIST CASE" );
            }
        }else{
            return new Atom( F_Error.SYNTAX, "PARENTHESES MISMATCH" );
        }
    }
    return new Atom( F_Error.PARSER, "PARSER FAILED, This branch was ?UNREACHABLE?" );
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


    specialForms["print"] = function ExprInContext( ExprInContext eINc ){  
        // Passthru for expression args 
        ExprInContext res = meaning( ExprInContext(
            textOf( eINc.expr ), 
            // get_cdr( eINc.expr ),
            eINc.context,
            "print"
        ) );
        prnt( res.expr );
        return res;
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
                    new Atom( false ), // Truthiness of the element
                    eINc.context, //- Original context
                    "truthiness" // ---- Tag
                );
            }
        }

        return ExprInContext(
            new Atom( true ), // Truthiness of the element
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
                    new Atom( true ), // Truthiness of the element
                    eINc.context, //- Original context
                    "truthiness" // ---- Tag
                );
            }
        }

        return ExprInContext(
            new Atom( false ), // Truthiness of the element
            eINc.context, // Original context
            "truthiness" // --- Tag
        );
    };


    specialForms["for"] = function ExprInContext( ExprInContext eINc ){
        // Execute a `for` loop, Loop meaning is the last statement of the last iteration, 
        // Default is to increment up by one, inclusive bounds

        // 1. Parse loop args
        Atom*[] loopArgs  = flatten_atom_list( second( eINc.expr ) ); // Fetch args as atom vector
        string  iVarName  = loopArgs[0].str; // ------------------------ Get the counter var name, WARNING: TYPE NOT CHECKED
        bool    incrByOne = (loopArgs.length == 3); // ----------------- Default is to increment up by one, inclusive bounds
        double  loBound   = 0.0;
        double  hiBound   = 0.0;
        double  incr      = 1.0;
        double  i /*---*/ = 0.0;
        Atom*   loopProg  = third( eINc.expr ); // WARNING: TYPE NOT CHECKED
        Atom*   rtnExpr   = null; 

        // Case: Default loop increments by 1.0
        if( incrByOne ){
            loBound = loopArgs[1].num;
            hiBound = loopArgs[2].num;

        // Case: User-specified increment
        }else if(loopArgs.length == 4){
            loBound = loopArgs[1].num;
            incr    = loopArgs[2].num;
            hiBound = loopArgs[3].num;

        // Else: There is a syntax error
        }else  return ExprInContext( 
            new Atom( F_Error.SYNTAX, loopArgs.length.to!string ~ " was an incorrect number to loop args. Expected 3 or 4." ),
            eINc.context,
            "`for` got an unexpected number of args"
        );

        // 2. Create a new nested context, bind the counter var
        Env* nuEnv   = new Env();
        nuEnv.parent = eINc.context;
        i /*------*/ = loBound;
        bind_atom( nuEnv, iVarName, new Atom( loBound ) );

        // 3. LOOP: If loop condition met, run block in nested context && increment, otherwise exit loop
        while( i <= hiBound ){
            // run block in nested context, Loop meaning is the last statement of the last iteration
            rtnExpr = block_meaning( ExprInContext (
                loopProg,
                nuEnv,
                "loop body"
            ) ).expr;
            i += incr; // increment
            bind_atom( nuEnv, iVarName, new Atom( i ) ); // Store new counter value so that loop body can access it
        }

        return ExprInContext( 
            rtnExpr,
            eINc.context,
            "loop result"
        );

    };

    // FIXME: LOAD FILE, A WAY TO IMPORT FILES WITHIN EITHER FILES OR REPL
    // specialForms["load"] = function ExprInContext( ExprInContext eINc ){ /* LOAD FILE */ } 



}



////////// EVALUATION //////////////////////////////////////////////////////////////////////////////
// This is the real living mechanism of the SPARROW interpreted language
// 2022-09-13: `atomize_string` will fetch primitive symbols, these were together w/ primitve functions in Little JS

bool p_eq( Atom* op1, Atom* op2 ){  return primitiveFunctions["eq?"]( make_list_of_2( op1, op2 ) ).bul;  }
bool p_else( Atom* x ){  return p_literal( x ) && p_eq( x, new Atom( "else" ) );  } // is the arg an 'else symbol?


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
        new Atom( false ),
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
            new Atom(
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
        new Atom( F_Error.NOVALUE, "There is no function with the name " ~ nameOf( input.expr ).str ),
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

    Atom* /*---*/ e /*-*/  = eINc.expr;
    if( _DEBUG_VERBOSE ){
        writeln( "`meaning`" );
        writeln( "\t" ~ str(e) );
        writeln( "\t" ~ str(get_cdr( e )) );
    } 
    Atom* /*---*/ balance  = get_cdr( e );
    Atom* /*---*/ rtnRoot  = null;
    Atom* /*---*/ inputPtr = null;
    string /*--*/ name     = "";
    Env* /*----*/ nuEnv    = null;
    ExprInContext rntResult;

    if( p_string( e ) ){  name = e.str;  }

    if( _DEBUG_VERBOSE )  writeln( "\t`meaning`" ~ ": init done" );

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

        // Fetch name for some cases, if it exists
        if( p_cons( e ) && p_string( get_car( e ) ) ){  name = get_car( e ).str;  }

        // Case Code Block
        if( p_block( e ) ){

            // Create a nested context
            nuEnv = new Env();
            nuEnv.parent = eINc.context;

            // Pass that context to meaning && Return the meaning of the last statement in the block
            rntResult = block_meaning( ExprInContext(
                e,
                nuEnv,
                "block meaning"
            ) );
        
        // Case Primitive Function
        }else if( p_primitve_function( name ) ){

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
            rtnRoot  = empty_cons();

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
    writeln( "\n##### SPARROW Interpreter, Version 2022.11.0 #####" );
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



////////// FILE OPERATIONS /////////////////////////////////////////////////////////////////////////


bool p_complete_expression( string[] sExpr ){
    // Return true if the tokenized statement has balanced parens / curly braces, and is terminated properly
    if( p_balanced_parens( sExpr ) && p_balanced_curlies( sExpr ) ){
        if( p_parent_semi( sExpr ) ) // FIXME: If this is not true there could be multiple s-expr on a line!
            return true;
        else if( p_parent_parens( sExpr ) )
            return true;
        else
            return false;
    }else  return false;
}


string[][] lex_many_one_line( string[] tokens ){
    // If there are multiple statements on the same line, then lex and return, Otherwise return empty
    string[][] statements;
    string[]   sttmnt;
    ulong /**/ seqLen = tokens.length;
    ulong /**/ index  = 0;
    uint /*-*/ depth  = 0;
    string     token;

    while( index < seqLen ){
        // token  = tokens[ index ];
        sttmnt = [];
        depth  = 0;
        if( p_open_paren( token ) ){
            do{
                token = tokens[ index ];
                if( p_open_paren( token ) ) depth++; 
                else
                if( p_clos_paren( token ) ) depth--;
                sttmnt ~= token;
                index++;
            }while( (depth > 0) && (index < seqLen) );
        }else{
            do{
                token = tokens[ index ];
                sttmnt ~= token;
                index++;
                if( p_semicolon( token ) )  break;
            }while( index < seqLen );
        }
        statements ~= sttmnt; // Can be complete, partial, or empty; checked elsewhere
    }
    return statements;
}

// string[] tokenize_file( string fName ){
//     string   tExpr;
//     string[] sExpr;
//     File     f = File( fName );
//     foreach( line; f.byLine ){
//         tExpr = line.to!string; // Fetch text expression
//         if( _DEBUG_VERBOSE )  writeln( "\tLex line: " ~ tExpr );
//         if( strip( tExpr ).length == 0 )  continue; // If newline only, there is nothing to do
//         sExpr ~= tokenize( tExpr );
//     }
//     return sExpr;
// }

// string[][] lex_file( string fName ){
//     // Read the contents of the file and lex into serial statements    
//     string     tExpr;
    
//     string[][] statememts;
//     string[][] lineContents;
//     ulong /**/ seqLen;
//     ulong /**/ index;
//     File /*-*/ f = File( fName );

//     if( _DEBUG_VERBOSE )  writeln( "`lex_file`" );

//     foreach( line; f.byLine ){
//         tExpr = line.to!string; // Fetch text expression

//         if( _DEBUG_VERBOSE )  writeln( "\tLex line: " ~ tExpr );

//         if( strip( tExpr ).length == 0 )  continue; // If newline only, there is nothing to do

//         sExpr ~= tokenize( tExpr ); // Text Expression --to-> S-Expression

//         // Make curlies their own "statements", see `parse_serial_statements`
//         if( p_open_curly( sExpr[0] ) || p_clos_curly( sExpr[0] ) ){
//             statememts ~= [ sExpr[0], ];
//             if( sExpr.length > 1 ) 
//                 sExpr = sExpr[1..$-1];
//             else     
//                 sExpr = [];
//         }
//         if( sExpr.length == 0 )  continue; // If we processed a lone curly, there is nothing else to do

//         // If the line is one complete statement on its own
//         if( p_complete_expression( sExpr ) ){
//             statememts ~= sExpr;
//             sExpr = [];
//         // Else check for multiple statements on the line
//         }else{
//             if( _DEBUG_VERBOSE )  writeln( "\tLex incomplete line: " ~ sExpr.to!string );
//             lineContents = lex_many_one_line( sExpr );
//             seqLen /*-*/ = lineContents.length;
//             index /*--*/ = 0;
//             while( index < seqLen ){
//                 // WARNING: Assume statements before the last are complete ;P
//                 if( index < (seqLen-1) )  
//                     statememts ~= lineContents[ index ];
//                 // Assume a non-empty last statement is partial
//                 else if( lineContents[ index ].length > 0 )  
//                     sExpr = lineContents[ index ];
//                 index++;
//             }
//         }
//     }
//     if( sExpr.length > 0 )  statememts ~= sExpr;
//     f.close();
//     return statememts;
// }


Atom*[] parse_serial_statements( string[][] statememts ){
    // Parse a block of tokenized string code into serial executable statements
    Atom*[]  program;
    ulong    seqLen = statememts.length;
    ulong    index  = 0;
    string[] sttmnt;
    Atom*[]  blockProg;

    if( _DEBUG_VERBOSE )  writeln( "`parse_serial_statements`" );

    while( index < seqLen ){

        sttmnt = statememts[ index ];
        if( _DEBUG_VERBOSE )  writeln( "\tSerial Statement: " ~ sttmnt.to!string );

        // Case: Standalone blocks
        // 2022-12-02: Lexing blocks that belong to loops will require changes to how complete statements are detected
        if( (sttmnt.length == 1) && p_open_curly( sttmnt[0] ) ){
            blockProg = [];
            index++;
            do{
                sttmnt = statememts[ index ];
                if( _DEBUG_VERBOSE )  writeln( "\tBlock Statement: " ~ sttmnt.to!string );
                if( (sttmnt.length == 1) && p_clos_curly( sttmnt[0] ) ){
                    // index++; // 2022-12-05: There were too many increments!
                    break;
                }  
                blockProg ~= consify_token_sequence( sttmnt );
                index++; // 2022-12-05: Forgot to advance the counter!
            }while( index < seqLen );
            if( blockProg.length > 0 )  program ~= new Atom( blockProg );
            // Reset for normal line by line lexing
            index++;
            sttmnt = statememts[ index ];
        }
        program ~= consify_token_sequence( sttmnt );
        index++;
    }
    return program;
}


// Parse an entire file into AST conses
// Atom*[] parse_file( string fName ){  return parse_serial_statements( lex_file( fName ) );  }


ExprInContext block_meaning( ExprInContext block ){
    Atom*[] statememts = block.expr.blk;
    ExprInContext lastResult;
    foreach( Atom* sttmnt; statememts ){
        lastResult = meaning(
            ExprInContext(
                sttmnt, // ------ Expression to be evaluated
                block.context, // Given context
                str( sttmnt ) //- String representation of the original expression
            )
        );
    }
    return lastResult;
}


// Atom* run_file( string fName ){
//     // Execute every statement in a file and return the result of the last expression
//     Atom*[] code = parse_file( fName );
//     return block_meaning(
//         ExprInContext(
//             new Atom( code ), // ----- Expression to be evaluated
//             baseEnv, // -------- Global context
//             fName // String representation of the original expression
//         )
//     ).expr;
// }

// FIXME: UPON COMPLETION DELETE THE FOLLOWING FUNCTIONS: 
//        `tokenize_file` (old), `lex_many_one_line`, `lex_file`,
string[] tokenize_file( string fName ){
    string   tExpr;
    string[] sExpr;
    File     f = File( fName );

    if( _DEBUG_VERBOSE )  writeln( "`tokenize_file`" );

    foreach( line; f.byLine ){
        tExpr = line.to!string; // Fetch text expression
        if( _DEBUG_VERBOSE )  writeln( "\tLex line: " ~ tExpr );
        if( strip( tExpr ).length == 0 )  continue; // If newline only, there is nothing to do
        sExpr ~= tokenize( tExpr );
    }
    return sExpr;
}

// bool 

enum F_Parser{ 
    // Possible States of Parser Jobs
    RUN, // ------------- Beginning state
    ONE_ATOM, // -------- There was one atom or less in the input
    CONSUME_STATEMENT, // Read one statement into the statement register
    CONSUME_BLOCK, // --- Read one block into the block register
    PARSE_STATEMENT, // - Parse statement into an executable AST
    PARSE_BLOCK, // ----- Parse block into a list of executable ASTs
    SUCCESS, // --------- Exit this job with success
    FAULT, // ----------- Exit this job with failure
    NO_JOB // ----------- NO-OP, nothing to do
}

struct ParserJobStack{
    // Global parser job stack
    
    // Members //
    F_Parser[] jobs; // All current parsing jobs
    
    // Methods //
    
    // this(){  jobs = [];   } // Constructor
    
    bool p_has_job(){  return (jobs.length > 0);  } //- Are there are one or more jobs?
    bool p_has_prev(){  return (jobs.length > 1);  } // Is the job depth at least 2?
    bool p_has_three(){  return (jobs.length > 2);  } // Is the job depth at least 3?
    void push( F_Parser job ){  jobs ~= job;  } // ---- Add a job to the stack top
    
    F_Parser pop(){  
        // Remove job from stack top and return it
        F_Parser rtnJob = get_current();
        if( p_has_job() )
            jobs.popBack();  
        return rtnJob;
    } 

    F_Parser get_prev(){  
        // Return status of the previous job without removing it
        if( p_has_prev() )
            return jobs[$-2];  
        else 
            return F_Parser.NO_JOB;
    }

    F_Parser get_current(){  
        // Return status of the current job without removing it
        if( p_has_job() )
            return jobs[$-1];  
        else 
            return F_Parser.NO_JOB;
        
    }

    void set_current( F_Parser job ){  
        // Set status of the current job without adding it
        if( p_has_job() )  jobs[$-1] = job;  
    } 
    
}

Atom* parse_one_statement( string[] tokens ){
    // Return to simpler days of Scheme parsing
    ulong    seqLen  = tokens.length;
    ulong    bgn     = 0;
    ulong    end     = seqLen-1;
    ulong    index   = 0;
    Atom*    lstRoot = null;
    string[] carPart;
    string   token;
    uint     depth = 0;
    
    // Are the parens correct?
    if( p_balanced_parens( tokens ) ){

        // Are we building a list?
        if(  p_parent_parens( tokens )  ||  p_parent_semi( tokens )  ){
            end--;
            if( p_parent_parens( tokens ) )  bgn++;
            
            // Begin list
            index   = bgn;
            lstRoot = empty_cons();
            if( _DEBUG_VERBOSE ){
                writeln( "\tInput: " ~ tokens.to!string );
                writeln( "\tRoot: " ~ lstRoot.kind.to!string ~ ", " ~ str( lstRoot ) );
            }  

            // While we are in the list bounds, find and append items
            while( index <= end ){
                
                // Find an element
                carPart = [];
                depth   = 0;
                
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

                // else element is an atom
                }else{

                    // index = lastDex;
                    carPart ~= tokens[index];
                    index++;
                }

                if( _DEBUG_VERBOSE ){
                    writeln( "\tappend list elem: " ~ carPart.to!string );
                }

                // Append element to list
                lstRoot = append( 
                    lstRoot,
                    parse_one_statement( carPart )
                );
            }
        }
    }else{
        return new Atom( F_Error.SYNTAX, "PARENTHESES MISMATCH" );
    }
    return lstRoot;
}

// Global parser job stack
ParserJobStack parserJobs = ParserJobStack();

// FIXME: UPON COMPLETION DELETE THE FOLLOWING FUNCTIONS: 
//        `consify_token_sequence`,  `parse_serial_statements`, `parse_serial_statements`
Atom* parse_token_sequence( string[] tokens ){
    // Init
    F_Parser state; // ------------------- Current parser state
    F_Parser prevState; // ------------------- Current parser state
    bool     parsing    = true; // -------- Is the parser at this depth running?
    Atom*[]  progBlock  = []; // -------- Block for this depth
    string[] sttmntReg  = []; // ---------- Statement "register"
    string[] progTokens = []; // ---------- Statement "register" for a loop body or other block
    string   faultMsg;
    Atom*    blockReg  = null;
    Atom*    loopReg   = null;
    ulong    seqLen    = tokens.length; // Input length
    ulong    regLen    = 0; // ----------- Input length
    // ulong    bgn       = 0; // ----------- Input begin index
    // ulong    end       = 0; // ----------- Input end   index
    ulong    index     = 0; // ----------- Index of current input token
    uint     depth     = 0;
    string   token; // ------------------- Current input token
    Atom*    rtnProg = null; // ----------------- Output returned at this depth

    parserJobs.push( F_Parser.RUN ); // Parser always begins in the `RUN` state

    if( _DEBUG_VERBOSE )  writeln( "`parse_token_sequence`" );

    ///// Parser Loop ////////////////////////////
    do{

        state = parserJobs.get_current(); // Model is to keep job on the stack until it is done

        // Handle state
        switch( state ){
            
            /// Case RUN: Decide on the next mode of the parser ///////////
            case F_Parser.RUN:
                if( _DEBUG_VERBOSE )  writeln( "\tCase RUN: Decide on the next mode of the parser" );
                
                // If there are one or less tokens, we can only return one atom
                if( seqLen < 2 )
                    parserJobs.set_current( F_Parser.ONE_ATOM );
                // Else there are many, check if we need to consume a block or a statement
                else{
                    token = tokens[ index ]; // Fetch token
                    if( p_open_curly( token ) ) // Open curly: Consume block

                        // parserJobs.set_current( F_Parser.CONSUME_BLOCK );
                        parserJobs.push( F_Parser.CONSUME_BLOCK ); // Push so that we can peek block context

                    else // else is an s-expression or an EZ list
                        parserJobs.set_current( F_Parser.CONSUME_STATEMENT );
                    break;
                }
                break;
                    

            /// Case ONE_ATOM: Parse one or empty ////////////////////
            case F_Parser.ONE_ATOM:    
                if( _DEBUG_VERBOSE )  writeln( "\tCase ONE_ATOM: Parse one or empty" );

                // Base Case: There were no tokens, return Empty
                if( seqLen == 0 ){  progBlock ~= empty_atom();  }
                // Base Case: There was one token, Return it
                if( seqLen == 1 ){  progBlock ~= atomize_string( tokens[0] );  }
                // Flag success
                parserJobs.set_current( F_Parser.SUCCESS );
                break;


            /// Case CONSUME_STATEMENT: Load statement ////////////////////
            case F_Parser.CONSUME_STATEMENT:
                if( _DEBUG_VERBOSE )  writeln( "\tCONSUME_STATEMENT: Load statement" );

                sttmntReg = [];
                token     = tokens[ index ];

                // If element is a list then scan it into the statement register
                if( p_open_paren( tokens[index] ) ){
                    if( _DEBUG_VERBOSE )  writeln( "\tScanning s-expression ..." );
                    do{
                        token = tokens[index];
                        if( p_open_paren( token ) ) depth++; 
                        else
                        if( p_clos_paren( token ) ) depth--;
                        sttmntReg ~= token;
                        index++;
                    }while( (depth > 0) && (index <= seqLen) );

                // else scan in an EZ list
                }else{
                    if( _DEBUG_VERBOSE )  writeln( "\tScanning EZ list ..." );
                    do{
                        if( _DEBUG_VERBOSE )  writeln( "\t`index` == " ~ index.to!string );

                        token = tokens[index];
                        sttmntReg ~= token;
                        index++;
                        if( p_semicolon( token ) ){
                            if( _DEBUG_VERBOSE )  writeln( "\tSemicolon!: " ~ token );
                            break;
                        }  
                    }while( (index <= seqLen) );
                }

                if( _DEBUG_VERBOSE )  writeln( "\tStatement Tokens: " ~ sttmntReg.to!string );


                // Flag statement register for parsing
                parserJobs.set_current( F_Parser.PARSE_STATEMENT );
                break;


            /// Case CONSUME_LOOP_BODY: Load block statements /////////////////
            /// Case CONSUME_BLOCK: Load block statements /////////////////
            case F_Parser.CONSUME_BLOCK:
                if( _DEBUG_VERBOSE )  writeln( "\tCase CONSUME_BLOCK: Load block statements" );

                sttmntReg = [];
                index++;
                depth = 1;
                do{
                    token = tokens[ index ];
                    if( p_open_curly( token ) )
                        depth++;
                    else if( p_clos_curly( token ) ){
                        depth--;
                        if(depth == 0) break;
                    }
                    sttmntReg ~= token;
                    index++;
                }while( index < seqLen );
                parserJobs.set_current( F_Parser.PARSE_BLOCK );
                break;


            /// Case PARSE_STATEMENT: Parse loaded statement //////////////
            case F_Parser.PARSE_STATEMENT:
                if( _DEBUG_VERBOSE )  writeln( "\tCase PARSE_STATEMENT: Parse loaded statement" );
                progBlock ~= parse_one_statement( sttmntReg );
                parserJobs.set_current( F_Parser.RUN );
                break;            


            /// Case PARSE_BLOCK: Recur on block //////////////////////////
            case F_Parser.PARSE_BLOCK:
                // Descend one depth and parse the consumed block
                if( _DEBUG_VERBOSE )  writeln( "\tCase PARSE_BLOCK: Recur on block" );

                blockReg = parse_token_sequence( sttmntReg );
                // Ascended one depth, now put the job in context
                // prevState = ;
                
                switch( parserJobs.get_prev() ){
                    
                    case F_Parser.RUN:
                        progBlock ~= blockReg;
                        break;
                    
                    case F_Parser.NO_JOB:
                        faultMsg = "A code block was built without context";
                        parserJobs.push( F_Parser.FAULT );
                        break;

                    default:
                        faultMsg = "IMPOSSIBLE CODE BLOCK CASE";
                        parserJobs.push( F_Parser.FAULT );
                        break;
                }
                break;


            /// Case SUCCESS: End of input with no errors /////////////////
            case F_Parser.SUCCESS:

                if( _DEBUG_VERBOSE ){
                    writeln( "\tCase SUCCESS: End of input with no errors" );
                    writeln( "\tSUCCESS, result: " ~ str( rtnProg ) );
                }
                state   = parserJobs.pop();
                parsing = false;
                break;
            

            /// Case FAULT: The input has caused problems /////////////////
            case F_Parser.FAULT:

                if( _DEBUG_VERBOSE ){
                    writeln( "\tCase FAULT: The input has caused problems" );

                    writeln( "\tFAULT, result: " ~ str( rtnProg ) );
                }
                state   = parserJobs.pop();
                parsing = false;
                rtnProg = new Atom( F_Error.PARSER, "Parser FAULT: " ~ faultMsg );
                break;


            /// Case NO_JOB: Input exhausted //////////////////////////////
            case F_Parser.NO_JOB:

                if( _DEBUG_VERBOSE ){
                    writeln( "\tCase NO_JOB: Input exhausted" );

                    writeln( "\tNO_JOB, parser job stack is EMPTY!" );
                }
                parsing = false;
                break;
            
            
            /// Case BAD: SOMETHING UNEXPECTED HAPPENED ///////////////////
            default: 
                state = parserJobs.pop();
                if( _DEBUG_VERBOSE ){
                    writeln( "\tCase BAD: SOMETHING UNEXPECTED HAPPENED" );

                    writeln( "\tdefault, SOMETHING UNEXPECTED HAPPENED!, Final State: " ~ state.to!string );
                }
                rtnProg = new Atom( F_Error.PARSER, "IMPOSSIBLE PARSER CASE ENCOUNTERED" );
                break;
        }
    }while( parsing && (index < seqLen) );
    
    if( _DEBUG_VERBOSE ){
        writeln( "\tParser EXIT!, Final State: " ~ state.to!string );
        writeln( "\tThere are " ~ progBlock.length.to!string ~ " statements in program at this level." );
        int forDex = 1;
        foreach( Atom* atm; progBlock ){
            write( "\t\t" ~ forDex.to!string ~ "/" ~ progBlock.length.to!string  ~ ", " );
            writeln( str( atm ) ); 
            forDex++;
        }
    }

    if( (rtnProg == null) && (progBlock.length > 0) ){
        if( _DEBUG_VERBOSE )  writeln( "\tReturn program" );
        rtnProg = new Atom( progBlock );
    }
        
    else if( _DEBUG_VERBOSE )
        writeln( "\tNO PROGRAM STORED" );

    return rtnProg;
}


Atom* run_file( string fName ){
    // Execute every statement in a file and return the result of the last expression
    string[] allTokens = tokenize_file( fName );
    Atom*    code = parse_token_sequence( allTokens );
    return block_meaning(
        ExprInContext(
            code, // ----- Expression to be evaluated
            baseEnv, // -------- Global context
            fName // String representation of the original expression
        )
    ).expr;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN //
////////////////////////////////////////////////////////////////////////////////////////////////////

void main( string[] args ){
    Atom* res = null;

    if( _DEBUG_VERBOSE )  writeln( "Args are: " ~ args.to!string );

    // Populate necessary interpreter components
    init_SPARROW();
    
    // Case: File passed as CLI arg
    if( args.length > 1 ){
        res = run_file( args[1] );
    
    // Case: No file, start REPL
    }else{
        // Begin REPL
        read_eval_prnt_loop();
    }

    
}
    