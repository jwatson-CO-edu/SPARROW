module sparrow_core;

/*  atoms.d
    Core structs and machinery for the SPARROW language
    James Watson, 2022-12 */

////////// INIT ////////////////////////////////////////////////////////////////////////////////////

///// Imports /////
import std.stdio; // ------------- `writeln`, `File`
import std.math.operations; // --- `NaN`
import std.conv; // -------------- string conversions
import std.algorithm.searching; // `canFind`
import std.random;


/// Language Components ///
import atoms; // ----- Basic datatypes and structs
import cons; // ------ Pair constructors, list processing, list structures
import lexer; // ----- Render raw text into meaningful tokens
import parser; // ---- Render meaningful tokens into executable CST
import compile_env; // Compile-time flags and macros


/// Globals ///
static Mt19937 rnd; // Randomness
string /*---*/ VERSION = "2023.1.0";


////////// RANDOMNESS //////////////////////////////////////////////////////////////////////////////

void init_random( ref Mt19937 RNG ){
    // Seed the RNG with the clock
    RNG = Random( unpredictableSeed );
}



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

double rand01( ref Mt19937 RNG ){
    // Uniform random sampling in [0,1)
    double rtnNum = uniform( 0.0, 1.0, RNG );
    return rtnNum;
}

///// Primitives /////

Atom* function( Atom* )[string] primitiveFunctions; // Dictionary of foundational operations, implemented in Dlang
Atom* function()[string] /*--*/ primitiveSymbols; // - Dictionary of text aliases of important symbols


bool p_primitve_symbol( string token ){    return (token in primitiveSymbols) !is null;  } // - In the primitive sym dict?
bool p_primitve_function( string token ){  return (token in primitiveFunctions) !is null;  } // In the primitive func dict?


void init_primitives(){

    /// Zero Arguments ///

    primitiveSymbols["true"]  = function Atom*(){  return new Atom(true); /*--*/ }; // Boolean True
    primitiveSymbols["#t"]    = function Atom*(){  return new Atom(true); /*--*/ }; // Boolean True
    primitiveSymbols["false"] = function Atom*(){  return new Atom(false); /*-*/ }; // Boolean False
    primitiveSymbols["#f"]    = function Atom*(){  return new Atom(false); /*-*/ }; // Boolean False

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
void writ( Atom* atm ){  write(   str( atm ) );  } // Print a cons structure


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
Env* envPtr; // Context pointer


void init_env(){
    // Create the base environment that is parent of all contexts in the interpreter
    baseEnv = new Env();
    envPtr  = baseEnv;
}


////////// SPECIAL FORMS ///////////////////////////////////////////////////////////////////////////

Atom* function( Atom* )[string] specialForms; // Dictionary of forms other than func applications, implemented in Dlang


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
    // No arg or null arg, return false
    return false;
}


void init_specials(){
    // Assign functions to handle special forms

    specialForms["quote"] = function Atom*( Atom* expr ){  
        // Passthru for expression args 
        return textOf( expr );
    };


    specialForms["print"] = function Atom*( Atom* expr ){  
        // Print the first arg
        Atom* res = meaning( textOf( expr ) );
        if( p_empty( res ) ){
            writeln("");
        }else
            prnt( res );
        return res;
    };


    specialForms["write"] = function Atom*( Atom* e ){  
        // Print the first arg
        Atom* res = meaning( textOf( e ) );
        if( p_empty( res ) ){
            write(" ");
        }else
            writ( res );
        return res;
    };


    specialForms["lambda"] = function Atom*( Atom* e ){  
        // Package anonymous function for eval
        Atom* forms = get_cdr( e );
        return make_function( first( forms ), get_cdr( forms ) );
    };


    specialForms["cond"] = function Atom*( Atom* e ){  
        // Package cond for eval
        return evcon( condLinesOf( e ) );
    };


    specialForms["define"] = function Atom*( Atom* e ){  
        // Bind expression result to a name

        if( _DEBUG_VERBOSE ) writeln( "`define`: " ~ str( e )  );
        
        // 1. Bind name
        bind_atom( 
            envPtr, // ---------- Context to bind to
            second( e ).str, // Name to bind
            meaning( third( e ) ) // Eval this
        );

        // N. Return bound name
        return second( e );
    };


    // specialForms["and"] = function ExprInContext( ExprInContext eINc ){  
    specialForms["and"] = function Atom*( Atom* e ){  
        // Return true only if all the forms evaluate to true, otherwise return false
        Atom*   forms   = formLinesOf( e );
        Atom*[] formLst = flatten_atom_list( forms );

        foreach (Atom* form; formLst){
            if( !truthiness(
                meaning( form )
            ) ){
                return new Atom( false );
            }
        }

        return new Atom( true );
    };


    specialForms["or"] = function Atom*( Atom* e ){  
        // Return true if any of the forms evaluate to true, otherwise return false
        Atom*   forms   = formLinesOf( e );
        Atom*[] formLst = flatten_atom_list( forms );

        if( _DEBUG_VERBOSE ) writeln( "OR" );

        foreach (Atom* form; formLst){
            if( truthiness(
                meaning( form )
            ) ){
                return new Atom( true );
            }
        }

        return new Atom( false );
    };


    specialForms["for"] = function Atom*( Atom* e ){
        // Execute a `for` loop, Loop meaning is the last statement of the last iteration, 
        // Default is to increment up by one, inclusive bounds

        // 1. Parse loop args
        Atom*[] loopArgs  = flatten_atom_list( second( e ) ); // Fetch args as atom vector
        string  iVarName  = loopArgs[0].str; // ------------------------ Get the counter var name, WARNING: TYPE NOT CHECKED
        bool    incrByOne = (loopArgs.length == 3); // ----------------- Default is to increment up by one, inclusive bounds
        double  loBound   = 0.0;
        double  hiBound   = 0.0;
        double  incr      = 1.0;
        double  i /*---*/ = 0.0;
        Atom*   loopProg  = third( e ); // WARNING: TYPE NOT CHECKED
        Env*    nuEnv     = null; 
        Atom*   runBlock  = null; 
        Atom*[] rtnExpr;

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
        }else  return new Atom( F_Error.SYNTAX, loopArgs.length.to!string ~ 
                                " was an incorrect number of loop args. Expected 3 or 4." );

        // 2. Create a new nested context, bind the counter var
        i     = loBound;
        nuEnv = new Env();
        nuEnv.parent = envPtr;
        bind_atom( nuEnv, iVarName, new Atom( loBound ) );

        // Descend to loop env
        envPtr = nuEnv;

        // 3. LOOP: If loop condition met, run block in nested context && increment, otherwise exit loop
        while( i <= hiBound ){
            rtnExpr ~= block_meaning( loopProg );
            i += incr; // increment
            bind_atom( nuEnv, iVarName, new Atom( i ) ); // Store new counter value so that loop body can access it
        }

        // Ascend to parent env
        envPtr = envPtr.parent;

        return rtnExpr[$-1];
    };

    // FIXME: LOAD FILE, A WAY TO IMPORT FILES WITHIN EITHER FILES OR REPL
    // specialForms["load"] = function ExprInContext( ExprInContext eINc ){ /* LOAD FILE */ } 

}



////////// EVALUATION //////////////////////////////////////////////////////////////////////////////
// This is the real living mechanism of the SPARROW interpreted language
// 2022-09-13: `atomize_string` will fetch primitive symbols, these were together w/ primitve functions in Little JS

bool p_eq( Atom* op1, Atom* op2 ){  return primitiveFunctions["eq?"]( make_list_of_2( op1, op2 ) ).bul;  }
bool p_else( Atom* x ){  return p_literal( x ) && p_eq( x, new Atom( "else" ) );  } // is the arg an 'else symbol?



Atom* evcon( Atom* e ){
    // evaluate cond form by form, this is the guts of cond
    Atom* /*---*/ forms     = e; // ------------------------------ Fetch cond lines from the expression
    Atom* /*---*/ condition = null;
    Atom* /*---*/ answer    = null;
    Atom* result; // ------------------------------------------- Evaluation result

    if( _DEBUG_VERBOSE ) writeln( "\t`evcon`: received the following forms: " ~ str( forms ) );

    Atom*[] formLst = flatten_atom_list( forms );

    foreach (Atom* form; formLst){
        if( _DEBUG_VERBOSE ) writeln( "\tevcon: " ~ "Working on form - " ~ str(form) );
        condition = questionOf( form );
        answer    = answerOf( form );
        if( _DEBUG_VERBOSE ) writeln( "\tevcon: " ~ "evaluate ..." );
        if( p_else( condition ) ){
            if( _DEBUG_VERBOSE ) writeln( "\tevcon: " ~ "Default to else" );
            return meaning( answer );
        }else{
            if( _DEBUG_VERBOSE ) writeln( "\tevcon: " ~ "Regular condition" );

            result = meaning( condition );

            if( truthiness( result ) ){

                if( _DEBUG_VERBOSE ) writeln( "\tevcon: " ~ "Expression was TRUE" );

                return meaning( answer );
            }
        }
    }

    return new Atom( false );
}


Atom* apply_primitive_function( Atom* e ){
    // Invocation of primitive function in a context

    if( _DEBUG_VERBOSE ) writeln( "`apply_primitive_function`" );

    string /*--*/ name /*-*/ = get_car( e ).str;
    Atom* /*---*/ interpArgs = null;
    Atom* result;

    if( p_primitve_function( name ) ){
        interpArgs = meaning( get_cdr( e ) );

        if( _DEBUG_VERBOSE ) writeln( 
            "\t`apply_primitive_function`: " ~ "About to call " ~ name ~ " with the following args -> " ~ str( interpArgs ) 
        );

        result = primitiveFunctions[ name ]( interpArgs );

        if( _DEBUG_VERBOSE ) writeln( 
            "\t`apply_primitive_function`: " ~ str( e ) ~ "--(" ~ name ~ ")-> " ~ str( result )
        );

        return result;
    }else{

        if( _DEBUG_VERBOSE ) writeln( 
            "\t`apply_primitive_function`: Search for primitive function " ~ name ~ " has no result!" 
        );

        return new Atom(
            F_Error.DNE,
            "Oops, \"" ~ get_car( e ).str ~ "\" is NOT a primitive function in SPARROW!"
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


Atom* apply_closure( Atom* input ){ 
    // apply a non-primitive function

    if( _DEBUG_VERBOSE )  writeln( "`apply_closure`" );

    Atom* rtnRes = null; // Enclosed environment
    Atom* func   = null;
    Atom* argMeaning;

    // 0. Determine if the function exists, then construct a new context
    if(  p_user_def_function( envPtr, nameOf( input ).str )  ){
        
        // 1. Create a new context with the arguments given values as a child of the containing context
        func /*-*/ = get_bound_atom( envPtr, nameOf( input ).str );
        argMeaning = meaning( argsOf( input ) );
        envPtr     = enclose( 
            envPtr, // ------- parent 
            formalsOf( func ), // --- parameters
            argMeaning, 
        );

        if( _DEBUG_VERBOSE ){  
            writeln( "\t`apply_closure`: " ~ "Found function " ~ nameOf( input ).str );
            writeln( "\t`apply_closure`: " ~ "Parameters - " ~ str(formalsOf( func ) ));
            writeln( "\t`apply_closure`: " ~ "Arguments  - " ~ str( argMeaning ) );
            writeln( "\t`apply_closure`: " ~ "Body       - " ~ str( bodyOf( func ) ) );
        }

        // 2. Evaluate the function within the new context
        rtnRes = meaning( bodyOf( func ) );

        // 3. Ascend from closure
        envPtr = envPtr.parent;

        // N. Return result
        return rtnRes;
    }

    if( _DEBUG_VERBOSE )  writeln( "`apply_closure`: " ~ "NO function named " ~ nameOf( input ).str );

    // 2. Evaluate the function within the new context
    return new Atom( F_Error.NOVALUE, "There is no function with the name " ~ nameOf( input ).str );
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


Atom* function()[string] /*--*/ runtimeSymbols; // - Dictionary of text aliases of important symbols
bool p_runtime_symbol( string token ){    return (token in runtimeSymbols) !is null;  } // - In the primitive sym dict?


void init_runtime_symbols(){

    runtimeSymbols["rand"] = function Atom*(){  
        // Random number on [0,1)
        Atom* rtnAtm = null;
        rtnAtm = new Atom( rand01( rnd ) );
        return rtnAtm; 
    }; 

}


Atom* meaning( Atom* e ){ 
    // Handle expressions more complex than literals

    if( _DEBUG_VERBOSE ){
        writeln( "`meaning`" );
        writeln( "\t" ~ str(e) );
        writeln( "\t" ~ str(get_cdr( e )) );
    } 

    Atom*  balance  = get_cdr( e );
    Atom*  rtnRoot  = null;
    Atom*  inputPtr = null;
    string name     = "";
    Env*   nuEnv    = null;
    Atom*  rtnResult;

    if( p_string( e ) ){  name = e.str;  }

    if( _DEBUG_VERBOSE )  writeln( "\t`meaning`" ~ ": init done" );

    /// Base Cases ///

    // Base Case: Runtime Symbol
    if( p_runtime_symbol( name ) ){

        if( _DEBUG_VERBOSE ) writeln( "\t`meaning`: " ~ "Runtime Symbol" );

        rtnResult = runtimeSymbols[ name ]();

    // Base Case: Bound Symbol
    }else if( p_binding_exists( envPtr, name ) && (!p_bound_function( envPtr, name )) ){

        if( _DEBUG_VERBOSE ){ 
            writeln( "\t`meaning`: " ~ "Bound Symbol, " ~ name );
            writeln( "\t`meaning`: " ~ name ~ " = " ~ str( get_bound_atom( envPtr, name ) ) );
            writeln( envPtr );
        }

        rtnResult = get_bound_atom( envPtr, name );

    // Base Case Literal -OR- Base Case Empty: Pass thru
    }else if( p_literal(e) || p_empty(e) ){
        if( _DEBUG_VERBOSE ) writeln( "\tmeaning: " ~ "Was atom, " ~ str(e) ~ " is a " ~ e.kind.to!string );
        rtnResult = e;

    /// Recursive Cases ///
    }else{

        // Fetch name for some cases, if it exists
        if( p_cons( e ) && p_string( get_car( e ) ) ){  name = get_car( e ).str;  }

        // Case Code Block
        if( p_block( e ) ){

            // Create a nested context && set pointer
            nuEnv = new Env();
            nuEnv.parent = envPtr;
            envPtr = nuEnv;

            // Pass that context to meaning && Return the meaning of the last statement in the block
            rtnResult = block_meaning( e );

            // Ascend pointer
            envPtr = envPtr.parent;
        
        // Case Primitive Function
        }else if( p_primitve_function( name ) ){

            if( _DEBUG_VERBOSE ){
                writeln( "\t`meaning`: " ~ "Primitive Function" );
                writeln( "\t`meaning`: " ~ "Args - ", str( balance ) );
            }

            rtnResult = apply_primitive_function( e ); // There are no context changes for primitive functions

        // Case Special Form
        }else if( p_special_form( name ) ){

            if( _DEBUG_VERBOSE ) writeln( "`meaning`: " ~ "Special Form" );

            rtnResult = specialForms[ name ]( e );

        // Case Function Application
        }else if( p_bound_function( envPtr, name ) ){

            if( _DEBUG_VERBOSE ) writeln( "`meaning`: " ~ "Bound Function (Closure)" );

            rtnResult = apply_closure( e );

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
                    meaning( first( inputPtr ) )
                );
                inputPtr = get_cdr( inputPtr );
            }while( !p_empty( inputPtr ) );

            rtnResult = rtnRoot;
        }
    } 
    if( _DEBUG_VERBOSE ) writeln( "\t`meaning`: " ~ "Final ->" ~ str( rtnResult ) );
    return rtnResult;
}


Atom* value( Atom* expression ){
    // call `meaning`` on expression in the '$global' context
    return meaning( expression );
}


void init_SPARROW(){
    // Populate necessary global structures
    init_random( rnd ); // --- RNG
    init_reserved(); // - Reserved symbols
    init_env(); // ------ Global context
    init_primitives(); // Special atoms and Primitive Functions
    init_specials(); // - Special forms
    init_runtime_symbols(); // Symbols evaluated at runtime
}



////////// READ, EVALUATE, PRINT, LOOP /////////////////////////////////////////////////////////////

void read_eval_prnt_loop(){
    // Terminal interaction with SPARROW
    bool   quit   = false; // Use request to quit
    string input; // -------- User input
    Atom*  expr   = null; //- S-Expression
    Atom*  output = null; //- Eval result
    
    // Preamble //
    writeln( "\n##### SPARROW Interpreter, Version " ~ VERSION ~ " #####" );
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


Atom* block_meaning( Atom* block ){
    // Execute the block, then return the meaning of the last statement in the block
    Atom*[] statememts = block.blk;
    Atom*   result;
    foreach( Atom* sttmnt; statememts ){
        result = meaning( sttmnt );
    }
    return result;
}


Atom* run_file( string fName ){
    // Execute every statement in a file and return the result of the last expression
    string[] allTokens = tokenize_file( fName );
    Atom*    code = parse_token_sequence( allTokens );
    return block_meaning( code );
}