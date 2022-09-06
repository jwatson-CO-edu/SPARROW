/* * Text:      [L]ISP [I]n [S]mall [P]ieces by Christian Quinnec 
   * Chapter:   1
   * Project:   SPARROW, [S]cheme [P]rogram [A]llowing [R]easonable [R]eckoning [O]f [W]ork
   * Component: Micro-evaluator, written in Dlang instead of Scheme 

   rdmd ab_eval.d
   
   https://github.com/jwatson-CO-edu/FINCH/blob/main/Nim/10_Cpp_Eval/ab_eval.nim
   James Watson, 2022-09 */

import std.string; // `string` type
import std.stdio; //- `writeln`
import std.conv; // - string conversions

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
    // NULL, // Null // 2022-09-03: Trying it w/o NULL
}

// 2022-09-05: Structs cannot hold references, switch to address passing

struct Atom{
    F_Type  kind; // ---------------- What kind of atom this is
    Atom*   car; // ----------------- Left  `Atom` Pointer
    Atom*   cdr; // ----------------- Right `Atom` Pointer
    double  num; // ----------------- Number value
    string  str; // ----------------- String value, currently limited to 16 characters
    F_Error err = F_Error.NOVALUE; /* Error code, 2022-09-03: Any atom can have an error code
                                      Instead of NULL, we can ask the Atom if it has a fault code assigned to it */
}

// 2022-09-03: Compiles

Atom* empty_atom(){
    // Allocate and return an a `NOVALUE` error
    return new Atom(
        F_Type.EROR,
        null,
        null,
        double.nan,
        "NO VALUE",
        F_Error.NOVALUE
    );
}

Atom* make_cons( Atom* car = null, Atom* cdr = null, ){
    // Make a pair
    F_Error mtCode;
    if( (car != null) || (cdr != null) ){  mtCode = F_Error.OKAY; }
    else{  mtCode = F_Error.NOVALUE;  }
    return new Atom(
        F_Type.CONS,
        car,
        cdr,
        double.nan,
        "",
        mtCode
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
        code
    );
}

// 2022-09-04: Compiles

////////// LIST PROCESSING /////////////////////////////////////////////////////////////////////////


///// Type Tests ////////////////////////////////

bool p_empty( Atom* atm ){  
    //- Atom either has the `NOVALUE` code or is null
    return (atm == null) || (atm.err == F_Error.NOVALUE);  
} 
bool p_has_error( Atom* atm ){  return (atm.err != F_Error.OKAY);  } // Atom has any code other than `OKAY`
bool p_cons( Atom* atm ){  return (atm.kind == F_Type.CONS);  } // ---- Return true if Atom is a pair


///// Accessing & Constructing ///////////////////

bool set_car_B( Atom* atm, Atom* carAtm ){  atm.car = carAtm;  return true;  } // Set left  pair item
bool set_cdr_B( Atom* atm, Atom* cdrAtm ){  atm.cdr = cdrAtm;  return true;  } // Set right pair item

Atom* consify_atom( Atom* carAtm ){
    // Wrap the `atm` in a cons, with `carAtm` as 'car'
    return make_cons( carAtm, empty_atom() );
}

// 2022-09-05: Tests PASS!

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
    if( list.kind == F_Type.CONS ){
        /* 2. If we were given an atom to append, it either belongs in the `car` of the empty cons,
              or in the `car` of a new terminal cons */
        if( !p_empty( atm ) ){
             if( p_empty( list.car ) ){
                list = make_cons( atm, empty_atom() );
             }else{
                endCns = find_terminus( list );
                set_cdr_B( endCns, consify_atom( atm ) );
             }
        }
    }else{ // 3. Else we either have one or two non-cons atoms
        rtnLst = consify_atom( list ); // --------------- ( `list` , [X] )
        if( !p_empty( atm ) ){
            set_cdr_B( rtnLst, consify_atom( atm ) ); // ( `list` , ( `atom` , [X] ) )
        }
    }
    return rtnLst;
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


////////// PARSING /////////////////////////////////////////////////////////////////////////////////
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

    }

    return tokens;
}
    

void main(){
    init_reserved();
    
    Atom* mt = empty_atom(); 
    writeln( mt ); // Address of new atom
    writeln( p_empty( mt ) ); // true
    writeln( p_has_error( mt ) ); // true
    Atom* atm1 = make_number( 2 );
    Atom* atm2 = make_number( 3 );
    Atom* atm3 = make_cons( atm1, atm2 );
    writeln( p_cons( atm2 ) ); // false
    writeln( p_cons( atm3 ) ); // true
    Atom* atm4 = consify_atom( atm2 );
    writeln( p_cons( atm4 ) ); // true
    Atom* atm5 = make_error( F_Error.NAN, "Invalid number string" );

    writeln( str( mt ) ); // - ⧄
    writeln( str( atm1 ) ); // 2
    writeln( str( atm3 ) ); // ( 2, 3 )
    writeln( str( atm4 ) ); // ( 3, ⧄ )
    append( atm4, make_number( 4 ) );
    append( atm4, make_number( 5 ) );
    append( atm4, make_number( 6 ) );
    writeln( str( atm4 ) ); // ( 3, ( 4, ( 5, ( 6, ⧄ ) ) ) )
    writeln( str( atm5 ) ); // ( ERROR: NAN, Invalid number string )

    writeln( find_reserved( "(" ) ); // - open_parn
    writeln( find_reserved( ")" ) ); // - clos_parn
    writeln( find_reserved( "foo" ) ); // <nothing>
}