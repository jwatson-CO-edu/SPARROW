module aa_atom;

////////// INIT //////////////////////////////////

///// Imports /////
import std.string; // ----------- `string` type
import std.stdio; // ------------ `writeln`
import std.conv; // ------------- string conversions


////////// ATOMS /////////////////////////////////

enum F_Error{ 
    OKAY, // -- No error code applicable
    NOVALUE, // There is no value held in this atom
    NAN, // --- Not A Number
    DNE, // --- Does Not Exist
    SYNTAX, //- Syntax error
    LEXER, // - Eval machinery failed
}


enum F_Type{ 
    CONS, // Cons pair
    STRN, // String/Symbol
    NMBR, // Number
    EROR, // Error object
    BOOL, // Boolean value
    FUNC, // Function
}

struct Atom{
    F_Type  kind; // What kind of atom this is
    union{
        double  num; // NMBR: Number value
        string  str; // STRN: String value, D-string 
        bool    bul; // BOOL: Boolean value
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
    this( F_Error e, string m ){ kind = F_Type.EROR; err = e; msg = m; } // make error
}

////////// MAIN //////////////////////////////////


void main(){

    Atom* atm;
    Atom* A = new Atom( 2.0 );
    Atom* D = new Atom( 3.0 );

    // make number
    atm = new Atom( 42.0 );
    writeln( atm.kind.to!string );
    writeln( atm.num );
    // writeln( atm.err ); // segfault

    // make string
    atm = new Atom( "foo" );
    writeln( atm.kind.to!string );
    writeln( atm.str );
    writeln( atm.bul ); // true, >>>WHY<<<?

    // make bool
    atm = new Atom( true );
    writeln( atm.kind.to!string );
    writeln( atm.bul ); 
    // writeln( atm.str ); // segfault
    
    // make string
    atm = new Atom( "foo" );
    writeln( atm.kind.to!string );
    writeln( atm.str );
    writeln( atm.bul ); // true, >>>WHY<<<?

    // make cons
    atm = new Atom( A, D );
    writeln( atm.kind.to!string );
    writeln( atm.car.num );
    writeln( atm.cdr.num );
    writeln( atm.bul ); // false, >>>WHY<<<?
    // writeln( atm.str ); // bad stuff

    // make cons
    atm = new Atom( F_Error.NOVALUE, "This atom can serve no further purpose." );
    writeln( atm.kind.to!string );
    writeln( atm.err.to!string );
    writeln( atm.msg );
    writeln( atm.bul ); // false, >>>WHY<<<?
    // writeln( atm.cdr.num ); // SegFault

    writeln( Atom.sizeof );
}