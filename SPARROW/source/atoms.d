module atoms;

/*  atoms.d
    Basic datatypes and structs for the SPARROW language
    James Watson, 2022-12 */

////////// ATOMS ///////////////////////////////////////////////////////////////////////////////////

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