import std.string;

enum F_Error{ 
    OKAY,    // No error code applicable
    NOVALUE, // There is no value held in this atom
    NAN,     // Not A Number
}

enum F_Type{ 
    CONS, // Cons pair
    STRN, // String/Symbol
    NMBR, // Number
    EROR, // Error object
    // NULL, // Null // 2022-09-03: Trying it w/o NULL
}

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

Atom empty_atom(){
    // Allocate and return an a `NOVALUE` error
    Atom rtnObj;
    rtnObj.kind = F_Type.EROR;
    rtnObj.err  = F_Error.NOVALUE;
    return rtnObj;
}

Atom make_cons( Atom* car = null, Atom* cdr = null, ){
    // Make a pair
    Atom rtnObj;
    rtnObj.kind = F_Type.CONS;
    rtnObj.car  = car;
    rtnObj.cdr  = cdr;
    if( (car != null) || (cdr != null) ){  rtnObj.err = F_Error.OKAY;  }
    return rtnObj;
}

Atom make_string( string str ){
    // Make a string
    Atom rtnObj;
    rtnObj.kind = F_Type.STRN;
    rtnObj.str = str;
    if( str.length > 0 ){  rtnObj.err = F_Error.OKAY;  }
    return rtnObj;
}

Atom make_number( double nmbr ){
    // Make a number
    Atom rtnObj;
    rtnObj.kind = F_Type.NMBR;
    rtnObj.num  = nmbr;
    rtnObj.err  = F_Error.OKAY;
    return rtnObj;
}

// 2022-09-04: Compiles


void main(){}