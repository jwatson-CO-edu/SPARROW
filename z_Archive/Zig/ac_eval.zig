////////// INIT ////////////////////////////////////////////////////////////////////////////////////
const print = @import("std").fmt.format;

////////// ATOMS ///////////////////////////////////////////////////////////////////////////////////

///// Atom Data Payload /////
// Atom data is held in a tagged union
const DataType   = enum { 
    // Type of payload the union has
    STRN, // String/Symbol
    NMBR, // Number
    NULL, // Null
    EROR, // Error object
};
const AtomData64 = union( DataType ){ 
    // Tagged Union for a 64bit payload
    STRN: [8]u8, // String/Symbol
    NMBR: f64, // Number
    NULL: u64, // Null
    EROR: u64  // Error object
};

///// Atom Definition /////

const AtomType = enum { 
    // Types of atoms supported by the language
    CONS, // Cons pair
    STRN, // String/Symbol
    NMBR, // Number
    NULL, // Null
    EROR, // Error object
};
const Atom64 = struct{
    // Most basic component of the language
    typ: AtomType, // - Type of atom
    dat: AtomData64, // Data payload
    car: *Atom64, // -- Pointer left  (CONS)
    cdr: *Atom64, // -- Pointer right (CONS)
};

////////// STRINGS /////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
////////// MAIN ////////////////////////////////////////////////////////////////////////////////////
pub fn main() void {

    const x = "goodcode";
    print("x = \"{s}\"\n",.{x});
    print("sizeOf( x ) = {} bytes\n",.{@sizeOf([8]u8)});
    print("sizeOf(f64) = {} bytes\n",.{@sizeOf(f64)});
    print("sizeOf(u64) = {} bytes\n",.{@sizeOf(u64)});

}