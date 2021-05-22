/* Text:    [L]ISP [I]n [S]mall [P]ieces by Christian Quinnec 
 * Chapter: 1
 * Project: Micro-evaluator, written in C++ instead of Scheme */

#include <string>
    using std::string;

enum Type{
    // This micro-language has the following types, all packaged into the every Atom
    CONS, // Cons pair
    STRN, // String/Symbol
    NMBR  // Number
};

struct Atom{
    // The most basic and interchangeable unit of this LISP
    Type   typ; // Type of atom data
    Atom*  car; // Pair left
    Atom*  cdr; // Pair right
    string str; // String data
    double num; // Numeric data
};

// FIXME: `make_cons`
// FIXME: `make_strn`
// FIXME: `make_nmbr`

Atom tokenize( string expStr ){
    // Parse an expression string into an s-expression
    // 0. For character in `expStr`
    // FIXME: START HERE
}