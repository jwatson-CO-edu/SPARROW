/* Text:    [L]ISP [I]n [S]mall [P]ieces by Christian Quinnec 
 * Chapter: 1
 * Project: Micro-evaluator, written in C++ instead of Scheme */

#include <string>
    using std::string;
#include<map>
    using std::map;
    using std::pair;



/********** ATOMS ********************************************************************************/

enum Type{
    // This micro-language has the following types, all packaged into the every Atom
    CONS, // Cons pair
    STRN, // String/Symbol
    NMBR, // Number
    Null  // Null
};

struct Atom{
    // The most basic and interchangeable unit of this LISP
    Type   typ; // Type of atom data
    Atom*  car; // Pair left
    Atom*  cdr; // Pair right
    string str; // String data
    double num; // Numeric data
};

Atom* make_strn( string str_ ){
    // Make a string
    Atom* rtnAtm = new Atom{};
    rtnAtm->typ  = STRN;
    rtnAtm->car  = nullptr;
    rtnAtm->cdr  = nullptr;
    rtnAtm->str  = str_; // String or symbol
    rtnAtm->str  = 0.0;
    return rtnAtm;
}

Atom* make_nmbr( double nmbr_ ){
    // Make a string
    Atom* rtnAtm = new Atom{};
    rtnAtm->typ  = NMBR;
    rtnAtm->car  = nullptr;
    rtnAtm->cdr  = nullptr;
    rtnAtm->str  = ""; 
    rtnAtm->str  = nmbr_; // Number
    return rtnAtm;
}

Atom* make_null(){
    // Make a Null
    Atom* rtnAtm = new Atom{};
    rtnAtm->typ  = Null; // Null
    rtnAtm->car  = nullptr;
    rtnAtm->cdr  = nullptr;
    rtnAtm->str  = ""; 
    rtnAtm->str  = 0.0; 
    return rtnAtm;
}

Atom* make_cons( Atom* car_ = nullptr, Atom* cdr_ = nullptr ){
    // Make a cons pair
    Atom* rtnAtm = new Atom{};
    rtnAtm->typ  = CONS;
    rtnAtm->car  = car_ ? car_ : make_null(); // pair left
    rtnAtm->cdr  = cdr_ ? cdr_ : make_null(); // pair right
    rtnAtm->str  = "";
    rtnAtm->str  = 0.0;
    return rtnAtm;
}


/********** LIST PROCESSING **********************************************************************/

// Return T if this atom is `Null`, Otherwise return F
bool p_Null( Atom* op ){  return (op->typ == Null);  }

void set_car_B( Atom* cons, Atom* valu ){  cons->car = valu;  }
void set_cdr_B( Atom* cons, Atom* valu ){  cons->cdr = valu;  }

Atom* find_terminus( Atom* list ){
    // Iterate to the end of the list
    Atom* ptr = list->cdr;
    if( list->typ == CONS ){
        while( !p_Null( ptr ) ){  ptr = list->cdr;  }
        
    }else{

    }
}

Atom* append( Atom* list ){
    if( list->typ == CONS ){
        // FIXME: START HERE
    }else{

    }
}


/********** PARSING ******************************************************************************/
map<string, string> _RESERVED;

Atom* tokenize( string expStr ){
    // Parse an expression string into an s-expression
    // 0. For character in `expStr`
    size_t depth = 0;
    size_t len   = expStr.length();
    string c;
    string token = "";
    Atom*  sExpr = make_cons();
    for( size_t i = 0 ; i < len ; ++i ){
        c = expStr[i];

        // Case Open Paren
        if( _RESERVED.find( c )->second == "open_parn" ){
            depth++;
        } else 

        // Case Close Paren
        if( _RESERVED.find( c )->second == "clos_parn" ){
            depth--;
        } //else 

    }
}

int main(){
    /// Reserved Symbols and Keywords ///
    _RESERVED.insert( pair<string, string>( "(", "open_parn" ) ); // Open  paren
    _RESERVED.insert( pair<string, string>( ")", "clos_parn" ) ); // Close paren
}