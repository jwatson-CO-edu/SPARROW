/* Text:      [L]ISP [I]n [S]mall [P]ieces by Christian Quinnec 
 * Chapter:   1
 * Project:   SPARROW, [S]cheme [P]rogram [A]llowing [R]easonable [R]eckoning [O]f [W]ork
 * Component: Micro-evaluator, written in C++ instead of Scheme */

#include <string>
    using std::string;
    using std::to_string;
#include <map>
    using std::map;
    using std::pair;
#include <vector>
    using std::vector;
#include "demo2400_helpers.hpp"



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

Atom* make_cons( Atom* car_ = nullptr, Atom* cdr_ = nullptr ){
    // Make a pair
    Atom* rtnAtm = new Atom{};
    rtnAtm->typ  = CONS;
    rtnAtm->car  = car_ ? car_ : make_null(); // pair left
    rtnAtm->cdr  = cdr_ ? cdr_ : make_null(); // pair right
    rtnAtm->str  = ""; 
    rtnAtm->num  = nan(""); 
    return rtnAtm;
}

Atom* make_strn( string str_ ){
    // Make a string
    Atom* rtnAtm = new Atom{};
    rtnAtm->typ  = STRN;
    rtnAtm->car  = nullptr;
    rtnAtm->cdr  = nullptr;
    rtnAtm->str  = str_; // String or symbol
    rtnAtm->num  = nan(""); 
    return rtnAtm;
}

Atom* make_nmbr( double nmbr_ ){
    // Make a string
    Atom* rtnAtm = new Atom{};
    rtnAtm->typ  = NMBR;
    rtnAtm->car  = nullptr;
    rtnAtm->cdr  = nullptr;
    rtnAtm->str  = ""; 
    rtnAtm->num  = nmbr_; // Number
    return rtnAtm;
}

Atom* make_null(){
    // Make a Null
    Atom* rtnAtm = new Atom{};
    rtnAtm->typ  = Null; // Null
    rtnAtm->car  = nullptr;
    rtnAtm->cdr  = nullptr;
    rtnAtm->str  = "";
    rtnAtm->num  = nan(""); 
    return rtnAtm;
}


/********** LIST PROCESSING **********************************************************************/

/***** Type Tests *****/

bool p_Null( Atom* op ){  return (op->typ == Null);  } // Return T if this atom is `Null`, Otherwise return F
bool p_cons( Atom* op ){  return (op->typ == CONS);  } // Return T if this atom is `CONS`, Otherwise return F


/***** Cons Modifiers *****/

void set_car_B( Atom* cons, Atom* valu ){  cons->car = valu;  }
void set_cdr_B( Atom* cons, Atom* valu ){  cons->cdr = valu;  }

Atom* consify_atom( Atom* atm ){
    // Wrap the `atm` in a cons, with `atm` as 'car'
    return make_cons( atm, make_null() );
}


Atom* find_terminus( Atom* list ){
    // Iterate to the ending cons of the list and return a pointer to it
    Atom* ptr = list->cdr;
    if( list->typ == CONS ){
        while( !p_Null( ptr->cdr ) ){  ptr = ptr->cdr;  }
        return ptr;
    }else{
        return list;
    }
}


Atom* append( Atom* list, Atom* atom = nullptr ){
    // Append an atom to the end of a conslist, Create a conslist if none exists
    Atom* rtnLst = list;
    Atom* endCns = nullptr;
    // If the list was actually a non-cons atom, This is an atom that needs to be a list
    if( list->typ != CONS ){  rtnLst = consify_atom( list );  }
    // If there was a second arg
    if( atom ){
        // Find the end of the list
        endCns = find_terminus( list );
        // Wrap the atom in a cons and append
        set_cdr_B( endCns, consify_atom( atom ) );
    }  
    return list;
}

/***** Printing *****/

string str( Atom* item ){
    // Return the string representation of the `item`
    string rtnStr = "<EMPTY>";
    switch (item->typ){
        /* Null */ case Null:  rtnStr = "\xE2\xA7\x84";  break; // https://www.fileformat.info/info/unicode/char/29c4/index.htm
        /* Str/Sym */ case STRN:  rtnStr = item->str; /*-*/ break;
        /* Num */ case NMBR:  rtnStr = to_string( item->num ); break;
        

        case CONS: // Cons pair
            if(  ) // FIXME: START HERE

        default:
            break;
    }
}


/********** PARSING ******************************************************************************/

map<string, string> _RESERVED;

vector<string> tokenize( string expStr, string sepChar = " " ){
    // Parse an expression string into an s-expression
    // 0. For character in `expStr`
    size_t /*---*/ len   = expStr.length();
    string /*---*/ c;
    string /*---*/ token = "";
    vector<string> tokens;

    auto stow_token = [&tokens, &token](){
        tokens.push_back( token );
        token = "";
    };
    auto stow_char  = [&c, &tokens](){  tokens.push_back( c );  };
    auto cache_char = [&c, &token ](){  token += c;             };

    // 0. Apply the postfix hack
    expStr += ' ';
    // 1. For every character in the string
    for( size_t i = 0 ; i < len ; ++i ){
        // 2. Fetch character
        c = expStr[i];

        // 3. Either add char to present token or create a new one
        // Case Open Paren
        if( _RESERVED.find( c )->second == "open_parn" ){  stow_char();  } else 
        // Case Close Paren
        if( _RESERVED.find( c )->second == "clos_parn" ){  
            if( token.length() ){  stow_token();  }
            stow_char();  
        } else 
        // Case separator
        if( c == sepChar ){  stow_token();  } else
        // Case any other char
        cache_char();
    }

    // N. Return the list of tokens
    return tokens;
}

Atom* consify_tokens( const vector<string>& tokens ){
    // Render tokens as a LISP list
    // FIXME: PART TWO
}

/********** TESTING ******************************************************************************/

void setup(){
    // Set variables and prepare environment for REPL

    /*** Reserved Symbols and Keywords ***/
    _RESERVED.insert( pair<string, string>( "(", "open_parn" ) ); // Open  paren
    _RESERVED.insert( pair<string, string>( ")", "clos_parn" ) ); // Close paren
}

int main(){
    /*** Init env ***/
    setup();

    /*** Test expression tokenization ***/
    string expr = "(cons a b)";
    cout << tokenize( expr ) << endl;

    /*** Test vector consification ***/
    // FIXME: CONSIFY
}