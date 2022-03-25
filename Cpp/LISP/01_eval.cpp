/* Text:      [L]ISP [I]n [S]mall [P]ieces by Christian Quinnec 
 * Chapter:   1
 * Project:   SPARROW, [S]cheme [P]rogram [A]llowing [R]easonable [R]eckoning [O]f [W]ork
 * Component: Micro-evaluator, written in C++ instead of Scheme */

#include <string>
    using std::string;
    using std::to_string;
    using std::stod;
#include <cstring>
    using std::toupper;
#include <iostream> // std::cout, std::skipws, std::noskipws
    using std::noskipws;
#include <sstream>
#include <map>
    using std::map;
    using std::pair;
#include <vector>
    using std::vector;
#include "demo2400_helpers.hpp"


/********** HELPER FUNCTIONS *********************************************************************/

string str_to_upper( const string& inputStr ){
    // Return the uppercase version of the string
    string rtnStr;
    char   c;
    size_t len = rtnStr.length();
    for( size_t i = 0 ; i < len ; ++i ){
        c = inputStr[i];
        rtnStr += toupper( c );
    }
    return rtnStr;
}


/********** ATOMS ********************************************************************************/

enum F_Type{
    // This micro-language has the following types, all packaged into the every Atom
    CONS = 10, // Cons pair
    STRN = 20, // String/Symbol
    NMBR = 30, // Number
    Null = 40, // Null
    EROR = 50, // Error object
};

enum F_Error{
    OKAY    =  0, // No error code applicable
    NOVALUE = 10, // There is no value held in this atom
};

struct Atom{
    // The most basic and interchangeable unit of this LISP
    F_Type  typ; // Type of atom data
    Atom*   car; // Pair left
    Atom*   cdr; // Pair right
    char*   str; // String data
    double  num; // Numeric data
    F_Error err; // Error code
};

Atom* empty_atom(){
    // Allocate and return an atom with default values
    Atom* rtnAtm = (Atom*) malloc( sizeof( Atom ) );
    rtnAtm->typ  = EROR; // ERROR
    rtnAtm->err  = NOVALUE;
    rtnAtm->car  = nullptr;
    rtnAtm->cdr  = nullptr;
    rtnAtm->str  = nullptr;
    rtnAtm->num  = nan(""); 
    return rtnAtm;
}


Atom* make_null(){
    // Make a Null
    Atom* rtnAtm = empty_atom();
    rtnAtm->typ  = Null; // Null
    rtnAtm->err  = OKAY; // We are happy this has a value
    return rtnAtm;
}


Atom* make_cons( Atom* car_ = nullptr, Atom* cdr_ = nullptr ){
    // Make a pair
    Atom* rtnAtm = empty_atom();
    rtnAtm->typ  = CONS;
    rtnAtm->err  = OKAY; // We are happy this has a value
    rtnAtm->car  = car_ ? car_ : make_null(); // pair left
    rtnAtm->cdr  = cdr_ ? cdr_ : make_null(); // pair right
    return rtnAtm;
}

Atom* make_string( const char* str_ ){
    // Make a string
    Atom* rtnAtm = empty_atom();
    rtnAtm->typ  = STRN;
    rtnAtm->err  = OKAY; // We are happy this has a value
    rtnAtm->str  = (char*) malloc( sizeof( char[16] ) );
    strcpy( rtnAtm->str, str_ ); // String or symbol
    return rtnAtm;
}

Atom* make_number( double nmbr_ ){
    // Make a number
    Atom* rtnAtm = empty_atom();
    rtnAtm->typ  = NMBR;
    rtnAtm->err  = OKAY; // We are happy this has a value
    rtnAtm->num  = nmbr_; // Number
    return rtnAtm;
}




/********** LIST PROCESSING **********************************************************************/

/***** Type Tests *****/

bool p_Null( Atom* op ){  return (op->typ == Null);  } // Return T if this atom is `Null`, Otherwise return F
bool p_cons( Atom* op ){  return (op->typ == CONS);  }
void set_car_B( Atom* cons, Atom* valu ){  cons->car = valu;  }
void set_cdr_B( Atom* cons, Atom* valu ){  cons->cdr = valu;  }


Atom* consify_atom( Atom* atm ){
    // Wrap the `atm` in a cons, with `atm` as 'car'
    return make_cons( atm, make_null() );
}


Atom* find_terminus( Atom* list ){
    // Iterate to the ending cons of the list and return a pointer to it
    // 0. Set the argument equal to our pointer
    Atom* ptr = list;
    // 1. If this is a cons structure, then we must find the end of it
    if( list->typ == CONS ){
        // 2. Iterate pointer to next `cdr` until we reach a pair that contains the terminating null, return pair
        while( !p_Null( ptr->cdr ) ){   ptr = ptr->cdr;  }
        return ptr;
    // Else atom was literal, it is its own terminus
    }else{  return list;  }
}

Atom* append( Atom* list, Atom* atom = nullptr ){
    // Append an atom to the end of a conslist, Create a conslist if none exists, return pointer to list head
    Atom* rtnLst = list;
    Atom* endCns = nullptr;

    // 1. If the given list is a cons list, it is either an empty cons or the head of a LISP list
    if( list->typ == CONS ){
        // 2. If we were given an atom to append, it either belongs in the `car` of the empty cons,
        //    or in the `car` of a new terminal cons
        if( atom ){
            if(  p_Null( list->car )  ){  
                set_car_B( list, atom );  
            }else{
                endCns = find_terminus( list );
                set_cdr_B( endCns, consify_atom( atom ) );
            }
        }
    // 3. Else we either have one or two non-cons atoms
    }else{
        rtnLst = consify_atom( list ); // ----------------------- ( `list` , [X] )
        if( atom )  set_cdr_B( rtnLst, consify_atom( atom ) ); // ( `list` , ( `atom` , [X] ) )
    }
    return rtnLst;
}

/***** Printing *****/

string str( Atom* item ){
    // Return the string representation of the `item`
    // Null Symbol: https://www.fileformat.info/info/unicode/char/29c4/index.htm
    string rtnStr = "";
    switch (item->typ){
        /* Null ---*/ case Null:  rtnStr = "\xE2\xA7\x84"; /*---------------------------*/ break; 
        /* Str/Sym */ case STRN:  rtnStr = item->str; /*--------------------------------*/ break;
        /* Num ----*/ case NMBR:  rtnStr = to_string( item->num ); /*-------------------*/ break;
        /* Pair ---*/ case CONS:  rtnStr = "( "+str(item->car)+", "+str(item->cdr)+" )";   break;
        /* Nothing */ default: /*-------------------------------------------------------*/ break;
    }
    return rtnStr;
}


/********** PARSING ******************************************************************************/

map<string, string> _RESERVED;

string find_reserved( const string& token ){
    // Return the name of the reserved symbol, or an empty string if not found
    map<string,string>::iterator it = _RESERVED.find( token );
    if( it == _RESERVED.end() ){  return "";  } // If the search failed, return an empty string
    else{  return it->second;  } // -------------- Else return the string name of the reserved name
}

vector<string> tokenize( string expStr, string sepChar = " " ){
    // Parse an expression string into an s-expression
    size_t /*---*/ len   = expStr.length(); // Length of the given string
    string /*---*/ token = ""; // ------------ Current token in progress
    string /*---*/ c; // --------------------- Current character
    vector<string> tokens; // ---------------- Vector of string tokens

    // LAMBDA: Add the current token to the vector and reset current token to empty
    auto stow_token = [&tokens, &token](){
        tokens.push_back( token );
        token = "";
    };
    // LAMBDA: Add the current character to the vector and reset token to empty
    auto stow_char  = [&c, &tokens](){  tokens.push_back( c );  };
    // LAMBDA: Add the current character to the current token
    auto cache_char = [&c, &token ](){  token += c;             };

    // 0. Apply the postfix hack
    expStr += ' ';
    // 1. For every character in the string
    for( size_t i = 0 ; i < len ; ++i ){
        // 2. Fetch character
        c = expStr[i];
        // 3. Either add char to present token or create a new one
        
        // Case Open Paren
        if( find_reserved( c ) == "open_parn" ){  stow_char();  } else 
        
        // Case Close Paren
        if( find_reserved( c ) == "clos_parn" ){  
            if( token.length() ){  stow_token();  }
            stow_char();  
        } else 
        
        // Case separator
        if( c == sepChar ){  stow_token();  } else
        
        // Case any other char
        cache_char();
    }
    // N. Return the vector of tokens
    return tokens;
}


bool p_float_string( const string& inputStr ){
    // Return T if the string is appropriate for float conversion, otherwise return F
    // Author: Bill the Lizard,  https://stackoverflow.com/a/447307
    std::istringstream iss( inputStr );
    float f;
    iss >> noskipws >> f; // noskipws considers leading whitespace invalid
    // Check the entire string was consumed and if either failbit or badbit is set
    return iss.eof() && !iss.fail(); 
}


bool p_null_string( const string& inputStr ){
    // Return T if the string is appropriate for Null conversion, otherwise return F
    // if( !inputStr.length() ) /*------------*/ return true; // 2021-06-18: For now, do not consider empty string null
    if( str_to_upper( inputStr ) == "NULL" )  return true; // Consider any capitalization of "NULL"
    return false;
}


Atom* atomize_string( const string& token ){
    // Convert a string into a non-cons atom
    if(  p_float_string( token )  ){  return make_number( stod( token ) );  }  
    if(  p_null_string( token )   ){  return make_null();                   }  
    /* else assume it is string ---*/ return make_string( token.c_str() );
}


Atom* consify_tokens( const vector<string>& tokens, size_t& i ){
    // Render tokens as a cons structure

    string token; // ----------------- Current token from the vector
    size_t len     = tokens.size(); // Number of tokens in the given vector
    Atom*  rtnTree = nullptr; // ----- Cons structure to return

    // 1. If there are one or more strings to process, then attempt to construct a token tree
    if( (tokens.size()-i) > 1 ){
        // 2. Start off by creating a cons list, if needed
        rtnTree = make_cons();

        // 3. For each token in the vector
        while( i < len ){
            // 4. Fetch token at this index
            token = tokens[i];
            i++;

            // 5. Case Open Paren
            if(  find_reserved( token ) == "open_parn"  ){
                // If there is a new level of depth to the structure, recur
                if(i>1)  append(  rtnTree , consify_tokens( tokens, i )  );
                // else this is the first level
                else     rtnTree = consify_tokens( tokens, i );
            }else

            // 6. Case Close Paren
            if(  find_reserved( token ) == "clos_parn"  ){  return rtnTree;  }else

            // 7. Case Null
            if(  p_null_string( token )  ){  append( rtnTree , make_null() ); } 

            // 8. Case Literal
            else{  append( rtnTree , atomize_string( token ) );  }
        }
        // return rtnTree;
    // 9. else there were no tokens, return Null
    }else{  return make_null();  }
    return rtnTree;
}


Atom* expression_from_string( string expStr, string sepChar = " " ){
    // Tokenize the `expStr`, express it as a conslist, and return
    size_t i = 0;
    vector<string> tokens = tokenize( expStr, sepChar );
    return consify_tokens( tokens, i );
}

/********** ENVIRONMENT **************************************************************************/

class Env{  public:
// Environment for evaluation

Env* /*--------*/ parent; // -- Pointer to the environment that contains this one
vector<Atom*>     freeVars; //- Free  variables, without binding
map<string,Atom*> boundVars; // Bound variables, have names given to them by statements

// Return T if the binding exists in `boundVars`, otherwise return F
bool p_binding_exists( const string& name ){  return ( boundVars.end() == boundVars.find( name ) );  }

Atom* get_bound_atom( const string& name ){
    // Return the name of the reserved symbol, or an empty string if not found
    map<string,Atom*>::iterator it = boundVars.find( name );
    if( it == boundVars.end() ){  return nullptr;  } // If the search failed, return a null pointer
    else{  return it->second;  } // ------------------- Else return the requested atom
}

// Bind an `atom` to a `name` by adding it to the mapping, If the name already exists, it will be updated
void bind_atom( const string& name, Atom* atom ){  boundVars[ name ] = atom;  }

};

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
    string t_expr = "(cons a b)";
    vector<string> l_expr = tokenize( t_expr );
    cout << l_expr << endl;

    /*** Test vector consification ***/
    
    size_t counter = 0;
    Atom* s_expr = consify_tokens( l_expr, counter );

    cout << s_expr << endl;
    cout << s_expr->typ << endl;
    cout << str( s_expr ) << endl;

    /*** Test Text --to-> s-expression ***/
    s_expr = expression_from_string( t_expr );
    // cout << str( s_expr ) << endl;
}