module lexer;

////////// LEXING //////////////////////////////////////////////////////////////////////////////////

string[string] RESERVED;


void init_reserved(){
    RESERVED["("] = "open_parn"; // Open  paren
    RESERVED[")"] = "clos_parn"; // Close paren
    RESERVED[";"] = "semicolon"; // Semicolon
    RESERVED["{"] = "open_crly"; // Open  curly brace
    RESERVED["}"] = "clos_crly"; // Close curly brace
}


string find_reserved( string token ){
    // Return the name of the reserved symbol, or an empty string if not found
    string* res = token in RESERVED;
    if( res !is null ){  return *res;  } // If key in dict, then return the string name of the reserved token
    else{  return "";  } // --------------- Else the search failed, return an empty string
}

bool p_open_paren( string token ){  return find_reserved( token ) == "open_parn";  } // Is an open  paren?
bool p_clos_paren( string token ){  return find_reserved( token ) == "clos_parn";  } // Is an close paren?
bool p_semicolon(  string token ){  return find_reserved( token ) == "semicolon";  } // Is a semicolon?
bool p_open_curly( string token ){  return find_reserved( token ) == "open_crly";  } // Is an open  curly?
bool p_clos_curly( string token ){  return find_reserved( token ) == "clos_crly";  } // Is an close curly?


string[] tokenize( string expStr, dchar sepChar = ' ' ){
    // Separate an expression string into tokens
    string[] tokens;
    dchar    c      = ' ';
    string   cStr;
    string   token  = "";
    bool     testWhite = false;
    if( sepChar == ' ' ) testWhite = true;


    // Helpers //
    void stow_token(){  tokens ~= token;  token = "";  }
    void stow_char(){  tokens ~= to!string( c );  }
    void cache_char(){  token ~= c;  }

    // 0. Apply the postfix hack
    expStr ~= sepChar;
    // 1. For every character in the string
    foreach( i, ch_i; expStr ){
        // 2. Fetch character
        c    = ch_i;
        cStr = c.to!string();
        // 3. Either add char to present token or create a new one
        // A. Case Open Paren
        if ( p_open_paren( cStr ) || p_open_curly( cStr ) ){  stow_char();  }  
        // B. Case end of expression or list
        else if(  p_clos_paren( cStr )  ||  p_clos_curly( cStr )  ||  p_semicolon( cStr )  ){  
            if( token.length > 0 ){  stow_token();  }
            stow_char();  
        }
        // C. Case separator
        else if( (testWhite && p_whitespace(c)) || (c == sepChar)){
            if(token.length > 0){  stow_token();  }
        // D. Case any other char
        }else{  cache_char();  } 
    }
    // N. Return the vector of tokens
    return tokens;
}


string[] tokenize_file( string fName ){
    string   tExpr;
    string[] sExpr;
    File     f = File( fName );

    if( _DEBUG_VERBOSE )  writeln( "`tokenize_file`" );

    foreach( line; f.byLine ){
        tExpr = line.to!string; // Fetch text expression
        if( _DEBUG_VERBOSE )  writeln( "\tLex line: " ~ tExpr );
        if( strip( tExpr ).length == 0 )  continue; // If newline only, there is nothing to do
        sExpr ~= tokenize( tExpr );
    }
    return sExpr;
}