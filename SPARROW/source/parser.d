module parser;

////////// PARSING /////////////////////////////////////////////////////////////////////////////////

///// Predicates /////

bool p_float_string( string inputStr ){
    // Predicate: Is this string suitable to be converted to a (double) number?
    string slimStr = strip( inputStr );
    try{
        slimStr.to!double();
        if(_DEBUG_VERBOSE){  writeln( "Converted \"", slimStr, "\" to a number" );  }
        return true;
    }catch( ConvException e ){
        if(_DEBUG_VERBOSE){  writeln( "CONVERSION ERROR: Cannot convert \"", slimStr, "\"" );  }
    }
    return false;
}


bool p_empty_atom_string( string inputStr ){
    // Return T if the string is appropriate for empty atom conversion, otherwise return F
    if( inputStr == null ){ /*------*/ return true;  }
    if( inputStr == "" ){ /*--------*/ return true;  }
    if( inputStr == "\xE2\xA7\x84" ){  return true;  }
    if( inputStr == "[/]" ){ /*-----*/ return true;  }
    return false;
}

///// Text Processing /////
Atom* atomize_string( string token ){
    // Convert a string token into a non-cons atom
    if( p_float_string( token ) ){       return new Atom( token.to!double() );  }  
    if( p_empty_atom_string( token ) ){  return empty_atom();  }  
    if( p_primitve_symbol( token ) ){    return primitiveSymbols[ token ]();  }
   /* else assume string -------------*/ return new Atom( token );
}


bool p_balanced_parens( string[] tokens ){
    // Return true if parens open and close in a quantity and order that returns to root depth, Otherwise return false
    int depth = 0;
    foreach( string token; tokens ){
        if( p_open_paren( token ) ) depth++; 
        else
        if( p_clos_paren( token ) ) depth--;
    }
    return (depth == 0);
}


bool p_balanced_curlies( string[] tokens ){
    // Return true if parens open and close in a quantity and order that returns to root depth, Otherwise return false
    int depth = 0;
    foreach( string token; tokens ){
        if( p_open_curly( token ) ) depth++; 
        else
        if( p_clos_curly( token ) ) depth--;
    }
    return (depth == 0);
}


bool p_bounded_parens( string[] tokens ){
    // Return true if the token sequence begins and ends with open and close parens, respectively
    if( p_open_paren( tokens[0] ) ){
        if( p_clos_paren( tokens[$-1] ) )  return true;  else  return false;
    }else return false;
}


bool p_bounded_curlies( string[] tokens ){
    // Return true if the token sequence begins and ends with open and close curly braces, respectively
    if( p_open_curly( tokens[0] ) ){
        if( p_clos_curly( tokens[$-1] ) )  return true;  else  return false;
    }else return false;
}


bool p_parent_parens( string[] tokens ){
    // Return true if the outermost parens define a single root list, possibly with nested lists
    int   depth  = 0;
    ulong i      = 0;
    ulong seqLen = tokens.length;
    if( p_bounded_parens( tokens ) ){
        foreach( string token; tokens ){
            i++;
            if( p_open_paren( token ) ) depth++; 
            else
            if( p_clos_paren( token ) ) depth--;
            if( (depth == 0) && (i<seqLen) ) return false;
        }
        return true;
    }else return false;
}


bool p_parent_curlies( string[] tokens ){
    // Return true if the outermost curlies define a single root block, possibly with nested blocks
    int   depth  = 0;
    ulong i      = 0;
    ulong seqLen = tokens.length;
    if( p_bounded_curlies( tokens ) ){
        foreach( string token; tokens ){
            i++;
            if( p_open_curly( token ) ) depth++; 
            else
            if( p_clos_curly( token ) ) depth--;
            if( (depth == 0) && (i<seqLen) ) return false;
        }
        return true;
    }else return false;
}


bool p_parent_semi( string[] tokens ){
    // Return true if the outermost level of the statement defines a list punctuated with a semicolon
    ulong seqLen = tokens.length;
    if( seqLen > 1 ){
        if(  p_semicolon( tokens[$-1] )  &&  !p_semicolon( tokens[0] )  ){
            foreach( string token; tokens[1..$-2] ){
                if( p_semicolon( token ) )  return false;
            }
            return true;
        }else  return false;
    }else  return false;
}


Atom* expression_from_string( string expStr, dchar sepChar = ' ' ){
    // Tokenize the `expStr`, express it as a nested cons struct, and return
    string[] tokens = tokenize( expStr, sepChar );
    // writeln( tokens );
    // return consify_token_sequence( tokens );
    return parse_one_statement( tokens );
}


enum F_Parser{ 
    // Possible States of Parser Jobs
    RUN, // ------------- Beginning state
    ONE_ATOM, // -------- There was one atom or less in the input
    CONSUME_STATEMENT, // Read one statement into the statement register
    CONSUME_BLOCK, // --- Read one block into the block register
    PARSE_STATEMENT, // - Parse statement into an executable AST
    PARSE_BLOCK, // ----- Parse block into a list of executable ASTs
    SUCCESS, // --------- Exit this job with success
    FAULT, // ----------- Exit this job with failure
    NO_JOB // ----------- NO-OP, nothing to do
}

struct ParserJobStack{
    // Global parser job stack
    
    // Members //
    F_Parser[] jobs; // All current parsing jobs
    
    // Methods //
    
    // this(){  jobs = [];   } // Constructor
    
    bool p_has_job(){  return (jobs.length > 0);  } //- Are there are one or more jobs?
    bool p_has_prev(){  return (jobs.length > 1);  } // Is the job depth at least 2?
    bool p_has_three(){  return (jobs.length > 2);  } // Is the job depth at least 3?
    void push( F_Parser job ){  jobs ~= job;  } // ---- Add a job to the stack top
    
    F_Parser pop(){  
        // Remove job from stack top and return it
        F_Parser rtnJob = get_current();
        if( p_has_job() )
            jobs.popBack();  
        return rtnJob;
    } 

    F_Parser get_prev(){  
        // Return status of the previous job without removing it
        if( p_has_prev() )
            return jobs[$-2];  
        else 
            return F_Parser.NO_JOB;
    }

    F_Parser get_current(){  
        // Return status of the current job without removing it
        if( p_has_job() )
            return jobs[$-1];  
        else 
            return F_Parser.NO_JOB;
        
    }

    void set_current( F_Parser job ){  
        // Set status of the current job without adding it
        if( p_has_job() )  jobs[$-1] = job;  
    } 
    
}

Atom* parse_one_statement( string[] tokens ){
    // Return to simpler days of Scheme parsing
    ulong    seqLen  = tokens.length;
    ulong    bgn     = 0;
    ulong    end     = seqLen-1;
    ulong    index   = 0;
    Atom*    lstRoot = null;
    string[] carPart;
    string   token;
    uint     depth = 0;

    if( _DEBUG_VERBOSE )  writeln( "`parse_one_statement`" );
    
    if( seqLen == 0 ){

        lstRoot = empty_atom();

    }else if( seqLen == 1 ){

        lstRoot = atomize_string( tokens[0] );

    // Are the parens correct?
    }else if( p_balanced_parens( tokens ) ){

        // Are we building a list?
        if(  p_parent_parens( tokens )  ||  p_parent_semi( tokens )  ){
            end--;
            if( p_parent_parens( tokens ) )  bgn++;
            
            // Begin list
            index   = bgn;
            lstRoot = empty_cons();
            if( _DEBUG_VERBOSE ){
                writeln( "\tInput: " ~ tokens.to!string );
                writeln( "\tRoot: " ~ lstRoot.kind.to!string ~ ", " ~ str( lstRoot ) );
            }  

            // While we are in the list bounds, find and append items
            while( index <= end ){
                
                // Find an element
                carPart = [];
                depth   = 0;
                
                // If element is a list
                if( p_open_paren( tokens[index] ) ){
                    do{
                        token = tokens[index];
                        if( p_open_paren( token ) ) depth++; 
                        else
                        if( p_clos_paren( token ) ) depth--;
                        carPart ~= token;
                        index++;
                    }while( (depth > 0) && (index <= end) );

                // else element is an atom
                }else{

                    // index = lastDex;
                    carPart ~= tokens[index];
                    index++;
                }

                if( _DEBUG_VERBOSE ){
                    writeln( "\tappend list elem: " ~ carPart.to!string );
                }

                // Append element to list
                lstRoot = append( 
                    lstRoot,
                    parse_one_statement( carPart )
                );
            }
        }
    }else{
        return new Atom( F_Error.SYNTAX, "PARENTHESES MISMATCH" );
    }
    if( _DEBUG_VERBOSE ){
        writeln( "\tabout to return: " ~ str( lstRoot ) );
    }    
    return lstRoot;
}

// Global parser job stack
ParserJobStack parserJobs = ParserJobStack();


Atom* parse_token_sequence( string[] tokens ){
    // Init
    F_Parser state; // ------------------- Current parser state
    F_Parser prevState; // ------------------- Current parser state
    bool     parsing    = true; // -------- Is the parser at this depth running?
    Atom*[]  progBlock  = []; // -------- Block for this depth
    string[] sttmntReg  = []; // ---------- Statement "register"
    string[] progTokens = []; // ---------- Statement "register" for a loop body or other block
    string   faultMsg;
    Atom*    blockReg  = null;
    Atom*    loopReg   = null;
    ulong    seqLen    = tokens.length; // Input length
    ulong    regLen    = 0; // ----------- Input length
    // ulong    bgn       = 0; // ----------- Input begin index
    // ulong    end       = 0; // ----------- Input end   index
    ulong    index     = 0; // ----------- Index of current input token
    uint     depth     = 0;
    string   token; // ------------------- Current input token
    Atom*    rtnProg = null; // ----------------- Output returned at this depth

    parserJobs.push( F_Parser.RUN ); // Parser always begins in the `RUN` state

    if( _DEBUG_VERBOSE )  writeln( "`parse_token_sequence`" );

    ///// Parser Loop ////////////////////////////
    do{

        state = parserJobs.get_current(); // Model is to keep job on the stack until it is done

        // Handle state
        switch( state ){
            
            /// Case RUN: Decide on the next mode of the parser ///////////
            case F_Parser.RUN:
                if( _DEBUG_VERBOSE )  writeln( "\tCase RUN: Decide on the next mode of the parser" );
                
                if(index < seqLen){
                    // If there are one or less tokens, we can only return one atom
                    if( seqLen < 2 )
                        parserJobs.set_current( F_Parser.ONE_ATOM );
                    // Else there are many, check if we need to consume a block or a statement
                    else{
                        token = tokens[ index ]; // Fetch token
                        if( p_open_curly( token ) ) // Open curly: Consume block

                            // parserJobs.set_current( F_Parser.CONSUME_BLOCK );
                            parserJobs.push( F_Parser.CONSUME_BLOCK ); // Push so that we can peek block context

                        else // else is an s-expression or an EZ list
                            parserJobs.set_current( F_Parser.CONSUME_STATEMENT );
                        break;
                    }
                }else if( sttmntReg.length > 0 ){
                    parserJobs.set_current( F_Parser.PARSE_STATEMENT );
                }else{
                    parserJobs.set_current( F_Parser.SUCCESS );
                }
                break;
                    

            /// Case ONE_ATOM: Parse one or empty ////////////////////
            case F_Parser.ONE_ATOM:    
                if( _DEBUG_VERBOSE )  writeln( "\tCase ONE_ATOM: Parse one or empty" );

                // Base Case: There were no tokens, return Empty
                if( seqLen == 0 ){  progBlock ~= empty_atom();  }
                // Base Case: There was one token, Return it
                if( seqLen == 1 ){  progBlock ~= atomize_string( tokens[0] );  }
                // Flag success
                parserJobs.set_current( F_Parser.SUCCESS );
                break;


            /// Case CONSUME_STATEMENT: Load statement ////////////////////
            case F_Parser.CONSUME_STATEMENT:
                if( _DEBUG_VERBOSE )  writeln( "\tCONSUME_STATEMENT: Load statement" );

                sttmntReg = [];
                token     = tokens[ index ];

                // If element is a list then scan it into the statement register
                if( p_open_paren( tokens[index] ) ){
                    if( _DEBUG_VERBOSE )  writeln( "\tScanning s-expression ..." );
                    do{
                        token = tokens[index];
                        if( p_open_paren( token ) ) depth++; 
                        else
                        if( p_clos_paren( token ) ) depth--;
                        sttmntReg ~= token;
                        index++;
                    }while( (depth > 0) && (index <= seqLen) );

                // else scan in an EZ list
                }else{
                    if( _DEBUG_VERBOSE )  writeln( "\tScanning EZ list ..." );
                    do{
                        if( _DEBUG_VERBOSE )  writeln( "\t`index` == " ~ index.to!string );

                        token = tokens[index];
                        sttmntReg ~= token;
                        index++;
                        if( p_semicolon( token ) ){
                            if( _DEBUG_VERBOSE )  writeln( "\tSemicolon!: " ~ token );
                            break;
                        }  
                    }while( (index <= seqLen) );
                }

                if( _DEBUG_VERBOSE )  writeln( "\tStatement Tokens: " ~ sttmntReg.to!string );


                // Flag statement register for parsing
                parserJobs.set_current( F_Parser.PARSE_STATEMENT );
                break;


            /// Case CONSUME_LOOP_BODY: Load block statements /////////////////
            /// Case CONSUME_BLOCK: Load block statements /////////////////
            case F_Parser.CONSUME_BLOCK:
                if( _DEBUG_VERBOSE )  writeln( "\tCase CONSUME_BLOCK: Load block statements" );

                sttmntReg = [];
                index++;
                depth = 1;
                do{
                    token = tokens[ index ];
                    if( p_open_curly( token ) )
                        depth++;
                    else if( p_clos_curly( token ) ){
                        depth--;
                        if(depth == 0){
                            index++; // Move past the final curly
                            break;
                        } 
                    }
                    sttmntReg ~= token;
                    index++;
                }while( index < seqLen );
                
                parserJobs.set_current( F_Parser.PARSE_BLOCK );
                break;


            /// Case PARSE_STATEMENT: Parse loaded statement //////////////
            case F_Parser.PARSE_STATEMENT:
                if( _DEBUG_VERBOSE )  writeln( "\tCase PARSE_STATEMENT: Parse loaded statement" );
                progBlock ~= parse_one_statement( sttmntReg );
                parserJobs.set_current( F_Parser.RUN );
                sttmntReg = [];
                break;            


            /// Case PARSE_BLOCK: Recur on block //////////////////////////
            case F_Parser.PARSE_BLOCK:
                // Descend one depth and parse the consumed block
                if( _DEBUG_VERBOSE )  writeln( "\tCase PARSE_BLOCK: Recur on block" );

                blockReg = parse_token_sequence( sttmntReg );
                // Ascended one depth, now put the job in context
                // prevState = ;
                
                switch( parserJobs.get_prev() ){
                    
                    case F_Parser.RUN:
                        progBlock ~= blockReg;
                        parserJobs.pop();
                        blockReg = null;
                        break;
                    
                    case F_Parser.NO_JOB:
                        faultMsg = "A code block was built without context";
                        parserJobs.push( F_Parser.FAULT );
                        break;

                    default:
                        faultMsg = "IMPOSSIBLE CODE BLOCK CASE";
                        parserJobs.push( F_Parser.FAULT );
                        break;
                }
                sttmntReg = [];
                break;


            /// Case SUCCESS: End of input with no errors /////////////////
            case F_Parser.SUCCESS:

                if( _DEBUG_VERBOSE ){
                    writeln( "\tCase SUCCESS: End of input with no errors" );
                    writeln( "\tSUCCESS, result: " ~ str( rtnProg ) );
                }
                state   = parserJobs.pop();
                parsing = false;
                break;
            

            /// Case FAULT: The input has caused problems /////////////////
            case F_Parser.FAULT:

                if( _DEBUG_VERBOSE ){
                    writeln( "\tCase FAULT: The input has caused problems" );

                    writeln( "\tFAULT, result: " ~ str( rtnProg ) );
                }
                state   = parserJobs.pop();
                parsing = false;
                rtnProg = new Atom( F_Error.PARSER, "Parser FAULT: " ~ faultMsg );
                break;


            /// Case NO_JOB: Input exhausted //////////////////////////////
            case F_Parser.NO_JOB:

                if( _DEBUG_VERBOSE ){
                    writeln( "\tCase NO_JOB: Input exhausted" );

                    writeln( "\tNO_JOB, parser job stack is EMPTY!" );
                }
                parsing = false;
                break;
            
            
            /// Case BAD: SOMETHING UNEXPECTED HAPPENED ///////////////////
            default: 
                state = parserJobs.pop();
                if( _DEBUG_VERBOSE ){
                    writeln( "\tCase BAD: SOMETHING UNEXPECTED HAPPENED" );

                    writeln( "\tdefault, SOMETHING UNEXPECTED HAPPENED!, Final State: " ~ state.to!string );
                }
                rtnProg = new Atom( F_Error.PARSER, "IMPOSSIBLE PARSER CASE ENCOUNTERED" );
                break;
        }
    }while( parsing );
    
    if( _DEBUG_VERBOSE ){
        writeln( "\tParser EXIT!, Final State: " ~ state.to!string );
        writeln( "\tThere are " ~ progBlock.length.to!string ~ " statements in program at this level." );
        int forDex = 1;
        foreach( Atom* atm; progBlock ){
            write( "\t\t" ~ forDex.to!string ~ "/" ~ progBlock.length.to!string  ~ ", " );
            writeln( str( atm ) ); 
            forDex++;
        }
    }

    if( (rtnProg == null) && (progBlock.length > 0) ){
        if( _DEBUG_VERBOSE )  writeln( "\tReturn program" );
        rtnProg = new Atom( progBlock );
    }
        
    else if( _DEBUG_VERBOSE )
        writeln( "\tNO PROGRAM STORED" );

    return rtnProg;
}