////////////////////////////////////////////////////////////////////////////////////////////////////
// MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN //
////////////////////////////////////////////////////////////////////////////////////////////////////

void main( string[] args ){
    Atom* res = null;

    if( _DEBUG_VERBOSE )  writeln( "Args are: " ~ args.to!string );

    // Populate necessary interpreter components
    init_SPARROW();
    
    // Case: File passed as CLI arg
    if( args.length > 1 ){
        res = run_file( args[1] );
    
    // Case: No file, start REPL
    }else{
        // Begin REPL
        read_eval_prnt_loop();
    }

    
}