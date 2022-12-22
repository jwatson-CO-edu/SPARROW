/* SPARROW, [S]cheme [P]rogram [A]llowing [R]easonable [R]eckoning [O]f [W]ork
   This is an outgrowth of an implementation of "Little Scheme".  
   The concept is to make small changes to it until it becomes my own language.
   This is for entertainment only and comes with no warrantee whatsoever.

   dub build
   
   James Watson, 2022-12 */


////////// INIT ////////////////////////////////////////////////////////////////////////////////////

///// Imports /////
import std.stdio; // ------------ `writeln`, `File`
import std.conv; // ------------- string conversions
import std.typecons; // ---------- Tuple
import std.random;


/// Language Components ///
import atoms; // ------ Basic datatypes and structs
import cons; // ------- Pair constructors, list processing, list structures
import sparrow_core; // Core structs and machinery for the language
import compile_env; //- Compile-time flags and macros



////////////////////////////////////////////////////////////////////////////////////////////////////
// MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN //
////////////////////////////////////////////////////////////////////////////////////////////////////

void main( string[] args ){

    Atom* res = null;

    if( _DEBUG_VERBOSE )  writeln( "Args are: " ~ args.to!string );

    // Populate necessary interpreter components
    init_SPARROW();
    // rnd = Random( unpredictableSeed );
    
    // Case: File passed as CLI arg
    if( args.length > 1 ){
        res = run_file( args[1] );
    
    // Case: No file, start REPL
    }else{
        // Begin REPL
        read_eval_prnt_loop();
    }

    
}