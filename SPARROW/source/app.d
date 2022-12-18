/* SPARROW, [S]cheme [P]rogram [A]llowing [R]easonable [R]eckoning [O]f [W]ork
   This is an outgrowth of an implementation of "Little Scheme".  
   The concept is to make small changes to it until it becomes my own language.
   This is for entertainment only and comes with no warrantee whatsoever.

   dub build
   
   James Watson, 2022-12 */


////////// INIT //////////////////////////////////

///// Imports /////
import std.string; // ----------- `string` type
import std.stdio; // ------------ `writeln`
import std.conv; // ------------- string conversions
import std.uni; // -------------- `strip`
import std.math.operations; // -- `NaN`
import std.typecons; // ---------- Tuple
import std.ascii; // ------------- Whitespace test
import std.algorithm.searching; // `canFind`
import std.range.primitives; // `popBack`
// import std.file; // -------------- `readText`
alias  p_whitespace = std.ascii.isWhite; 

///// Env Vars /////
bool _DEBUG_VERBOSE  =  false; // Set true for debug prints
bool _TEST_ALL_PARTS =  true; // Set true to run all unit tests

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