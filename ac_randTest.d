// ///// PROGRAM 1 /////

// ////////// INIT //////////////////////////////////

// ///// Imports /////
// import std.stdio; // ------------ `writeln`, `File`
// import std.conv; // ------------- string conversions
// import std.random;


// /// Globals ///
// Mt19937 rnd; // Randomness


// ////////// MAIN //////////////////////////////////


// void main(){
//     rnd = Random( unpredictableSeed );
//     for( uint i = 0; i < 10; i++ ){
//         writeln( uniform( 0.0, 1.0, rnd) );
//     }
// }

///// PROGRAM 2 /////
import std.stdio; 
import std.conv; 
import std.random;

Mt19937 rnd; 

double rand01(){
    // Uniform random sampling in [0,1)
    return uniform( 0.0, 1.0, rnd);
}

void main(){
    rnd = Random( unpredictableSeed );
    for( uint i = 0; i < 6; i++ ){
        writeln( rand01() );
    }
}