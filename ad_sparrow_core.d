module ad_sparrow_core;

import std.random;
import af_atom;

Mt19937 rnd; // Randomness

Atom* function()[string] /*--*/ primitiveSymbols; // - Dictionary of text aliases of important symbols

double rand01( ref Mt19937 RNG ){
    // Uniform random sampling in [0,1)
    return uniform( 0.0, 1.0, RNG );
}

Atom* rand_atom( ref Mt19937 RNG ){
    return new Atom( rand01( RNG ) );
}

void load_symbols(){
    primitiveSymbols["rand"] = function Atom*(){  
        // Random number on [0,1)
        Atom* rtnAtom = null;
        // rtnAtom = new Atom( rand01() );
        rtnAtom = rand_atom( rnd );
        return rtnAtom;  
    };
} 

void init_random( ref Mt19937 RNG ){
    // Seed the RNG with the clock
    RNG = Random( unpredictableSeed );
}

void init_SPARROW(){
    // Populate necessary global structures
    init_random( rnd ); // --- RNG
}