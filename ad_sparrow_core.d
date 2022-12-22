module ad_sparrow_core;

import std.random;

Mt19937 rnd; // Randomness

struct Atom{
    double data;
}

Atom* function()[string] /*--*/ primitiveSymbols; // - Dictionary of text aliases of important symbols

double rand01(){
    // Uniform random sampling in [0,1)
    return uniform( 0.0, 1.0, rnd);
}

Atom* rand_atom(){
    return new Atom( rand01() );
}

void load_symbols(){
    primitiveSymbols["rand"] = function Atom*(){  
        // Random number on [0,1)
        Atom* rtnAtom = null;
        // rtnAtom = new Atom( rand01() );
        rtnAtom = rand_atom();
        return rtnAtom;  
    };
}
 

void init_random(){
    // Seed the RNG with the clock
    rnd = Random( unpredictableSeed );
}



void init_SPARROW(){
    // Populate necessary global structures
    init_random(); // --- RNG
}