module ae_app;

import ad_sparrow_core;
import std.stdio;
import std.conv;

void main(){
    init_SPARROW();
    load_symbols();
    
    
    foreach (i; 0 .. 3){
        writeln( primitiveSymbols["rand"]().data );
        writeln( primitiveSymbols["rand"]().data );
    }  
}