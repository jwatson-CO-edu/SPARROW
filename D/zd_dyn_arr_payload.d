import std.stdio;

struct Payload{
    double[] dbbls;
    string[] strns;
}

bool function( Payload )[string] funcLookup;

void main(){
    funcLookup["foo"] = function bool(Payload args) { return true; };
    Payload input;
    input.dbbls ~= 3;
    input.dbbls ~= 4;
    input.dbbls ~= 5;
    writeln( input.dbbls );

    writeln( funcLookup["foo"]( input ) ); // true
}