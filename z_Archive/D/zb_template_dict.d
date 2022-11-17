template ArrArgs(T){
    alias bool function(T[])[string] FuncDict; // compiles
    FuncDict lookup; // This compiles but name `lookup` is not understood
}

void main(){
    lookup["foo"] = function bool( string[] args ){ return true; }; // undefined identifier `lookup`
}