alias FuncDict = bool function(T[])[string]; // undefined identifier `T`

FuncDict lookup;

void main(){
    lookup["foo"] = function bool( string[] args ){ return true; };
}