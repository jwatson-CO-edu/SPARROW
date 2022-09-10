import std.string;

void temp(Func)(Func func){
    static if (is (Func == int function(double))) {
        pragma(msg, "Found an int(double)");

    } else static if (is (Func == ReturnT function(string, size_t), ReturnT)) {
        pragma(msg, format("This one returns %s", ReturnT.stringof));
    }
}

int foo(double d){  return 42;  }

struct S{}

S bar(string s, size_t st){
    return S.init;
}

void main(){
    temp(&foo);
    temp(&bar);
}