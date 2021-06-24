#include <stdlib.h>
#include <string>
    using std::string;
    using std::to_string;
    using std::stod;
#include <iostream>
    using std::cout;
    using std::endl;

double* dyn_dbl( double val ){
    double* ptr = (double*) malloc( sizeof( double ) );
    *ptr = val;
    return ptr;
}

int main(){
    double* dyn = dyn_dbl( 3.14 );
    cout << *dyn << endl;
    return 0;
}