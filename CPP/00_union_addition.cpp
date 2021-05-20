#include <stdint.h> // uint
#include <variant>
    using std::get;
    using std::variant;
#include <stdlib.h> // malloc
#include "introspect.hpp"
#include <iostream>
    using std::ostream;
    using std::cout;
    using std::cerr;
    using std::endl;

class Val64{
public:


variant<uint64_t, int64_t, double> val;

template<typename T>
Val64( T number ){  val = number;  }

Val64 operator+( const Val64& other ){
    uint8_t myTyp = val.index();
    uint8_t otrTp = other.val.index();

    void* myVal = malloc( 8 );
    void* otrVl = malloc( 8 );
    
    switch( myTyp ){
        case 0:  myVal = &get<0>( val );  break;
        case 1:  myVal = &get<1>( val );  break;
        case 2:  myVal = &get<2>( val );  break;
    }

    switch( otrTp ){
        case 0:  otrVl = (void*) &get<0>( other.val );  break;
        case 1:  otrVl = (void*) &get<1>( other.val );  break;
        case 2:  otrVl = (void*) &get<2>( other.val );  break;
    }

    switch( myTyp ){

        case 0:  {    
            switch( otrTp ){
                case 0:  return Val64( *((uint64_t*) myVal) + *((uint64_t*) otrVl) );
                case 1:  return Val64( *((uint64_t*) myVal) + *((int64_t*)  otrVl) );
                case 2:  return Val64( *((uint64_t*) myVal) + *((double*)   otrVl) );
            }
        }  break;
        
        case 1:  {    
            switch( otrTp ){
                case 0:  return Val64( *((int64_t*) myVal) + *((uint64_t*) otrVl) );
                case 1:  return Val64( *((int64_t*) myVal) + *((int64_t*)  otrVl) );
                case 2:  return Val64( *((int64_t*) myVal) + *((double*)   otrVl) );
            }
        }  break;
        
        case 2:  {    
            switch( otrTp ){
                case 0:  return Val64( *((double*) myVal) + *((uint64_t*) otrVl) );
                case 1:  return Val64( *((double*) myVal) + *((int64_t*)  otrVl) );
                case 2:  return Val64( *((double*) myVal) + *((double*)   otrVl) );
            }
        }  break;
    }

    return Val64( 0.0 );
}

};

ostream& operator<<( ostream& o, Val64& V ){
    uint8_t myTyp = V.val.index();
    switch( myTyp ){
        case 0:  o << get<0>( V.val );  return o;
        case 1:  o << get<1>( V.val );  return o;
        case 2:  o << get<2>( V.val );  return o;
    }
    return o;
}

int main(){

    Val64 a(  3    );
    Val64 b( -3    );
    Val64 c(  3.14 );

    Val64 d = a + b + c;

    cout << d << endl;

    return 0;
}