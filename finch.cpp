/********** INIT *********************************************************************************/
#define INIT_NODE_NUM       64
#define DEFAULT_ALLOC_BYTES  8

#include<iostream>
#include<vector>
#include<string>
#include<memory>
#include<cstring>

using std::cout;
using std::cerr;
using std::endl;
using std::vector;
using std::shared_ptr;
using std::string;

template<typename T>
void delete_as( T* ptr ){
    if( ptr ){  
        T* addr = (T*) ptr;
        delete addr;
    }
}

/********** VARIABLES ****************************************************************************/

/***** Primitive Types *****/

enum FINCH_TYPE{
    INT_F,
    FLOAT_F,
    CHAR_F,
    USRTYP_F,
    ERRTYP_F
};

// Idea: Pre-allocate var memory for spacial locality?
class Var{ 

private:

FINCH_TYPE typ_f;
void* /**/ dataPtr;
size_t     N_bytes;
string     typKw;


public:

Var( FINCH_TYPE typ_ ) : typ_f( typ_ ) {  
    switch( typ_ ){
        case FINCH_TYPE::INT_F:
            N_bytes = sizeof( int64_t );
            typKw   = "int";
            break;

        case FINCH_TYPE::FLOAT_F:
            N_bytes = sizeof( double );  
            typKw   = "float";
            break;

        case FINCH_TYPE::CHAR_F:
            N_bytes = sizeof( char );
            typKw   = "int";
            break;
        
        case FINCH_TYPE::USRTYP_F:
            N_bytes = sizeof( void* );
            typKw   = "type";
            break;

        default:
            N_bytes = sizeof( void* );
            typKw   = "BROKEN";
            break;
    }

    dataPtr = malloc( N_bytes );
}

~Var(){  if( dataPtr ) free( dataPtr );  }

template<typename T>
T get_as(){  return *((T*) dataPtr);  }

auto get(){  
    // CHEAT: https://devblogs.microsoft.com/oldnewthing/20191106-00/?p=103066
    struct value{
        Var* var;
        operator int64_t(){
            return var->get_as<int64_t>();
        }
        operator double(){
            return var->get_as<double>();
        }
        operator char(){
            return var->get_as<char>();
        }
    };
    return value{ this };
};

template<typename T>
void set( const T& data_ ){
    void* src = (void*) (&data_);
    memcpy( dataPtr, src, N_bytes );
}

};



/********** NODE CLASSES *************************************************************************/

/***** Node *****/

class Node{ public:

void* data;

auto get_ptr(){
    return data;
}

template<typename T>
auto get(){
    return *((T*) data);
}

template<typename T>
void set_ptr( T* data_ ){
    data = (void*) data_;
}

template<typename T>
void set( T& data_ ){
    data = (void*) (&data_);
}

};


/********** TEST/MAIN ****************************************************************************/
int main(){

cout << "Test 1" << endl;
Var a{INT};
Var b{FINCH_TYPE.INT};
int64_t val1 = 4;
a.set( val1 );
b.set( 3 );
cout << a.get() << " + " << b.get();
cout << " = " << a.get() + b.get() << endl;
cout << endl << endl;


cout << "Test 2" << endl;
Node /*----*/ foo{};
int val = 1;
foo.set_ptr( &val );
cout << *(int*) foo.get_ptr() << ", " << foo.get<int>() << endl;
cout << endl << endl;
int val2 = foo.get<int>();


cout << "Test 3" << endl;
Var xur{};

cout << "Complete!" << endl;
return 0;
}