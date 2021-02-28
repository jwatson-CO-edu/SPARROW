/********** INIT *********************************************************************************/
#define INIT_NODE_NUM       64
#define DEFAULT_ALLOC_BYTES 64

#include <iostream>
#include <vector>
#include <memory>

using std::cout;
using std::cerr;
using std::endl;
using std::vector;
using std::shared_ptr;

/********** VARIABLES ****************************************************************************/
class Var{ public:

void* data;

Var(){  data = malloc( DEFAULT_ALLOC_BYTES );  }

template<typename T>
T get_as(){
    return *((T*) data);
}

template<typename T>
void set( T& data_ ){
    *data = data_;
}

template<typename T>
void set( const T& data_ ){
    *data = data_;
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
T get(){
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
Node /*----*/ foo{};
int val = 1;

foo.set_ptr( &val );
cout << *(int*) foo.get_ptr() << ", " << foo.get<int>() << endl;



cout << endl << endl;

cout << "Complete!" << endl;
return 0;
}