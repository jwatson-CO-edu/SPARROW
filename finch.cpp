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

/***** Primitive Types *****/

enum FINCH_TYPE{
    INT_F,
    FLOAT_F,
    CHAR_F,
    USRTYP_F,
    ERRTYP_F
};

/********** VARIABLES ****************************************************************************/

/***** Value_F *********************************/

// Idea: Pre-allocate var memory for spacial locality?
class Value_F{ 

protected:

size_t N_bytes;
void*  dataPtr;


public:

size_t n_bytes(){  return N_bytes;  }

Value_F( size_t alloc_ = DEFAULT_ALLOC_BYTES ) : N_bytes(alloc_) {  
    dataPtr = malloc( N_bytes );
}

template<typename T>
T get_as(){  return *((T*) dataPtr);  }

template<typename T>
void set( const T* dataPtr_ ){
    void* src = (void*) dataPtr_;
    memcpy( dataPtr, src, N_bytes );
}

template<typename T>
void set( const T& dataRef_ ){
    void* src = (void*) (&dataRef_);
    memcpy( dataPtr, src, N_bytes );
}

/** Rule of 3 **/

// I. destructor
~Value_F(){  if( dataPtr ) free( dataPtr );  }

// II. copy constructor
Value_F( const Value_F& other ) : N_bytes( other.N_bytes ){
    set( other.dataPtr );
}

// III. copy assignment
Value_F operator=( const Value_F& other ){
    if (this == &other) return *this;
    N_bytes = other.N_bytes;
    set( other.dataPtr );
    return *this;
}

};

/***** Arcs_F **********************************/

class Arcs_F{ public:

size_t /*----*/ N_arcs;
vector<Var_F*>  arcs;
vector<Value_F> weights;

Arcs_F(){  N_arcs = 0;  }

void add_arc( Var_F* target ){
    // http://www.cplusplus.com/reference/memory/shared_ptr/operator=/
    arcs.push_back( target );
    N_arcs++;
}

Var_F* pop_arc( size_t index = SIZE_MAX ){
    Var_F* rtnVal = nullptr;
    if( N_arcs == 0 ){
        return rtnVal;
    }else if( index == SIZE_MAX ){
        rtnVal = arcs.back();
        arcs.pop_back();
        N_arcs--;
    }else if( index < N_arcs ){
        rtnVal = arcs[ index ];
        arcs.erase( arcs.begin() + index );
        N_arcs--;
    }
    return rtnVal;
}
};

/***** Var_F ***********************************/

class Var_F{ 

protected: friend class Arcs_F;

size_t refCount;

public:

Var_F*  parent;
Value_F data; // Value
Arcs_F  in;
Arcs_F  out;

string /*---*/ type_f;

Var_F( size_t dataBytes ) : data( Value_F( dataBytes ) ){}

/** Value Wrappers **/

template<typename T>
T get_as(){  return data.get_as<T>();  }

template<typename T>
void set( const T* dataPtr_ ){  data.set( dataPtr_ );  }

template<typename T>
void set( const T& dataRef_ ){  data.set( dataRef_ );  }


/** Graph Modification **/

void add_in( Var_F* target ){ 
    // I reference `target`
    in.add_arc( target );
    target->refCount++;

    // Target references me
    target->add_out( this );
    refCount++;
}

Var_F* pop_in( size_t index = SIZE_MAX ){  
    // I forget `target`
    Var_F* rtnPtr = in.pop_arc( index );   

}

void   add_out( Var_F* target           ){  out.add_arc( target );         }
Var_F* pop_out( size_t index = SIZE_MAX ){  return out.pop_arc( index );   }

};

/********** INTERPRETER **************************************************************************/




/********** TEST/MAIN ****************************************************************************/
int main(){

cout << "`Value_F` Test" << endl;
Value_F a( 8 );
Value_F b( 8 );
int64_t val1 = 4;
a.set( val1 );
b.set( (int64_t) 3 );
cout << a.get_as<int64_t>() << " + " << b.get_as<int64_t>();
cout << " = " << a.get_as<int64_t>() + b.get_as<int64_t>() << endl;
cout << endl << endl;
int64_t val2 = a.get_as<int64_t>();
int64_t val3 = b.get_as<int64_t>();


cout << "Complete!" << endl;
return 0;
}