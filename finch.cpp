/********** INIT *********************************************************************************/
#define INIT_NODE_NUM       64
#define DEFAULT_ALLOC_BYTES  8

#include<iostream>
#include<string>
#include<memory>
#include<cstring>
#include<vector>
#include<iterator>
#include<map>

using std::cout;
using std::cerr;
using std::endl;
using std::vector;
using std::shared_ptr;
using std::string;
using std::map;
using std::pair;


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

void* get_ref(){  return dataPtr;  }

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
class Var_F;

class Arcs_F{ public:

size_t /*----*/ N_arcs;
vector<Var_F*>  arcs;
vector<Value_F> weights;
map<Var_F*,size_t> lookup;

Arcs_F(){  N_arcs = 0;  }

void add_arc( Var_F* target ){
    // http://www.cplusplus.com/reference/memory/shared_ptr/operator=/
    arcs.push_back( target );
    lookup[ target ] = N_arcs;
    N_arcs++;
}

Var_F* pop_arc( size_t index = SIZE_MAX ){
    Var_F* rtnVal = nullptr;
    bool   removd = false;
    if( N_arcs == 0 ){
        return rtnVal;
    }else if( index == SIZE_MAX ){
        rtnVal = arcs.back();
        arcs.pop_back();
        N_arcs--;
        removd = true;
    }else if( index < N_arcs ){
        rtnVal = arcs[ index ];
        arcs.erase( arcs.begin() + index );
        N_arcs--;
        removd = true;
    }
    if( removd ){  lookup.erase( rtnVal );  }
    return rtnVal;
}

Var_F* pop_by_addr( Var_F* target ){  return pop_arc( lookup[ target ] );  }

};

/***** Var_F ***********************************/
class ValueTable_F;

class Var_F{ 

protected: 
    friend class Arcs_F;
    friend class ValueTable_F;

size_t refCount;
uint   bound;

public:

Var_F*     parent;
Value_F    data; 
Arcs_F     in;
Arcs_F     out;
FINCH_TYPE typ_f;

Var_F( size_t dataBytes ) : data( Value_F( dataBytes ) ){}

/** Value Wrappers **/

template<typename T>
T get_as(){  return data.get_as<T>();  }

void* get_ref(){  return data.get_ref();  }

template<typename T>
void set( const T* dataPtr_ ){  data.set( dataPtr_ );  }

template<typename T>
void set( const T& dataRef_ ){  data.set( dataRef_ );  }


/** Graph Modification **/

void add_in( Var_F* target ){ 
    // I reference `target`
    if( in.N_arcs == 0 ){  parent = target;  }
    in.add_arc( target );
    // `target` references me
    target->out.add_arc( this );

    if( target != this ){
        target->refCount++; // I reference `target`
        refCount++; // ------- `target` references me
    }
}

Var_F* pop_in( size_t index = SIZE_MAX ){  
    // I forget `target`
    Var_F* rtnPtr = in.pop_arc( index );   
    if( in.N_arcs == 0 ){  parent = nullptr;  }
    // `target` forgets me
    rtnPtr->out.pop_by_addr( this );
    // Update ref counts
    if( rtnPtr != this ){
        rtnPtr->refCount--; // I forget `target`
        refCount--; // ------- `target` forgets me
    }
    return rtnPtr;
}

Var_F* pop_in_addr( Var_F* target ){  
    // I forget `target`
    Var_F* rtnPtr = in.pop_by_addr( target );   
    if( in.N_arcs == 0 ){  parent = nullptr;  }
    // `target` forgets me
    rtnPtr->out.pop_by_addr( this );
    // Update ref counts
    if( target != this ){
        rtnPtr->refCount--; // I forget `target`
        refCount--; // ------- `target` forgets me
    }
    return rtnPtr;
}

void add_out( Var_F* target ){ 
    // I reference `target`
    out.add_arc( target );
    // `target` references me
    if( target->in.N_arcs == 0 ){  target->parent = this;  }
    target->in.add_arc( this );
    // Update ref counts
    if( target != this ){
        target->refCount++; // I reference `target`
        refCount++; // ------- `target` references me
    }
}

Var_F* pop_out( size_t index = SIZE_MAX ){  
    // I forget `target`
    Var_F* rtnPtr = out.pop_arc( index );   
    // `target` forgets me
    rtnPtr->in.pop_by_addr( this );
    if( rtnPtr->in.N_arcs == 0 ){  rtnPtr->parent = nullptr;  }
    if( rtnPtr != this ){
        rtnPtr->refCount--; // I forget `target`
        refCount--; // ------- `target` forgets me
    }
    return rtnPtr;
}

Var_F* pop_out_addr( Var_F* target ){  
    // I forget `target`
    Var_F* rtnPtr = out.pop_by_addr( target );   
    // `target` forgets me
    rtnPtr->in.pop_by_addr( this );
    if( rtnPtr->in.N_arcs == 0 ){  rtnPtr->parent = nullptr;  }
    // Update ref counts
    if( target != this ){
        rtnPtr->refCount--; // I forget `target`
        refCount--; // ------- `target` forgets me
    }
    return rtnPtr;
}

};

/********** INTERPRETER **************************************************************************/

/***** ValueTable_F ****************************/

class ValueTable_F{ public:

map<string,Var_F*> table;

bool bind( string name, Var_F* var ){
    var->refCount++;
    var->bound = 1;
    table[ name ] = var;
}

bool unbind( string name ){
    Var_F* var = table[ name ];
    var->refCount--;
    var->bound = 0;
    table[ name + "_DEAD" ] = var;
}

};

auto get_var_value( Var_F* var ){
    
    switch( var->typ_f ){
    
    case INT_F:
        auto result = var->get_as<int64_t>();
        return result;

    case FLOAT_F:
        auto result = var->get_as<double>();
        return result;

    case CHAR_F:
        auto result = var->get_as<char>();
        return result;

    case USRTYP_F:
        auto result = var->get_ref();
        return result;
    
    default:
        break;
    }
}

Var_F* add( Var_F* op1, Var_F* op2 ){
    if( (op1->typ_f != USRTYP_F) || (op1->typ_f != ERRTYP_F) ){
        auto result = get_var_value( op1 ) + get_var_value( op2 );
        Var_F* rtnVal = new Var_F( sizeof(result) );
        rtnVal->set( result );
        return rtnVal;
    }
}



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