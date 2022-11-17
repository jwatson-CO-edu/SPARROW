#include <stdint.h> // uint
#include "string_.h"

////////// TYPES //////////////////////////////////////////////////////////////////////////////////

typedef union{
    uint64_t U;
    int64_t  L;
    double   F;
} Var64;

typedef union {
    uint32_t U;
    int32_t  L;
    float    F;
} Var32;

typedef struct {
    uint8_t typ;
    Var64   val;
} Num64;

uint64_t value_of_64_U( Num64 N ){  return N.val.U;  }
int64_t  value_of_64_L( Num64 N ){  return N.val.L;  }
double   value_of_64_F( Num64 N ){  return N.val.F;  }

typedef struct {
    uint8_t typ;
    Var32   val;
} Num32;

uint32_t value_of_32_U( Num32 N ){  return N.val.U;  }
int32_t  value_of_32_L( Num32 N ){  return N.val.L;  }
float    value_of_32_F( Num32 N ){  return N.val.F;  }

///// TYPE INTROSPECTION ////////////////////////

String get_type_64( Num64* N ){ switch( N->typ ){
    case 0: return make_String( "uint64_t" );
    case 1: return make_String( "int64_t"  );
    case 2: return make_String( "double"   );
} }

String get_type_32( Num32* N ){ switch( N->typ ){
    case 0: return make_String( "uint32_t" );
    case 1: return make_String( "int32_t"  );
    case 2: return make_String( "float"   );
} }

#define get_type( expr )                               \
        _Generic( (expr),                              \
                  uint64_t: make_String( "uint64_t" ), \
                  int64_t:  make_String( "int64_t"  ), \
                  double:   make_String( "double"   ), \
                  uint32_t: make_String( "uint32_t" ), \
                  int32_t:  make_String( "int32_t"  ), \
                  float:    make_String( "float"    )  )

        
#define get_type_ext( expr ) \
        _Generic( (expr) , \
                  Num64:  get_type_64, \
                  Num32:  get_type_32, \
        )( expr )


#define get_value( expr )


Num64 add64( Num64 op1, Num64 op2 ){
    Num64 rtnNum;
    rtnNum.typ = max( op1.typ, op2.typ );
}



Num64 make_ulong( uint64_t v ){
    Num64 rtn = { 0, v };
    return rtn;
}

Num32 make_int( int32_t v ){
    Num32 rtn = { 1, v };
    return rtn;
}

int main(){

    int a = 1;

    printf(  get_type( a )  );
    printf(  a  );

    return 0;
}