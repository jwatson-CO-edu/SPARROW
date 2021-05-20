#ifndef STRING_H
#define STRING_H

#include <stdint.h> // uint
#include <string.h>

typedef struct{
    uint64_t len;
    char*    str;
} String;

String make_String( const char* inStr ){
    String rtnStr;
    strcpy( rtnStr.str, inStr);
    rtnStr.len = strlen( rtnStr.str );
    return rtnStr;
}

void assign_String( const char* inStr, String* Str ){
    *Str = make_String( inStr );
}

#endif