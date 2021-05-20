#ifndef INTROSPECT_H
#define INTROSPECT_H

#define get_type( expr )                               \
        _Generic( (expr),                              \
                  uint64_t: string( "uint64_t" ), \
                  int64_t:  string( "int64_t"  ), \
                  double:   string( "double"   ), \
                  uint32_t: string( "uint32_t" ), \
                  int32_t:  string( "int32_t"  ), \
                  float:    string( "float"    )  )

#endif