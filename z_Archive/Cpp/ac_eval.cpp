#include <stdint.h> // uint
#include <stdlib.h> // malloc
#include <iostream> // << printing
    using std::ostream;
    using std::cout;
    using std::cerr;
    using std::endl;

union Data64
{
    std::int32_t n;     // occupies 4 bytes
    std::uint16_t s[2]; // occupies 4 bytes
    std::uint8_t c;     // occupies 1 byte
}; 