#[
    Lesson AG: Builtin and User-Defined Types
    https://nim-lang.org/docs/tut1.html#advanced-types-enumerations
    nim c -d:release --hints:off --run -o:exec/ag_types -r ag_types.nim
]#


##### Enum Types #####

type
    Direction = enum
        north, east, south, west

var x = south     # `x` is of type `Direction`; its value is `south`
echo x            # prints "south"



##### Sets #####
# https://nim-lang.org/docs/tut1.html#advanced-types-sets