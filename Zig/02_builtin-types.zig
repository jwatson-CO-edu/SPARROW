// https://ziglearn.org/chapter-1/#assignment

////////// ASSIGNMENT && PRIMITIVE TYPES ///////////////////////////////////////////////////////////

const constant: i32 =    5; // signed 32-bit constant
var   variable: u32 = 5000; // unsigned 32-bit variable

// @as performs an explicit type coercion
const inferred_constant = @as(i32, 5);
var   inferred_variable = @as(u32, 5000);

//     Constants and variables must have a value. If no known value can be given, the undefined value, 
// which coerces to any type, may be used as long as a type annotation is provided.
const a: i32 = undefined;
var   b: u32 = undefined;



////////// ADVANCED ASSIGNMENT && COMPOUND TYPES ///////////////////////////////////////////////////

const a = [5]u8{ 'h', 'e', 'l', 'l', 'o' }; // Arrays are denoted by [N]T, where N is the number of elements 
//in the array and T is the type of those elements (i.e., the array’s child type).

const b = [_]u8{ 'w', 'o', 'r', 'l', 'd' }; // For array literals, N may be replaced by `_` to infer the size of the array.



////////// INTEGER RULES ///////////////////////////////////////////////////////////////////////////
// Zig supports hex, octal and binary integer literals.
const decimal_int: i32 = 98222;
const hex_int: u8 = 0xff;
const another_hex_int: u8 = 0xFF;
const octal_int: u16 = 0o755;
const binary_int: u8 = 0b11110000;
// Underscores may also be placed between digits as a visual separator.
const one_billion: u64 = 1_000_000_000;
const binary_mask: u64 = 0b1_1111_1111;
const permissions: u64 = 0o7_5_5;
const big_address: u64 = 0xFF80_0000_0000_0000;

// Integers by default are not allowed to overflow. Overflows are detectable illegal behaviour. 
// Sometimes being able to overflow integers in a well defined manner is wanted behaviour. 
// For this use case, Zig provides overflow operators.
// Normal  Wrapping
// + 	    +%
// - 	    -%
// * 	    *%
// += 	    +%=
// -= 	    -%=
// *= 	    *%=
test "well defined overflow" {
    var a: u8 = 255;
    a +%= 1;
    try expect(a == 0);
}


////////// FLOAT RULES /////////////////////////////////////////////////////////////////////////////
// https://ziglearn.org/chapter-1/#floats


////////// MAIN ////////////////////////////////////////////////////////////////////////////////////

pub fn main() void {
    // NOTE: Unused local variables will throw compiler errors!


}
