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


////////// MAIN ////////////////////////////////////////////////////////////////////////////////////

pub fn main() void {
    // NOTE: Unused local variables will throw compiler errors!


}
