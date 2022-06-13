////////// INIT ////////////////////////////////////////////////////////////////////////////////////
const expect = @import("std").testing.expect; // `expect` == `assert`

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

const h = [5]u8{ 'h', 'e', 'l', 'l', 'o' }; // Arrays are denoted by [N]T, where N is the number of elements 
//in the array and T is the type of those elements (i.e., the array’s child type).

const i = [_]u8{ 'w', 'o', 'r', 'l', 'd' }; // For array literals, N may be replaced by `_` to infer the size of the array.



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
    var d: u8 = 255;
    d +%= 1;
    try expect(d == 0);
}


////////// FLOAT RULES /////////////////////////////////////////////////////////////////////////////
// Zig’s floats are strictly IEEE compliant unless `@setFloatMode(.Optimized)` is used, 
// which is equivalent to GCC’s `-ffast-math`. Floats coerce to larger float types.

test "##### Float Widening #####" {
    const e: f16 = 0;
    const f: f32 = a;
    const g: f128 = b;
    _ = f;
    try expect(g == @as(f128, e));
}

// Floats support multiple kinds of literal.
const floating_point:        f64 = 123.0E+77;
const another_float:         f64 = 123.0;
const yet_another:           f64 = 123.0e+77;
const hex_floating_point:    f64 = 0x103.70p-5;
const another_hex_float:     f64 = 0x103.70;
const yet_another_hex_float: f64 = 0x103.70P-5;

// Underscores may also be placed between digits.
const lightspeed: f64 = 299_792_458.000_000;
const nanosecond: f64 = 0.000_000_001;
const more_hex:   f64 = 0x1234_5678.9ABC_CDEFp-10;

// Integers and floats may be converted using the built-in functions `@intToFloat` and `@floatToInt` 
// @intToFloat is always safe, whereas `@floatToInt` is detectable illegal behaviour if the float 
// value cannot fit in the integer destination type.

test "int-float conversion" {
    const a: i32 = 0;
    const b = @intToFloat(f32, a);
    const c = @floatToInt(i32, b);
    try expect(c == a);
}


////////// SPECIAL EXPRESSIONS /////////////////////////////////////////////////////////////////////

///// Labelled Blocks /////
// Blocks in Zig are expressions and can be given labels, which are used to yield values. 
// Blocks yield values, meaning that they can be used in place of a value. 
// The value of an empty block `{}` is a value of the type `void`.

test "labelled blocks" {
    const count = blk: { //Here, we are using a label called `blk`
        var sum: u32 = 0;
        var i: u32 = 0;
        while (i < 10) : (i += 1) sum += i;
        break :blk sum;
    };
    const inc_op = pp: {
        const tmp = i;
        i += 1;
        break :pp tmp;
    }
    try expect(count == 45);
    try expect(@TypeOf(count) == u32);
}

///// Labelled Loops /////
// Loops can be given labels, allowing you to break and continue to outer loops.
test "nested continue" {
    var count: usize = 0;
    outer: for ([_]i32{ 1, 2, 3, 4, 5, 6, 7, 8 }) |_| {
        for ([_]i32{ 1, 2, 3, 4, 5 }) |_| {
            count += 1;
            continue :outer;
        }
    }
    try expect(count == 8);
}


///// Loops as Expressions /////
// Like `return`, `break` accepts a value. This can be used to yield a value from a loop. 
// Loops in Zig also have an `else` branch on loops, which is evaluated when the loop is not exited from with a break.

fn rangeHasNumber(begin: usize, end: usize, number: usize) bool {
    var i = begin;
    return while (i < end) : (i += 1) {
        if (i == number) {
            break true;
        }
    } else false;
}

test "while loop expression" {
    try expect(rangeHasNumber(0, 10, 3));
}


///// Optionals /////

// Optionals use the syntax `?T` and are used to store the data null, or a value of type `T`.
test "optional" {
    var found_index: ?usize = null;
    const data = [_]i32{ 1, 2, 3, 4, 5, 6, 7, 8, 12 };
    for (data) |v, i| {
        if (v == 10) found_index = i;
    }
    try expect(found_index == null);
}


test "##### `orelse` #####" {
    var a: ?f32 = null;
    // Optionals support the orelse expression, which acts when the optional is null. 
    var b = a orelse 0; // This unwraps the optional to its child type.
    try expect(b == 0);
    try expect(@TypeOf(b) == f32);
}


test "##### orelse unreachable #####" {
    const a: ?f32 = 5;
    const b = a orelse unreachable;
    //`.?` is a shorthand for `orelse unreachable`. 
    const c = a.?; // This is used for when you know it is impossible for an optional value to be null, 
    // -------------- and using this to unwrap a null value is detectable illegal behaviour.
    try expect(b == c);
    try expect(@TypeOf(c) == f32);
}


///// Optional Payload Capture /////
// https://ziglearn.org/chapter-1/#optionals




////////// MAIN ////////////////////////////////////////////////////////////////////////////////////

pub fn main() void {
    // NOTE: Unused local variables will throw compiler errors!


}
