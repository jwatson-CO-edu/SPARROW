////////// INIT ////////////////////////////////////////////////////////////////////////////////////
const expect = @import("std").testing.expect; // `expect` == `assert`

////////// SAFETY //////////////////////////////////////////////////////////////////////////////////
// https://ziglearn.org/chapter-1/#runtime-safety
// Zig provides a level of safety, where problems may be found during execution. 
// Safety can be left on, or turned off. Zig has many cases of so-called detectable illegal behaviour, 
// meaning that illegal behaviour will be caught (causing a panic) with safety on, but will result 
// in undefined behaviour with safety off. Users are strongly recommended to develop and test their 
// software with safety on, despite its speed penalties.

// Safety is off for some build modes. // FIXME: IS SAFETY DISABLED FOR `run`?

test "##### Out of Bounds #####" {
    @setRuntimeSafety(true);
    const a = [3]u8{ 1, 2, 3 };
    var index: u8 = 5;
    const b = a[index]; // FIXME: WHY DID THIS NOT CAUSE AN ERROR AT COMPILE TIME?
    _ = b;
}


test "##### Unreachable #####" {
    // `unreachable` is an assertion to the compiler that this statement will not be reached. 
    // It can be used to tell the compiler that a branch is impossible, which the optimiser can then 
    // take advantage of. Reaching an `unreachable` is detectable illegal behaviour.
    const x: i32 = 1;
    const y: u32 = if (x == 2) 5 else unreachable; // As it is of the type noreturn, 
    //                                                it is compatible with all other types. 
    //                                                Here it coerces to u32.
    _ = y; // Satisfy unusued local constant error
}

fn asciiToUpper(x: u8) u8 {
    return switch (x) {
        'a'...'z' => x + 'A' - 'a',
        'A'...'Z' => x,
        else => unreachable,
    };
}

test "##### Unreachable Switch #####" {
    try expect( asciiToUpper('a') == 'A' );
    try expect( asciiToUpper('A') == 'A' );
}



////////// POINTERS ////////////////////////////////////////////////////////////////////////////////
// https://ziglearn.org/chapter-1/#pointers
// Normal pointers in Zig aren’t allowed to have 0 or null as a value. 
// They follow the syntax *T, where T is the child type.
// Referencing is done with &variable, and dereferencing is done with variable.

fn increment(num: *u8) void { // Function takes an unsigned byte as an argument
    num.* += 1;
}

test "##### Pointers #####" {
    var x: u8 = 1;
    increment(&x);
    try expect(x == 2);
}

test "##### Zero Pointer Assignment #####" {
    var x: u16 = 0;
    var y: *u8 = @intToPtr(*u8, x); // FIXME: YOU SHOULD NOT BE ABLE TO ASSIGN POINTER TO ZERO
    _ = y;
}


test "##### Const Pointers #####" {
    // Zig also has const pointers, which cannot be used to modify the referenced data. 
    // Referencing a const variable will yield a const pointer.
    const x: u8 = 1;
    var y = &x;
    y.* += 1; // FIXME: YOU SHOULD NOT BE ABLE TO DO THIS
}

test "##### `usize`: The size of a pointer #####" {
    try expect(@sizeOf(usize) == @sizeOf(*u8));
    try expect(@sizeOf(isize) == @sizeOf(*u8));
}

///// Array Pointer /////
// Sometimes you may have a pointer to an unknown amount of elements. 
// [*]T is the solution for this, which works like *T but also supports indexing syntax, 
// pointer arithmetic, and slicing. Unlike *T, it cannot point to a type which does not have a known size. 
// *T coerces to [*]T

///// Slices /////
// Slices can be thought of as a pair of `[*]T` (the pointer to the data) and a `usize` (the element count). 
// Their syntax is given as []T, with T being the child type. Slices are used heavily throughout Zig 
// for when you need to operate on arbitrary amounts of data. Slices have the same attributes as pointers, 
// meaning that there also exists const slices. For loops also operate over slices. 
// String literals in Zig coerce to []const u8.

// The syntax `x[n..m]` is used to create a slice from an array. This is called slicing, and creates 
// a slice of the elements starting at `x[n]` and ending at `x[m - 1]`. This example uses a const 
// slice as the values which the slice points to do not need to be modified.

fn total(values: []const u8) usize {
    var sum: usize = 0;
    for (values) |v| sum += v;
    return sum;
}

test "slices" {
    const array = [_]u8{ 1, 2, 3, 4, 5 };
    const slice = array[0..3];
    try expect(total(slice) == 6);
}


test "slices 2" {
    const array = [_]u8{ 1, 2, 3, 4, 5 };
    // When these `n` and `m` values are both known at compile time, slicing will actually produce a 
    // pointer to an array. This is not an issue as a pointer to an array i.e. `*[N]T` will coerce to a `[]T`.
    const slice = array[0..3];
    try expect(@TypeOf(slice) == *const [3]u8);
}


test "slices 3" {
    var array = [_]u8{ 1, 2, 3, 4, 5 };
    var slice = array[0..]; // The syntax `x[n..]` can also be used for when you want to slice to the end.
    _ = slice;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
////////// MAIN, UNUSED: ONLY HERE FOR COMPILATION PURPOSES ////////////////////////////////////////
pub fn main() void {
// NOTE: Unused local variables will throw compiler errors!
}