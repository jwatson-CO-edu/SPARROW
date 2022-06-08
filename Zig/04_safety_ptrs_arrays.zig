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



////////////////////////////////////////////////////////////////////////////////////////////////////
////////// MAIN, UNUSED: ONLY HERE FOR COMPILATION PURPOSES ////////////////////////////////////////
pub fn main() void {
// NOTE: Unused local variables will throw compiler errors!
}