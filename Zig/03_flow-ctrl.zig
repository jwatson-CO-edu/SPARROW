// https://ziglearn.org/chapter-1/#if

//     Values can be ignored by using `_` in place of a variable or const declaration. 
// This does not work at the global scope (i.e. it only works inside functions and blocks), 
// and is useful for ignoring the values returned from functions if you do not need them.

////////// INIT ////////////////////////////////////////////////////////////////////////////////////
const expect = @import("std").testing.expect; // `expect` == `assert`




////////// CONDITIONALS && FLOW ////////////////////////////////////////////////////////////////////

test "##### If-Else #####" {
    const a = true;
    var x: u16 = 0;
    if (a) {
        x += 1;
    } else {
        x += 2;
    }
    try expect(x == 1);
}

test "##### Ternary #####" {
    const a = true;
    var x: u16 = 0;
    x += if (a) 1 else 2;
    try expect(x == 1);
}

test "##### While #####" {
    var i: u8 = 2;
    while (i < 100) {
        i *= 2;
    }
    try expect(i == 128);
}

test "##### While w/ Update Expression #####" {
    var sum: u8 = 0;
    var i: u8 = 1; //  vvvvvv---- Update every loop
    while (i <= 10) : (i += 1) {
        sum += i;
    }
    try expect(sum == 55);
}

test "##### Continue #####" {
    var sum: u8 = 0;
    var i: u8 = 0;
    while (i <= 3) : (i += 1) {
        if (i == 2) continue; // Skip rest of loop
        sum += i;
    }
    try expect(sum == 4);
}

test "##### Break #####" {
    var sum: u8 = 0;
    var i: u8 = 0;
    while (i <= 3) : (i += 1) {
        if (i == 2) break;
        sum += i;
    }
    try expect(sum == 1);
}


test "########## For, different forms ##########" {
    //character literals are equivalent to integer literals
    const string = [_]u8{ 'a', 'b', 'c' };

    for (string) |character, index| {
        _ = character;
        _ = index;
    }

    for (string) |character| {
        _ = character;
    }

    for (string) |_, index| {
        _ = index;
    }

    for (string) |_| {}
}


test "##### Defer #####" {
// Defer is used to execute a statement while exiting the current block.
    var x: i16 = 5;
    {
        defer x += 2;
        try expect(x == 5);
    }
    try expect(x == 7);
}

test "##### Multi-Defer #####" {
// When there are multiple defers in a single block, they are executed in reverse order.
    var x: f32 = 5;
    {
        defer x += 2;
        defer x /= 2;
    }
    try expect(x == 4.5);
}



////////// FUNCTIONS ///////////////////////////////////////////////////////////////////////////////

///// Function Definition /////

fn addFive(x: u32) u32 {
    return x + 5;
}

test "function" {
    const y = addFive(0);
    try expect(@TypeOf(y) == u32);
    try expect(y == 5);
}


///// Recursion /////
// When recursion happens, the compiler is no longer able to work out the maximum stack size. 
// This may result in unsafe behaviour - a stack overflow.
fn fibonacci(n: u16) u16 {
    if (n == 0 or n == 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

test "function recursion" {
    const x = fibonacci(10);
    try expect(x == 55);
}



////////// ERROR HANDLING //////////////////////////////////////////////////////////////////////////
// https://ziglearn.org/chapter-1/#errors

// An error set is like an enum (details on Zig’s enums later), where each error in the set is a value. 
// There are no exceptions in Zig; errors are values. 

const FileOpenError = error{ // Error Enum
    AccessDenied,
    OutOfMemory,
    FileNotFound,
};

const AllocationError = error{OutOfMemory}; // Error sets coerce to their supersets.

test "coerce error from a subset to a superset" {
    const err: FileOpenError = AllocationError.OutOfMemory;
    try expect(err == FileOpenError.OutOfMemory);
}

// An error set type and a normal type can be combined with the `!` operator to form an error union type. 
// Values of these types may be an error value, or a value of the normal type.

pub fn main() void {
// NOTE: Unused local variables will throw compiler errors!
}
