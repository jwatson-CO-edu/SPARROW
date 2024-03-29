// https://ziglearn.org/chapter-1/#if

//     Values can be ignored by using `_` in place of a variable or const declaration. 
// This does not work at the global scope (i.e. it only works inside functions and blocks), 
// and is useful for ignoring the values returned from functions if you do not need them.

////////// INIT ////////////////////////////////////////////////////////////////////////////////////
const expect = @import("std").testing.expect; // `expect` == `assert`
const eql    = @import("std").mem.eql;



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


test "##### Switch Statement #####" {
    // Zig’s switch works as both a statement and an expression. The types of all branches must 
    // coerce to the type which is being switched upon. All possible values must have an associated branch;
    // values cannot be left out. Cases cannot fall through to other branches.
    var x: i8 = 10;
    switch (x) {
        -1...1 => {
            x = -x;
        },
        10, 100 => {
            //special considerations must be made
            //when dividing signed integers
            x = @divExact(x, 10);
        },
        else => {}, // The `else` is required to satisfy the exhaustiveness of this switch.
    }
    try expect(x == 1);
}


test "##### Switch Expression #####" { // Here is the former, but as a switch expression.
    var x: i8 = 10;
    x = switch (x) { // Setting a value with `switch`
        -1...1 => -x,
        10, 100 => @divExact(x, 10),
        else => x,
    };
    try expect(x == 1);
}


////////// LOOPS ///////////////////////////////////////////////////////////////////////////////////

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


///// Inline Loops /////
// `inline` loops are unrolled, and allow some things to happen which only work at compile time. 
// Using these for performance reasons is inadvisable unless you’ve tested that explicitly unrolling is faster; 
// the compiler tends to make better decisions here than you.

test "inline for" { // Here we use a for, but a while works similarly.
    const types = [_]type{ i32, f32, u8, bool };
    var sum: usize = 0;
    inline for (types) |T| sum += @sizeOf(T);
    try expect(sum == 10);
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

test "##### Function Definition #####" {
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

test "##### Recursion #####" {
    const x = fibonacci(10);
    try expect(x == 55);
}


///// Returning Unions /////

// Functions often return error unions. 
fn failingFunction() error{Oops}!void {
    // A function that always fails, used for examples
    return error.Oops;
}

test "returning an error" {
    failingFunction() catch |err| { // |err| syntax receives the value of the error. 
        // --------------------------- This is called payload capturing
        try expect(err == error.Oops);
        return;
    };
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

test "error union" {
    const maybe_error: AllocationError!u16 = 10; // Create an error union with `!`
    const no_error = maybe_error catch 0;

    try expect(@TypeOf(no_error) == u16);
    try expect(no_error == 10);
}

///// `try x` Idiom /////
// `try x` is a shortcut for `x catch |err| return err`, and is commonly used in places where 
// handling an error isn’t appropriate. Zig’s try and catch are *unrelated* to try-catch in other languages.

fn failFn() error{Oops}!i32 {
    try failingFunction();
    return 12;
}

test "##### `try x` Idiom #####" {
    var v = failFn() catch |err| {
        try expect(err == error.Oops);
        return;
    };
    try expect(v == 12); // is never reached
}


///// errdefer /////
// errdefer works like defer, but only executing when the function is returned from 
// with an error inside of the errdefer’s block.

var problems: u32 = 98;

fn failFnCounter() error{Oops}!void {
    errdefer problems += 1; // Action to perform on error
    try failingFunction(); //- Thing to try
}

test "errdefer" {
    failFnCounter() catch |err| {
        try expect(err == error.Oops);
        try expect(problems == 99);
        return;
    };
}


///// Inferred Error Unions /////
// Error unions returned from a function can have their error sets inferred by not having an 
// explicit error set. This inferred error set contains all possible errors which the function may return.

fn createFile() !void { // Blank space unioned with `void`
    return error.AccessDenied;
}

test "inferred error set" {
    //type coercion successfully takes place
    const x: error{AccessDenied}!void = createFile();

    //Zig does not let us ignore error unions via _ = x;
    //we must unwrap it with "try", "catch", or "if" by any means
    _ = x catch {};
}


///// Merging Error Sets /////
const A = error{ NotDir, PathNotFound };
const B = error{ OutOfMemory, PathNotFound };
const C = A || B;


///// `anyerror` Note /////
// `anyerror` is the global error set which due to being the superset of all error sets, 
// can have an error from any set coerce to a value of it. Its usage should be generally avoided



////////// PAYLOAD CAPTURES ////////////////////////////////////////////////////////////////////////
// https://ziglearn.org/chapter-1/#payload-captures
// Payload captures use the syntax `|value|`. Wherever they appear, 
// they are used to “capture” the value from something, and return it.


test "##### Optional-If Payload #####" { // Playload Capture with if statements and optionals.
    var maybe_num: ?usize = 10;
    if (maybe_num) |n| {
        try expect(@TypeOf(n) == usize);
        try expect(n == 10);
    } else {
        unreachable;
    }
}


test "##### While Optional Payload #####" { // With while loops and optionals. This may have an else block.
    var i: ?u32 = 10;
    while (i) |num| : (i.? -= 1) {
        try expect(@TypeOf(num) == u32);
        if (num == 1) {
            i = null;
            break;
        }
    }
    try expect(i == null);
}


///// Error Union Capture /////
// With while loops and error unions. The else with the error capture is required here.

var numbers_left2: u32 = undefined;

fn eventuallyErrorSequence() !u32 {
    return if (numbers_left2 == 0) error.ReachedZero else blk: {
        numbers_left2 -= 1;
        break :blk numbers_left2;
    };
}

test "while error union capture" {
    var sum: u32 = 0;
    numbers_left2 = 3;
    while (eventuallyErrorSequence()) |value| {
        sum += value;
    } else |err| {
        try expect(err == error.ReachedZero);
    }
}


test "##### for Capture #####" {
    const x = [_]i8{1, 5, 120, -5};
    for (x) |v| try expect(@TypeOf(v) == i8);
}


///// Switch cases on tagged unions /////

const Info = union(enum) {
    a: u32,
    b: []const u8,
    c,
    d: u32,
};

test "switch capture" {
    var b = Info{ .a = 10 };
    const x = switch (b) {
        .b => |str| blk: {
            try expect(@TypeOf(str) == []const u8);
            break :blk 1;
        },
        .c => 2,
        //if these are of the same type, they
        //may be inside the same capture group
        .a, .d => |num| blk: {
            try expect(@TypeOf(num) == u32);
            break :blk num * 2;
        },
    };
    try expect(x == 20);
}


///// Pointer Capture /////
// As we saw in the Union and Optional sections above, values captured with the `|val|` syntax are immutable 
// (similar to function arguments), but we can use pointer capture to modify the original values. 
// This captures the values as pointers that are themselves still immutable, but because the value is now a pointer, 
// we can modify the original value by dereferencing it:

test "for with pointer capture" {
    var data = [_]u8{1, 2, 3};
    for (data) |*byte| byte.* += 1;
    try expect(eql(u8, &data, &[_]u8{2, 3, 4}));
}




////////////////////////////////////////////////////////////////////////////////////////////////////
////////// MAIN, UNUSED: ONLY HERE FOR COMPILATION PURPOSES ////////////////////////////////////////
pub fn main() void {
// NOTE: Unused local variables will throw compiler errors!
}
