////////// INIT ////////////////////////////////////////////////////////////////////////////////////
const expect = @import("std").testing.expect; // `expect` == `assert`



////////// COMPILER DIRECTIVES /////////////////////////////////////////////////////////////////////

///// Comptime /////
// Blocks of code may be forcibly executed at compile time using the comptime keyword. 
test "comptime blocks" {
    // In this example, the variables x and y are equivalent.
    var x = comptime fibonacci(10);
    _ = x;

    var y = comptime blk: {
        break :blk fibonacci(10);
    };
    _ = y;
}


///// Comptime Params /////
// Function parameters in Zig can be tagged as being comptime. This means that the value passed 
// to that function parameter must be known at compile time. 

fn Matrix(
    // Let’s make a function that returns a type. Notice how this function is PascalCase, as it returns a type.
    comptime T: type,
    comptime width: comptime_int,
    comptime height: comptime_int,
) type {
    return [height][width]T;
}

test "returning a type" {
    try expect(Matrix(f32, 4, 4) == [4][4]f32);
}


///// Comptime + Reflection /////
// We can reflect upon types using the built-in `@typeInfo`, which takes in a type and returns a tagged union. 
// This tagged union type can be found in `std.builtin.TypeInfo`
fn addSmallInts(comptime T: type, a: T, b: T) T {
    return switch (@typeInfo(T)) {
        .ComptimeInt => a + b,
        .Int => |info| if (info.bits <= 16)
            a + b
        else
            @compileError("ints too large"),
        else => @compileError("only ints accepted"),
    };
}

test "typeinfo switch" {
    const x = addSmallInts(u16, 20, 30);
    try expect(@TypeOf(x) == u16);
    try expect(x == 50);
}