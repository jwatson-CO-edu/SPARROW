////////// INIT ////////////////////////////////////////////////////////////////////////////////////
const std    = @import("std");
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


///// Concatenation at **comptime** /////
// Comptime also introduces the operators `++` and `**` for concatenating and repeating arrays and slices. 
// **These operators do not work at runtime.**
test "++" {
    const x: [4]u8 = undefined;
    const y = x[0..];

    const d: [6]u8 = undefined;
    const e = a[0..];

    const new = y ++ e;
    _ = d;
    try expect(new.len == 10);
}

test "**" {
    const pattern = [_]u8{ 0xCC, 0xAA };
    const memory = pattern ** 3;
    try expect(eql(
        u8,
        &memory,
        &[_]u8{ 0xCC, 0xAA, 0xCC, 0xAA, 0xCC, 0xAA }
    ));
}

////////// VECTORS /////////////////////////////////////////////////////////////////////////////////
// https://ziglearn.org/chapter-1/#vectors
// Zig provides vector types for SIMD. These are not to be conflated with vectors in a mathematical sense, 
//or vectors like C++’s std::vector (for this, see “Arraylist” in chapter 2). 
// Vectors may be created using the `@Type` built-in we used earlier, and `std.meta.Vector` provides a shorthand for this.

// Vectors can only have child types of booleans, integers, floats and pointers.

// Operations between vectors with the same child type and length can take place. These operations 
// are performed on each of the values in the vector.std.meta.eql is used here to check for equality 
// between two vectors (also useful for other types like structs).

// It is worth noting that using explicit vectors may result in slower software if you do not make the right decisions - 
// the compiler’s auto-vectorisation is fairly smart as-is.

const meta = @import("std").meta;
const Vector = meta.Vector;

test "##### vector add #####" {
    const x: Vector(4, f32) = .{ 1, -10, 20, -1 };
    const y: Vector(4, f32) = .{ 2, 10, 0, 1 };
    const z = x + y;
    try expect(meta.eql(z, Vector(4, f32){ 3, 0, 20, 0 }));
}


test "##### vector indexing #####" {
    const x: Vector(4, u8) = .{ 255, 0, 255, 0 };
    try expect(x[0] == 255);
}


///// Splat /////
// The built-in function `@splat` may be used to construct a vector where all of the values are the same. 
// Here we use it to multiply a vector by a scalar.

test "vector * scalar" {
    const x: Vector(3, f32) = .{ 12.5, 37.5, 2.5 };
    const y = x * @splat(3, @as(f32, 2));
    try expect(meta.eql(y, Vector(3, f32){ 25, 75, 5 }));
}


///// Vector Looping /////
// Vectors do not have a `len` field like arrays, but may still be looped over. 
// Here, `std.mem.len` is used as a shortcut for `@typeInfo(@TypeOf(x)).Vector.len`.
const len = @import("std").mem.len;

test "##### vector looping #####" {
    const x = Vector(4, u8){ 255, 0, 255, 0 };
    var sum = blk: {
        var tmp: u10 = 0;
        var j: u8 = 0;
        while (j < len(x)) : (j += 1) tmp += x[j];
        break :blk tmp;
    };
    try expect(sum == 510);
}

///// Vector Coercion /////
// Vectors coerce to their respective arrays.
const arr: [4]f32 = @Vector(4, f32){ 1, 2, 3, 4 };



////////// Arraylist ///////////////////////////////////////////////////////////////////////////////
// https://ziglearn.org/chapter-2/#arraylist
// The `std.ArrayList` is commonly used throughout Zig, and serves as a buffer which can change in size. 
// `std.ArrayList(T)` is similar to C++’s `std::vector<T>` and Rust’s `Vec<T>`. The `deinit()` method 
// frees all of the ArrayList’s memory. The memory can be read from and written to via its slice field: `.items`

const eql = std.mem.eql;
const ArrayList = std.ArrayList;
// Here we will introduce the usage of the testing allocator. This is a special allocator that 
// only works in tests, and can detect memory leaks. In your code, use whatever allocator is appropriate.
const test_allocator = std.testing.allocator;

test "#### ArrayList #####" {
    var list = ArrayList(u8).init(test_allocator);
    defer list.deinit();
    try list.append('H');
    try list.append('e');
    try list.append('l');
    try list.append('l');
    try list.append('o');
    try list.appendSlice(" World!");

    try expect(eql(u8, list.items, "Hello World!"));
}


////////// HASH MAPS ///////////////////////////////////////////////////////////////////////////////
// The standard library provides `std.AutoHashMap`, which lets you easily create a hash map type 
// from a key type and a value type. These must be initiated with an allocator.
// `std.StringHashMap` and `std.AutoHashMap` are just wrappers for `std.HashMap`. 
// If these two do not fulfil your needs, using `std.HashMap` directly gives you much more control.

// If having your elements backed by an array is wanted behaviour, try `std.ArrayHashMap` 
// and its wrapper `std.AutoArrayHashMap`.


test "##### hashing #####" {
    const Point = struct { x: i32, y: i32 };

    // `std.AutoHashMap( <KEY TYPE>, <VALUE TYPE> )`
    var map = std.AutoHashMap(u32, Point).init(
        test_allocator,
    );
    defer map.deinit();

    // Let’s put some values in a hash map.
    try map.put(1525, .{ .x = 1, .y = -4 });
    try map.put(1550, .{ .x = 2, .y = -3 });
    try map.put(1575, .{ .x = 3, .y = -2 });
    try map.put(1600, .{ .x = 4, .y = -1 });

    try expect(map.count() == 4);

    var sum = Point{ .x = 0, .y = 0 };
    var iterator = map.iterator();

    while (iterator.next()) |entry| {
        sum.x += entry.value_ptr.x;
        sum.y += entry.value_ptr.y;
    }

    try expect(sum.x == 10);
    try expect(sum.y == -10);
}


test "##### fetchPut #####" {
    var map = std.AutoHashMap(u8, f32).init(
        test_allocator,
    );
    defer map.deinit();

    try map.put(255, 10);
    // `.fetchPut` puts a value in the hash map, returning a value if there was previously a value for that key.
    const old = try map.fetchPut(255, 100);

    try expect(old.?.value == 10);
    try expect(map.get(255).? == 100);
}


test "##### string hashmap #####" {
    // `std.StringHashMap` is also provided for when you need strings as keys.
    var map = std.StringHashMap(enum { cool, uncool }).init(
        test_allocator,
    );
    defer map.deinit();

    try map.put("loris", .uncool);
    try map.put("me", .cool);

    try expect(map.get("me").? == .cool);
    try expect(map.get("loris").? == .uncool);
}


////////// STACKS //////////////////////////////////////////////////////////////////////////////////
// `std.ArrayList` provides the methods necessary to use it as a stack.

test "##### stack #####" {
    const string = "(()())";
    var stack = std.ArrayList(usize).init(
        test_allocator,
    );
    defer stack.deinit();

    const Pair = struct { open: usize, close: usize };
    var pairs = std.ArrayList(Pair).init(
        test_allocator,
    );
    defer pairs.deinit();

    for (string) |char, u| {
        if (char == '(') try stack.append(u);
        if (char == ')')
            try pairs.append(.{
                .open = stack.pop(),
                .close = u,
            });
    }

    for (pairs.items) |pair, u| {
        try expect(std.meta.eql(pair, switch (u) {
            0 => Pair{ .open = 1, .close = 2 },
            1 => Pair{ .open = 3, .close = 4 },
            2 => Pair{ .open = 0, .close = 5 },
            else => unreachable,
        }));
    }
}



////////// SORTING /////////////////////////////////////////////////////////////////////////////////
// The standard library provides utilities for in-place sorting slices. 
// `std.sort.asc` and `.desc` create a comparison function for the given type at comptime; 
// if non-numerical types should be sorted, the user must provide their own comparison function.
// `std.sort.sort` has a best case of O(n), and an average and worst case of `O(n*log(n))`.

test "##### sorting #####" {
    var data = [_]u8{ 10, 240, 0, 0, 10, 5 };
    std.sort.sort(u8, &data, {}, comptime std.sort.asc(u8));
    try expect(eql(u8, &data, &[_]u8{ 0, 0, 5, 10, 10, 240 }));
    std.sort.sort(u8, &data, {}, comptime std.sort.desc(u8));
    try expect(eql(u8, &data, &[_]u8{ 240, 10, 10, 5, 0, 0 }));
}



////////// ITERATORS ///////////////////////////////////////////////////////////////////////////////
// It is a common idiom to have a struct type with a next function with an optional in its return type, 
// so that the function may return a `null` to indicate that iteration is finished.

test "##### split iterator #####" {
    const text = "robust, optimal, reusable, maintainable, ";
    // `std.mem.SplitIterator` (and the subtly different `std.mem.TokenIterator`) is an example of this pattern.
    var iter = std.mem.split(u8, text, ", ");
    try expect(eql(u8, iter.next().?, "robust"));
    try expect(eql(u8, iter.next().?, "optimal"));
    try expect(eql(u8, iter.next().?, "reusable"));
    try expect(eql(u8, iter.next().?, "maintainable"));
    try expect(eql(u8, iter.next().?, ""));
    try expect(iter.next() == null);
}

///// Iterator Unpacking and Looping /////
// Some iterators have a `!?T` return type, as opposed to `?T`. `!?T` requires that we unpack 
// the error union before the optional, meaning that the work done to get to the next iteration may error. 

test "iterator looping" {
    // `cwd` has to be opened with iterate permissions in order for the directory iterator to work.
    var iter = (try std.fs.cwd().openDir(
        ".",
        .{ .iterate = true },
    )).iterate();

    var file_count: usize = 0;
    while (try iter.next()) |entry| {
        if (entry.kind == .File) file_count += 1;
    }

    try expect(file_count > 0);
}


///// Custom Iterator /////
// This will iterate over a slice of strings, yielding the strings which contain a given string.

const ContainsIterator = struct {
    strings: []const []const u8,
    needle: []const u8,
    index: usize = 0,
    fn next(self: *ContainsIterator) ?[]const u8 {
        const index = self.index;
        for (self.strings[index..]) |string| {
            self.index += 1;
            if (std.mem.indexOf(u8, string, self.needle)) |_| {
                return string;
            }
        }
        return null;
    }
};

test "custom iterator" {
    var iter = ContainsIterator{
        .strings = &[_][]const u8{ "one", "two", "three" },
        .needle = "e",
    };

    try expect(eql(u8, iter.next().?, "one"));
    try expect(eql(u8, iter.next().?, "three"));
    try expect(iter.next() == null);
}



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
    const f: i32 = 0;
    const g = @intToFloat(f32, f);
    const l = @floatToInt(i32, g);
    try expect(l == f);
}


////////// SPECIAL EXPRESSIONS /////////////////////////////////////////////////////////////////////

///// Labelled Blocks /////
// Blocks in Zig are expressions and can be given labels, which are used to yield values. 
// Blocks yield values, meaning that they can be used in place of a value. 
// The value of an empty block `{}` is a value of the type `void`.

test "labelled blocks" {
    const count = blk: { //Here, we are using a label called `blk`
        var sum: u32 = 0;
        var m: u32 = 0;
        while (m < 10) : (m += 1) sum += m;
        break :blk sum;
    };
    const inc_op = pp: {
        const tmp = i;
        i += 1;
        break :pp tmp;
    };
    _ = inc_op;
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
    var n = begin;
    return while (n < end) : (n += 1) {
        if (n == number) {
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
    for (data) |v, o| {
        if (v == 10) found_index = o;
    }
    try expect(found_index == null);
}


test "##### `orelse` #####" {
    var p: ?f32 = null;
    // Optionals support the orelse expression, which acts when the optional is null. 
    var q = p orelse 0; // This unwraps the optional to its child type.
    try expect(q == 0);
    try expect(@TypeOf(q) == f32);
}


test "##### orelse unreachable #####" {
    const r: ?f32 = 5;
    const s = r orelse unreachable;
    //`.?` is a shorthand for `orelse unreachable`. 
    const t = s.?; // This is used for when you know it is impossible for an optional value to be null, 
    // -------------- and using this to unwrap a null value is detectable illegal behaviour.
    try expect(b == t);
    try expect(@TypeOf(t) == f32);
}



////////// OPAQUE //////////////////////////////////////////////////////////////////////////////////
// https://ziglearn.org/chapter-1/#opaque
// `opaque` types in Zig have an unknown (albeit non-zero) size and alignment. 
// Because of this these data types cannot be stored directly. 
// These are used to maintain type safety with pointers to types that we don’t have information about.
// The typical usecase of opaque is to maintain type safety when interoperating with 
// C code that does not expose complete type information.

const Window = opaque {};
const Button = opaque {};

extern fn show_window1(*Window) callconv(.C) void;

test "opaque" {
    var main_window: *Window = undefined;
    show_window1(main_window);

    var ok_button: *Button = undefined;
    show_window1(ok_button);
}

// Opaque types may have declarations in their definitions (the same as structs, enums and unions).

const Window2 = opaque {
    fn show(self: *Window) void {
        show_window1(self);
    }
};

extern fn show_window2(*Window2) callconv(.C) void;

test "opaque with declarations" {
    var main_window: *Window2 = undefined;
    main_window.show_window2();
}

////////// MAIN ////////////////////////////////////////////////////////////////////////////////////

pub fn main() void {
    // NOTE: Unused local variables will throw compiler errors!


}
