// https://ziglearn.org/chapter-1/#enums

////////// INIT ////////////////////////////////////////////////////////////////////////////////////
const expect = @import("std").testing.expect; // `expect` == `assert`


////////// ENUMS ///////////////////////////////////////////////////////////////////////////////////

///// Declare Enum /////
const Direction = enum { north, south, east, west };
// Enums types may have specified (integer) tag types.
const Value = enum(u2) { zero, one, two };

test "enum ordinal value" {
    try expect(@enumToInt(Value.zero) == 0);
    try expect(@enumToInt(Value.one) == 1);
    try expect(@enumToInt(Value.two) == 2);
}

///// Enum Value Override /////
// Values can be overridden, with the next values continuing from there.
const Value2 = enum(u32) {
    hundred = 100,
    thousand = 1000,
    million = 1000000,
    next,
};

test "set enum ordinal value" {
    try expect(@enumToInt(Value2.hundred) == 100);
    try expect(@enumToInt(Value2.thousand) == 1000);
    try expect(@enumToInt(Value2.million) == 1000000);
    try expect(@enumToInt(Value2.next) == 1000001);
}


///// Enum Functions /////
// Methods can be given to enums. These act as namespaced functions that can be called with dot syntax.

const Suit = enum {
    clubs,
    spades,
    diamonds,
    hearts,
    pub fn isClubs(self: Suit) bool {
        return self == Suit.clubs;
    }
};

test "enum method" {
    try expect(Suit.spades.isClubs() == Suit.isClubs(.spades));
}


///// Instance Enums: const/var /////
// Enums can also be given var and const declarations. These act as namespaced globals, 
// and their values are unrelated and unattached to instances of the enum type.

const Mode = enum { // Make this enum a prototype
    var count: u32 = 0; // This can be set by each instance
    on,
    off,
};

test "hmm" {
    Mode.count += 1;
    try expect(Mode.count == 1);
}


////////// STRUCTS /////////////////////////////////////////////////////////////////////////////////
// Structs are Zig’s most common kind of composite data type, 
// allowing you to define types that can store a fixed set of named fields. 
// Zig gives no guarantees about the in-memory order of fields in a struct, or its size. 
// Like arrays, structs are also neatly constructed with T{} syntax.


///// Struct Declaration /////
const Vec3 = struct {
    x: f32, 
    y: f32, 
    z: f32
};

test "##### Struct Usage #####" {
    const my_vector = Vec3{
        .x = 0,
        .y = 100,
        .z = 50,
    };
    _ = my_vector;
}


test "##### Missing Struct Field #####" {
    // All fields must be given a value.
    const my_vector = Vec3{
        .x = 0,
        .z = 50,
    };
    _ = my_vector;
}

///// Struct Field Defaults /////
const Vec4 = struct {
    x: f32, 
    y: f32, 
    z: f32 = 0, 
    w: f32 = undefined
};

test "struct defaults" {
    const my_vector = Vec4{
        .x = 25,
        .y = -50,
    };
    _ = my_vector;
}

///// Struct Members /////
// Like enums, structs may also contain functions and declarations.
// Structs have the unique property that when given a pointer to a struct, 
// one level of dereferencing is done automatically when accessing fields. 

const Stuff = struct {
    x: i32,
    y: i32,
    // Notice how in this example, self.x and self.y are accessed in the swap function 
    // without needing to dereference the self pointer.
    fn swap(self: *Stuff) void {
        const tmp = self.x;
        self.x = self.y;
        self.y = tmp;
    }
};

test "automatic dereference" {
    var thing = Stuff{ .x = 10, .y = 20 };
    thing.swap();
    try expect(thing.x == 20);
    try expect(thing.y == 10);
}


///// Struct Formatting /////
// Let’s create a type with custom formatting by giving it a format function. This function must be 
// marked as pub so that `std.fmt` can access it. You may notice the usage of {s} instead of {} - 
// this is the format specifier for strings. This is used here as {} defaults to array printing over string printing.

const Person = struct {
    name: []const u8,
    birth_year: i32,
    death_year: ?i32,
    pub fn format(
        self: Person,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s} ({}-", .{
            self.name, self.birth_year,
        });

        if (self.death_year) |year| {
            try writer.print("{}", .{year});
        }

        try writer.writeAll(")");
    }
};

test "custom fmt" {
    const john = Person{
        .name = "John Carmack",
        .birth_year = 1970,
        .death_year = null,
    };

    const john_string = try std.fmt.allocPrint(
        test_allocator,
        "{s}",
        .{john},
    );
    defer test_allocator.free(john_string);

    try expect(eql(
        u8,
        john_string,
        "John Carmack (1970-)",
    ));

    const claude = Person{
        .name = "Claude Shannon",
        .birth_year = 1916,
        .death_year = 2001,
    };

    const claude_string = try std.fmt.allocPrint(
        test_allocator,
        "{s}",
        .{claude},
    );
    defer test_allocator.free(claude_string);

    try expect(eql(
        u8,
        claude_string,
        "Claude Shannon (1916-2001)",
    ));
}



///// Anonymous Structs //////////////////////////
// https://ziglearn.org/chapter-1/#anonymous-structs
// The struct type may be omitted from a struct literal. These literals may coerce to other struct types.

test "##### anonymous struct literal #####" {
    const Point = struct { x: i32, y: i32 };

    var pt: Point = .{
        .x = 13,
        .y = 67,
    };
    try expect(pt.x == 13);
    try expect(pt.y == 67);
}


test "##### fully anonymous struct #####" {
    // Anonymous structs may be completely anonymous i.e. without being coerced to another struct type.
    try dump(.{
        .int = @as(u32, 1234),
        .float = @as(f64, 12.34),
        .b = true,
        .s = "hi",
    });
}

fn dump(args: anytype) !void {
    try expect(args.int == 1234);
    try expect(args.float == 12.34);
    try expect(args.b);
    try expect(args.s[0] == 'h');
    try expect(args.s[1] == 'i');
}


///// Tuples /////////////////////////////////////
// Anonymous structs without field names may be created, and are referred to as tuples. 
// These have many of the properties that arrays do; tuples can be iterated over, indexed, 
// can be used with the `++` and `**` operators, and have a `len` field. 
// Internally, these have numbered field names starting at "0", 
// which may be accessed with the special syntax `@"0"` which acts as an escape for the syntax.
// Things inside `@""` are always recognised as identifiers.

test "tuple" {
    const values = .{
        @as(u32, 1234),
        @as(f64, 12.34),
        true,
        "hi",
    } ++ .{false} ** 2;
    try expect(values[0] == 1234);
    try expect(values[4] == false);
    // An `inline` loop must be used to iterate over the tuple here, as the type of each tuple field may differ.
    inline for (values) |v, i| {
        if (i != 2) continue;
        try expect(v);
    }
    try expect(values.len == 6);
    try expect(values.@"3"[0] == 'h');
}


////////// UNIONS /////////////////////////////////////////////////////////////////////////////////
// Zig’s unions allow you to define types which store one value of many possible typed fields; 
// only one field may be active at one time.
// Bare union types do not have a guaranteed memory layout. Because of this, 
// bare unions cannot be used to reinterpret memory. 
// Accessing a field in a union which is not active is detectable illegal behaviour.

///// Declare a Union /////
const Result = union {
    int: i64,
    float: f64,
    bool: bool,
};


///// Tagged Union /////
// Tagged unions are unions which use an enum to detect which field is active. 
// Here we make use of payload capturing again, to switch on the tag type of a union 
// while also capturing the value it contains. Here we use a pointer capture; captured values are immutable, 
// but with the |*value| syntax we can capture a pointer to the values instead of the values themselves. 
// This allows us to use dereferencing to mutate the original value.

const Tag    = enum { a, b, c };
const Tagged = union(Tag) { a: u8, b: f32, c: bool };

// The tag type of a tagged union can also be inferred. This is equivalent to the Tagged type above.
const Tagged2 = union(enum) { a: u8, b: f32, c: bool };


test "switch on tagged union" {
    var value = Tagged{ .b = 1.5 };
    switch (value) {
        .a => |*byte| byte.* += 1,
        .b => |*float| float.* *= 2,
        .c => |*b| b.* = !b.*,
    }
    try expect(value.b == 3);
}



////////// COMPTIME TYPES //////////////////////////////////////////////////////////////////////////

///// @Type /////

// We can use the `@Type` function to create a type from a `@typeInfo`. 
// `@Type` is implemented for most types but is notably unimplemented for enums, unions, functions, and structs.
fn GetBiggerInt(comptime T: type) type {
    // Here anonymous struct syntax is used with .{}, because the T in T{} can be inferred. 
    //In this example we will get a compile error if the Int tag isn’t set.
    return @Type(.{
        .Int = .{
            .bits = @typeInfo(T).Int.bits + 1,
            .signedness = @typeInfo(T).Int.signedness,
        },
    });
}

test "@Type" {
    try expect(GetBiggerInt(u8) == u9);
    try expect(GetBiggerInt(i31) == i32);
}


///// Data Structures /////
// Returning a struct type is how you make generic data structures in Zig. The usage of `@This` 
// is required here, which gets the type of the innermost struct, union, or enum. 


fn Vec(
    comptime count: comptime_int,
    comptime T: type,
) type {
    return struct {
        data: [count]T,
        const Self = @This();

        fn abs(self: Self) Self {
            var tmp = Self{ .data = undefined };
            for (self.data) |elem, i| {
                tmp.data[i] = if (elem < 0)
                    -elem
                else
                    elem;
            }
            return tmp;
        }

        fn init(data: [count]T) Self {
            return Self{ .data = data };
        }
    };
}

const eql = @import("std").mem.eql; // Here `std.mem.eql` is also used which compares two slices.

test "generic vector" {
    const x = Vec(3, f32).init([_]f32{ 10, -10, 5 });
    const y = x.abs();
    try expect(eql(f32, &y.data, &[_]f32{ 10, 10, 5 }));
}


///// Type Inferrence /////
// The types of function parameters can also be inferred by using anytype in place of a type. 
// `@TypeOf` can then be used on the parameter.
fn plusOne(x: anytype) @TypeOf(x) {
    return x + 1;
}

test "inferred function parameter" {
    try expect(plusOne(@as(u32, 1)) == 2);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
////////// MAIN, UNUSED: ONLY HERE FOR COMPILATION PURPOSES ////////////////////////////////////////
pub fn main() void {
// NOTE: Unused local variables will throw compiler errors!
}