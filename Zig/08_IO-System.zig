////////// INIT ////////////////////////////////////////////////////////////////////////////////////
const std            = @import("std");
const expect         = std.testing.expect;
const test_allocator = std.testing.allocator;
const eql            = std.mem.eql;
const ArrayList      = std.ArrayList;


////////// STRING / DATA FORMATTING ////////////////////////////////////////////////////////////////
// `std.fmt` provides ways to format data to and from strings.


test "##### fmt #####" {
    // A basic example of creating a formatted string. The format string must be known at compile time.
    const string = try std.fmt.allocPrint(
        test_allocator,
        "{d} + {d} = {d}", // The d here denotes that we want a decimal number.
        .{ 9, 10, 19 },
    );
    defer test_allocator.free(string);

    try expect(eql(u8, string, "9 + 10 = 19"));
}


test "##### print #####" {
    // Writers conveniently have a print method, which works similarly.
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();
    try list.writer().print(
        "{} + {} = {}",
        .{ 9, 10, 19 },
    );
    try expect(eql(u8, list.items, "9 + 10 = 19"));
}


test "##### stdout writer #####" {
    const out_file = std.io.getStdOut();
    try out_file.writer().print(
        "Hello, {s}!\n",
        .{"World"},
    );
}


test "##### array printing #####" {
    const string = try std.fmt.allocPrint(
        test_allocator,
        // We have used the {s} format specifier up until this point to print strings. 
        // Here we will use {any}, which gives us the default formatting.
        "{any} + {any} = {any}",
        .{
            @as([]const u8, &[_]u8{ 1, 4 }),
            @as([]const u8, &[_]u8{ 2, 5 }),
            @as([]const u8, &[_]u8{ 3, 9 }),
        },
    );
    defer test_allocator.free(string);

    try expect(eql(
        u8,
        string,
        "{ 1, 4 } + { 2, 5 } = { 3, 9 }",
    ));
}



////////// FILESYSTEM //////////////////////////////////////////////////////////////////////////////
// Let’s create and open a file in our current working directory, write to it, and then read from it. 

test "##### createFile, write, seekTo, read #####" {
    const file = try std.fs.cwd().createFile(
        "junk_file.txt",
        .{ .read = true },
    );
    defer file.close();

    const bytes_written = try file.writeAll("Hello File!");
    _ = bytes_written;

    var buffer: [100]u8 = undefined;
    // Here we have to use .seekTo in order to go back to the start of the file before reading what we have written.
    try file.seekTo(0);
    const bytes_read = try file.readAll(&buffer);

    try expect(eql(u8, buffer[0..bytes_read], "Hello File!"));
}


test "##### file stat #####" {
    const file = try std.fs.cwd().createFile(
        "junk_file2.txt",
        .{ .read = true },
    );
    defer file.close();
    // We can get various information about files by using `.stat()` on them.
    // Stat also contains fields for `.inode` and `.mode`
    const stat = try file.stat();
    try expect(stat.size == 0);
    try expect(stat.kind == .File);
    try expect(stat.ctime <= std.time.nanoTimestamp());
    try expect(stat.mtime <= std.time.nanoTimestamp());
    try expect(stat.atime <= std.time.nanoTimestamp());
}


test "##### make dir #####" {
    // We can make directories and iterate over their contents.  
    try std.fs.cwd().makeDir("test-tmp");
    const dir = try std.fs.cwd().openDir(
        "test-tmp",
        .{ .iterate = true },
    );
    defer {
        std.fs.cwd().deleteTree("test-tmp") catch unreachable;
    }

    _ = try dir.createFile("x", .{});
    _ = try dir.createFile("y", .{});
    _ = try dir.createFile("z", .{});

    var file_count: usize = 0;
    // Here we will use an iterator. This directory (and its contents) will be deleted after this test finishes.
    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind == .File) file_count += 1;
    }

    try expect(file_count == 3);
}


test "##### io writer usage #####"{
    var list = ArrayList(u8).init(test_allocator);
    defer list.deinit();
    // `std.io.Writer` and `std.io.Reader` provide standard ways of making use of IO. 
    // `std.ArrayList(u8)` has a writer method which gives us a writer. Let’s use it.
    const bytes_written = try list.writer().write(
        "Hello World!",
    );
    try expect(bytes_written == 12);
    try expect(eql(u8, list.items, "Hello World!"));
}


///// Read Line /////
// A common usecase for readers is to read until the next line (e.g. for user input). 
// Here we will do this with the `std.io.getStdIn()` file.

fn nextLine(reader: anytype, buffer: []u8) !?[]const u8 {
    var line = (try reader.readUntilDelimiterOrEof(
        buffer,
        '\n',
    )) orelse return null;
    // trim annoying windows-only carriage return character
    if (@import("builtin").os.tag == .windows) {
        return std.mem.trimRight(u8, line, "\r");
    } else {
        return line;
    }
}

test "read until next line" {
    const stdout = std.io.getStdOut();
    const stdin = std.io.getStdIn();

    try stdout.writeAll(
        \\ Enter your name:
    );

    var buffer: [100]u8 = undefined;
    const input = (try nextLine(stdin.reader(), &buffer)).?;
    try stdout.writer().print(
        "Your name is: \"{s}\"\n",
        .{input},
    );
}


///// Custom Writer /////
// An `std.io.Writer` type consists of a context type, error set, and a write function. 
// The write function must take in the context type and a byte slice.
// The write function must also return an error union of the Writer type’s error set and the amount 
// of bytes written. 

// Don't create a type like this! Use an
// arraylist with a fixed buffer allocator
const MyByteList = struct {
    // Let’s create a type that implements a writer.
    data: [100]u8 = undefined,
    items: []u8 = &[_]u8{},

    const Writer = std.io.Writer(
        *MyByteList,
        error{EndOfBuffer},
        appendWrite,
    );

    fn appendWrite(
        self: *MyByteList,
        data: []const u8,
    ) error{EndOfBuffer}!usize {
        if (self.items.len + data.len > self.data.len) {
            return error.EndOfBuffer;
        }
        std.mem.copy(
            u8,
            self.data[self.items.len..],
            data,
        );
        self.items = self.data[0..self.items.len + data.len];
        return data.len;
    }

    fn writer(self: *MyByteList) Writer {
        return .{ .context = self };
    }
};

test "custom writer" {
    var bytes = MyByteList{};
    _ = try bytes.writer().write("Hello");
    _ = try bytes.writer().write(" Writer!");
    try expect(eql(u8, bytes.items, "Hello Writer!"));
}



////////// JSON ////////////////////////////////////////////////////////////////////////////////////
// https://ziglearn.org/chapter-2/#json



////////// MAIN ////////////////////////////////////////////////////////////////////////////////////

pub fn main() void {
    // NOTE: Unused local variables will throw compiler errors!


}