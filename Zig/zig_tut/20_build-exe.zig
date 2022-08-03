////////// INIT ////////////////////////////////////////////////////////////////////////////////////
// const std            = @import("std");
// const expect         = std.testing.expect;


////////// Outputting an Executable ////////////////////////////////////////////////////////////////
// The commands `zig build-exe`, `zig build-lib`, and `zig build-obj` can be used to output 
//               executables   ,  libraries       and  objects,       respectively. 
// These commands take in a source file and arguments.

// Some common arguments:

//     --single-threaded, which asserts the binary is single-threaded. This will turn thread safety measures such as mutexes into no-ops.
//     --strip, which removes debug info from the binary.
//     --dynamic, which is used in conjunction with zig build-lib to output a dynamic/shared library.

// Let’s create a tiny hello world. and run `zig build-exe 20_build-exe.zig -O ReleaseSmall --strip`. 


////////// MAIN ////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");

pub fn main() void {
    std.io.getStdOut().writeAll(
        "\nHello World!\n",
    ) catch unreachable;
}