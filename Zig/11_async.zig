////////// INIT ////////////////////////////////////////////////////////////////////////////////////
const std    = @import("std");
const expect = std.testing.expect;



////////// ASYNC ///////////////////////////////////////////////////////////////////////////////////
// A functioning understanding of Zig’s async requires familiarity with the concept of the call stack.

// A traditional function call comprises of three things:

//     Initiate the called function with its arguments, pushing the function’s stack frame
//     Transfer control to the function
//     Upon function completion, hand control back to the caller, retrieving the function’s return value and popping the function’s stack frame

// With Zig’s async functions we can do more than this, with the transfer of control being an ongoing 
// two-way conversation (i.e. we can give control to the function and take it back multiple times). 
// Because of this, special considerations must be made when calling a function in an async context; 
// we can no longer push and pop the stack frame as normal (as the stack is volatile, and things 
// “above” the current stack frame may be overwritten), instead explicitly storing the async function’s frame. 
// While most people won’t make use of its full feature set, this style of async is useful for 
// creating more powerful constructs such as event loops.

// The style of Zig’s async may be described as suspendible stackless coroutines.
//  Zig’s async is very different to something like an OS thread which has a stack, 
// and can only be suspended by the kernel. Furthermore, Zig’s async is there to provide you with 
// control flow structures and code generation; async does not imply parallelism or the usage of threads.


////////// Suspend / Resume ////////////////////////////////////////////////////////////////////////
// In the previous section we talked of how async functions can give control back to the caller, 
// and how the async function can later take control back. 
// This functionality is provided by the keywords `suspend`, and `resume`. When a function suspends, 
// control flow returns to wherever it was last resumed; when a function is called via an async invocation, 
// this is an implicit `resume`.

// The comments in these examples indicate the order of execution. 
// There are a few things to take in here:

    // The async keyword is used to invoke functions in an async context.
    // async func() returns the function’s frame.
    // We must store this frame.
    // The resume keyword is used on the frame, whereas suspend is used from the called function.


///// Matched Suspend/Resume /////
// In well formed code, each suspend is matched with a resume.

var bar: i32 = 1;

test "suspend with resume" {
    var frame = async func2();  //1
    resume frame;               //4
    try expect(bar == 3);       //6
}

fn func2() void {
    bar += 1;                   //2
    suspend {}                  //3
    bar += 1;                   //5
}
