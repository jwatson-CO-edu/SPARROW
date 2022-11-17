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


////////// Asynch / Await //////////////////////////////////////////////////////////////////////////

// Similar to how well formed code has a `suspend` for every `resume`, each `async` function invocation 
// with a return value must be matched with an `await`. The value yielded by `await` on the `async` frame 
// corresponds to the function’s return.

fn func3() u32 {
    return 5;
}

test "async / await" {
    // You may notice that `func3` here is a normal function (i.e. it has no suspend points - 
    // it is not an async function). Despite this, `func3` can work as an `async` function when called 
    // from an `async` invocation; the calling convention of `func3` doesn’t have to be changed to `async` - 
    // `func3` can be of any calling convention.
    var frame = async func3();
    try expect(await frame == 5);
}

///// `nosuspend` /////
// When calling a function which is determined to be async (i.e. it may suspend) without an async invocation, 
// the function which called it is also treated as being async. When a function of a concrete (non-async) 
// calling convention is determined to have suspend points, this is a compile error as async requires 
// its own calling convention. This means, for example, that `main` cannot be async.

// If you want to call an `async` function without using an `async` invocation, and without the caller 
// of the function also being `async`, the `nosuspend` keyword comes in handy. This allows the caller 
// of the `async` function to not also be `async`, by asserting that the potential suspends do not happen.

// const std = @import("std");

fn doTicksDuration(ticker: *u32) i64 {
    const start = std.time.milliTimestamp();

    while (ticker.* > 0) {
        suspend {}
        ticker.* -= 1;
    }

    return std.time.milliTimestamp() - start;
}

// pub fn main() !void {
//     var ticker: u32 = 5; // Putting 0 here is detectable illegal behavior
//     const duration = nosuspend doTicksDuration(&ticker);
// }


///// `@Frame` /////

fn add(a: i32, b: i32) i64 {
    return a + b;
}

test "@Frame" {
    // @Frame(function) returns the frame type of the function. This works for async functions, 
    // and functions without a specific calling convention.
    var frame: @Frame(add) = async add(1, 2);
    try expect(await frame == 3);
}


///// `@frame` /////
// https://ziglearn.org/chapter-5/#async-frames-suspend-blocks



////////// Basic Event Loop Implementation /////////////////////////////////////////////////////////
// An event loop is a design pattern in which events are dispatched and/or waited upon. 
// This will mean some kind of service or runtime that resumes suspended async frames when conditions are met. 
// This is the most powerful and useful use case of Zig’s `async`.

// Here we will implement a basic event loop. This one will allow us to submit tasks to be executed 
// in a given amount of time. We will use this to submit pairs of tasks which will print the time 
// since the program’s start. Here is an example of the output.

// This chapter is incomplete and in future should contain usage of std.event.Loop, and evented IO.

// const std = @import("std");

// used to get monotonic time, as opposed to wall-clock time
var timer: ?std.time.Timer = null;
fn nanotime() u64 {
    if (timer == null) {
        timer = std.time.Timer.start() catch unreachable;
    }
    return timer.?.read();
}

// holds the frame, and the nanotime of
// when the frame should be resumed
const Delay = struct {
    frame: anyframe,
    expires: u64,
};

// suspend the caller, to be resumed later by the event loop
fn waitForTime(time_ms: u64) void {
    suspend timer_queue.add(Delay{
        .frame = @frame(),
        .expires = nanotime() + (time_ms * std.time.ns_per_ms),
    }) catch unreachable;
}

fn waitUntilAndPrint(
    time1: u64,
    time2: u64,
    name: []const u8,
) void {
    const start = nanotime();

    // suspend self, to be woken up when time1 has passed
    waitForTime(time1);
    std.debug.print(
        "[{s}] it is now {} ms since start!\n",
        .{ name, (nanotime() - start) / std.time.ns_per_ms },
    );

    // suspend self, to be woken up when time2 has passed
    waitForTime(time2);
    std.debug.print(
        "[{s}] it is now {} ms since start!\n",
        .{ name, (nanotime() - start) / std.time.ns_per_ms },
    );
}

fn asyncMain() void {
    // stores the async frames of our tasks
    var tasks = [_]@Frame(waitUntilAndPrint){
        async waitUntilAndPrint(1000, 1200, "task-pair a"),
        async waitUntilAndPrint(500, 1300, "task-pair b"),
    };
    // |*t| is used, as |t| would be a *const @Frame(...)
    // which cannot be awaited upon
    for (tasks) |*t| await t;
}

// priority queue of tasks
// lower .expires => higher priority => to be executed before
var timer_queue: std.PriorityQueue(Delay, void, cmp) = undefined;
fn cmp(context: void, a: Delay, b: Delay) std.math.Order {
    _ = context;
    return std.math.order(a.expires, b.expires);
}

pub fn main() !void {
    timer_queue = std.PriorityQueue(Delay, void, cmp).init(
        std.heap.page_allocator, undefined
    );
    defer timer_queue.deinit();

    var main_task = async asyncMain();

    // the body of the event loop
    // pops the task which is to be next executed
    while (timer_queue.removeOrNull()) |delay| {
        // wait until it is time to execute next task
        const now = nanotime();
        if (now < delay.expires) {
            std.time.sleep(delay.expires - now);
        }
        // execute next task
        resume delay.frame;
    }

    nosuspend await main_task;
}