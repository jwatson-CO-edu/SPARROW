# Design Principles

* The Partial Order: FINCHes Fly Freely
    - Choose fun over efficiency
    - Choose ergonomics over safety
    - Choose capability over safety
    - Choose efficiency over formalism
    - Choose speed efficiency over space efficiency; Assume enough RAM, but don't be Greedy
    - Choose freedom over security
    - Choose Optimization over Compilation
    
* Parallel processing must be easy and intuitive

* Optimization > Compilation
    - Optimization should produce human-readable and human-tunable code
    - The decisions that govern optimization should be tunable
    - The syntax for un-optimized and optimized code should be the same, However
    - Greater literacy unlocks finer control --> Optimized code does NOT have to be easy (for beginners) to read
* Target Intel i5-XXXX by default, but make the environment tunable
    - Cache Page Size <-- Stack must fit on one page
        * Unlimited heap?
    - Max simultaneous processes

# Implementation
## Decisions
* `Atom` struct contains members to support *all* primitive types, `(-)` Wastes space, `(+)` Makes prototyping easier
* Free variables in a `std::vector`
* Bound variables in a `std::map`
* When there is no obvious return value, a form should return the `OKAY` Error
* Truthiness
    - `Null` is False
    - `OKAY` error is True

## TODO
* Free memory of all created atoms!
* "Interchangeable" Data Types
    - `std`
        * https://en.cppreference.com/w/cpp/utility/any,  
        https://www.cppstories.com/2018/06/any/,  
        https://www.nextptr.com/tutorial/ta1571648512/stdany-comparison-with-void-and-motivating-examples,  
        https://devblogs.microsoft.com/cppblog/stdany-how-when-and-why/
            - https://en.cppreference.com/w/cpp/types/is_arithmetic
            - https://stackoverflow.com/questions/55057937/find-an-element-in-stdvector-of-stdany
            - https://stackoverflow.com/questions/3224915/how-to-print-boostany-to-a-stream
        - https://en.cppreference.com/w/cpp/utility/variant
        
    - Boost
        - https://www.boost.org/doc/libs/1_64_0/doc/html/variant/misc.html#:~:text=Variant%20provides%20compile%2Dtime%20checked,Boost.
        - https://www.boost.org/doc/libs/1_38_0/doc/html/variant.html
        - https://www.boost.org/doc/libs/1_77_0/doc/html/boost/any.html
* Reimplement `Atom` as the chosen variable type



## Ideas
* Array type
* Dynamic Array Type
    - Each section contains a C array of length X
    - Cons another section onto list as needed `(+)`
    - `(-)` Lookup not constant, but `(+)` n/X lookup time
* Random Variable Type: Whenever this varable is evaluated, sample from the distribution specified at var creation time
* Prototype Classes
    - Instances are thin copies of an original prototype
* Hash Objects: Each object is just an environment with a name lookup
    - Interface YES
    - Inheritance NO
* Interface typing > Inheritance: Make promises instead of structures

* Node: Extended Cons, Possible basis for objects?
    - N pointers by default
        - dat:0, nxt:1, prv:2
        - car:0, cdr:1
        - up:0 , dn:1 , lf:2 , rt:3
        - nr:0 , so:1 , es:2 , we:3
    - Can I use alignment to alias an array of pointers to the above names?

* DIY Memory Management
    - Begin interpreter by allocating a massive memory block
    - Use "placement new" to write objects directly to a memory address
        - https://stackoverflow.com/a/1554808
        - https://isocpp.org/wiki/faq/dtors#placement-new
    - You are now able to control where things are placed
    - Memory Cells
        * Remember cells from Comp Sys Malloc Lab
            - length
            - ptr to next cell
            - ptr to last cell
            - live/dead flag
            - contents --> beginning address of cell contents is returned by a malloc
    - Separate variable storage for quick access
        - Atom Store
            * Choose an alignment length --> Do not allow access in between alignment width
        - Node and/or Object Store

    - GC is tunable
        * Control options: Time? Bytes?

* Fanciful Machine: A virtual machine, but more fun
    - Scratch Registers: Split the difference between ASM and interpreted language
        * Named variables that are always available in all parts of every program
        * Interpreters can write to registers of other interpreters
    - Keywords that allow the user to de/allocate memory on their own
    - Allow ASM-flavored memory calcs
    - Tracked processes with scheduling

# Concepts
* Execution is a job-shop problem where interpreters are workers
    - JIT compiler outputs a job schedule instead of bytecode
* One process per interpreter
* Spin up new interpreters as efficiencies are identified
* User-tunable parameters
    - Time (ms) when parallelism is a net gain
* Learning Interpreter
    - Collect stats on code snippets
    - Learn to recognize efficiencies
    - Parameter search on scalar settings
    - Graph learning on syntax

# Future
* Transpile to C/++
* Compile to bytecode
* Treat remote jobs the same as local jobs
* Shared memory between interpreter units
