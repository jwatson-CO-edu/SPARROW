# Design Principles

* The Partial Order: Fly Freely
    - Choose fun over efficiency
    - Choose ergonomics over safety
    - Choose capability over safety
    - Choose efficiency over formalism
    - Choose speed efficiency over space efficiency; Assume enough RAM, but don't be Greedy
    - Choose freedom over security
    - Choose Optimization over Compilation
    


* Petri Net defines execution
    - Think about transforms instead of stacks
    - Everything is global, unless explicitly nested
    - Allow both flow and text programming paradigms,  
    *but flow runs in the background*
    - Parallel processing must be easy and intuitive

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
    * Current
        - Cons
        - Double
        - C-string
        - Error
        - Null
* Free variables in a `std::vector`
* Bound variables in a `std::map`
* When there is no obvious return value, a form should return the `OKAY` Error
* Truthiness
    - `Null` is False
    - `OKAY` error is True

# TODO
1. Free memory of all created atoms!
1. Make `Atom` as small as possible using unions
1. Drop `char*` and reimplement c-strings as LISP lists
1. Investigate `std::thread` / `std::jthread` (C++11 or later)

# Dev Plan
1. Follow L.I.S.P. book until satisfied
1. Implement memory model
1. Implement Petri model
1. In*tra*-host parallelism
1. In*ter*-host parallelism
1. Send It

## Guidelines
* Test as you go
* Benchmark as you go, especially after you leave LISP

# Petri Concept
* Instructions
    - Token? `==` Fragment: An partial environment
        * Some subset of the entire envioronment
        * May create vars
        * May act on vars
    - Place?: An node between processes that evaluates fragments
        * Fragment Requirements
        * Un/met
        * Can well allow multiple fragment sets to accumulate? --> If so, need something like a structure to keep bundles of satisfactory fragments together?
            - How can the programmer violate bundle requirments?
            - Should the place be allowed to resolve?
            - Is there a way to implement per-bundle synchronization?, When is this necessary?
                * Literature should have something to say about bottlenecks
    - Process?: A node that transforms fragments
        * Compute cost
    - Places and processes can probably go in the Instruction Memory
    - All programs begin at the origin node with an empty environment

* Execution
    - Where to analyze dependencies?
    - Allow object contents to span fragments --> Execution structure may not mimic data structure
    - Two fragments can occupy the same place?
        * Satisfy conditions with any fragment combination

* Optimization
    - Examine cost of splitting places
    - Examine cost of splitting processes
    - Examine cost of splitting fragments
    - Split
        - If different subsequences operate on different names in the fragment, then it is a candidate for parallelization

# Automated Parallelism
* Fragment may not span workers
* One worker per fragment?
* May a worker carry more than one fragment?
* What does it mean when interpreter resources are shared among workers?

## Architecture
* How to share state between intepreters?
    - Can automated synchonization be created?
    - Is resource modeling required?
* How to manage intepreters on the same host?
* How to manage intepreters across hosts?





# Ideas
* Atoms
    - Cons --> Pair: For ergonomics
    - Word Union
        - Long Int
    - Half Union
        - Integer
        - Float
        - Character(s)
    - Fragment assignment?
* Array type
* Dynamic Array Type
    - Each section contains a C array of length X
    - Cons another section onto list as needed `(+)`
    - `(-)` Lookup not constant, but `(+)` n/X lookup time
* Random Variable Type: Whenever this varable is evaluated, sample from the distribution specified at var creation time

* Interface typing > Inheritance: Make promises instead of structures

* Object:
    * Prototype Classes
        - Instances are thin copies of an original prototype
    * Hash Objects: Each object is just an environment with a name lookup
        - Interface YES
        - Inheritance NO
    * Allow the user to explicitly erase objects before the GC gets to it

* Node: Extended Cons, Possible basis for objects?
    - `N` pointers by default, tunable
        - dat:0, nxt:1, prv:2
        - car:0, cdr:1
        - up:0 , dn:1 , lf:2 , rt:3
        - nr:0 , so:1 , es:2 , we:3
        - Can I use alignment to alias an array of pointers to the above names?
    - `>N` pointers in a dynamic array

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
    - Data Memory: Separate variable storage for quick access
        - Atom Store
            * Choose an alignment length --> Do not allow access in between alignment width
        - Node Store
        - Object Store
    - Instruction Memory
        * Instruction Nodes belong here


    - GC is tunable
        * Control options: Time? Bytes?
        * GC can be disabled

* Fanciful Machine: A virtual machine, but more fun
    - Scratch Registers: Split the difference between ASM and interpreted language
        * Named variables that are always available in all parts of every program
        * Interpreters can write to registers of other interpreters
    - Keywords that allow the user to de/allocate memory on their own
    - Allow ASM-flavored memory calcs
    - Tracked processes with scheduling
    - Instruction pointer: Points to the current Node in the AST currently executed
    - Petri guarantees are invalidated where instructions access the FM registers

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

# Questions
* Can a variable belong to more than one fragment?

# Future
* Transpile to C/++
* Compile to bytecode
* Treat remote jobs the same as local jobs
* Shared memory between interpreter units

# Spare Parts
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
* Instruction Node: Unit of currency for Petri-Net AST
    - Operation
    - Operands
        - `N` pointers by default, tunable
        - `>N` pointers in a dynamic array
    - Stored in instruction memory
    - How to handle the case when the number of instructions exceeds inst memory?