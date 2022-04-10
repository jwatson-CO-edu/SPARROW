# Petri Language  
An interpreted language written in [Nim](https://nim-lang.org/), with the goals of learning language design and introducing straightforward, automated parallelism to the computing world.

## `DEV PLAN`
Task Key:  
* `T:` Test to Conduct
* `Q:` Question to Answer
    - `A:` Answer to a Question
* `E:` Evaluation to Consider  

Completion Key:
* `[Y]` Done
* `[N]` Will NOT Be Done
* `[~]` Partially Done and/or Skipped for Now
```
### Phase 0, Nim Language: COMPLETE! ###
[Y] Finish Nim tuts, 2022-03-24
    [Y] Part 1: Basics, 2022-03-23
    [Y] Part 2: Classes and Exceptions, 2022-03-24
    [Y] Part 3: Metaprogramming and Macros, 2022-03-24

### Phase 1, Basic Function and Preliminaty Tests ###
[Y] Translate first evaluator to Nim, 2022-03-30, Great Success!
    [Y] All Functions Implemented, 2022-03-30, However there is a segfault in `append` that has 
        revealed some misunderstandings with pointers, see below.
    [Y] All Tests, Pointer Learning, 2022-03-30, Dropped pointers, using `ref object`s instead
        [Y] DLL Heap and Stress Tests (Concepts folder), 2022-04-02,
        [Y] SPARROW debugging, 2022-03-30, Now using smart refs instead of pointers
            [N] Refactor?, 2022-03-30, Not Needed, However need to learn when to use a `ref object`
        [Y] T: All tests pass, 2022-03-30, Added tests for untested functions in Cpp
    [Y] Q: How big is each `Atom`?, 2022-03-30, Answer seems wrong
            Size of a number: 8
            Size of a NULL:   8
            Size of a String: 8
            Size of a cons:   8
            Size of an error: 8, Is this because they are all references? 64bit address?
[Y] When to use object -vs- ref object?, 2022-04-03, Listened to some helpful YouTube tutorials
    * User-Defined Types
        - Tuple:   Stack, collection of unnamed fields in which only type and order matter, Copy by value    , Can be accessed by index
        - Object:  Stack, collection of named   fields that must be accessed by name      , Copy by value    , Thread safe
        - Ref Obj: Heap , collection of named   fields that must be accessed by name      , Copy by reference, NOT thread safe (v1.6.4)
    * Tuts:
        - Objects: _____ https://www.youtube.com/watch?v=aME-tyPCPvE
        - Ref Objects: _ https://www.youtube.com/watch?v=kkSAVKKIoVc
        - Obj. Variants: https://www.youtube.com/watch?v=fi7UasgzFhQ
    [N] D: Evaluate plain object refactor, 2022-04-03,  Atoms and variables will be stored in the `Env` and accessed/used elsewhere, 
           so it would not be efficient to copy by value
[ ] Translate "The Little Javascripter" by Douglas Crockford
    https://www.crockford.com/little.html (See `JS` folder for reference implementation)
    [ ] Adapt JS file to match names used in Nim file
    [ ] Make changes to reference implementation when necessary and/or convenient, That is use Nim advantages when they present themselves
[ ] Parse "Easy S-Expressions" instead: 
    [ ] Implicit open paren, `;` is close paren
    [ ] <funcName> <arg1> ... <argN>;
    [ ] Program Blocks: All of `{}`, `()`, `[]` 
    [ ] Verify that statement precedence with block brackets already exists in evaluator, If not then Implement it! 
    [ ] Special Blocks, _<prefix> immediately followed by block brackets
        [] `_m{}`: Math block, infix operators are allowed
    [ ] Line Continuation: `\+`    
[ ] Evaluate "The Seasoned Schemer" for useful features and structures (If yes, then expand this bullet with REQUIRED topics ONLY )
[ ] Experiment with memory models
    [ ] Q: Which is faster? `object` -or- `ref object`
        [ ] T: Copy -vs- Reference Simple Test in folder 05
        [ ] T: Attempt simple conversion of SPARROW to copy mode
    [ ] Q: Is selective compilation possible in Nim?
        [ ] T: If so, test a program that needs it to be compatible w/ Windows
    [ ] Read: How does Nim allocate memory?
    [ ] Q: Can a block of "null pointer" memory be allocated?
    [ ] Q: Can a large array of variant `Atoms` be allocated? What is the per-unit size in memory?
    [ ] T: Which is faster; (Pre-allocated block -vs- Dynamic vars); Create 1000 vars and assign randomly for 10k steps
[ ] Evaluate "Practical Common Lisp" (PCL) for useful features and structures (If yes, then expand this bullet with REQUIRED topics ONLY )

### Phase 2, Feature Research ###
[ ] Q: What are the use cases? (Need more)
    A: Standing up a small network of devices and automatically and cleanly giving them their own processes
        - R: Must be able to talk to Python
            * Q: Does it make sense for FINCH to do data plumbing for Python
    A: Offloading multiprocessing work from Python
[Y] Q: Where do I introduce type checking?, 2022-04-10, A: In the compile-time functions --> before a script is executed
[ ] Collect flow programming requirments
    [ ] Petri Reading Program: Read selected papers in the FINCH collection while digging into available flow programming systems
    [ ] What are Node Red's capabilities?
[ ] Q: What are ORC and ARC?
[ ] Q: Does Nim target LLVM yet? Is there another intermediate representation instead?
[ ] Type System
    [ ] Q: How does the Nim type system work?
    [ ] User-defined types? -and/or- Entity Component System (ECS) with interfaces?
        [ ] Evaluate Entity Component System ([Polymorph](https://github.com/rlipsc/polymorph))
        [ ] Alternatives to ECS?
        [ ] Alternatives to OOP?
[ ] Composite Types
    [ ] Arrays? - Access block evaluates to an index
    [ ] Hashes?
    [ ] Dynamic Arrays?
    [ ] Heterogeneous (Python) Arrays? (Unlikely for static typing focus)
[ ] Concurrency Model & Scheduling
    [ ] Nim Threads   in Linux and Windows
    [ ] Nim Processes in Linux and Windows
    [ ] READ: "Actors" by Gul Agha
    [ ] Petri Representation? (Value Dependency/History Graph)
[ ] Pharo (SmallTalk) Mooc (Listen only)
    [ ] Evaluate save states

### Phase 3, Static Expansion ###
[ ] Split Interpreter into Functional Areas
    [ ] Components:  Basic types that also compose AST
    [ ] Analyzer:    Run before interpretation
    [ ] Interpreter: REPL frontend, Core execution backend, invokes analyzer 
    [ ] Transpiler:  Turn interpreter code into Nim code that never wastes time on type checks

### Phase 4, Parallel Concepts ###
[ ] Types of Sync
    [ ] Hard Sync: Must wait for an updated value
    [ ] Soft Sync: Retrieves the current value as soon as the mutex allows, regardless of whether or not it is ``fresh''
[ ] Publisher/Subscriber
    [ ] Push Model w Callbacks
    [ ] Pull Model
[ ] Tick/Lockstep
[ ] How much of ROS do I need?
    
### Phase 5, Parallel Expansion ###
[ ] Value Dependency Graph
[ ] Interpreter Program
    [ ] Inter-process connections
[ ] Parallel Test 1
    [ ] Choose Task
    [ ] Write and Test Task
        [ ] Single process
        [ ] Multi-Process
    [ ] Run Comparison Test
    [ ] Document Results
[ ] Petri Rep for Parallel Test 1
[ ] Auto parallelize Parallel Test 1
    [ ] Same Conclusion?
    
### Phase 6, Optimization ###
[ ] Transpile back to Nim --> C --> Binary (See Daydream paper notes)
```

## (Possible) Names
* Language: [F]lexible [I]nterpreted [N]ode [C]omputing [H]elper
    - Birds are cool
* Execution Model: [B]asic [O]peration e[X]change for [F]lows [A]agents and [B]ehaviors
    - Possible extension to behavior trees
    - An execution model based on Petri Nets that seeks to support commonsense solutions to Job Shop Scheduling problems as they relate to computing
* Interpreter: BOX? HOUSE? PERCH? (Either a riff on BOXFAB or birds)


# Daydreams
* Enhanced alternative to Behavior Trees
    - Q: How to retain reactivity in a Graph?
        - Q: Is there a PN flavor that has a conditional similar to a BT::Selector node? 
          A: 2022-04-10: Yes, there is an option transition.  Needs more investigation
    - Q: Does the introduction of cycles and loops create significant problems.
    - Enhanced representation of state with Colored and/or Object Petri Nets
    - Q: State operations like {GPS, BT::HashedExpression}?
* Simulations
    - "Tick" regulation across nodes
    - Constructivist simulation: Program flow follows fluid flow
    - Bond Graphs, Circuit Sim, Pipe Flow
        - Source, Sink, Resistance, Power, Energy Conversions
    - Can the mechanisms that regulate execution be used to regulate simulation
* Compilation
    - Target LLVM
* Execution System
    - Automatic network compute discovery 
    - Automatic load balancing
    - FINCH-OS, a (realtime?) operating system with advanced task scheduling
    - Node Red capabilities
* Mathematics / Statistics
    - Stochastic Programming
    - Deep Learning
    - Geometric Algebra
* AI
    - Plug-and-Play learning appliances (Brain Book)
        - Reason over domain structures in order to re-use capabilities or suggest the structure of new capabilities
    - DL auto-tuning of interpreter parameters
* Hardware
    - Home/Life automation
    - Hardware Emulation
        - CHIP8, Fantasy CPU
        - Zilog 80A, RadioShack TRS-80 / Sinclair ZX Spectrum
        - Ricoh 5A22, SNES CPU
        - Super FX Chip, SNES Cartridge Coprocessor
           - SNES 3D Art
        - The ability to use instantiate Finch nodes while using the message passing bus as a data bus, thus acheiving full system emulation
        - The ability to compile the graph of chip emulators down into a full system emulator using the normal compression and compilation pipeline
            - Emulate SNES from a ROM!

# Resources

## Links

## Future Applications
* [Flow-Based Programming For Machine Learning](https://assets.researchsquare.com/files/rs-707294/v1_covered.pdf), 2021 Mahapatra
