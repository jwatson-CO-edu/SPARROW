# **F**low **I**nterpreter for **N**ovice **C**omputer language **H**acking [FINCH]
An interpreted language written in [D](https://dlang.org/), with the goal of learning language design.  That's it.  
FINCH's ability to solve problems for others is a side-effect.  
Spelled "FINCH" or "Finch".

# Design Goals
1. This is for my own entertainment and edification.
1. Flow-Based & Modular
1. Data construction & mutation > OOP Hierarchy & Polymorphims
1. Ergonimic & Fun to use
1. Begin simply, and add justifiable complexity with caution!

# Purpose & Roadmap
## Phase 1: Learn Computer Language Design via Scheme -- SPARROW
1. Parsing & Syntax
1. Program Blocks
1. Memory Management & Garbage Collection

## Phase 2: Learn Flow-Based Programming -- FINCH
1. Data Production + Mutation + Flow paradigm
1. Flow execution model
    1. Message Passing
    1. Variables are for connecting ports, not primarily for storage
1. Efficient data sharing
1. Event-based programming (Evaluate Need)
    * Event loops as a first class structure?
1. Networking? (Evaluate Need)

## Phase 3: Tune the Machine -- FINCH
1. Memory/Cache Efficiency
1. Execution Efficiency
1. Evaluate Design Choices
    * What makes FINCH fun to use?
    * What makes FINCH hard to use?
    * What would make FINCH more syntactically compact?
    * What would make FINCH use less memory?
    * What would make FINCH use less faster?
        - Find benchmarks and run them
    * Is a virtual/fanciful machine needed?

## Phase 4: Learn Parallel Programming -- FINCH
**ALERT: DO NOT ATTEMPT THIS PHASE UNTIL YOU HAVE ACHIEVED YOUR DEGREE**
1. Select a use-case.  
Possible options:
    * A fun simulation
    * Personal webserver
    * ???
1. Multiprocess
    1. Share data _efficiently_ across nodes
    1. OpenMPI
1. Multi-machine

## Phase 5: Basic Cognitive Architecture -- CORVID
**ALERT: USE ONLY IF THERE ARE JUSTIFIABLE ADVANTAGES**
1. AI-focused dialect of FINCH

## Phase 6: Heavy Simulation -- MAGPIE
**ALERT: USE ONLY IF THERE ARE JUSTIFIABLE ADVANTAGES**
1. A simulation-focused dialect of CORVID

## Phase N: Far Future -- ???

# `DEV PLAN` 
Task Key:  
* `T:` Test to Conduct
* `Q:` Question to Answer
    - `A:` Answer to a Question
* `E:` Evaluation to Consider  

Completion Key:
* `[>]` In Progress
* `[Y]` Done
* `[N]` Will NOT Be Done
* `[~]` Partially Done and/or Skipped for Now

## Phase 1: Learn Computer Language Design via Scheme -- SPARROW
```
[Y] Translate first "ab_" evaluator to Dlang - 2022-09-07: COMPLETE
    [Y] All Functions Implemented - 2022-09-07: Nim had pointer references, so some implementation needed changing
    [Y] All tests pass - 2022-09-07: No Nim tests had been written for variable binding, so these functions were not implemented
    [Y] Q: How big is each `Atom`? - 2022-09-07: 64 bytes for all kinds because strings are pointers
[Y] Translate second "ac_" evaluator to Dlang - 2022-09-08: COMPLETE
    [Y] All Functions Implemented - 2022-09-08: COMPLETE
    [Y] All tests pass - 2022-09-08: Environment bound and unbound variables implemented, are ref counts needed?

[ ] Translate "The Little Javascripter" by Douglas Crockford
    https://www.crockford.com/little.html (See `JS` folder for reference implementation)
    [ ] T: All REPLacement tests pass
    
[ ] Parse "Easy S-Expressions" instead: 2022-04-19, Ignore whitespace except to separate symbols
    [ ] Implicit open paren, `;` is close paren
    [ ] <funcName> <arg1> ... <argN>;
    [ ] T: Parse and run a plaintext file
        [ ] E: File extension: .BRD, .SPW, .FNC,
    [ ] Program Blocks:
        [ ] Curly Braces {}
        [ ] Pythonic / Implicit
    [ ] T: Verify that statement precedence with block brackets already exists in evaluator, If not then Implement it! 
    [ ] T: Make sure that nested statements and blocks do not cause ambiguity
    [ ] Line Continuation: `\+`    
    [ ] T: Translate and repeat all REPLacement tests!
[ ] Evaluate "The Seasoned Schemer" for useful features and structures (If yes, then expand this bullet with REQUIRED topics ONLY )
[ ] Atom Size Reduction
    [ ] Try unions: 1{car, str, num} + 2{cdr, err}
    [ ] T: Size of Union -vs- Speed of computation
        * "Fat Atoms" might be faster if we can assume the components are always there
        * Do "Slim Atoms" offer any performance improvements other than memory usage?
[ ] Experiment with memory models
    [ ] Q: Which is faster? 
        * Pointer
        * Copy
    [ ] Q: Is selective compilation possible in Dlang?
        [ ] T: If so, test a program that requires selective compilation in order to be compatible w/ Windows
    [ ] Read: How does Dlang allocate memory?
    [ ] Q: Can a block of "null pointer" memory be allocated?
    [ ] Q: Can a large array of `Atoms` be allocated? 
    [ ] T: Which is faster; (Pre-allocated block -vs- Dynamic vars); Create 1000 vars and assign randomly for 10k steps
[ ] E: Evaluate "Practical Common Lisp" (PCL) for useful features and structures (If yes, then expand this bullet with REQUIRED topics ONLY )
[ ] E: R6RS, Complete Scheme without Error System
[ ] E: R7RS, Complete Scheme *with*  Error System
[ ] E: Identify Philosophical Differences
    [ ] Identify Dynamic Typing, Where does the language burn time matching?
    [ ] Identify where value history can be built
[ ] T: Will SPARROW compile & run in Windows without modification?


```
## Phase 2: Learn Flow-Based Programming -- FINCH
```
[ ] Read & Take Notes on Flow Programming Sources
[ ] Collect flow programming requirments
    [ ] How is data constructed?
    [ ] How is data passed?
    [ ] How is data mutated?
    [ ] Petri Reading Program: Read selected papers in the FINCH collection while digging into available flow programming systems
    [ ] What are Node Red's capabilities?
[ ] Type System
    [ ] User-defined types? -and/or- Entity Component System (ECS) with interfaces?
        [ ] Evaluate Entity Component System ([Polymorph](https://github.com/rlipsc/polymorph))
        [ ] Alternatives to ECS?
        [ ] Alternatives to OOP?
[ ] Composite Types
    [ ] Arrays? - Access block evaluates to an index
    [ ] Hashes?
    [ ] Dynamic Arrays?
    [ ] Heterogeneous (Python) Arrays? (Unlikely for static typing focus)
```

## Phase 3: Tune the Machine -- FINCH
```
[ ] E: Loops?
[ ] E: Iterators?
[ ] Optiional Whitespace Formatting
    [ ] End statements at newline by default
    [ ] Check previous, then check new in the cases that the new has replaced it
    [ ] <function signature>:  <-- Begins a function block
[ ] Split Interpreter into Functional Areas
    [ ] Components:  Basic types that also compose AST
    [ ] Analyzer:    Run before interpretation
    [ ] Interpreter: REPL frontend, Core execution backend, invokes analyzer 
[ ] Special Blocks, _<prefix> immediately followed by block brackets
    [ ] `_math(...){...}` ________ : Math block, infix operators are allowed
    [ ] `_name(<string>){...}`: Named block, can be used to copy portions of code only
    [ ] Block Flags
        [ ] Parens are optional for special blocks, if they are missing then assume no args or options
        [ ] `_<bloc>(-v){...}`: Verbatim, do not wrap in its own context!, instead add to existing context
[ ] Pharo (SmallTalk) Mooc (Listen only)
    [ ] Evaluate save states
[ ] Q: What are the advantages of a SmallTalk save state?  When have you ever wished for this? 
    * A: I use saved vars in Jupyter all the time
```

## Phase 4: Learn Parallel Programming -- FINCH
```
[ ] Concurrency Model & Scheduling
    [ ] Dlang Threads   in Linux and Windows
        [ ] Q: Managed by the interpreter?
    [ ] Dlang Processes in Linux and Windows
        [ ] Q: Managed by the BIRDBOX?
    [ ] READ: "Actors" by Gul Agha
[ ] E: Petri Representation? 
    [ ] Value Dependency/History Graph
[ ] Types of Sync
    [ ] Hard Sync: Must wait for an updated value
    [ ] Soft Sync: Retrieves the current value as soon as the mutex allows, regardless of whether or not it is ``fresh''
[ ] Publisher/Subscriber
    [ ] Push Model w Callbacks
    [ ] Pull Model
[ ] Tick/Lockstep
[ ] How much of ROS do I need?
[ ] Interpreter Program
    [ ] Inter-process connections
[ ] Parallel Test 1
    [ ] Choose Task
    [ ] Write and Test Task
        [ ] Single process
        [ ] Multi-Process
    [ ] Run Comparison Test
    [ ] Document Results
    [ ] Petri Rep for Parallel Test 1 ?
```

## Phase 5: Basic Cognitive Architecture -- CORVID
1. AI-focused dialect of FINCH
* AI
    - Plug-and-Play learning appliances (Brain Book)
        - Reason over domain structures in order to re-use capabilities or suggest the structure of new capabilities
    - DL auto-tuning of interpreter parameters
* Resource Links
    - [Flow-Based Programming For Machine Learning](https://assets.researchsquare.com/files/rs-707294/v1_covered.pdf), 2021 Mahapatra

## Phase 6: Heavy Simulation -- MAGPIE
1. A simulation-focused dialect of CORVID
* Mathematics / Statistics
    - Stochastic Programming
    - Linear Algebra
    - Deep Learning
    - Geometric Algebra

## Phase N: Far Future -- ???

# Resources
???

# (Possible) Names
* Language: [F]low [I]nterpreter for [N]ovice [C]omputer language [H]acking
    - Birds are cool
* Base Node: [B]ase [I]nterpreter, [R]EPL, and [D]ispatcher for [B]asic [O]peration e[X]change, Per-host coordination & control
* Sub  Node: [B]asic [I]nstruction and [R]esource, and [D]ispatcher, Implements FINCH per job

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
* Hardware
    - Home/Life automation
    - Real-time control
    - Hardware Emulation
        - CHIP8, Fantasy CPU
        - Zilog 80A, RadioShack TRS-80 / Sinclair ZX Spectrum
        - Ricoh 5A22, SNES CPU
        - Super FX Chip, SNES Cartridge Coprocessor
           - SNES 3D Art
        - The ability to use instantiate Finch nodes while using the message passing bus as a data bus, thus acheiving full system emulation
        - The ability to compile the graph of chip emulators down into a full system emulator using the normal compression and compilation pipeline
            - Emulate SNES from a ROM!


