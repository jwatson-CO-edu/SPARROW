# Petri Language  
An interpreted language written in [Nim](https://nim-lang.org/), with the goals of learning language design and introducing straightforward, automated parallelism to the computing world.

## `DEV PLAN`
```
### Phase 0, Nim Language: COMPLETE! ###
[Y] Finish Nim tuts, 2022-03-24
    [Y] Part 1: Basics, 2022-03-23
    [Y] Part 2: Classes and Exceptions, 2022-03-24
    [Y] Part 3: Metaprogramming and Macros, 2022-03-24

### Phase 1, Basic Function ###
[ ] Translate first evaluator to Nim
    [ ] All Functions && All Tests
    [ ] Q: How big is each `Atom`?
[ ] Translate "The Little Javascripter" by Douglas Crockford
    https://www.crockford.com/little.html (See `JS` folder)
[ ] Parse "Easy S-Expressions" instead: 
    Implicit open paren, `;` is close paren
    <funcName> <arg1> ... <argN>;
    [ ] How to define nested expressions? Always blocks? `{}`? `()`?
        [ ] Implement statement precedence with `()` and determine its relationship with nested expressions. (Identical to nested?)
        [ ] Eval whether to allow "Classic S-Expressions": (<funcName> <arg1> ... <argN>)
    [ ] Allow infix math? Special math block instead?
[ ] Line Continuation: `\+`    
[ ] Evaluate "The Seasoned Schemer" for useful features and structures (If yes, then expand this bullet with REQUIRED topics ONLY )
[ ] Experiment with memory models
    [ ] Read: How does Nim allocate memory?
    [ ] Q: Can a block of "null pointer" memory be allocated?
    [ ] Q: Can a large array of variant `Atoms` be allocated? What is the per-unit size in memory?
    [ ] T: Which is faster; (Pre-allocated block -vs- Dynamic vars); Create 1000 vars and assign randomly for 10k steps
[ ] Evaluate "Practical Common Lisp" (PCL) for useful features and structures (If yes, then expand this bullet with REQUIRED topics ONLY )

### Phase 2, Feature Research ###
[ ] Collect flow programming requirments
[ ] Q: What are ORC and ARC?
[ ] Q: Does Nim target LLVM yet? Is there another intermediate representation instead?
[ ] Type System
    [ ] Q: How does the Nim type system work?
    [ ] User-defined types? -and/or- Entity Component System (ECS) with interfaces?
        [ ] Evaluate Entity Component System ([Polymorph](https://github.com/rlipsc/polymorph))
        [ ] Alternatives to ECS?
        [ ] Alternatives to OOP?
[ ] Composite Types
    [ ] Arrays?
    [ ] Hashes?
    [ ] Dynamic Arrays?
    [ ] Heterogeneous (Python) Arrays? (Unlikely for static typing focus)
[ ] Concurrency Model & Scheduling
    [ ] Nim Threads   in Linux and Windoes
    [ ] Nim Processes in Linux and Windoes
    [ ] READ: "Actors" by Gul Agha
    [ ] Petri Representation?
    
### Phase 3, Expansion ###
[ ] Abstract Source Graph Nodes
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
    
### Phase 4, Optimization ###
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
    - How to retain reactivity in a Graph?
    - Does  the introduction of cycles and loops create significant problems.
* Simulations
    - "Tick" regulation across nodes
    - Constructivist simulation: Program flow follows fluid flow
    - Bond Graphs and Circuit Sim
    - Can the mechanisms that regulate execution be used to regulate simulation
* Execution System
    - Automatic network compute discovery 
    - Automatic load balancing
    - FINCH-OS, an operating system with advanced task scheduling
* Mathematics / Statistics
    - Stochastic Programming
    - Deep Learning
    - Geometric Algebra
* AI
    - Plug-and-Play learning appliances (Brain Book)
    - DL auto-tuning of interpreter parameters

# Resources

## Links

## Future Applications
* [Flow-Based Programming For Machine Learning](https://assets.researchsquare.com/files/rs-707294/v1_covered.pdf), 2021 Mahapatra
