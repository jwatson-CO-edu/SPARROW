# **S**cheme **P**rogram **A**llowing **R**easonable **R**eckoning **O**f **W**ork [SPARROW]
An implementation of "[Little Scheme](https://mitpress.mit.edu/9780262560993/the-little-schemer/)" written in [D](https://dlang.org/), with the goal of learning language design.  That's it.  
The concept is to slowly morph it into my own language. 

#### Notes: 
* SPARROW's ability to solve problems for others is an unintentional side-effect.  
* Spelled "SPARROW" or "Sparrow".

# Design Goals
1. This is for my own entertainment and edification.
1. Ergonimic & Fun to use
1. Begin simply, and add justifiable complexity with caution!

# Purpose & Roadmap
## Phase 1: Learn Computer Language Design via Scheme -- SPARROW
1. Parsing & Syntax
1. Program Blocks
1. Memory Management & Garbage Collection


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

# Phase 1: Learn Computer Language Design via Scheme -- SPARROW
### Basic Implementation
```
[Y] Translate first "ab_" evaluator to Dlang - 2022-09-07: COMPLETE
    [Y] All Functions Implemented - 2022-09-07: Nim had pointer references, so some implementation needed changing
    [Y] All tests pass - 2022-09-07: No Nim tests had been written for variable binding, so these functions were not implemented
    [Y] Q: How big is each `Atom`? - 2022-09-07: 64 bytes for all kinds because strings are pointers
[Y] Translate second "ac_" evaluator to Dlang - 2022-09-08: COMPLETE
    [Y] All Functions Implemented - 2022-09-08: COMPLETE
    [Y] All tests pass - 2022-09-08: Environment bound and unbound variables implemented, are ref counts needed?
[Y] Translate "The Little Javascripter" by Douglas Crockford - 2022-11-16, AWESOME
    https://www.crockford.com/little.html (See `JS` folder for reference implementation)
    [Y] T: Test each component as it is developed - 2022-11-16, AWESOME
    [Y] Change name from FINCH, make public, and reorganize around SPARROW only. - 2022-11-16
    [Y] Clean source - 2022-11-16, READY FOR REPL
    [Y] Implement REPL - 2022-11-17, `readln` returns a string, including the newline
    [Y] Clean source of {tests, debug prints} - 2022-11-17, Point interested parties to commit with tests if asked
    [Y] T: Ask Dlang forum for feedback - 2022-11-17
        * Variant/Union: https://forum.dlang.org/thread/icpmctbrsscuimkxtuby@forum.dlang.org
        * Code Critique: https://forum.dlang.org/thread/uhziclioiviwzztojofy@forum.dlang.org
    [N] Apply Dlang forum changes, 2022-11-19: Ring buffer isn't so interesting since atoms won't die in order 
    [N] T: Ask StackOverflow for feedback - 2022-11-17, Not for projects of this size
    [N] Apply StackOverflow changes - 2022-11-17, Not for projects of this size
[Y] Atom Size Reduction - 2022-11-20, Several lines shorted
    [Y] Try unions: 1{car, str, num} + 2{cdr, err} - 2022-11-19, 72 --to-> 32 bytes!
    [Y] Integrate change into Little Schemer ---> Sparrow - 2022-11-20, Several lines shorted
[Y] Parse "Easy S-Expressions" instead, 2022-11-23: ONLY at the top level!
    [N] Lexer state machine, 2022-11-23: NOT needed at this time
    [Y] Implicit open paren, `;` is close paren, 2022-11-23: ONLY at the top level!
    [Y] <funcName> <arg1> ... <argN>;, 2022-11-23: ONLY at the top level!
        [Y] E: What is the most ergonimic way to handle nested expressions?
         *  A, 2022-11-23: At this time, semicolon endings are only allowed at the root level of each string expression
[Y] Allow SPARROW to either run a file or run a REPL, depending on how it is called, 2022-11-30: Forgot to commit work from home, but remembered all my hard-won gains
    2022-11-23: Program blocks are easiest from a file instead of the terminal
    [Y] Compile to named executable, 2022-11-24: dmd sparrow.d -of=sparrow.app // Named outfile and suppressed obj file
    [Y] Implement `print` special form to test serial statements
        [Y] End print statements with newline, 2022-11-30
        [Y] Test multiline, multi-print, 2022-11-30
    [Y] Execute file: `sparrow <FILENAME>`, 2022-11-30
        [Y] Handle multiple statements per line, 2022-11-30
        [Y] E: File extension: .BRD, .SPRW, .FNC, 2022-11-27: .SPRW chosen
    [Y] Run REPL: `sparrow`, 2022-11-27: May exec file or use REPL      
[Y] Program Blocks, 2022-12-05: Excellent!
    [Y] Treat a block as a list of instructions, instead of a list of values, 2022-11-27: New block atom
    [Y] Curly Braces {}, 2022-11-28: Added to lexer and parser, requires testing!
        [Y] Parser change: Allow for statements within a nested block to be EZ lists, 2022-11-28: Added to lexer and parser, requires testing!
    [Y] Special `meaning` actions for encountering a block, 2022-12-05: Excellent!
        [Y] New block == new context, 2022-12-05: Excellent!
        [Y] T: Local block variable(s), 2022-12-05: Excellent!
        [Y] T: Verify that statement precedence with block brackets already exists in evaluator, If not then Implement it!, 2022-12-05: No issue!
        [Y] T: Make sure that nested statements and blocks do not cause ambiguity, 2022-12-05: No issue!
    [Y] Root of the parsed input file is a block, 2022-11-27: Executed in the `baseEnv` context
    [Y] Last line is the meaning of the block, (prog ... ) ?, 2022-11-27: Needs to be a special form as well 
    [~] Pythonic: Implicit by indentation, 2022-11-23: Not needed at this time + adds complexity
        [~] How to cleanly handle nested indented blocks?, 2022-11-23: Not needed at this time + adds complexity

[>] Implement loops
    [>] Write a new parser!
         *  A stack is required to handle arbitrarily nested blocks, and possibly other nested things!
        [>] Implement FSM operating on a stack
        [ ] T: Run all existing examples other than the loop example
        [ ] T: Inline block { <BLOCK> } <OUTSIDE>
        [ ] Tear out the old parser (See notes near to new functions)
    [>] for (<counter> <bgn> <end>) {<BLOCK>}; - Counter bounds inclusive, Note DOUBLE bounds!
        [Y] Special Form, 2022-12-06: Wrote special form, requires a new parser and also testing
        [Y] Loop iterates with <counter> <= <end>, 2022-12-06: Requires a new parser and also testing
        [Y] Loop exits    with <counter> > <end>, 2022-12-06: Requires a new parser and also testing
         *  2022-12-06: At this time not allowing the user to specify custom conditions!
        [>] T: Loop that accesses the counter var within the loop body
    [ ] Allow for all types of ergonomic loops
        [ ] for (<counter> <bgn> <end>) {
                <BLOCK>
            };
        [ ] for (<counter> <bgn> <end>) 
            {
                <BLOCK>
            };
        [ ] (for (<counter> <bgn> <end>) {
                <BLOCK>
            })
        [ ] (for (<counter> <bgn> <end>) 
            {
                <BLOCK>
            })
    [ ] for (<counter> <bgn> <incr> <end>) {<BLOCK>}; - Counter bounds inclusive with increment value
    [ ] T: Print Slash Maze
        [ ] Implement `rand`, Uniform random sampling in [0,1)
        [ ] As a function with settable rows and columns
    [ ] while (<cond>)

[ ] Streamline `ExprInContext` by reusing the input struct as it will be copied anyway
[ ] Ergonomics Testing - Multiline and Indented
    [ ] T: S-expressions
    [Y] T: EZ Lists, 2022-12-05: Excellent!
    [ ] T: Loop within a block!
    [~] Line Continuation: `\+`, 2022-11-25: Block parser possibly already covers this? Only implement `\+` if needed!
```
### Modest Extensions
```
[ ] Replace `ExprInContext` with global `Env` pointer, Assume one context per interpreter/thread
[ ] Change Closures to block implementation
{ } E: Source file > 2k lines? 
    { } Yes: Split functions into separate categories and organize as a project
    { } No:  Are the functions at least arranged in a meaningful way?
[ ] Experiment with memory models
    [ ] Research
        [ ] Read: How does Dlang allocate memory?
        [ ] Q: Can a block of "null pointer" memory be allocated?
    [ ] Test 
        [ ] SPARROW Implementation with Pre-allocated block of var memory (No GC)
            [ ] Q: Can a large array of `Atoms` be allocated from a null pointer block? 
            { } If not, then implement var memory as an array of `Atom`s
        [ ] Create 10k vars and assign randomly for 10k steps
[ ] (Simple!) Install script
[ ] Evaluate "The Seasoned Schemer" for useful features and structures (If yes, then expand this bullet with REQUIRED topics ONLY )
[ ] E: Evaluate "Practical Common Lisp" (PCL) for useful features and structures (If yes, then expand this bullet with REQUIRED topics ONLY )
[ ] E: R6RS, Complete Scheme without Error System (If yes, then expand this bullet with SELECTED topics ONLY )
[ ] E: R7RS, Complete Scheme *with*  Error System (If yes, then expand this bullet with SELECTED topics ONLY )
```
### Compatibility Testing & Structural Evaluation
```
[ ] T: Will SPARROW compile & run in Windows without modification?
    [ ] Q: Is selective compilation required and/or possible in Dlang?
[ ] E: Identify Implementation Choices
    [ ] What are the advantages    of being a LISP?
    [ ] What are the disadvantages of being a LISP?
    [ ] Identify Dynamic Typing handler code, Where does the language burn time matching?
[ ] E: Single-threaded Efficiency and Readability
    [ ] Efficiency
        [ ] Develop a benchmark use case
        [ ] Run solving algo in SPARROW
            [ ] Does D have a benchmarking facility like Python's `timeit`?
        [ ] Compare same algo to Python3.10
        [ ] Compare same algo to Python3.11
    [ ] Readability
        [ ] Is the algo easy or hard to read in SPARROW?
```
## Deferred to FINCH
* FINCH is an interpretted, flow-based language based on a graph structure instead of a stack structure
* The purpose is to support my own investigation into cognitive architecture and simulation.
* Possible transpilation to D in the far future?
* Possible bytecode interpreter in the far far future?
```
[ ] E: AST Nodes? The parser outputs AST nodes that can have multiple children, not just 2 in the case of conses
     *  At this point SPARROW moves away from being Scheme and becomes its own language!
    [ ] T: Repeat efficiency test(s)
    [ ] E: Was this easier or harder to parse?
    [ ] E: Did you notice any advantages?
{ } Garbage Collection, 2022-11-25 - If SPARROW has a pre-allocated implementation by this point, then investigate GC
```
# Phase 2: **FINCH Transition**: What are you even doing?
### Manage the FINCH vision 
* `[ ]` What is the purpose of FINCH?
    - Construct a flow-based language and the structure to support it
    - Imagine a modular language that is **not** based on *stacks*
    - Prelude to research in cognitive architecture and cognitive robotics  
* `[ ]` Gather non-parallel goals from FINCH paper notes.
* `[ ]` Read the next phase's goals.  
* `[ ]` Consolidate paper and README goals.
* `[ ]` Choose the main theme and remove conflicting goals
* `[ ]` Does your list of goals match your purpose in creating FINCH?
* `[ ]` Do they make sense to you?
* `[ ]` What is FINCH's value to you?
* `[ ]` Does FINCH interfere with your professional goals? 
    - There were a number of times I let it interfere.
    - Have I managed to keep it from becoming an obsession?

### Read & Take Notes
* `[ ]` [Minimalism in Programming Language Design](https://pointersgonewild.com/2022/05/23/minimalism-in-programming-language-design/)
* `[ ]` [Racket is an Acceptable Python](https://dustycloud.org/blog/racket-is-an-acceptable-python/)
* `[ ]` [Computer Language Design in the Real World](https://blog.sigplan.org/2022/05/19/language-design-in-the-real-world/)  

### Investigate small(ish) languages and their philosophies
* `[ ]` [Duck](http://ducklang.org/designing-a-programming-language-i)
* `[ ]` [Lil](https://beyondloom.com/decker/lil.html)
* `[ ]` [LISP](https://texdraft.github.io/lisp-compiler/internals.html)
* `[ ]` [Om](http://www.om-language.org/)  
* `[ ]` [Pony](https://www.ponylang.io/)  

### Absorb PL Advice
* `[ ]` [Programming Language Design Notes](https://cs.lmu.edu/~ray/notes/languagedesignnotes/)
* `[ ]` [Great Works in PL](https://www.jsoftware.com/papers/tot.htm)
    - `[ ]` Create reading list for the next phase  

### Investigate flow language(s) and their philosophies  
* `[ ]` Select Zotero materials
* `[ ]` Read Zotero materials

# Language 3
A programming language in which computation graphs (for differentiation and backpropagation) and tensors (to build neural models from elementary layers) are first-class structures.
* `[ ]`  Choose a name
    - MAGPIE: Multiple Agency Graph Program for Intelligent Execution
    - FINCH: 
        * Flexible Intepreter for Novel Cognition Heirarchies
        * Flow Interpreter for Novice Computer language Hacking
    - CASSOWARI: Cognitive Agent Simulation System, Objects With ANN, RL, and Ingenuity
    - CROW: Cognitive REPL Object Wrangler
    - GRACKLE: Graph Routing of Agent Cognition for Knowledge, Learning, and Execution
    - BIRD: Basic Instruction REPL Demonstrator

# Resources
## Novice Language Design
* https://mitpress.mit.edu/9780262560993/the-little-schemer/
* https://www.crockford.com/little.html
* See reading lists, above.
## Parsing
* https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm
## Optimization
* https://johnysswlab.com/vectorization-dependencies-and-outer-loop-vectorization-if-you-cant-beat-them-join-them/
## Type Checking
* https://www.haskellforall.com/2022/03/the-hard-part-of-type-checking-nix.html
## Advanced Language Design
* https://mooc.pharo.org/
* https://www.cis.upenn.edu/~bcpierce/courses/670Fall04/GreatWorksInPL.shtml
