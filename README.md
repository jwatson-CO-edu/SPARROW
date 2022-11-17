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

## Phase 1: Learn Computer Language Design via Scheme -- SPARROW
```
[Y] Translate first "ab_" evaluator to Dlang - 2022-09-07: COMPLETE
    [Y] All Functions Implemented - 2022-09-07: Nim had pointer references, so some implementation needed changing
    [Y] All tests pass - 2022-09-07: No Nim tests had been written for variable binding, so these functions were not implemented
    [Y] Q: How big is each `Atom`? - 2022-09-07: 64 bytes for all kinds because strings are pointers
[Y] Translate second "ac_" evaluator to Dlang - 2022-09-08: COMPLETE
    [Y] All Functions Implemented - 2022-09-08: COMPLETE
    [Y] All tests pass - 2022-09-08: Environment bound and unbound variables implemented, are ref counts needed?

[>] Translate "The Little Javascripter" by Douglas Crockford
    https://www.crockford.com/little.html (See `JS` folder for reference implementation)
    [Y] T: Test each component as it is developed - 2022-11-16, AWESOME
    [Y] Change name from FINCH, make public, and reorganize around SPARROW only. - 2022-11-16
    [Y] Clean source - 2022-11-16, READY FOR REPL
    [Y] Implement REPL - 2022-11-17, `readln` returns a string, including the newline
    [Y] Clean source of {tests, debug prints} - 2022-11-17, Point interested parties to commit with tests if asked
    [Y] T: Ask Dlang forum for feedback - 2022-11-17
        * Variant/Union: https://forum.dlang.org/thread/icpmctbrsscuimkxtuby@forum.dlang.org
        * Code Critique: https://forum.dlang.org/thread/uhziclioiviwzztojofy@forum.dlang.org
    [>] Apply Dlang forum changes
    [N] T: Ask StackOverflow for feedback - 2022-11-17, Not for projects of this size
    [N] Apply StackOverflow changes - 2022-11-17, Not for projects of this size
    
[ ] Parse "Easy S-Expressions" instead: 2022-04-19, Ignore whitespace except to separate symbols
    [ ] Implicit open paren, `;` is close paren
    [ ] <funcName> <arg1> ... <argN>;
        [ ] E: What is the most ergonimic way to handle nested expressions?
    [ ] T: Parse and run a plaintext file
        [ ] E: File extension: .BRD, .SPRW, .FNC,
    [ ] Program Blocks: 2022-11-16, Last line is the meaning of the block
        [ ] Curly Braces {}
        [ ] Pythonic / Implicit
        [ ] New block == new context
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
[ ] Small Optimizations
    [ ] Iterate LISP lists instead of converting to dyn arrays
    [ ] Unify and/or streamline `ExprInContext` usage
[ ] E: Evaluate "Practical Common Lisp" (PCL) for useful features and structures (If yes, then expand this bullet with REQUIRED topics ONLY )
[ ] E: R6RS, Complete Scheme without Error System
[ ] E: R7RS, Complete Scheme *with*  Error System
[ ] E: Identify Philosophical Differences
    [ ] Identify Dynamic Typing, Where does the language burn time matching?
    [ ] Identify where value history can be built
[ ] T: Will SPARROW compile & run in Windows without modification?
[ ] Allow SPARROW to either run a file or run a REPL, depending on how it is called
    [ ] Execute file: `sparrow <FILENAME>`
    [ ] Run REPL: `sparrow`
[ ] E: Single-threaded Efficiency and Readability
    [ ] T: Ask Dlang forum for feedback
    [ ] T: Ask StackOverflow for feedback
[ ] E: Read the next phase's goals.  
    [ ] E: Do they make match your purpose in creating FINCH?
    [ ] E: Do they make sense to you?
    [ ] E: What is FINCH's value to you?
```

## Phase 2: A different language ...

# Resources
* https://mitpress.mit.edu/9780262560993/the-little-schemer/
* https://www.crockford.com/little.html
