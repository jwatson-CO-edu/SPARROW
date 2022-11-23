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
        
[ ] Allow SPARROW to either run a file or run a REPL, depending on how it is called
    2022-11-23: Program blocks are easiest from a file instead of the terminal
    [ ] Compile to named executable
    [ ] Execute file: `sparrow <FILENAME>`
    [ ] Run REPL: `sparrow`
    [ ] E: File extension: .BRD, .SPRW, .FNC,

[ ] Program Blocks
    [ ] Curly Braces {}
    [ ] New block == new context
    [ ] Treat a block as a list of instructions, instead of a list of values
    [ ] T: Verify that statement precedence with block brackets already exists in evaluator, If not then Implement it! 
    [ ] T: Make sure that nested statements and blocks do not cause ambiguity
    [ ] Root of the parsed input file is a block
    [ ] Last line is the meaning of the block, (prog ... ) ?
    [~] Pythonic: Implicit by indentation, 2022-11-23: Not needed at this time
        [~] How to cleanly handle nested indented blocks?, 2022-11-23: Not needed at this time
[ ] Implement loops
    [ ] for (<counter> <bgn> <end>) {<BLOCK>}; - Counter bounds inclusive, Note DOUBLE bounds!
        [ ] Loop iterates with <counter> == <end>
        [ ] Loop exits    with <counter> > <end>
         *  At this time not allowing the user to specify custom conditions!
    [ ] for (<counter> <bgn> <incr> <end>) {<BLOCK>}; - Counter bounds inclusive with increment value
    [ ] while (<cond>)
[ ] Line Continuation: `\+`    
[ ] Unify and/or streamline `ExprInContext` usage

[ ] (Simple!) Install script
```
### Modest Extensions
```
[ ] Experiment with memory models
    [ ] Read: How does Dlang allocate memory?
    [ ] Q: Can a block of "null pointer" memory be allocated?
    [ ] Q: Can a large array of `Atoms` be allocated? 
    [ ] T: Which is faster; (Pre-allocated block -vs- Dynamic vars); 
        [ ] Create 10k vars and assign randomly for 10k steps
[ ] Evaluate "The Seasoned Schemer" for useful features and structures (If yes, then expand this bullet with REQUIRED topics ONLY )
[ ] E: Evaluate "Practical Common Lisp" (PCL) for useful features and structures (If yes, then expand this bullet with REQUIRED topics ONLY )
[ ] E: R6RS, Complete Scheme without Error System (If yes, then expand this bullet with SELECTED topics ONLY )
[ ] E: R7RS, Complete Scheme *with*  Error System (If yes, then expand this bullet with SELECTED topics ONLY )
```
### Structural Changes
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
        [ ] Compare same algo to Python3.10
        [ ] Compare same algo to Python3.11
    [ ] Readability
        [ ] Is the algo easy or hard to read in SPARROW?
[ ] E: AST Nodes? The parser outputs AST nodes that can have multiple children, not just 2 in the case of conses
     *  At this point SPARROW moves away from being Scheme and becomes its own language!
    [ ] T: Repeat efficiency test(s)
    [ ] E: Was this easier or harder to parse?
    [ ] E: Did you notice any advantages?
```
### **FINCH Transition**: What are you even doing?
`[ ]` Manage the FINCH vision 
* `[ ]` What is the purpose of FINCH?
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

`[ ]` Read & Take Notes
* `[ ]` [Minimalism in Programming Language Design](https://pointersgonewild.com/2022/05/23/minimalism-in-programming-language-design/)
* `[ ]` [Racket is an Acceptable Python](https://dustycloud.org/blog/racket-is-an-acceptable-python/)
* `[ ]` [Computer Language Design in the Real World](https://blog.sigplan.org/2022/05/19/language-design-in-the-real-world/)  

`[ ]` Investigate small(ish) languages and their philosophies
* `[ ]` [Duck](http://ducklang.org/designing-a-programming-language-i)
* `[ ]` [Lil](https://beyondloom.com/decker/lil.html)
* `[ ]` [LISP](https://texdraft.github.io/lisp-compiler/internals.html)
* `[ ]` [Om](http://www.om-language.org/)  

`[ ]` Absorb PL Advice
* `[ ]` [Programming Language Design Notes](https://cs.lmu.edu/~ray/notes/languagedesignnotes/)
* `[ ]` [Great Works in PL](https://www.jsoftware.com/papers/tot.htm)
    - `[ ]` Create reading list for the next phase  

`[ ]` Investigate floaw language(s) and their philosophies  
* `[ ]` Select Zotero materials
* `[ ]` Read Zotero materials
## Phase 2: A different language ...

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
