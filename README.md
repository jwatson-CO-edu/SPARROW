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
    [ ] How to define nested expressions? Always blocks? `{}`
    [ ] Allow infix math? Special math block instead?
[ ] Experiment with memory models
    [ ] Read: How does Nim allocate memory?
    [ ] Q: Can a block of "null pointer" memory be allocated?
    [ ] Q: Can a large array of variant `Atoms` be allocated? What is the per-unit size in memory?
    [ ] T: Which is faster; (Pre-allocated block -vs- Dynamic vars); Create 1000 vars and assign randomly for 10k steps
[ ] Evaluate "The Seasoned Schemer" for useful features and structures (If yes, then expand this bullet with REQUIRED topics ONLY )
[ ] Evaluate "Practical Common Lisp" (PCL) for useful features and structures (If yes, then expand this bullet with REQUIRED topics ONLY )

### Phase 2, Features ###
[ ] Collect flow programming requirments
[ ] Q: What are ORC and ARC?
[ ] Q: Does Nim target LLVM yet? Is there another intermediate representation instead?
[ ] Arrays?
[ ] Hashes?
[ ] User-defined types? -and/or- Entity Component System (ECS) with interfaces?
[ ] Concurrency Model & Scheduling
    [ ] Petri Representation?
[ ] Q: How does the Nim type system work?
```

## (Possible) Names
* Language: [F]lexible [I]nterpreted [N]ode [C]omputing [H]elper
    - Birds are cool
* Execution Model: [B]asic [O]peration e[X]change for [F]lows [A]agents and [B]ehaviors
    - Possible extension to behavior trees
    - An execution model based on Petri Nets that seeks to support commonsense solutions to Job Shop Scheduling problems as they relate to computing
* Interpreter: BOX? HOUSE? (Either a riff on BOXFAB or birds)

# Resources

## Links

## Future Applications
* [Flow-Based Programming For Machine Learning](https://assets.researchsquare.com/files/rs-707294/v1_covered.pdf), 2021 Mahapatra
