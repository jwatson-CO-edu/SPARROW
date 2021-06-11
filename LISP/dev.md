# Implementation
## Decisions
* `Atom` struct contains members to support *all* primitive types, `(-)` Wastes space, `(+)` Makes prototyping easier

## Ideas
* Array type
* Dynamic Array Type
    - Each section contains a C array of length X
    - Cons another section onto list as needed `(+)`
    - `(-)` Lookup not constant, but `(+)` n/X lookup time

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
