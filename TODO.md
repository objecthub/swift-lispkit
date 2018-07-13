# Todo

Last update: 2018-07-13


## Compiler

- [ ] Manage source locations in compiler and generated code and include source locations in error messages
- [ ] Optimize letrec; only use variables if bindings are mutable or if there is a forward reference.
- [ ] Figure out that references to `define`, `define-values`, `unquote`, etc. are referring to the right
      syntax/definitions; right now, there are checks only for the definition to happen at the top-level in an
      immutable fashion
- [ ] Treat define-values like define in compiler (wrt. internal definitions)
- [ ] `#;` comments (datum comments) not correctly working
- [X] Optimization/checkpointing framework
- [X] Use values instead of variables for all immutable bindings (do this by compiling
      forms with bindings twice, if the first run shows that a binding is mutable)
- [X] Support (define ...) in control flow special forms
- [X] Refactor creation of compiler; checkpointer needs to be always passed explicitly
- [X] Support compiled system functions
- [X] Support tail patterns in `syntax-rules`
- [X] Within library definitions, support: `include`, `include-ci`, `include-library-declarations`, `cond-expand`
- [X] Support `define-values`
- [X] Support `cond-expand`
- [X] Support `include-ci`
- [X] Support features mechanism
- [X] Fix bug preventing to nest definitions three or more times


## Runtime

- [ ] Go through procedures that call back into the VM to check that all expressions are
      tracked by the managed object pool
- [X] Redesign error data model
- [ ] Profile virtual machine
- [X] Implement new primitive procedure type that returns closures for execution instead
      of results
- [X] Evaluate what it takes to support `call/cc`
- [X] Implement full support for `call/cc`
- [X] Implement full support for `dynamic-wind`
- [X] Register parameter hash map (incl. mutable parts of the data structure, if needed)
      such that it's managed by the managed object pool


## Library

- [ ] Reimplement records: to improve performance, to use make-type, to make them extensible
- [ ] Record type checks when record accessors are called
- [X] Implement text + binary ports based on the R7RS standard
- [X] Implement bytevector library based on the R7RS standard
- [X] Implement hash table library based on the R6RS standard
- [X] Implement record library based on the R7RS standard
- [X] Implement delay-force; provide full support for R7RS delayed evaluation primitives
- [X] Implement libraries based on the R7RS standard
- [X] Support mutable strings


## Tests

- [X] Build regression testing framework
- [X] Add simple regression tests 
- [X] Add unit tests


## Documentation

- [ ] Document architecture of framework
- [ ] Document components of virtual machine
- [X] Document bytecode instructions
- [ ] Document supported Scheme procedures and special forms
