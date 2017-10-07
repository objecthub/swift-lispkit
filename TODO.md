# Todo

Last update: 2017-09-03


## Compiler

- [X] Optimization/checkpointing framework
- [X] Use values instead of variables for all immutable bindings (do this by compiling
      forms with bindings twice, if the first run shows that a binding is mutable)
- [ ] Optimize letrec; only use variables if bindings are mutable or if there is a forward
      reference.
- [X] Support (define ...) in control flow special forms
- [X] Refactor creation of compiler; checkpointer needs to be always passed explicitly
- [X] Support compiled system functions
- [ ] Support tail patterns in `syntax-rules`
- [ ] Within library definitions, support: `include`, `include-ci`,
      `include-library-declarations`, `cond-expand`
- [X] Support `define-values`
- [ ] Support `cond-expand`
- [ ] Support `include-ci`
- [ ] Support features mechanism
- [ ] Fix bug preventing to nest definitions three or more times

## Runtime

- [ ] Go through procedures that call back into the VM to check that all expressions are
      tracked by the managed object pool
- [ ] Redesign error data model
- [ ] Profile virtual machine
- [X] Implement new primitive procedure type that returns closures for execution instead
      of results
- [X] Evaluate what it takes to support `call/cc`
- [X] Implement full support for `call/cc`
- [X] Implement full support for `dynamic-wind`
- [X] Register parameter hash map (incl. mutable parts of the data structure, if needed)
      such that it's managed by the managed object pool


## Library

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
- [ ] Add unit tests


## Documentation

- [ ] Document architecture of framework
- [ ] Document components of virtual machine
- [X] Document bytecode instructions
- [ ] Document supported Scheme procedures and special forms
