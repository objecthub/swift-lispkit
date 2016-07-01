# Todo

Last update: 2016-07-01


## Compiler

- [X] Optimization/checkpointing framework
- [X] Use values instead of variables for all immutable bindings (do this by compiling
      forms with bindings twice, if the first run shows that a binding is mutable)
- [ ] Optimize letrec; only use variables if bindings are mutable or if there is a forward
      reference.
- [X] Support (define ...) in control flow special forms
- [X] Refactor creation of compiler; checkpointer needs to be always passed explicitly
- [X] Support compiled system functions


## Runtime

- [ ] Go through procedures that call back into the VM to check that all expressions are
      tracked by the managed object pool
- [ ] Redesign error data model
- [ ] Evaluate what it takes to support `call/cc`
- [ ] Profile virtual machine
- [X] Implement new primitive procedure type that returns closures for execution instead
      of results

## Library

- [X] Adopt text + binary ports from R7RS
- [X] Adopt bytevectors from R7RS
- [X] Implement bytevector library
- [ ] Implement library functions supporting mutable strings
- [X] Implement delay-force; provide full support for R7RS delayed evaluation primitives

## Tests

- [X] Build regression testing framework
- [X] Add simple regression tests 
- [ ] Add unit tests


## Documentation

- [ ] Document architecture of framework
- [ ] Document components of virtual machine
- [X] Document bytecode instructions
- [ ] Document supported Scheme procedures and special forms
