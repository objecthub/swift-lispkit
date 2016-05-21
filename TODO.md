# Todo

Last update: 2016-05-21


## Compiler

- [X] Optimization/checkpointing framework
- [X] Use values instead of variables for all immutable bindings (do this by compiling
      forms with bindings twice, if the first run shows that a binding is mutable)
- [ ] Optimize letrec; only use variables if bindings are mutable or if there is a forward
      reference.
- [ ] Support (define ...) in control flow special forms
- [ ] Refactor creation of compiler; checkpointer needs to be always passed explicitly


## Runtime

- [ ] Go through procedures that call back into the VM to check that all expressions are tracked
      by the managed object pool
- [ ] Redesign error data model
- [ ] Evaluate what it takes to support `call/cc`
- [ ] Profile virtual machine


## Library

- [ ] Adopt text + binary ports from R7RS
- [ ] Adopt bytevectors from R7RS


## Tests

- [X] Build regression testing framework
- [X] Add simple regression tests 
- [ ] Add unit tests


## Documentation

- [ ] Document architecture of framework
- [ ] Document components of virtual machine
- [ ] Document bytecode instructions
- [ ] Document supported Scheme procedures and special forms
