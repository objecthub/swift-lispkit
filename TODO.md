# Todo


## Compiler

- [ ] Use values instead of variables for all immutable bindings (do this by compiling
      forms with bindings twice, if the first run shows that a binding is mutable)


## Runtime

- [ ] Go through procedures that call back into the VM to check that all expressions are tracked
      by the managed object pool
- [ ] Redesign error data model
- [ ] Evaluate what it takes to support `call/cc`
- [ ] Profile virtual machine


## Library

- [ ] Adopt text + binary ports from R7RS
- [ ] Adopt bytevectors from R7RS


## Testing

- [ ] Build regression testing framework
- [ ] Add unit tests


## Documentation

- [ ] Document architecture of framework
- [ ] Document components of virtual machine and bytecode instructions
- [ ] Document supported Scheme procedures and special forms
