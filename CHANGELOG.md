# Changelog

## 1.3 (2017-12-03)
- Support simple HTTP API
- Support compression for bytevectors
- Implement call tracing
- Fixed bug preventing some internal definitions to not work
- Support all standard R7RS small Scheme libraries
- Support for: `(srfi 158)`, `(lispkit wt-tree)`, `(lispkit object)`

## 1.2 (2017-10-22)
- Support for tail patterns in `syntax-rules`
- Support for `features` and `cond-expand`
- Support for `include` and `include-library-declarations`
- Support for `syntax-error`
- Support for `define-values`
- Support a new lightweight custom type declaration mechanism via `make-type`
- Added SRFI 112-style support for human-readable information about the hardware
   and software configuration on which LispKit is being executed
- Added support for the following libraries: `(srfi 63)`, `(srfi 64)`, `(srfi 128)`,
   `(lispkit iteration)`, `(lispkit json)`

## 1.1 (2017-09-25)
- Migrated project to Xcode 9 and Swift 4
- Adopted Swift 4-version of NumberKit

## 1.0 (2017-08-06)
- Support for custom ellipsis in `syntax-rules`
- Fixed serious scoping issues in `syntax-rules`
- Fixed hash functions to prevent overflows
- Support for R6RS enumeration operations

## 0.7 (2017-04-30)
- Implemented native support for fixnum (fx*) and flonum (fl*) operations
- Completed support for all R7RS string operations
- Completed support for all R7RS character operations
- Added common list operations (sort, filter, partition)
- Completed support for all R7RS vector operations

## 0.6 (2017-02-12)
- Automatically load libraries
- Made stack grow automatically
- Support externally triggered termination of evaluation
– Implemented new system library; added new file functions
- Implemented support for [R7RS](http://www.r7rs.org)-compliant exceptions
- Implemented support for multiple return values
– Ported various SRFIs and included them in the LispKit package

## 0.5 (2016-11-13)
- Implemented environments as first-class values
- Introduced a new R7RS-compatible library abstraction
- Reimplemented all functions using libraries

## 0.4 (2016-09-04)
- Migration from Swift 2.2 to Swift 3.0
- Implemented [R6RS](http://www.r6rs.org)-compliant hash tables
- Implemented [R7RS](http://www.r7rs.org)-compliant parameters (supporting dynamic scoping)
- Implemented [R7RS](http://www.r7rs.org)-compliant record types

## 0.3 (2016-07-05)
- Implemented [R7RS](http://www.r7rs.org)-compliant promises
- Implemented [R7RS](http://www.r7rs.org)-compliant port library, supporting both textual
  ports and binary ports; built on top of Foundation API (not using low-level C port
  abstractions) 
- Fixed a few cases where the runtime didn't do proper tail calls
- Implemented full support for `call/cc`

## 0.2 (2016-05-16)
- Revised bytecode instruction set. Added documentation to the
  [LispKit Wiki](https://github.com/objecthub/swift-lispkit/wiki).
- Implemented compiler optimization framework. Turned compiler into a two-phase compiler.
- Optimized usage of variables for function arguments.

## 0.1 (2016-05-02)
- Initial version consisting of the framework and a very simple read-eval-print loop
- The LispKit framework implements a subset of the
  [R5RS Scheme standard](http://www.schemers.org/Documents/Standards/R5RS/HTML/); the
  biggest feature missing is support for `call/cc`
- LispKit coonsists of a compiler generating bytecode and a virtual machine which
  interprets the bytecode
- The framework is incomplete and work in progress
