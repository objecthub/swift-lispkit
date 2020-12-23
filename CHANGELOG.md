# Changelog

## 1.9.2 (2020-12-23)
- Included new libraries: `(srfi 9)`, `(srfi 180)`, `(srfi 209)`, `(srfi 210)`
- Extended library `(lispkit log)` with syntax `log-time`
- Extended library `(lispkit debug)` with syntax `time-values`
- Extended library `(lispkit math)` with procedures `fxodd?`, `fxeven?`, `fx-width`, `fx-greatest`, and `fx-least` in library `(lispkit math)`.; generalized procedure `number->string`
- Fixed procedure `environment-bindings`, renamed `date-time-has-dst`
- Fixed crash when execution gets aborted while loading and executing a file
- Included new sample code: `DrawTrees.scm`, `EditDistance.scm`
- Minor tweaks to the REPL

## 1.9.1 (2020-10-04)
- Revision of library `(lispkit test)` involving procedures `test-group-failed-tests`, `test-group-passed-tests`, `failed-tests`, `passed-tests`, `current-test-epsilon`
- Fixed comparison of complex numbers and numbers involving NaN in library `(lispkit test)`
- Fixed small bugs in `(lispkit match)` and included support for `=..`, `*..`, and `**1`
- Refactored debugging functionality into library `(lispkit debug)`
- New libraries: `(srfi 194)`, `(srfi 204)`, `(lispkit sxml)`, `(lispkit sxml html)`, and `(lispkit sxml xml)`

## 1.9.0 (2020-06-20)
- New libraries: `(lispkit sqlite)`, `(lispkit combinator)`, `(lispkit system os)`, `(srfi 195)` and `(srfi 196)`
- Renamed library `(lispkit iteration)` into `(lispkit iterate)`
- Extended library `(lispkit date-time)` with procedures `date-time-add`, `date-time-add-seconds`, `date-time-diff-seconds`, `date-time-in-timezone`, `date-time-same?`, `date-time=?`, `date-time<?`, `date-time>?`, `date-time<=?`, `date-time>=?`, and `date-time-hash`.
- Extended library `(lispkit core)` with procedures `thunk`, `thunk*`, `define-values`, and `apply-with-values`
- Extended library `(lispkit string)` with procedures `string-normalize-diacritics` and `string-normalize-separators`
- Extended library `(lispkit dynamic)` with procedures `unwind-protect` and `try`
- Extended library `(lispkit control)` with procedure `letrec-values`; `define-values` can now be used wherever `define` can be used.
- Extended library `(lispkit system)` with procedures `home-directory`, `system-directory`, `path-extension`, `append-path-extension`, `remove-path-extension`, `file-readable?`, `file-writeable?`, `file-deletable?`, `directory-readable?`, `directory-writeable?`, and `directory-deletable?`.
- Extended library `(lispkit draw)` with procedures `set-image-size!`, `bitmap-size`, `bitmap-pixels`, `bitmap-exif-data`, and `set-bitmap-exif-data!`
- Support for timeouts in procedures handling HTTP ports in library `(lispkit port)`
- Small tweaks to library `(lispkit csv)`
- Library `(lispkit box)` now supports multi-value boxes
- New sample code `Covid.scm`, including data up until June 19, 2020

## 1.8.4 (2020-03-30)
- Several garbage collector enhancements
- Fixed serious memory leak in the compiler
- Fixed read command logic in the LispKit REPL
- REPL now uses default colors and thus also works with dark mode.
- Made field access of records type safe.
- New libraries: `(srfi 6)`, `(srfi 54)`, `(srfi 162)`
- Extended library `(lispkit dynamic)` with assertion support: `make-assertion-error`, `assertion`, `assert`
- New sample code `Polynomials.scm`

## 1.8.3 (2020-01-19)
- New libraries: `(lispkit markdown)`, `(lispkit disjoint-set)`, `(srfi 167)`, `(srfi 98)`, `(srfi 87)`
- Extended library `(lispkit system)` with procedures `asset-file-path`, `path-components`, `parent-path`, `path`, and `source-directory`
- Extended library `(lispkit bytevector)` with procedures `read-binary-file` and `write-binary-file`
- Extended library `(lispkit string)` with procedures `write-file`, `read-file`, `string-pad-center`, `string-empty?`; `string-concatenate` now supports an optional separator character
- Extended library `(lispkit port)` with procedures `open-input-asset`, `open-binary-input-asset`, `call-with-output-bytevector`, and `call-with-output-string`
- Extended library `(lispkit hashtable)` with procedure `hashtable-empty-copy`; `hashtable-hash-function` can now return a hash function for all hashtables
- Extended library `(lispkit math)` with procedure `fxsqrt`
- Extended library `(lispkit core)` with procedure `opt`
- Extended library `(lispkit draw)` to support assets, color lists and drawing of HTML
- Added character set `char-set:newlines` to `(lispkit char-set)`
- Extended sample code `Math.scm`
- New sample code `SpellNumbers.scm`
- Introduced asset concept, enabling libraries to depend on data files in an extensible fashion

## 1.8.2 (2019-11-21)
- Extend pattern language supported by library `(lispkit datatype)`
- Support `append-map` and `filter-map` in `(lispkit list)`; support `fxsqrt` in `(lispkit math)`; new functions in `(lispkit string)`: `string-empty?`, `string-pad-center`; `string-concatenate` now supports an optional separator character
- Fix bug in `(lispkit set)` which was leading to multi-set behavior
- New example code for solving Sudoku puzzles
- Garbage collector optimizations
- Refactored object types to make them more extensible
- A new REPL framework is available via a new framework `LispKitTools`
- New library: `(srfi 175)`

## 1.8.1 (2019-10-20)
- Update dependency on NumberKit 2.3.2
- New libraries: `(srfi 174)`, `(srfi 177)`

## 1.8.0 (2019-10-17)
- Migrated project to Xcode 11.1
- Ported code to Swift 5.1
- Simplify printed representation of procedures
- Rewrite of garbage collector: replaced recursive garbage collector with iterative version
- Completed hash functions of library `(lispkit hashtable)`
- New library: `(lispkit comparator)`
- Several fixes in `(lispkit math)`: `integer->fx` renamed to `integer->fixnum`, introduced `fxlogical-shift-right` and `real->flonum`, fixed `bit-count`, fixed `fxmodulo` to work with negative numbers
- Statically link libraries. Removed Carthage support

## 1.7.2 (2019-09-08)
- New libraries: `(lispkit stream)`, `(scheme mapping)`, `(srfi 146)`, `(srfi 165)`, `(srfi 173)`
- Fixed memory leak involving recursive local functions
- Support uninterned symbols
- Implement algebraic datatypes in terms of more efficient internal functions
- Extension and re-implementation of library `(lispkit type)`
- Include `miniAdapton` in the new third-party directory
- Support custom keywords in `let-keywords`
- New example code for generating mazes

## 1.7.1 (2019-03-31)
- Migrated project to Xcode 10.2
- Ported code to Swift 5

## 1.7.0 (2019-02-24)
- New libraries: `(lispkit csv)`, `(lispkit match)`, `(lispkit regexp)`, `(lispkit gvector)`, `(lispkit date-time)`
- Extended `(lispkit vector)` and `(lispkit list)` libraries
- Support for `let-keywords` and `let*-keywords` in library `(lispkit control)`
- Complete re-write of library import and export logic fixing numerous bugs and incompatibilities with R7RS
- Improved reporting of errors in library definitions
- Support call tracing for individual procedures
- Make the last three REPL results available via `*1`, `*2`, and `*3`
- Allow `@` as initial character in identifiers

## 1.6.0 (2019-01-04)
- New libraries: `(lispkit log)`, `(lispkit char-set)`, `(scheme char)`, `(srfi 14 ascii)`, `(srfi 101)`, `(srfi 125)`
- Support Scheme libraries from R7RS large/Red edition:  `(scheme box)`,  `(scheme charset)`,
  `(scheme comparator)`, `(scheme generator)`,
  `(scheme hash-table)`, `(scheme ideque)`, `(scheme list)`, `(scheme rlist)`, `(scheme set)`,
  `(scheme sort)`, `(scheme stream)`, `(scheme text)`, `(scheme vector)`
- Extended library `(lispkit test)`: support nested test groups, approximate tests, and handle exceptions correctly
- Handle closing of ports correctly in library `(lispkit port)`
- Fix major bug in library `(lispkit system)` affecting the composition of file paths
- Bug fixes affecting `fold-left`, `max`, `min`, `numerator`, `denominator`, `log`, `magnitude`, `gcd` and `lcm`, as well as the escaping of symbols
- Move from `#\dx????` syntax to `#\x????` to represent character literals
- Return more user-friendly error messages for operating system errors

## 1.5.4 (2018-11-03)
- Migrated project to Xcode 10.1 and ported code to Swift 4.2.1
- Included implementation of "Tiny CLOS" as library `(lispkit clos)`
- New SRFI libraries: SRFI 23, SRFI 34, SRFI 39, SRFI 95

## 1.5.3 (2018-10-21)
- Migrated project to Xcode 10.0 and ported code to Swift 4.2
- Small bug fixes in library `(lispkit draw)`
- Fixed serious hashing bug (crashing LispKit)
- New SRFI library: SRFI 14, SRFI 16

## 1.5.2 (2018-09-16)
- Several substantial extensions of library `(lispkit draw)`
- Support for turtle graphics via library `(lispkit draw turtle)`
- New example code showcasing `(lispkit draw turtle)` features
- New SRFI library: SRFI 11, SRFI 51, SRFI 161

## 1.5.1 (2018-08-19)
- Bugfixes and name changes in `(lispkit draw)`
- New example code showcasing `(lispkit draw)` features

## 1.5.0 (2018-08-10)
- Allow importing multiple libraries with one `import` invocation
- Mark continuations correctly and fix `continuation?`
- Turn `current-input-port`, `current-output-port`, and `current-error-port` into parameter objects
- New library: `(lispkit draw)`
- New SRFI libraries: SRFI 111, SRFI 112, SRFI 113
- Fixed bugs in SRFI 69
- Extend `(lispkit test)` to be more compatible to similar libraries

## 1.4.1 (2018-06-23)
- Fix memory leaks
- Provide a comfortable command-line interface supporting both a read-eval-print loop and the execution of scripts
- Prelude and libraries path preferences are now handled correctly and do not result in access issues anymore
- Programs blocking on functions like `read` can now be terminated
- Minor bugs in bitwise operations for exact integers of arbitrary size fixed
- `string-split` now returns a list instead of a vector
- Complete rewrite of the error reporting subsystem, including support for `file-error?` and `read-error?`
- New library: `(lispkit test)`
- New SRFI libraries: SRFI 69, SRFI 129, SRFI 137, SRFI 145, SRFI 151
- New example code for coroutines, HTTP support, and a small compiler for arithmetic expressions

## 1.4.0 (2018-03-30)
- Migrated project to Xcode 9.3 and Swift 4
- Bug fixes (esp. in `syntax-rules`)
- Fixed logic for referencing `unquote`, etc. in `backquote`.
- Include native date/time operations and functionality for accessing user data
- Native support for a few common string functions
- Support for `read-token` (generalization of `read-line`)
- Added libraries `(lispkit stack)`, `(lispkit queue)`, `(lispkit logic)`
- Implement bitwise operations for exact integers of arbitrary size
- Complete rewrite of the error reporting and representation sub-system
- Preparations for managing source locations

## 1.3.0 (2017-12-03)
- Support simple HTTP API
- Support compression for bytevectors
- Implement call tracing
- Fixed bug preventing some internal definitions to not work
- Support all standard R7RS small Scheme libraries
- Support for: `(srfi 158)`, `(lispkit wt-tree)`, `(lispkit object)`

## 1.2.0 (2017-10-22)
- Support for tail patterns in `syntax-rules`
- Support for `features` and `cond-expand`
- Support for `include` and `include-library-declarations`
- Support for `syntax-error`
- Support for `define-values`
- Support a new lightweight custom type declaration mechanism via `make-type`
- Added SRFI 112-style support for human-readable information about the hardware
   and software configuration on which LispKit is being executed
- Added support for the following libraries: `(srfi 63)`, `(srfi 64)`, `(srfi 128)`,
   `(lispkit iterate)`, `(lispkit json)`

## 1.1.0 (2017-09-25)
- Migrated project to Xcode 9 and Swift 4
- Adopted Swift 4-version of NumberKit

## 1.0.0 (2017-08-06)
- Support for custom ellipsis in `syntax-rules`
- Fixed serious scoping issues in `syntax-rules`
- Fixed hash functions to prevent overflows
- Support for R6RS enumeration operations

## 0.7.0 (2017-04-30)
- Implemented native support for fixnum (fx*) and flonum (fl*) operations
- Completed support for all R7RS string operations
- Completed support for all R7RS character operations
- Added common list operations (sort, filter, partition)
- Completed support for all R7RS vector operations

## 0.6.0 (2017-02-12)
- Automatically load libraries
- Made stack grow automatically
- Support externally triggered termination of evaluation
– Implemented new system library; added new file functions
- Implemented support for [R7RS](http://www.r7rs.org)-compliant exceptions
- Implemented support for multiple return values
– Ported various SRFIs and included them in the LispKit package

## 0.5.0 (2016-11-13)
- Implemented environments as first-class values
- Introduced a new R7RS-compatible library abstraction
- Reimplemented all functions using libraries

## 0.4.0 (2016-09-04)
- Migration from Swift 2.2 to Swift 3.0
- Implemented [R6RS](http://www.r6rs.org)-compliant hash tables
- Implemented [R7RS](http://www.r7rs.org)-compliant parameters (supporting dynamic scoping)
- Implemented [R7RS](http://www.r7rs.org)-compliant record types

## 0.3.0 (2016-07-05)
- Implemented [R7RS](http://www.r7rs.org)-compliant promises
- Implemented [R7RS](http://www.r7rs.org)-compliant port library, supporting both textual
  ports and binary ports; built on top of Foundation API (not using low-level C port
  abstractions) 
- Fixed a few cases where the runtime didn't do proper tail calls
- Implemented full support for `call/cc`

## 0.2.0 (2016-05-16)
- Revised bytecode instruction set. Added documentation to the
  [LispKit Wiki](https://github.com/objecthub/swift-lispkit/wiki).
- Implemented compiler optimization framework. Turned compiler into a two-phase compiler.
- Optimized usage of variables for function arguments.

## 0.1.0 (2016-05-02)
- Initial version consisting of the framework and a very simple read-eval-print loop
- The LispKit framework implements a subset of the
  [R5RS Scheme standard](http://www.schemers.org/Documents/Standards/R5RS/HTML/); the
  biggest feature missing is support for `call/cc`
- LispKit coonsists of a compiler generating bytecode and a virtual machine which
  interprets the bytecode
- The framework is incomplete and work in progress
