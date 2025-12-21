# Changelog

## 2.6.0 (2025-12-21)

- Documentation for libraries is now included in this repository
- New library for handling locations: `(lispkit location)`
- New library for creating and editing PDF documents: `(lispkit pdf)`
- New library for solving computer vision problems: `(lispkit vision)`; the library supports optical character recognition (OCR), shape detection, barcode recognition, and image classification.
- New libraries for processing/manipulating images, providing access to Apple's _Core Image_ framework: `(lispkit image)`, `(lispkit image process)`
- New library for drawing maps: `(lispkit draw map)`
- New library for making and drawing snapshots of web pages: `(lispkit draw web)`
- New exported definitions in library `(lispkit draw)`: `clip-drawing`, `bitmap-ppi`, `closed-polygon`, `inset-rect`, `size-ratio`, `rect-mid-point`, `rect-mid-x`, `rect-mid-y`, `zero-size`, `transpose`. `make-shape` accepts a second optional argument `freeze?` which makes it possible to create a new shape from an existing one without introducing a dependency.
- Improvements for reading and writing images via `(lispkit draw)`: PDF files can now be read via `load-image`, `load-image-asset` and `bytevector->image` also on iOS. New procedures `save-image` and `image->bytevector` support saving images as PDF. `save-bitmap` and `bitmap->bytevector` now support a "quality factor" which indicates how strongly an image should be compressed.
- New procedures in library `(lispkit core)`: `generate-uninterned-symbol`, `symbol<?`
- New functionality for library `(lispkit enum)`: procedure `enum-tag-mapper` and enum constructors without name (to return the enum type)
- Make it easier to generate `date-time` strings with procedure `date-time->iso8601-string` and parse date-time strings via `string->date-time` conforming with ISO 8601/RFC 3339.
- Fixes `quasiquote` for expressions involving array literals
- Fixes the `(library ...)` directive of `cond-expand`
- Fixes classes in library `(lispkit object)`; generic procedure `object->string` was renamed to `object-description`
- New sample code: `ImageComposition.scm`, `OCR.scm`, `MermaidDiagrams.scm`, `Pinterest.scm`

## 2.5.0 (2024-11-21)

- Support for all major JSON standards via libraries `(lispkit json)` and `(lispkit json schema)` supporting _JSON Pointer_ \([RFC 6901](https://datatracker.ietf.org/doc/html/rfc6901/)\), _JSON Path_ \([RFC 9535](https://datatracker.ietf.org/doc/html/rfc9535/)\), _JSON Patch_ \([RFC 6902](https://datatracker.ietf.org/doc/html/rfc6902/)\), _JSON Merge Patch_ \([RFC 7396](https://datatracker.ietf.org/doc/html/rfc7396/)\) and _JSON Schema_ \([2020-12 Internet Draft specification](https://datatracker.ietf.org/doc/draft-bhutton-json-schema/)\).
- Support for HTTP-based networking via libraries `(lispkit http)`, `(lispkit http oauth)`, and `(lispkit http server)`, including support for _OAuth 2.0_ \([RFC 6749](https://datatracker.ietf.org/doc/html/rfc6749)\) and simple HTTP servers.
- Deeper integrations into macOS and iOS operating systems via libraries `(lispkit system keychain)` and `(lispkit system pasteboard)`.
- New library for handling URLs: `(lispkit url)`
- New concurrency features with support for _atomic boxes_ via library `(lispkit box)` and thread-safe shared queues via library `(lispkit thread shared-queue)`.
- Serialization of data into a binary representation for a subset of LispKit's data types via library `(lispkit serialize)`.
- Support for drawing a variety of different types of bar codes via library `(lispkit draw barcode)`
- New procedure in library `(lispkit thread)`: `abort-running-threads`
- New procedures in library `(lispkit system)`: `available-network-interfaces`, `region-continent`, `region-parent`, and `region-subregions`
- New procedures in library `(lispkit string)`: `url-encode` and `url-decode`
- New procedures in library `(lispkit bytevector)`: `bytevector-zip-header?` and `bytevector-gzip-header?`
- New procedures in library `(lispkit markdown)`: `markdown->sxml`, `blocks->sxml`, `text->sxml`, `markdown->raw-string`, and `blocks->raw-string`
- New/updated procedures in library `(lispkit dynamic)`: `error-object->string` and `error-object-message`
- New special forms in library `(lispkit control)`: `if-let*` and `when-let*`
- LispKitRepl now supports asynchronous libraries (libraries which need a run loop) via command-line argument `-x`
- New sample code: `Keychain.scm`, `WebAPIs.scm`, and `Webserver.scm`

## 2.4.0 (2024-01-15)

- Ported universal formatting facility from Common Lisp and made it available via library `(lispkit format)`
- Library `(lispkit records)` now supports extensible records compatible to SRFI 131
- New procedures in library `(lispkit system)`: `available-region?`, `region-flag`, `available-language?`, `available-currencies`, `available-currency?`, `currency-name`, `currency-code`, `currency-numeric-code`, and `currency-symbol`
- New procedures in library `(lispkit port)`: `display-format`, `write-formatted`
- New procedures in library `(lispkit bitset)`: `fixnum->bitset`, `bitset->fixnum`
- New procedures in library `(lispkit system)`: `terminal-size`, `make-uuid-string`, `make-uuid-bytevector`
- New procedure in library `(lispkit draw)`: `clear-drawing`
- New procedures in library `(lispkit draw turtle)`: `arc`, `turtle-x`, `turtle-y`, `turtle-angle`, `turtle-pen-down?`
- New procedure `load-program` in library `(lispkit core)` supports executing programs in an empty environment
- Procedure `type-of` of library `(lispkit type)` now returns a list of type tags (from most specific to least specific type)
- Renamed library `(lispkit system os)` to `(lispkit system call)`
- Renamed `object` in library `(lispkit type)` to `obj` to prevent conflicts with other libraries
- Extend library `(lispkit bytevector)` with procedures `bytevector=?`, `bytevector->hex`, and `hex->bytevector`
- Reimplementation of procedure `open-file` in library `(lispkit system)` supporting custom application paths
- Enforce that imported (immutable) definitions cannot be mutated, unless it's in the REPL; `export-mutable` in library definitions enable mutability
- Allow configuration of REPL output via format config `repl-format-config` of library `(lispkit format)`
- Improved display of OS-level exceptions and errors in REPL
- New libraries: `(lispkit format)`, `(lispkit crypto)`, `(lispkit archive tar)`, `(lispkit list set)`, `(srfi 239)`, `(srfi 235)`.
- New sample code: `Blockchain.scm`

## 2.3.2 (2023-02-05)

- Handle assets correctly in the LispKit REPL
- Improved R7RS regression tests
- Bugfixes in library `(lispkit bytevector)` for procedures `bytevector-copy` and `bytevector-copy!`
- Revamp of library `(lispkit graph)` with new procedures: `graph-topological-sort`, `graph-graph-weakly-connected-components`, `graph-strongly-connected-components`, and `graph-shortest-paths`
- New libraries: `(srfi 228)`, `(srfi 233)`, and `(srfi 236)`

## 2.3.1 (2022-12-10)

- Fixed bug leading to deadlocks when using text ports
- Fixed bug allowing to execute empty lists
- Fixed serious bug leading to an infinite loop when iterating through stack traces
- Limit stack size to prevent application crashes
- New procedure in library `(lispkit thread)`: `thread-max-stack`

## 2.3.0 (2022-08-01)

- Major revamp of all type-related functionality: each type is now represented by a type tag/symbol; procedure `type-for` from library `(lispkit type)` can be used to determine the type tag of a given object; breaking change for `make-type`, which now returns 5 values (the first is a new type tag)
- Support complex numbers for trigonometric and inverse trigonometric functions in library `(lispkit math)`: `sin`, `cos`, `tan`, `asin`, `acos`, and `atan`
- Bug fixes in library `(lispkit draw chart bar)` and `(lispkit enum)`
- New library: `(lispkit math matrix)`

## 2.2.1 (2022-05-28)

- Fixed bug preventing LispKit to correctly determine the maximum number of threads
- Optimized display of objects of custom types
- Optimized code generation for lambdas without captured expressions
- Revamped code disassembly
- Fixed division by zero issues with truncate and floor procedures
- Removed duplicates in results of procedures `available-fonts` and `available-font-families` in library `(lispkit draw)`
- Changed defaults for procedure `string-insert!` of library `(lispkit string)`
- Made procedure `open-file` of library `(lispkit system)` work on iOS
- Reimplemented and extended library `(lispkit enum)` making it compatible with SRFI 209
- Major revision of library `(lispkit clos)`
- New procedures in library `(lispkit draw)`: `bytevector->image`, `draw-styled-text`, `styled-text-size`
- New procedure in library `(lispkit core)`: `procedure-rename`
- New libraries: `(srfi 118)`, `(srfi 141)`, `(srfi 149)`, `(srfi 232)`, `(lispkit bitset)`, `(lispkit draw chart bar)`, `(lispkit styled-text)`
- New example code: `ObjectOrientation.scm`, `DrawBarCharts.scm`, `StyledTextDoc.scm`

## 2.2.0 (2022-02-06)

- Multi-threaded evaluator, executing multiple virtual machines in parallel
- Go-inspired channels for synchronizing threads
- Revamp of math libraries, addressing incompatibilities and fixing numerous bugs:
    - Don't crash on division by zero for `fx/`
    - Fix bug making `exact` work also for negative numbers
    - Improved `inexact` when used with rationals with a very large numerator or denominator
    - Consistent interface for: `random`, `flrandom`, and `fxrandom`
    - Support unary argument usage for `fx-`, `fl-`, and `fl/`
    - Removed `fixnum-width`, `least-fixnum` and `greatest-fixnum`
    - New procedures `make-flonum`, `flexponent`, `flsignificand`, `flnext`, `flprev`, `fx-width`, `fx-greatest`, `fx-least`, `fl-epsilon`, `fl-greatest`, and `fl-least`
    - Support many arguments for `flmin`, `flmax`, `fxmin`, `fxmax`, `fx+`, `fx-`, `fx*`, `fx/`, `fx=`, `fx<`, `fx>`, `fx<=`, `fx>=`, `fl+`, `fl-`, `fl*`, `fl/`, `fl=`, `fl<`, `fl>`, `fl<=`, and `fl>=`
- New procedures in library `(lispkit math util)`: `make-nan`, `nan-negative?`, `nan-quiet?`, `nan-payload`, and `nan=?`
- New procedures in library `(lispkit system)`: `physical-memory`, `memory-footprint`, and `system-uptime`
- New procedure in library `(lispkit port)`: `display*`
- New procedures in library `(lispkit debug)`: `stack-size`, `call-stack-procedures`, `call-stack-trace`, and `set-max-call-stack!`
- Enabled concurrency support for library `(srfi sicp)`
- Included new libraries: `(lispkit thread)`, `(lispkit thread channel)`, `(scheme flonum)`, `(srfi 18)`, `(srfi 144)`, `(srfi 208)`, `(srfi 230)`
- Fixed scope of `<sym>` in `(let <sym> ...)` form, making it not accessible in the bindings
- Exceptions now include more information about the active call stack
- Included tutorial for channels as new example code
- Support comments in command-line input

## 2.1.0 (2021-12-11)

- Fixed bug compiling every procedure twice
- Fixed implementation of procedure `expt`
- Fixed overflow issues in procedure `approximate`
- Fixed crashes in the rounding functions when used with rational big integers
- Moved procedure `load` into library `(lispkit core)`
- Integrated simple bytecode optimizer
- Implemented support for tagged procedures
- Implemented support for procedures with optional arguments
- Included new libraries: `(lispkit math util)`, `(lispkit math stats)`, `(srfi 166)`, `(srfi 227)`, and `(srfi 229)`
- Ported Peter Norvig's pattern matcher and algebraic simplifier from Common Lisp to Scheme and included it as new example code
- Extended sample code `Math.scm`

## 2.0.3 (2021-09-12)

- Fixed bug in logic to detect valid local definitions
- Handle libraries with errors more carefully to prevent crashes
- Fixed bit counting bug in library `(srfi 143)`
- New procedures in library `(lispkit core)`: `thunk?`, `procedure-of-arity?`, `procedure-name`, `procedure-arity`, `procedure-arity-range`, `procedure-arity-includes?`, `arity-at-least?`, `arity-at-least-value`
- Included new libraries: `(srfi sicp)`, `(srfi 102)`, `(srfi 217)`, `(srfi 224)`
- New sample code: `EUStats.scm`

## 2.0.2 (2021-08-08)

- Fixed serious bug in procedure `load` (previously, `load` always returned no result instead of the result of the last executed expression).
- Included new libraries: `(srfi 215)`, `(srfi 216)`, `(srfi 222)`, `(lispkit text-table)`
- Extended sample code `Math.scm`

## 2.0.1 (2021-07-02)

- New procedures in library `(lispkit draw)`: `bitmap->bytevector`, `bitmap-blur`, `bitmap-crop`
- Support tables and definition lists in library `(lispkit markdown)`
- Support gzip and zlib container formats for deflate compression via library `(lispkit bytevector)`
- Support symlink resolution via procedure `file-path` of library `(lispkit system)`
- Bug fixes in library `(lispkit system)`: `home-directory` also supports a non-sandboxed mode, `file-path` handles tilde correctly
- Bug fixes for a few procedures of library `(lispkit archive zip)`
- Included new libraries: `(srfi 219)`, `(srfi 221)`, `(srfi 223)`

## 2.0.0 (2021-05-01)

- Support for iOS
- Included an iOS REPL application
- Implemented simple support for associating description text with definitions (in Common Lisp style)
- Changed `let*` and `letrec*` to allow for the redefinition of variables
- Included new libraries: `(lispkit archive zip)`, `(lispkit prolog)`, `(srfi 143)`, `(srfi 189)`, `(srfi 214)`, `(scheme red)`, `(scheme fixum)`, `(scheme bitwise)`, `(scheme division)`
- Extended library `(lispkit string)` with procedures `string-decode-named-chars` and `string-encode-named-chars`
- Extended library `(lispkit char)` with procedure `char-name`
- Extended library `(lispkit core)` with new environment procedures: `interaction-environment?`, `custom-environment?`, `the-environment`, `environment-definable?`, `environment-define`, `environment-define-syntax`, `environment-import`, `environment-documentation`, and `environment-assign-documentation!`
- Extended library `(lispkit sqlite)` with procedures `sqlite-database?` and `sqlite-statement?`
- Included support for an optional environment for the `compile` procedure from `(lispkit debug)`
- Included new sample code: `VisualizePointSets.scm`, `Schelog.scm`
- Migrated project to Xcode 12.5 and Swift 5.4

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
