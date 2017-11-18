<img src="Assets/lispkit_logo_small.png" alt="LispKit" width="80" height="80" align="middle" />&nbsp;Swift LispKit
========================

[![Platform: macOS](https://img.shields.io/badge/Platform-macOS-blue.svg?style=flat)](https://developer.apple.com/osx/)
[![Language: Swift 4.0](https://img.shields.io/badge/Language-Swift%204.0-green.svg?style=flat)](https://developer.apple.com/swift/)
[![IDE: Xcode 9.1](https://img.shields.io/badge/IDE-Xcode%209.1-orange.svg?style=flat)](https://developer.apple.com/xcode/)
[![Carthage: compatible](https://img.shields.io/badge/Carthage-compatible-4BC51D.svg?style=flat)](https://github.com/Carthage/Carthage)
[![License: Apache](http://img.shields.io/badge/License-Apache-lightgrey.svg?style=flat)](https://raw.githubusercontent.com/objecthub/swift-lispkit/master/LICENSE)


## Overview

_LispKit_ is a framework for building Lisp-based extension and scripting languages
for macOS applications. _LispKit_ is fully written in the programming language
[Swift](http://www.swift.org). _LispKit_ implements a core language based on the
[R7RS (small) Scheme standard](http://www.r7rs.org). It is extensible,
allowing the inclusion of new native libraries written in Swift, of new libraries written
in Scheme, as well as custom modifications of the core environment consisting of
a compiler, a virtual machine as well as the core libraries.

So far, performance was not a priority in the development of _LispKit_. The _LispKit_
compiler does not perform many code optimizations and the performance of the system is
below state of the art Lisp and Scheme implementations.

[LispPad](http://lisppad.objecthub.net) implements a simple, lightweight, integrated
development environment for _LispKit_ on macOS.


## Features

_LispKit_ provides support for the following core features, many of which are based on R7RS:

  - Modules based on R7RS libraries
  - Hygienic macros based on `syntax-rules`
  - First-class environments
  - `call/cc`, `dynamic-wind` and exceptions
  - Dynamically-scoped parameters
  - Multiple return values
  - Delayed execution via promises and streams
  - Support for the full numerical tower consisting of arbitrary size integers, rationals,
    real numbers, and inexact complex numbers.
  - Unicode strings and characters
  - Vectors and bytevectors
  - Text and binary ports
  - R7RS-compliant records
  - [R6RS](http://www.r6rs.org)-compliant hashtables
  - [R6RS](http://www.r6rs.org)-compliant enumerations
  - All R7RS libraries: `(scheme base)`, `(scheme case-lambda)`, `(scheme complex)`, `(scheme cxr)`,
    `(scheme eval)`, `(scheme file)`, `(scheme inexact)`, `(scheme lazy)`, `(scheme load)`,
    `(scheme procss-context)`, `(scheme read)`, `(scheme repl)`, `(scheme time)`, `(scheme write)`,
    `(scheme r5rs)`
  - LispKit-specific libraries: `(lispkit datatype)`, `(lispkit iteration)`, `(lispkit set)`, `(lispkit heap)`,
    `(lispkit wt-tree)`, `(lispkit prettify)`, `(lispkit json)`, and `(lispkit pdf)`

_LispKit_ is incompatible or incomplete with respect to the following R7RS features:

  - Lists are immutable. Mutable cons-cells are supported in a way similar to
    [Racket](https://racket-lang.org)
  - Error support is incomplete; the following functions are missing: `read-error?`, `file-error?`
  - `current-input-port`, `current-output-port`, and `current-error-port` are functions
    (as required by R5RS) and not parameter objects (as required by R7RS)
  - The R5RS-fallback mechanism for handling symbols in a case-insensitive manner is not
    implemented yet; this includes support for special form `include-ci` as well as the `include-ci`
    in library definitions.
  - Environment-support lacks the following functions: `scheme-report-environment`,
    `null-environment`.
  - Datum comments introduced via `#;` do not always work as expected.

The following  [SRFI](https://srfi.schemers.org/) libraries have been ported to _LispKit_ and are included in the
framework:

  - [SRFI 1: List Library](https://srfi.schemers.org/srfi-1/srfi-1.html)
  - [SRFI 2: AND-LET* - an AND with local bindings, a guarded LET* special
             form](https://srfi.schemers.org/srfi-2/srfi-2.html)
  - [SRFI 8: receive - Binding to multiple values](https://srfi.schemers.org/srfi-8/srfi-8.html)
  - [SRFI 17: Generalized set!](https://srfi.schemers.org/srfi-17/srfi-17.html)
  - [SRFI 19: Time Data Types and Procedures](https://srfi.schemers.org/srfi-19/srfi-19.html)
  - [SRFI 27: Sources of Random Bits](https://srfi.schemers.org/srfi-27/srfi-27.html)
  - [SRFI 28: Basic Format Strings](https://srfi.schemers.org/srfi-28/srfi-28.html)
  - [SRFI 31: A special form rec for recursive evaluation](https://srfi.schemers.org/srfi-31/srfi-31.html)
  - [SRFI 35: Conditions](https://srfi.schemers.org/srfi-35/srfi-35.html)
  - [SRFI 41: Streams](https://srfi.schemers.org/srfi-41/srfi-41.html)
  - [SRFI 48: Intermediate Format Strings](https://srfi.schemers.org/srfi-48/srfi-48.html)
  - [SRFI 63: Homogeneous and Heterogeneous Arrays](https://srfi.schemers.org/srfi-63/srfi-63.html)
  - [SRFI 64: A Scheme API for test suites](https://srfi.schemers.org/srfi-64/srfi-64.html)
  - [SRFI 121: Generators](https://srfi.schemers.org/srfi-121/srfi-121.html)
  - [SRFI 128: Comparators](https://srfi.schemers.org/srfi-128/srfi-128.html)
  - [SRFI 132: Sort Libraries](https://srfi.schemers.org/srfi-132/srfi-132.html)
  - [SRFI 133: Vector Library](https://srfi.schemers.org/srfi-133/srfi-133.html)
  - [SRFI 134: Immutable Deques](https://srfi.schemers.org/srfi-134/srfi-134.html)
  - [SRFI 135: Immutable Texts](https://srfi.schemers.org/srfi-135/srfi-135.html)
  - [SRFI 142: Bitwise Operations](https://srfi.schemers.org/srfi-142/srfi-142.html)
  - [SRFI 152: String Library](https://srfi.schemers.org/srfi-152/srfi-152.html)
  - [SRFI 158: Generators and Accumulators](https://srfi.schemers.org/srfi-158/srfi-158.html)

## Architecture

From an architectural perspective, _LispKit_ consists of:

1. a compiler translating _LispKit_ expressions into bytecode, and
2. a virtual machine for interpreting the generated bytecode. The virtual machine is
stack-based, handles tail calls and continuations, and provides a garbage collector.

Details can be found in the [LispKit Wiki](https://github.com/objecthub/swift-lispkit/wiki).

The project provides a read-eval-print loop in form of a command-line tool.
This command-line tool that can be used to try out and experiment with the
framework. The read-eval-print loop parses the entered _LispKit_ expression,
compiles it to bytecode, executes it, and displays the result.


## Building and running the read-eval-print loop

First, clone the _LispKit_ repository via `git`. The following command will create a
directory `swift-lispkit`.

```sh
> git clone https://github.com/objecthub/swift-lispkit.git
Cloning into 'swift-lispkit'...
remote: Counting objects: 1849, done.
remote: Compressing objects: 100% (39/39), done.
remote: Total 1849 (delta 9), reused 0 (delta 0), pack-reused 1806
Receiving objects: 100% (1849/1849), 689.43 KiB | 666.00 KiB/s, done.
Resolving deltas: 100% (1430/1430), done.
```

### Running the read-eval-print loop in Xcode

Fetch dependencies and build them from scratch via `carthage`:
```sh
> cd swift-lispkit
> carthage bootstrap
*** Checking out swift-numberkit at "1.6.0"
*** xcodebuild output can be found in /var/folders/c_/h31lvvtx72s3zhc9bvxd0p480000gn/T/carthage-xcodebuild.46W8Z7.log
*** Building scheme "NumberKit (shared)" in NumberKit.xcodeproj
```

Now, it's possible to switch to Xcode and build the read-eval-print loop via
scheme `LispKitRepl`:
```sh
> open LispKit.xcodeproj
```

### Compiling the read-eval-print loop with the Swift Package Manager

A debug binary can be built in the following way:
```sh
> cd swift-lispkit
> swift build -Xswiftc "-target" -Xswiftc "x86_64-apple-macosx10.11" \
  -Xswiftc "-D" -Xswiftc "SPM"
Compile Swift Module 'NumberKit' (6 sources)
Compile Swift Module 'LispKit' (75 sources)
Compile Swift Module 'LispKitRepl' (2 sources)
Linking ./.build/x86_64-apple-macosx10.10/debug/LispKitRepl
```

A release binary can be built like this:
```sh
> cd swift-lispkit
> swift build -c release -Xswiftc -static-stdlib -Xswiftc "-target" \
  -Xswiftc "x86_64-apple-macosx10.11" -Xswiftc "-D" -Xswiftc "SPM"
Compile Swift Module 'NumberKit' (6 sources)
Compile Swift Module 'LispKit' (75 sources)
Compile Swift Module 'LispKitRepl' (2 sources)
Linking ./.build/x86_64-apple-macosx10.10/release/LispKitRepl
```


## Requirements

The read-eval-print loop can be built using the Swift Package Manager. Nevertheless, it is
recommended to use Xcode 9 in combination with Carthage:

- Xcode 9.1
- [Carthage](https://github.com/Carthage/Carthage)
- Swift 4.0
- [NumberKit](http://github.com/objecthub/swift-numberkit)
