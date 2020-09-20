<img src="Assets/lispkit_logo_small.png" alt="LispKit" width="80" height="80" align="middle" />&nbsp;Swift LispKit
========================

[![Platform: macOS](https://img.shields.io/badge/Platform-macOS-blue.svg?style=flat)](https://developer.apple.com/osx/)
[![Language: Swift 5.3](https://img.shields.io/badge/Language-Swift%205.3-green.svg?style=flat)](https://developer.apple.com/swift/)
[![IDE: Xcode 12.0](https://img.shields.io/badge/IDE-Xcode%2012.0-orange.svg?style=flat)](https://developer.apple.com/xcode/)
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

The [NumericalScheme](https://github.com/objecthub/swift-numericalscheme) demo
showcases how to create a derived _LispKit_ interpreter that inherits everything from
_LispKit_ (without code duplication) and defines a new native as well as Scheme-based library.

[LispPad](http://lisppad.objecthub.net) implements a simple, lightweight, integrated
development environment for _LispKit_ on macOS with a Cocoa-based UI. The
[LispPad Library Reference](http://lisppad.objecthub.net/resources/LispPad_Reference_1.5.pdf)
documents the core LispPad and LispKit libraries in
[PDF](http://lisppad.objecthub.net/resources/LispPad_Reference_1.5.pdf) form. A simpler
command-line tool with similar functionality is provided by the LispKit framework itself
(see below).


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
  - All R7RS (small) libraries: `(scheme base)`, `(scheme case-lambda)`, `(scheme char)`, `(scheme complex)`,
    `(scheme cxr)`, `(scheme eval)`, `(scheme file)`, `(scheme inexact)`, `(scheme lazy)`,
    `(scheme load)`, `(scheme process-context)`, `(scheme read)`, `(scheme repl)`, `(scheme time)`,
    `(scheme write)`, `(scheme r5rs)`
  - Some R7RS (large) libraries from Scheme Red and Scheme Tangerine editions:
    `(scheme box)`,  `(scheme charset)`, `(scheme comparator)`, `(scheme generator)`,
    `(scheme hash-table)`, `(scheme ideque)`, `(scheme list)`, `(scheme mapping)`,
    `(scheme rlist)`, `(scheme set)`,
    `(scheme sort)`, `(scheme stream)`, `(scheme text)`, `(scheme vector)`
  - LispKit-specific libraries: 
    [`(lispkit base)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Base),
    [`(lispkit core)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Core),
    [`(lispkit control)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Control),
    [`(lispkit system)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-System),
    [`(lispkit system os)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-System-OS),
    [`(lispkit box)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Box),
    [`(lispkit math)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Math),
    [`(lispkit list)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-List),
    [`(lispkit hashtable)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Hashtable),
    [`(lispkit dynamic)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Dynamic),
    [`(lispkit type)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Type),
    [`(lispkit vector)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Vector),
    [`(lispkit gvector)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Gvector),
    [`(lispkit record)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Record),
    [`(lispkit bytevector)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Bytevector),
    [`(lispkit char)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Char),
    [`(lispkit char-set)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Char-Set),
    [`(lispkit string)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-String),
    [`(lispkit port)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Port),
    [`(lispkit date-time)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Date-Time),
    [`(lispkit draw)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Draw),
    [`(lispkit draw turtle)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Draw-Turtle),
    [`(lispkit datatype)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Datatype),
    [`(lispkit object)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Object),
    [`(lispkit enum)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Enum),
    [`(lispkit regexp)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Regexp),
    [`(lispkit stream)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Stream),
    `(lispkit graph)`,
    [`(lispkit match)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Match),
    [`(lispkit iterate)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Iterate),
    [`(lispkit log)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Log),
    `(lispkit debug)`,
    [`(lispkit set)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Set),
    [`(lispkit stack)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Stack), 
    [`(lispkit queue)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Queue), 
    [`(lispkit heap)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Heap),
    [`(lispkit disjoint-set)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Disjoint-Set),
    `(lispkit wt-tree)`,
    [`(lispkit comparator)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Comparator),
    [`(lispkit combinator)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Combinator),
    `(lispkit logic)`,
    `(lispkit clos)`,
    [`(lispkit test)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Test),
    `(lispkit prettify)`,
    [`(lispkit csv)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-CSV),
    [`(lispkit markdown)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-Markdown),
    [`(lispkit sqlite)`](https://github.com/objecthub/swift-lispkit/wiki/LispKit-SQLite),
    `(lispkit json)`,
    `(lispkit sxml)`,
    `(lispkit sxml xml)`,
    `(lispkit sxml html)`,
    and `(lispkit pdf)`.

_LispKit_ is incompatible or incomplete with respect to the following R7RS features:

  - Lists are immutable. Mutable cons-cells are supported in a way similar to
    [Racket](https://racket-lang.org)
  - Datum comments introduced via `#;` do not always work as in other Scheme dialects.

The following  [SRFI](https://srfi.schemers.org/) libraries have been ported to _LispKit_ and are included in the
framework:

  - [SRFI 1: List Library](https://srfi.schemers.org/srfi-1/srfi-1.html)
  - [SRFI 2: AND-LET* - an AND with local bindings, a guarded LET* special form](https://srfi.schemers.org/srfi-2/srfi-2.html)
  - [SRFI 6: Basic String Ports](https://srfi.schemers.org/srfi-6/srfi-6.html)
  - [SRFI 8: receive - Binding to multiple values](https://srfi.schemers.org/srfi-8/srfi-8.html)
  - [SRFI 11: Syntax for receiving multiple values](https://srfi.schemers.org/srfi-11/srfi-11.html)
  - [SRFI 14: Character-set library](https://srfi.schemers.org/srfi-14/srfi-14.html)
  - [SRFI 16: Syntax for procedures of variable arity](https://srfi.schemers.org/srfi-16/srfi-16.html)
  - [SRFI 17: Generalized set!](https://srfi.schemers.org/srfi-17/srfi-17.html)
  - [SRFI 19: Time Data Types and Procedures](https://srfi.schemers.org/srfi-19/srfi-19.html)
  - [SRFI 23: Error reporting mechanism](https://srfi.schemers.org/srfi-23/srfi-23.html)
  - [SRFI 26: Notation for Specializing Parameters without Currying](https://srfi.schemers.org/srfi-26/srfi-26.html)
  - [SRFI 27: Sources of Random Bits](https://srfi.schemers.org/srfi-27/srfi-27.html)
  - [SRFI 28: Basic Format Strings](https://srfi.schemers.org/srfi-28/srfi-28.html)
  - [SRFI 31: A special form rec for recursive evaluation](https://srfi.schemers.org/srfi-31/srfi-31.html)
  - [SRFI 33: Integer Bitwise-operation Library](https://srfi.schemers.org/srfi-33/srfi-33.html)
  - [SRFI 34: Exception Handling for Programs](https://srfi.schemers.org/srfi-34/srfi-34.html)
  - [SRFI 35: Conditions](https://srfi.schemers.org/srfi-35/srfi-35.html)
  - [SRFI 39: Parameter objects](https://srfi.schemers.org/srfi-39/srfi-39.html)
  - [SRFI 41: Streams](https://srfi.schemers.org/srfi-41/srfi-41.html)
  - [SRFI 46: Basic Syntax-rules Extensions](https://srfi.schemers.org/srfi-46/srfi-46.html)
  - [SRFI 48: Intermediate Format Strings](https://srfi.schemers.org/srfi-48/srfi-48.html)
  - [SRFI 51: Handling rest list](https://srfi.schemers.org/srfi-51/srfi-51.html)
  - [SRFI 54: Formatting](https://srfi.schemers.org/srfi-54/srfi-54.html)
  - [SRFI 55: require-extension](https://srfi.schemers.org/srfi-55/srfi-55.html)
  - [SRFI 63: Homogeneous and Heterogeneous Arrays](https://srfi.schemers.org/srfi-63/srfi-63.html)
  - [SRFI 64: A Scheme API for test suites](https://srfi.schemers.org/srfi-64/srfi-64.html)
  - [SRFI 69: Basic hash tables](https://srfi.schemers.org/srfi-69/srfi-69.html)
  - [SRFI 87: `=>` in case clauses](https://srfi.schemers.org/srfi-87/srfi-87.html)
  - [SRFI 95: Sorting and Merging](https://srfi.schemers.org/srfi-95/srfi-95.html)
  - [SRFI 98: An interface to access environment variables](https://srfi.schemers.org/srfi-98/srfi-98.html)
  - [SRFI 101: Purely Functional Random-Access Pairs and Lists](https://srfi.schemers.org/srfi-101/srfi-101.html)
  - [SRFI 111: Boxes](https://srfi.schemers.org/srfi-111/srfi-111.html)
  - [SRFI 112: Environment inquiry](https://srfi.schemers.org/srfi-112/srfi-112.html)
  - [SRFI 113: Sets and bags](https://srfi.schemers.org/srfi-113/srfi-113.html)
  - [SRFI 121: Generators](https://srfi.schemers.org/srfi-121/srfi-121.html)
  - [SRFI 125: Intermediate hash tables](https://srfi.schemers.org/srfi-125/srfi-125.html)
  - [SRFI 128: Comparators](https://srfi.schemers.org/srfi-128/srfi-128.html)
  - [SRFI 129: Titlecase procedures](https://srfi.schemers.org/srfi-129/srfi-129.html)
  - [SRFI 132: Sort Libraries](https://srfi.schemers.org/srfi-132/srfi-132.html)
  - [SRFI 133: Vector Library](https://srfi.schemers.org/srfi-133/srfi-133.html)
  - [SRFI 134: Immutable Deques](https://srfi.schemers.org/srfi-134/srfi-134.html)
  - [SRFI 135: Immutable Texts](https://srfi.schemers.org/srfi-135/srfi-135.html)
  - [SRFI 137: Minimal Unique Types](https://srfi.schemers.org/srfi-137/srfi-137.html)
  - [SRFI 142: Bitwise Operations](https://srfi.schemers.org/srfi-142/srfi-142.html)
  - [SRFI 145: Assumptions](https://srfi.schemers.org/srfi-145/srfi-145.html)
  - [SRFI 146: Mappings](https://srfi.schemers.org/srfi-146/srfi-146.html)
  - [SRFI 151: Bitwise Operations](https://srfi.schemers.org/srfi-151/srfi-151.html)
  - [SRFI 152: String Library](https://srfi.schemers.org/srfi-152/srfi-152.html)
  - [SRFI 158: Generators and Accumulators](https://srfi.schemers.org/srfi-158/srfi-158.html)
  - [SRFI 161: Unifiable Boxes](https://srfi.schemers.org/srfi-161/srfi-161.html)
  - [SRFI 162: Comparators sublibrary](https://srfi.schemers.org/srfi-162/srfi-162.html)  
  - [SRFI 165: The Environment Monad](https://srfi.schemers.org/srfi-165/srfi-165.html)
  - [SRFI 167: Ordered Key Value Store](https://srfi.schemers.org/srfi-167/srfi-167.html)
  - [SRFI 173: Hooks](https://srfi.schemers.org/srfi-173/srfi-173.html)
  - [SRFI 174: POSIX Timespecs](https://srfi.schemers.org/srfi-174/srfi-174.html)
  - [SRFI 175: ASCII Character Library](https://srfi.schemers.org/srfi-175/srfi-175.html)
  - [SRFI 177: Portable keyword arguments](https://srfi.schemers.org/srfi-177/srfi-177.html)
  - [SRFI 195: Multiple-value boxes](https://srfi.schemers.org/srfi-195/srfi-195.html)
  - [SRFI 196: Range Objects](https://srfi.schemers.org/srfi-196/srfi-196.html)


## Project

The project defines three different targets:

- __LispKit__: the core interpreter framework, including all support files
- __LispKitTools__: a framework for tools supporting _LispKit_; e.g. a read-eval-print framework
- __LispKitRepl__: a command-line tool implementing a read-eval-print loop


## Architecture

From an architectural perspective, _LispKit_ consists of:

1. a compiler translating _LispKit_ expressions into bytecode,
2. a virtual machine for interpreting the generated bytecode. The virtual machine is
stack-based, handles tail calls and continuations, and provides a garbage collector.
3. a large range of libraries, all packaged together with the framework.

Details can be found in the [LispKit Wiki](https://github.com/objecthub/swift-lispkit/wiki).


## Command-line tool

### Overview

This project also includes a command-line tool, called the _LispKit Shell_, for executing
LispKit applications in the terminal. It can  be used to try out and experiment with
the LispKit framework. The command-line tool can also be used interactively as a
read-eval-print loop. The read-eval-print loop parses the entered _LispKit_ expression,
compiles it to bytecode, executes it, and displays the result.

### Downloading the source code

First, clone the _LispKit_ repository via `git`. The following command will create a directory `swift-lispkit`.

```sh
> git clone https://github.com/objecthub/swift-lispkit.git
Cloning into 'swift-lispkit'...
remote: Enumerating objects: 14, done.
remote: Counting objects: 100% (14/14), done.
remote: Compressing objects: 100% (12/12), done.
remote: Total 5300 (delta 1), reused 14 (delta 1), pack-reused 5286
Receiving objects: 100% (5300/5300), 1.76 MiB | 132.00 KiB/s, done.
Resolving deltas: 100% (3724/3724), done.
```

Next, switch to Xcode and build the _LispKit_ command-line tool via scheme `LispKitRepl`:

```sh
> open LispKit.xcodeproj
```

### Compiling the command-line tool with the Swift Package Manager

A debug binary can be built in the following way:

```sh
> cd swift-lispkit
> swift build -Xswiftc "-D" -Xswiftc "SPM"
Fetching https://github.com/objecthub/swift-numberkit.git
Fetching https://github.com/objecthub/swift-commandlinekit.git
Fetching https://github.com/objecthub/swift-markdownkit.git
Completed resolution in 6.47s
Cloning https://github.com/objecthub/swift-numberkit.git
Resolving https://github.com/objecthub/swift-numberkit.git at 2.3.2
Cloning https://github.com/objecthub/swift-markdownkit.git
Resolving https://github.com/objecthub/swift-markdownkit.git at 0.2.0
Cloning https://github.com/objecthub/swift-commandlinekit.git
Resolving https://github.com/objecthub/swift-commandlinekit.git at 0.3.1
[124/124] Linking LispKitRepl
```

The debug binary can be run by invoking `.build/debug/LispKitRepl -d LispKit`
assuming that directory `~/Documents/LispKit` contains a copy of the
[resources directory](https://github.com/objecthub/swift-lispkit/tree/master/Sources/LispKit/Resources)
needed to run the command-line tool.

A release binary can be built like this:

```sh
> cd swift-lispkit
> swift build -c release -Xswiftc "-D" -Xswiftc "SPM"
Fetching https://github.com/objecthub/swift-numberkit.git
Fetching https://github.com/objecthub/swift-commandlinekit.git
Fetching https://github.com/objecthub/swift-markdownkit.git
Completed resolution in 4.02s
Cloning https://github.com/objecthub/swift-numberkit.git
Resolving https://github.com/objecthub/swift-numberkit.git at 2.3.2
Cloning https://github.com/objecthub/swift-markdownkit.git
Resolving https://github.com/objecthub/swift-markdownkit.git at 0.2.0
Cloning https://github.com/objecthub/swift-commandlinekit.git
Resolving https://github.com/objecthub/swift-commandlinekit.git at 0.3.1
[6/6] Linking LispKitRepl
```

The release binary can be run by invoking `.build/release/LispKitRepl -r Sources/LispKit/Resources`
in the directory `swift-lispkit` (in which the binary was build above).

Assuming that directory `~/Documents/LispKit` contains a copy of the
[resources directory](https://github.com/objecthub/swift-lispkit/tree/master/Sources/LispKit/Resources), it is also
possible to run the release binary by invoking `.build/release/LispKitRepl -d LispKit`.


## Requirements

The following technologies are needed to build the components of the LispKit framework. For building the
command-line tool, all that is needed is the Swift Package Manager. For compiling the framework and trying
the command-line tool directly in Xcode, the Swift Package Manager is not needed.

- [Swift 5.3](https://developer.apple.com/swift/)
- [Xcode 12.0](https://developer.apple.com/xcode/)
- [Swift Package Manager](https://swift.org/package-manager/)
- [NumberKit](http://github.com/objecthub/swift-numberkit)
- [MarkdownKit](http://github.com/objecthub/swift-markdownkit)
- [CommandLineKit](http://github.com/objecthub/swift-commandlinekit)
- [SQLiteExpress](http://github.com/objecthub/swift-sqliteexpress)
