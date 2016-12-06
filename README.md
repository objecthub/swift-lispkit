<img src="Assets/lispkit_logo_small.png" alt="LispKit" width="80" height="80" align="middle" />&nbsp;Swift LispKit
======================================================

<p>
<a href="https://developer.apple.com/osx/"><img src="https://img.shields.io/badge/Platform-macOS-blue.svg?style=flat" alt="Platform: macOS" /></a>
<a href="https://developer.apple.com/swift/"><img src="https://img.shields.io/badge/Language-Swift%203.0-green.svg?style=flat" alt="Language: Swift 3.0" /></a>
<a href="https://developer.apple.com/xcode/"><img src="https://img.shields.io/badge/IDE-Xcode%208.1-orange.svg?style=flat" alt="IDE: Xcode 8.1" /></a>
<a href="https://raw.githubusercontent.com/objecthub/swift-lispkit/master/LICENSE"><img src="http://img.shields.io/badge/License-Apache-lightgrey.svg?style=flat" alt="License: Apache" /></a>
</p>

## Overview

_LispKit_ is a _Mac OS X_ framework for building Lisp-based extension and scripting languages
for Mac applications. _LispKit_ is fully implemented in the programming language
[Swift](http://www.swift.org). _LispKit_ implements a core language based on the
[R5RS Scheme standard](http://www.schemers.org/Documents/Standards/R5RS/HTML/). It supports
almost the full numerical tower consisting of arbitrary size integers, rationals, real numbers,
and complex numbers. _LispKit_ implements hygienic macros in terms of the _syntax-rules_ standard.
While _LispKit_ supports almost all standard functions and special forms of R5RS, it has one
major limitation: lists are immutable.

From an architectural perspective, _LispKit_ consists of:

  1. a compiler translating _LispKit_ expressions into bytecode, and
  2. a virtual machine for interpreting the generated byteocde. The virtual machine is
     stack-based, handles tail calls and continuations, and provides a garbage collector.

Details can be found in the [LispKit Wiki](https://github.com/objecthub/swift-lispkit/wiki).

## Current state

In July 2016, _LispKit_ reached a major milestone in providing full support for `call/cc`.
The recently implemented libraries supporting _ports_, _promises_, and _bytevectors_ are
based on the [R7RS (small)](http://www.r7rs.org) standard from 2013. Currently, support for
libraries based on the R7RS specification is being added. Over time, it's my goal to
base the implementation primarily on R7RS and fall back to R6RS only when there is no
corresponding specification provided by R7RS (e.g. for hash tables).

In November 2016, _LispKit_ reached an important milestone for supporting R7RS libraries.
Environments got reimplemented from scratch and made first-class values. Libraries were
introduced and all built-in functions were modularly reimplemented in terms of the new
library abstraction.

_LispKit_ relies on [NumberKit](http://github.com/objecthub/swift-numberkit)
for its support of numeric datatypes like rationals, complex numbers, and big integers.
The _LispKit_ compiler does not perform any code optimizations and the performance of the
system is significantly below state of the art Scheme implementations.

The read-eval-print loop is a command-line tool that can be used to try out the framework.
It parses the entered _LispKit_ expression, compiles it to bytecode, executes it, and
displays the result.

## Requirements

- XCode 8.1
- Swift 3.0.1
- [NumberKit](http://github.com/objecthub/swift-numberkit)

