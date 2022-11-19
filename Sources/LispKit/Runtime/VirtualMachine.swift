//
//  VirtualMachine.swift
//  LispKit
//
//  Created by Matthias Zenger on 13/02/2016.
//  Copyright © 2016-2022 ObjectHub. All rights reserved.
//
//  Licensed under the Apache License, Version 2.0 (the "License");
//  you may not use this file except in compliance with the License.
//  You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS,
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  See the License for the specific language governing permissions and
//  limitations under the License.
//

import Foundation

///
/// Class `VirtualMachine` implements a virtual stack machine for executing LispKit `Code`
/// objects. The virtual machine consists of the following components:
///    - *context*: This is a reference to the context in which this virtual machine is embedded.
///    - *stack*: This is the stack for passing parameters, storing intermediate results,
///      and for returning results.
///    - *sp*: The stack pointer, i.e. the index of the next available position on the stack.
///
/// The stack is segmented into frames, each representing the invocation of a procedure. The
/// layout of each stack frame looks like this:
///
///    │        ...        │
///    ╞═══════════════════╡
///    │        ...        │
///    │ Stack value 1     │
///    │ Stack value 0     │
///    │ Local variable 1  │
///    │ Local variable 0  │  ⟸ fp + args
///    │ Argument 1        │
///    │ Argument 0        │  ⟸ fp
///    │ Procedure         │  ⟸ fp - 1
///    ├───────────────────┤                 ⥥ makeFrame ⥥
///    │ Return address    │  ⟸ fp - 2
///    │ Dynamic link      │  ⟸ fp - 3
///    ╞═══════════════════╡
///    │        ...        │
///
public final class VirtualMachine: ManagedObject {
  
  internal final class Winder: Reference {
    let before: Procedure
    let after: Procedure
    let handlers: Expr?
    let next: Winder?
    
    init(before: Procedure, after: Procedure, handlers: Expr?, next: Winder?) {
      self.before = before
      self.after = after
      self.handlers = handlers
      self.next = next
    }
    
    var id: Int64 {
      return Int64(bitPattern: UInt64(self.identity))
    }
    
    var count: Int {
      var ws: Winder? = self
      var n = 0
      while let next = ws {
        n += 1
        ws = next.next
      }
      return n
    }
    
    func commonPrefix(_ with: Winder?) -> Winder? {
      guard with != nil else {
        return nil
      }
      var this: Winder? = self
      var that: Winder? = with!
      let thisLen = self.count
      let thatLen = with!.count
      if thisLen > thatLen {
        for _ in thatLen..<thisLen {
          this = this!.next
        }
      } else if thatLen > thisLen {
        for _ in thisLen..<thatLen {
          that = that!.next
        }
      }
      while let thisWinder = this, let thatWinder = that , thisWinder !== thatWinder {
        this = thisWinder.next
        that = thatWinder.next
      }
      return this
    }

    public func mark(in gc: GarbageCollector) {
      gc.mark(self.before)
      gc.mark(self.after)
      if let handlers = self.handlers {
        gc.markLater(handlers)
      }
      self.next?.mark(in: gc)
    }
  }
  
  /// The context of this virtual machine
  public unowned let context: Context
  
  /// The stack used by this virtual machine
  private var stack: Exprs
  
  /// The stack pointer (pointing at the next available position on the stack); this variable
  /// should be private, but since it's needed in tests, it remains internal.
  internal var sp: Int {
    didSet {
      if sp > self.maxSp {
        self.maxSp = self.sp
      }
    }
  }
  
  /// The stack pointer never grows beyond `limitSp`
  public private(set) var limitSp: Int
  
  /// The maximum value of the stack pointer so far (used for debugging)
  public private(set) var maxSp: Int = 0
  
  /// Registers
  private var registers: Registers
  
  /// Winders
  internal private(set) var winders: Winder?
  
  /// Parameters
  internal var parameters: HashTable
  
  /// Internal counter used for triggering the garbage collector.
  private var execInstr: UInt64
  
  /// Set to true while `onTopLevelDo` is executing
  internal private(set) var executing: Bool = false
  
  /// When set to true, it will trigger an abortion of the machine evaluator as soon as possible.
  internal private(set) var abortionRequested: Bool = false
  
  /// Initializes a new virtual machine for the given context.
  public init(for context: Context, limitStack: Int = 10000000) {
    self.context = context
    self.stack = Exprs(repeating: .undef, count: 1024)
    self.sp = 0
    self.limitSp = limitStack <= Int.max - 100 ? limitStack : Int.max - 100
    self.registers = Registers(code: Code([], [], []), captured: [], fp: 0, root: true)
    self.winders = nil
    self.parameters = HashTable(equiv: .eq)
    self.execInstr = 0
    super.init()
    context.objects.manage(self)
  }
  
  /// Initializes a new virtual machine for the given context.
  public init(parent machine: VirtualMachine) {
    self.context = machine.context
    self.stack = Exprs(repeating: .undef, count: 1024)
    self.sp = 0
    self.limitSp = machine.limitSp
    self.registers = Registers(code: Code([], [], []), captured: [], fp: 0, root: true)
    self.winders = nil
    self.parameters = HashTable(copy: machine.parameters)
    self.execInstr = 0
  }
  
  /// Sets the stack size limit to `limitSp`
  public func setStackLimit(to limitSp: Int) -> Bool {
    if limitSp > self.sp && limitSp <= Int.max - 100 {
      self.limitSp = limitSp
      return true
    } else {
      return false
    }
  }
  
  /// Returns a copy of the current virtual machine state.
  public func getState() -> VirtualMachineState {
    return VirtualMachineState(stack: self.stack,
                               sp: self.sp,
                               spDelta: -2,
                               ipDelta: -1,
                               registers: self.registers,
                               winders: self.winders)
  }
  
  /// Requests abortion of the machine evaluator.
  public func requestAbortion() {
    if self.executing {
      self.abortionRequested = true
    }
  }
  
  /// Checks that computation happens on top level and fails if the conditions are not met.
  private func assertTopLevel() {
    guard self.sp == 0 && !self.abortionRequested && !self.executing else {
      preconditionFailure("preconditions for top-level evaluation not met")
    }
  }
  
   /// Executes an evaluation function at the top-level.
  func onTopLevelDo(_ eval: () throws -> Expr) -> Expr {
    // Prepare for the evaluation
    self.assertTopLevel()
    self.executing = true
    // Reset machine once evaluation finished
    defer {
      for i in 0..<self.sp {
        self.stack[i] = .undef
      }
      self.sp = 0
      self.winders = nil
      self.abortionRequested = false
      self.executing = false
    }
    // Perform evaluation
    var exception: RuntimeError? = nil
    do {
      return try eval()
    } catch let error as RuntimeError { // handle Lisp-related issues
      exception = error
    } catch let error { // handle OS-related issues
      exception = RuntimeError.os(error)
    }
    // Abortions ignore dynamic environments
    guard !self.abortionRequested else {
      return .error(RuntimeError.uncaught(.error(exception!)))
    }
    // Return thrown exception if there is no `raise` procedure. In such a case, there is no
    // unwind of the dynamic environment.
    guard let raiseProc = self.context.evaluator.raiseProc else {
      return .error(RuntimeError.uncaught(.error(exception!)))
    }
    // Raise thrown exceptions
    while let obj = exception {
      do {
        return try self.apply(.procedure(raiseProc), to: .pair(.error(obj), .null))
      } catch let error as RuntimeError { // handle Lisp-related issues
        guard !self.abortionRequested else { // abortions bypass the raise mechanism
          return .error(RuntimeError.uncaught(.error(error)))
        }
        exception = error
      } catch let error { // handle OS-related issues
        exception = RuntimeError.os(error)
      }
    }
    // Never happens
    return .void
  }
  
  /// Loads the file at file patch `path`, compiles it in the interaction environment, and
  /// executes it using this virtual machine.
  public func eval(file path: String, foldCase: Bool = false) throws -> Expr {
    // Load file and parse expressions
    let exprs = try self.context.evaluator.parse(file: path, foldCase: foldCase)
    let sourceDir = self.context.fileHandler.directory(path)
    // Hand over work to `compileAndEvalFirst`
    return try self.apply(.procedure(self.context.evaluator.loader!),
                          to: .makeList(exprs,
                                        .makeString(sourceDir),
                                        .env(self.context.environment)))
  }
  
  /// Loads the file at file patch `path`, compiles it in the interaction environment, and
  /// executes it using this virtual machine.
  public func eval(file path: String,
                   in env: Env,
                   as name: String? = nil,
                   optimize: Bool = true,
                   foldCase: Bool = false) throws -> Expr {
    let (sourceId, text) = try self.context.sources.readSource(for: path)
    return try self.eval(str: text,
                         sourceId: sourceId,
                         in: env,
                         as: name,
                         optimize: optimize,
                         inDirectory: self.context.fileHandler.directory(path),
                         foldCase: foldCase)
  }
  
  /// Loads the file at file patch `path`, compiles it in the interaction environment, and
  /// executes it using this virtual machine.
  public func eval(str: String,
                   sourceId: UInt16,
                   inDirectory: String? = nil,
                   foldCase: Bool = false) throws -> Expr {
    // Parse expressions
    let exprs = try self.context.evaluator.parse(str: str, sourceId: sourceId, foldCase: foldCase)
    // Hand over work to loader
    return try self.apply(.procedure(self.context.evaluator.loader!),
                          to: .makeList(exprs,
                                        inDirectory == nil ? .false : .makeString(inDirectory!),
                                        .env(self.context.environment)))
  }
  
  /// Parses the given string, compiles it in the interaction environment, and executes it using
  /// this virtual machine.
  public func eval(str: String,
                   sourceId: UInt16,
                   in env: Env,
                   as name: String? = nil,
                   optimize: Bool = true,
                   inDirectory: String? = nil,
                   foldCase: Bool = false) throws -> Expr {
    return try self.eval(exprs: self.context.evaluator.parse(str: str,
                                                             sourceId: sourceId,
                                                             foldCase: foldCase),
                         in: env,
                         as: name,
                         optimize: optimize,
                         inDirectory: inDirectory)
  }
  
  /// Compiles the given list of expressions in the interaction environment and executes
  /// it using this virtual machine.
  public func eval(exprs: Expr,
                   in env: Env,
                   as name: String? = nil,
                   optimize: Bool = true,
                   inDirectory: String? = nil) throws -> Expr {
    var exprlist = exprs
    var res = Expr.void
    while case .pair(let expr, let rest) = exprlist {
      let code = try Compiler.compile(expr: .makeList(expr),
                                      in: env,
                                      optimize: optimize,
                                      inDirectory: inDirectory)
      // log(code.description)
      if let name = name {
        res = try self.execute(code, as: name)
      } else {
        res = try self.execute(code)
      }
      exprlist = rest
    }
    guard exprlist.isNull else {
      throw RuntimeError.type(exprs, expected: [.properListType])
    }
    return res
  }
  
  /// Compiles the given expression in the interaction environment and executes it using this
  /// virtual machine.
  public func eval(expr: Expr,
                   in env: Env,
                   as name: String? = nil,
                   optimize: Bool = true,
                   inDirectory: String? = nil) throws -> Expr {
    return try self.eval(exprs: .makeList(expr),
                         in: env,
                         as: name,
                         optimize: optimize,
                         inDirectory: inDirectory)
  }
  
  /// Compiles the given expression `expr` in the environment `env` and executes it using
  /// this virtual machine.
  public func compileAndEval(expr: Expr,
                             in env: Env,
                             usingRulesEnv renv: Env? = nil,
                             optimize: Bool = true,
                             inDirectory: String? = nil) throws -> Expr {
    let code = try Compiler.compile(expr: .makeList(expr),
                                    in: env,
                                    and: renv,
                                    optimize: optimize,
                                    inDirectory: inDirectory)
    return try self.apply(.procedure(Procedure(code)), to: .null)
  }
  
  /// Applies `args` to the function `fun` in environment `env`.
  public func apply(_ fun: Expr, to args: Expr) throws -> Expr {
    try self.push(fun)
    var n = try self.pushArguments(args)
    let proc = try self.invoke(&n, 1)
    switch proc.kind {
      case .closure(_, _, let captured, let code):
        return try self.execute(code, args: n, captured: captured)
      case .rawContinuation(_):
        return try self.execute()
      case .transformer(let rules):
        if n != 1 {
          throw RuntimeError.argumentCount(min: 1, max: 1, args: args)
        }
        let res = try rules.expand(self.pop())
        self.drop()
        return res
      default:
        return self.pop()
    }
  }
  
  /// Pushes the given expression onto the stack.
  @inline(__always) private func push(_ expr: Expr) throws {
    if self.sp < self.stack.count {
      self.stack[self.sp] = expr
    } else {
      if self.sp >= self.stack.capacity && self.sp < (Int.max / 2) {
        self.stack.reserveCapacity(self.sp * 2)
      }
      self.stack.append(expr)
      if self.sp >= self.limitSp {
        // We need to bypass exception handling for stack overflows
        self.abortionRequested = true
        throw RuntimeError.eval(.stackOverflow)
      }
    }
    self.sp &+= 1
  }
  
  /// Pushes the given list of arguments onto the stack and returns the number of arguments pushed
  /// onto the stack.
  @inline(__always) @discardableResult private func pushArguments(_ arglist: Expr) throws -> Int {
    var args = arglist
    var n = 0
    while case .pair(let arg, let rest) = args {
      try self.push(arg)
      n = n &+ 1
      args = rest
    }
    guard args.isNull else {
      throw RuntimeError.eval(.malformedArgumentList, arglist)
    }
    return n
  }
  
  /// Removes the top `n` elements from the stack.
  @inline(__always) private func pop(_ n: Int) {
    var i = self.sp
    self.sp = self.sp &- n
    while i > self.sp {
      i = i &- 1
      self.stack[self.sp] = .undef
    }
  }
  
  /// Removes the top element from the stack without resetting it and returns its value.
  @inline(__always) private func popUnsafe() -> Expr {
    self.sp = self.sp &- 1
    return self.stack[self.sp]
  }
  
  /// Returns the top element of the stack.
  @inline(__always) private func top() -> Expr {
    return self.stack[self.sp &- 1]
  }
  
  /// Removes the top element from the stack and returns it.
  @inline(__always) private func pop() -> Expr {
    self.sp = self.sp &- 1
    let res = self.stack[self.sp]
    self.stack[self.sp] = .undef
    return res
  }
  
  /// Removes the top element from the stack
  @inline(__always) private func drop() {
    self.sp = self.sp &- 1
    self.stack[self.sp] = .undef
  }
  
  @inline(__always) private func popAsList(_ n: Int) -> Expr {
    var res = Expr.null
    var i = n
    while i > 0 {
      res = .pair(self.pop(), res)
      i = i &- 1
    }
    return res
  }
  
  @inline(__always) private func captureExprs(_ n: Int) -> Exprs {
    var captures = Exprs()
    var i = self.sp &- n
    while i < self.sp {
      captures.append(self.stack[i])
      self.stack[i] = .undef
      i = i &+ 1
    }
    self.sp = self.sp &- n
    return captures
  }
  
  internal func windUp(before: Procedure, after: Procedure, handlers: Expr? = nil) {
    self.winders = Winder(before: before, after: after, handlers: handlers , next: self.winders)
  }
  
  internal func windDown() -> Winder? {
    guard let res = self.winders else {
      return nil
    }
    self.winders = res.next
    return res
  }
  
  internal func currentHandlers() -> Expr? {
    var winders = self.winders
    while let w = winders, w.handlers == nil {
      winders = w.next
    }
    return winders?.handlers
  }
  
  private func exitFrame() {
    let fp = self.registers.fp
    // Determine former ip
    guard case .fixnum(let newip) = self.stack[fp &- 2] else {
      preconditionFailure()
    }
    self.registers.ip = Int(newip)
    // Determine former fp
    guard case .fixnum(let newfp) = self.stack[fp &- 3] else {
      preconditionFailure()
    }
    // Shift result down
    self.stack[fp &- 3] = self.stack[self.sp &- 1]
    // Clean up stack that is freed up
    for i in (fp &- 2)..<self.sp {
      self.stack[i] = .undef
    }
    // Set new fp and sp
    self.sp = fp &- 2
    self.registers.fp = Int(newfp)
    // Determine closure to which execution returns to
    guard case .procedure(let proc) = self.stack[Int(newfp) - 1] else {
      preconditionFailure()
    }
    // Extract code and capture list
    guard case .closure(_, _, let newcaptured, let newcode) = proc.kind else {
      preconditionFailure()
    }
    // Set new code and capture list
    self.registers.captured = newcaptured
    self.registers.code = newcode
  }
  
  public func getStackTrace(current: Procedure? = nil) -> [Procedure] {
    var stackTrace: [Procedure] = []
    if let current = current {
      stackTrace.append(current)
    }
    var fp = self.registers.fp
    while fp > 0 {
      guard case .procedure(let proc) = self.stack[fp &- 1] else {
        // This may happen if an error is thrown
        return stackTrace
      }
      stackTrace.append(proc)
      if fp > 2 {
        guard case .fixnum(let newfp) = self.stack[fp &- 3] else {
          // This may happen if an error is thrown
          return stackTrace
        }
        fp = Int(newfp)
      } else {
        fp = 0
      }
    }
    return stackTrace
  }
  
  public func getCallTrace(current: Procedure? = nil, cap: Int? = nil) -> [Expr]? {
    var cap = cap ?? self.context.evaluator.maxCallStack
    guard cap > 0 else {
      return nil
    }
    var stackTrace: [Expr] = []
    if let current = current {
      cap -= 1
      stackTrace.append(.makeList(.symbol(self.context.symbols.intern(current.name)),
                                  .symbol(self.context.symbols.dotdotdot)))
    }
    var fp = self.registers.fp
    while fp > 0 && cap > 0 {
      guard case .procedure(let proc) = self.stack[fp &- 1] else {
        // This may happen if an error is thrown
        return stackTrace
      }
      let arities = proc.arity
      var min = Int.max
      var max: Int? = 0
      for arity in arities {
        switch arity {
          case .exact(let n):
            if n < min {
              min = n
            }
            if let m = max, n > m {
              max = n
            }
          case .atLeast(let n):
            if n < min {
              min = n
            }
            max = nil
        }
      }
      var call = Expr.null
      if max == nil || max! > min {
        call = .pair(.symbol(self.context.symbols.dotdotdot), call)
      }
      while min > 0 {
        min -= 1
        let offset = fp &+ min
        if offset >= 0 && offset < self.sp {
          call = .pair(self.stack[fp &+ min], call)
        }
      }
      cap -= 1
      stackTrace.append(.pair(.symbol(self.context.symbols.intern(proc.name)), call))
      if fp > 2 {
        guard case .fixnum(let newfp) = self.stack[fp &- 3] else {
          // This may happen if an error is thrown
          return stackTrace
        }
        fp = Int(newfp)
      } else {
        fp = 0
      }
    }
    return stackTrace
  }
  
  public func getCallTraceInfo(current: Procedure? = nil, cap: Int? = nil) -> [String]? {
    return self.getCallTrace(current: current, cap: cap)?.map { expr in expr.description }
  }
  
  @inline(__always) private func printCallTrace(_ n: Int, tailCall: Bool = false) -> Procedure? {
    if self.context.evaluator.traceCalls != .off && self.sp > (n &+ 1) {
      if case .procedure(let proc) = self.stack[self.sp &- n &- 1],
         self.context.evaluator.traceCalls == .on || proc.traced {
        var args = Exprs()
        for i in 0..<n {
          args.append(self.stack[self.sp &- n &+ i])
        }
        self.context.delegate?.trace(call: proc,
                                     args: args,
                                     tailCall: tailCall,
                                     in: self)
        return proc
      }
    }
    return nil
  }
  
  @inline(__always) private func printReturnTrace(_ proc: Procedure, tailCall: Bool = false) {
    if (self.context.evaluator.traceCalls == .on ||
        (self.context.evaluator.traceCalls == .byProc && proc.traced)) && self.sp > 0 {
      self.context.delegate?.trace(return: proc,
                                   result: self.stack[self.sp &- 1],
                                   tailCall: tailCall,
                                   in: self)
    }
  }
  
  private func invoke(_ n: inout Int, _ overhead: Int) throws -> Procedure {
    // Get procedure to call
    guard case .procedure(let p) = self.stack[self.sp &- n &- 1] else {
      throw RuntimeError.eval(.nonApplicativeValue, self.stack[self.sp &- n &- 1])
    }
    var proc = p
    // Handle parameter procedures
    if case .parameter(let tuple) = proc.kind {
      switch n {
        case 0:                             // Return parameter value
          self.pop(overhead)
          try self.push(self.context.evaluator.getParam(proc)!)
          return proc
        case 1 where tuple.fst.isNull:      // Set parameter value without setter
          let a0 = self.pop()
          self.pop(overhead)
          try self.push(self.context.evaluator.setParam(proc, to: a0))
          return proc
        case 1:                             // Set parameter value with setter
          let a0 = self.pop()
          self.drop()
          try self.push(tuple.fst)
          try self.push(.procedure(proc))
          try self.push(a0)
          try self.push(.procedure(self.context.evaluator.setParameterProc))
          n = 3
          proc = try tuple.fst.asProcedure()
        default:
          throw RuntimeError.argumentCount(min: 1, max: 1, args: self.popAsList(n))
      }
    }
    // Handle primitive procedures; this is required to loop because of applicators
    do {
      loop: while case .primitive(_, let impl, _) = proc.kind {
        switch impl {
          case .eval(let eval):
            let generated = Procedure(try eval(self.stack[(self.sp &- n)..<self.sp]))
            self.pop(n + 1)
            try self.push(.procedure(generated))
            n = 0
            return generated
          case .apply(let apply):
            let (next, args) = try apply(self.stack[(self.sp &- n)..<self.sp])
            self.pop(n + 1)
            try self.push(.procedure(next))
            for arg in args {
              try self.push(arg)
            }
            n = args.count
            proc = next
          case .native0(let exec):
            guard n == 0 else {
              throw RuntimeError.argumentCount(min: 0, max: 0, args: self.popAsList(n))
            }
            self.pop(overhead)
            try self.push(try exec())
            return proc
          case .native1(let exec):
            guard n == 1 else {
              throw RuntimeError.argumentCount(min: 1, max: 1, args: self.popAsList(n))
            }
            let a0 = self.pop()
            self.pop(overhead)
            try self.push(try exec(a0))
            return proc
          case .native2(let exec):
            guard n == 2 else {
              throw RuntimeError.argumentCount(min: 2, max: 2, args: self.popAsList(n))
            }
            let a1 = self.pop()
            let a0 = self.pop()
            self.pop(overhead)
            try self.push(try exec(a0, a1))
            return proc
          case .native3(let exec):
            guard n == 3 else {
              throw RuntimeError.argumentCount(min: 3, max: 3, args: self.popAsList(n))
            }
            let a2 = self.pop()
            let a1 = self.pop()
            let a0 = self.pop()
            self.pop(overhead)
            try self.push(try exec(a0, a1, a2))
            return proc
          case .native4(let exec):
            guard n == 4 else {
              throw RuntimeError.argumentCount(min: 4, max: 4, args: self.popAsList(n))
            }
            let a3 = self.pop()
            let a2 = self.pop()
            let a1 = self.pop()
            let a0 = self.pop()
            self.pop(overhead)
            try self.push(try exec(a0, a1, a2, a3))
            return proc
          case .native0O(let exec):
            if n == 0 {
              self.pop(overhead)
              try self.push(try exec(nil))
            } else if n == 1 {
              let a0 = self.pop()
              self.pop(overhead)
              try self.push(try exec(a0))
            } else {
              throw RuntimeError.argumentCount(min: 1, max: 1, args: self.popAsList(n))
            }
            return proc
          case .native1O(let exec):
            if n == 1 {
              let a0 = self.pop()
              self.pop(overhead)
              try self.push(try exec(a0, nil))
            } else if n == 2 {
              let a1 = self.pop()
              let a0 = self.pop()
              self.pop(overhead)
              try self.push(try exec(a0, a1))
            } else {
              throw RuntimeError.argumentCount(min: 2, max: 2, args: self.popAsList(n))
            }
            return proc
          case .native2O(let exec):
            if n == 2 {
              let a1 = self.pop()
              let a0 = self.pop()
              self.pop(overhead)
              try self.push(try exec(a0, a1, nil))
            } else if n == 3 {
              let a2 = self.pop()
              let a1 = self.pop()
              let a0 = self.pop()
              self.pop(overhead)
              try self.push(try exec(a0, a1, a2))
            } else {
              throw RuntimeError.argumentCount(min: 3, max: 3, args: self.popAsList(n))
            }
            return proc
          case .native3O(let exec):
            if n == 3 {
              let a2 = self.pop()
              let a1 = self.pop()
              let a0 = self.pop()
              self.pop(overhead)
              try self.push(try exec(a0, a1, a2, nil))
            } else if n == 4 {
              let a3 = self.pop()
              let a2 = self.pop()
              let a1 = self.pop()
              let a0 = self.pop()
              self.pop(overhead)
              try self.push(try exec(a0, a1, a2, a3))
            } else {
              throw RuntimeError.argumentCount(min: 3, max: 3, args: self.popAsList(n))
            }
            return proc
          case .native0OO(let exec):
            if n == 0 {
              self.pop(overhead)
              try self.push(try exec(nil, nil))
            } else if n == 1 {
              let a0 = self.pop()
              self.pop(overhead)
              try self.push(try exec(a0, nil))
            } else if n == 2 {
              let a1 = self.pop()
              let a0 = self.pop()
              self.pop(overhead)
              try self.push(try exec(a0, a1))
            } else {
              throw RuntimeError.argumentCount(min: 2, max: 2, args: self.popAsList(n))
            }
            return proc
          case .native1OO(let exec):
            if n == 1 {
              let a0 = self.pop()
              self.pop(overhead)
              try self.push(try exec(a0, nil, nil))
            } else if n == 2 {
              let a1 = self.pop()
              let a0 = self.pop()
              self.pop(overhead)
              try self.push(try exec(a0, a1, nil))
            } else if n == 3 {
              let a2 = self.pop()
              let a1 = self.pop()
              let a0 = self.pop()
              self.pop(overhead)
              try self.push(try exec(a0, a1, a2))
            } else {
              throw RuntimeError.argumentCount(min: 3, max: 3, args: self.popAsList(n))
            }
            return proc
          case .native2OO(let exec):
            if n == 2 {
              let a1 = self.pop()
              let a0 = self.pop()
              self.pop(overhead)
              try self.push(try exec(a0, a1, nil, nil))
            } else if n == 3 {
              let a2 = self.pop()
              let a1 = self.pop()
              let a0 = self.pop()
              self.pop(overhead)
              try self.push(try exec(a0, a1, a2, nil))
            } else if n == 4 {
              let a3 = self.pop()
              let a2 = self.pop()
              let a1 = self.pop()
              let a0 = self.pop()
              self.pop(overhead)
              try self.push(try exec(a0, a1, a2, a3))
            } else {
              throw RuntimeError.argumentCount(min: 4, max: 4, args: self.popAsList(n))
            }
            return proc
          case .native3OO(let exec):
            if n == 3 {
              let a2 = self.pop()
              let a1 = self.pop()
              let a0 = self.pop()
              self.pop(overhead)
              try self.push(try exec(a0, a1, a2, nil, nil))
            } else if n == 4 {
              let a3 = self.pop()
              let a2 = self.pop()
              let a1 = self.pop()
              let a0 = self.pop()
              self.pop(overhead)
              try self.push(try exec(a0, a1, a2, a3, nil))
            } else if n == 5 {
              let a4 = self.pop()
              let a3 = self.pop()
              let a2 = self.pop()
              let a1 = self.pop()
              let a0 = self.pop()
              self.pop(overhead)
              try self.push(try exec(a0, a1, a2, a3, a4))
            } else {
              throw RuntimeError.argumentCount(min: 5, max: 5, args: self.popAsList(n))
            }
            return proc
          case .native0R(let exec):
            let res = try exec(self.stack[(self.sp &- n)..<self.sp])
            self.pop(n &+ overhead)
            try self.push(res)
            return proc
          case .native1R(let exec):
            if n >= 1 {
              let res = try exec(self.stack[self.sp &- n], self.stack[(self.sp &- n &+ 1)..<self.sp])
              self.pop(n &+ overhead)
              try self.push(res)
            } else {
              throw RuntimeError.argumentCount(min: 1, args: self.popAsList(n))
            }
            return proc
          case .native2R(let exec):
            if n >= 2 {
              let res = try exec(self.stack[self.sp &- n],
                                 self.stack[self.sp &- n &+ 1],
                                 self.stack[(self.sp &- n &+ 2)..<self.sp])
              self.pop(n &+ overhead)
              try self.push(res)
            } else {
              throw RuntimeError.argumentCount(min: 2, args: self.popAsList(n))
            }
            return proc
          case .native3R(let exec):
            if n >= 3 {
              let res = try exec(self.stack[self.sp &- n],
                                 self.stack[self.sp &- n &+ 1],
                                 self.stack[self.sp &- n &+ 2],
                                 self.stack[(self.sp &- n &+ 3)..<self.sp])
              self.pop(n &+ overhead)
              try self.push(res)
            } else {
              throw RuntimeError.argumentCount(min: 3, args: self.popAsList(n))
            }
            return proc
        }
      }
    } catch let error as RuntimeError {
      if error.stackTrace == nil {
        error.attach(vm: self, current: proc)
      }
      throw error
    } catch let error {
      throw RuntimeError.os(error).attach(vm: self, current: proc)
    }
    // Handle continuations
    if case .rawContinuation(let vmState) = proc.kind {
      // Check that we apply the continuation in the right context
      guard vmState.registers.rid == self.registers.rid else {
        throw RuntimeError.eval(.illegalContinuationApplication,
                                .procedure(proc),
                                .makeNumber(self.registers.rid))
      }
      // Continuations accept exactly one argument
      guard n == 1 else {
        throw RuntimeError.argumentCount(min: 1, max: 1, args: self.popAsList(n))
      }
      // Retrieve argument
      let arg = self.pop()
      // Restore virtual machine from state encapsulated in continuation
      self.stack = vmState.stack
      self.sp = vmState.sp
      self.registers = vmState.registers
      // Push identity function and argument onto restored virtual machine stack
      try self.push(.procedure(CoreLibrary.idProc))
      try self.push(arg)
    }
    return proc
  }
  
  @inline(__always) private func collectGarbageIfNeeded() {
    self.execInstr = self.execInstr &+ 1
    if self.execInstr % 0b011111111111111111111 == 0 {
      // let res =
      _ = self.context.objects.collectGarbage()
      // log("[collect garbage; freed up objects: \(res)]")
    }
  }
  
  @inline(__always) private func execute(_ code: Code, as name: String) throws -> Expr {
    try self.push(.procedure(Procedure(name, code)))
    return try self.execute(code, args: 0, captured: noExprs)
  }
  
  @inline(__always) private func execute(_ code: Code) throws -> Expr {
    try self.push(.procedure(Procedure(code)))
    return try self.execute(code, args: 0, captured: noExprs)
  }
  
  @inline(__always) private func execute(_ code: Code, args: Int, captured: Exprs) throws -> Expr {
    // Use new registers
    let savedRegisters = self.registers
    self.registers = Registers(code: code,
                               captured: captured,
                               fp: self.sp &- args,
                               root: !savedRegisters.isInitialized)
    // Restore old registers when leaving `execute`
    defer {
      self.registers = savedRegisters
    }
    do {
      let res = try self.execute()
      guard !self.abortionRequested else {
        throw RuntimeError.abortion(stackTrace: self.getStackTrace(),
                                    callTrace: self.getCallTraceInfo())
      }
      return res
    } catch let error as RuntimeError {
      if !self.abortionRequested && error.stackTrace == nil {
        error.attach(vm: self)
      }
      throw error
    } catch let error {
      throw RuntimeError.os(error).attach(vm: self)
    }
  }
  
  private func execute() throws -> Expr {
    while self.registers.ip >= 0 && self.registers.ip < self.registers.code.instructions.count {
      guard !self.abortionRequested else {
        throw RuntimeError.abortion(stackTrace: self.getStackTrace(),
                                    callTrace: self.getCallTraceInfo())
      }
      self.collectGarbageIfNeeded()
      /*
      print("╔══════════════════════════════════════════════════════")
      if self.registers.ip > 0 {
        print("║     \(self.registers.ip - 1)  \(self.registers.code.instructions[self.registers.ip - 1])")
      }
      print("║    [\(self.registers.ip)] \(self.registers.code.instructions[self.registers.ip])")
      if self.registers.ip < self.registers.code.instructions.count - 1 {
        print("║     \(self.registers.ip + 1)  \(self.registers.code.instructions[self.registers.ip + 1])")
      }
      print(stackFragmentDescr(self.registers.ip, self.registers.fp, header: "╟──────────────────────────────────────────────────────\n"))
      */
      let ip = self.registers.ip
      self.registers.ip = ip &+ 1
      switch self.registers.code.instructions[ip] {
        case .noOp:
          break
        case .pop:
          self.sp = self.sp &- 1
          self.stack[self.sp] = .undef
        case .dup:
          try self.push(self.stack[self.sp &- 1])
        case .swap:
          let top = self.stack[self.sp &- 1]
          self.stack[self.sp &- 1] = self.stack[self.sp &- 2]
          self.stack[self.sp &- 2] = top
        case .pushGlobal(let index):
          do {
            self.context.objects.lock.lock()
            defer {
              self.context.objects.lock.unlock()
            }
            let value = self.context.heap.locations[index]
            switch value {
              case .undef:
                throw RuntimeError.eval(.variableUndefined, .undef)
              case .uninit(let sym):
                throw RuntimeError.eval(.variableNotYetInitialized, .symbol(sym))
              case .special(_):
                throw RuntimeError.eval(.illegalKeywordUsage, value)
              default:
                try self.push(value)
            }
          }
        case .setGlobal(let index):
          do {
            self.context.objects.lock.lock()
            defer {
              self.context.objects.lock.unlock()
            }
            let value = self.context.heap.locations[index]
            switch value {
              case .undef:
                throw RuntimeError.eval(.variableNotYetInitialized, .undef)
              case .uninit(let sym):
                throw RuntimeError.eval(.unboundVariable, .symbol(sym))
              default:
                self.context.heap.locations[index] = self.pop()
            }
          }
        case .defineGlobal(let index):
          self.context.objects.lock.lock()
          self.context.heap.locations[index] = self.pop()
          self.context.objects.lock.unlock()
        case .pushCaptured(let index):
          try self.push(self.registers.captured[index])
        case .pushCapturedValue(let index):
          guard case .box(let cell) = self.registers.captured[index] else {
            preconditionFailure("pushCapturedValue cannot push \(self.registers.captured[index])")
          }
          if case .undef = cell.value {
            throw RuntimeError.eval(.variableNotYetInitialized, .undef)
          }
          try self.push(cell.value)
        case .setCapturedValue(let index):
          guard case .box(let cell) = self.registers.captured[index] else {
            preconditionFailure("setCapturedValue cannot set value of \(self.registers.captured[index])")
          }
          cell.value = self.pop()
          if !cell.managed && !cell.value.isAtom {
            self.context.objects.manage(cell)
          }
        case .pushLocal(let index):
          try self.push(self.stack[self.registers.fp &+ index])
        case .setLocal(let index):
          self.stack[self.registers.fp &+ index] = self.pop()
        case .setLocalValue(let index):
          guard case .box(let cell) = self.stack[self.registers.fp &+ index] else {
            preconditionFailure(
              "setLocalValue cannot set value of \(self.stack[self.registers.fp &+ index])")
          }
          cell.value = self.pop()
          if !cell.managed && !cell.value.isAtom {
            self.context.objects.manage(cell)
          }
        case .makeLocalVariable(let index):
          let cell = Cell(self.pop())
          // TODO: I can't think of a reason to manage such cells since they only live on the
          //       stack and are never stored anywhere else.
          // self.context.objects.manage(cell)
          self.stack[self.registers.fp &+ index] = .box(cell)
        case .makeVariableArgument(let index):
          let cell = Cell(self.stack[self.registers.fp &+ index])
          // TODO: I can't think of a reason to manage such cells since they only live on the
          //       stack and are never stored anywhere else.
          // self.context.objects.manage(cell)
          self.stack[self.registers.fp &+ index] = .box(cell)
        case .pushLocalValue(let index):
          guard case .box(let cell) = self.stack[self.registers.fp &+ index] else {
            preconditionFailure(
              "pushLocalValue cannot push \(self.stack[self.registers.fp &+ index])")
          }
          if case .undef = cell.value {
            throw RuntimeError.eval(.variableNotYetInitialized, .undef)
          }
          try self.push(cell.value)
        case .pushConstant(let index):
          try self.push(self.registers.code.constants[index])
        case .pushProcedure(let index):
          try self.push(self.registers.code.constants[index])
        case .pushUndef:
          try self.push(.undef)
        case .pushVoid:
          try self.push(.void)
        case .pushEof:
          try self.push(.void)
        case .pushNull:
          try self.push(.null)
        case .pushTrue:
          try self.push(.true)
        case .pushFalse:
          try self.push(.false)
        case .pushFixnum(let num):
          try self.push(.fixnum(num))
        case .pushBignum(let num):
          try self.push(.bignum(num))
        case .pushRat(let num):
          try self.push(.rational(.fixnum(num.numerator), .fixnum(num.denominator)))
        case .pushBigrat(let num):
          try self.push(.rational(.bignum(num.numerator), .bignum(num.denominator)))
        case .pushFlonum(let num):
          try self.push(.flonum(num))
        case .pushComplex(let num):
          try self.push(.complex(ImmutableBox(num)))
        case .pushChar(let char):
          try self.push(.char(char))
        case .pack(let n):
          var list = Expr.null
          for _ in 0..<n {
            list = .pair(self.pop(), list)
          }
          try self.push(.values(list))
        case .flatpack(let n):
          var list = Expr.null
          for _ in 0..<n {
            let e = self.pop()
            switch e {
              case .values(.pair(let x, .pair(let y, .null))):
                list = .pair(x, .pair(y, list))
              case .values(.pair(let x, .pair(let y, .pair(let z, .null)))):
                list = .pair(x, .pair(y, .pair(z, list)))
              case .values(let xs):
                list = list.isNull ? xs : .makeList(xs.toExprs().0, append: list)
              default:
                list = .pair(e, list)
            }
          }
          try self.push(.values(list))
        case .unpack(let n, let overflow):
          switch self.top() {
            case .void:
              guard n == 0 else {
                throw RuntimeError.eval(.multiValueCountError, .makeNumber(n), .null)
              }
              self.drop()
              if overflow {
                try self.push(.null)
              }
            case .values(let list):
              self.drop()
              var m = n
              var next = list
              while case .pair(let value, let rest) = next {
                if m <= 0 {
                  if overflow {
                    break
                  } else {
                    throw RuntimeError.eval(.multiValueCountError, .makeNumber(n), list)
                  }
                }
                try self.push(value)
                m -= 1
                next = rest
              }
              guard m == 0 else {
                throw RuntimeError.eval(.multiValueCountError, .makeNumber(n), list)
              }
              if overflow {
                try self.push(next)
              }
            default:
              if n == 1 {
                if overflow {
                  try self.push(.null)
                }
              } else if n == 0 && overflow {
                try self.push(.pair(self.popUnsafe(), .null))
              } else {
                throw RuntimeError.eval(.multiValueCountError,
                                        .makeNumber(n),
                                        .pair(self.top(), .null))
              }
          }
        case .makeClosure(let i, let n, let index):
          let type: Procedure.ClosureType
          if i >= 0 {
            guard case .symbol(let sym) = self.registers.code.constants[i] else {
              preconditionFailure(
                "makeClosure has broken closure name \(self.registers.code.constants[i])")
            }
            type = .named(sym.description)
          } else {
            type = i == -2 ? .continuation : .anonymous
          }
          try self.push(.procedure(Procedure(type,
                                         self.captureExprs(n),
                                         self.registers.code.fragments[index])))
        case .makeTaggedClosure(let i, let n, let index):
          let type: Procedure.ClosureType
          if i >= 0 {
            guard case .symbol(let sym) = self.registers.code.constants[i] else {
              preconditionFailure(
                "makeClosure has broken closure name \(self.registers.code.constants[i])")
            }
            type = .named(sym.description)
          } else {
            type = i == -2 ? .continuation : .anonymous
          }
          let captured = self.captureExprs(n)
          try self.push(.procedure(Procedure(type,
                                         self.pop(),
                                         captured,
                                         self.registers.code.fragments[index])))
        case .makePromise:
          let top = self.pop()
          guard case .procedure(let proc) = top else {
            preconditionFailure("makePromise cannot create promise from \(top)")
          }
          let future = Promise(kind: .promise, thunk: proc)
          future.managementRef = self.context.objects.reference(to: future)
          try self.push(.promise(future))
        case .makeStream:
          let top = self.pop()
          guard case .procedure(let proc) = top else {
            preconditionFailure("makeStream cannot create stream from \(top)")
          }
          let future = Promise(kind: .stream, thunk: proc)
          future.managementRef = self.context.objects.reference(to: future)
          try self.push(.promise(future))
        case .makeSyntax(let i):
          let transformer = self.pop()
          guard case .procedure(let proc) = transformer else {
            throw RuntimeError.eval(.malformedTransformer, transformer)
          }
          if i >= 0 {
            guard case .symbol(let sym) = self.registers.code.constants[i] else {
              preconditionFailure(
                "makeSyntax has broken syntax name \(self.registers.code.constants[i])")
            }
            try self.push(.special(SpecialForm(sym.identifier, proc)))
          } else {
            try self.push(.special(SpecialForm(nil, proc)))
          }
        case .compile:
          let environment = try self.pop().asEnvironment()
          let code = try Compiler.compile(expr: .makeList(self.pop()),
                                          in: .global(environment),
                                          optimize: true)
          try self.push(.procedure(Procedure(code)))
        case .apply(let m):
          let arglist = self.pop()
          var args = arglist
          var n = m &- 1
          while case .pair(let arg, let rest) = args {
            try self.push(arg)
            n = n &+ 1
            args = rest
          }
          guard args.isNull else {
            throw RuntimeError.eval(.malformedArgumentList, arglist)
          }
          // Store instruction pointer
          self.stack[self.sp &- n &- 2] = .fixnum(Int64(self.registers.ip))
          // Invoke native function
          if case .closure(_, _, let newcaptured, let newcode) = try self.invoke(&n, 3).kind {
            self.registers.use(code: newcode, captured: newcaptured, fp: self.sp &- n)
          }
        case .makeFrame:
          // Push frame pointer
          try self.push(.fixnum(Int64(self.registers.fp)))
          // Reserve space for instruction pointer
          try self.push(.undef)
        case .injectFrame:
          let top = self.pop()
          // Push frame pointer
          try self.push(.fixnum(Int64(self.registers.fp)))
          // Reserve space for instruction pointer
          try self.push(.undef)
          // Push top value onto stack again
          try self.push(top)
        case .call(let n):
          let tproc = self.printCallTrace(n, tailCall: false)
          // Store instruction pointer
          self.stack[self.sp &- n &- 2] = .fixnum(Int64(self.registers.ip))
          // Invoke native function
          var m = n
          if case .closure(_, _, let newcaptured, let newcode) = try self.invoke(&m, 3).kind {
            self.registers.use(code: newcode, captured: newcaptured, fp: self.sp &- m)
          } else if let tproc = tproc {
            self.printReturnTrace(tproc, tailCall: false)
          }
        case .tailCall(let m):
          _ = self.printCallTrace(m, tailCall: true)
          // Invoke native function
          var n = m
          let proc = try self.invoke(&n, 1)
          if case .closure(_, _, let newcaptured, let newcode) = proc.kind {
            // Execute closure
            self.registers.use(code: newcode, captured: newcaptured, fp: self.registers.fp)
            // Shift stack frame down to next stack frame
            for i in 0...n {
              self.stack[self.registers.fp &- 1 &+ i] = self.stack[self.sp &- n &- 1 &+ i]
            }
            // Wipe the now empty part of the stack
            for i in (self.registers.fp &+ n)..<self.sp {
              self.stack[i] = .undef
            }
            // Adjust the stack pointer
            self.sp = self.registers.fp &+ n
          } else if case .rawContinuation(_) = proc.kind {
            break
          } else if self.registers.topLevel {
            if self.registers.fp > 0,
               case .procedure(let tproc) = self.stack[self.registers.fp &- 1] {
              self.printReturnTrace(tproc, tailCall: true)
            }
            // Return to interactive environment
            let res = self.pop()
            // Wipe the stack
            for i in (self.registers.initialFp &- 1)..<self.sp {
              self.stack[i] = .undef
            }
            self.sp = self.registers.initialFp &- 1
            return res
          } else {
            if case .procedure(let tproc) = self.stack[self.registers.fp &- 1] {
              self.printReturnTrace(tproc, tailCall: true)
            }
            self.exitFrame()
          }
        case .assertArgCount(let n):
          guard self.sp &- n == self.registers.fp else {
            throw RuntimeError.argumentCount(min: n,
                                             max: n,
                                             args: self.popAsList(self.sp &- self.registers.fp))
          }
        case .assertMinArgCount(let n):
          guard self.sp &- n >= self.registers.fp else {
            throw RuntimeError.argumentCount(min: n,
                                             args: self.popAsList(self.sp &- self.registers.fp))
          }
        case .noMatchingArgCount:
          throw RuntimeError.eval(.noMatchingCase,
                                  self.popAsList(self.sp &- self.registers.fp),
                                  self.pop())
        case .collectRest(let n):
          var rest = Expr.null
          while self.sp > self.registers.fp &+ n {
            rest = .pair(self.pop(), rest)
          }
          try self.push(rest)
        case .alloc(let n):
          if self.sp &+ n > self.stack.count {
            if self.sp &+ n > self.limitSp {
              // We need to bypass exception handling for stack overflows
              self.abortionRequested = true
              throw RuntimeError.eval(.stackOverflow)
            }
            if self.sp &+ n >= self.stack.capacity && self.sp < (Int.max / 2) {
              self.stack.reserveCapacity(self.sp * 2)
            }
            for _ in 0..<(self.sp &+ n &- self.stack.count) {
              self.stack.append(.undef)
            }
          }
          self.sp &+= n
        case .allocBelow(let n):
          let top = self.pop()
          if self.sp &+ n > self.stack.count {
            if self.sp &+ n > self.limitSp {
              // We need to bypass exception handling for stack overflows
              self.abortionRequested = true
              throw RuntimeError.eval(.stackOverflow)
            }
            if self.sp &+ n >= self.stack.capacity && self.sp < (Int.max / 2) {
              self.stack.reserveCapacity(self.sp * 2)
            }
            for _ in 0..<(self.sp &+ n &- self.stack.count) {
              self.stack.append(.undef)
            }
          }
          self.sp &+= n
          try self.push(top)
        case .reset(let index, let n):
          for i in (self.registers.fp &+ index)..<(self.registers.fp &+ index &+ n) {
            self.stack[i] = .undef
          }
        case .return:
          // Return to interactive environment
          if self.registers.topLevel {
            if self.registers.fp > 0,
               case .procedure(let tproc) = self.stack[self.registers.fp &- 1] {
              self.printReturnTrace(tproc, tailCall: true)
            }
            let res = self.pop()
            // Wipe the stack
            for i in (self.registers.initialFp &- 1)..<self.sp {
              self.stack[i] = .undef
            }
            // Reset stack and frame pointer
            self.sp = self.registers.initialFp &- 1
            return res
          } else {
            if case .procedure(let tproc) = self.stack[self.registers.fp &- 1] {
              self.printReturnTrace(tproc, tailCall: true)
            }
            self.exitFrame()
          }
        case .branch(let offset):
          self.registers.ip = self.registers.ip &+ offset &- 1
        case .branchIf(let offset):
          if self.pop().isTrue {
            self.registers.ip = self.registers.ip &+ offset &- 1
          }
        case .branchIfNot(let offset):
          if self.pop().isFalse {
            self.registers.ip = self.registers.ip &+ offset &- 1
          }
        case .keepOrBranchIfNot(let offset):
          let top = self.pop()
          if top.isFalse {
            self.registers.ip = self.registers.ip &+ offset &- 1
          } else {
            try self.push(top)
          }
        case .branchIfArgMismatch(let n, let offset):
          if self.sp &- n != self.registers.fp {
            self.registers.ip = self.registers.ip &+ offset &- 1
          }
        case .branchIfMinArgMismatch(let n, let offset):
          if self.sp &- n < self.registers.fp {
            self.registers.ip = self.registers.ip &+ offset &- 1
          }
        case .and(let offset):
          if self.stack[self.sp &- 1].isFalse {
            self.registers.ip = self.registers.ip &+ offset &- 1
          } else {
            self.drop()
          }
        case .or(let offset):
          if self.stack[self.sp &- 1].isTrue {
            self.registers.ip = self.registers.ip &+ offset &- 1
          } else {
            self.drop()
          }
        case .force:
          if case .promise(let future) = self.stack[self.sp &- 1] {
            switch future.state {
              case .lazy(let proc):
                // Push frame pointer
                try self.push(.fixnum(Int64(self.registers.fp)))
                // Push instruction pointer
                try self.push(.fixnum(Int64(self.registers.ip)))
                // Push procedure that yields the forced result
                try self.push(.procedure(proc))
                // Invoke native function
                var n = 0
                if case .closure(_, _, let newcaptured, let newcode) = try self.invoke(&n, 3).kind {
                  self.registers.use(code: newcode, captured: newcaptured, fp: self.sp)
                }
              case .shared(let future):
                // Replace the promise with the shared promise on the stack
                self.stack[self.sp &- 1] = .promise(future)
                // Execute force with the new promise on the stack
                self.registers.ip = self.registers.ip &- 1
              case .value(let value):
                // Replace the promise with the value on the stack
                self.stack[self.sp &- 1] = value
                // Jump over StoreInPromise operation
                self.registers.ip = self.registers.ip &+ 1
            }
          }
        case .storeInPromise:
          guard case .promise(let future) = self.stack[self.sp &- 2] else {
            preconditionFailure()
          }
          switch future.state {
            case .lazy(_), .shared(_):
              guard case .promise(let result) = self.stack[self.sp &- 1],
                    future.kind == result.kind else {
                let type: Type = future.kind == Promise.Kind.promise ? .promiseType : .streamType
                throw RuntimeError.type(self.stack[self.sp &- 1], expected: [type])
              }
              future.state = result.state
              result.state = .shared(future)
              if let ref = result.managementRef {
                self.context.objects.unmanage(ref)
                result.managementRef = nil
              }
              if let ref = future.managementRef, case .value(_) = future.state {
                self.context.objects.unmanage(ref)
                future.managementRef = nil
              }
            default:
              break
          }
          // Pop result of execution of thunk from stack
          self.sp = self.sp &- 1
          self.stack[self.sp] = .undef
          // Re-execute force
          self.registers.ip = self.registers.ip &- 2
        case .makeThread(let start):
          let th = self.context.evaluator.thread(for: try self.popUnsafe().asProcedure(),
                                                 name: .false,
                                                 tag: .false)
          try self.push(.object(th))
          if start {
            try th.value.start()
          }
        case .failIfNotNull:
          let top = self.pop()
          guard top.isNull else {
            throw RuntimeError(SourcePosition.unknown,
                               ErrorDescriptor.eval(.listTooLong),
                               [top])
          }
        case .raiseError(let err, let n):
          var irritants: [Expr] = []
          for _ in 0..<n {
            irritants.insert(self.pop(), at: 0)
          }
          throw RuntimeError(SourcePosition.unknown,
                             ErrorDescriptor.eval(EvalError(rawValue: err)!),
                             irritants)
        case .pushCurrentTime:
          try self.push(.flonum(Timer.absoluteTimeInSec))
        case .display:
          let obj = self.pop()
          switch obj {
            case .string(let str):
              self.context.delegate?.print(str as String)
            default:
              self.context.delegate?.print(obj.description)
          }
        case .newline:
          self.context.delegate?.print("\n")
        case .eq:
          try self.push(.makeBoolean(eqExpr(self.pop(), self.popUnsafe())))
        case .eqv:
          try self.push(.makeBoolean(eqvExpr(self.pop(), self.popUnsafe())))
        case .equal:
          try self.push(.makeBoolean(equalExpr(self.pop(), self.popUnsafe())))
        case .isPair:
          if case .pair(_, _) = self.popUnsafe() {
            try self.push(.true)
          } else {
            try self.push(.false)
          }
        case .isNull:
          try self.push(.makeBoolean(self.popUnsafe() == .null))
        case .isUndef:
          try self.push(.makeBoolean(self.popUnsafe() == .undef))
        case .cons:
          let cdr = self.pop()
          try self.push(.pair(self.popUnsafe(), cdr))
        case .decons:
          let expr = self.popUnsafe()
          guard case .pair(let car, let cdr) = expr else {
            throw RuntimeError.type(expr, expected: [.pairType])
          }
          try self.push(cdr)
          try self.push(car)
        case .deconsKeyword:
          let expr = self.popUnsafe()
          guard case .pair(let fst, .pair(let snd, let cdr)) = expr else {
            throw RuntimeError.eval(.expectedKeywordArg, expr)
          }
          try self.push(cdr)
          try self.push(snd)
          try self.push(fst)
        case .car:
          let expr = self.popUnsafe()
          guard case .pair(let car, _) = expr else {
            throw RuntimeError.type(expr, expected: [.pairType])
          }
          try self.push(car)
        case .cdr:
          let expr = self.popUnsafe()
          guard case .pair(_, let cdr) = expr else {
            throw RuntimeError.type(expr, expected: [.pairType])
          }
          try self.push(cdr)
        case .list(let n):
          var res = Expr.null
          for _ in 0..<n {
            res = .pair(self.pop(), res)
          }
          try self.push(res)
        case .vector(let n):
          let vector = Collection(kind: .vector)
          var i = self.sp &- n
          while i < self.sp {
            vector.exprs.append(self.stack[i])
            i = i &+ 1
          }
          self.pop(n)
          try self.push(.vector(vector))
        case .listToVector:
          let expr = self.popUnsafe()
          let vector = Collection(kind: .vector)
          var list = expr
          while case .pair(let car, let cdr) = list {
            vector.exprs.append(car)
            list = cdr
          }
          guard list.isNull else {
            throw RuntimeError.type(expr, expected: [.properListType])
          }
          try self.push(.vector(vector))
        case .vectorAppend(let n):
          let vector = Collection(kind: .vector)
          var i = self.sp &- n
          while i < self.sp {
            vector.exprs.append(contentsOf: try self.stack[i].vectorAsCollection().exprs)
            i = i &+ 1
          }
          self.pop(n)
          try self.push(.vector(vector))
        case .isVector:
          if case .vector(let vector) = self.popUnsafe(), !vector.isGrowableVector {
            try self.push(.true)
          } else {
            try self.push(.false)
          }
        case .not:
          if case .false = self.popUnsafe() {
            try self.push(.true)
          } else {
            try self.push(.false)
          }
        case .fxPlus:
          let rhs = self.pop()
          try self.push(.fixnum(try self.popUnsafe().asInt64() &+ rhs.asInt64()))
        case .fxMinus:
          let rhs = self.pop()
          try self.push(.fixnum(try self.popUnsafe().asInt64() &- rhs.asInt64()))
        case .fxMult:
          let rhs = self.pop()
          try self.push(.fixnum(try self.popUnsafe().asInt64() &* rhs.asInt64()))
        case .fxDiv:
          let rhs = try self.pop().asInt64()
          guard rhs != 0 else {
            throw RuntimeError.eval(.divisionByZero)
          }
          try self.push(.fixnum(try self.popUnsafe().asInt64() / rhs))
        case .fxInc:
          let idx = self.sp &- 1
          switch self.stack[idx] {
            case .fixnum(let x):
              self.stack[idx] = .fixnum(x &+ 1)
            default:
              throw RuntimeError.type(self.stack[idx], expected: [.fixnumType])
          }
        case .fxDec:
          let idx = self.sp &- 1
          switch self.stack[idx] {
            case .fixnum(let x):
              self.stack[idx] = .fixnum(x &- 1)
            default:
              throw RuntimeError.type(self.stack[idx], expected: [.fixnumType])
          }
        case .fxIsZero:
          let idx = self.sp &- 1
          switch self.stack[idx] {
            case .fixnum(let x):
              self.stack[idx] = x == 0 ? .true : .false
            default:
              throw RuntimeError.type(self.stack[idx], expected: [.fixnumType])
          }
        case .fxEq(let n):
          var rhs = try self.pop().asInt64()
          var res = true
          for _ in 2..<n {
            let lhs = try self.pop().asInt64()
            res = res && (lhs == rhs)
            rhs = lhs
          }
          try self.push(.makeBoolean(try (self.popUnsafe().asInt64() == rhs) && res))
        case .fxLt(let n):
          var rhs = try self.pop().asInt64()
          var res = true
          for _ in 2..<n {
            let lhs = try self.pop().asInt64()
            res = res && (lhs < rhs)
            rhs = lhs
          }
          try self.push(.makeBoolean(try (self.popUnsafe().asInt64() < rhs) && res))
        case .fxGt(let n):
          var rhs = try self.pop().asInt64()
          var res = true
          for _ in 2..<n {
            let lhs = try self.pop().asInt64()
            res = res && (lhs > rhs)
            rhs = lhs
          }
          try self.push(.makeBoolean(try (self.popUnsafe().asInt64() > rhs) && res))
        case .fxLtEq(let n):
          var rhs = try self.pop().asInt64()
          var res = true
          for _ in 2..<n {
            let lhs = try self.pop().asInt64()
            res = res && (lhs <= rhs)
            rhs = lhs
          }
          try self.push(.makeBoolean(try (self.popUnsafe().asInt64() <= rhs) && res))
        case .fxGtEq(let n):
          var rhs = try self.pop().asInt64()
          var res = true
          for _ in 2..<n {
            let lhs = try self.pop().asInt64()
            res = res && (lhs >= rhs)
            rhs = lhs
          }
          try self.push(.makeBoolean(try (self.popUnsafe().asInt64() >= rhs) && res))
        case .fxAssert:
          guard case .fixnum(_) = self.stack[self.sp &- 1] else {
            throw RuntimeError.type(self.stack[self.sp &- 1], expected: [.fixnumType])
          }
        case .flPlus:
          let rhs = self.pop()
          try self.push(.flonum(try self.popUnsafe().asDouble() + rhs.asDouble()))
        case .flMinus:
          let rhs = self.pop()
          try self.push(.flonum(try self.popUnsafe().asDouble() - rhs.asDouble()))
        case .flMult:
          let rhs = self.pop()
          try self.push(.flonum(try self.popUnsafe().asDouble() * rhs.asDouble()))
        case .flDiv:
          let rhs = self.pop()
          try self.push(.flonum(try self.popUnsafe().asDouble() / rhs.asDouble()))
        case .flNeg:
          let idx = self.sp &- 1
          switch self.stack[idx] {
            case .flonum(let x):
              self.stack[idx] = .flonum(-x)
            default:
              throw RuntimeError.type(self.stack[idx], expected: [.floatType])
          }
        case .flEq(let n):
          var rhs = try self.pop().asDouble()
          var res = true
          for _ in 2..<n {
            let lhs = try self.pop().asDouble()
            res = res && (lhs == rhs)
            rhs = lhs
          }
          try self.push(.makeBoolean(try (self.popUnsafe().asDouble() == rhs) && res))
        case .flLt(let n):
          var rhs = try self.pop().asDouble()
          var res = true
          for _ in 2..<n {
            let lhs = try self.pop().asDouble()
            res = res && (lhs < rhs)
            rhs = lhs
          }
          try self.push(.makeBoolean(try (self.popUnsafe().asDouble() < rhs) && res))
        case .flGt(let n):
          var rhs = try self.pop().asDouble()
          var res = true
          for _ in 2..<n {
            let lhs = try self.pop().asDouble()
            res = res && (lhs > rhs)
            rhs = lhs
          }
          try self.push(.makeBoolean(try (self.popUnsafe().asDouble() > rhs) && res))
        case .flLtEq(let n):
          var rhs = try self.pop().asDouble()
          var res = true
          for _ in 2..<n {
            let lhs = try self.pop().asDouble()
            res = res && (lhs <= rhs)
            rhs = lhs
          }
          try self.push(.makeBoolean(try (self.popUnsafe().asDouble() <= rhs) && res))
        case .flGtEq(let n):
          var rhs = try self.pop().asDouble()
          var res = true
          for _ in 2..<n {
            let lhs = try self.pop().asDouble()
            res = res && (lhs >= rhs)
            rhs = lhs
          }
          try self.push(.makeBoolean(try (self.popUnsafe().asDouble() >= rhs) && res))
        case .flAssert:
          guard case .flonum(_) = self.stack[self.sp &- 1] else {
            throw RuntimeError.type(self.stack[self.sp &- 1], expected: [.floatType])
          }
      }
    }
    return .null
  }
  
  /// Debugging output
  private func stackFragmentDescr(_ ip: Int, _ fp: Int, header: String? = nil) -> String {
    var res = header ?? "╔══════════════════════════════════════════════════════\n"
    res += "║ ip = \(ip), fp = \(fp), sp = \(self.sp), max_sp = \(self.maxSp)\n"
    res += "╟──────────────────────────────────────────────────────\n"
    let start = fp > 2 ? fp - 3 : 0
    for i in start..<self.sp {
      if i == fp {
        res += "║  ➤ [\(i)] \(self.stack[i])\n"
      } else {
        res += "║    [\(i)] \(self.stack[i])\n"
      }
    }
    res += "╚══════════════════════════════════════════════════════\n"
    return res
  }
  
  /// Mark virual machine
  public func mark(in gc: GarbageCollector) {
    if self.tag != gc.tag {
      self.tag = gc.tag
      for i in 0..<self.sp {
        gc.markLater(self.stack[i])
      }
      self.registers.mark(in: gc)
      self.winders?.mark(in: gc)
      gc.mark(self.parameters)
    }
  }
  
  /// Reset virtual machine
  public override func clean() {
    self.stack = Exprs(repeating: .undef, count: 1024)
    self.sp = 0
    self.maxSp = 0
    self.registers = Registers(code: Code([], [], []), captured: [], fp: 0, root: true)
    self.winders = nil
    self.parameters = HashTable(equiv: .eq)
    self.execInstr = 0
    self.abortionRequested = false
  }
}
