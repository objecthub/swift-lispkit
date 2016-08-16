//
//  VirtualMachine.swift
//  LispKit
//
//  Created by Matthias Zenger on 13/02/2016.
//  Copyright © 2016 ObjectHub. All rights reserved.
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

import Darwin

///
/// Class `VirtualMachine` implements a virtual stack machine for executing LispKit `Code`
/// objects. The virtual machine consists of the following components:
///    - *context*: This is a reference to the context in which this virtual machine is embedded.
///    - *stack*: This is the stack for passing parameters, storing intermediate results,
///      and for returning results.
///    - *sp*: The stack pointer, i.e. the index of the next available position on the stack.
///
/// The stack is segmented into frames, each representing the invocation of a closure. The
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
///    │ Closure           │  ⟸ fp - 1
///    ├───────────────────┤                 ⥥ MakeFrame ⥥
///    │ Return address    │  ⟸ fp - 2
///    │ Dynamic link      │  ⟸ fp - 3
///    ╞═══════════════════╡
///    │        ...        │
///
public final class VirtualMachine: TrackedObject {
  
  /// Collects all registers in a single struct
  internal struct Registers {
    let rid: Int
    var code: Code
    var captured: [Expr]
    var ip: Int
    var fp: Int
    let initialFp: Int
    
    init(code: Code, captured: [Expr], fp: Int, root: Bool) {
      if root {
        self.rid = 0
      } else {
        VirtualMachine.nextRid += 1
        self.rid = VirtualMachine.nextRid
      }
      self.code = code
      self.captured = captured
      self.ip = 0
      self.fp = fp
      self.initialFp = fp
    }
    
    mutating func use(code code: Code, captured: [Expr], fp: Int) {
      self.code = code
      self.captured = captured
      self.ip = 0
      self.fp = fp
    }
    
    var topLevel: Bool {
      return self.fp == self.initialFp
    }
    
    var isInitialized: Bool {
      return self.rid == 0 && self.code.instructions.count > 0
    }
    
    func mark(tag: UInt8) {
      self.code.mark(tag)
      for expr in self.captured {
        expr.mark(tag)
      }
    }
  }
  
  internal class Winder: Reference {
    let before: Procedure
    let after: Procedure
    let next: Winder?
    
    init(before: Procedure, after: Procedure, next: Winder?) {
      self.before = before
      self.after = after
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
    
    func commonPrefix(with: Winder?) -> Winder? {
      guard with != nil else {
        return self
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
      while let thisWinder = this, thatWinder = that where thisWinder !== thatWinder {
        this = thisWinder.next
        that = thatWinder.next
      }
      return this
    }
    
    func mark(tag: UInt8) {
      self.before.mark(tag)
      self.after.mark(tag)
      self.next?.mark(tag)
    }
  }
  
  /// Counter for managing register ids
  private static var nextRid: Int = 0
  
  /// The context of this virtual machine
  private let context: Context
  
  /// The stack used by this virtual machine
  private var stack: [Expr]
  
  /// The stack pointer (pointing at the next available position on the stack); this variable
  /// should be private, but since it's needed in tests, it remains internal.
  internal var sp: Int {
    didSet {
      if sp > self.maxSp {
        self.maxSp = self.sp
      }
    }
  }
  
  /// The maximum value of the stack pointer so far (used for debugging)
  private var maxSp: Int
  
  /// Registers
  private var registers: Registers
  
  /// Winders
  internal private(set) var winders: Winder?
  
  /// Parameters
  internal var parameters: HashMap
  
  private var setParameterProc: Procedure!
  
  /// Internal counter used for triggering the garbage collector
  private var execInstr: UInt64
  
  /// Constant representing an empty capture set
  private static let NO_CAPTURES = [Expr]()
  
  /// Initializes a new virtual machine for the given context
  public init(_ context: Context) {
    self.context = context
    self.stack = [Expr](count: 1024, repeatedValue: .Undef)
    self.sp = 0
    self.maxSp = 0
    self.registers = Registers(code: Code([], [], []), captured: [], fp: 0, root: true)
    self.winders = nil
    self.parameters = HashMap(equiv: .Eq)
    self.execInstr = 0
    super.init()
    self.setParameterProc = Procedure("_set-parameter", self.setParameter, nil)
  }
  
  /// Returns a copy of the current virtual machine state
  public func getState() -> VirtualMachineState {
    return VirtualMachineState(stack: self.stack,
                               sp: self.sp,
                               spDelta: -2,
                               ipDelta: -1,
                               registers: self.registers,
                               winders: self.winders)
  }
  
  /// Loads the file at file patch `path`, compiles it in the interaction environment, and
  /// executes it using this virtual machine.
  public func evalFile(path: String, in env: Env = .Interaction, optimize: Bool = true) -> Expr {
    do {
      return self.evalStr(try String(contentsOfFile: path, encoding: NSUTF8StringEncoding),
                          in: env,
                          optimize: optimize)
    } catch let error as NSError {
      return .Error(AnyError(OsError(error)))
    }
  }
  
  /// Parses the given string, compiles it in the interaction environment, and executes it using
  /// this virtual machine.
  public func evalStr(str: String, in env: Env = .Interaction, optimize: Bool = true) -> Expr {
    do {
      let parser = Parser(symbols: self.context.symbols, src: str)
      var exprs = Exprs()
      while !parser.finished {
        exprs.append(try parser.parse())
      }
      return self.evalExprs(.List(exprs), in: env, optimize: optimize)
    } catch let error as LispError { // handle Lisp-related issues
      return .Error(AnyError(error))
    } catch { // handle internal issues
      // TODO: Figure out what needs to be logged here
      print("UNKNOWN ERROR in Evaluator.evalStr")
      return .Undef
    }
  }
  
  /// Compiles the given expression in the interaction environment and executes it using this
  /// virtual machine.
  public func evalExpr(expr: Expr, in env: Env = .Interaction, optimize: Bool = true) -> Expr {
    return self.evalExprs(.List(expr), in: env, optimize: optimize)
  }
  
  /// Compiles the given list of expressions in the interaction environment and executes
  /// it using this virtual machine.
  public func evalExprs(exprs: Expr, in env: Env = .Interaction, optimize: Bool = true) -> Expr {
    do {
      var exprlist = exprs
      var res = Expr.Void
      while case .Pair(let expr, let rest) = exprlist {
        let code =
          try Compiler.compile(self.context, expr: .List(expr), in: env, optimize: optimize)
        log(code.description)
        res = try self.execute(code)
        exprlist = rest
      }
      guard exprlist.isNull else {
        throw EvalError.TypeError(exprs, [.ProperListType])
      }
      return res
    } catch let error as LispError { // handle Lisp-related issues
      return .Error(AnyError(error))
    } catch let error as NSError { // handle OS-related issues
      return .Error(AnyError(OsError(error)))
    } catch { // handle internal issues
      // TODO: Figure out what needs to be logged here
      print("UNKNOWN ERROR in Evaluator.evalExprs")
      return .Undef
    }
  }
  
  /// Compiles the given expression `expr` in the environment `env` and executes it using
  /// this virtual machine.
  public func eval(expr: Expr,
                   in env: Env = .Interaction,
                   usingRulesEnv renv: Env? = nil) throws -> Expr {
    let code =
      try Compiler.compile(self.context, expr: .List(expr), in: env, and: renv, optimize: true)
    return try self.apply(.Proc(Procedure(code)), to: .Null)
  }
  
  /// Applies `args` to the function `fun` in environment `env`.
  public func apply(fun: Expr, to args: Expr, in env: Env = .Interaction) throws -> Expr {
    self.push(fun)
    var n = try self.pushArguments(args)
    let proc = try self.invoke(&n, 1)
    switch proc.kind {
      case .Closure(_, let captured, let code):
        return try self.execute(code, args: n, captured: captured)
      case .Continuation(_):
        return try self.execute()
      case .Transformer(let rules):
        if n != 1 {
          throw EvalError.ArgumentCountError(formals: 1, args: args)
        }
        let res = try rules.expand(self.pop())
        self.pop()
        return res
      default:
        return self.pop()
    }
  }
  
  /// Pushes the given expression onto the stack.
  @inline(__always) private func push(expr: Expr) {
    self.stack[self.sp] = expr
    self.sp += 1
  }
  
  /// Pushes the given list of arguments onto the stack and returns the number of arguments pushed
  /// onto the stack.
  @inline(__always) private func pushArguments(arglist: Expr) throws -> Int {
    var args = arglist
    var n = 0
    while case .Pair(let arg, let rest) = args {
      self.stack[self.sp] = arg
      self.sp += 1
      n += 1
      args = rest
    }
    guard args.isNull else {
      throw EvalError.MalformedArgumentList(arglist)
    }
    return n
  }
  
  /// Removes the top `n` elements from the stack.
  @inline(__always) private func pop(n: Int) {
    var i = n
    while i > 0 {
      self.sp -= 1
      self.stack[self.sp] = .Undef
      i -= 1
    }
  }
  
  /// Removes the top element from the stack and returns it.
  @inline(__always) private func pop() -> Expr {
    self.sp -= 1
    let res = self.stack[self.sp]
    self.stack[self.sp] = .Undef
    return res
  }
  
  @inline(__always) private func popAsList(n: Int) -> Expr {
    var res = Expr.Null
    var i = n
    while i > 0 {
      res = .Pair(self.pop(), res)
      i -= 1
    }
    return res
  }
  
  @inline(__always) private func popAsArray(n: Int) -> Exprs {
    var res = Exprs()
    var i = n
    while i > 0 {
      res.append(self.stack[self.sp - i])
      self.stack[self.sp - i] = .Undef
      i -= 1
    }
    return res
  }
  
  private func captureExprs(n: Int) -> [Expr] {
    var captures = [Expr]()
    var i = n
    while i > 0 {
      /* guard case .Var(let variable) = self.stack[self.sp - i] else {
       preconditionFailure("pushed as capture: \(self.stack[self.sp - i])")
       } */
      captures.append(self.stack[self.sp - i])
      self.stack[self.sp - i] = .Undef
      i -= 1
    }
    self.sp -= n
    return captures
  }
  
  internal func windUp(before before: Procedure, after: Procedure) {
    self.winders = Winder(before: before, after: after, next: self.winders)
  }
  
  internal func windDown() -> Winder? {
    guard let res = self.winders else {
      return nil
    }
    self.winders = res.next
    return res
  }
  
  internal func getParam(param: Procedure) -> Expr? {
    return self.getParameter(.Proc(param))
  }
  
  internal func getParameter(param: Expr) -> Expr? {
    guard case .Some(.Pair(_, .Box(let cell))) = self.parameters.get(param) else {
      guard case .Proc(let proc) = param, .Parameter(let tuple) = proc.kind else {
        return nil
      }
      return tuple.snd
    }
    return cell.value
  }
  
  internal func setParam(param: Procedure, to value: Expr) -> Expr {
    return self.setParameter(.Proc(param), to: value)
  }
  
  internal func setParameter(param: Expr, to value: Expr) -> Expr {
    guard case .Some(.Pair(_, .Box(let cell))) = self.parameters.get(param) else {
      guard case .Proc(let proc) = param, .Parameter(let tuple) = proc.kind else {
        preconditionFailure("cannot set parameter \(param)")
      }
      tuple.snd = value
      return .Void
    }
    cell.value = value
    return .Void
  }
  
  internal func bindParameter(param: Expr, to value: Expr) -> Expr {
    self.parameters.add(param, .Box(Cell(value)))
    return .Void
  }
  
  internal func bindParameters(alist: Expr) {
    self.parameters = HashMap(copy: self.parameters, mutable: true)
    var current = alist
    while case .Pair(.Pair(let param, let value), let next) = current {
      self.parameters.add(param, .Box(Cell(value)))
      current = next
    }
  }
  
  private func exitFrame() {
    // Determine former ip
    guard case .Fixnum(let newip) = self.stack[self.registers.fp - 2] else {
      preconditionFailure()
    }
    self.registers.ip = Int(newip)
    // Determine former fp
    guard case .Fixnum(let newfp) = self.stack[self.registers.fp - 3] else {
      preconditionFailure()
    }
    // Shift result down
    self.stack[self.registers.fp - 3] = self.stack[self.sp - 1]
    // Clean up stack that is freed up
    for i in self.registers.fp-2..<self.sp {
      self.stack[i] = .Undef
    }
    // Set new fp and sp
    self.sp = self.registers.fp - 2
    self.registers.fp = Int(newfp)
    // Determine closure to which execution returns to
    guard case .Proc(let proc) = self.stack[newfp - 1] else {
      preconditionFailure()
    }
    // Extract code and capture list
    guard case .Closure(_, let newcaptured, let newcode) = proc.kind else {
      preconditionFailure()
    }
    // Set new code and capture list
    self.registers.captured = newcaptured
    self.registers.code = newcode
  }
  
  private func invoke(inout n: Int, _ overhead: Int) throws -> Procedure {
    // Get procedure to call
    guard case .Proc(let p) = self.stack[self.sp - n - 1] else {
      throw EvalError.NonApplicativeValue(self.stack[self.sp - n - 1])
    }
    var proc = p
    // Handle parameter procedures
    if case .Parameter(let tuple) = proc.kind {
      switch n {
        case 0:                             // Return parameter value
          self.pop(overhead)
          self.push(self.getParam(proc)!)
          return proc
        case 1 where tuple.fst.isNull:      // Set parameter value without setter
          let a0 = self.pop()
          self.pop(overhead)
          self.push(self.setParam(proc, to: a0))
          return proc
        case 1:                             // Set parameter value with setter
          let a0 = self.pop()
          self.pop()
          self.push(tuple.fst)
          self.push(.Proc(proc))
          self.push(a0)
          self.push(.Proc(self.setParameterProc))
          n = 3
          proc = try tuple.fst.asProc()
        default:
          throw EvalError.ArgumentCountError(formals: 1, args: self.popAsList(n))
      }
    }
    // Handle primitive procedures; this is required to loop because of applicators
    loop: while case .Primitive(_, let impl, _) = proc.kind {
      switch impl {
        case .Eval(let eval):
          let generated = Procedure(try eval(self.stack[self.sp-n..<self.sp]))
          self.pop(n + 1)
          self.push(.Proc(generated))
          n = 0
          return generated
        case .Apply(let apply):
          let (next, args) = try apply(self.stack[self.sp-n..<self.sp])
          self.pop(n + 1)
          self.push(.Proc(next))
          for arg in args {
            self.push(arg)
          }
          n = args.count
          proc = next
        case .Native0(let exec):
          guard n == 0 else {
            throw EvalError.ArgumentCountError(formals: 0, args: self.popAsList(n))
          }
          self.pop(overhead)
          self.push(try exec())
          return proc
        case .Native1(let exec):
          guard n == 1 else {
            throw EvalError.ArgumentCountError(formals: 1, args: self.popAsList(n))
          }
          let a0 = self.pop()
          self.pop(overhead)
          self.push(try exec(a0))
          return proc
        case .Native2(let exec):
          guard n == 2 else {
            throw EvalError.ArgumentCountError(formals: 2, args: self.popAsList(n))
          }
          let a1 = self.pop()
          let a0 = self.pop()
          self.pop(overhead)
          self.push(try exec(a0, a1))
          return proc
        case .Native3(let exec):
          guard n == 3 else {
            throw EvalError.ArgumentCountError(formals: 3, args: self.popAsList(n))
          }
          let a2 = self.pop()
          let a1 = self.pop()
          let a0 = self.pop()
          self.pop(overhead)
          self.push(try exec(a0, a1, a2))
          return proc
        case .Native4(let exec):
          guard n == 4 else {
            throw EvalError.ArgumentCountError(formals: 4, args: self.popAsList(n))
          }
          let a3 = self.pop()
          let a2 = self.pop()
          let a1 = self.pop()
          let a0 = self.pop()
          self.pop(overhead)
          self.push(try exec(a0, a1, a2, a3))
          return proc
        case .Native0O(let exec):
          if n == 0 {
            self.pop(overhead)
            self.push(try exec(nil))
          } else if n == 1 {
            let a0 = self.pop()
            self.pop(overhead)
            self.push(try exec(a0))
          } else {
            throw EvalError.ArgumentCountError(formals: 1, args: self.popAsList(n))
          }
          return proc
        case .Native1O(let exec):
          if n == 1 {
            let a0 = self.pop()
            self.pop(overhead)
            self.push(try exec(a0, nil))
          } else if n == 2 {
            let a1 = self.pop()
            let a0 = self.pop()
            self.pop(overhead)
            self.push(try exec(a0, a1))
          } else {
            throw EvalError.ArgumentCountError(formals: 2, args: self.popAsList(n))
          }
          return proc
        case .Native2O(let exec):
          if n == 2 {
            let a1 = self.pop()
            let a0 = self.pop()
            self.pop(overhead)
            self.push(try exec(a0, a1, nil))
          } else if n == 3 {
            let a2 = self.pop()
            let a1 = self.pop()
            let a0 = self.pop()
            self.pop(overhead)
            self.push(try exec(a0, a1, a2))
          } else {
            throw EvalError.ArgumentCountError(formals: 3, args: self.popAsList(n))
          }
          return proc
        case .Native3O(let exec):
          if n == 3 {
            let a2 = self.pop()
            let a1 = self.pop()
            let a0 = self.pop()
            self.pop(overhead)
            self.push(try exec(a0, a1, a2, nil))
          } else if n == 4 {
            let a3 = self.pop()
            let a2 = self.pop()
            let a1 = self.pop()
            let a0 = self.pop()
            self.pop(overhead)
            self.push(try exec(a0, a1, a2, a3))
          } else {
            throw EvalError.ArgumentCountError(formals: 3, args: self.popAsList(n))
          }
          return proc
        case .Native0R(let exec):
          let res = try exec(self.stack[self.sp-n..<self.sp])
          self.pop(n + overhead)
          self.push(res)
          return proc
        case .Native1R(let exec):
          if n >= 1 {
            let res = try exec(self.stack[self.sp - n], self.stack[self.sp-n+1..<self.sp])
            self.pop(n + overhead)
            self.push(res)
          } else {
            throw EvalError.LeastArgumentCountError(formals: 1, args: self.popAsList(n))
          }
          return proc
        case .Native2R(let exec):
          if n >= 2 {
            let res = try exec(self.stack[self.sp - n],
                               self.stack[self.sp - n + 1],
                               self.stack[self.sp-n+2..<self.sp])
            self.pop(n + overhead)
            self.push(res)
          } else {
            throw EvalError.LeastArgumentCountError(formals: 2, args: self.popAsList(n))
          }
          return proc
        case .Native3R(let exec):
          if n >= 2 {
            let res = try exec(self.stack[self.sp - n],
                               self.stack[self.sp - n + 1],
                               self.stack[self.sp - n + 2],
                               self.stack[self.sp-n+3..<self.sp])
            self.pop(n + overhead)
            self.push(res)
          } else {
            throw EvalError.LeastArgumentCountError(formals: 2, args: self.popAsList(n))
          }
          return proc
      }
    }
    // Handle continuations
    if case .Continuation(let vmState) = proc.kind {
      // Check that we apply the continuation in the right context
      guard vmState.registers.rid == self.registers.rid else {
        throw EvalError.IllegalContinuationApplication(proc, self.registers.rid)
      }
      // Continuations accept exactly one argument
      guard n == 1 else {
        throw EvalError.ArgumentCountError(formals: 1, args: self.popAsList(n))
      }
      // Retrieve argument
      let arg = self.pop()
      // Restore virtual machine from state encapsulated in continuation
      self.stack = vmState.stack
      self.sp = vmState.sp
      self.registers = vmState.registers
      // Push identity function and argument onto restored virtual machine stack
      self.push(.Proc(BaseLibrary.idProc))
      self.push(arg)
    }
    return proc
  }
  
  @inline(__always) private func collectGarbageIfNeeded() {
    self.execInstr = self.execInstr &+ 1
    if self.execInstr % 0b0111111111111111111 == 0 {
      let res = self.context.objects.collectGarbage()
      log("[collect garbage; freed up objects: \(res)]")
    }
  }
  
  private func execute(code: Code) throws -> Expr {
    self.sp = 0
    self.push(.Proc(Procedure(code)))
    return try self.execute(code, args: 0, captured: VirtualMachine.NO_CAPTURES)
  }
  
  private func execute(code: Code, args: Int, captured: [Expr]) throws -> Expr {
    // Use new registers
    let savedRegisters = self.registers
    self.registers = Registers(code: code,
                               captured: captured,
                               fp: self.sp - args,
                               root: !savedRegisters.isInitialized)
    // Restore old registers when leaving `execute`
    defer {
      self.registers = savedRegisters
    }
    return try self.execute()
  }
  
  private func execute() throws -> Expr {
    while self.registers.ip >= 0 && self.registers.ip < self.registers.code.instructions.count {
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
      self.registers.ip += 1
      switch self.registers.code.instructions[self.registers.ip - 1] {
        case .NoOp:
          break
        case .Pop:
          self.sp -= 1
          self.stack[self.sp] = .Undef
        case .Dup:
          self.push(self.stack[self.sp - 1])
        case .Swap:
          let top = self.stack[self.sp - 1]
          self.stack[self.sp - 1] = self.stack[self.sp - 2]
          self.stack[self.sp - 2] = top
        case .PushGlobal(let index):
          guard case .Sym(let sym) = self.registers.code.constants[index] else {
            preconditionFailure("PushGlobal expects a symbol at index \(index)")
          }
          guard let symval = context.userScope[sym] else {
            throw EvalError.UnboundVariable(sym)
          }
          switch symval {
            case .Undef:
              throw EvalError.VariableNotYetInitialized(sym)
            case .Special(_):
              throw EvalError.IllegalKeywordUsage(.Sym(sym))
            default:
              self.push(symval)
          }
        case .SetGlobal(let index):
          guard case .Sym(let sym) = self.registers.code.constants[index] else {
            preconditionFailure("SetGlobal expects a symbol at index \(index)")
          }
          if let scope = context.userScope.scopeWithBindingFor(sym) {
            scope[sym] = self.pop()
          } else {
            throw EvalError.UnboundVariable(sym)
          }
        case .DefineGlobal(let index):
          guard case .Sym(let sym) = self.registers.code.constants[index] else {
            preconditionFailure("DefineGlobal expects a symbol at index \(index)")
          }
          context.userScope[sym] = self.pop()
        case .PushCaptured(let index):
          self.push(self.registers.captured[index])
        case .PushCapturedValue(let index):
          guard case .Box(let cell) = self.registers.captured[index] else {
            preconditionFailure("PushCapturedValue cannot push \(self.registers.captured[index])")
          }
          if case .Undef = cell.value {
            throw EvalError.VariableNotYetInitialized(nil)
          }
          self.push(cell.value)
        case .SetCapturedValue(let index):
          guard case .Box(let cell) = self.registers.captured[index] else {
            preconditionFailure("SetCapturedValue cannot set value of \(self.registers.captured[index])")
          }
          cell.value = self.pop()
        case .PushLocal(let index):
          self.push(self.stack[self.registers.fp + index])
        case .SetLocal(let index):
          self.stack[self.registers.fp + index] = self.pop()
        case .SetLocalValue(let index):
          guard case .Box(let cell) = self.stack[self.registers.fp + index] else {
            preconditionFailure("SetLocalValue cannot set value of \(self.stack[self.registers.fp + index])")
          }
          cell.value = self.pop()
        case .MakeLocalVariable(let index):
          let cell = Cell(self.pop())
          self.context.objects.manage(cell)
          self.stack[self.registers.fp + index] = .Box(cell)
        case .MakeVariableArgument(let index):
          let cell = Cell(self.stack[self.registers.fp + index])
          self.context.objects.manage(cell)
          self.stack[self.registers.fp + index] = .Box(cell)
        case .PushLocalValue(let index):
          guard case .Box(let cell) = self.stack[self.registers.fp + index] else {
            preconditionFailure("PushLocalValue cannot push \(self.stack[self.registers.fp + index])")
          }
          if case .Undef = cell.value {
            throw EvalError.VariableNotYetInitialized(nil)
          }
          self.push(cell.value)
        case .PushConstant(let index):
          self.push(self.registers.code.constants[index])
        case .PushUndef:
          self.push(.Undef)
        case .PushVoid:
          self.push(.Void)
        case .PushEof:
          self.push(.Void)
        case .PushNull:
          self.push(.Null)
        case .PushTrue:
          self.push(.True)
        case .PushFalse:
          self.push(.False)
        case .PushFixnum(let num):
          self.push(.Fixnum(num))
        case .PushBignum(let num):
          self.push(.Bignum(num))
        case .PushRat(let num):
          self.push(.Rat(ImmutableBox(num)))
        case .PushBigrat(let num):
          self.push(.Bigrat(ImmutableBox(num)))
        case .PushFlonum(let num):
          self.push(.Flonum(num))
        case .PushComplex(let num):
          self.push(.Complexnum(ImmutableBox(num)))
        case .PushChar(let char):
          self.push(.Char(char))
        case .MakeClosure(let i, let n, let index):
          if i >= 0 {
            guard case .Sym(let sym) = self.registers.code.constants[i] else {
              preconditionFailure(
                "MakeClosure has broken closure name \(self.registers.code.constants[i])")
            }
            self.push(.Proc(Procedure(sym.description,
                                      self.captureExprs(n),
                                      self.registers.code.fragments[index])))
          } else {
            self.push(.Proc(Procedure(nil,
                                      self.captureExprs(n),
                                      self.registers.code.fragments[index])))
        }
        case .MakePromise:
          let top = self.pop()
          guard case .Proc(let proc) = top else {
            preconditionFailure("MakePromise cannot create promise from \(top)")
          }
          let future = Future(proc)
          self.context.objects.manage(future)
          self.push(.Promise(future))
        case .MakeSyntax:
          let transformer = self.pop()
          guard case .Proc(let proc) = transformer else {
            throw EvalError.MalformedTransformer(transformer)
          }
          self.push(.Special(SpecialForm(proc)))
        case .Compile:
          let code = try Compiler.compile(self.context, expr: .List(self.pop()), optimize: true)
          self.push(.Proc(Procedure(code)))
        case .Apply(let m):
          let arglist = self.pop()
          var args = arglist
          var n = m - 1
          while case .Pair(let arg, let rest) = args {
            self.push(arg)
            n += 1
            args = rest
          }
          guard args.isNull else {
            throw EvalError.MalformedArgumentList(arglist)
          }
          // Store instruction pointer
          self.stack[self.sp - n - 2] = .Fixnum(Int64(self.registers.ip))
          // Invoke native function
          if case .Closure(_, let newcaptured, let newcode) = try self.invoke(&n, 3).kind {
            self.registers.use(code: newcode, captured: newcaptured, fp: self.sp - n)
          }
        case .MakeFrame:
          // Push frame pointer
          self.push(.Fixnum(Int64(self.registers.fp)))
          // Reserve space for instruction pointer
          self.push(.Undef)
        case .Call(let n):
          // Store instruction pointer
          self.stack[self.sp - n - 2] = .Fixnum(Int64(self.registers.ip))
          // Invoke native function
          var m = n
          if case .Closure(_, let newcaptured, let newcode) = try self.invoke(&m, 3).kind {
            self.registers.use(code: newcode, captured: newcaptured, fp: self.sp - m)
          }
        case .TailCall(let m):
          // Invoke native function
          var n = m
          let proc = try self.invoke(&n, 1)
          if case .Closure(_, let newcaptured, let newcode) = proc.kind {
            // Execute closure
            self.registers.use(code: newcode, captured: newcaptured, fp: self.registers.fp)
            // Shift stack frame down to next stack frame
            for i in 0...n {
              self.stack[self.registers.fp - 1 + i] = self.stack[self.sp - n - 1 + i]
            }
            // Wipe the now empty part of the stack
            for i in self.registers.fp+n..<self.sp {
              self.stack[i] = .Undef
            }
            // Adjust the stack pointer
            self.sp = self.registers.fp + n
          } else if case .Continuation(_) = proc.kind {
            break
          } else if self.registers.topLevel {
            // Return to interactive environment
            let res = self.pop()
            // Wipe the stack
            for i in self.registers.initialFp-1..<self.sp {
              self.stack[i] = .Undef
            }
            self.sp = self.registers.initialFp - 1
            return res
          } else {
            self.exitFrame()
          }
        case .AssertArgCount(let n):
          guard self.sp - n == self.registers.fp else {
            throw EvalError.ArgumentCountError(formals: n, args: self.popAsList(self.sp - self.registers.fp))
          }
        case .AssertMinArgCount(let n):
          guard self.sp - n >= self.registers.fp else {
            throw EvalError.ArgumentCountError(formals: n, args: self.popAsList(self.sp - self.registers.fp))
          }
        case .NoMatchingArgCount:
          throw EvalError.NoMatchingCase(
            args: self.popAsList(self.sp - self.registers.fp), proc: self.pop())
        case .CollectRest(let n):
          var rest = Expr.Null
          while self.sp > self.registers.fp + n {
            rest = .Pair(self.pop(), rest)
          }
          self.push(rest)
        case .Alloc(let n):
          self.sp += n
        case .Reset(let index, let n):
          for i in self.registers.fp+index..<self.registers.fp+index+n {
            self.stack[i] = .Undef
          }
        case .Return:
          // Return to interactive environment
          if self.registers.topLevel {
            let res = self.pop()
            // Wipe the stack
            for i in self.registers.initialFp-1..<self.sp {
              self.stack[i] = .Undef
            }
            // Reset stack and frame pointer
            self.sp = self.registers.initialFp - 1
            return res
          } else {
            self.exitFrame()
          }
        case .Branch(let offset):
          self.registers.ip += offset - 1
        case .BranchIf(let offset):
          if self.pop().isTrue {
            self.registers.ip += offset - 1
          }
        case .BranchIfNot(let offset):
          if self.pop().isFalse {
            self.registers.ip += offset - 1
          }
        case .BranchIfArgMismatch(let n, let offset):
          if self.sp - n != self.registers.fp {
            self.registers.ip += offset - 1
          }
        case .BranchIfMinArgMismatch(let n, let offset):
          if self.sp - n < self.registers.fp {
            self.registers.ip += offset - 1
          }
        case .And(let offset):
          if self.stack[self.sp - 1].isFalse {
            self.registers.ip += offset - 1
          } else {
            self.pop()
          }
        case .Or(let offset):
          if self.stack[self.sp - 1].isTrue {
            self.registers.ip += offset - 1
          } else {
            self.pop()
          }
        case .Force:
          if case .Promise(let future) = self.stack[self.sp - 1] {
            switch future.state {
              case .Lazy(let proc):
                // Push frame pointer
                self.push(.Fixnum(Int64(self.registers.fp)))
                // Push instruction pointer
                self.push(.Fixnum(Int64(self.registers.ip)))
                // Push procedure that yields the forced result
                self.push(.Proc(proc))
                // Invoke native function
                var n = 0
                if case .Closure(_, let newcaptured, let newcode) = try self.invoke(&n, 3).kind {
                  self.registers.use(code: newcode, captured: newcaptured, fp: self.sp)
                }
              case .Shared(let future):
                // Replace the promise with the shared promise on the stack
                self.stack[self.sp - 1] = .Promise(future)
                // Execute force with the new promise on the stack
                self.registers.ip -= 1
              case .Value(let value):
                // Replace the promise with the value on the stack
                self.stack[self.sp - 1] = value
                // Jump over StoreInPromise operation
                self.registers.ip += 1
              case .Thrown(let error):
                throw error
            }
          }
        case .StoreInPromise:
          guard case .Promise(let future) = self.stack[self.sp - 2] else {
            preconditionFailure()
          }
          switch future.state {
            case .Lazy(_), .Shared(_):
              guard case .Promise(let result) = self.stack[self.sp - 1] else {
                throw EvalError.TypeError(self.stack[self.sp - 1], [.PromiseType])
              }
              future.state = result.state
              result.state = .Shared(future)
            default:
              break
          }
          // Pop result of execution of thunk from stack
          self.sp -= 1
          self.stack[self.sp] = .Undef
          // Re-execute force
          self.registers.ip -= 2
        case .PushCurrentTime:
          self.push(.Flonum(Timer.currentTimeInSec))
        case .Display:
          let obj = self.pop()
          switch obj {
            case .Str(let str):
              context.console.print(str as String)
            default:
              context.console.print(obj.description)
          }
        case .Newline:
          context.console.print("\n")
        case .Eq:
          self.push(.Boolean(eqExpr(self.pop(), self.pop())))
        case .Eqv:
          self.push(.Boolean(eqvExpr(self.pop(), self.pop())))
        case .Equal:
          self.push(.Boolean(equalExpr(self.pop(), self.pop())))
        case .IsPair:
          if case .Pair(_, _) = self.pop() {
            self.push(.True)
          } else {
            self.push(.False)
          }
        case .IsNull:
          self.push(.Boolean(self.pop() == .Null))
        case .Cons:
          let cdr = self.pop()
          self.push(.Pair(self.pop(), cdr))
        case .Car:
          let expr = self.pop()
          guard case .Pair(let car, _) = expr else {
            throw EvalError.TypeError(expr, [.PairType])
          }
          self.push(car)
        case .Cdr:
          let expr = self.pop()
          guard case .Pair(_, let cdr) = expr else {
            throw EvalError.TypeError(expr, [.PairType])
          }
          self.push(cdr)
        case .List(let n):
          var res = Expr.Null
          for _ in 0..<n {
            res = .Pair(self.pop(), res)
          }
          self.push(res)
        case .Vector(let n):
          let vector = Collection(kind: .Vector)
          var i = self.sp - n
          while i < self.sp {
            vector.exprs.append(self.stack[i])
            i += 1
          }
          self.pop(n)
          self.push(.Vector(vector))
        case .ListToVector:
          let expr = self.pop()
          let vector = Collection(kind: .Vector)
          var list = expr
          while case .Pair(let car, let cdr) = list {
            vector.exprs.append(car)
            list = cdr
          }
          guard list.isNull else {
            throw EvalError.TypeError(expr, [.ProperListType])
          }
          self.push(.Vector(vector))
        case .VectorAppend(let n):
          let vector = Collection(kind: .Vector)
          var i = self.sp - n
          while i < self.sp {
            vector.exprs.appendContentsOf(try self.stack[i].asVector().exprs)
            i += 1
          }
          self.pop(n)
          self.push(.Vector(vector))
        case .IsVector:
          if case .Vector(_) = self.pop() {
            self.push(.True)
          } else {
            self.push(.False)
          }
        case .FxPlus:
          let rhs = self.pop()
          self.push(.Fixnum(try self.pop().asInteger() &+ rhs.asInteger()))
        case .FxMinus:
          let rhs = self.pop()
          self.push(.Fixnum(try self.pop().asInteger() &- rhs.asInteger()))
        case .FxMult:
          let rhs = self.pop()
          self.push(.Fixnum(try self.pop().asInteger() &* rhs.asInteger()))
        case .FxDiv:
          let rhs = self.pop()
          self.push(.Fixnum(try self.pop().asInteger() / rhs.asInteger()))
        case .FlPlus:
          let rhs = self.pop()
          self.push(.Flonum(try self.pop().asFloat() + rhs.asFloat()))
        case .FlMinus:
          let rhs = self.pop()
          self.push(.Flonum(try self.pop().asFloat() - rhs.asFloat()))
        case .FlMult:
          let rhs = self.pop()
          self.push(.Flonum(try self.pop().asFloat() * rhs.asFloat()))
        case .FlDiv:
          let rhs = self.pop()
          self.push(.Flonum(try self.pop().asFloat() / rhs.asFloat()))
      }
    }
    return .Null
  }
  
  public override func mark(tag: UInt8) {
    super.mark(tag)
    for i in 0..<self.sp {
      self.stack[i].mark(tag)
    }
    self.registers.mark(tag)
    self.winders?.mark(tag)
    self.parameters.mark(tag)
  }
  
  /// Debugging output
  private func stackFragmentDescr(ip: Int, _ fp: Int, header: String? = nil) -> String {
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
}
