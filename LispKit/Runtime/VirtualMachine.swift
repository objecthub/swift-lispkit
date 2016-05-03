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
///    | ...              |
///    | Intermed. val. 0 |
///    +==================+
///    | Local variable 1 |
///    | Local variable 0 | <- fp + args
///    | Argument 1       |
///    | Argument 0       | <- fp
///    | Program          | <- fp - 1
///    +------------------+                 | PushFrame |
///    | Return address   | <- fp - 2       v           v
///    | Dynamic link     | <- fp - 3
///    +==================+
///    |                  |
///
public final class VirtualMachine: TrackedObject {
  
  /// The context of this virtual machine
  private let context: Context
  
  /// The stack used by this virtual machine
  private var stack: [Expr]
  
  /// The stack pointer (pointing at the next available position on the stack)
  private var sp: Int {
    didSet {
      if sp > self.maxSp {
        self.maxSp = self.sp
      }
    }
  }
  
  /// The maximum value of the stack pointer so far (used for debugging)
  private var maxSp: Int
  
  /// Internal counter used for triggering the garbage collector
  private var execInstr: UInt64
  
  /// Constant representing an empty capture set
  private static let NO_CAPTURES = [Variable]()
  
  /// Initializes a new virtual machine for the given context
  public init(_ context: Context) {
    self.context = context
    self.stack = [Expr](count: 1024, repeatedValue: .Undef)
    self.sp = 0
    self.maxSp = 0
    self.execInstr = 0
  }
  
  /// Loads the file at file patch `path`, compiles it in the interaction environment, and
  /// executes it using this virtual machine.
  public func evalFile(path: String) -> Expr {
    do {
      return self.evalStr(try String(contentsOfFile: path, encoding: NSUTF8StringEncoding))
    } catch let error as NSError {
      return .Error(AnyError(OsError(error)))
    }
  }
  
  /// Parses the given string, compiles it in the interaction environment, and executes it using
  /// this virtual machine.
  public func evalStr(str: String) -> Expr {
    do {
      let parser = Parser(symbols: self.context.symbols, src: str)
      var exprs = Exprs()
      while !parser.finished {
        exprs.append(try parser.parse())
      }
      let compiler = Compiler(self.context, .Interaction)
      try compiler.compileBody(.List(exprs))
      let code = compiler.bundle()
      print(code.description)
      return try self.execute(code)
    } catch let error as LispError { // handle Lisp-related issues
      return .Error(AnyError(error))
    } catch { // handle internal issues
      // TODO: Figure out what needs to be logged here
      print("UNKNOWN ERROR in Evaluator.evalExprProvidedBy")
      return .Undef
    }
  }
  
  /// Compiles the given expression in the interaction environment and executes it using this
  /// virtual machine.
  public func evalExpr(expr: Expr) -> Expr {
    do {
      let compiler = Compiler(self.context, .Interaction)
      try compiler.compileBody(.List(expr))
      let code = compiler.bundle()
      // context.console.printStr(code.description)
      return try self.execute(code)
    } catch let error as LispError { // handle Lisp-related issues
      return .Error(AnyError(error))
    } catch let error as NSError { // handle OS-related issues
      return .Error(AnyError(OsError(error)))
    } catch { // handle internal issues
      // TODO: Figure out what needs to be logged here
      print("UNKNOWN ERROR in Evaluator.evalExpr")
      return .Undef
    }
  }
  
  /// Compiles the given expression `expr` in the environment `env` and executes it using
  /// this virtual machine.
  public func eval(expr: Expr,
                   in env: Env = .Interaction,
                   usingRulesEnv renv: Env? = nil) throws -> Expr {
    let compiler = Compiler(self.context, env, renv)
    try compiler.compileBody(.List(expr))
    let code = compiler.bundle()
    return try self.apply(.Proc(Procedure(code)), to: .Null)
  }
  
  /// Applies `args` to the function `fun` in environment `env`.
  public func apply(fun: Expr, to args: Expr, in env: Env = .Interaction) throws -> Expr {
    self.push(fun)
    let n = try self.pushArguments(args)
    let proc = try self.invoke(n, 1)
    switch proc.kind {
      case .Closure(let captured, let code):
        return try self.execute(code, args: n, captured: captured)
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
  private func push(expr: Expr) {
    self.stack[self.sp] = expr
    self.sp += 1
  }
  
  /// Pushes the given list of arguments onto the stack and returns the number of arguments pushed
  /// onto the stack.
  private func pushArguments(arglist: Expr) throws -> Int {
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
  private func pop(n: Int) {
    var i = n
    while i > 0 {
      self.sp -= 1
      self.stack[self.sp] = .Undef
      i -= 1
    }
  }
  
  /// Removes the top element from the stack and returns it.
  private func pop() -> Expr {
    self.sp -= 1
    let res = self.stack[self.sp]
    self.stack[self.sp] = .Undef
    return res
  }
  
  private func popAsList(n: Int) -> Expr {
    var res = Expr.Null
    var i = n
    while i > 0 {
      res = .Pair(self.pop(), res)
      i -= 1
    }
    return res
  }
  
  private func popAsArray(n: Int) -> Exprs {
    var res = Exprs()
    var i = n
    while i > 0 {
      res.append(self.stack[self.sp - i])
      self.stack[self.sp - i] = .Undef
      i -= 1
    }
    return res
  }
  
  private func captureVariables(n: Int) -> [Variable] {
    var captures = [Variable]()
    var i = n
    while i > 0 {
      guard case .Var(let variable) = self.stack[self.sp - i] else {
        preconditionFailure("pushed as capture: \(self.stack[self.sp - i])")
      }
      captures.append(variable)
      self.stack[self.sp - i] = .Undef
      i -= 1
    }
    self.sp -= n
    return captures
  }
  
  private func exitFrame(inout fp: Int) -> Procedure {
    // Determine former fp
    guard case .Fixnum(let newfp) = self.stack[fp - 3] else {
      preconditionFailure()
    }
    // Shift result down
    self.stack[fp - 3] = self.stack[self.sp - 1]
    // Clean up stack that is freed up
    for i in fp-2..<self.sp {
      self.stack[i] = .Undef
    }
    // Set new fp and sp
    self.sp = fp - 2
    fp = Int(newfp)
    // Determine closure to which execution returns to
    guard case .Proc(let proc) = self.stack[newfp - 1] else {
      preconditionFailure()
    }
    return proc
  }
  
  private func invoke(n: Int, _ overhead: Int) throws -> Procedure {
    // Get procedure to call
    guard case .Proc(let proc) = self.stack[self.sp - n - 1] else {
      throw EvalError.NonApplicativeValue(self.stack[self.sp - n - 1])
    }
    // Return non-primitive procedures
    guard case .Primitive(let impl, _) = proc.kind else {
      return proc
    }
    // Invoke primitive
    switch impl {
      case .Impl0(let proc):
        guard n == 0 else {
          throw EvalError.ArgumentCountError(formals: 0, args: self.popAsList(n))
        }
        self.pop(overhead)
        self.push(try proc())
      case .Impl1(let proc):
        guard n == 1 else {
          throw EvalError.ArgumentCountError(formals: 1, args: self.popAsList(n))
        }
        let a0 = self.pop()
        self.pop(overhead)
        self.push(try proc(a0))
      case .Impl2(let proc):
        guard n == 2 else {
          throw EvalError.ArgumentCountError(formals: 2, args: self.popAsList(n))
        }
        let a1 = self.pop()
        let a0 = self.pop()
        self.pop(overhead)
        self.push(try proc(a0, a1))
      case .Impl3(let proc):
        guard n == 3 else {
          throw EvalError.ArgumentCountError(formals: 3, args: self.popAsList(n))
        }
        let a2 = self.pop()
        let a1 = self.pop()
        let a0 = self.pop()
        self.pop(overhead)
        self.push(try proc(a0, a1, a2))
      case .Impl0O(let proc):
        if n == 0 {
          self.pop(overhead)
          self.push(try proc(nil))
        } else if n == 1 {
          let a0 = self.pop()
          self.pop(overhead)
          self.push(try proc(a0))
        } else {
          throw EvalError.ArgumentCountError(formals: 1, args: self.popAsList(n))
        }
      case .Impl1O(let proc):
        if n == 1 {
          let a0 = self.pop()
          self.pop(overhead)
          self.push(try proc(a0, nil))
        } else if n == 2 {
          let a1 = self.pop()
          let a0 = self.pop()
          self.pop(overhead)
          self.push(try proc(a0, a1))
        } else {
          throw EvalError.ArgumentCountError(formals: 2, args: self.popAsList(n))
        }
      case .Impl2O(let proc):
        if n == 2 {
          let a1 = self.pop()
          let a0 = self.pop()
          self.pop(overhead)
          self.push(try proc(a0, a1, nil))
        } else if n == 3 {
          let a2 = self.pop()
          let a1 = self.pop()
          let a0 = self.pop()
          self.pop(overhead)
          self.push(try proc(a0, a1, a2))
        } else {
          throw EvalError.ArgumentCountError(formals: 3, args: self.popAsList(n))
        }
      case .Impl0R(let proc):
        let res = try proc(self.stack[self.sp-n..<self.sp])
        self.pop(n + overhead)
        self.push(res)
      case .Impl1R(let proc):
        if n >= 1 {
          let res = try proc(self.stack[self.sp - n], self.stack[self.sp-n+1..<self.sp])
          self.pop(n + overhead)
          self.push(res)
        } else {
          throw EvalError.LeastArgumentCountError(formals: 1, args: self.popAsList(n))
        }
      case .Impl2R(let proc):
        if n >= 2 {
          let res = try proc(self.stack[self.sp - n],
            self.stack[self.sp - n + 1],
            self.stack[self.sp-n+2..<self.sp])
          self.pop(n + overhead)
          self.push(res)
        } else {
          throw EvalError.LeastArgumentCountError(formals: 2, args: self.popAsList(n))
        }
    }
    return proc
  }
  
  private func collectGarbageIfNeeded() {
    self.execInstr = self.execInstr &+ 1
    if self.execInstr % 0b0111111111111111111 == 0 {
      let res = self.context.objects.collectGarbage()
      if DEBUG_OUTPUT {
        print("[collect garbage; freed up objects: \(res)]")
      }
    }
  }
  
  private func execute(code: Code) throws -> Expr {
    self.sp = 0
    self.push(.Proc(Procedure(code)))
    return try self.execute(code, args: 0, captured: VirtualMachine.NO_CAPTURES)
  }
  
  private func execute(code: Code, args: Int, captured: [Variable]) throws -> Expr {
    var code = code
    var captured = captured
    var ip = 0
    var fp = self.sp - args
    let initialFp = fp
    while true {
      if ip < 0 || ip >= code.instructions.count {
        self.context.console.print(self.stackFragmentDescr(ip, fp))
        return .Null
      }
      self.collectGarbageIfNeeded()
      /*
      print("╔══════════════════════════════════════════════════════")
      if ip > 0 {
        print("║     \(ip - 1)  \(code.instructions[ip - 1])")
      }
      print("║    [\(ip)] \(code.instructions[ip])")
      if ip < code.instructions.count - 1 {
        print("║     \(ip + 1)  \(code.instructions[ip + 1])")
      }
      print(stackFragmentDescr(ip, fp, header: "╟──────────────────────────────────────────────────────\n"))
      */
      ip += 1
      switch code.instructions[ip - 1] {
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
          guard case .Sym(let sym) = code.constants[index] else {
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
          guard case .Sym(let sym) = code.constants[index] else {
            preconditionFailure("SetGlobal expects a symbol at index \(index)")
          }
          if let scope = context.userScope.scopeWithBindingFor(sym) {
            scope[sym] = self.pop()
          } else {
            throw EvalError.UnboundVariable(sym)
          }
        case .DefineGlobal(let index):
          guard case .Sym(let sym) = code.constants[index] else {
            preconditionFailure("DefineGlobal expects a symbol at index \(index)")
          }
          context.userScope[sym] = self.pop()
        case .PushCaptured(let index):
          self.push(.Var(captured[index]))
        case .PushCapturedValue(let index):
          let value = captured[index].value
          if case .Undef = value {
            throw EvalError.VariableNotYetInitialized(nil)
          }
          self.push(value)
        case .SetCapturedValue(let index):
          captured[index].value = self.pop()
        case .PushLocal(let index):
          self.push(self.stack[fp + index])
        case .SetLocal(let index):
          self.stack[fp + index] = self.pop()
        case .SetLocalVariable(let index):
          let variable = Variable(self.pop())
          self.context.objects.manage(variable)
          self.stack[fp + index] = .Var(variable)
        case .PushLocalValue(let index):
          guard case .Var(let variable) = self.stack[fp + index] else {
            preconditionFailure("PushLocalValue cannot push \(captured[index])")
          }
          if case .Undef = variable.value {
            throw EvalError.VariableNotYetInitialized(nil)
          }
          self.push(variable.value)
        case .SetLocalValue(let index):
          guard case .Var(let variable) = self.stack[fp + index] else {
            preconditionFailure("SetLocalValue cannot set value of \(captured[index])")
          }
          variable.value = self.pop()
        case .PushConstant(let index):
          self.push(code.constants[index])
        case .MakeLocalVariable(let index):
          let variable = Variable(self.stack[fp + index])
          self.context.objects.manage(variable)
          self.stack[fp + index] = .Var(variable)
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
          self.push(.Rat(num))
        case .PushBigrat(let num):
          self.push(.Bigrat(num))
        case .PushFlonum(let num):
          self.push(.Flonum(num))
        case .PushComplex(let num):
          self.push(.Complexnum(num))
        case .PushChar(let char):
          self.push(.Char(char))
        case .PushClosure(let n, let index):
          self.push(.Proc(Procedure(self.captureVariables(n), code.fragments[index])))
        case .MakePromise(let n, let index):
          let future = Future(Procedure(self.captureVariables(n), code.fragments[index]))
          self.context.objects.manage(future)
          self.push(.Promise(future))
        case .MakeSyntax:
          let transformer = self.pop()
          guard case .Proc(let proc) = transformer else {
            throw EvalError.MalformedTransformer(transformer)
          }
          self.push(.Special(SpecialForm(proc)))
        case .Compile:
          let compiler = Compiler(self.context, .Interaction)
          try compiler.compileBody(.List(self.pop()))
          self.push(.Proc(Procedure(compiler.bundle())))
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
          self.stack[self.sp - n - 2] = .Fixnum(Int64(ip))
          // Invoke native function
          if case .Closure(let newcaptured, let newcode) = try self.invoke(n, 3).kind {
            // Invoke compiled closure
            code = newcode
            captured = newcaptured
            ip = 0
            fp = self.sp - n
          }
        case .PushFrame:
          // Push frame pointer
          self.push(.Fixnum(Int64(fp)))
          // Reserve space for instruction pointer
          self.push(.Undef)
        case .Call(let n):
          // Store instruction pointer
          self.stack[self.sp - n - 2] = .Fixnum(Int64(ip))
          // Invoke native function
          if case .Closure(let newcaptured, let newcode) = try self.invoke(n, 3).kind {
            // Invoke compiled closure
            code = newcode
            captured = newcaptured
            ip = 0
            fp = self.sp - n
          }
        case .TailCall(let n):
          // Invoke native function
          if case .Closure(let newcaptured, let newcode) = try self.invoke(n, 1).kind {
            // Invoke compiled closure
            code = newcode
            captured = newcaptured
            ip = 0
            // Shift stack frame down to next stack frame
            for i in 0...n {
              self.stack[fp - 1 + i] = self.stack[self.sp - n - 1 + i]
            }
            // Wipe the now empty part of the stack
            for i in fp+n..<self.sp {
              self.stack[i] = .Undef
            }
            // Adjust the stack pointer
            self.sp = fp + n
          } else if fp == initialFp {
            // Return to interactive environment
            let res = self.pop()
            // Wipe the stack
            for i in initialFp-1..<self.sp {
              self.stack[i] = .Undef
            }
            self.sp = initialFp - 1
            return res
          } else {
            // Determine former ip
            guard case .Fixnum(let newip) = self.stack[fp - 2] else {
              preconditionFailure()
            }
            ip = Int(newip)
            // Determine former closure and frame pointer
            guard case .Closure(let newcaptured, let newcode) = self.exitFrame(&fp).kind else {
              preconditionFailure()
            }
            captured = newcaptured
            code = newcode
          }
        case .AssertArgCount(let n):
          guard self.sp - n == fp else {
            throw EvalError.ArgumentCountError(formals: n, args: self.popAsList(self.sp - fp))
          }
        case .AssertMinArgs(let n):
          guard self.sp - n >= fp else {
            throw EvalError.ArgumentCountError(formals: n, args: self.popAsList(self.sp - fp))
          }
        case .CollectRest(let n):
          var rest = Expr.Null
          while self.sp > fp + n {
            rest = .Pair(self.pop(), rest)
          }
          self.push(rest)
        case .ReserveLocals(let n):
          self.sp += n
        case .ResetLocals(let index, let n):
          for i in fp+index..<fp+index+n {
            self.stack[i] = .Undef
          }
        case .Return:
          // Return to interactive environment
          guard fp > initialFp else {
            let res = self.pop()
            // Wipe the stack
            for i in initialFp-1..<self.sp {
              self.stack[i] = .Undef
            }
            // Reset stack and frame pointer
            self.sp = initialFp - 1
            return res
          }
          // Determine former ip
          guard case .Fixnum(let newip) = self.stack[fp - 2] else {
            preconditionFailure()
          }
          ip = Int(newip)
          // Determine former closure and frame pointer
          guard case .Closure(let newcaptured, let newcode) = self.exitFrame(&fp).kind else {
            preconditionFailure()
          }
          captured = newcaptured
          code = newcode
        case .Branch(let offset):
          ip += offset - 1
        case .BranchIf(let offset):
          if self.pop().isTrue {
            ip += offset - 1
          }
        case .BranchIfNot(let offset):
          if self.pop().isFalse {
            ip += offset - 1
          }
        case .And(let offset):
          if self.stack[self.sp - 1].isFalse {
            ip += offset - 1
          } else {
            self.pop()
          }
        case .Or(let offset):
          if self.stack[self.sp - 1].isTrue {
            ip += offset - 1
          } else {
            self.pop()
          }
        case .Force:
          if case .Promise(let future) = self.stack[self.sp - 1] {
            switch future.state {
              case .Unevaluated(let proc):
                // Push frame pointer
                self.push(.Fixnum(Int64(fp)))
                // Push instruction pointer
                self.push(.Fixnum(Int64(ip)))
                // Push procedure that yields the forced result
                self.push(.Proc(proc))
                // Invoke native function
                if case .Closure(let newcaptured, let newcode) = try self.invoke(0, 3).kind {
                  // Invoke compiled closure
                  code = newcode
                  captured = newcaptured
                  ip = 0
                  fp = self.sp
                }
              case .Value(let value):
                // Replace the promise with the value on the stack
                self.stack[self.sp - 1] = value
                // Jump over StoreInPromise operation
                ip += 1
              case .Thrown(let error):
                throw error
            }
          }
        case .StoreInPromise:
          guard case .Promise(let future) = self.stack[self.sp - 2] else {
            preconditionFailure()
          }
          future.state = .Value(self.stack[self.sp - 1])
          self.stack[self.sp - 2] = self.stack[self.sp - 1]
          self.sp -= 1
          self.stack[self.sp] = .Undef
        case .PushCurrentTime:
          self.push(.Flonum(Timer.currentTimeInSec))
        case .Display:
          let obj = self.pop()
          switch obj {
            case .Str(let str):
              context.console.print(str.value)
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
          let vector = Vector()
          var i = self.sp - n
          while i < self.sp {
            vector.exprs.append(self.stack[i])
            i += 1
          }
          self.pop(n)
          self.push(.Vec(vector))
        case .ListToVector:
          let expr = self.pop()
          let vector = Vector()
          var list = expr
          while case .Pair(let car, let cdr) = list {
            vector.exprs.append(car)
            list = cdr
          }
          guard list.isNull else {
            throw EvalError.TypeError(expr, [.ProperListType])
          }
          self.push(.Vec(vector))
        case .VectorAppend(let n):
          let vector = Vector()
          var i = self.sp - n
          while i < self.sp {
            vector.exprs.appendContentsOf(try self.stack[i].asVector().exprs)
            i += 1
          }
          self.pop(n)
          self.push(.Vec(vector))
        case .IsVector:
          if case .Vec(_) = self.pop() {
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
  }
  
  public override func mark(tag: UInt8) {
    super.mark(tag)
    for i in 0..<self.sp {
      self.stack[i].mark(tag)
    }
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
