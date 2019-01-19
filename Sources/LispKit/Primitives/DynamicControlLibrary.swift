//
//  DynamicControlLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 14/07/2016.
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

import Cocoa

public final class DynamicControlLibrary: NativeLibrary {
  
  private var raiseProcLoc: Int? = nil
  public var raiseProc: Procedure? {
    if let loc = self.raiseProcLoc {
      return self.procedure(loc)
    } else {
      return nil
    }
  }
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "dynamic"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"],      "define", "set!", "define-syntax",
                                                  "syntax-rules", "lambda", "eqv?", "void", "or",
                                                  "call-with-values", "apply-with-values")
    self.`import`(from: ["lispkit", "control"],   "if", "let", "let*", "do", "begin")
    self.`import`(from: ["lispkit", "box"],       "box")
    self.`import`(from: ["lispkit", "list"],      "car", "cdr", "list", "null?")
    self.`import`(from: ["lispkit", "hashtable"], "hashtable-add!")
  }
  
  /// Declarations of the library.
  public override func declarations() {
    // Multiple values
    self.define(Procedure("_make-values", makeValues), export: false)
    
    // Continuations
    self.define(Procedure("continuation?", isContinuation))
    self.define(SpecialForm("_continuation", compileContinuation), export: false)
    self.define(Procedure("_call-with-unprotected-continuation", callWithUnprotectedContinuation),
                export: false)
    self.define(Procedure("_wind-down", windDown), export: false)
    self.define(Procedure("_wind-up", windUp), export: false)
    self.define(Procedure("_wind-up-raise", windUpRaise), export: false)
    self.define(Procedure("_dynamic-wind-base", dynamicWindBase), export: false)
    self.define(Procedure("_dynamic-wind-current", dynamicWindCurrent), export: false)
    self.define(Procedure("_dynamic-winders", dynamicWinders), export: false)
    self.define("call-with-current-continuation", via:
      "(define (call-with-current-continuation f)",
      "  (_call-with-unprotected-continuation",
      "     (lambda (cont)",
      "       (f (_continuation args",
      "            (do ((base (_dynamic-wind-base cont)))",
      "                ((eqv? (_dynamic-wind-current) base))",
      "              ((cdr (_wind-down))))",
      "            (do ((winders (_dynamic-winders cont) (cdr winders)))",
      "                ((null? winders) (cont (_make-values args)))",
      "              ((car (car winders)))",
      "              (_wind-up (car (car winders)) (cdr (car winders)))))))))")
    self.define("call/cc", via: "(define call/cc call-with-current-continuation)")
    self.define("dynamic-wind", via:
      "(define (dynamic-wind before during after)",
      "  (before)",
      "  (_wind-up before after)",
      "  (apply-with-values (lambda args ((cdr (_wind-down))) (_make-values args)) (during)))")
    
    // Errors
    self.define(Procedure("make-error", makeError))
    self.define(Procedure("error-object-message", errorObjectMessage))
    self.define(Procedure("error-object-irritants", errorObjectIrritants))
    self.define(Procedure("error-object?", isErrorObject))
    self.define(Procedure("read-error?", isReadError))
    self.define(Procedure("file-error?", isFileError))
    
    // Exceptions
    self.define("return", mutable: true, via: "(define return 0)")
    self.execute("(call-with-current-continuation (lambda (cont) (set! return cont)))")
    self.define("with-exception-handler", via:
      "(define (with-exception-handler handler thunk)",
      "  (_wind-up void void handler)",
      "  (let ((res (thunk))) (_wind-down) res))")
    self.raiseProcLoc = self.define("raise", via:
      "(define (raise obj)",
      "  ((or (_wind-up-raise void void) return) obj)",
      "  (raise (make-error \"exception handler returned\" obj)))")
    self.define("raise-continuable", via:
      "(define (raise-continuable obj)",
      "  (let ((res ((_wind-up-raise void void) obj))) (_wind-down) res))")
    self.define("guard-aux", export: false, via:
      "(define-syntax guard-aux",
      "  (syntax-rules (else =>)",
      "    ((guard-aux reraise (else result1 result2 ...))",
      "      (begin result1 result2 ...))",
      "    ((guard-aux reraise (test => result))",
      "      (let ((temp test)) (if temp (result temp) reraise)))",
      "    ((guard-aux reraise (test => result) clause1 clause2 ...)",
      "      (let ((temp test)) (if temp (result temp) (guard-aux reraise clause1 clause2 ...))))",
      "    ((guard-aux reraise (test))",
      "      (or test reraise))",
      "    ((guard-aux reraise (test) clause1 clause2 ...)",
      "      (let ((temp test)) (if temp temp (guard-aux reraise clause1 clause2 ...))))",
      "    ((guard-aux reraise (test result1 result2 ...))",
      "      (if test (begin result1 result2 ...) reraise))",
      "    ((guard-aux reraise (test result1 result2 ...) clause1 clause2 ...)",
      "      (if test (begin result1 result2 ...) (guard-aux reraise clause1 clause2 ...)))))")
    self.define("guard", via:
      "(define-syntax guard",
      "  (syntax-rules ()",
      "    ((guard (var clause ...) e1 e2 ...)",
      "      ((call-with-current-continuation",
      "        (lambda (guard-k)",
      "          (with-exception-handler",
      "            (lambda (condition)",
      "              ((call-with-current-continuation",
      "                (lambda (handler-k)",
      "                  (guard-k",
      "                    (lambda ()",
      "                      (let ((var condition))",
      "                        (guard-aux",
      "                          (handler-k (lambda () (raise-continuable condition)))",
      "                          clause ...))))))))",
      "            (lambda ()",
      "              (call-with-values",
      "                (lambda () e1 e2 ...)",
      "                (lambda args (guard-k (lambda () (_make-values args)))))))))))))")
    self.define("error", via:
      "(define (error message . irritants) (raise (make-error message irritants)))")
    self.define(Procedure("_trigger-exit", triggerExit), export: false)
    self.define("exit", mutable: true, via: "(define exit 0)")
    self.execute("(call-with-current-continuation " +
      "  (lambda (cont) (set! exit (lambda args (_trigger-exit cont args)))))")
    self.define(Procedure("emergency-exit", emergencyExit))
    
    // Parameters
    self.define(Procedure("dynamic-environment", dynamicEnvironment))
    self.define(Procedure("make-dynamic-environment", makeDynamicEnvironment))
    self.define(Procedure("set-dynamic-environment!", setDynamicEnvironment))
    self.define(Procedure("_make-parameter", makeParameter), export: false)
    self.define(Procedure("_bind-parameter", bindParameter), export: false)
    self.define("_dynamic-bind", export: false, via:
      "(define (_dynamic-bind parameters values body)" +
      "  (let* ((old-env (dynamic-environment))" +
      "         (new-env (make-dynamic-environment)))" +
      "    (do ((ps parameters (cdr ps))" +
      "         (vs values (cdr vs)))" +
      "        ((null? ps))" +
      "      (_bind-parameter (car ps)" +
      "                       (car vs)" +
      "                       (lambda (p v) (hashtable-add! new-env p (box v)))))" +
      "    (dynamic-wind (lambda () (set-dynamic-environment! new-env))" +
      "                  body" +
      "                  (lambda () (set-dynamic-environment! old-env)))))")
    self.define("make-parameter", via:
      "(define (make-parameter val . conv)" +
      "  (if (null? conv)" +
      "    (_make-parameter val)" +
      "    (_make-parameter" +
      "      ((car conv) val)" +
      "      (lambda (param val setter) (setter param ((car conv) val))))))")
    self.define("parameterize", via:
      "(define-syntax parameterize" +
      "  (syntax-rules ()" +
      "    ((parameterize ((expr1 expr2) ...) body ...)" +
      "      (_dynamic-bind (list expr1 ...) (list expr2 ...) (lambda () body ...)))))")
  }
  
  // This implementation must only be used in call/cc; it's inherently unsafe to use outside
  // since it doesn't check the structure of expr.
  func makeValues(expr: Expr) -> Expr {
    switch expr {
      case .null:
        return .void // .values(.null)
      case .pair(let x, .null):
        return x
      default:
        return .values(expr)
    }
  }
  
  func isContinuation(_ expr: Expr) -> Expr {
    guard case .procedure(let proc) = expr else {
      return .false
    }
    switch proc.kind {
      case .rawContinuation(_),
           .closure(.continuation, _, _):
        return .true
      default:
        return .false
    }
  }
  
  private func compileContinuation(compiler: Compiler,
                                   expr: Expr,
                                   env: Env,
                                   tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let arglist, let body)) = expr else {
      throw RuntimeError.argumentCount(num: 1, expr: expr)
    }
    try compiler.compileLambda(nil, arglist, body, env, continuation: true)
    return false
  }
  
  func callWithUnprotectedContinuation(_ args: Arguments) throws -> (Procedure, Exprs) {
    guard args.count == 1 else {
      throw RuntimeError.argumentCount(min: 1, max: 1, args: .makeList(args))
    }
    guard case .procedure(let proc) = args.first! else {
      throw RuntimeError.type(args.first!, expected: [.procedureType])
    }
    // Create continuation, removing current argument and the call/cc procedure from the
    // stack of the continuation
    let vmstate = self.context.machine.getState()
    let cont = Procedure(vmstate)
    // Return procedure to call with continuation as argument
    return (proc, [.procedure(cont)])
  }
  
  func windUp(_ before: Expr, after: Expr, handler: Expr?) throws -> Expr {
    var handlers: Expr? = nil
    if let handler = handler {
      handlers = .pair(handler, self.context.machine.currentHandlers() ?? .null)
    }
    self.context.machine.windUp(before: try before.asProcedure(),
                                after: try after.asProcedure(),
                                handlers: handlers)
    return .void
  }
  
  func windUpRaise(_ before: Expr, after: Expr) throws -> Expr {
    switch self.context.machine.currentHandlers() {
      case .some(.pair(let handler, let rest)):
        self.context.machine.windUp(before: try before.asProcedure(),
                                    after: try after.asProcedure(),
                                    handlers: rest)
        return handler
      default:
        return .false
    }
  }
  
  func windDown() -> Expr {
    guard let winder = self.context.machine.windDown() else {
      return .null
    }
    return .pair(.procedure(winder.before), .procedure(winder.after))
  }
  
  func dynamicWindBase(_ cont: Expr) throws -> Expr {
    guard case .rawContinuation(let vmState) = try cont.asProcedure().kind else {
      preconditionFailure("_dynamic-wind-base(\(cont))")
    }
    let base = self.context.machine.winders?.commonPrefix(vmState.winders)
    return .fixnum(base?.id ?? 0)
  }
  
  func dynamicWindCurrent() -> Expr {
    return .fixnum(self.context.machine.winders?.id ?? 0)
  }
  
  func dynamicWinders(_ cont: Expr) throws -> Expr {
    guard case .rawContinuation(let vmState) = try cont.asProcedure().kind else {
      preconditionFailure("_dynamic-winders(\(cont))")
    }
    let base = self.context.machine.winders
    var res: Expr = .null
    var next = vmState.winders
    while let winder = next , (base == nil) || winder !== base! {
      res = .pair(.pair(.procedure(winder.before), .procedure(winder.after)), res)
      next = winder.next
    }
    return res
  }
  
  private func makeError(message: Expr, irritants: Expr) -> Expr {
    return .error(RuntimeError.custom("error",
                                      message.unescapedDescription,
                                      Array(irritants.toExprs().0)))
  }
  
  private func errorObjectMessage(expr: Expr) throws -> Expr {
    switch expr {
      case .error(let err):
        return .makeString(err.message)
      default:
        throw RuntimeError.type(expr, expected: [.errorType])
    }
  }
  
  private func errorObjectIrritants(expr: Expr) throws -> Expr {
    switch expr {
      case .error(let err):
        return Expr.makeList(Exprs(err.irritants), append: .null)
      default:
        throw RuntimeError.type(expr, expected: [.errorType])
    }
  }
  
  private func isErrorObject(expr: Expr) -> Expr {
    switch expr {
      case .error(_):
        return .true
      default:
        return .false
    }
  }

  private func isReadError(expr: Expr) -> Expr {
    guard case .error(let err) = expr else {
      return .false
    }
    return err.descriptor.isReadError ? .true : .false
  }

  private func isFileError(expr: Expr) -> Expr {
    guard case .error(let err) = expr else {
      return .false
    }
    return err.descriptor.isFileError ? .true : .false
  }
  
  private func triggerExit(args: Arguments) throws -> (Procedure, Exprs) {
    guard args.count == 2 else {
      throw RuntimeError.argumentCount(min: 2, max: 2, args: .makeList(args))
    }
    let exit = try args.first!.asProcedure()
    let obj: Expr
    switch args[args.startIndex + 1] {
      case .null:
        obj = .true
      case .pair(let expr, .null):
        obj = expr
      default:
        throw RuntimeError.argumentCount(min: 1, max: 1, args: args[args.startIndex + 1])
    }
    self.context.machine.exitTriggered = true
    return (exit, [obj])
  }
  
  private func emergencyExit(expr: Expr?) -> Expr {
    self.context.delegate.emergencyExit(obj: expr)
    return .undef
  }
  
  func makeParameter(_ value: Expr, setter: Expr?) -> Expr {
    return .procedure(Procedure(setter ?? .null, value))
  }
  
  func bindParameter(_ args: Arguments) throws -> (Procedure, Exprs) {
    guard args.count == 3 else {
      throw RuntimeError.argumentCount(min: 3, max: 3, args: .makeList(args))
    }
    guard case .parameter(let tuple) = try args.first!.asProcedure().kind else {
      throw RuntimeError.type(args.first!, expected: [.parameterType])
    }
    if case .procedure(let proc) = tuple.fst {
      return (proc, [args.first!, args[args.startIndex + 1], args[args.startIndex + 2]])
    } else {
      return (try args[args.startIndex + 2].asProcedure(), [args.first!, args[args.startIndex + 1]])
    }
  }
  
  func dynamicEnvironment() -> Expr {
    return .table(self.context.machine.parameters)
  }
  
  func makeDynamicEnvironment() -> Expr {
    return .table(HashTable(copy: self.context.machine.parameters, mutable: true))
  }
  
  func setDynamicEnvironment(_ expr: Expr) throws -> Expr {
    self.context.machine.parameters = try expr.asHashTable()
    return .void
  }
}

