//
//  ContinuationLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 14/07/2016.
//  Copyright © 2016 ObjectHub. All rights reserved.
//

import Foundation


public final class DynamicControlLibrary: NativeLibrary {
  
  public override func export() {
    // Continuations
    define(Procedure("continuation?", isContinuation))
    define(Procedure("_call-with-unprotected-continuation", callWithUnprotectedContinuation))
    define(Procedure("_wind-down", windDown))
    define(Procedure("_wind-up", windUp))
    define(Procedure("_dynamic-wind-base", dynamicWindBase))
    define(Procedure("_dynamic-wind-current", dynamicWindCurrent))
    define(Procedure("_dynamic-winders", dynamicWinders))
    define("call-with-current-continuation", compile:
      "(lambda (f)" +
      "  (_call-with-unprotected-continuation" +
      "     (lambda (cont)" +
      "       (f (lambda (x)" +
      "            (do ((base (_dynamic-wind-base cont)))" +
      "                ((eqv? (_dynamic-wind-current) base))" +
      "              ((cdr (_wind-down))))" +
      "            (do ((winders (_dynamic-winders cont) (cdr winders)))" +
      "                ((null? winders) (cont x))" +
      "              ((car (car winders)))" +
      "              (_wind-up (car (car winders)) (cdr (car winders)))))))))")
    define("dynamic-wind", compile:
      "(lambda (before during after)" +
      "  (before)" +
      "  (_wind-up before after)" +
      "  (let ((res (during))) ((cdr (_wind-down))) res))")
    
    // Parameters
    define(Procedure("dynamic-environment", dynamicEnvironment))
    define(Procedure("make-dynamic-environment", makeDynamicEnvironment))
    define(Procedure("set-dynamic-environment!", setDynamicEnvironment))
    define(Procedure("_make-parameter", makeParameter))
    define(Procedure("_bind-parameter", bindParameter))
    define("_dynamic-bind", compile:
      "(lambda (parameters values body)" +
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
    define("make-parameter", compile:
      "(lambda (val . conv)" +
      "  (if (null? conv)" +
      "    (_make-parameter val)" +
      "    (_make-parameter" +
      "      ((car conv) val)" +
      "      (lambda (param val setter) (setter param ((car conv) val))))))")
    define("parameterize", syntax:
      "(syntax-rules ()" +
      "  ((parameterize ((expr1 expr2) ...) body ...)" +
      "    (_dynamic-bind (list expr1 ...) (list expr2 ...) (lambda () body ...))))")
  }
  
  func isContinuation(_ expr: Expr) -> Expr {
    guard case .procedure(let proc) = expr else {
      return .false
    }
    guard case .continuation(_) = proc.kind else {
      return .false
    }
    return .true
  }
  
  func callWithUnprotectedContinuation(_ args: Arguments) throws -> (Procedure, [Expr]) {
    guard args.count == 1 else {
      throw EvalError.argumentCountError(formals: 1, args: .makeList(args))
    }
    guard case .procedure(let proc) = args.first! else {
      throw EvalError.typeError(args.first!, [.procedureType])
    }
    // Create continuation, removing current argument and the call/cc procedure from the
    // stack of the continuation
    let vmstate = self.context.machine.getState()
    let cont = Procedure(vmstate)
    // Return procedure to call with continuation as argument
    return (proc, [.procedure(cont)])
  }
  
  func windUp(_ before: Expr, after: Expr) throws -> Expr {
    self.context.machine.windUp(before: try before.asProc(), after: try after.asProc())
    return .void
  }
  
  func windDown() -> Expr {
    guard let winder = self.context.machine.windDown() else {
      return .null
    }
    return .pair(.procedure(winder.before), .procedure(winder.after))
  }
  
  func dynamicWindBase(_ cont: Expr) throws -> Expr {
    guard case .continuation(let vmState) = try cont.asProc().kind else {
      preconditionFailure("_dynamic-wind-base(\(cont))")
    }
    let base = self.context.machine.winders?.commonPrefix(vmState.winders)
    return .fixnum(base?.id ?? 0)
  }
  
  func dynamicWindCurrent() -> Expr {
    return .fixnum(self.context.machine.winders?.id ?? 0)
  }
  
  func dynamicWinders(_ cont: Expr) throws -> Expr {
    guard case .continuation(let vmState) = try cont.asProc().kind else {
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
  
  func makeParameter(_ value: Expr, setter: Expr?) -> Expr {
    return .procedure(Procedure(setter ?? .null, value))
  }
  
  func bindParameter(_ args: Arguments) throws -> (Procedure, [Expr]) {
    guard args.count == 3 else {
      throw EvalError.argumentCountError(formals: 3, args: .makeList(args))
    }
    guard case .parameter(let tuple) = try args.first!.asProc().kind else {
      throw EvalError.typeError(args.first!, [.parameterType])
    }
    if case .procedure(let proc) = tuple.fst {
      return (proc, [args.first!, args[args.startIndex + 1], args[args.startIndex + 2]])
    } else {
      return (try args[args.startIndex + 2].asProc(), [args.first!, args[args.startIndex + 1]])
    }
  }
  
  func dynamicEnvironment() -> Expr {
    return .table(self.context.machine.parameters)
  }
  
  func makeDynamicEnvironment() -> Expr {
    return .table(HashTable(copy: self.context.machine.parameters, mutable: true))
  }
  
  func setDynamicEnvironment(_ expr: Expr) throws -> Expr {
    self.context.machine.parameters = try expr.asMap()
    return .void
  }
}
