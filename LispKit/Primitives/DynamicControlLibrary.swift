//
//  ContinuationLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 14/07/2016.
//  Copyright © 2016 ObjectHub. All rights reserved.
//

import Foundation


public final class DynamicControlLibrary: Library {
  
  public override func export() {
    define(Procedure("continuation?", isContinuation))
    define(Procedure("_call-with-unprotected-continuation", callWithUnprotectedContinuation))
    define(Procedure("_wind-down", windDown))
    define(Procedure("_wind-up", windUp))
    define(Procedure("_dynamic-wind-base", dynamicWindBase))
    define(Procedure("_dynamic-wind-current", dynamicWindCurrent))
    define(Procedure("_dynamic-winders", dynamicWinders))
    define("call-with-current-continuation",
           compile: "(lambda (f)" +
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
    define("dynamic-wind",
           compile: "(lambda (before during after)" +
            "  (before)" +
            "  (_wind-up before after)" +
      "  (let ((res (during))) ((cdr (_wind-down))) res))")
  }
  
  func isContinuation(expr: Expr) -> Expr {
    guard case .Proc(let proc) = expr else {
      return .False
    }
    guard case .Continuation(_) = proc.kind else {
      return .False
    }
    return .True
  }
  
  func callWithUnprotectedContinuation(args: Arguments) throws -> (Procedure, [Expr]) {
    guard args.count == 1 else {
      throw EvalError.ArgumentCountError(formals: 1, args: .List(args))
    }
    guard case .Proc(let proc) = args.first! else {
      throw EvalError.TypeError(args.first!, [.ProcedureType])
    }
    // Create continuation, removing current argument and the call/cc procedure from the
    // stack of the continuation
    let vmstate = self.context.machine.getState()
    let cont = Procedure(vmstate)
    // Return procedure to call with continuation as argument
    return (proc, [.Proc(cont)])
  }
  
  func windUp(before: Expr, after: Expr) throws -> Expr {
    self.context.machine.windUp(before: try before.asProc(), after: try after.asProc())
    return .Void
  }
  
  func windDown() -> Expr {
    guard let winder = self.context.machine.windDown() else {
      return .Null
    }
    return .Pair(.Proc(winder.before), .Proc(winder.after))
  }
  
  func dynamicWindBase(cont: Expr) throws -> Expr {
    guard case .Continuation(let vmState) = try cont.asProc().kind else {
      preconditionFailure("_dynamic-wind-base(\(cont))")
    }
    let base = self.context.machine.winders?.commonPrefix(vmState.winders)
    return .Fixnum(base?.id ?? 0)
  }
  
  func dynamicWindCurrent() -> Expr {
    return .Fixnum(self.context.machine.winders?.id ?? 0)
  }
  
  func dynamicWinders(cont: Expr) throws -> Expr {
    guard case .Continuation(let vmState) = try cont.asProc().kind else {
      preconditionFailure("_dynamic-winders(\(cont))")
    }
    let base = self.context.machine.winders
    var res: Expr = .Null
    var next = vmState.winders
    while let winder = next where (base == nil) || winder !== base! {
      res = .Pair(.Pair(.Proc(winder.before), .Proc(winder.after)), res)
      next = winder.next
    }
    return res
  }
}
