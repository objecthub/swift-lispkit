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

import Foundation

public final class DynamicControlLibrary: NativeLibrary {
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "dynamic"]
  }
  
  /// Declarations of the library.
  public override func declarations() {
    // Continuations
    self.define(Procedure("continuation?", isContinuation))
    self.define(Procedure("_call-with-unprotected-continuation", callWithUnprotectedContinuation))
    self.define(Procedure("_wind-down", windDown))
    self.define(Procedure("_wind-up", windUp))
    self.define(Procedure("_dynamic-wind-base", dynamicWindBase))
    self.define(Procedure("_dynamic-wind-current", dynamicWindCurrent))
    self.define(Procedure("_dynamic-winders", dynamicWinders))
    self.define("call-with-current-continuation", via:
      "(define (call-with-current-continuation f)",
      "  (_call-with-unprotected-continuation",
      "     (lambda (cont)",
      "       (f (lambda (x)",
      "            (do ((base (_dynamic-wind-base cont)))",
      "                ((eqv? (_dynamic-wind-current) base))",
      "              ((cdr (_wind-down))))",
      "            (do ((winders (_dynamic-winders cont) (cdr winders)))",
      "                ((null? winders) (cont x))",
      "              ((car (car winders)))",
      "              (_wind-up (car (car winders)) (cdr (car winders)))))))))")
    self.define("dynamic-wind", via:
      "(define (dynamic-wind before during after)",
      "  (before)",
      "  (_wind-up before after)",
      "  (let ((res (during))) ((cdr (_wind-down))) res))")
    
    // Parameters
    define(Procedure("dynamic-environment", dynamicEnvironment))
    define(Procedure("make-dynamic-environment", makeDynamicEnvironment))
    define(Procedure("set-dynamic-environment!", setDynamicEnvironment))
    define(Procedure("_make-parameter", makeParameter))
    define(Procedure("_bind-parameter", bindParameter))
    define("_dynamic-bind", via:
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
    define("make-parameter", via:
      "(define (make-parameter val . conv)" +
      "  (if (null? conv)" +
      "    (_make-parameter val)" +
      "    (_make-parameter" +
      "      ((car conv) val)" +
      "      (lambda (param val setter) (setter param ((car conv) val))))))")
    define("parameterize", via:
      "(define-syntax parameterize" +
      "  (syntax-rules ()" +
      "    ((parameterize ((expr1 expr2) ...) body ...)" +
      "      (_dynamic-bind (list expr1 ...) (list expr2 ...) (lambda () body ...))))")
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
    self.context.machine.windUp(before: try before.asProcedure(), after: try after.asProcedure())
    return .void
  }
  
  func windDown() -> Expr {
    guard let winder = self.context.machine.windDown() else {
      return .null
    }
    return .pair(.procedure(winder.before), .procedure(winder.after))
  }
  
  func dynamicWindBase(_ cont: Expr) throws -> Expr {
    guard case .continuation(let vmState) = try cont.asProcedure().kind else {
      preconditionFailure("_dynamic-wind-base(\(cont))")
    }
    let base = self.context.machine.winders?.commonPrefix(vmState.winders)
    return .fixnum(base?.id ?? 0)
  }
  
  func dynamicWindCurrent() -> Expr {
    return .fixnum(self.context.machine.winders?.id ?? 0)
  }
  
  func dynamicWinders(_ cont: Expr) throws -> Expr {
    guard case .continuation(let vmState) = try cont.asProcedure().kind else {
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
    guard case .parameter(let tuple) = try args.first!.asProcedure().kind else {
      throw EvalError.typeError(args.first!, [.parameterType])
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
