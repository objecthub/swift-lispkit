//
//  ListLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 22/01/2016.
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

///
/// List library: based on R7RS spec, but with one big difference: cons cells are not
/// mutable in LispKit. Thus, functionality for mutating and copying lists is missing.
/// Mutable cons cells created via `mcons` can be used as a replacement for mutable lists.
/// `mcons` is based on Racket (see BoxLibrary.swift).
///
public final class ListLibrary: NativeLibrary {
  
  public override func export() {
    define(Procedure("pair?", isPair, compileIsPair))
    define(Procedure("list?", isList))
    define(Procedure("null?", isNull, compileIsNull))
    define(Procedure("make-list", makeList))
    define(Procedure("list", list, compileList))
    define(Procedure("cons", cons, compileCons))
    define(Procedure("car", car, compileCar))
    define(Procedure("cdr", cdr, compileCdr))
    define(Procedure("caar", caar))
    define(Procedure("cadr", cadr))
    define(Procedure("cdar", cdar))
    define(Procedure("cddr", cddr))
    define("caaar", compile: "(lambda (x) (car (caar x)))")
    define("caadr", compile: "(lambda (x) (car (cadr x)))")
    define("cadar", compile: "(lambda (x) (car (cdar x)))")
    define("caddr", compile: "(lambda (x) (car (cddr x)))")
    define("cdaar", compile: "(lambda (x) (cdr (caar x)))")
    define("cdadr", compile: "(lambda (x) (cdr (cadr x)))")
    define("cddar", compile: "(lambda (x) (cdr (cdar x)))")
    define("cdddr", compile: "(lambda (x) (cdr (cddr x)))")
    define("caaaar", compile: "(lambda (x) (caar (caar x)))")
    define("caaadr", compile: "(lambda (x) (caar (cadr x)))")
    define("caadar", compile: "(lambda (x) (caar (cdar x)))")
    define("caaddr", compile: "(lambda (x) (caar (cddr x)))")
    define("cadaar", compile: "(lambda (x) (cadr (caar x)))")
    define("cadadr", compile: "(lambda (x) (cadr (cadr x)))")
    define("caddar", compile: "(lambda (x) (cadr (cdar x)))")
    define("cadddr", compile: "(lambda (x) (cadr (cddr x)))")
    define("cdaaar", compile: "(lambda (x) (cdar (caar x)))")
    define("cdaadr", compile: "(lambda (x) (cdar (cadr x)))")
    define("cdadar", compile: "(lambda (x) (cdar (cdar x)))")
    define("cdaddr", compile: "(lambda (x) (cdar (cddr x)))")
    define("cddaar", compile: "(lambda (x) (cddr (caar x)))")
    define("cddadr", compile: "(lambda (x) (cddr (cadr x)))")
    define("cdddar", compile: "(lambda (x) (cddr (cdar x)))")
    define("cddddr", compile: "(lambda (x) (cddr (cddr x)))")
    define(Procedure("length", length))
    define(Procedure("append", append))
    define(Procedure("reverse", reverse))
    define(Procedure("list-tail", listTail))
    define("list-ref", compile: "(lambda (x k) (car (list-tail x k)))")
    define(Procedure("key", key))
    define(Procedure("value", value))
    define(Procedure("memq", memq))
    define(Procedure("memv", memv))
    define("member", compile:
      "(define (member x list . comp)" +
      "  (let ((eq (if (pair? comp) (car comp) equal?)))" +
      "    (let lp ((ls list))" +
      "      (and (pair? ls) (if (eq x (car ls)) ls (lp (cdr ls)))))))")
    define(Procedure("assq", assq))
    define(Procedure("assv", assv))
    define("assoc", compile:
      "(lambda (x list . comp) " +
      "  (let ((eq (if (pair? comp) (car comp) equal?)))" +
      "    (let assoc ((ls list))" +
      "      (cond ((null? ls) #f)" +
      "            ((eq x (caar ls)) (car ls))" +
      "            (else (assoc (cdr ls)))))))")
    define(Procedure("decons", decons))
    define("map", compile:
      "(lambda (f list1 . lists)" +
      "  (let ((res '()))" +
      "       (do ((pair (decons (cons list1 lists)) (decons (cdr pair))))" +
      "           ((null? pair) (reverse res))" +
      "           (set! res (cons (apply f (car pair)) res)))))")
    define("for-each", compile:
      "(lambda (f list1 . lists)" +
      "  (do ((pair (decons (cons list1 lists)) (decons (cdr pair))))" +
      "      ((null? pair))" +
      "        (apply f (car pair))))")
  }
  
  //---------MARK: - List predicates
  
  func isPair(_ expr: Expr) -> Expr {
    if case .pair(_, _) = expr {
      return .true
    }
    return .false
  }
  
  func compileIsPair(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    return try self.invoke(.isPair, with: expr, in: env, for: compiler)
  }
  
  func isList(_ expr: Expr) -> Expr {
    var expr = expr
    while case .pair(_, let cdr) = expr {
      expr = cdr
    }
    return Expr.makeBoolean(expr.isNull)
  }
  
  func isNull(_ expr: Expr) -> Expr {
    return Expr.makeBoolean(expr.isNull)
  }
  
  func compileIsNull(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    return try self.invoke(.isNull, with: expr, in: env, for: compiler)
  }
  
  //-------- MARK: - Construction and deconstruction
  
  func makeList(_ k: Expr, expr: Expr?) throws -> Expr {
    let def = expr ?? .null
    var res: Expr = .null
    var n = try k.asInt(below: Int.max)
    while n > 0 {
      res = .pair(def, res)
      n -= 1
    }
    return res
  }
  
  func list(_ exprs: Arguments) -> Expr {
    var res = Expr.null
    for expr in exprs.reversed() {
      res = .pair(expr, res)
    }
    return res
  }
  
  func compileList(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, let cdr) = expr else {
      preconditionFailure()
    }
    switch try compiler.compileExprs(cdr, in: env) {
      case 0:
        compiler.emit(.pushNull)
      case 1:
        compiler.emit(.pushNull)
        compiler.emit(.cons)
      case let n:
        compiler.emit(.list(n))
    }
    return false
  }
  
  func cons(_ car: Expr, _ cdr: Expr) -> Expr {
    return .pair(car, cdr)
  }
  
  func compileCons(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let head, .pair(let tail, .null))) = expr else {
      throw EvalError.argumentCountError(formals: 2, args: expr)
    }
    _ = try compiler.compile(head, in: env, inTailPos: false)
    _ = try compiler.compile(tail, in: env, inTailPos: false)
    compiler.emit(.cons)
    return false
  }
  
  func car(_ expr: Expr) throws -> Expr {
    guard case .pair(let car, _) = expr else {
      throw EvalError.typeError(expr, [.pairType])
    }
    return car
  }
  
  func compileCar(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    return try self.invoke(.car, with: expr, in: env, for: compiler)
  }
  
  func cdr(_ expr: Expr) throws -> Expr {
    guard case .pair(_, let cdr) = expr else {
      throw EvalError.typeError(expr, [.pairType])
    }
    return cdr
  }
  
  func compileCdr(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    return try self.invoke(.cdr, with: expr, in: env, for: compiler)
  }
  
  func caar(_ expr: Expr) throws -> Expr {
    guard case .pair(let car, _) = expr else {
      throw EvalError.typeError(expr, [.pairType])
    }
    guard case .pair(let caar, _) = car else {
      throw EvalError.typeError(car, [.pairType])
    }
    return caar
  }
  
  func cadr(_ expr: Expr) throws -> Expr {
    guard case .pair(_, let cdr) = expr else {
      throw EvalError.typeError(expr, [.pairType])
    }
    guard case .pair(let cadr, _) = cdr else {
      throw EvalError.typeError(cdr, [.pairType])
    }
    return cadr
  }

  func cdar(_ expr: Expr) throws -> Expr {
    guard case .pair(let car, _) = expr else {
      throw EvalError.typeError(expr, [.pairType])
    }
    guard case .pair(_, let cdar) = car else {
      throw EvalError.typeError(car, [.pairType])
    }
    return cdar
  }
  
  func cddr(_ expr: Expr) throws -> Expr {
    guard case .pair(_, let cdr) = expr else {
      throw EvalError.typeError(expr, [.pairType])
    }
    guard case .pair(_, let cddr) = cdr else {
      throw EvalError.typeError(cdr, [.pairType])
    }
    return cddr
  }

  
  //-------- MARK: - Basic list functions
  
  func length(_ expr: Expr) throws -> Expr {
    var expr = expr
    var len: Int64 = 0
    while case .pair(_, let cdr) = expr {
      len += 1
      expr = cdr
    }
    guard expr.isNull else {
      throw EvalError.typeError(expr, [.properListType])
    }
    return .fixnum(len)
  }
  
  func appendList(_ fst: Expr, _ tail: Expr) throws -> Expr {
    guard case (let exprs, .null) = fst.toExprs() else {
      throw EvalError.typeError(fst, [.properListType])
    }
    return Expr.makeList(exprs, append: tail)
  }
  
  func append(_ exprs: Arguments) throws -> Expr {
    var res: Expr? = nil
    for expr in exprs.reversed() {
      res = (res == nil) ? expr : try appendList(expr, res!)
    }
    return res ?? .null
  }
  
  func reverse(_ expr: Expr) throws -> Expr {
    var res = Expr.null
    var list = expr
    while case .pair(let car, let cdr) = list {
      res = .pair(car, res)
      list = cdr
    }
    guard list.isNull else {
      throw EvalError.typeError(expr, [.properListType])
    }
    return res
  }
  
  func listTail(_ expr: Expr, _ count: Expr) throws -> Expr {
    var k = try count.asInt64()
    guard k >= 0 else {
      throw EvalError.indexOutOfBounds(k, -1, expr)
    }
    if k == 0 {
      return expr
    } else {
      var list = expr
      var len: Int64 = 0
      while case .pair(_, let cdr) = list {
        k -= 1
        if k == 0 {
          return cdr
        }
        len += 1
        list = cdr
      }
      throw EvalError.indexOutOfBounds(try count.asInt64(), len, expr)
    }
  }
  
  func key(_ expr: Expr, def: Expr?) throws -> Expr {
    guard case .pair(let car, _) = expr else {
      return def ?? .false
    }
    return car
  }
  
  func value(_ expr: Expr, def: Expr?) throws -> Expr {
    guard case .pair(_, let cdr) = expr else {
      return def ?? .false
    }
    return cdr
  }
  
  func memq(_ obj: Expr, expr: Expr) throws -> Expr {
    var list = expr
    while case .pair(let car, let cdr) = list {
      if eqExpr(obj, car) {
        return list
      }
      list = cdr
    }
    guard list.isNull else {
      throw EvalError.typeError(expr, [.properListType])
    }
    return .false
  }
  
  func memv(_ obj: Expr, expr: Expr) throws -> Expr {
    var list = expr
    while case .pair(let car, let cdr) = list {
      if eqvExpr(obj, car) {
        return list
      }
      list = cdr
    }
    guard list.isNull else {
      throw EvalError.typeError(expr, [.properListType])
    }
    return .false
  }
  
  func assq(_ obj: Expr, expr: Expr) throws -> Expr {
    var list = expr
    while case .pair(.pair(let key, let value), let cdr) = list {
      if eqExpr(obj, key) {
        return .pair(key, value)
      }
      list = cdr
    }
    guard list.isNull else {
      throw EvalError.typeError(expr, [.assocListType])
    }
    return .false
  }
  
  func assv(_ obj: Expr, expr: Expr) throws -> Expr {
    var list = expr
    while case .pair(.pair(let key, let value), let cdr) = list {
      if eqvExpr(obj, key) {
        return .pair(key, value)
      }
      list = cdr
    }
    guard list.isNull else {
      throw EvalError.typeError(expr, [.assocListType])
    }
    return .false
  }
  
  func decons(_ expr: Expr) throws -> Expr {
    guard case (let lists, .null) = expr.toExprs() else {
      throw EvalError.typeError(expr, [.properListType])
    }
    guard lists.count > 0 else {
      return .null
    }
    var cars: Expr = .null
    var cdrs: Expr = .null
    for i in lists.indices.reversed() {
      switch lists[i] {
        case .null:
          return .null
        case .pair(let car, let cdr):
          cars = .pair(car, cars)
          cdrs = .pair(cdr, cdrs)
        default:
          throw EvalError.typeError(lists[i], [.properListType])
      }
    }
    return .pair(cars, cdrs)
  }
}
