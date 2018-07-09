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

import Foundation

///
/// List library: based on R7RS spec, but with one big difference: cons cells are not
/// mutable in LispKit. Thus, functionality for mutating and copying lists is missing.
/// Mutable cons cells created via `mcons` can be used as a replacement for mutable lists.
/// `mcons` is based on Racket (see BoxLibrary.swift).
///
public final class ListLibrary: NativeLibrary {
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "list"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"],    "define", "apply", "quote", "and", "or",
                                                "not", "equal?", "lambda", "identity", "set!",
                                                "values")
    self.`import`(from: ["lispkit", "control"], "if", "cond", "let", "do")
    self.`import`(from: ["lispkit", "math"],    "+", "-", "*", "=", "truncate-quotient",
                                                "fxnegative?", "fx1-")
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("pair?", isPair, compileIsPair))
    self.define(Procedure("list?", isList))
    self.define(Procedure("null?", isNull, compileIsNull))
    self.define(Procedure("make-list", makeList))
    self.define(Procedure("list", list, compileList))
    self.define(Procedure("cons", cons, compileCons))
    self.define(Procedure("cons*", consStar))
    self.define(Procedure("car", car, compileCar))
    self.define(Procedure("cdr", cdr, compileCdr))
    self.define(Procedure("caar", caar, compileCaar))
    self.define(Procedure("cadr", cadr, compileCadr))
    self.define(Procedure("cdar", cdar, compileCdar))
    self.define(Procedure("cddr", cddr, compileCddr))
    self.define("caaar", via: "(define (caaar x) (car (caar x)))")
    self.define("caadr", via: "(define (caadr x) (car (cadr x)))")
    self.define("cadar", via: "(define (cadar x) (car (cdar x)))")
    self.define("caddr", via: "(define (caddr x) (car (cddr x)))")
    self.define("cdaar", via: "(define (cdaar x) (cdr (caar x)))")
    self.define("cdadr", via: "(define (cdadr x) (cdr (cadr x)))")
    self.define("cddar", via: "(define (cddar x) (cdr (cdar x)))")
    self.define("cdddr", via: "(define (cdddr x) (cdr (cddr x)))")
    self.define("caaaar", via: "(define (caaaar x) (caar (caar x)))")
    self.define("caaadr", via: "(define (caaadr x) (caar (cadr x)))")
    self.define("caadar", via: "(define (caadar x) (caar (cdar x)))")
    self.define("caaddr", via: "(define (caaddr x) (caar (cddr x)))")
    self.define("cadaar", via: "(define (cadaar x) (cadr (caar x)))")
    self.define("cadadr", via: "(define (cadadr x) (cadr (cadr x)))")
    self.define("caddar", via: "(define (caddar x) (cadr (cdar x)))")
    self.define("cadddr", via: "(define (cadddr x) (cadr (cddr x)))")
    self.define("cdaaar", via: "(define (cdaaar x) (cdar (caar x)))")
    self.define("cdaadr", via: "(define (cdaadr x) (cdar (cadr x)))")
    self.define("cdadar", via: "(define (cdadar x) (cdar (cdar x)))")
    self.define("cdaddr", via: "(define (cdaddr x) (cdar (cddr x)))")
    self.define("cddaar", via: "(define (cddaar x) (cddr (caar x)))")
    self.define("cddadr", via: "(define (cddadr x) (cddr (cadr x)))")
    self.define("cdddar", via: "(define (cdddar x) (cddr (cdar x)))")
    self.define("cddddr", via: "(define (cddddr x) (cddr (cddr x)))")
    self.define(Procedure("length", length))
    self.define(Procedure("append", append))
    self.define(Procedure("concatenate", concatenate))
    self.define(Procedure("reverse", reverse))
    self.define(Procedure("list-tail", listTail))
    self.define(Procedure("decons", decons), export: false)
    self.define("list-ref", via: "(define (list-ref x k) (car (list-tail x k)))")
    self.define("member", via:
      "(define (member x list . comp)",
      "  (let ((eq (if (pair? comp) (car comp) equal?)))",
      "    (let lp ((ls list))",
      "      (and (pair? ls) (if (eq x (car ls)) ls (lp (cdr ls)))))))")
    self.define("_assoc", via:
      "(define (_assoc x ls eq)",
      "  (cond ((null? ls)       #f)",
      "        ((eq x (caar ls)) (car ls))",
      "        (else             (_assoc x (cdr ls) eq))))")
    self.define("assoc", via:
      "(define (assoc x list . comp)",
      "  (_assoc x list (if (pair? comp) (car comp) equal?)))")
    self.define("delete", via:
      "(define (delete x list . comp)",
      "  (let ((eq (if (pair? comp) (car comp) equal?)))",
      "    (let lp ((ls list))",
      "      (if (pair? ls)",
      "          (if (eq x (car ls)) (lp (cdr ls)) (cons (car ls) (lp (cdr ls))))",
      "          ls))))")
    self.define("alist-delete", via:
      "(define (alist-delete x list . comp)",
      "  (let ((eq (if (pair? comp) (car comp) equal?)))",
      "    (let lp ((ls list))",
      "      (if (pair? ls)",
      "          (if (eq x (caar ls)) (lp (cdr ls)) (cons (car ls) (lp (cdr ls))))",
      "          ls))))")
    self.define("map", via:
      "(define (map f xs . xss)",
      "  (if (null? xss)",
      "      (do ((res '() (cons (f (car xs)) res))",
      "           (xs xs (cdr xs)))",
      "          ((null? xs) (reverse res)))",
      "      (do ((res '() (cons (apply f (car pair)) res))",
      "           (pair (decons (cons xs xss)) (decons (cdr pair))))",
      "          ((null? pair) (reverse res)))))")
    self.define("for-each", via:
      "(define (for-each f list1 . lists)",
      "  (do ((pair (decons (cons list1 lists)) (decons (cdr pair))))",
      "      ((null? pair))",
      "        (apply f (car pair))))")
    self.define(Procedure("key", key))
    self.define(Procedure("value", value))
    self.define(Procedure("memq", memq))
    self.define(Procedure("memv", memv))
    self.define(Procedure("assq", assq))
    self.define(Procedure("assv", assv))
    self.define(Procedure("delq", delq))
    self.define(Procedure("delv", delv))
    self.define(Procedure("alist-delq", alistDelq))
    self.define(Procedure("alist-delv", alistDelv))
    self.define("merge", via:
      "(define (merge pred l1 l2)",
      "  (cond ((null? l1)               l2)",
      "        ((null? l2)               l1)",
      "        ((pred (car l2) (car l1)) (cons (car l2) (merge pred l1 (cdr l2))))",
      "        (else                     (cons (car l1) (merge pred (cdr l1) l2)))))")
    self.define("sort", via:
      "(define (sort pred l)",
      "  (if (null? l)",
      "      l",
      "      (let isort ((ls l) (n (length l)))",
      "        (if (= n 1)",
      "            (list (car ls))",
      "            (let ((i (truncate-quotient n 2)))",
      "              (merge pred (isort ls i) (isort (list-tail ls i) (- n i))))))))")
    self.define("fold-left", via:
      "(define (fold-left f z xs . xss)",
      "  (if (null? xss)",
      "      (do ((acc z (f (car xs) acc))",
      "           (xs xs (cdr xs)))",
      "          ((null? xs) acc))",
      "      (do ((acc z (apply f (append (car pair) (list acc))))",
      "           (pair (decons (cons xs xss)) (decons (cdr pair))))",
      "          ((null? pair) acc))))")
    self.define("fold-right", via:
      "(define (fold-right f z xs . xss)",
      "  (if (null? xss)",
      "      (let rec ((cont identity) (xs xs))",
      "        (if (null? xs)",
      "            (cont z)",
      "            (rec (lambda (acc) (cont (f (car xs) acc))) (cdr xs))))",
      "      (let rec ((cont identity) (pair (decons (cons xs xss))))",
      "        (if (null? pair)",
      "            (cont z)",
      "            (rec (lambda (acc) (cont (apply f (append (car pair) (list acc)))))",
      "                 (decons (cdr pair)))))))")
    self.define("every?", via:
      "(define (every? pred xs . xss)",
      "  (if (null? xss)",
      "      (do ((xs xs (cdr xs)))",
      "          ((or (null? xs) (not (pred (car xs)))) (null? xs)))",
      "      (do ((pair (decons (cons xs xss)) (decons (cdr pair))))",
      "          ((or (null? pair) (not (apply pred (car pair)))) (null? pair)))))")
    self.define("any?", via:
      "(define (any? pred xs . xss)",
      "  (if (null? xss)",
      "      (do ((xs xs (cdr xs)))",
      "          ((or (null? xs) (pred (car xs))) (not (null? xs))))",
      "      (do ((pair (decons (cons xs xss)) (decons (cdr pair))))",
      "          ((or (null? pair) (apply pred (car pair))) (not (null? pair))))))")
    self.define("filter", via:
      "(define (filter pred xs)",
      "  (do ((xs xs (cdr xs))",
      "       (res '() (if (pred (car xs)) (cons (car xs) res) res)))",
      "      ((null? xs) (reverse res))))")
    self.define("partition", via:
      "(define (partition pred xs)",
      "  (do ((xs xs (cdr xs)) (ys '()) (zs '()))",
      "      ((null? xs) (values (reverse ys) (reverse zs)))",
      "    (if (pred (car xs)) (set! ys (cons (car xs) ys)) (set! zs (cons (car xs) zs)))))")
    self.define("tabulate", via:
      "(define (tabulate count proc)",
      "  (do ((count (fx1- count) (fx1- count))",
      "       (acc   '() (cons (proc count) acc)))",
      "      ((fxnegative? count) acc)))")
    self.define("iota", via:
      "(define (iota count . lst)",
      "  (let ((start (if (pair? lst) (car lst) 0))",
      "        (step  (if (and (pair? lst) (pair? (cdr lst))) (cadr lst) 1)))",
      "    (do ((count (fx1- count) (fx1- count))",
      "         (acc   '() (cons (+ start (* count step)) acc)))",
      "        ((fxnegative? count) acc))))")
  }
  
  //---------MARK: - List predicates
  
  private func isPair(_ expr: Expr) -> Expr {
    if case .pair(_, _) = expr {
      return .true
    }
    return .false
  }
  
  private func compileIsPair(_ compiler: Compiler,
                             expr: Expr,
                             env: Env,
                             tail: Bool) throws -> Bool {
    return try self.invoke(.isPair, with: expr, in: env, for: compiler)
  }
  
  private func isList(_ expr: Expr) -> Expr {
    var expr = expr
    while case .pair(_, let cdr) = expr {
      expr = cdr
    }
    return Expr.makeBoolean(expr.isNull)
  }
  
  private func isNull(_ expr: Expr) -> Expr {
    return Expr.makeBoolean(expr.isNull)
  }
  
  private func compileIsNull(_ compiler: Compiler,
                             expr: Expr,
                             env: Env,
                             tail: Bool) throws -> Bool {
    return try self.invoke(.isNull, with: expr, in: env, for: compiler)
  }
  
  //-------- MARK: - Construction and deconstruction
  
  private func makeList(_ k: Expr, expr: Expr?) throws -> Expr {
    let def = expr ?? .null
    var res: Expr = .null
    var n = try k.asInt(below: Int.max)
    while n > 0 {
      res = .pair(def, res)
      n -= 1
    }
    return res
  }
  
  private func list(_ exprs: Arguments) -> Expr {
    var res = Expr.null
    for expr in exprs.reversed() {
      res = .pair(expr, res)
    }
    return res
  }
  
  private func compileList(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
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
  
  private func cons(_ car: Expr, _ cdr: Expr) -> Expr {
    return .pair(car, cdr)
  }
  
  private func compileCons(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let head, .pair(let tail, .null))) = expr else {
      throw RuntimeError.argumentCount(of: "cons", min: 2, max: 2, expr: expr)
    }
    _ = try compiler.compile(head, in: env, inTailPos: false)
    _ = try compiler.compile(tail, in: env, inTailPos: false)
    compiler.emit(.cons)
    return false
  }
  
  private func consStar(_ expr: Expr, _ args: Arguments) -> Expr {
    var res: Expr? = nil
    for arg in args.reversed() {
      res = res == nil ? arg : Expr.pair(arg, res!)
    }
    return res == nil ? expr : Expr.pair(expr, res!)
  }
  
  private func car(_ expr: Expr) throws -> Expr {
    guard case .pair(let car, _) = expr else {
      throw RuntimeError.type(expr, expected: [.pairType])
    }
    return car
  }
  
  private func compileCar(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    return try self.invoke(.car, with: expr, in: env, for: compiler)
  }
  
  private func cdr(_ expr: Expr) throws -> Expr {
    guard case .pair(_, let cdr) = expr else {
      throw RuntimeError.type(expr, expected: [.pairType])
    }
    return cdr
  }
  
  private func compileCdr(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    return try self.invoke(.cdr, with: expr, in: env, for: compiler)
  }
  
  private func caar(_ expr: Expr) throws -> Expr {
    guard case .pair(let car, _) = expr else {
      throw RuntimeError.type(expr, expected: [.pairType])
    }
    guard case .pair(let caar, _) = car else {
      throw RuntimeError.type(car, expected: [.pairType])
    }
    return caar
  }
  
  private func compileCaar(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    return try self.invoke(.car, .car, with: expr, in: env, for: compiler)
  }
  
  private func cadr(_ expr: Expr) throws -> Expr {
    guard case .pair(_, let cdr) = expr else {
      throw RuntimeError.type(expr, expected: [.pairType])
    }
    guard case .pair(let cadr, _) = cdr else {
      throw RuntimeError.type(cdr, expected: [.pairType])
    }
    return cadr
  }
  
  private func compileCadr(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    return try self.invoke(.cdr, .car, with: expr, in: env, for: compiler)
  }

  private func cdar(_ expr: Expr) throws -> Expr {
    guard case .pair(let car, _) = expr else {
      throw RuntimeError.type(expr, expected: [.pairType])
    }
    guard case .pair(_, let cdar) = car else {
      throw RuntimeError.type(car, expected: [.pairType])
    }
    return cdar
  }
  
  private func compileCdar(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    return try self.invoke(.car, .cdr, with: expr, in: env, for: compiler)
  }
  
  private func cddr(_ expr: Expr) throws -> Expr {
    guard case .pair(_, let cdr) = expr else {
      throw RuntimeError.type(expr, expected: [.pairType])
    }
    guard case .pair(_, let cddr) = cdr else {
      throw RuntimeError.type(cdr, expected: [.pairType])
    }
    return cddr
  }
  
  private func compileCddr(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    return try self.invoke(.cdr, .cdr, with: expr, in: env, for: compiler)
  }
  
  //-------- MARK: - Basic list functions
  
  private func length(_ expr: Expr) throws -> Expr {
    var expr = expr
    var len: Int64 = 0
    while case .pair(_, let cdr) = expr {
      len += 1
      expr = cdr
    }
    guard expr.isNull else {
      throw RuntimeError.type(expr, expected: [.properListType])
    }
    return .fixnum(len)
  }
  
  private func appendList(_ fst: Expr, _ tail: Expr) throws -> Expr {
    guard case (let exprs, .null) = fst.toExprs() else {
      throw RuntimeError.type(fst, expected: [.properListType])
    }
    return Expr.makeList(exprs, append: tail)
  }
  
  private func append(_ exprs: Arguments) throws -> Expr {
    var res: Expr? = nil
    for expr in exprs.reversed() {
      res = (res == nil) ? expr : try appendList(expr, res!)
    }
    return res ?? .null
  }
  
  private func concatenate(_ expr: Expr) throws -> Expr {
    var res: Expr? = nil
    var lists = try self.reverse(expr)
    while case .pair(let list, let next) = lists {
      res = (res == nil) ? list : try appendList(list, res!)
      lists = next
    }
    return res ?? .null
  }
  
  private func reverse(_ expr: Expr) throws -> Expr {
    var res = Expr.null
    var list = expr
    while case .pair(let car, let cdr) = list {
      res = .pair(car, res)
      list = cdr
    }
    guard list.isNull else {
      throw RuntimeError.type(expr, expected: [.properListType])
    }
    return res
  }
  
  private func listTail(_ expr: Expr, _ count: Expr) throws -> Expr {
    var k = try count.asInt64()
    guard k >= 0 else {
      throw RuntimeError.range(count, min: 0, max: Int64.max)
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
      throw RuntimeError.range(count, min: 0, max: len)
    }
  }
  
  private func key(_ expr: Expr, def: Expr?) throws -> Expr {
    guard case .pair(let car, _) = expr else {
      return def ?? .false
    }
    return car
  }
  
  private func value(_ expr: Expr, def: Expr?) throws -> Expr {
    guard case .pair(_, let cdr) = expr else {
      return def ?? .false
    }
    return cdr
  }
  
  private func memq(_ obj: Expr, expr: Expr) throws -> Expr {
    var list = expr
    while case .pair(let car, let cdr) = list {
      if eqExpr(obj, car) {
        return list
      }
      list = cdr
    }
    guard list.isNull else {
      throw RuntimeError.type(expr, expected: [.properListType])
    }
    return .false
  }
  
  private func memv(_ obj: Expr, expr: Expr) throws -> Expr {
    var list = expr
    while case .pair(let car, let cdr) = list {
      if eqvExpr(obj, car) {
        return list
      }
      list = cdr
    }
    guard list.isNull else {
      throw RuntimeError.type(expr, expected: [.properListType])
    }
    return .false
  }
  
  private func assq(_ obj: Expr, expr: Expr) throws -> Expr {
    var list = expr
    while case .pair(.pair(let key, let value), let cdr) = list {
      if eqExpr(obj, key) {
        return .pair(key, value)
      }
      list = cdr
    }
    guard list.isNull else {
      throw RuntimeError.type(expr, expected: [.assocListType])
    }
    return .false
  }
  
  private func assv(_ obj: Expr, expr: Expr) throws -> Expr {
    var list = expr
    while case .pair(.pair(let key, let value), let cdr) = list {
      if eqvExpr(obj, key) {
        return .pair(key, value)
      }
      list = cdr
    }
    guard list.isNull else {
      throw RuntimeError.type(expr, expected: [.assocListType])
    }
    return .false
  }
  
  private func delq(_ obj: Expr, expr: Expr) throws -> Expr {
    var list = expr
    var elems = Exprs()
    while case .pair(let car, let cdr) = list {
      if !eqExpr(obj, car) {
        elems.append(car)
      }
      list = cdr
    }
    guard list.isNull else {
      throw RuntimeError.type(expr, expected: [.properListType])
    }
    return .makeList(elems)
  }
  
  private func delv(_ obj: Expr, expr: Expr) throws -> Expr {
    var list = expr
    var elems = Exprs()
    while case .pair(let car, let cdr) = list {
      if !eqvExpr(obj, car) {
        elems.append(car)
      }
      list = cdr
    }
    guard list.isNull else {
      throw RuntimeError.type(expr, expected: [.properListType])
    }
    return .makeList(elems)
  }
  
  private func alistDelq(_ obj: Expr, expr: Expr) throws -> Expr {
    var list = expr
    var elems = Exprs()
    while case .pair(.pair(let key, let value), let cdr) = list {
      if !eqExpr(obj, key) {
        elems.append(.pair(key, value))
      }
      list = cdr
    }
    guard list.isNull else {
      throw RuntimeError.type(expr, expected: [.assocListType])
    }
    return .makeList(elems)
  }
  
  private func alistDelv(_ obj: Expr, expr: Expr) throws -> Expr {
    var list = expr
    var elems = Exprs()
    while case .pair(.pair(let key, let value), let cdr) = list {
      if !eqvExpr(obj, key) {
        elems.append(.pair(key, value))
      }
      list = cdr
    }
    guard list.isNull else {
      throw RuntimeError.type(expr, expected: [.assocListType])
    }
    return .makeList(elems)
  }
  
  private func decons(_ expr: Expr) throws -> Expr {
    guard case (let lists, .null) = expr.toExprs() else {
      throw RuntimeError.type(expr, expected: [.properListType])
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
          throw RuntimeError.type(lists[i], expected: [.properListType])
      }
    }
    return .pair(cars, cdrs)
  }
}

