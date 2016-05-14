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


public final class ListLibrary: Library {
  
  public override func export() {
    define("pair?", Procedure(isPair))
    define("list?", Procedure(isList))
    define("null?", Procedure(isNull))
    define("list", Procedure(list, compileList))
    define("cons", Procedure(cons, compileCons))
    define("car", Procedure(ListLibrary.car))
    define("cdr", Procedure(cdr))
    define("length", Procedure(length))
    define("append", Procedure(append))
    define("reverse", Procedure(reverse))
    define("list-tail", Procedure(listTail))
    define("map", Procedure(map))
    define("for-each", Procedure(forEach))
    define("memq", Procedure(memq))
    define("memv", Procedure(memv))
    define("member", Procedure(member))
    define("assq", Procedure(assq))
    define("assv", Procedure(assv))
    define("assoc", Procedure(assoc))
  }
  
  //---------MARK: - List predicates
  
  func isPair(expr: Expr) -> Expr {
    if case .Pair(_, _) = expr {
      return .True
    }
    return .False
  }
  
  func isList(expr: Expr) -> Expr {
    var expr = expr
    while case .Pair(_, let cdr) = expr {
      expr = cdr
    }
    return Expr.Boolean(expr.isNull)
  }
  
  func isNull(expr: Expr) -> Expr {
    return Expr.Boolean(expr.isNull)
  }
  
  //-------- MARK: - Construction and deconstruction
  
  func list(exprs: Arguments) -> Expr {
    var res = Expr.Null
    for expr in exprs.reverse() {
      res = .Pair(expr, res)
    }
    return res
  }
  
  func compileList(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .Pair(_, let cdr) = expr else {
      preconditionFailure()
    }
    switch try compiler.compileExprs(cdr, in: env) {
      case 0:
        compiler.emit(.PushNull)
      case 1:
        compiler.emit(.PushNull)
        compiler.emit(.Cons)
      case let n:
        compiler.emit(.List(n))
    }
    return false
  }
  
  func cons(car: Expr, _ cdr: Expr) -> Expr {
    return .Pair(car, cdr)
  }
  
  func compileCons(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .Pair(_, .Pair(let head, .Pair(let tail, .Null))) = expr else {
      throw EvalError.ArgumentCountError(formals: 2, args: expr)
    }
    try compiler.compile(head, in: env, inTailPos: false)
    try compiler.compile(tail, in: env, inTailPos: false)
    compiler.emit(.Cons)
    return false
  }
  
  static func car(expr: Expr) throws -> Expr {
    guard case .Pair(let car, _) = expr else {
      throw EvalError.TypeError(expr, [.PairType])
    }
    return car
  }
  
  func cdr(expr: Expr) throws -> Expr {
    guard case .Pair(_, let cdr) = expr else {
      throw EvalError.TypeError(expr, [.PairType])
    }
    return cdr
  }
  
  //-------- MARK: - Basic list functions
  
  func length(expr: Expr) throws -> Expr {
    var expr = expr
    var len: Int64 = 0
    while case .Pair(_, let cdr) = expr {
      len += 1
      expr = cdr
    }
    guard expr.isNull else {
      throw EvalError.TypeError(expr, [.ProperListType])
    }
    return .Fixnum(len)
  }
  
  func appendList(fst: Expr, _ tail: Expr) throws -> Expr {
    guard case (let exprs, .Null) = fst.toExprs() else {
      throw EvalError.TypeError(fst, [.ProperListType])
    }
    return Expr.List(exprs, append: tail)
  }
  
  func append(exprs: Arguments) throws -> Expr {
    var res: Expr? = nil
    for expr in exprs.reverse() {
      res = (res == nil) ? expr : try appendList(expr, res!)
    }
    return res ?? .Null
  }
  
  func reverse(expr: Expr) throws -> Expr {
    var res = Expr.Null
    var list = expr
    while case .Pair(let car, let cdr) = list {
      res = .Pair(car, res)
      list = cdr
    }
    guard list.isNull else {
      throw EvalError.TypeError(expr, [.ProperListType])
    }
    return res
  }
  
  func listTail(expr: Expr, _ count: Expr) throws -> Expr {
    var k = try count.asInteger()
    guard k >= 0 else {
      throw EvalError.IndexOutOfBounds(k, -1, expr)
    }
    if k == 0 {
      return expr
    } else {
      var list = expr
      var len: Int64 = 0
      while case .Pair(_, let cdr) = list {
        k -= 1
        if k == 0 {
          return cdr
        }
        len += 1
        list = cdr
      }
      throw EvalError.IndexOutOfBounds(try count.asInteger(), len, expr)
    }
  }
  
  func map(fun: Expr, _ expr: Expr, _ arglists: Arguments) throws -> Expr {
    var argFor = Exprs()
    argFor.append(expr)
    argFor.appendContentsOf(arglists)
    var res = Exprs()
    while true {
      var args = Expr.Null
      for i in 1...argFor.count {
        if case .Pair(let car, let cdr) = argFor[argFor.count - i] {
          args = .Pair(car, args)
          argFor[argFor.count - i] = cdr
        } else {
          return Expr.List(res)
        }
      }
      res.append(try context.machine.apply(fun, to: args))
    }
  }
  
  func forEach(fun: Expr, _ expr: Expr, _ arglists: Arguments) throws -> Expr {
    var argFor = Exprs()
    argFor.append(expr)
    argFor.appendContentsOf(arglists)
    while true {
      var args = Expr.Null
      for i in 1...argFor.count {
        if case .Pair(let car, let cdr) = argFor[argFor.count - i] {
          args = .Pair(car, args)
          argFor[argFor.count - i] = cdr
        } else {
          return .Void
        }
      }
      try context.machine.apply(fun, to: args)
    }
  }
  
  func memq(obj: Expr, expr: Expr) throws -> Expr {
    var list = expr
    while case .Pair(let car, let cdr) = list {
      if eqExpr(obj, car) {
        return list
      }
      list = cdr
    }
    guard list.isNull else {
      throw EvalError.TypeError(expr, [.ProperListType])
    }
    return .False
  }
  
  func memv(obj: Expr, expr: Expr) throws -> Expr {
    var list = expr
    while case .Pair(let car, let cdr) = list {
      if eqvExpr(obj, car) {
        return list
      }
      list = cdr
    }
    guard list.isNull else {
      throw EvalError.TypeError(expr, [.ProperListType])
    }
    return .False
  }
  
  func member(obj: Expr, expr: Expr) throws -> Expr {
    var list = expr
    while case .Pair(let car, let cdr) = list {
      if equalExpr(obj, car) {
        return list
      }
      list = cdr
    }
    guard list.isNull else {
      throw EvalError.TypeError(expr, [.ProperListType])
    }
    return .False
  }
  
  func assq(obj: Expr, expr: Expr) throws -> Expr {
    var list = expr
    while case .Pair(.Pair(let key, let value), let cdr) = list {
      if eqExpr(obj, key) {
        return .Pair(key, value)
      }
      list = cdr
    }
    guard list.isNull else {
      throw EvalError.TypeError(expr, [.AssocListType])
    }
    return .False
  }
  
  func assv(obj: Expr, expr: Expr) throws -> Expr {
    var list = expr
    while case .Pair(.Pair(let key, let value), let cdr) = list {
      if eqvExpr(obj, key) {
        return .Pair(key, value)
      }
      list = cdr
    }
    guard list.isNull else {
      throw EvalError.TypeError(expr, [.AssocListType])
    }
    return .False
  }

  func assoc(obj: Expr, expr: Expr) throws -> Expr {
    var list = expr
    while case .Pair(.Pair(let key, let value), let cdr) = list {
      if equalExpr(obj, key) {
        return .Pair(key, value)
      }
      list = cdr
    }
    guard list.isNull else {
      throw EvalError.TypeError(expr, [.AssocListType])
    }
    return .False
  }
}

  