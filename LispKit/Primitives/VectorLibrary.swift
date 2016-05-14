//
//  VectorLibrary.swift
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


public final class VectorLibrary: Library {
  
  public override func export() {
    define("vector?", Procedure(isVector, compileIsVector))
    define("vector", Procedure(vector, compileVector))
    define("make-vector", Procedure(makeVector))
    define("vector-append", Procedure(vectorAppend))
    define("vector-ref", Procedure(vectorRef))
    define("vector-set!", Procedure(vectorSet))
    define("list->vector", Procedure(listToVector))
    define("vector->list", Procedure(vectorToList))
    define("vector-fill!", Procedure(vectorFill))
  }

  //-------- MARK: - Vector primitives
  
  func isVector(expr: Expr) -> Expr {
    switch expr {
    case .Vec(_):
      return .True
    default:
      return .False
    }
  }
  
  func compileIsVector(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .Pair(_, .Pair(let arg, .Null)) = expr else {
      throw EvalError.ArgumentCountError(formals: 1, args: expr)
    }
    try compiler.compile(arg, in: env, inTailPos: false)
    compiler.emit(.IsVector)
    return false
  }
  
  func makeVector(count: Expr, _ fill: Expr?) throws -> Expr {
    let k = try count.asInteger()
    guard k >= 0 && k <= Int64(Int.max) else {
      throw EvalError.ParameterOutOfBounds("make-vector", 1, k, 0, Int64(Int.max))
    }
    return .Vec(self.context.objects.manage(Vector(count: Int(k), repeatedValue: fill ?? .Null)))
  }
  
  func vector(args: Arguments) -> Expr {
    let res = Vector()
    for arg in args {
      res.exprs.append(arg)
    }
    return .Vec(self.context.objects.manage(res))
  }
  
  func compileVector(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .Pair(_, let cdr) = expr else {
      preconditionFailure()
    }
    compiler.emit(.Vector(try compiler.compileExprs(cdr, in: env)))
    return false
  }
  
  func vectorAppend(exprs: Arguments) throws -> Expr {
    let res = Vector()
    for expr in exprs {
      res.exprs.appendContentsOf(try expr.asVector().exprs)
    }
    return .Vec(self.context.objects.manage(res))
  }
  
  func vectorRef(vec: Expr, _ index: Expr) throws -> Expr {
    let vector = try vec.asVector()
    let i = try index.asInteger()
    guard i >= 0 && i < Int64(vector.exprs.count) else {
      throw EvalError.IndexOutOfBounds(i, Int64(vector.exprs.count - 1), vec)
    }
    return vector.exprs[Int(i)]
  }
  
  func vectorSet(vec: Expr, _ index: Expr, _ expr: Expr) throws -> Expr {
    // Extract arguments
    let vector = try vec.asVector()
    let i = try index.asInt()
    guard i >= 0 && i <= vector.exprs.count else {
      throw EvalError.IndexOutOfBounds(Int64(i), Int64(vector.exprs.count), vec)
    }
    // Set value at index `i`. Guarantee that vectors for which `vector-set!` is
    // called are managed by a managed object pool.
    self.context.objects.manage(vector).exprs[i] = expr
    return .Void
  }
  
  func listToVector(expr: Expr) throws -> Expr {
    guard case (let exprs, .Null) = expr.toExprs() else {
      throw EvalError.TypeError(expr, [.ProperListType])
    }
    return .Vec(self.context.objects.manage(Vector(exprs)))
  }
  
  func vectorToList(vec: Expr) throws -> Expr {
    let vector = try vec.asVector()
    var res = Expr.Null
    for expr in vector.exprs.reverse() {
      res = .Pair(expr, res)
    }
    return res
  }
  
  func vectorFill(vec: Expr, _ expr: Expr) throws -> Expr {
    let vector = try vec.asVector()
    for i in vector.exprs.indices {
      vector.exprs[i] = expr
    }
    return .Void
  }
}