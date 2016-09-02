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

///
/// Vector library: based on R7RS spec.
/// 
public final class VectorLibrary: NativeLibrary {
  
  public override func export() {
    define(Procedure("vector?", isVector, compileIsVector))
    define(Procedure("vector", vector, compileVector))
    define(Procedure("make-vector", makeVector))
    define(Procedure("vector-append", vectorAppend))
    define(Procedure("vector-ref", vectorRef))
    define(Procedure("vector-set!", vectorSet))
    define(Procedure("list->vector", listToVector))
    define(Procedure("vector->list", vectorToList))
    define(Procedure("string->vector", stringToVector))
    define(Procedure("vector->string", vectorToString))
    define(Procedure("vector-copy", vectorCopy))
    define(Procedure("vector-copy!", vectorOverwrite))
    define(Procedure("vector-fill!", vectorFill))
  }
  
  func isVector(expr: Expr) -> Expr {
    switch expr {
      case .Vector(_):
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
  
  func makeVector(count: Expr, fill: Expr?) throws -> Expr {
    let k = try count.asInteger()
    guard k >= 0 && k <= Int64(Int.max) else {
      throw EvalError.ParameterOutOfBounds("make-vector", 1, k, 0, Int64(Int.max))
    }
    return .Vector(Collection(kind: .Vector, count: Int(k), repeatedValue: fill ?? .Null))
  }
  
  func vector(args: Arguments) -> Expr {
    let res = Collection(kind: .Vector)
    for arg in args {
      res.exprs.append(arg)
    }
    return .Vector(res)
  }
  
  func compileVector(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .Pair(_, let cdr) = expr else {
      preconditionFailure()
    }
    compiler.emit(.Vector(try compiler.compileExprs(cdr, in: env)))
    return false
  }
  
  func vectorAppend(exprs: Arguments) throws -> Expr {
    let res = Collection(kind: .Vector)
    for expr in exprs {
      res.exprs.appendContentsOf(try expr.asVector().exprs)
    }
    return .Vector(res)
  }
  
  func vectorRef(vec: Expr, index: Expr) throws -> Expr {
    let vector = try vec.asVector()
    let i = try index.asInteger()
    guard i >= 0 && i < Int64(vector.exprs.count) else {
      throw EvalError.IndexOutOfBounds(i, Int64(vector.exprs.count - 1), vec)
    }
    return vector.exprs[Int(i)]
  }
  
  func vectorSet(vec: Expr, index: Expr, expr: Expr) throws -> Expr {
    // Extract arguments
    let vector = try vec.asVector()
    let i = try index.asInt()
    guard i >= 0 && i <= vector.exprs.count else {
      throw EvalError.IndexOutOfBounds(Int64(i), Int64(vector.exprs.count), vec)
    }
    guard case .Vector = vector.kind else {
      throw EvalError.AttemptToModifyImmutableData(vec)
    }
    // Set value at index `i`. Guarantee that vectors for which `vector-set!` is
    // called are managed by a managed object pool.
    (expr.isSimple ? vector : self.context.objects.manage(vector)).exprs[i] = expr
    return .Void
  }
  
  func listToVector(expr: Expr) throws -> Expr {
    guard case (let exprs, .Null) = expr.toExprs() else {
      throw EvalError.TypeError(expr, [.ProperListType])
    }
    return .Vector(Collection(kind: .Vector, exprs: exprs))
  }
  
  func vectorToList(vec: Expr, start: Expr?, end: Expr?) throws -> Expr {
    let vector = try vec.asVector()
    let end = try end?.asInt(below: vector.exprs.count + 1) ?? vector.exprs.count
    let start = try start?.asInt(below: end + 1) ?? 0
    var res = Expr.Null
    for expr in vector.exprs[start..<end].reverse() {
      res = .Pair(expr, res)
    }
    return res
  }
  
  func stringToVector(expr: Expr, start: Expr?, end: Expr?) throws -> Expr {
    let str = try expr.asStr().utf16
    let max = try end?.asInt(below: str.count + 1) ?? str.count
    let end = str.startIndex.advancedBy(max)
    let start = str.startIndex.advancedBy(try start?.asInt(below: max + 1) ?? 0)
    let res = Collection(kind: .Vector)
    for ch in str[start..<end] {
      res.exprs.append(.Char(ch))
    }
    return .Vector(res)
  }
  
  func vectorToString(vec: Expr, start: Expr?, end: Expr?) throws -> Expr {
    let vector = try vec.asVector()
    let end = try end?.asInt(below: vector.exprs.count + 1) ?? vector.exprs.count
    let start = try start?.asInt(below: end + 1) ?? 0
    var uniChars: [UniChar] = []
    for expr in vector.exprs[start..<end] {
      uniChars.append(try expr.asChar())
    }
    return .Str(NSMutableString(string: String(utf16CodeUnits: uniChars, count: uniChars.count)))
  }
  
  func vectorCopy(vec: Expr, start: Expr?, end: Expr?) throws -> Expr {
    let vector = try vec.asVector()
    let end = try end?.asInt(below: vector.exprs.count + 1) ?? vector.exprs.count
    let start = try start?.asInt(below: end + 1) ?? 0
    let res = Collection(kind: .Vector)
    for expr in vector.exprs[start..<end] {
      res.exprs.append(expr)
    }
    return .Vector(res)
  }

  func vectorOverwrite(trgt: Expr, at: Expr, src: Expr, start: Expr?, end: Expr?) throws -> Expr {
    let target = try trgt.asVector()
    guard case .Vector = target.kind else {
      throw EvalError.AttemptToModifyImmutableData(trgt)
    }
    let from = try at.asInt(below: target.exprs.count + 1) ?? target.exprs.count
    let vector = try src.asVector()
    let end = try end?.asInt(below: vector.exprs.count + 1) ?? vector.exprs.count
    let start = try start?.asInt(below: end + 1) ?? 0
    guard target.exprs.count - from >= end - start else {
      throw EvalError.ParameterOutOfBounds(
        "vector-copy!", 2, Int64(from), Int64(0), Int64(start + target.exprs.count - end))
    }
    // Decide on right order in case `target` and `vector` are identical vectors
    var isSimple = true
    if start < from {
      for i in (start..<end).reverse() {
        isSimple = isSimple && vector.exprs[i].isSimple
        target.exprs[from + i - start] = vector.exprs[i]
      }
    } else {
      for i in start..<end {
        isSimple = isSimple && vector.exprs[i].isSimple
        target.exprs[from + i - start] = vector.exprs[i]
      }
    }
    if !isSimple {
      self.context.objects.manage(target)
    }
    return .Void
  }
  
  func vectorFill(vec: Expr, expr: Expr, start: Expr?, end: Expr?) throws -> Expr {
    let vector = try vec.asVector()
    guard case .Vector = vector.kind else {
      throw EvalError.AttemptToModifyImmutableData(vec)
    }
    let end = try end?.asInt(below: vector.exprs.count + 1) ?? vector.exprs.count
    let start = try start?.asInt(below: end + 1) ?? 0
    if start < end && !expr.isSimple {
      self.context.objects.manage(vector)
    }
    for i in start..<end {
      vector.exprs[i] = expr
    }
    return .Void
  }
}