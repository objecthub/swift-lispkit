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
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "vector"]
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("vector?", isVector, compileIsVector))
    self.define(Procedure("vector", vector, compileVector))
    self.define(Procedure("make-vector", makeVector))
    self.define(Procedure("vector-append", vectorAppend))
    self.define(Procedure("vector-ref", vectorRef))
    self.define(Procedure("vector-set!", vectorSet))
    self.define(Procedure("list->vector", listToVector))
    self.define(Procedure("vector->list", vectorToList))
    self.define(Procedure("string->vector", stringToVector))
    self.define(Procedure("vector->string", vectorToString))
    self.define(Procedure("vector-copy", vectorCopy))
    self.define(Procedure("vector-copy!", vectorOverwrite))
    self.define(Procedure("vector-fill!", vectorFill))
  }
  
  func isVector(_ expr: Expr) -> Expr {
    switch expr {
      case .vector(_):
        return .true
      default:
        return .false
    }
  }
  
  func compileIsVector(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let arg, .null)) = expr else {
      throw EvalError.argumentCountError(formals: 1, args: expr)
    }
    try compiler.compile(arg, in: env, inTailPos: false)
    compiler.emit(.isVector)
    return false
  }
  
  func makeVector(_ count: Expr, fill: Expr?) throws -> Expr {
    let k = try count.asInt64()
    guard k >= 0 && k <= Int64(Int.max) else {
      throw EvalError.parameterOutOfBounds("make-vector", 1, k, 0, Int64(Int.max))
    }
    return .vector(Collection(kind: .vector, count: Int(k), repeatedValue: fill ?? .null))
  }
  
  func vector(_ args: Arguments) -> Expr {
    let res = Collection(kind: .vector)
    for arg in args {
      res.exprs.append(arg)
    }
    return .vector(res)
  }
  
  func compileVector(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, let cdr) = expr else {
      preconditionFailure()
    }
    compiler.emit(.vector(try compiler.compileExprs(cdr, in: env)))
    return false
  }
  
  func vectorAppend(_ exprs: Arguments) throws -> Expr {
    let res = Collection(kind: .vector)
    for expr in exprs {
      res.exprs.append(contentsOf: try expr.vectorAsCollection().exprs)
    }
    return .vector(res)
  }
  
  func vectorRef(_ vec: Expr, index: Expr) throws -> Expr {
    let vector = try vec.vectorAsCollection()
    let i = try index.asInt64()
    guard i >= 0 && i < Int64(vector.exprs.count) else {
      throw EvalError.indexOutOfBounds(i, Int64(vector.exprs.count - 1), vec)
    }
    return vector.exprs[Int(i)]
  }
  
  func vectorSet(_ vec: Expr, index: Expr, expr: Expr) throws -> Expr {
    // Extract arguments
    let vector = try vec.vectorAsCollection()
    let i = try index.asInt()
    guard i >= 0 && i <= vector.exprs.count else {
      throw EvalError.indexOutOfBounds(Int64(i), Int64(vector.exprs.count), vec)
    }
    guard case .vector = vector.kind else {
      throw EvalError.attemptToModifyImmutableData(vec)
    }
    // Set value at index `i`. Guarantee that vectors for which `vector-set!` is
    // called are managed by a managed object pool.
    (expr.isAtom ? vector : self.context.objects.manage(vector)).exprs[i] = expr
    return .void
  }
  
  func listToVector(_ expr: Expr) throws -> Expr {
    guard case (let exprs, .null) = expr.toExprs() else {
      throw EvalError.typeError(expr, [.properListType])
    }
    return .vector(Collection(kind: .vector, exprs: exprs))
  }
  
  func vectorToList(_ vec: Expr, start: Expr?, end: Expr?) throws -> Expr {
    let vector = try vec.vectorAsCollection()
    let end = try end?.asInt(below: vector.exprs.count + 1) ?? vector.exprs.count
    let start = try start?.asInt(below: end + 1) ?? 0
    var res = Expr.null
    for expr in vector.exprs[start..<end].reversed() {
      res = .pair(expr, res)
    }
    return res
  }
  
  func stringToVector(_ expr: Expr, start: Expr?, end: Expr?) throws -> Expr {
    let str = try expr.asString().utf16
    let max = try end?.asInt(below: str.count + 1) ?? str.count
    let end = str.index(str.startIndex, offsetBy: max)
    let start = str.index(str.startIndex, offsetBy: try start?.asInt(below: max + 1) ?? 0)
    let res = Collection(kind: .vector)
    for ch in str[start..<end] {
      res.exprs.append(.char(ch))
    }
    return .vector(res)
  }
  
  func vectorToString(_ vec: Expr, start: Expr?, end: Expr?) throws -> Expr {
    let vector = try vec.vectorAsCollection()
    let end = try end?.asInt(below: vector.exprs.count + 1) ?? vector.exprs.count
    let start = try start?.asInt(below: end + 1) ?? 0
    var uniChars: [UniChar] = []
    for expr in vector.exprs[start..<end] {
      uniChars.append(try expr.asUniChar())
    }
    return .string(NSMutableString(string: String(utf16CodeUnits: uniChars, count: uniChars.count)))
  }
  
  func vectorCopy(_ vec: Expr, start: Expr?, end: Expr?) throws -> Expr {
    let vector = try vec.vectorAsCollection()
    let end = try end?.asInt(below: vector.exprs.count + 1) ?? vector.exprs.count
    let start = try start?.asInt(below: end + 1) ?? 0
    let res = Collection(kind: .vector)
    for expr in vector.exprs[start..<end] {
      res.exprs.append(expr)
    }
    return .vector(res)
  }

  func vectorOverwrite(_ trgt: Expr, at: Expr, src: Expr, start: Expr?, end: Expr?) throws -> Expr {
    let target = try trgt.vectorAsCollection()
    guard case .vector = target.kind else {
      throw EvalError.attemptToModifyImmutableData(trgt)
    }
    let from = try at.asInt(below: target.exprs.count + 1)
    let vector = try src.vectorAsCollection()
    let end = try end?.asInt(below: vector.exprs.count + 1) ?? vector.exprs.count
    let start = try start?.asInt(below: end + 1) ?? 0
    guard target.exprs.count - from >= end - start else {
      throw EvalError.parameterOutOfBounds(
        "vector-copy!", 2, Int64(from), Int64(0), Int64(start + target.exprs.count - end))
    }
    // Decide on right order in case `target` and `vector` are identical vectors
    var isAtom = true
    if start < from {
      for i in (start..<end).reversed() {
        isAtom = isAtom && vector.exprs[i].isAtom
        target.exprs[from + i - start] = vector.exprs[i]
      }
    } else {
      for i in start..<end {
        isAtom = isAtom && vector.exprs[i].isAtom
        target.exprs[from + i - start] = vector.exprs[i]
      }
    }
    if !isAtom {
      self.context.objects.manage(target)
    }
    return .void
  }
  
  func vectorFill(_ vec: Expr, expr: Expr, start: Expr?, end: Expr?) throws -> Expr {
    let vector = try vec.vectorAsCollection()
    guard case .vector = vector.kind else {
      throw EvalError.attemptToModifyImmutableData(vec)
    }
    let end = try end?.asInt(below: vector.exprs.count + 1) ?? vector.exprs.count
    let start = try start?.asInt(below: end + 1) ?? 0
    if start < end && !expr.isAtom {
      self.context.objects.manage(vector)
    }
    for i in start..<end {
      vector.exprs[i] = expr
    }
    return .void
  }
}
