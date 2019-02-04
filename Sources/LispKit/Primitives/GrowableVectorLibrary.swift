//
//  GrowableVectorLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 17/01/2019.
//  Copyright © 2019 ObjectHub. All rights reserved.
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
/// Growable vector library: based on corresponding Racket library
///
public final class GrowableVectorLibrary: NativeLibrary {
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "gvector"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"],    "define", "set!", "or", "not", "apply")
    self.`import`(from: ["lispkit", "control"], "let", "let*", "do", "unless", "when", "if")
    self.`import`(from: ["lispkit", "math"],    "fx1+", "fx1-", "fx=", "fx>", "fx<", "fx<=", "fx>=")
    self.`import`(from: ["lispkit", "list"],    "cons", "null?")
    self.`import`(from: ["lispkit", "vector"],  "vector-swap!")
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("gvector?", isGvector))
    self.define(Procedure("gvector-empty?", isGvectorEmpty))
    self.define(Procedure("gvector-length", gvectorLength))
    self.define(Procedure("gvector", gvector))
    self.define(Procedure("make-gvector", makeGvector))
    self.define(Procedure("gvector-append", gvectorAppend))
    self.define(Procedure("gvector-concatenate", gvectorConcatenate))
    self.define(Procedure("gvector-append!", gvectorAppendDestructive))
    self.define(Procedure("gvector-ref", gvectorRef))
    self.define(Procedure("gvector-set!", gvectorSet))
    self.define(Procedure("gvector-add!", gvectorAdd))
    self.define(Procedure("gvector-insert!", gvectorInsert))
    self.define(Procedure("gvector-remove!", gvectorRemove))
    self.define(Procedure("gvector-remove-last!", gvectorRemoveLast))
    self.define(Procedure("list->gvector", listToGvector))
    self.define(Procedure("gvector->list", gvectorToList))
    self.define(Procedure("vector->gvector", vectorToGvector))
    self.define(Procedure("gvector->vector", gvectorToVector))
    self.define(Procedure("gvector-copy", gvectorCopy))
    self.define(Procedure("gvector-copy!", gvectorOverwrite))
    self.define(Procedure("gvector-reverse!", gvectorReverse))
    self.define(Procedure("_gvector-pivot!", gvectorPivot), export: false)
    self.define("_gvector-partition!", export: false, via:
      "(define (_gvector-partition! pred v lo hi)",
      "  (do ((i (fx1+ lo) (fx1+ i))",
      "       (j (fx1+ hi))",
      "       (pivot (_gvector-pivot! v lo hi)))",
      "      ((fx> i j) (vector-swap! v lo j) j)",
      "    (do () ((or (fx= i hi) (not (pred (gvector-ref v i) pivot))))",
      "      (set! i (fx1+ i)))",
      "    (set! j (fx1- j))",
      "    (do () ((or (fx= j lo) (not (pred pivot (gvector-ref v j)))))",
      "      (set! j (fx1- j)))",
      "    (when (fx< i j) (vector-swap! v i j))))")
    self.define("_quick-sort!", export: false, via:
      "(define (_quick-sort! pred v lo hi)",
      "  (unless (fx<= hi lo)",
      "    (let ((j (_gvector-partition! pred v lo hi)))",
      "      (_quick-sort! pred v lo (fx1- j))",
      "      (_quick-sort! pred v (fx1+ j) hi))))")
    self.define("gvector-sort!", via:
      "(define (gvector-sort! pred v)",
      "  (_quick-sort! pred v 0 (fx1- (gvector-length v))))")
    self.define(Procedure("_gvector-list-ref", gvectorListRef), export: false)
    self.define(Procedure("_gvector-list-length", gvectorListLength), export: false)
    self.define("gvector-map", via:
      "(define (gvector-map f xs . xss)",
      "  (let* ((vecs (cons xs xss))",
      "         (len (_gvector-list-length vecs))",
      "         (res (make-gvector len)))",
      "    (if (null? xss)",
      "        (do ((i 0 (fx1+ i)))",
      "            ((fx>= i len) res)",
      "          (gvector-set! res i (f (gvector-ref xs i))))",
      "        (do ((i 0 (fx1+ i)))",
      "            ((fx>= i len) res)",
      "          (gvector-set! res i (apply f (_gvector-list-ref i vecs)))))))")
    self.define("gvector-map/index", via:
      "(define (gvector-map/index f xs . xss)",
      "  (let* ((vecs (cons xs xss))",
      "         (len (_gvector-list-length vecs))",
      "         (res (make-gvector len)))",
      "    (if (null? xss)",
      "        (do ((i 0 (fx1+ i)))",
      "            ((fx>= i len) res)",
      "          (gvector-set! res i (f i (gvector-ref xs i))))",
      "        (do ((i 0 (fx1+ i)))",
      "            ((fx>= i len) res)",
      "          (gvector-set! res i (apply f (cons i (_gvector-list-ref i vecs))))))))")
    self.define("gvector-map!", via:
      "(define (gvector-map! f xs . xss)",
      "  (let* ((vecs (cons xs xss))",
      "         (len (_gvector-list-length vecs)))",
      "    (if (null? xss)",
      "        (do ((i 0 (fx1+ i)))",
      "            ((fx>= i len))",
      "          (gvector-set! xs i (f (gvector-ref xs i))))",
      "        (do ((i 0 (fx1+ i)))",
      "            ((fx>= i len))",
      "          (gvector-set! xs i (apply f (_gvector-list-ref i vecs)))))))")
    self.define("gvector-map/index!", via:
      "(define (gvector-map/index! f xs . xss)",
      "  (let* ((vecs (cons xs xss))",
      "         (len (_gvector-list-length vecs)))",
      "    (if (null? xss)",
      "        (do ((i 0 (fx1+ i)))",
      "            ((fx>= i len))",
      "          (gvector-set! xs i (f i (gvector-ref xs i))))",
      "        (do ((i 0 (fx1+ i)))",
      "            ((fx>= i len))",
      "          (gvector-set! xs i (apply f (cons i (_gvector-list-ref i vecs))))))))")
    self.define("gvector-for-each", via:
      "(define (gvector-for-each f xs . xss)",
      "  (let* ((vecs (cons xs xss))",
      "         (len (_gvector-list-length vecs)))",
      "    (do ((i 0 (fx1+ i)))",
      "         ((fx>= i len))",
      "      (apply f (_gvector-list-ref i vecs)))))")
    self.define("gvector-for-each/index", via:
      "(define (gvector-for-each/index f xs . xss)",
      "  (let* ((vecs (cons xs xss))",
      "         (len (_gvector-list-length vecs)))",
      "    (do ((i 0 (fx1+ i)))",
      "         ((fx>= i len))",
      "      (apply f (cons i (_gvector-list-ref i vecs))))))")
  }
  
  private func isGvector(_ expr: Expr) -> Expr {
    guard case .vector(let vector) = expr, vector.isGrowableVector else {
      return .false
    }
    return .true
  }
  
  private func isGvectorEmpty(_ expr: Expr) throws -> Expr {
    return .makeBoolean(try expr.vectorAsCollection(growable: true).exprs.isEmpty)
  }
  
  private func gvectorLength(vec: Expr) throws -> Expr {
    return .fixnum(Int64(try vec.vectorAsCollection(growable: true).exprs.count))
  }
  
  private func gvectorListLength(vectors: Expr) throws -> Expr {
    var n = Int.max
    var list = vectors
    while case .pair(let vec, let next) = list {
      let vector = try vec.vectorAsCollection()
      if vector.exprs.count < n {
        n = vector.exprs.count
      }
      list = next
    }
    guard list.isNull else {
      throw RuntimeError.type(vectors, expected: [.properListType])
    }
    return .fixnum(Int64(n))
  }
  
  private func makeGvector(_ capacity: Expr?) throws -> Expr {
    let res = Collection(kind: .growableVector)
    if let capacity = try capacity?.asInt() {
      res.exprs.reserveCapacity(capacity)
    }
    return .vector(res)
  }
  
  private func gvector(_ args: Arguments) -> Expr {
    let res = Collection(kind: .growableVector)
    for arg in args {
      res.exprs.append(arg)
    }
    return .vector(res)
  }
  
  private func gvectorAppend(_ exprs: Arguments) throws -> Expr {
    let res = Collection(kind: .growableVector)
    for expr in exprs {
      res.exprs.append(contentsOf: try expr.vectorAsCollection().exprs)
    }
    return .vector(res)
  }
  
  private func gvectorConcatenate(_ expr: Expr) throws -> Expr {
    let res = Collection(kind: .growableVector)
    var list = expr
    while case .pair(let vec, let next) = list {
      res.exprs.append(contentsOf: try vec.vectorAsCollection().exprs)
      list = next
    }
    guard list.isNull else {
      throw RuntimeError.type(expr, expected: [.properListType])
    }
    return .vector(res)
  }
  
  private func gvectorAppendDestructive(_ expr: Expr, _ args: Arguments) throws -> Expr {
    let vec = try expr.vectorAsCollection(growable: true)
    for arg in args {
      vec.exprs.append(contentsOf: try arg.vectorAsCollection().exprs)
    }
    return .void
  }
  
  private func gvectorRef(_ vec: Expr, index: Expr) throws -> Expr {
    let vector = try vec.vectorAsCollection(growable: true)
    let i = try index.asInt64()
    guard i >= 0 && i < Int64(vector.exprs.count) else {
      throw RuntimeError.range(parameter: 2,
                               of: "gvector-ref",
                               index,
                               min: 0,
                               max: Int64(vector.exprs.count - 1))
    }
    return vector.exprs[Int(i)]
  }
  
  private func gvectorListRef(_ index: Expr, _ vectors: Expr) throws -> Expr {
    let i = try index.asInt64()
    var res = Exprs()
    var list = vectors
    while case .pair(let vec, let next) = list {
      let vector = try vec.vectorAsCollection()
      guard i >= 0 && i < Int64(vector.exprs.count) else {
        return .false
      }
      res.append(vector.exprs[Int(i)])
      list = next
    }
    guard list.isNull else {
      throw RuntimeError.type(vectors, expected: [.properListType])
    }
    return .makeList(res)
  }
  
  private func gvectorSet(_ vec: Expr, index: Expr, expr: Expr) throws -> Expr {
    // Extract arguments
    let vector = try vec.vectorAsCollection(growable: true)
    let i = try index.asInt()
    guard i >= 0 && i < vector.exprs.count else {
      throw RuntimeError.range(parameter: 2,
                               of: "gvector-set!",
                               index,
                               min: Int64(i),
                               max: Int64(vector.exprs.count - 1))
    }
    // Set value at index `i`. Guarantee that vectors for which `vector-set!` is
    // called are managed by a managed object pool.
    (expr.isAtom ? vector : self.context.objects.manage(vector)).exprs[i] = expr
    return .void
  }
  
  private func gvectorAdd(_ vec: Expr, _ args: Arguments) throws -> Expr {
    // Extract arguments
    let vector = try vec.vectorAsCollection(growable: true)
    // Guarantee that vectors for which `vector-add!` is called are managed by a managed
    // object pool.
    var managed = false
    for arg in args {
      managed = managed || !arg.isAtom
      vector.exprs.append(arg)
    }
    if managed {
      self.context.objects.manage(vector)
    }
    return .void
  }
  
  private func gvectorInsert(_ vec: Expr, _ index: Expr, _ expr: Expr) throws -> Expr {
    let vector = try vec.vectorAsCollection(growable: true)
    let i = try index.asInt()
    guard i >= 0 && i <= vector.exprs.count else {
      throw RuntimeError.range(parameter: 2,
                               of: "gvector-insert!",
                               index,
                               min: Int64(i),
                               max: Int64(vector.exprs.count - 1))
    }
    (expr.isAtom ? vector : self.context.objects.manage(vector)).exprs.insert(expr, at: i)
    return .void
  }
  
  private func gvectorRemove(_ vec: Expr, _ index: Expr) throws -> Expr {
    let vector = try vec.vectorAsCollection(growable: true)
    let i = try index.asInt()
    guard i >= 0 && i <= vector.exprs.count else {
      throw RuntimeError.range(parameter: 2,
                               of: "gvector-insert!",
                               index,
                               min: Int64(i),
                               max: Int64(vector.exprs.count - 1))
    }
    let res = vector.exprs[i]
    vector.exprs.remove(at: i)
    return res
  }
  
  private func gvectorRemoveLast(_ vec: Expr, _ num: Expr?) throws -> Expr {
    let vector = try vec.vectorAsCollection(growable: true)
    if let n = try num?.asInt() {
      guard n >= 0 && n <= vector.exprs.count else {
        throw RuntimeError.range(parameter: 2,
                                 of: "gvector-remove-last!",
                                 num!,
                                 min: 0,
                                 max: Int64(vector.exprs.count))
      }
      vector.exprs.removeLast(n)
      return .void
    } else if let last = vector.exprs.last {
      vector.exprs.removeLast()
      return last
    } else {
      throw RuntimeError.eval(.vectorEmpty, vec)
    }
  }
  
  private func listToGvector(_ expr: Expr) throws -> Expr {
    guard case (let exprs, .null) = expr.toExprs() else {
      throw RuntimeError.type(expr, expected: [.properListType])
    }
    return .vector(Collection(kind: .growableVector, exprs: exprs))
  }
  
  private func gvectorToList(_ vec: Expr, start: Expr?, end: Expr?) throws -> Expr {
    let vector = try vec.vectorAsCollection(growable: true)
    let end = try end?.asInt(below: vector.exprs.count + 1) ?? vector.exprs.count
    let start = try start?.asInt(below: end + 1) ?? 0
    var res = Expr.null
    for expr in vector.exprs[start..<end].reversed() {
      res = .pair(expr, res)
    }
    return res
  }
  
  private func vectorToGvector(_ expr: Expr) throws -> Expr {
    let vector = try expr.vectorAsCollection()
    let res = Collection(kind: .growableVector)
    for expr in vector.exprs {
      res.exprs.append(expr)
    }
    return .vector(res)
  }
  
  private func gvectorToVector(_ vec: Expr, start: Expr?, end: Expr?) throws -> Expr {
    let vector = try vec.vectorAsCollection(growable: true)
    let end = try end?.asInt(below: vector.exprs.count + 1) ?? vector.exprs.count
    let start = try start?.asInt(below: end + 1) ?? 0
    let res = Collection(kind: .vector)
    for expr in vector.exprs[start..<end] {
      res.exprs.append(expr)
    }
    return .vector(res)
  }
  
  private func gvectorCopy(_ vec: Expr, _ args: Arguments) throws -> Expr {
    let vector = try vec.vectorAsCollection(growable: true)
    guard let (start, end, mutable) = args.optional(.fixnum(0),
                                                    .makeNumber(vector.exprs.count),
                                                    .true) else {
      throw RuntimeError.argumentCount(of: "gvector-copy",
                                       min: 1,
                                       max: 4,
                                       args: .pair(vec, .makeList(args)))
    }
    if args.count == 1 {
      switch start {
        case .fixnum(_):
          break;
        case .false:
          let res = Collection(kind: .immutableVector)
          for expr in vector.exprs {
            res.exprs.append(expr)
          }
          return .vector(res)
        default:
          let res = Collection(kind: .growableVector)
          for expr in vector.exprs {
            res.exprs.append(expr)
          }
          return .vector(res)
      }
    }
    let e = try end.asInt(below: vector.exprs.count + 1)
    let s = try start.asInt(below: e + 1)
    let res = Collection(kind: mutable.isFalse ? .immutableVector : .growableVector)
    for expr in vector.exprs[s..<e] {
      res.exprs.append(expr)
    }
    return .vector(res)
  }
  
  private func gvectorOverwrite(_ trgt: Expr,
                                at: Expr,
                                src: Expr,
                                start: Expr?,
                                end: Expr?) throws -> Expr {
    let target = try trgt.vectorAsCollection(growable: true)
    let from = try at.asInt(below: target.exprs.count + 1)
    let vector = try src.vectorAsCollection()
    let end = try end?.asInt(below: vector.exprs.count + 1) ?? vector.exprs.count
    let start = try start?.asInt(below: end + 1) ?? 0
    guard target.exprs.count - from >= end - start else {
      throw RuntimeError.range(parameter: 2,
                               of: "vector-copy!",
                               at,
                               min: 0,
                               max: Int64(start + target.exprs.count - end))
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
  
  private func gvectorReverse(_ vec: Expr, _ start: Expr?, _ end: Expr?) throws -> Expr {
    let vector = try vec.vectorAsCollection(growable: true)
    let end = try end?.asInt(below: vector.exprs.count + 1) ?? vector.exprs.count
    let start = try start?.asInt(below: end + 1) ?? 0
    vector.exprs[start..<end].reverse()
    return .void
  }
  
  private func gvectorPivot(_ vec: Expr, ilo: Expr, ihi: Expr) throws -> Expr {
    let vector = try vec.vectorAsCollection(growable: true)
    let hi = try ihi.asInt(below: vector.exprs.count)
    let lo = try ilo.asInt(below: hi &+ 1)
    guard lo < hi else {
      return vector.exprs[lo]
    }
    let i = Int(arc4random_uniform(UInt32(hi &- lo) &+ UInt32(1))) &+ lo
    let temp = vector.exprs[i]
    if i != lo {
      vector.exprs[i] = vector.exprs[lo]
      vector.exprs[lo] = temp
    }
    return temp
  }
}
