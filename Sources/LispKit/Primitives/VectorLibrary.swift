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

import Foundation

///
/// Vector library: based on R7RS spec.
///
public final class VectorLibrary: NativeLibrary {

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "vector"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"],    "define", "set!", "or", "not", "apply")
    self.`import`(from: ["lispkit", "control"], "let", "let*", "let-optionals", "do", "unless",
                                                "when", "if")
    self.`import`(from: ["lispkit", "math"],    "fx1+", "fx1-", "fx=", "fx>", "fx<", "fx<=", "fx>=")
    self.`import`(from: ["lispkit", "list"],    "null?", "cons", "car", "cdr")
  }

  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("make-vector", makeVector))
    self.define(Procedure("vector", vector, compileVector))
    self.define(Procedure("immutable-vector", immutableVector))
    self.define(Procedure("vector?", isVector, compileIsVector))
    self.define(Procedure("mutable-vector?", isMutableVector))
    self.define(Procedure("immutable-vector?", isImmutableVector))
    self.define(Procedure("vector-length", vectorLength))
    self.define(Procedure("vector-append", vectorAppend))
    self.define(Procedure("vector-concatenate", vectorConcatenate))
    self.define(Procedure("vector-ref", vectorRef))
    self.define(Procedure("vector-set!", vectorSet))
    self.define(Procedure("vector-swap!", vectorSwap))
    self.define(Procedure("list->vector", listToVector))
    self.define(Procedure("list->immutable-vector", listToImmutableVector))
    self.define(Procedure("vector->list", vectorToList))
    self.define(Procedure("string->vector", stringToVector))
    self.define(Procedure("vector->string", vectorToString))
    self.define(Procedure("vector-copy", vectorCopy))
    self.define(Procedure("vector-copy!", vectorOverwrite))
    self.define(Procedure("vector-fill!", vectorFill))
    self.define(Procedure("vector-reverse!", vectorReverse))
    self.define(Procedure("_vector-pivot!", vectorPivot))
    self.define("_vector-partition!", via: """
      (define (_vector-partition! pred v lo hi)
        (do ((i (fx1+ lo) (fx1+ i))
             (j (fx1+ hi))
             (pivot (_vector-pivot! v lo hi)))
            ((fx> i j) (vector-swap! v lo j) j)
          (do () ((or (fx= i hi) (not (pred (vector-ref v i) pivot))))
            (set! i (fx1+ i)))
          (set! j (fx1- j))
          (do () ((or (fx= j lo) (not (pred pivot (vector-ref v j)))))
            (set! j (fx1- j)))
          (when (fx< i j) (vector-swap! v i j))))
    """)
    self.define("_quick-sort!", via: """
      (define (_quick-sort! pred v lo hi)
        (unless (fx<= hi lo)
          (let ((j (_vector-partition! pred v lo hi)))
            (_quick-sort! pred v lo (fx1- j))
            (_quick-sort! pred v (fx1+ j) hi))))
    """)
    self.define("vector-sort", via: """
      (define (vector-sort pred v . args)
        (let-optionals args ((start 0)
                             (end (vector-length v)))
          (let ((sv (vector-copy v start end)))
            (_quick-sort! pred sv 0 (fx1- (vector-length sv)))
            sv)))
    """)
    self.define("vector-sort!", via: """
      (define (vector-sort! pred v . args)
        (let-optionals args ((start 0)
                             (end (vector-length v)))
          (_quick-sort! pred v start (fx1- end))))
    """)
    self.define(Procedure("_vector-list-ref", vectorListRef))
    self.define(Procedure("_vector-list-length", vectorListLength))
    self.define("vector-map", via: """
      (define (vector-map f xs . xss)
        (let* ((vecs (cons xs xss))
               (len (_vector-list-length vecs))
               (res (make-vector len)))
          (if (null? xss)
              (do ((i 0 (fx1+ i)))
                  ((fx>= i len) res)
                (vector-set! res i (f (vector-ref xs i))))
              (do ((i 0 (fx1+ i)))
                  ((fx>= i len) res)
                (vector-set! res i (apply f (_vector-list-ref i vecs)))))))
    """)
    self.define("vector-map!", via: """
      (define (vector-map! f xs . xss)
        (let* ((vecs (cons xs xss))
               (len (_vector-list-length vecs)))
          (if (null? xss)
              (do ((i 0 (fx1+ i)))
                  ((fx>= i len))
                (vector-set! xs i (f (vector-ref xs i))))
              (do ((i 0 (fx1+ i)))
                  ((fx>= i len))
                (vector-set! xs i (apply f (_vector-list-ref i vecs)))))))
    """)
    self.define("vector-map/index", via: """
      (define (vector-map/index f xs . xss)
        (let* ((vecs (cons xs xss))
               (len (_vector-list-length vecs))
               (res (make-vector len)))
          (if (null? xss)
              (do ((i 0 (fx1+ i)))
                  ((fx>= i len) res)
                (vector-set! res i (f i (vector-ref xs i))))
              (do ((i 0 (fx1+ i)))
                  ((fx>= i len) res)
                (vector-set! res i (apply f (cons i (_vector-list-ref i vecs))))))))
    """)
    self.define("vector-map/index!", via: """
      (define (vector-map/index! f xs . xss)
        (let* ((vecs (cons xs xss))
               (len (_vector-list-length vecs)))
          (if (null? xss)
              (do ((i 0 (fx1+ i)))
                  ((fx>= i len))
                (vector-set! xs i (f i (vector-ref xs i))))
              (do ((i 0 (fx1+ i)))
                  ((fx>= i len))
                (vector-set! xs i (apply f (cons i (_vector-list-ref i vecs))))))))
    """)
    self.define("vector-for-each", via: """
      (define (vector-for-each f xs . xss)
        (let* ((vecs (cons xs xss))
               (len (_vector-list-length vecs)))
          (do ((i 0 (fx1+ i)))
               ((fx>= i len))
            (apply f (_vector-list-ref i vecs)))))
    """)
    self.define("vector-for-each/index", via: """
      (define (vector-for-each/index f xs . xss)
        (let* ((vecs (cons xs xss))
               (len (_vector-list-length vecs)))
          (do ((i 0 (fx1+ i)))
               ((fx>= i len))
            (apply f (cons i (_vector-list-ref i vecs))))))
    """)
    self.define("vector=", via: """
      (define (vector= pred . vs)
        (if (null? vs)
            #t
            (let ((len (vector-length (car vs))))
              (if (do ((ws (cdr vs) (cdr ws)))
                      ((or (null? ws) (not (fx= (vector-length (car ws)) len))) (null? ws)))
                  (do ((i 0 (fx1+ i)))
                      ((or (fx= i len)
                           (let ((elems (_vector-list-ref i vs)))
                              (do ((x (car elems))
                                   (xs (cdr elems) (cdr xs)))
                                  ((or (null? xs) (not (pred x (car xs)))) (not (null? xs))))))
                       (fx= i len)))
                  #f))))
    """)
  }

  private func makeVector(_ count: Expr, fill: Expr?) throws -> Expr {
    let k = try count.asInt64()
    guard k >= 0 && k <= Int64(Int.max) else {
      throw RuntimeError.range(parameter: 1, of: "make-vector", count, min: 0)
    }
    return .vector(Collection(kind: .vector, count: Int(k), repeatedValue: fill ?? .null))
  }

  private func vector(_ args: Arguments) -> Expr {
    let res = Collection(kind: .vector)
    for arg in args {
      res.exprs.append(arg)
    }
    return .vector(res)
  }

  private func compileVector(_ compiler: Compiler,
                             expr: Expr,
                             env: Env,
                             tail: Bool) throws -> Bool {
    guard case .pair(_, let cdr) = expr else {
      preconditionFailure()
    }
    compiler.emit(.vector(try compiler.compileExprs(cdr, in: env)))
    return false
  }

  private func immutableVector(_ args: Arguments) -> Expr {
    let res = Collection(kind: .immutableVector)
    for arg in args {
      res.exprs.append(arg)
    }
    return .vector(res)
  }

  private func isVector(_ expr: Expr) -> Expr {
    switch expr {
      case .vector(let vector):
        return .makeBoolean(!vector.isGrowableVector)
      default:
        return .false
    }
  }

  private func compileIsVector(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let arg, .null)) = expr else {
      throw RuntimeError.argumentCount(of: "vector?", min: 1, max: 1, expr: expr)
    }
    try compiler.compile(arg, in: env, inTailPos: false)
    compiler.emit(.isVector)
    return false
  }

  private func isMutableVector(_ expr: Expr) -> Expr {
    switch expr {
      case .vector(let vector):
        return .makeBoolean(vector.isGrowableVector || vector.isMutableVector)
      default:
        return .false
    }
  }

  private func isImmutableVector(_ expr: Expr) -> Expr {
    switch expr {
      case .vector(let vector):
        return .makeBoolean(!vector.isGrowableVector && !vector.isMutableVector)
      default:
        return .false
    }
  }

  private func vectorLength(vec: Expr) throws -> Expr {
    return .fixnum(Int64(try vec.vectorAsCollection().exprs.count))
  }

  private func vectorListLength(vectors: Expr) throws -> Expr {
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

  private func vectorListRef(_ index: Expr, _ vectors: Expr) throws -> Expr {
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

  private func vectorAppend(_ exprs: Arguments) throws -> Expr {
    let res = Collection(kind: .vector)
    for expr in exprs {
      res.exprs.append(contentsOf: try expr.vectorAsCollection().exprs)
    }
    return .vector(res)
  }

  private func vectorConcatenate(_ expr: Expr) throws -> Expr {
    let res = Collection(kind: .vector)
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

  private func vectorRef(_ vec: Expr, index: Expr) throws -> Expr {
    let vector = try vec.vectorAsCollection()
    let i = try index.asInt64()
    guard i >= 0 && i < Int64(vector.exprs.count) else {
      throw RuntimeError.range(parameter: 2,
                               of: "vector-ref",
                               index,
                               min: 0,
                               max: Int64(vector.exprs.count - 1))
    }
    return vector.exprs[Int(i)]
  }

  private func vectorSet(_ vec: Expr, index: Expr, expr: Expr) throws -> Expr {
    // Extract arguments
    let vector = try vec.vectorAsCollection()
    let i = try index.asInt()
    guard i >= 0 && i < vector.exprs.count else {
      throw RuntimeError.range(parameter: 2,
                               of: "vector-set!",
                               index,
                               min: Int64(i),
                               max: Int64(vector.exprs.count - 1))
    }
    guard vector.isMutableVector else {
      throw RuntimeError.eval(.attemptToModifyImmutableData, vec)
    }
    // Set value at index `i`. Guarantee that vectors for which `vector-set!` is
    // called are managed by a managed object pool.
    (expr.isAtom ? vector : self.context.objects.manage(vector)).exprs[i] = expr
    return .void
  }

  private func vectorSwap(_ vec: Expr, index1: Expr, index2: Expr) throws -> Expr {
    let vector = try vec.vectorAsCollection()
    let i = try index1.asInt(below: vector.exprs.count)
    let j = try index2.asInt(below: vector.exprs.count)
    guard vector.isMutableVector else {
      throw RuntimeError.eval(.attemptToModifyImmutableData, vec)
    }
    let temp = vector.exprs[i]
    vector.exprs[i] = vector.exprs[j]
    vector.exprs[j] = temp
    return .void
  }

  private func listToVector(_ expr: Expr) throws -> Expr {
    guard case (let exprs, .null) = expr.toExprs() else {
      throw RuntimeError.type(expr, expected: [.properListType])
    }
    return .vector(Collection(kind: .vector, exprs: exprs))
  }

  private func listToImmutableVector(_ expr: Expr) throws -> Expr {
    guard case (let exprs, .null) = expr.toExprs() else {
      throw RuntimeError.type(expr, expected: [.properListType])
    }
    return .vector(Collection(kind: .immutableVector, exprs: exprs))
  }

  private func vectorToList(_ vec: Expr, start: Expr?, end: Expr?) throws -> Expr {
    let vector = try vec.vectorAsCollection()
    let end = try end?.asInt(below: vector.exprs.count + 1) ?? vector.exprs.count
    let start = try start?.asInt(below: end + 1) ?? 0
    var res = Expr.null
    for expr in vector.exprs[start..<end].reversed() {
      res = .pair(expr, res)
    }
    return res
  }

  private func stringToVector(_ expr: Expr, start: Expr?, end: Expr?) throws -> Expr {
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

  private func vectorToString(_ vec: Expr, start: Expr?, end: Expr?) throws -> Expr {
    let vector = try vec.vectorAsCollection()
    let end = try end?.asInt(below: vector.exprs.count + 1) ?? vector.exprs.count
    let start = try start?.asInt(below: end + 1) ?? 0
    var uniChars: [UniChar] = []
    for expr in vector.exprs[start..<end] {
      uniChars.append(try expr.asUniChar())
    }
    return .string(NSMutableString(string: String(utf16CodeUnits: uniChars, count: uniChars.count)))
  }

  private func vectorCopy(_ vec: Expr, _ args: Arguments) throws -> Expr {
    let vector = try vec.vectorAsCollection()
    guard let (start, end, mutable) = args.optional(.fixnum(0),
                                                    .makeNumber(vector.exprs.count),
                                                    .true) else {
      throw RuntimeError.argumentCount(of: "vector-copy",
                                       min: 1,
                                       max: 4,
                                       args: .pair(vec, .makeList(args)))
    }
    if args.count == 1 {
      switch start {
        case .fixnum:
          break;
        case .false:
          let res = Collection(kind: .immutableVector)
          for expr in vector.exprs {
            res.exprs.append(expr)
          }
          return .vector(res)
        default:
          let res = Collection(kind: .vector)
          for expr in vector.exprs {
            res.exprs.append(expr)
          }
          return .vector(res)
      }
    }
    let e = try end.asInt(below: vector.exprs.count + 1)
    let s = try start.asInt(below: e + 1)
    let res = Collection(kind: mutable.isFalse ? .immutableVector : .vector)
    for expr in vector.exprs[s..<e] {
      res.exprs.append(expr)
    }
    return .vector(res)
  }

  private func vectorOverwrite(_ trgt: Expr,
                               at: Expr,
                               src: Expr,
                               start: Expr?,
                               end: Expr?) throws -> Expr {
    let target = try trgt.vectorAsCollection()
    guard target.isMutableVector else {
      throw RuntimeError.eval(.attemptToModifyImmutableData, trgt)
    }
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

  private func vectorFill(_ vec: Expr, expr: Expr, start: Expr?, end: Expr?) throws -> Expr {
    let vector = try vec.vectorAsCollection()
    guard vector.isMutableVector else {
      throw RuntimeError.eval(.attemptToModifyImmutableData, vec)
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

  private func vectorReverse(_ vec: Expr, _ start: Expr?, _ end: Expr?) throws -> Expr {
    let vector = try vec.vectorAsCollection()
    guard vector.isMutableVector else {
      throw RuntimeError.eval(.attemptToModifyImmutableData, vec)
    }
    let end = try end?.asInt(below: vector.exprs.count + 1) ?? vector.exprs.count
    let start = try start?.asInt(below: end + 1) ?? 0
    vector.exprs[start..<end].reverse()
    return .void
  }

  private func vectorPivot(_ vec: Expr, ilo: Expr, ihi: Expr) throws -> Expr {
    let vector = try vec.vectorAsCollection()
    let hi = try ihi.asInt(below: vector.exprs.count)
    let lo = try ilo.asInt(below: hi &+ 1)
    guard vector.isMutableVector else {
      throw RuntimeError.eval(.attemptToModifyImmutableData, vec)
    }
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
