//
//  HashTableLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 18/07/2016.
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
/// Hashtable library: based on R6RS spec.
/// 
public final class HashTableLibrary: NativeLibrary {
  
  private static let equalHashProc = Procedure("equal-hash", HashTableLibrary.equalHashVal)
  private static let eqvHashProc   = Procedure("eqv-hash", HashTableLibrary.eqvHashVal)
  private static let eqHashProc    = Procedure("eq-hash", HashTableLibrary.eqHashVal)
  
  private static let defaultCapacity = 127
  
  private var bucketsProc: Procedure! = nil
  private var bucketAddProc: Procedure! = nil
  private var bucketDelProc: Procedure! = nil
  private var equalProc: Procedure! = nil
  private var eqvProc: Procedure! = nil
  private var eqProc: Procedure! = nil
  
  private var hashtableUnionLoc = 0
  private var hashtableIntersectionLoc = 0
  private var hashtableDifferenceLoc = 0
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "hashtable"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "base"],    "define", "lambda", "equal?", "eqv?", "eq?", "not")
    self.`import`(from: ["lispkit", "control"], "let*", "letrec", "if", "do")
    self.`import`(from: ["lispkit", "list"],    "cons", "car", "cdr", "pair?", "for-each", "value",
                                                "null?")
    self.`import`(from: ["lispkit", "math"],    ">", "+", "*")
  }
  
  /// Access to imported native procedures.
  public override func initializations() {
    self.equalProc = self.procedure(self.imported("equal?"))
    self.eqvProc = self.procedure(self.imported("eqv?"))
    self.eqProc = self.procedure(self.imported("eq?"))
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.bucketsProc = Procedure("_buckets", self.hBuckets)
    self.bucketAddProc = Procedure("_bucket-add!", self.hBucketAdd)
    self.bucketDelProc = Procedure("_bucket-repl!", self.hBucketRepl)
    self.define(HashTableLibrary.equalHashProc)
    self.define(HashTableLibrary.eqvHashProc)
    self.define(HashTableLibrary.eqHashProc)
    self.define(Procedure("_make-hashtable", makeHashTable))
    self.define(Procedure("make-eq-hashtable", makeEqHashTable))
    self.define(Procedure("make-eqv-hashtable", makeEqvHashTable))
    self.define(Procedure("make-equal-hashtable", makeEqualHashTable))
    self.define("make-hashtable", via:
      "(define (make-hashtable hash eql . size)",
      "  (letrec ((k (if (pair? size) (car size) \(HashTableLibrary.defaultCapacity)))",
      "           (find (lambda (key bs)",
      "                   (if (pair? bs)",
      "                       (if (eql (car (car bs)) key) (car bs) (find key (cdr bs)))",
      "                       #f)))",
      "           (drop (lambda (key bs)",  // TODO: Use a built-in filter function for this
      "                   (if (pair? bs)",
      "                     (if (eql (car (car bs)) key)",
      "                       bs",
      "                       (let ((res (drop key (cdr bs))))",
      "                         (cons (car res) (cons (car bs) (cdr res)))))",
      "                     (cons #f bs)))))",
      "    (_make-hashtable k eql hash",
      "      (lambda (ht buckets key)",
      "        (find key (buckets ht (hash key))))",
      "      (lambda (ht buckets add key value)",
      "        (add ht (hash key) key value)",
      "        (if (> (hashtable-load ht) 0.75)",
      "          (let ((ms (buckets ht)))",
      "            (hashtable-clear! ht (+ (* (hashtable-size ht) 3) 1))",
      "            (for-each (lambda (m) (add ht (hash (car m)) (car m) (cdr m))) ms))))",
      "      (lambda (ht buckets replace key)",
      "        (let* ((h (hash key))",
      "               (res (drop key (buckets ht h))))",
      "          (replace ht h (cdr res)) (car res))))))")
    self.define(Procedure("hashtable?", isHashTable))
    self.define(Procedure("eq-hashtable?", isEqHashTable))
    self.define(Procedure("eqv-hashtable?", isEqvHashTable))
    self.define(Procedure("equal-hashtable?", isEqualHashTable))
    self.define(Procedure("hashtable-mutable?", isHashTableMutable))
    self.define(Procedure("hashtable-size", hashTableSize))
    self.define(Procedure("hashtable-load", hashTableLoad))
    self.define(Procedure("hashtable-get", hashTableGet))
    self.define(Procedure("hashtable-add!", hashTableAdd))
    self.define("hashtable-contains?", via:
      "(define (hashtable-contains? map key) (pair? (hashtable-get map key)))")
    self.define("hashtable-ref", via:
      "(define (hashtable-ref map key default) (value (hashtable-get map key) default))")
    self.define("hashtable-set!", via:
      "(define (hashtable-set! map key value)",
      "  (hashtable-delete! map key)(hashtable-add! map key value))")
    self.define("hashtable-update!", via:
      "(define (hashtable-update! map key proc default)" +
      "  (hashtable-add! map key (proc (value (hashtable-delete! map key) default))))")
    self.define(Procedure("hashtable-delete!", hashTableDelete))
    self.define(Procedure("hashtable-clear!", hashTableClear))
    self.hashtableUnionLoc =
      self.define("xhashtable-union!", via:
        "(define (xhashtable-union! ht1 ht2)",
        "  (do ((ks (hashtable->alist ht2) (cdr ks)))",
        "      ((null? ks))",
        "    (if (not (hashtable-contains? ht1 (car (car ks))))",
        "        (hashtable-add! ht1 (car (car ks)) (cdr (car ks))))))")
    self.hashtableIntersectionLoc =
      self.define("_hashtable-intersection!", via:
        "(define (_hashtable-intersection! ht1 ht2)",
        "  (do ((ks (hashtable->alist ht1) (cdr ks)))",
        "      ((null? ks))",
        "    (if (not (hashtable-contains? ht2 (car (car ks))))",
        "        (hashtable-delete! ht1 (car (car ks))))))")
    self.hashtableDifferenceLoc =
      self.define("_hashtable-difference!", via:
        "(define (_hashtable-difference! ht1 ht2)",
        "  (do ((ks (hashtable->alist ht1) (cdr ks)))",
        "      ((null? ks))",
        "    (if (hashtable-contains? ht2 (car (car ks)))",
        "        (hashtable-delete! ht1 (car (car ks))))))")
    self.define(Procedure("hashtable-union!", hashTableUnion))
    self.define(Procedure("hashtable-intersection!", hashTableIntersection))
    self.define(Procedure("hashtable-difference!", hashTableDifference))
    self.define(Procedure("hashtable-copy", hashTableCopy))
    self.define(Procedure("hashtable-keys", hashTableKeys))
    self.define(Procedure("hashtable-values", hashTableValues))
    self.define(Procedure("hashtable-key-list", hashTableKeyList))
    self.define(Procedure("hashtable-value-list", hashTableValueList))
    self.define(Procedure("hashtable->alist", hashTableToAlist))
    self.define("alist->hashtable!", via:
      "(define (alist->hashtable! hm alist)",
      "  (for-each (lambda (m) (hashtable-add! hm (car m) (cdr m))) alist))")
    self.define(Procedure("alist->eq-hashtable", alistToEqHashTable))
    self.define(Procedure("alist->eqv-hashtable", alistToEqvHashTable))
    self.define(Procedure("alist->equal-hashtable", alistToEqualHashTable))
    self.define(Procedure("hashtable-equivalence-function", hashTableEquivalenceFunction))
    self.define(Procedure("hashtable-hash-function", hashTableHashFunction))
    self.define(Procedure("string-hash", stringHashVal))
    self.define(Procedure("string-ci-hash", stringCiHashVal))
    self.define(Procedure("symbol-hash", symbolHashVal))
  }
  
  func makeHashTable(_ capacity: Expr, _ eql: Expr, _ hsh: Expr, _ args: Arguments) throws -> Expr {
    guard args.count == 3 else {
      throw EvalError.argumentCountError(
        formals: 6, args: .pair(capacity, .pair(eql, .pair(hsh, .makeList(args)))))
    }
    let numBuckets = try capacity.asInt()
    let eqlProc = try eql.asProcedure()
    let hshProc = try hsh.asProcedure()
    if eqlProc == self.equalProc && hshProc == HashTableLibrary.equalHashProc {
      return .table(HashTable(capacity: numBuckets, mutable: true, equiv: .equal))
    } else if eqlProc == self.eqvProc && hshProc == HashTableLibrary.eqvHashProc {
      return .table(HashTable(capacity: numBuckets, mutable: true, equiv: .eqv))
    } else if eqlProc == self.eqProc && hshProc == HashTableLibrary.eqHashProc {
      return .table(HashTable(capacity: numBuckets, mutable: true, equiv: .eq))
    } else {
      let procs = HashTable.CustomProcedures(eql: eqlProc,
                                             hsh: hshProc,
                                             get: try args[args.startIndex].asProcedure(),
                                             add: try args[args.startIndex + 1].asProcedure(),
                                             del: try args[args.startIndex + 2].asProcedure())
      return .table(HashTable(capacity: numBuckets, mutable: true, equiv: .custom(procs)))
    }
  }
  
  func makeEqHashTable(_ capacity: Expr?) throws -> Expr {
    let numBuckets = try capacity?.asInt() ?? HashTableLibrary.defaultCapacity
    return .table(HashTable(capacity: numBuckets, mutable: true, equiv: .eq))
  }
  
  func makeEqvHashTable(_ capacity: Expr?) throws -> Expr {
    let numBuckets = try capacity?.asInt() ?? HashTableLibrary.defaultCapacity
    return .table(HashTable(capacity: numBuckets, mutable: true, equiv: .eqv))
  }
  
  func makeEqualHashTable(_ capacity: Expr?) throws -> Expr {
    let numBuckets = try capacity?.asInt() ?? HashTableLibrary.defaultCapacity
    return .table(HashTable(capacity: numBuckets, mutable: true, equiv: .equal))
  }
  
  func isHashTable(_ expr: Expr) -> Expr {
    guard case .table(_) = expr else {
      return .false
    }
    return .true
  }
  
  func isEqHashTable(_ expr: Expr) -> Expr {
    guard case .table(let table) = expr,
          case .eq = table.equiv else {
      return .false
    }
    return .true
  }
  
  func isEqvHashTable(_ expr: Expr) -> Expr {
    guard case .table(let table) = expr,
          case .eqv = table.equiv else {
      return .false
    }
    return .true
  }
  
  func isEqualHashTable(_ expr: Expr) -> Expr {
    guard case .table(let table) = expr,
          case .equal = table.equiv else {
      return .false
    }
    return .true
  }
  
  func isHashTableMutable(_ expr: Expr) -> Expr {
    guard case .table(let table) = expr else {
      return .false
    }
    return .makeBoolean(table.mutable)
  }
  
  func hashTableSize(_ expr: Expr) throws -> Expr {
    return .fixnum(Int64(try expr.asHashTable().count))
  }
  
  func hashTableLoad(_ expr: Expr) throws -> Expr {
    let map = try expr.asHashTable()
    return .makeNumber(Double(map.count) / Double(map.bucketCount))
  }
  
  func hashTableGet(_ args: Arguments) throws -> (Procedure, [Expr]) {
    try EvalError.assert(args, count: 2)
    let map = try args[args.startIndex].asHashTable()
    let key = args[args.startIndex + 1]
    guard case .custom(let procs) = map.equiv else {
      return (BaseLibrary.idProc, [map.get(key) ?? .false])
    }
    return (procs.get, [.table(map), .procedure(self.bucketsProc), key])
  }
  
  func hashTableAdd(_ args: Arguments) throws -> (Procedure, [Expr]) {
    try EvalError.assert(args, count: 3)
    let map = try args.first!.asHashTable()
    let key = args[args.startIndex + 1]
    let value = args[args.startIndex + 2]
    guard map.mutable else {
      throw EvalError.attemptToModifyImmutableData(.table(map))
    }
    guard case .custom(let procs) = map.equiv else {
      guard map.add(key: key, mapsTo: value) else {
        preconditionFailure("trying to set mapping in immutable hash map")
      }
      return (BaseLibrary.idProc, [.void])
    }
    return (procs.add, [.table(map),
                        .procedure(self.bucketsProc),
                        .procedure(self.bucketAddProc),
                        key,
                        value])
  }
  
  func hashTableDelete(_ args: Arguments) throws -> (Procedure, [Expr]) {
    try EvalError.assert(args, count: 2)
    let map = try args.first!.asHashTable()
    let key = args[args.startIndex + 1]
    guard map.mutable else {
      throw EvalError.attemptToModifyImmutableData(.table(map))
    }
    guard case .custom(let procs) = map.equiv else {
      guard let res = map.remove(key: key) else {
        preconditionFailure("trying to delete mapping in immutable hash map")
      }
      return (BaseLibrary.idProc, [res])
    }
    return (procs.del, [.table(map),
                        .procedure(self.bucketsProc),
                        .procedure(self.bucketDelProc),
                        key])
  }
  
  func hashTableCopy(_ expr: Expr, mutable: Expr?) throws -> Expr {
    let map = try expr.asHashTable()
    return .table(HashTable(copy: map, mutable: mutable == .true))
  }
  
  func hashTableClear(_ expr: Expr, k: Expr?) throws -> Expr {
    let map = try expr.asHashTable()
    guard map.mutable else {
      throw EvalError.attemptToModifyImmutableData(.table(map))
    }
    guard try map.clear(k?.asInt()) else {
      preconditionFailure("trying to clear immutable hash map")
    }
    return .void
  }
  
  func hashTableUnion(_ args: Arguments) throws -> (Procedure, [Expr]) {
    try EvalError.assert(args, count: 2)
    let map1 = try args.first!.asHashTable()
    let map2 = try args[args.startIndex + 1].asHashTable()
    guard map1.mutable else {
      throw EvalError.attemptToModifyImmutableData(.table(map1))
    }
    guard case .custom(_) = map1.equiv else {
      guard map1.union(map2) else {
        preconditionFailure("trying to union mapping with immutable hash map")
      }
      return (BaseLibrary.voidProc, [])
    }
    return (self.procedure(self.hashtableUnionLoc), [.table(map1), .table(map2)])
  }
  
  func hashTableIntersection(_ args: Arguments) throws -> (Procedure, [Expr]) {
    try EvalError.assert(args, count: 2)
    let map1 = try args.first!.asHashTable()
    let map2 = try args[args.startIndex + 1].asHashTable()
    guard map1.mutable else {
      throw EvalError.attemptToModifyImmutableData(.table(map1))
    }
    guard case .custom(_) = map1.equiv else {
      guard map1.difference(map2, intersect: true) else {
        preconditionFailure("trying to intersect mapping with immutable hash map")
      }
      return (BaseLibrary.voidProc, [])
    }
    return (self.procedure(self.hashtableIntersectionLoc), [.table(map1), .table(map2)])
  }
  
  func hashTableDifference(_ args: Arguments) throws -> (Procedure, [Expr]) {
    try EvalError.assert(args, count: 2)
    let map1 = try args.first!.asHashTable()
    let map2 = try args[args.startIndex + 1].asHashTable()
    guard map1.mutable else {
      throw EvalError.attemptToModifyImmutableData(.table(map1))
    }
    guard case .custom(_) = map1.equiv else {
      guard map1.difference(map2, intersect: false) else {
        preconditionFailure("trying to compute difference with immutable hash map")
      }
      return (BaseLibrary.voidProc, [])
    }
    return (self.procedure(self.hashtableDifferenceLoc), [.table(map1), .table(map2)])
  }
  
  func hashTableKeys(_ expr: Expr) throws -> Expr {
    return .vector(Collection(kind: .immutableVector, exprs: try expr.asHashTable().keys))
  }
  
  func hashTableValues(_ expr: Expr) throws -> Expr {
    return .vector(Collection(kind: .immutableVector, exprs: try expr.asHashTable().values))
  }
  
  func hashTableKeyList(_ expr: Expr) throws -> Expr {
    return try expr.asHashTable().keyList()
  }
  
  func hashTableValueList(_ expr: Expr) throws -> Expr {
    return try expr.asHashTable().valueList()
  }
    
  func hashTableToAlist(_ expr: Expr) throws -> Expr {
    return try expr.asHashTable().entryList()
  }
  
  fileprivate func newHashTable(_ equiv: HashTable.Equivalence, capacity: Int?) -> HashTable {
    let numBuckets = capacity ?? HashTableLibrary.defaultCapacity
    return HashTable(capacity: numBuckets, mutable: true, equiv: equiv)
  }
  
  fileprivate func enterAlist(_ expr: Expr, into map: HashTable) throws {
    var list = expr
    while case .pair(.pair(let key, let value), let cdr) = list {
      map.add(key: key, mapsTo: value)
      list = cdr
    }
    guard list.isNull else {
      throw EvalError.typeError(expr, [.assocListType])
    }
  }
  
  func alistToEqHashTable(_ expr: Expr, capacity: Expr?) throws -> Expr {
    let table = self.newHashTable(.eq, capacity: try capacity?.asInt())
    try self.enterAlist(expr, into: table)
    return .table(table)
  }
  
  func alistToEqvHashTable(_ expr: Expr, capacity: Expr?) throws -> Expr {
    let table = self.newHashTable(.eqv, capacity: try capacity?.asInt())
    try self.enterAlist(expr, into: table)
    return .table(table)
  }
  
  func alistToEqualHashTable(_ expr: Expr, capacity: Expr?) throws -> Expr {
    let table = self.newHashTable(.equal, capacity: try capacity?.asInt())
    try self.enterAlist(expr, into: table)
    return .table(table)
  }
  
  func hashTableEquivalenceFunction(_ expr: Expr) throws -> Expr {
    switch try expr.asHashTable().equiv {
      case .eq:
        return .procedure(self.eqProc)
      case .eqv:
        return .procedure(self.eqvProc)
      case .equal:
        return .procedure(self.equalProc)
      case .custom(let procs):
        return .procedure(procs.eql)
    }
  }
  
  func hashTableHashFunction(_ expr: Expr) throws -> Expr {
    switch try expr.asHashTable().equiv {
      case .eq, .eqv:
        return .false
      case .equal:
        return .procedure(HashTableLibrary.equalHashProc)
      case .custom(let procs):
        return .procedure(procs.hsh)
    }
  }
  
  private static func eqHashVal(_ expr: Expr) -> Expr {
    return .fixnum(Int64(eqHash(expr)))
  }
  
  private static func eqvHashVal(_ expr: Expr) -> Expr {
    return .fixnum(Int64(eqvHash(expr)))
  }
  
  private static func equalHashVal(_ expr: Expr) -> Expr {
    return .fixnum(Int64(equalHash(expr)))
  }
  
  func stringHashVal(_ expr: Expr) throws -> Expr {
    guard case .string(_) = expr else {
      throw EvalError.typeError(expr, [.strType])
    }
    return .fixnum(Int64(expr.hashValue))
  }
  
  func stringCiHashVal(_ expr: Expr) throws -> Expr {
    guard case .string(let str) = expr else {
      throw EvalError.typeError(expr, [.strType])
    }
    return .fixnum(Int64(str.lowercased.hashValue))
  }
  
  func symbolHashVal(_ expr: Expr) throws -> Expr {
    guard case .symbol(_) = expr else {
      throw EvalError.typeError(expr, [.symbolType])
    }
    return .fixnum(Int64(expr.hashValue))
  }
  
  private func hBuckets(_ expr: Expr, hval: Expr?) throws -> Expr {
    let map = try expr.asHashTable()
    if let hashValue = try hval?.asInt64() {
      return map.bucketList(Int(hashValue % Int64(map.bucketCount)))
    } else {
      return map.bucketList()
    }
  }
  
  private func hBucketAdd(_ expr: Expr, hval: Expr, key: Expr, value: Expr) throws -> Expr {
    guard case .table(let map) = expr else {
      throw EvalError.typeError(expr, [.tableType])
    }
    if !key.isAtom || !value.isAtom {
      self.context.objects.manage(map)
    }
    map.add(Int(try hval.asInt64() % Int64(map.bucketCount)), key, value)
    return .void
  }
  
  private func hBucketRepl(_ expr: Expr, hval: Expr, bucket: Expr) throws -> Expr {
    guard case .table(let map) = expr else {
      throw EvalError.typeError(expr, [.tableType])
    }
    map.replace(Int(try hval.asInt64() % Int64(map.bucketCount)), bucket)
    return .void
  }
}
