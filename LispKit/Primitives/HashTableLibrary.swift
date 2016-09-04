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

///
/// Hashtable library: based on R6RS spec.
/// 
public final class HashTableLibrary: NativeLibrary {
  
  fileprivate static let equalHashProc = Procedure("equal-hash", HashTableLibrary.equalHashVal)
  fileprivate static let eqvHashProc   = Procedure("eqv-hash", HashTableLibrary.eqvHashVal)
  fileprivate static let eqHashProc    = Procedure("eq-hash", HashTableLibrary.eqHashVal)
  
  fileprivate static let defaultCapacity = 127
  
  fileprivate let equalProc: Procedure
  fileprivate let eqvProc: Procedure
  fileprivate let eqProc: Procedure
  fileprivate var bucketsProc: Procedure! = nil
  fileprivate var bucketAddProc: Procedure! = nil
  fileprivate var bucketDelProc: Procedure! = nil
  
  /// Import
  public required init(_ context: Context) {
    self.equalProc = NativeLibrary.importProc(from: context, name: "equal?")
    self.eqvProc = NativeLibrary.importProc(from: context, name: "eqv?")
    self.eqProc = NativeLibrary.importProc(from: context, name: "eq?")
    super.init(context)
    self.bucketsProc = Procedure("_buckets", self.hBuckets)
    self.bucketAddProc = Procedure("_bucket-add!", self.hBucketAdd)
    self.bucketDelProc = Procedure("_bucket-repl!", self.hBucketRepl)
  }
  
  /// Export
  public override func export() {
    define(Procedure("_make-hashtable", makeHashTable))
    define(Procedure("make-eq-hashtable", makeEqHashTable))
    define(Procedure("make-eqv-hashtable", makeEqvHashTable))
    define(Procedure("make-equal-hashtable", makeEqualHashTable))
    define("make-hashtable", compile:
      "(lambda (hash eql . size)" +
      "  (letrec ((k (if (pair? size) (car size) \(HashTableLibrary.defaultCapacity)))" +
      "           (find (lambda (key bs)" +
      "                   (if (pair? bs)" +
      "                     (if (eql (car (car bs)) key) (car bs) (find key (cdr bs))) #f)))" +
      "           (drop (lambda (key bs)" +  // TODO: Use a built-in filter function for this
      "                   (if (pair? bs)" +
      "                     (if (eql (car (car bs)) key)" +
      "                       bs" +
      "                       (let ((res (drop key (cdr bs))))" +
      "                         (cons (car res) (cons (car bs) (cdr res)))))" +
      "                     (cons #f bs)))))" +
      "    (_make-hashtable k eql hash" +
      "      (lambda (ht buckets key)" +
      "        (find key (buckets ht (hash key))))" +
      "      (lambda (ht buckets add key value)" +
      "        (add ht (hash key) key value)" +
      "        (if (> (hashtable-load ht) 0.75)" +
      "          (let ((ms (buckets ht)))" +
      "            (hashtable-clear! ht (+ (* (hashtable-size ht) 3) 1))" +
      "            (for-each (lambda (m) (add ht (hash (car m)) (car m) (cdr m))) ms))))" +
      "      (lambda (ht buckets replace key)" +
      "        (let* ((h (hash key))" +
      "               (res (drop key (buckets ht h))))" +
      "          (replace ht h (cdr res)) (car res))))))")
    define(Procedure("hashtable?", isHashTable))
    define(Procedure("eq-hashtable?", isEqHashTable))
    define(Procedure("eqv-hashtable?", isEqvHashTable))
    define(Procedure("equal-hashtable?", isEqualHashTable))
    define(Procedure("hashtable-mutable?", isHashTableMutable))
    define(Procedure("hashtable-size", hashTableSize))
    define(Procedure("hashtable-load", hashTableLoad))
    define(Procedure("hashtable-get", hashTableGet))
    define(Procedure("hashtable-add!", hashTableAdd))
    define("hashtable-include!", compile:
      "(lambda (hm alist) (for-each (lambda (m) (hashtable-add! hm (car m) (cdr m))) alist))")
    define("hashtable-contains", compile:
      "(lambda (map key) (pair? (hashtable-get map key)))")
    define("hashtable-ref", compile:
      "(lambda (map key default) (value (hashtable-get map key) default))")
    define("hashtable-set!", compile:
      "(lambda (map key value) (hashtable-delete! map key)(hashtable-add! map key value))")
    define("hashtable-update!", compile:
      "(lambda (map key proc default)" +
      "  (hashtable-add! map key (proc (value (hashtable-delete! map key) default))))")
    define(Procedure("hashtable-delete!", hashTableDelete))
    define(Procedure("hashtable-clear!", hashTableClear))
    define(Procedure("hashtable-copy", hashTableCopy))
    define(Procedure("hashtable-keys", hashTableKeys))
    define(Procedure("hashtable-values", hashTableValues))
    define(Procedure("hashtable-key-list", hashTableKeyList))
    define(Procedure("hashtable-value-list", hashTableValueList))
    define(Procedure("hashtable->alist", hashTableToAlist))
    define(Procedure("alist->eq-hashtable", alistToEqHashTable))
    define(Procedure("alist->eqv-hashtable", alistToEqvHashTable))
    define(Procedure("alist->equal-hashtable", alistToEqualHashTable))
    define(Procedure("hashtable-equivalence-function", hashTableEquivalenceFunction))
    define(Procedure("hashtable-hash-function", hashTableHashFunction))
    define(HashTableLibrary.equalHashProc)
    define(HashTableLibrary.eqvHashProc)
    define(HashTableLibrary.eqHashProc)
    define(Procedure("string-hash", stringHashVal))
    define(Procedure("string-ci-hash", stringCiHashVal))
    define(Procedure("symbol-hash", symbolHashVal))
  }
  
  func makeHashTable(_ capacity: Expr, _ eql: Expr, _ hsh: Expr, _ args: Arguments) throws -> Expr {
    guard args.count == 3 else {
      throw EvalError.argumentCountError(
        formals: 6, args: .pair(capacity, .pair(eql, .pair(hsh, .makeList(args)))))
    }
    let numBuckets = try capacity.asInt()
    let eqlProc = try eql.asProc()
    let hshProc = try hsh.asProc()
    if eqlProc == self.equalProc && hshProc == HashTableLibrary.equalHashProc {
      return .table(HashTable(capacity: numBuckets, mutable: true, equiv: .equal))
    } else if eqlProc == self.eqvProc && hshProc == HashTableLibrary.eqvHashProc {
      return .table(HashTable(capacity: numBuckets, mutable: true, equiv: .eqv))
    } else if eqlProc == self.eqProc && hshProc == HashTableLibrary.eqHashProc {
      return .table(HashTable(capacity: numBuckets, mutable: true, equiv: .eq))
    } else {
      let procs = HashTable.CustomProcedures(eql: eqlProc,
                                           hsh: hshProc,
                                           get: try args[args.startIndex].asProc(),
                                           add: try args[args.startIndex + 1].asProc(),
                                           del: try args[args.startIndex + 2].asProc())
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
    return .fixnum(Int64(try expr.asMap().count))
  }
  
  func hashTableLoad(_ expr: Expr) throws -> Expr {
    let map = try expr.asMap()
    return .makeNumber(Double(map.count) / Double(map.bucketCount))
  }
  
  func hashTableGet(_ args: Arguments) throws -> (Procedure, [Expr]) {
    try EvalError.assert(args, count: 2)
    let map = try args[args.startIndex].asMap()
    let key = args[args.startIndex + 1]
    guard case .custom(let procs) = map.equiv else {
      return (BaseLibrary.idProc, [map.get(key) ?? .false])
    }
    return (procs.get, [.table(map), .procedure(self.bucketsProc), key])
  }
  
  func hashTableAdd(_ args: Arguments) throws -> (Procedure, [Expr]) {
    try EvalError.assert(args, count: 3)
    let map = try args.first!.asMap()
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
    let map = try args.first!.asMap()
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
    let map = try expr.asMap()
    return .table(HashTable(copy: map, mutable: mutable == .true))
  }
  
  func hashTableClear(_ expr: Expr, k: Expr?) throws -> Expr {
    let map = try expr.asMap()
    guard map.mutable else {
      throw EvalError.attemptToModifyImmutableData(.table(map))
    }
    guard try map.clear(k?.asInt()) else {
      preconditionFailure("trying to clear immutable hash map")
    }
    return .void
  }
  
  func hashTableKeys(_ expr: Expr) throws -> Expr {
    return .vector(Collection(kind: .immutableVector, exprs: try expr.asMap().keys))
  }
  
  func hashTableValues(_ expr: Expr) throws -> Expr {
    return .vector(Collection(kind: .immutableVector, exprs: try expr.asMap().values))
  }
  
  func hashTableEntries(_ expr: Expr) throws -> Expr {
    return .undef
  }
  
  func hashTableKeyList(_ expr: Expr) throws -> Expr {
    return try expr.asMap().keyList()
  }
  
  func hashTableValueList(_ expr: Expr) throws -> Expr {
    return try expr.asMap().valueList()
  }
  
  func hashTableEntryList(_ expr: Expr) throws -> Expr {
    return .undef
  }
  
  func hashTableToAlist(_ expr: Expr) throws -> Expr {
    return try expr.asMap().entryList()
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
    switch try expr.asMap().equiv {
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
    switch try expr.asMap().equiv {
    case .eq, .eqv, .equal:
      return .false
    case .custom(let procs):
      return .procedure(procs.hsh)
    }
  }
  
  fileprivate static func eqHashVal(_ expr: Expr) -> Expr {
    return .fixnum(Int64(eqHash(expr)))
  }
  
  fileprivate static func eqvHashVal(_ expr: Expr) -> Expr {
    return .fixnum(Int64(eqvHash(expr)))
  }
  
  fileprivate static func equalHashVal(_ expr: Expr) -> Expr {
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
  
  fileprivate func hBuckets(_ expr: Expr, hval: Expr?) throws -> Expr {
    let map = try expr.asMap()
    if let hashValue = try hval?.asInteger() {
      return map.bucketList(Int(hashValue % Int64(map.bucketCount)))
    } else {
      return map.bucketList()
    }
  }
  
  fileprivate func hBucketAdd(_ expr: Expr, hval: Expr, key: Expr, value: Expr) throws -> Expr {
    guard case .table(let map) = expr else {
      throw EvalError.typeError(expr, [.tableType])
    }
    if !key.isSimple || !value.isSimple {
      self.context.objects.manage(map)
    }
    map.add(Int(try hval.asInteger() % Int64(map.bucketCount)), key, value)
    return .void
  }
  
  fileprivate func hBucketRepl(_ expr: Expr, hval: Expr, bucket: Expr) throws -> Expr {
    guard case .table(let map) = expr else {
      throw EvalError.typeError(expr, [.tableType])
    }
    map.replace(Int(try hval.asInteger() % Int64(map.bucketCount)), bucket)
    return .void
  }
}
