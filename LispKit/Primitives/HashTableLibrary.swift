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


public final class HashTableLibrary: Library {
  
  private static let bucketsProc   = Procedure("_buckets", HashTableLibrary.hBuckets)
  private static let bucketAddProc = Procedure("_bucket-add!", HashTableLibrary.hBucketAdd)
  private static let bucketDelProc = Procedure("_bucket-repl!", HashTableLibrary.hBucketRepl)
  private static let equalHashProc = Procedure("equal-hash", HashTableLibrary.equalHashVal)
  private static let eqvHashProc   = Procedure("eqv-hash", HashTableLibrary.eqvHashVal)
  private static let eqHashProc    = Procedure("eq-hash", HashTableLibrary.eqHashVal)
  
  private static let defaultCapacity = 127
  
  private let equalProc: Procedure
  private let eqvProc: Procedure
  private let eqProc: Procedure
  
  /// Import
  public required init(_ context: Context) {
    self.equalProc = Library.importProc(from: context, name: "equal?")
    self.eqvProc = Library.importProc(from: context, name: "eqv?")
    self.eqProc = Library.importProc(from: context, name: "eq?")
    super.init(context)
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
  
  func makeHashTable(capacity: Expr, _ eql: Expr, _ hsh: Expr, _ args: Arguments) throws -> Expr {
    guard args.count == 3 else {
      throw EvalError.ArgumentCountError(
        formals: 6, args: .Pair(capacity, .Pair(eql, .Pair(hsh, .List(args)))))
    }
    let numBuckets = try capacity.asInt()
    let eqlProc = try eql.asProc()
    let hshProc = try hsh.asProc()
    if eqlProc == self.equalProc && hshProc == HashTableLibrary.equalHashProc {
      return .Map(HashMap(capacity: numBuckets, mutable: true, equiv: .Equal))
    } else if eqlProc == self.eqvProc && hshProc == HashTableLibrary.eqvHashProc {
      return .Map(HashMap(capacity: numBuckets, mutable: true, equiv: .Eqv))
    } else if eqlProc == self.eqProc && hshProc == HashTableLibrary.eqHashProc {
      return .Map(HashMap(capacity: numBuckets, mutable: true, equiv: .Eq))
    } else {
      let procs = HashMap.CustomProcedures(eql: eqlProc,
                                           hsh: hshProc,
                                           get: try args[args.startIndex].asProc(),
                                           add: try args[args.startIndex + 1].asProc(),
                                           del: try args[args.startIndex + 2].asProc())
      return .Map(HashMap(capacity: numBuckets, mutable: true, equiv: .Custom(procs)))
    }
  }
  
  func makeEqHashTable(capacity: Expr?) throws -> Expr {
    let numBuckets = try capacity?.asInt() ?? HashTableLibrary.defaultCapacity
    return .Map(HashMap(capacity: numBuckets, mutable: true, equiv: .Eq))
  }
  
  func makeEqvHashTable(capacity: Expr?) throws -> Expr {
    let numBuckets = try capacity?.asInt() ?? HashTableLibrary.defaultCapacity
    return .Map(HashMap(capacity: numBuckets, mutable: true, equiv: .Eqv))
  }
  
  func makeEqualHashTable(capacity: Expr?) throws -> Expr {
    let numBuckets = try capacity?.asInt() ?? HashTableLibrary.defaultCapacity
    return .Map(HashMap(capacity: numBuckets, mutable: true, equiv: .Equal))
  }
  
  func isHashTable(expr: Expr) -> Expr {
    guard case .Map(_) = expr else {
      return .False
    }
    return .True
  }
  
  func isEqHashTable(expr: Expr) -> Expr {
    guard case .Map(let table) = expr, .Eq = table.equiv else {
      return .False
    }
    return .True
  }
  
  func isEqvHashTable(expr: Expr) -> Expr {
    guard case .Map(let table) = expr, .Eqv = table.equiv else {
      return .False
    }
    return .True
  }
  
  func isEqualHashTable(expr: Expr) -> Expr {
    guard case .Map(let table) = expr, .Equal = table.equiv else {
      return .False
    }
    return .True
  }
  
  func isHashTableMutable(expr: Expr) -> Expr {
    guard case .Map(let table) = expr else {
      return .False
    }
    return .Boolean(table.mutable)
  }
  
  func hashTableSize(expr: Expr) throws -> Expr {
    return .Fixnum(Int64(try expr.asMap().count))
  }
  
  func hashTableLoad(expr: Expr) throws -> Expr {
    let map = try expr.asMap()
    return .Number(Double(map.count) / Double(map.bucketCount))
  }
  
  func hashTableGet(args: Arguments) throws -> (Procedure, [Expr]) {
    try EvalError.assert(args, count: 2)
    let map = try args[args.startIndex].asMap()
    let key = args[args.startIndex + 1]
    guard case .Custom(let procs) = map.equiv else {
      if let value = map.get(key) {
        return (BaseLibrary.idProc, [.Pair(key, value)])
      } else {
        return (BaseLibrary.idProc, [.False])
      }
    }
    return (procs.get, [.Map(map), .Proc(HashTableLibrary.bucketsProc), key])
  }
  
  func hashTableAdd(args: Arguments) throws -> (Procedure, [Expr]) {
    try EvalError.assert(args, count: 3)
    let map = try args.first!.asMap()
    let key = args[args.startIndex + 1]
    let value = args[args.startIndex + 2]
    guard map.mutable else {
      throw EvalError.AttemptToModifyImmutableData(.Map(map))
    }
    guard case .Custom(let procs) = map.equiv else {
      guard map.add(key, value) else {
        preconditionFailure("trying to set mapping in immutable hash map")
      }
      return (BaseLibrary.idProc, [.Void])
    }
    return (procs.add, [.Map(map),
      .Proc(HashTableLibrary.bucketsProc),
      .Proc(HashTableLibrary.bucketAddProc),
      key,
      value])
  }
  
  func hashTableDelete(args: Arguments) throws -> (Procedure, [Expr]) {
    try EvalError.assert(args, count: 2)
    let map = try args.first!.asMap()
    let key = args[args.startIndex + 1]
    guard map.mutable else {
      throw EvalError.AttemptToModifyImmutableData(.Map(map))
    }
    guard case .Custom(let procs) = map.equiv else {
      guard let res = map.remove(key) else {
        preconditionFailure("trying to delete mapping in immutable hash map")
      }
      return (BaseLibrary.idProc, [res])
    }
    return (procs.del, [.Map(map),
                        .Proc(HashTableLibrary.bucketsProc),
                        .Proc(HashTableLibrary.bucketDelProc),
                        key])
  }
  
  func hashTableCopy(expr: Expr, mutable: Expr?) throws -> Expr {
    let map = try expr.asMap()
    return .Map(HashMap(copy: map, mutable: mutable == .True))
  }
  
  func hashTableClear(expr: Expr, k: Expr?) throws -> Expr {
    let map = try expr.asMap()
    guard map.mutable else {
      throw EvalError.AttemptToModifyImmutableData(.Map(map))
    }
    guard try map.clear(k?.asInt()) else {
      preconditionFailure("trying to clear immutable hash map")
    }
    return .Void
  }
  
  func hashTableKeys(expr: Expr) throws -> Expr {
    return .Vec(Vector(try expr.asMap().keys))
  }
  
  func hashTableValues(expr: Expr) throws -> Expr {
    return .Vec(Vector(try expr.asMap().values))
  }
  
  func hashTableEntries(expr: Expr) throws -> Expr {
    return .Undef
  }
  
  func hashTableKeyList(expr: Expr) throws -> Expr {
    return try expr.asMap().keyList()
  }
  
  func hashTableValueList(expr: Expr) throws -> Expr {
    return try expr.asMap().valueList()
  }
  
  func hashTableEntryList(expr: Expr) throws -> Expr {
    return .Undef
  }
  
  func hashTableToAlist(expr: Expr) throws -> Expr {
    return try expr.asMap().entryList()
  }
  
  private func newHashTable(equiv: HashMap.Equivalence, capacity: Int?) -> HashMap {
    let numBuckets = capacity ?? HashTableLibrary.defaultCapacity
    return HashMap(capacity: numBuckets, mutable: true, equiv: equiv)
  }
  
  private func enterAlist(expr: Expr, into map: HashMap) throws {
    var list = expr
    while case .Pair(.Pair(let key, let value), let cdr) = list {
      map.add(key, value)
      list = cdr
    }
    guard list.isNull else {
      throw EvalError.TypeError(expr, [.AssocListType])
    }
  }
  
  func alistToEqHashTable(expr: Expr, capacity: Expr?) throws -> Expr {
    let table = self.newHashTable(.Eq, capacity: try capacity?.asInt())
    try self.enterAlist(expr, into: table)
    return .Map(table)
  }
  
  func alistToEqvHashTable(expr: Expr, capacity: Expr?) throws -> Expr {
    let table = self.newHashTable(.Eqv, capacity: try capacity?.asInt())
    try self.enterAlist(expr, into: table)
    return .Map(table)
  }
  
  func alistToEqualHashTable(expr: Expr, capacity: Expr?) throws -> Expr {
    let table = self.newHashTable(.Equal, capacity: try capacity?.asInt())
    try self.enterAlist(expr, into: table)
    return .Map(table)
  }
  
  func hashTableEquivalenceFunction(expr: Expr) throws -> Expr {
    switch try expr.asMap().equiv {
    case .Eq:
      return .Proc(self.eqProc)
    case .Eqv:
      return .Proc(self.eqvProc)
    case .Equal:
      return .Proc(self.equalProc)
    case .Custom(let procs):
      return .Proc(procs.eql)
    }
  }
  
  func hashTableHashFunction(expr: Expr) throws -> Expr {
    switch try expr.asMap().equiv {
    case .Eq, .Eqv, .Equal:
      return .False
    case .Custom(let procs):
      return .Proc(procs.hsh)
    }
  }
  
  private static func eqHashVal(expr: Expr) -> Expr {
    return .Fixnum(Int64(eqHash(expr)))
  }
  
  private static func eqvHashVal(expr: Expr) -> Expr {
    return .Fixnum(Int64(eqvHash(expr)))
  }
  
  private static func equalHashVal(expr: Expr) -> Expr {
    return .Fixnum(Int64(equalHash(expr)))
  }
  
  func stringHashVal(expr: Expr) throws -> Expr {
    guard case .Str(_) = expr else {
      throw EvalError.TypeError(expr, [.StrType])
    }
    return .Fixnum(Int64(expr.hashValue))
  }
  
  func stringCiHashVal(expr: Expr) throws -> Expr {
    guard case .Str(let str) = expr else {
      throw EvalError.TypeError(expr, [.StrType])
    }
    return .Fixnum(Int64(str.lowercaseString.hashValue))
  }
  
  func symbolHashVal(expr: Expr) throws -> Expr {
    guard case .Sym(_) = expr else {
      throw EvalError.TypeError(expr, [.SymbolType])
    }
    return .Fixnum(Int64(expr.hashValue))
  }
  
  private static func hBuckets(expr: Expr, hval: Expr?) throws -> Expr {
    let map = try expr.asMap()
    if let hashValue = try hval?.asInteger() {
      return map.bucketList(Int(hashValue % Int64(map.bucketCount)))
    } else {
      return map.bucketList()
    }
  }
  
  private static func hBucketAdd(expr: Expr,
                                 hval: Expr,
                                 key: Expr,
                                 value: Expr) throws -> Expr {
    guard case .Map(let map) = expr else {
      throw EvalError.TypeError(expr, [.MapType])
    }
    map.add(Int(try hval.asInteger() % Int64(map.bucketCount)), key, value)
    return .Void
  }
  
  private static func hBucketRepl(expr: Expr, hval: Expr, bucket: Expr) throws -> Expr {
    guard case .Map(let map) = expr else {
      throw EvalError.TypeError(expr, [.MapType])
    }
    map.replace(Int(try hval.asInteger() % Int64(map.bucketCount)), bucket)
    return .Void
  }
}
