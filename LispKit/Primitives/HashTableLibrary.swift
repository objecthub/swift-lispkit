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
  
  private static let bucketsProc   = Procedure("_buckets", HashTableLibrary.hashBuckets)
  private static let bucketAddProc = Procedure("_bucket-add!", HashTableLibrary.hashBucketAdd)
  private static let bucketDelProc = Procedure("_bucket-del!", HashTableLibrary.hashBucketDel)
  
  private let updateBoxProc: Procedure
  private let equalProc: Procedure
  private let eqvProc: Procedure
  private let eqProc: Procedure
  
  /// Import
  public required init(_ context: Context) {
    self.updateBoxProc = Library.importProc(from: context, name: "update-box!")
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
      "  (letrec ((k (if (pair? size) (car size) 257))" +
      "           (find (lambda (key bs)" +
      "                   (if (pair? bs)" +
      "                         (if (eql (car (car bs)) key) (car bs) (find key (cdr bs)))" +
      "                         #f))))" +
      "    (_make-hashtable k eql hash" +
      "      (lambda (ht buckets key)" +
      "        (if (find key (buckets ht (hash key))) #t #f))" +
      "      (lambda (ht buckets key def)" +
      "        (let ((b (find key (buckets ht (hash key)))))" +
      "          (if b (unbox (cdr b)) def)))" +
      "      (lambda (ht buckets add key value)" +
      "        (let* ((h (hash key))" +
      "               (b (find key (buckets ht h))))" +
      "          (if b (set-box! (cdr b) value)" +
      "                (begin (add ht h key value)" +
      "                       (if (> (hashtable-load ht) 0.75)" +
      "                           (let ((ms (buckets ht)))" +
      "                             (hashtable-clear! ht (+ (* (hashtable-size ht) 3) 1))" +
      "                             (for-each (lambda (m)" +
      "                               (add ht (hash (car m)) (car m) (unbox (cdr m))))" +
      "                               ms)))))))" +
      "      (lambda (ht buckets add key proc def)" +
      "        (let* ((h (hash key))" +
      "               (b (find key (buckets ht h))))" +
      "          (if b (set-box! (cdr b) (proc (unbox (cdr b))))" +
      "                (add ht h key (proc def)))))" +
      "      (lambda (ht buckets del key)" +
      "        (let* ((h (hash key))" +
      "               (b (find key (buckets ht h))))" +
      "          (if b (del ht h (cdr b)) (void)))))))")
    define(Procedure("hashtable?", isHashTable))
    define(Procedure("eq-hashtable?", isEqHashTable))
    define(Procedure("eqv-hashtable?", isEqvHashTable))
    define(Procedure("equal-hashtable?", isEqualHashTable))
    define(Procedure("hashtable-mutable?", isHashTableMutable))
    define(Procedure("hashtable-size", hashTableSize))
    define(Procedure("hashtable-load", hashTableLoad))
    define(Procedure("hashtable-contains", hashTableContains))
    define(Procedure("hashtable-ref", hashTableRef))
    define(Procedure("hashtable-set!", hashTableSet))
    define(Procedure("hashtable-update!", hashTableUpdate))
    define(Procedure("hashtable-delete!", hashTableDelete))
    define(Procedure("hashtable-copy", hashTableCopy))
    define(Procedure("hashtable-clear!", hashTableClear))
    define(Procedure("hashtable-keys", hashTableKeys))
    define(Procedure("hashtable-values", hashTableValues))
    define(Procedure("hashtable->alist", hashTableToAlist))
    define(Procedure("alist->eq-hashtable", alistToEqHashTable))
    define(Procedure("alist->eqv-hashtable", alistToEqvHashTable))
    define(Procedure("alist->equal-hashtable", alistToEqualHashTable))
    // TODO: enable hashtable-entries once there is support for multiple return values
    // define(Procedure("hashtable-entries", hashTableEntries))
    define(Procedure("hashtable-equivalence-function", hashTableEquivalenceFunction))
    define(Procedure("hashtable-hash-function", hashTableHashFunction))
    define(Procedure("eq-hash", eqHashVal))
    define(Procedure("eqv-hash", eqvHashVal))
    define(Procedure("equal-hash", equalHashVal))
    define(Procedure("string-hash", stringHashVal))
    define(Procedure("string-ci-hash", stringCiHashVal))
    define(Procedure("symbol-hash", symbolHashVal))
  }
  
  func makeHashTable(capacity: Expr, _ eql: Expr, _ hsh: Expr, _ args: Arguments) throws -> Expr {
    guard args.count == 5 else {
      throw EvalError.ArgumentCountError(
        formals: 8, args: .Pair(capacity, .Pair(eql, .Pair(hsh, .List(args)))))
    }
    let procs = HashTable.CustomProcedures(eql: try eql.asProc(),
                                           hsh: try hsh.asProc(),
                                           has: try args[args.startIndex].asProc(),
                                           get: try args[args.startIndex + 1].asProc(),
                                           set: try args[args.startIndex + 2].asProc(),
                                           upd: try args[args.startIndex + 3].asProc(),
                                           del: try args[args.startIndex + 4].asProc())
    return .Map(HashTable(capacity: try capacity.asInt(), mutable: true, equiv: .Custom(procs)))
  }
  
  func makeEqHashTable(capacity: Expr?) throws -> Expr {
    return .Map(HashTable(capacity: try capacity?.asInt() ?? 257, mutable: true, equiv: .Eq))
  }
  
  func makeEqvHashTable(capacity: Expr?) throws -> Expr {
    return .Map(HashTable(capacity: try capacity?.asInt() ?? 257, mutable: true, equiv: .Eqv))
  }
  
  func makeEqualHashTable(capacity: Expr?) throws -> Expr {
    return .Map(HashTable(capacity: try capacity?.asInt() ?? 257, mutable: true, equiv: .Equal))
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
  
  func hashTableContains(args: Arguments) throws -> (Procedure, [Expr]) {
    try EvalError.assert(args, count: 2)
    let map = try args.first!.asMap()
    let key = args[args.startIndex + 1]
    guard case .Custom(let procs) = map.equiv else {
      return (BaseLibrary.idProc, [.Boolean(map.get(key) != nil)])
    }
    return (procs.has, [.Map(map), .Proc(HashTableLibrary.bucketsProc), key])
  }
  
  func hashTableRef(args: Arguments) throws -> (Procedure, [Expr]) {
    try EvalError.assert(args, count: 3)
    let map = try args[args.startIndex].asMap()
    let key = args[args.startIndex + 1]
    let def = args[args.startIndex + 2]
    guard case .Custom(let procs) = map.equiv else {
      return (BaseLibrary.idProc, [map.get(key) ?? def])
    }
    return (procs.get, [.Map(map),
                        .Proc(HashTableLibrary.bucketsProc),
                        key,
                        def])
  }
  
  func hashTableSet(args: Arguments) throws -> (Procedure, [Expr]) {
    try EvalError.assert(args, count: 3)
    let map = try args.first!.asMap()
    let key = args[args.startIndex + 1]
    let value = args[args.startIndex + 2]
    guard map.mutable else {
      throw EvalError.AttemptToModifyImmutableData(.Map(map))
    }
    guard case .Custom(let procs) = map.equiv else {
      guard map.set(key, value) else {
        preconditionFailure("trying to set mapping in immutable hash map")
      }
      return (BaseLibrary.idProc, [.Void])
    }
    // TODO: Deal with immutable hashtables
    return (procs.set, [.Map(map),
                        .Proc(HashTableLibrary.bucketsProc),
                        .Proc(HashTableLibrary.bucketAddProc),
                        key,
                        value])
  }
  
  func hashTableUpdate(args: Arguments) throws -> (Procedure, [Expr]) {
    try EvalError.assert(args, count: 4)
    let map = try args.first!.asMap()
    let key = args[args.startIndex + 1]
    let proc = args[args.startIndex + 2]
    let def = args[args.startIndex + 3]
    guard map.mutable else {
      throw EvalError.AttemptToModifyImmutableData(.Map(map))
    }
    guard case .Custom(let procs) = map.equiv else {
      let hashValue = map.hash(key)
      guard let cell = map.getCell(key, hashValue) ?? map.addCell(key, def, hashValue) else {
        preconditionFailure("trying to update mapping in immutable hash map")
      }
      return (self.updateBoxProc, [.Box(cell), proc])
    }
    // TODO: Deal with immutable hashtables
    return (procs.upd, [.Map(map),
                        .Proc(HashTableLibrary.bucketsProc),
                        .Proc(HashTableLibrary.bucketAddProc),
                        key,
                        proc,
                        def])
  }
  
  func hashTableDelete(args: Arguments) throws -> (Procedure, [Expr]) {
    try EvalError.assert(args, count: 2)
    let map = try args.first!.asMap()
    let key = args[args.startIndex + 1]
    guard map.mutable else {
      throw EvalError.AttemptToModifyImmutableData(.Map(map))
    }
    guard case .Custom(let procs) = map.equiv else {
      guard map.remove(key) else {
        preconditionFailure("trying to delete mapping in immutable hash map")
      }
      return (BaseLibrary.idProc, [.Void])
    }
    return (procs.del, [.Map(map),
                        .Proc(HashTableLibrary.bucketsProc),
                        .Proc(HashTableLibrary.bucketDelProc),
                        key])
  }
  
  func hashTableCopy(expr: Expr, mutable: Expr?) throws -> Expr {
    return .Map(HashTable(copy: try expr.asMap(), mutable: mutable == .True))
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
    return try expr.asMap().keys()
  }
  
  func hashTableValues(expr: Expr) throws -> Expr {
    return try expr.asMap().values()
  }
  
  func hashTableEntries(expr: Expr) throws -> Expr {
    return .Undef
  }
  
  func hashTableToAlist(expr: Expr) throws -> Expr {
    return try expr.asMap().alist()
  }
  
  private func newHashTable(equiv: HashTable.Equivalence, capacity: Int? = nil) -> HashTable {
    return HashTable(capacity: capacity ?? 257, mutable: true, equiv: equiv)
  }
  
  private func enterAlist(expr: Expr, into table: HashTable) throws {
    var list = expr
    while case .Pair(.Pair(let key, let value), let cdr) = list {
      table.set(key, value)
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
  
  func eqHashVal(expr: Expr) -> Expr {
    return .Fixnum(Int64(eqHash(expr)))
  }
  
  func eqvHashVal(expr: Expr) -> Expr {
    return .Fixnum(Int64(eqvHash(expr)))
  }
  
  func equalHashVal(expr: Expr) -> Expr {
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
  
  private static func hashBuckets(expr: Expr, hval: Expr?) throws -> Expr {
    let map = try expr.asMap()
    if let hashValue = try hval?.asInteger() {
      return map.bucketList(Int(hashValue % Int64(map.bucketCount)))
    } else {
      return map.bucketList()
    }
  }
  
  private static func hashBucketAdd(expr: Expr,
                                    hval: Expr,
                                    key: Expr,
                                    value: Expr) throws -> Expr {
    guard case .Map(let map) = expr else {
      throw EvalError.TypeError(expr, [.MapType])
    }
    map.add(Int(try hval.asInteger() % Int64(map.bucketCount)), key, value)
    return .Void
  }
  
  private static func hashBucketDel(expr: Expr,
                                    hval: Expr,
                                    value: Expr) throws -> Expr {
    guard case .Map(let map) = expr else {
      throw EvalError.TypeError(expr, [.MapType])
    }
    guard case .Box(let cell) = value else {
      throw EvalError.TypeError(expr, [.BoxType])
    }
    map.remove(Int(try hval.asInteger() % Int64(map.bucketCount)), cell)
    return .Void
  }
}
