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
  
  private static let bucketsProc   = Procedure("_hash-buckets", HashTableLibrary.hashBuckets)
  private static let bucketAddProc = Procedure("_hash-bucket-add!", HashTableLibrary.hashBucketAdd)
  private static let bucketDelProc = Procedure("_hash-bucket-del!", HashTableLibrary.hashBucketDel)
  
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
      "    (_make-hashtable k" +
      "      (lambda (ht buckets key)" +
      "        (if (find key (buckets ht (hash key))) #t #f))" +
      "      (lambda (ht buckets key def)" +
      "        (let ((b (find key (buckets ht (hash key)))))" +
      "          (if b (unbox (cdr b)) def)))" +
      "      (lambda (ht buckets add key value)" +
      "        (let* ((h (hash key))" +
      "               (b (find key (buckets ht h))))" +
      "          (if b (set-box! (cdr b) value) (add ht h key value))))" +
      "      (lambda (ht buckets del key)" +
      "        (let* ((h (hash key))" +
      "               (b (find key (buckets ht h))))" +
      "          (if b (del ht h (cdr b)) (void)))))))")
    define(Procedure("hashtable?", isHashTable))
    define(Procedure("hashtable-ref", hashTableRef))
    define(Procedure("hashtable-set!", hashTableSet))
    define(Procedure("hashtable-delete!", hashTableDelete))
    define(Procedure("hashtable-contains", hashTableContains))
    define(Procedure("equal-hash", equalHash))
  }
  
  func makeHashTable(capacity: Expr, _ has: Expr, _ get: Expr, args: Arguments) throws -> Expr {
    guard args.count == 0 || args.count == 2 else {
      throw EvalError.ArgumentCountError(formals: 5,
                                         args: .Pair(capacity, .Pair(has, .Pair(get, .List(args)))))
    }
    let set: Expr? = args.first
    let del: Expr? = (args.count == 2) ? args[args.startIndex + 1] : nil
    return .Map(HashTable(capacity: try capacity.asInt(),
                          mutable: true,
                          equiv: .Custom(has: try has.asProc(),
                                         get: try get.asProc(),
                                         set: try set?.asProc(),
                                         del: try del?.asProc())))
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
  
  func hashTableRef(args: Arguments) throws -> (Procedure, [Expr]) {
    try EvalError.assert(args, count: 3)
    let map = try args.first!.asMap()
    let key = args[args.startIndex + 1]
    let def = args[args.startIndex + 2]
    guard case .Custom(_, let get, _, _) = map.equiv else {
      return (BaseLibrary.idProc, [map.get(key) ?? def])
    }
    return (get, [.Map(map),
                  .Proc(HashTableLibrary.bucketsProc),
                  key,
                  def])
  }
  
  func hashTableSet(args: Arguments) throws -> (Procedure, [Expr]) {
    try EvalError.assert(args, count: 3)
    let map = try args.first!.asMap()
    let key = args[args.startIndex + 1]
    let value = args[args.startIndex + 2]
    guard case .Custom(_, _, let set, _) = map.equiv else {
      map.set(key, value)
      return (BaseLibrary.idProc, [.Void])
    }
    // TODO: Deal with immutable hashtables
    return (set!, [.Map(map),
                  .Proc(HashTableLibrary.bucketsProc),
                  .Proc(HashTableLibrary.bucketAddProc),
                  key,
                  value])
  }
  
  func hashTableDelete(args: Arguments) throws -> (Procedure, [Expr]) {
    try EvalError.assert(args, count: 2)
    let map = try args.first!.asMap()
    let key = args[args.startIndex + 1]
    guard case .Custom(_, _, _, let del) = map.equiv else {
      map.remove(key)
      return (BaseLibrary.idProc, [.Void])
    }
    return (del!, [.Map(map),
                  .Proc(HashTableLibrary.bucketsProc),
                  .Proc(HashTableLibrary.bucketDelProc),
                  key])
  }
  
  func hashTableContains(args: Arguments) throws -> (Procedure, [Expr]) {
    try EvalError.assert(args, count: 2)
    let map = try args.first!.asMap()
    let key = args[args.startIndex + 1]
    guard case .Custom(let has, _, _, _) = map.equiv else {
      return (BaseLibrary.idProc, [.Boolean(map.get(key) != nil)])
    }
    return (has, [.Map(map), .Proc(HashTableLibrary.bucketsProc), key])
  }
  
  func equalHash(expr: Expr) -> Expr {
    return .Fixnum(Int64(expr.hashValue))
  }
  
  private static func hashBuckets(expr: Expr, hval: Expr) throws -> Expr {
    guard case .Map(let map) = expr else {
      throw EvalError.TypeError(expr, [.MapType])
    }
    return map.alist(Int(try hval.asInteger() % Int64(map.count)))
  }
  
  private static func hashBucketAdd(expr: Expr, hval: Expr, key: Expr, value: Expr) throws -> Expr {
    guard case .Map(let map) = expr else {
      throw EvalError.TypeError(expr, [.MapType])
    }
    map.add(Int(try hval.asInteger() % Int64(map.count)), key, value)
    return .Void
  }
  
  private static func hashBucketDel(expr: Expr, hval: Expr, value: Expr) throws -> Expr {
    guard case .Map(let map) = expr else {
      throw EvalError.TypeError(expr, [.MapType])
    }
    guard case .Box(let cell) = value else {
      throw EvalError.TypeError(expr, [.BoxType])
    }
    map.remove(Int(try hval.asInteger() % Int64(map.count)), cell)
    return .Void
  }
}
