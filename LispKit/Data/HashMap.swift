//
//  HashTable.swift
//  LispKit
//
//  Created by Matthias Zenger on 14/07/2016.
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
/// `HashTable` implements hash maps natively.
///
public final class HashTable: ManagedObject, CustomStringConvertible {
  
  public struct CustomProcedures {
    let eql: Procedure
    let hsh: Procedure
    let get: Procedure
    let add: Procedure
    let del: Procedure
  }
  
  public enum Equivalence {
    case eq
    case eqv
    case equal
    case custom(CustomProcedures)
  }
  
  /// Maintain object statistics.
  internal static let stats = Stats("HashTable")
  
  /// The hash buckets.
  fileprivate var buckets: [Expr]
  
  /// Number of mappings in this hash table
  public fileprivate(set) var count: Int
  
  /// Is this `HashTable` object mutable?
  public let mutable: Bool
  
  /// What equivalence relation is used?
  public fileprivate(set) var equiv: Equivalence
  
  /// Update object statistics.
  deinit {
    HashTable.stats.dealloc()
  }
  
  /// Create a new empty hash table with the given size.
  public init(capacity: Int = 127, mutable: Bool = true, equiv: Equivalence) {
    self.buckets = [Expr](repeating: .null, count: capacity)
    self.count = 0
    self.mutable = mutable
    self.equiv = equiv
    super.init(HashTable.stats)
  }
  
  /// Create a copy of another hash table. Make it immutable if `mutable` is set to false.
  public init(copy other: HashTable, mutable: Bool = true) {
    self.buckets = [Expr]()
    for i in 0..<other.buckets.count {
      self.buckets.append(other.buckets[i])
    }
    self.count = other.count
    self.mutable = mutable
    self.equiv = other.equiv
    super.init(HashTable.stats)
  }
  
  /// A string representation of this variable.
  public var description: String {
    return "«\(self.buckets)»"
  }
  
  /// Clear entries in hash table and resize if capacity is supposed to change. This
  /// method creates a new array of buckets if the `capacity` parameter is provided.
  public func clear(_ capacity: Int? = nil) -> Bool {
    guard self.mutable else {
      return false
    }
    if let capacity = capacity {
      self.buckets = [Expr](repeating: .null, count: capacity)
    } else {
      for i in self.buckets.indices {
        self.buckets[i] = .null
      }
    }
    self.count = 0
    return true
  }
  
  /// Recreates the hash table for the given capacity; this only works for non-custom
  /// hash tables.
  public func rehash(_ capacity: Int) {
    // Skip custom hash tables
    if case .custom(_) = self.equiv {
      return
    }
    // Save old bucket array
    let oldBuckets = self.buckets
    // Create new bucket array
    self.buckets = [Expr](repeating: .null, count: capacity)
    // Rehash the mappings
    for bucket in oldBuckets {
      var current = bucket
      while case .pair(.pair(let key, let value), let next) = current {
        let bid = self.hash(key) % capacity
        self.buckets[bid] = .pair(.pair(key, value), self.buckets[bid])
        current = next
      }
    }
  }
  
  /// Array of mappings
  public var mappings: [(Expr, Expr)] {
    var res = [(Expr, Expr)]()
    for bucket in self.buckets {
      HashTable.insertMappings(into: &res, from: bucket)
    }
    return res
  }
  
  /// Insert the mappings from the given bucket list into the array `arr`.
  fileprivate static func insertMappings(into arr: inout [(Expr, Expr)], from bucket: Expr) {
    var current = bucket
    while case .pair(.pair(let key, let value), let next) = current {
      arr.append((key, value))
      current = next
    }
  }
  
  /// Returns the number of hash buckets in the hash table.
  public var bucketCount: Int {
    return self.buckets.count
  }
  
  /// Returns the mappings in the hash table as an association list with boxed values
  public func bucketList(_ bid: Int? = nil) -> Expr {
    if let bid = bid {
      return self.buckets[bid]
    } else {
      var res: Expr = .null
      for bucket in self.buckets {
        var current = bucket
        while case .pair(let mapping, let next) = current {
          res = .pair(mapping, res)
          current = next
        }
      }
      return res
    }
  }
  
  // Key/value accessors
  
  /// Returns a list of all keys in the hash table
  public var keys: [Expr] {
    var res = [Expr]()
    for bucket in self.buckets {
      var current = bucket
      while case .pair(.pair(let key, _), let next) = current {
        res.append(key)
        current = next
      }
    }
    return res
  }
  
  /// Returns a list of all keys in the hash table
  public func keyList() -> Expr {
    var res: Expr = .null
    for bucket in self.buckets {
      var current = bucket
      while case .pair(.pair(let key, _), let next) = current {
        res = .pair(key, res)
        current = next
      }
    }
    return res
  }
  
  /// Returns a list of all values in the hash table
  public var values: [Expr] {
    var res = [Expr]()
    for bucket in self.buckets {
      var current = bucket
      while case .pair(.pair(_, let value), let next) = current {
        res.append(value)
        current = next
      }
    }
    return res
  }
  
  /// Returns a list of all values in the hash table
  public func valueList() -> Expr {
    var res: Expr = .null
    for bucket in self.buckets {
      var current = bucket
      while case .pair(.pair(_, let value), let next) = current {
        res = .pair(value, res)
        current = next
      }
    }
    return res
  }
  
  /// Array of mappings
  public var entries: [(Expr, Expr)] {
    var res = [(Expr, Expr)]()
    for bucket in self.buckets {
      HashTable.insertMappings(into: &res, from: bucket)
    }
    return res
  }
  
  /// Returns the mappings in the hash table as an association list
  public func entryList() -> Expr {
    var res: Expr = .null
    for bucket in self.buckets {
      var current = bucket
      while case .pair(.pair(let key, let value), let next) = current {
        res = .pair(.pair(key, value), res)
        current = next
      }
    }
    return res
  }
  
  // Setting and getting mappings
  
  /// Returns the value associated with `key` in bucket `bid`.
  public func get(_ bid: Int, _ key: Expr, _ eql: (Expr, Expr) -> Bool) -> Expr? {
    var current = self.buckets[bid]
    while case .pair(.pair(let k, let v), let next) = current {
      if eql(key, k) {
        return .pair(k, v)
      }
      current = next
    }
    return nil
  }

  /// Adds a new mapping to bucket `bid`. If the hash table load factor is above 0.75, rehash
  /// the whole table.
  @discardableResult public func add(_ bid: Int, _ key: Expr, _ value: Expr) -> Bool {
    guard self.mutable else {
      return false
    }
    self.buckets[bid] = .pair(.pair(key, value), self.buckets[bid])
    self.count += 1
    if (self.count * 4 / self.buckets.count) > 3 {
      self.rehash(self.buckets.count * 2 + 1)
    }
    return true
  }
  
  /// Replaces bucket `bid` with a new bucket expression.
  @discardableResult public func replace(_ bid: Int, _ bucket: Expr) -> Bool {
    guard self.mutable else {
      return false
    }
    self.count += bucket.length - self.buckets[bid].length
    self.buckets[bid] = bucket
    if (self.count * 4 / self.buckets.count) > 3 {
      self.rehash(self.buckets.count * 2 + 1)
    }
    return true
  }
  
  // Support for non-custom HashMaps
  
  /// Compares two expressions for non-custom HashMaps.
  internal func eql(_ left: Expr, _ right: Expr) -> Bool {
    switch self.equiv {
      case .eq:
        return eqExpr(right, left)
      case .eqv:
        return eqvExpr(right, left)
      case .equal:
        return equalExpr(right, left)
      case .custom(_):
        preconditionFailure("cannot access custom HashTable internally")
    }
  }
  
  /// Computes the hash value for non-custom HashMaps.
  internal func hash(_ expr: Expr) -> Int {
    switch self.equiv {
      case .eq:
        return eqHash(expr)
      case .eqv:
        return eqvHash(expr)
      case .equal:
        return equalHash(expr)
      case .custom(_):
        preconditionFailure("cannot access custom HashTable internally")
    }
  }
  
  public func get(_ key: Expr) -> Expr? {
    return self.get(self.hash(key) % self.buckets.count, key, self.eql)
  }
  
  @discardableResult public func set(key: Expr, mapsTo value: Expr) -> Bool {
    return self.remove(key: key) != nil && self.add(key: key, mapsTo: value)
  }
  
  @discardableResult public func add(key: Expr, mapsTo value: Expr) -> Bool {
    return self.add(self.hash(key) % self.buckets.count, key, value)
  }
  
  public func remove(key: Expr) -> Expr? {
    guard self.mutable else {
      return nil
    }
    let bid = self.hash(key) % self.buckets.count
    var ms = [(Expr, Expr)]()
    var current = self.buckets[bid]
    while case .pair(.pair(let k, let v), let next) = current {
      if eql(key, k) {
        var bucket = next
        for (k2, v2) in ms.reversed() {
          bucket = .pair(.pair(k2, v2), bucket)
        }
        self.buckets[bid] = bucket
        self.count -= 1
        return .pair(k, v)
      }
      ms.append((k, v))
      current = next
    }
    return .false
  }
  
  // Support for managed objects
  
  /// Mark hash table content.
  public override func mark(_ tag: UInt8) {
    if self.tag != tag {
      self.tag = tag
      for bucket in self.buckets {
        var current = bucket
        while case .pair(.pair(let key, .box(let cell)), let next) = current {
          key.mark(tag)
          cell.mark(tag)
          current = next
        }
      }
      if case .custom(let procs) = self.equiv {
        procs.eql.mark(tag)
        procs.hsh.mark(tag)
        procs.get.mark(tag)
        procs.add.mark(tag)
        procs.del.mark(tag)
      }
    }
  }
  
  /// Clear variable value
  public override func clean() {
    self.buckets = [Expr](repeating: .null, count: 1)
    self.count = 0
    self.equiv = .eq
  }
}
