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
/// `HashTable` implements hash tables natively.
///
public final class HashTable: ManagedObject, CustomStringConvertible {
  
  public struct CustomProcedures {
    let eql: Procedure
    let hsh: Procedure
    let has: Procedure
    let get: Procedure
    let set: Procedure
    let upd: Procedure
    let del: Procedure
  }
  
  public enum Equivalence {
    case Eq
    case Eqv
    case Equal
    case Custom(CustomProcedures)
  }
  
  /// Maintain object statistics.
  internal static let stats = Stats("HashTable")
  
  /// The hash buckets.
  private var buckets: [Expr]
  
  /// Number of mappings in this hash table
  public private(set) var count: Int
  
  /// Is this `HashTable` object mutable?
  public let mutable: Bool
  
  /// What equivalence relation is used?
  public private(set) var equiv: Equivalence
  
  /// Update object statistics.
  deinit {
    HashTable.stats.dealloc()
  }
  
  /// Create a new empty hash table with the given size.
  public init(capacity: Int = 499,
              mutable: Bool = true,
              equiv: Equivalence) {
    self.buckets = [Expr](count: capacity, repeatedValue: .Null)
    self.count = 0
    self.mutable = mutable
    self.equiv = equiv
    super.init(HashTable.stats)
  }
  
  /// Create a copy of another hash table. Make it immutable if `mutable` is set to false.
  public init(copy other: HashTable, mutable: Bool = true) {
    self.buckets = [Expr]()
    for i in 0..<other.buckets.count {
      self.buckets.append(HashTable.copyBucket(other.buckets[i]))
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

  private static func copyBucket(other: Expr) -> Expr {
    var res: Expr = .Null
    if case .Pair(_, _) = other {
      var mappings = [(Expr, Expr)]()
      HashTable.insertMappings(into: &mappings, from: other)
      for i in 0..<mappings.count {
        let (key, value) = mappings[mappings.count - i - 1]
        res = .Pair(.Pair(key, .Box(Cell(value))), res)
      }
    }
    return res
  }
  
  /// Clear entries in hash table and resize if capacity is supposed to change. This
  /// method creates a new array of buckets if the `capacity` parameter is provided.
  public func clear(capacity: Int? = nil) -> Bool {
    guard self.mutable else {
      return false
    }
    if let capacity = capacity {
      self.buckets = [Expr](count: capacity, repeatedValue: .Null)
    } else {
      for i in self.buckets.indices {
        self.buckets[i] = .Null
      }
    }
    self.count = 0
    return true
  }
  
  /// Recreates the hash table for the given capacity; this only works for non-custom
  /// hash tables.
  public func rehash(capacity: Int) {
    // Skip custom hash tables
    if case .Custom(_) = self.equiv {
      return
    }
    // Save old bucket array
    let oldBuckets = self.buckets
    // Create new bucket array
    self.buckets = [Expr](count: capacity, repeatedValue: .Null)
    // Rehash the mappings
    for bucket in oldBuckets {
      var current = bucket
      while case .Pair(.Pair(let key, let value), let next) = current {
        let bid = self.hash(key) % capacity
        self.buckets[bid] = .Pair(.Pair(key, value), self.buckets[bid])
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
  private static func insertMappings(inout into arr: [(Expr, Expr)], from bucket: Expr) {
    var current = bucket
    while case .Pair(.Pair(let key, .Box(let cell)), let next) = current {
      arr.append((key, cell.value))
      current = next
    }
  }
  
  /// Returns the number of hash buckets in the hash table.
  public var bucketCount: Int {
    return self.buckets.count
  }
  
  /// Returns the mappings in the hash table as an association list with boxed values
  public func bucketList(bid: Int? = nil) -> Expr {
    if let bid = bid {
      return self.buckets[bid]
    } else {
      var res: Expr = .Null
      for bucket in self.buckets {
        var current = bucket
        while case .Pair(let mapping, let next) = current {
          res = .Pair(mapping, res)
          current = next
        }
      }
      return res
    }
  }
  
  /// Returns a list of all keys in the hash table
  public func keys() -> Expr {
    var res: Expr = .Null
    for bucket in self.buckets {
      var current = bucket
      while case .Pair(.Pair(let key, _), let next) = current {
        res = .Pair(key, res)
        current = next
      }
    }
    return res
  }
  
  /// Returns a list of all values in the hash table
  public func values() -> Expr {
    var res: Expr = .Null
    for bucket in self.buckets {
      var current = bucket
      while case .Pair(.Pair(_, .Box(let cell)), let next) = current {
        res = .Pair(cell.value, res)
        current = next
      }
    }
    return res
  }
  
  /// Returns the mappings in the hash table as an association list
  public func alist() -> Expr {
    var res: Expr = .Null
    for bucket in self.buckets {
      var current = bucket
      while case .Pair(.Pair(let key, .Box(let cell)), let next) = current {
        res = .Pair(.Pair(key, cell.value), res)
        current = next
      }
    }
    return res
  }
  
  // Setting and getting mappings
  
  /// Adds a new mapping to bucket at index `bid`. If the hash table load factor is
  /// above 0.75, re-hash the whole table.
  public func add(bid: Int, _ key: Expr, _ value: Expr) -> Cell? {
    guard self.mutable else {
      return nil
    }
    let cell = Cell(value)
    self.buckets[bid] = .Pair(.Pair(key, .Box(cell)), self.buckets[bid])
    self.count += 1
    if (self.count * 4 / self.buckets.count) > 3 {
      self.rehash(self.buckets.count * 2 + 1)
    }
    return cell
  }
  
  /// Removes a mapping identified by the boxed value `delete` in bucket at index `bid`
  public func remove(bid: Int, _ delete: Cell) -> Bool {
    guard self.mutable else {
      return false
    }
    var stack = [(Expr, Cell)]()
    var current = self.buckets[bid]
    while case .Pair(.Pair(let key, .Box(let cell)), let next) = current {
      guard cell !== delete else {
        var res = next
        for i in 0..<stack.count {
          let pair = stack[stack.count - i - 1]
          res = .Pair(.Pair(pair.0, .Box(pair.1)), res)
        }
        self.buckets[bid] = res
        self.count -= 1
        return true
      }
      stack.append((key, cell))
      current = next
    }
    return true
  }
  
  internal func getCell(key: Expr, _ hashValue: Int, _ eql: (Expr, Expr) -> Bool) -> Cell? {
    var current = self.buckets[hashValue % self.buckets.count]
    while case .Pair(.Pair(let k, .Box(let cell)), let next) = current {
      if eql(key, k) {
        return cell
      }
      current = next
    }
    return nil
  }
  
  internal func addCell(key: Expr, _ value: Expr, _ hashValue: Int) -> Cell? {
    return self.add(hashValue % self.buckets.count, key, value)
  }
  
  // Support for non-custom hashtables
  
  /// Compares two expressions for non-custom hashtables.
  internal func eq(left: Expr, _ right: Expr) -> Bool {
    switch self.equiv {
      case .Eq:
        return eqExpr(left, right)
      case .Eqv:
        return eqvExpr(left, right)
      case .Equal:
        return equalExpr(left, right)
      case .Custom(_):
        preconditionFailure("cannot access custom hashtable internally")
    }
  }
  
  /// Computes the hash value for non-custom hashtables.
  internal func hash(expr: Expr) -> Int {
    switch self.equiv {
      case .Eq:
        return eqHash(expr)
      case .Eqv:
        return eqvHash(expr)
      case .Equal:
        return equalHash(expr)
      case .Custom(_):
        preconditionFailure("cannot access custom hashtable internally")
    }
  }
  
  public func get(key: Expr) -> Expr? {
    return self.getCell(key)?.value
  }
  
  public func set(key: Expr, _ value: Expr) -> Bool {
    let hashValue = self.hash(key)
    if let cell = self.getCell(key, hashValue) {
      guard self.mutable else {
        return false
      }
      cell.value = value
      return true
    } else {
      return self.addCell(key, value, hashValue) != nil
    }
  }
  
  public func remove(key: Expr) -> Bool {
    let hashValue = self.hash(key)
    if let cell = self.getCell(key, hashValue) {
      return self.remove(hashValue % self.buckets.count, cell)
    }
    return true
  }
  
  internal func getCell(key: Expr) -> Cell? {
    return self.getCell(key, self.hash(key))
  }
  
  internal func getCell(key: Expr, _ hashValue: Int) -> Cell? {
    return self.getCell(key, hashValue, self.eq)
  }
  
  // Support for managed objects
  
  /// Mark hash table content.
  public override func mark(tag: UInt8) {
    if self.tag != tag {
      self.tag = tag
      for bucket in self.buckets {
        var current = bucket
        while case .Pair(.Pair(let key, .Box(let cell)), let next) = current {
          key.mark(tag)
          cell.mark(tag)
          current = next
        }
      }
      if case .Custom(let procs) = self.equiv {
        procs.eql.mark(tag)
        procs.hsh.mark(tag)
        procs.has.mark(tag)
        procs.get.mark(tag)
        procs.set.mark(tag)
        procs.upd.mark(tag)
        procs.del.mark(tag)
      }
    }
  }
  
  /// Clear variable value
  public override func clean() {
    self.buckets = [Expr](count: 1, repeatedValue: .Null)
    self.count = 0
    self.equiv = .Eq
  }
}

