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
  
  public enum Equivalence {
    case Eq
    case Eqv
    case Equal
    case Custom(has: Procedure, get: Procedure, set: Procedure?, del: Procedure?)
  }
  
  private enum Bucket {
    case Empty
    indirect case Mapping(Expr, Cell, Bucket)
    
    init() {
      self = Empty
    }
    
    init(key: Expr, value: Expr, next: Bucket? = nil) {
      self = Mapping(key, Cell(value), next ?? Empty)
    }
  }
  
  /// Maintain object statistics.
  internal static let stats = Stats("HashTable")
  
  /// The hash buckets
  private var buckets: [Bucket]
  
  /// Is this `HashTable` object mutable?
  public let mutable: Bool
  
  /// What equivalence relation is used?
  public private(set) var equiv: Equivalence
  
  /// Update object statistics.
  deinit {
    HashTable.stats.dealloc()
  }
  
  /// Create a new empty hash table with the given size
  public init(capacity: Int = 499,
              mutable: Bool = true,
              equiv: Equivalence) {
    self.buckets = [Bucket](count: capacity, repeatedValue: .Empty)
    self.mutable = mutable
    self.equiv = equiv
    super.init(HashTable.stats)
  }
  
  /// Returns the number of hash buckets in the hash table
  public var count: Int {
    return self.buckets.count
  }
  
  /// Returns a list of all keys in the hash table
  public func keys() -> Expr {
    var res: Expr = .Null
    for bucket in self.buckets {
      var current = bucket
      while case .Mapping(let key, _, let next) = current {
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
      while case .Mapping(_, let cell, let next) = current {
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
      while case .Mapping(let key, let cell, let next) = current {
        res = .Pair(.Pair(key, .Box(cell)), res)
        current = next
      }
    }
    return res
  }
  
  /// Returns the mappings in the hash table as an association list
  public func alist(bid: Int) -> Expr {
    var res: Expr = .Null
    var current = self.buckets[bid]
    while case .Mapping(let key, let cell, let next) = current {
      res = .Pair(.Pair(key, .Box(cell)), res)
      current = next
    }
    return res
  }
  
  public func add(bid: Int, _ key: Expr, _ value: Expr) {
    self.buckets[bid] = .Mapping(key, Cell(value), self.buckets[bid])
  }
  
  public func remove(bid: Int, _ delete: Cell) {
    var stack = [(Expr, Cell)]()
    var current = self.buckets[bid]
    while case .Mapping(let key, let cell, let next) = current {
      guard cell !== delete else {
        var res = next
        for i in 0..<stack.count {
          let pair = stack[stack.count - i - 1]
          res = .Mapping(pair.0, pair.1, res)
        }
        self.buckets[bid] = res
        return
      }
      stack.append((key, cell))
      current = next
    }
  }
  
  /// Array of mappings
  public var mappings: [(Expr, Expr)] {
    var res = [(Expr, Expr)]()
    for bucket in self.buckets {
      var current: Bucket? = bucket
      while case .Some(.Mapping(let key, let cell, let next)) = current {
        res.append((key, cell.value))
        current = next
      }
    }
    return res
  }
  
  private func eq(left: Expr, _ right: Expr) -> Bool {
    switch self.equiv {
      case .Eq:
        return eqExpr(left, right)
      case .Eqv:
        return eqvExpr(left, right)
      case .Equal:
        return equalExpr(left, right)
      case .Custom(_, _, _, _):
        preconditionFailure("cannot access custom hashtable internally")
    }
  }
  
  private func hash(expr: Expr) -> Int {
    switch self.equiv {
      case .Eq:
        return eqHash(expr)
      case .Eqv:
        return eqvHash(expr)
      case .Equal:
        return equalHash(expr)
      case .Custom(_, _, _, _):
        preconditionFailure("cannot access custom hashtable internally")
    }
  }
  
  internal func getCell(key: Expr) -> Cell? {
    return self.getCell(key, self.hash(key))
  }
  
  internal func getCell(key: Expr, _ hashValue: Int) -> Cell? {
    return self.getCell(key, self.hash(key), self.eq)
  }
  
  internal func getCell(key: Expr, _ hashValue: Int, _ eql: (Expr, Expr) -> Bool) -> Cell? {
    var current: Bucket? = self.buckets[hashValue % self.buckets.count]
    while case .Some(.Mapping(let k, let cell, let next)) = current {
      if eql(key, k) {
        return cell
      }
      current = next
    }
    return nil
  }
  
  public func get(key: Expr) -> Expr? {
    return self.getCell(key, self.hash(key))?.value
  }
  
  public func set(key: Expr, _ value: Expr) {
    let hashValue = self.hash(key)
    if let cell = self.getCell(key, hashValue) {
      cell.value = value
    } else {
      let bid = hashValue % self.buckets.count
      self.buckets[bid] = Bucket(key: key, value: value, next: self.buckets[bid])
    }
  }
  
  public func remove(key: Expr) {
    let hashValue = self.hash(key)
    if let cell = self.getCell(key, hashValue) {
      let bid = hashValue % self.buckets.count
      self.remove(bid, cell)
    }
  }
  
  /// Mark hash table content.
  public override func mark(tag: UInt8) {
    if self.tag != tag {
      self.tag = tag
      for bucket in self.buckets {
        var current: Bucket? = bucket
        while case .Some(.Mapping(let key, let cell, let next)) = current {
          key.mark(tag)
          cell.mark(tag)
          current = next
        }
      }
      if case .Custom(let has, let get, let set, let del) = self.equiv {
        has.mark(tag)
        get.mark(tag)
        set?.mark(tag)
        del?.mark(tag)
      }
    }
  }
  
  /// Clear variable value
  public override func clean() {
    self.buckets = [Bucket](count: 1, repeatedValue: .Empty)
    self.equiv = .Eq
  }
  
  /// A string representation of this variable.
  public var description: String {
    return "«\(self.buckets)»"
  }
}

