//
//  Collection.swift
//  LispKit
//
//  Created by Matthias Zenger on 23/01/2016.
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
/// Class `Collection` is an indexed sequence of expressions which is used to represent
/// vectors, records, and record types in LispKit. A `Collection` object is a boxed
/// expression array of one of the following kinds:
///    - Vector
///    - Immutable vector
///    - Record type
///    - Record instance
///
public final class Collection: ManagedObject, CustomStringConvertible {
  
  public enum Kind: CustomStringConvertible {
    case vector
    case immutableVector
    case recordType
    case record(Collection)
    
    public var description: String {
      switch self {
        case .vector:
          return "vector"
        case .immutableVector:
          return "immutable vector"
        case .recordType:
          return "record type"
        case .record(let type):
          return "record of type \(type)"
      }
    }
  }
  
  /// The kind of this collection
  public private(set) var kind: Kind
  
  /// The collection values
  public var exprs: Exprs
  
  /// Maintain object statistics.
  internal static var stats = Stats("Collection")
  
  /// Update object statistics.
  deinit {
    Collection.stats.dealloc()
  }
  
  /// Creates an immutable Collection from the given array
  public init(kind: Kind, exprs: Exprs = []) {
    self.kind = kind
    self.exprs = exprs
    super.init(Collection.stats)
  }
  
  /// Creates a mutable Collection of the given length and prefilled with the given value
  public convenience init(kind: Kind, count n: Int, repeatedValue value: Expr = .null) {
    self.init(kind: kind, exprs: Exprs(repeating: value, count: n))
  }
  
  public func sameKindAs(_ other: Collection) -> Bool {
    switch (self.kind, other.kind) {
      case (.vector, .vector), (.immutableVector, .immutableVector), (.recordType, .recordType):
        return true
      case (.record(let type1), .record(let type2)):
        return type1 == type2
      default:
        return true
    }
  }
  
  public override func mark(_ tag: UInt8) {
    if self.tag != tag {
      self.tag = tag
      if case .record(let type) = self.kind {
        type.mark(tag)
      }
      for expr in self.exprs {
        expr.mark(tag)
      }
    }
  }
  
  public override func clean() {
    self.kind = .vector
    self.exprs.removeAll()
  }
  
  public var description: String {
    switch self.kind {
      case .vector:
        return "#<vector \(self.identityString)>"
      case .immutableVector:
        return "#<immutable-vector \(self.identityString)>"
      case .recordType:
        return "#<record-type:\(self.exprs[0]) \(self.identityString)>"
      case .record(let type):
        return "#<record-type:\(type.exprs[0]) \(self.identityString)>"
    }
  }
}
