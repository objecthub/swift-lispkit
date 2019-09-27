//
//  Collection.swift
//  LispKit
//
//  Created by Matthias Zenger on 23/01/2016.
//  Copyright © 2016-2019 ObjectHub. All rights reserved.
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
/// arrays, vectors, records, and record types in LispKit. A `Collection` object is a boxed
/// expression array of one of the following kinds:
///    - Array
///    - Vector
///    - Immutable vector
///    - Growable vector
///    - Record type
///    - Record instance
///
public final class Collection: ManagedObject, CustomStringConvertible {
  
  public enum Kind: CustomStringConvertible {
    case array
    case vector
    case immutableVector
    case growableVector
    case recordType
    case record(Collection)
    
    public var description: String {
      switch self {
        case .array:
          return "array"
        case .vector:
          return "vector"
        case .immutableVector:
          return "immutable vector"
        case .growableVector:
          return "growable vector"
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
  public static var allocated: UInt64 = 0
  
  /// Update object statistics.
  deinit {
    Collection.allocated -= 1
  }
  
  /// Creates an immutable Collection from the given array
  public init(kind: Kind, exprs: Exprs = []) {
    self.kind = kind
    self.exprs = exprs
    Collection.allocated += 1
  }
  
  /// Creates a mutable Collection of the given length and prefilled with the given value
  public convenience init(kind: Kind, count n: Int, repeatedValue value: Expr = .null) {
    self.init(kind: kind, exprs: Exprs(repeating: value, count: n))
  }
  
  public func sameKindAs(_ other: Collection) -> Bool {
    switch (self.kind, other.kind) {
      case (.array, .array),
           (.vector, .vector),
           (.immutableVector, .immutableVector),
           (.growableVector, .growableVector),
           (.recordType, .recordType):
        return true
      case (.record(let type1), .record(let type2)):
        return type1 == type2
      default:
        return false
    }
  }
  
  public var isVector: Bool {
    switch self.kind {
      case .vector, .immutableVector, .growableVector:
        return true
      default:
        return false
    }
  }
  
  public var isMutableVector: Bool {
    switch self.kind {
      case .vector, .growableVector:
        return true
      default:
        return false
    }
  }
  
  public var isGrowableVector: Bool {
    switch self.kind {
      case .growableVector:
        return true
      default:
        return false
    }
  }
  
  public override func clean() {
    self.kind = .vector
    self.exprs.removeAll()
  }
  
  public var description: String {
    switch self.kind {
      case .array:
        return "#<array \(self.identityString)>"
      case .vector:
        return "#<vector \(self.identityString)>"
      case .immutableVector:
        return "#<immutable-vector \(self.identityString)>"
      case .growableVector:
        return "#<gvector \(self.identityString)>"
      case .recordType:
        return "#<record-type:\((try? self.exprs[0].asString()) ?? self.exprs[0].description) " +
               "\(self.identityString)>"
      case .record(let type):
        return "#<record:\((try? type.exprs[0].asString()) ?? type.exprs[0].description) " +
               "\(self.identityString)>"
    }
  }
}
