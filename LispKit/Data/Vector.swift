//
//  Vector.swift
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
/// Class `Vector` is used to represent vectors in LispKit. A `Vector` object is a boxed
/// expression array.
///
public final class Vector: ManagedObject, CustomStringConvertible {
  public var exprs: Exprs
  public let mutable: Bool  // TODO: enforce mutability flag
  
  /// Maintain object statistics.
  internal static var stats = Stats("Vector")
  
  /// Update object statistics.
  deinit {
    Vector.stats.dealloc()
  }
  
  /// Creates an immutable vector from the given array
  public init(_ exprs: Exprs, mutable: Bool = false) {
    self.exprs = exprs
    self.mutable = mutable
    super.init(Vector.stats)
  }
  
  /// Creates a mutable empty vector
  public convenience init() {
    self.init(Exprs(), mutable: true)
  }
  
  /// Creates a mutable vector of the given length and prefilled with the given value
  public convenience init(count n: Int, repeatedValue value: Expr = .Null) {
    self.init(Exprs(count: n, repeatedValue: value), mutable: true)
  }
  
  public override func mark(tag: UInt8) {
    if self.tag != tag {
      self.tag = tag
      for expr in self.exprs {
        expr.mark(tag)
      }
    }
  }
  
  public override func clean() {
    self.exprs.removeAll()
  }
  
  public var description: String {
    return self.exprs.descriptionWithPrefix("#(", andSeparator: " ") + ")"
  }
}
