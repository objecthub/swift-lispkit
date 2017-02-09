//
//  Heap.swift
//  LispKit
//
//  Created by Matthias Zenger on 29/12/2016.
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
/// Class encapsulating all objects referenced from global environments.
///
public final class Heap: TrackedObject {
  
  /// Global locations.
  public var locations: Exprs
  
  /// Capacity of the heap.
  public var capacity: Int {
    return self.locations.capacity
  }
  
  /// Number of globally allocated locations.
  public var count: Int {
    return self.locations.count
  }
  
  /// Initializer
  public init(capacity: Int = 1000) {
    self.locations = Exprs()
    self.locations.reserveCapacity(capacity)
  }
  
  /// Allocates a new global location and initializes it with `expr`.
  public func allocateLocation(for expr: Expr = .undef) -> Int {
    self.locations.append(expr)
    return self.locations.count - 1
  }
  
  /// Reserve at least the given capacity to avoid continuous reallocations of the underlying
  /// locations array.
  public func reserveCapacity(_ capacity: Int) {
    if self.locations.capacity < capacity {
      self.locations.reserveCapacity(capacity)
    }
  }
  
  /// Mark all elements on the heap
  public override func mark(_ tag: UInt8) {
    for i in self.locations.indices {
      self.locations[i].mark(tag)
    }
  }
}
