//
//  ManagedObject.swift
//  LispKit
//
//  Created by Matthias Zenger on 20/03/2016.
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
/// A managed object can be registered with a managed object pool. Once registered, the
/// `managed` property will be set to true. As soon as there is no reference pointing at this
/// object anymore, `clean` will be called to reset the object and break any strong cyclic
/// references.
///
/// Managed objects are currently:
///    - Cells
///    - Tuples
///    - Vectors
///    - Hashtables
///    - Futures
///
public class ManagedObject: Reference {
  
  /// Used internally to declare that a managed object is registered in a managed object pool.
  internal var managed: Bool = false
  
  /// A tag that defines the last GC cyle in which this object was marked (by following the
  /// root set references).
  internal var tag: UInt8 = 0
  
  /// Each class inheriting from `ManagedObject` creates a static `Stats` instance and
  /// passes it on to the constructor of `ManagedObject`. The `deinit` method of each
  /// `ManagedObject` subclass needs to invoke `dealloc` on this stats object to update
  /// the count for the number of allocated objects.
  public class Stats {
    let entityName: String
    var created: UInt64 = 0
    var allocated: UInt64 = 0
    
    public init(_ entityName: String) {
      self.entityName = entityName
    }
    
    public func dealloc() {
      self.allocated -= 1
      log("[releasing \(self.entityName)]")
    }
  }
  
  /// Initializes stats for this managed object type.
  public init(_ stats: Stats) {
    stats.created += 1
    stats.allocated += 1
    log("[allocating \(stats.entityName), alive = \(stats.allocated), total = \(stats.created)]")
  }
  
  /// Mark the managed object with the given tag.
  public func mark(tag: UInt8) {
    self.tag = tag
  }
  
  /// Clean up the object; i.e. remove possible cycles to free up the object for
  /// garbage collection.
  public func clean() {}
}
