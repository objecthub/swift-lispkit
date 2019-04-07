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
///    - Records
///    - Hashtables
///    - Futures
///
open class ManagedObject: Reference {
  
  /// Used internally to declare that a managed object is registered in a managed object pool.
  internal final var managed: Bool = false
  
  /// A tag that defines the last GC cyle in which this object was marked (by following the
  /// root set references).
  internal final var tag: UInt8 = 0

  /// Mark the managed object with the given tag.
  open func mark(_ tag: UInt8) {
    self.tag = tag
  }
  
  /// Clean up the object; i.e. remove possible cycles to free up the object for
  /// garbage collection.
  open func clean() {}
}
