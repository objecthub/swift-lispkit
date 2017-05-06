//
//  TrackedObject.swift
//  LispKit
//
//  Created by Matthias Zenger on 29/03/2016.
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
/// Objects inheriting from `TrackedObject` can be registered in `ManagedObjectPool`
/// instances for tracking managed objects.
///
open class TrackedObject {
  open func mark(_ tag: UInt8) {}
}

///
/// Objects which implement protocol `Trackable` can be wrapped in `Tracked` proxies
/// and registered in `ManagedObjectPool` instances for tracking managed objects.
///
public protocol Trackable {
  func mark(_ tag: UInt8)
}

///
/// Class `Tracked` implements a generic wrapper for objects implementing the `Trackable`
/// protocol.
///
open class Tracked<T: Trackable>: TrackedObject {
  open var value: T
  
  internal init(_ value: T) {
    self.value = value
  }
  
  open override func mark(_ tag: UInt8) {
    self.value.mark(tag)
  }
}
