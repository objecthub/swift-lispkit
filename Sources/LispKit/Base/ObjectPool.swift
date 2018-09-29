//
//  ObjectPool.swift
//  LispKit
//
//  Created by Matthias Zenger on 24/01/2016.
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
/// An object pool refers to a set of objects via weak references. Objects are added
/// to the pool via method `add`. They are automatically removed as soon as there is no
/// strong reference to this object anymore.
///
public final class ObjectPool<T: AnyObject>: Sequence, CustomStringConvertible {
  
  /// Internal representation of a weak variable that can be recycled.
  private struct WeakVariable {
    var recycled: Bool
    weak var obj: T?
  }
  
  /// Weak references to objects in the pool
  private var references: ContiguousArray<WeakVariable>
  
  /// A set of indices of free weak references
  private var free: ContiguousArray<Int>
  
  /// How many references have been added after the last collection of free references
  private var added: Int
  
  /// Initializes an empty object pool
  public init() {
    self.references = []
    self.free = []
    self.added = 0
  }
  
  /// Returns the capacity of the object pool. This is the number of allocated
  /// weak references.
  public var capacity: Int {
    return references.count
  }
  
  /// Returns the number of weakly referenced objects in the pool.
  public var count: Int {
    collectFreeReferences()
    return references.count - free.count
  }
  
  /// Returns true if the object pool is empty.
  public var isEmpty: Bool {
    return self.count == 0
  }
  
  /// Adds the given object to the object pool.
  public func add(_ obj: T) {
    // Collect free references on a regular basis; frequency is based on the capacity
    if let i = free.first {
      self.added += 1
      free.removeFirst()
      self.references[i].recycled = false
      self.references[i].obj = obj
    } else if self.added > (100 + self.capacity / 10) {
      self.added = 1
      collectFreeReferences()
      if let i = free.first {
        free.removeFirst()
        self.references[i].recycled = false
        self.references[i].obj = obj
      } else {
        references.append(WeakVariable(recycled: false, obj: obj))
      }
    } else {
      self.added += 1
      references.append(WeakVariable(recycled: false, obj: obj))
    }
  }
  
  /// Removes all objects from the object pool.
  public func clear() {
    self.references.removeAll()
    self.free.removeAll()
    self.added = 0
  }
  
  /// Finds all free references in the object pool.
  private func collectFreeReferences() {
    for i in self.references.indices {
      if !self.references[i].recycled && self.references[i].obj == nil {
        self.references[i].recycled = true
        self.free.append(i)
      }
    }
  }
  
  /// Returns a generator for iterating over all objects in the object pool.
  public func makeIterator() -> AnyIterator<T> {
    var i = 0
    return AnyIterator {
      while i < self.references.count {
        if let obj = self.references[i].obj {
          i += 1
          return obj
        }
        i += 1
      }
      return nil
    }
  }
  
  /// Returns a textual representation of this object pool.
  public var description: String {
    return "ObjectPool{refcount = \(self.references.count), freecount = \(self.free.count), " +
           "free = \(self.free)}"
  }
}

