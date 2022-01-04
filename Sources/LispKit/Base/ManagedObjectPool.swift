//
//  ManagedObjectPool.swift
//  LispKit
//
//  Created by Matthias Zenger on 23/01/2016.
//  Copyright © 2016-2022 ObjectHub. All rights reserved.
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
/// A managed object pool allows clients to register two kinds of objects:
///    - Managed objects: A managed object pool figures out if there are no references
///      left to them. Since those managed objects are referenced weakly only by the object pool,
///      strong dependency cycles prevent these objects from being garbage collected. The
///      object pool breaks such dependencies and allows such objects to be freed by ARC.
///    - Tracked objects: Are maintained for figuring out which managed objects are still
///      referenced and thus can't be garbage collected. A managed object pool keeps only week
///      references to such tracked objects.
/// 
public final class ManagedObjectPool: CustomStringConvertible {

  /// Mutex to synchronize access to the managed object pool
  private let lock = EmbeddedUnfairLock()
  
  /// Object marker
  private let marker: ObjectMarker

  /// Root set of tracked objects.
  private var rootSet: ObjectPool<TrackedObject>
  
  /// Pool of managed objects.
  private var objectPool: ObjectPool<ManagedObject>
  
  /// Does this managed object pool own the managed objects? If yes, the objects will be
  /// cleaned as soon as this managed object pool is being de-initialized.
  private let ownsManagedObjects: Bool

  /// Set to true if there have been new managed objects added since the last GC cycle.
  private var newManagedObjects: Bool

  /// Maximum delay of garbage collection in seconds.
  private let gcDelay: Double

  /// Time of last garbage collection.
  private var lastGcTime: Double

  /// Callback invoked whenever garbage collection was performed.
  private let gcCallback: ((ManagedObjectPool, Double, Int) -> Void)?

  /// Initializes an empty managed object pool.
  public init(ownsManagedObjects: Bool = true,
              marker: ObjectMarker,
              gcDelay: Double = 0.0,
              gcCallback: ((ManagedObjectPool, Double, Int) -> Void)? = nil) {
    self.marker = marker
    self.rootSet = ObjectPool<TrackedObject>()
    self.objectPool = ObjectPool<ManagedObject>()
    self.ownsManagedObjects = ownsManagedObjects
    self.newManagedObjects = false
    self.gcDelay = gcDelay
    self.lastGcTime = Timer.absoluteTimeInSec
    self.gcCallback = gcCallback
  }
  
  /// Destory all potential cyclic dependencies if this managed object pool owns the managed
  /// objects.
  deinit {
    if self.ownsManagedObjects {
      for obj in self.objectPool {
        obj.clean()
      }
    }
    self.lock.release()
  }
  
  /// Next tag for garbage collection run.
  public var tag: UInt8 {
    return self.marker.tag
  }

  /// Number of garbage collection cycles.
  public var cycles: UInt64 {
    return self.marker.cycles
  }

  /// Capacity of object marker backlog
  public var backlogCapacity: Int {
    return self.marker.backlogCapacity
  }
  
  /// Returns number of tracked objects.
  public var numTrackedObjects: Int {
    return self.rootSet.count
  }
  
  /// Returns number of managed objects.
  public var numManagedObjects: Int {
    return self.objectPool.count
  }
  
  /// Returns capacity of tracked objects; i.e. the number of tracked objects that can be
  /// registered without reallocating memory for the root set.
  public var trackedObjectCapacity: Int {
    return self.rootSet.capacity
  }
  
  /// Returns capacity of managed objects; i.e. the number of managed objects that can be
  /// registered without reallocating memory for the object pool.
  public var managedObjectCapacity: Int {
    return self.objectPool.capacity
  }
  
  /// Track the given tracked object.
  public func track(_ obj: TrackedObject) {
    self.lock.lock()
    self.rootSet.add(obj)
    self.lock.unlock()
  }

  /// Manage the given managed object.
  @discardableResult public func manage<T: ManagedObject>(_ obj: T) -> T {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    if !obj.managed {
      self.newManagedObjects = true
      obj.managed = true
      self.objectPool.add(obj)
    }
    return obj
  }
  
  /// Manage the given managed object.
  @discardableResult public func reference(to obj: ManagedObject) -> Int? {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    if !obj.managed {
      self.newManagedObjects = true
      obj.managed = true
      return self.objectPool.add(obj)
    }
    return nil
  }

  /// Unmanage the object at which the given reference points.
  public func unmanage(_ ref: Int) {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    self.objectPool.remove(ref)
  }
  
  /// Perform garbage collection.
  public func collectGarbage() -> Int {
    if self.lock.tryLock() {
      defer {
        self.lock.unlock()
      }
      // Start time
      let startTime = Timer.absoluteTimeInSec
      // Early exit
      guard self.newManagedObjects || (startTime - self.lastGcTime) >= self.gcDelay else {
        return 0
      }
      // Proceed with garbage collection
      self.newManagedObjects = false
      self.lastGcTime = startTime
      // Mark
      self.marker.mark(self.rootSet)
      // Sweep
      let oldManagedObjectCount = self.numManagedObjects
      for obj in self.objectPool {
        // Does this object still have a tag from a previous run?
        if obj.tag != self.marker.tag {
          obj.clean()
        }
      }
      // Invoke callback
      if let callback = self.gcCallback {
        callback(self, Timer.absoluteTimeInSec - startTime, oldManagedObjectCount)
      }
      // Return number of freed up objects
      return oldManagedObjectCount - self.numManagedObjects
    } else {
      return 0
    }
  }
  
  /// Returns a short description of the current managed object pool state.
  public var shortDescription: String {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    return "tracked \(self.numTrackedObjects) of " +
           "\(self.trackedObjectCapacity), managed \(self.numManagedObjects) of " +
           "\(self.managedObjectCapacity), gc cycles = \(self.cycles), " +
           "last tag = \(self.marker.tag)"
  }
  
  /// Returns a description of the current managed object pool state.
  public var description: String {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    return "ManagedObjectPool{ tracked \(self.numTrackedObjects) of " +
           "\(self.trackedObjectCapacity), managed \(self.numManagedObjects) of " +
           "\(self.managedObjectCapacity), gc cycles = \(self.cycles), " +
           "last tag = \(self.marker.tag) }"
  }
  
  /// Returns a distribution of type names of managed objects
  public var managedObjectDistribution: [String : Int] {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    var distrib: [String : Int] = [:]
    for object in self.objectPool {
      let typeName = String(describing: type(of: object))
      if let count = distrib[typeName] {
        distrib[typeName] = count + 1
      } else {
        distrib[typeName] = 1
      }
    }
    return distrib
  }
}

public protocol ObjectMarker {
  var tag: UInt8 { get }
  var cycles: UInt64 { get }
  var backlogCapacity: Int { get }
  func mark(_ rootSet: ObjectPool<TrackedObject>)
}
