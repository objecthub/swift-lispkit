//
//  ThreadManager.swift
//  LispKit
//
//  Created by Matthias Zenger on 23/12/2021.
//  Copyright © 2021 ObjectHub. All rights reserved.
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

import Foundation

/// 
/// The `ThreadManager` manages all the threads executing LispKit code.
/// 
public final class ThreadManager {
  private let mutex = EmbeddedConditionLock(recursive: true)
  private var threads: [Thread : NativeThread] = [:]
  
  deinit {
    self.mutex.release()
  }
  
  public var current: NativeThread? {
    return self.nativeThread(for: .current)
  }
  
  public var count: Int {
    return self.threads.count
  }
  
  public func nativeThread(for thread: Thread) -> NativeThread? {
    self.mutex.lock()
    defer {
      self.mutex.unlock()
    }
    return self.threads[thread]
  }
  
  public func register(thread: Thread, as nativeThread: NativeThread) {
    self.mutex.lock()
    defer {
      self.mutex.unlock()
    }
    self.threads[thread] = nativeThread
    self.mutex.broadcast()
  }
  
  public func remove(thread: Thread) {
    self.mutex.lock()
    defer {
      self.mutex.unlock()
    }
    self.threads.removeValue(forKey: thread)
    self.mutex.broadcast()
  }
  
  public func removeAll() {
    self.mutex.lock()
    defer {
      self.mutex.unlock()
    }
    self.threads.removeAll()
    self.mutex.broadcast()
  }
  
  @discardableResult func waitForTermination(of thread: Thread, timeout: Double? = nil) -> Bool {
    let deadline: Double? = timeout == nil ? nil : Timer.currentTimeInSec + timeout!
    self.mutex.lock()
    defer {
      self.mutex.unlock()
    }
    while self.threads[thread] != nil {
      if let deadline = deadline {
        if !self.mutex.wait(until: deadline) {
          return false
        }
      } else {
        self.mutex.wait()
      }
    }
    return true
  }
  
  @discardableResult func waitForTerminationOfAll(timeout: Double? = nil) -> Bool {
    let deadline: Double? = timeout == nil ? nil : Timer.currentTimeInSec + timeout!
    self.mutex.lock()
    defer {
      self.mutex.unlock()
    }
    while self.threads.count > 1 {
      if let deadline = deadline {
        if !self.mutex.wait(until: deadline) {
          return false
        }
      } else {
        self.mutex.wait()
      }
    }
    return true
  }
  
  public func abortAll(except main: Thread? = nil) {
    self.mutex.lock()
    defer {
      self.mutex.unlock()
    }
    for (thread, nativeThread) in self.threads {
      if main == nil || thread !== main! {
        _ = nativeThread.value.abort()
      }
    }
  }
  
  public func cancelAll(except main: Thread? = nil) {
    self.mutex.lock()
    defer {
      self.mutex.unlock()
    }
    for (thread, _) in self.threads {
      if main == nil || thread !== main! {
        thread.cancel()
      }
    }
  }
  
  public func mark(in gc: GarbageCollector) {
    self.mutex.lock()
    defer {
      self.mutex.unlock()
    }
    for thread in self.threads.values {
      thread.value.mark(in: gc)
    }
  }
  
  public func releaseAll() {
    self.mutex.lock()
    defer {
      self.mutex.unlock()
    }
    for thread in self.threads.values {
      thread.value.clean()
    }
    self.threads.removeAll()
  }
}
