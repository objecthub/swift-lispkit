//
//  Locks.swift
//  LispKit
//
//  Created by Matthias Zenger on 20/12/2021.
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

/// Implements an embeddable mutex. Such a struct is typically embedded in a class.
/// It is important to invoke `release` when an object using an embedded mutex is being
/// de-initialized.
public struct EmbeddedMutex {

  /// The underlying system mutex
  fileprivate var systemMutex = pthread_mutex_t()
  
  public init(recursive: Bool = false) {
    var attr = pthread_mutexattr_t()
    guard pthread_mutexattr_init(&attr) == 0 else {
      preconditionFailure()
    }
    if recursive {
      pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE)
    } else {
      pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_NORMAL)
    }
    guard pthread_mutex_init(&self.systemMutex, &attr) == 0 else {
      preconditionFailure()
    }
    pthread_mutexattr_destroy(&attr)
  }
  
  public mutating func release() {
    pthread_mutex_destroy(&self.systemMutex)
  }
  
  public mutating func lock() {
    pthread_mutex_lock(&self.systemMutex)
  }
  
  public mutating func tryLock() -> Bool {
    return pthread_mutex_trylock(&self.systemMutex) == 0
  }
  
  public mutating func unlock() {
    pthread_mutex_unlock(&self.systemMutex)
  }
  
  public mutating func sync<T>(execute block: () throws -> T) rethrows -> T {
    self.lock()
    defer { self.unlock() }
    return try block()
  }
  
  public mutating func trySync<T>(execute block: () throws -> T) rethrows -> T? {
    guard self.tryLock() else {
      return nil
    }
    defer { self.unlock() }
    return try block()
  }
}

/// Provides standalone mutex objects.
public final class Mutex {
  fileprivate var mutex: EmbeddedMutex
  
  public init(recursive: Bool = false) {
    self.mutex = EmbeddedMutex(recursive: recursive)
  }
  
  deinit {
    self.mutex.release()
  }
  
  public func lock() {
    self.mutex.lock()
  }
  
  public func tryLock() -> Bool {
    return self.mutex.tryLock()
  }
  
  public func unlock() {
    self.mutex.unlock()
  }
  
  public func sync<R>(execute block: () throws -> R) rethrows -> R {
    return try self.mutex.sync(execute: block)
  }
  
  public func trySync<R>(execute block: () throws -> R) rethrows -> R? {
    return try self.mutex.trySync(execute: block)
  }
}

/// Implements an embeddable read/write lock (multiple readers can access critical sections
/// but at most one writer). Such a struct is typically embedded in a class. It is important
/// to invoke `release` when an object using an embedded read/write lock is being
/// de-initialized.
public struct EmbeddedReadWriteLock {

  /// The underlying system read/write lock
  private var systemLock = pthread_rwlock_t()
  
  public init() {
    pthread_rwlock_init(&self.systemLock, nil)
  }
  
  public mutating func release() {
    pthread_rwlock_destroy(&self.systemLock)
  }
  
  public mutating func readLock() {
    pthread_rwlock_rdlock(&self.systemLock)
  }
  
  public mutating func tryReadLock() -> Bool {
    return pthread_rwlock_tryrdlock(&self.systemLock) == 0
  }
  
  public mutating func writeLock() {
    pthread_rwlock_wrlock(&self.systemLock)
  }
  
  public mutating func tryWriteLock() -> Bool {
    return pthread_rwlock_trywrlock(&self.systemLock) == 0
  }
  
  public mutating func unlock() {
    pthread_rwlock_unlock(&self.systemLock)
  }
  
  public mutating func syncRead<T>(execute block: () throws -> T) rethrows -> T {
    self.readLock()
    defer { self.unlock() }
    return try block()
  }
  
  public mutating func trySyncRead<T>(execute block: () throws -> T) rethrows -> T? {
    guard self.tryReadLock() else {
      return nil
    }
    defer { self.unlock() }
    return try block()
  }
  
  public mutating func syncWrite<T>(execute block: () throws -> T) rethrows -> T {
    self.readLock()
    defer { self.unlock() }
    return try block()
  }
  
  public mutating func trySyncWrite<T>(execute block: () throws -> T) rethrows -> T? {
    guard self.tryReadLock() else {
      return nil
    }
    defer { self.unlock() }
    return try block()
  }
}

/// Provides standalone read/write locks.
public final class ReadWriteLock {
  private var lock = EmbeddedReadWriteLock()
  
  deinit {
    self.lock.release()
  }
  
  public func readLock() {
    self.lock.readLock()
  }
  
  public func tryReadLock() -> Bool {
    return self.lock.tryReadLock()
  }
  
  public func writeLock() {
    self.lock.writeLock()
  }
  
  public func tryWriteLock() -> Bool {
    return self.lock.tryWriteLock()
  }
  
  public func unlock() {
    return self.lock.unlock()
  }
  
  public func syncRead<T>(execute block: () throws -> T) rethrows -> T {
    return try self.lock.syncRead(execute: block)
  }
  
  public func trySyncRead<T>(execute block: () throws -> T) rethrows -> T? {
    return try self.lock.trySyncRead(execute: block)
  }
  
  public func syncWrite<T>(execute block: () throws -> T) rethrows -> T {
    return try self.lock.syncWrite(execute: block)
  }
  
  public func trySyncWrite<T>(execute block: () throws -> T) rethrows -> T? {
    return try self.lock.trySyncWrite(execute: block)
  }
}

/// Implements an embeddable condition variable. This is used in combination with an
/// embeddable mutex. Both are typically embedded in a class. It is important to invoke
/// `release` when an object using an embedded mutex is being de-initialized.
public struct EmbeddedCondition {
  
  /// The underlying system condition variable
  private var systemCond = pthread_cond_t()
  
  public init() {
    pthread_cond_init(&self.systemCond, nil)
  }
  
  public mutating func release() {
    pthread_cond_destroy(&self.systemCond)
  }
  
  /// Wakes all threads waiting on this condition.
  public mutating func broadcast() {
    pthread_cond_broadcast(&self.systemCond)
  }
  
  /// Wakes one thread waiting on this condition.
  public mutating func signal() {
    pthread_cond_signal(&self.systemCond)
  }
  
  /// Atomically releases mutex and causes the calling thread to block on the condition
  /// variable until either `signal` or `broadcast` is invoked on the condition variable,
  /// in which case the thread will re-acquire the lock.
  public mutating func wait(unlocking mutex: inout EmbeddedMutex) {
    pthread_cond_wait(&self.systemCond, &mutex.systemMutex)
  }
  
  /// Atomically releases mutex and causes the calling thread to block on the condition
  /// variable until either `signal` or `broadcast` is invoked on the condition variable,
  /// in which case the thread will re-acquire the lock. This method returns false if
  /// the waiting times out.
  @discardableResult public mutating func wait(_ timeout : TimeInterval,
                                               unlocking mutex: inout EmbeddedMutex) -> Bool {
    if timeout < 0 {
      pthread_cond_wait(&self.systemCond, &mutex.systemMutex)
      return true
    }
    let timeInMs = Int(timeout * 1000)
    var tv = timeval()
    var ts = timespec()
    gettimeofday(&tv, nil)
    ts.tv_sec = time(nil) + timeInMs / 1000
    ts.tv_nsec = Int(Int(tv.tv_usec * 1000) + 1000000 * (timeInMs % 1000))
    ts.tv_sec += ts.tv_nsec / 1000000000
    ts.tv_nsec %= 1000000000
    return pthread_cond_timedwait(&self.systemCond, &mutex.systemMutex, &ts) == 0
  }
}

public final class Condition {
  private var condition = EmbeddedCondition()
  
  deinit {
    self.condition.release()
  }
  
  public func broadcast() {
    self.condition.broadcast()
  }
  
  public func signal() {
    self.condition.signal()
  }
  
  public func wait(unlocking mutex: Mutex) {
    self.condition.wait(unlocking: &mutex.mutex)
  }
  
  @discardableResult public func wait(_ timeout : TimeInterval, unlocking mutex: Mutex) -> Bool {
    return self.condition.wait(timeout, unlocking: &mutex.mutex)    
  }
}

/// Implements an embeddable condition lock, combining a condition variable with a mutex.
/// Such a struct is typically embedded in a class. It is important to invoke `release`
/// when an object using an embedded read/write lock is being de-initialized.
public struct EmbeddedConditionLock {
  private var condition: EmbeddedCondition
  private var mutex: EmbeddedMutex
  
  public init(recursive: Bool = false) {
    self.condition = EmbeddedCondition()
    self.mutex = EmbeddedMutex(recursive: recursive)
  }
  
  public mutating func release() {
    self.condition.release()
    self.mutex.release()
  }
  
  public mutating func lock() {
    self.mutex.lock()
  }
  
  public mutating func unlock() {
    self.mutex.unlock()
  }
  
  public mutating func broadcast() {
    self.condition.broadcast()
  }
  
  public mutating func signal() {
    self.condition.signal()
  }
  
  public mutating func wait() {
    self.condition.wait(unlocking: &self.mutex)
  }
  
  @discardableResult public mutating func wait(_ timeout : TimeInterval) -> Bool {
    return self.condition.wait(timeout, unlocking: &self.mutex)
  }
}

public final class ConditionLock {
  private var conditionLock: EmbeddedConditionLock
  
  public init(recursive: Bool = false) {
    self.conditionLock = EmbeddedConditionLock(recursive: recursive)
  }
  
  deinit {
    self.conditionLock.release()
  }
  
  public func lock() {
    self.conditionLock.lock()
  }
  
  public func unlock() {
    self.conditionLock.unlock()
  }
  
  public func broadcast() {
    self.conditionLock.broadcast()
  }
  
  public func signal() {
    self.conditionLock.signal()
  }
  
  public func wait() {
    self.conditionLock.wait()
  }
  
  @discardableResult public func wait(_ timeout : TimeInterval) -> Bool {
    return self.conditionLock.wait(timeout)
  }
}
