//
//  Locks.swift
//  LispKit
//
//  Created by Matthias Zenger on 20/12/2021.
//  Copyright © 2022 ObjectHub. All rights reserved.
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
  fileprivate let systemMutex: UnsafeMutablePointer<pthread_mutex_t>
  
  public init(recursive: Bool) {
    let pointer = UnsafeMutablePointer<pthread_mutex_t>.allocate(capacity: 1)
    pointer.initialize(to: pthread_mutex_t())
    var attr = pthread_mutexattr_t()
    guard pthread_mutexattr_init(&attr) == 0 else {
      preconditionFailure()
    }
    if recursive {
      pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE)
    } else {
      pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_NORMAL)
    }
    guard pthread_mutex_init(pointer, &attr) == 0 else {
      preconditionFailure()
    }
    pthread_mutexattr_destroy(&attr)
    self.systemMutex = pointer
  }
  
  public func release() {
    pthread_mutex_destroy(self.systemMutex)
    self.systemMutex.deinitialize(count: 1)
    self.systemMutex.deallocate()
  }
  
  public func lock() {
    pthread_mutex_lock(self.systemMutex)
  }
  
  public func tryLock() -> Bool {
    return pthread_mutex_trylock(self.systemMutex) == 0
  }
  
  public func unlock() {
    pthread_mutex_unlock(self.systemMutex)
  }
  
  public func sync<T>(execute block: () throws -> T) rethrows -> T {
    self.lock()
    defer { self.unlock() }
    return try block()
  }
  
  public func trySync<T>(execute block: () throws -> T) rethrows -> T? {
    guard self.tryLock() else {
      return nil
    }
    defer { self.unlock() }
    return try block()
  }
}

/// Provides standalone mutex objects.
public final class Mutex {
  fileprivate let mutex: EmbeddedMutex
  
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

/// Implements an embeddable unfair (but very efficient) mutex. Such a struct is typically
/// embedded in a class. It is important to invoke `release` when an object using an embedded
/// mutex is being de-initialized.
public struct EmbeddedUnfairLock {
  
  private let unfairLock: UnsafeMutablePointer<os_unfair_lock>
  
  public init() {
    let pointer = UnsafeMutablePointer<os_unfair_lock>.allocate(capacity: 1)
    pointer.initialize(to: os_unfair_lock())
    self.unfairLock = pointer
  }
  
  public func release() {
    self.unfairLock.deinitialize(count: 1)
    self.unfairLock.deallocate()
  }

  public func lock() {
    os_unfair_lock_lock(self.unfairLock)
  }

  public func tryLock() -> Bool {
    os_unfair_lock_trylock(self.unfairLock)
  }

  public func unlock() {
    os_unfair_lock_unlock(self.unfairLock)
  }
  
  public func sync<T>(execute block: () throws -> T) rethrows -> T {
    self.lock()
    defer { self.unlock() }
    return try block()
  }
  
  public func trySync<T>(execute block: () throws -> T) rethrows -> T? {
    guard self.tryLock() else {
      return nil
    }
    defer { self.unlock() }
    return try block()
  }
}

/// Provides standalone unfair locks.
public final class UnfairLock {
  private let unfairLock = EmbeddedUnfairLock()
  
  deinit {
    self.unfairLock.release()
  }

  public func lock() {
    self.unfairLock.lock()
  }

  public func tryLock() -> Bool {
    self.unfairLock.tryLock()
  }

  public func unlock() {
    self.unfairLock.unlock()
  }
  
  public func sync<R>(execute block: () throws -> R) rethrows -> R {
    return try self.unfairLock.sync(execute: block)
  }
  
  public func trySync<R>(execute block: () throws -> R) rethrows -> R? {
    return try self.unfairLock.trySync(execute: block)
  }
}

/// Implements an embeddable read/write lock (multiple readers can access critical sections
/// but at most one writer). Such a struct is typically embedded in a class. It is important
/// to invoke `release` when an object using an embedded read/write lock is being
/// de-initialized.
public struct EmbeddedReadWriteLock {
  
  /// The underlying system read/write lock
  private let systemLock: UnsafeMutablePointer<pthread_rwlock_t>
  
  public init() {
    let pointer = UnsafeMutablePointer<pthread_rwlock_t>.allocate(capacity: 1)
    pointer.initialize(to: pthread_rwlock_t())
    pthread_rwlock_init(pointer, nil)
    self.systemLock = pointer
  }
  
  public func release() {
    pthread_rwlock_destroy(self.systemLock)
    self.systemLock.deinitialize(count: 1)
    self.systemLock.deallocate()
  }
  
  public func readLock() {
    pthread_rwlock_rdlock(self.systemLock)
  }
  
  public func tryReadLock() -> Bool {
    return pthread_rwlock_tryrdlock(self.systemLock) == 0
  }
  
  public func writeLock() {
    pthread_rwlock_wrlock(self.systemLock)
  }
  
  public func tryWriteLock() -> Bool {
    return pthread_rwlock_trywrlock(self.systemLock) == 0
  }
  
  public func unlock() {
    pthread_rwlock_unlock(self.systemLock)
  }
  
  public func syncRead<T>(execute block: () throws -> T) rethrows -> T {
    self.readLock()
    defer { self.unlock() }
    return try block()
  }
  
  public func trySyncRead<T>(execute block: () throws -> T) rethrows -> T? {
    guard self.tryReadLock() else {
      return nil
    }
    defer { self.unlock() }
    return try block()
  }
  
  public func syncWrite<T>(execute block: () throws -> T) rethrows -> T {
    self.readLock()
    defer { self.unlock() }
    return try block()
  }
  
  public func trySyncWrite<T>(execute block: () throws -> T) rethrows -> T? {
    guard self.tryReadLock() else {
      return nil
    }
    defer { self.unlock() }
    return try block()
  }
}

/// Provides standalone read/write locks.
public final class ReadWriteLock {
  private let lock = EmbeddedReadWriteLock()
  
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
  private var systemCond: UnsafeMutablePointer<pthread_cond_t>
  
  public init() {
    let pointer = UnsafeMutablePointer<pthread_cond_t>.allocate(capacity: 1)
    pointer.initialize(to: pthread_cond_t())
    pthread_cond_init(pointer, nil)
    self.systemCond = pointer
  }
  
  public func release() {
    pthread_cond_destroy(self.systemCond)
    self.systemCond.deinitialize(count: 1)
    self.systemCond.deallocate()
  }
  
  /// Wakes all threads waiting on this condition.
  public func broadcast() {
    pthread_cond_broadcast(self.systemCond)
  }
  
  /// Wakes one thread waiting on this condition.
  public func signal() {
    pthread_cond_signal(self.systemCond)
  }
  
  /// Atomically releases mutex and causes the calling thread to block on the condition
  /// variable until either `signal` or `broadcast` is invoked on the condition variable,
  /// in which case the thread will re-acquire the lock.
  public func wait(unlocking mutex: EmbeddedMutex) {
    pthread_cond_wait(self.systemCond, mutex.systemMutex)
  }
  
  /// Atomically releases mutex and causes the calling thread to block on the condition
  /// variable until either `signal` or `broadcast` is invoked on the condition variable,
  /// in which case the thread will re-acquire the lock.
  public func wait(unlocking cl: EmbeddedConditionLock) {
    pthread_cond_wait(self.systemCond, cl.mutex.systemMutex)
  }
  
  /// Atomically releases mutex and causes the calling thread to block on the condition
  /// variable until either `signal` or `broadcast` is invoked on the condition variable,
  /// in which case the thread will re-acquire the lock. This method returns false if
  /// the waiting times out.
  @discardableResult public func wait(_ timeout: TimeInterval,
                                      unlocking mutex: EmbeddedMutex) -> Bool {
    if timeout < 0 {
      pthread_cond_wait(self.systemCond, mutex.systemMutex)
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
    return pthread_cond_timedwait(self.systemCond, mutex.systemMutex, &ts) == 0
  }
  
  /// Atomically releases mutex and causes the calling thread to block on the condition
  /// variable until either `signal` or `broadcast` is invoked on the condition variable,
  /// in which case the thread will re-acquire the lock. This method returns false if
  /// the waiting times out.
  @discardableResult public func wait(until absTime: Double,
                                      unlocking mutex: EmbeddedMutex) -> Bool {
    var tv = timeval(tv_sec: 0, tv_usec: 0)
    gettimeofday(&tv, nil)
    let current = Double(tv.tv_sec) + (Double(tv.tv_usec) / 1000000.0)
    if absTime <= current {
      return false
    }
    let timeInMs = Int((absTime - current) * 1000.0)
    var ts = timespec()
    ts.tv_sec = time(nil) + timeInMs / 1000
    ts.tv_nsec = Int(Int(tv.tv_usec * 1000) + 1000000 * (timeInMs % 1000))
    ts.tv_sec += ts.tv_nsec / 1000000000
    ts.tv_nsec %= 1000000000
    return pthread_cond_timedwait(self.systemCond, mutex.systemMutex, &ts) == 0
  }
  
  /// Atomically releases mutex and causes the calling thread to block on the condition
  /// variable until either `signal` or `broadcast` is invoked on the condition variable,
  /// in which case the thread will re-acquire the lock. This method returns false if
  /// the waiting times out.
  @discardableResult public func wait(_ timeout: TimeInterval,
                                      unlocking cl: EmbeddedConditionLock) -> Bool {
    return self.wait(timeout, unlocking: cl.mutex)
  }
  
  /// Atomically releases mutex and causes the calling thread to block on the condition
  /// variable until either `signal` or `broadcast` is invoked on the condition variable,
  /// in which case the thread will re-acquire the lock. This method returns false if
  /// the waiting times out.
  @discardableResult public func wait(until absTime: Double,
                                      unlocking cl: EmbeddedConditionLock) -> Bool {
    return self.wait(until: absTime, unlocking: cl.mutex)
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
    self.condition.wait(unlocking: mutex.mutex)
  }
  
  @discardableResult public func wait(_ timeout: TimeInterval, unlocking mutex: Mutex) -> Bool {
    return self.condition.wait(timeout, unlocking: mutex.mutex)    
  }
  
  @discardableResult public func wait(until absTime: Double, unlocking mutex: Mutex) -> Bool {
    return self.condition.wait(until: absTime, unlocking: mutex.mutex)    
  }
}

/// Implements an embeddable condition lock, combining a condition variable with a mutex.
/// Such a struct is typically embedded in a class. It is important to invoke `release`
/// when an object using an embedded read/write lock is being de-initialized.
public struct EmbeddedConditionLock {
  private var condition: EmbeddedCondition
  fileprivate var mutex: EmbeddedMutex
  
  public init(recursive: Bool = false) {
    self.condition = EmbeddedCondition()
    self.mutex = EmbeddedMutex(recursive: recursive)
  }
  
  public func release() {
    self.condition.release()
    self.mutex.release()
  }
  
  public func lock() {
    self.mutex.lock()
  }
  
  public func unlock() {
    self.mutex.unlock()
  }
  
  public func broadcast() {
    self.condition.broadcast()
  }
  
  public func signal() {
    self.condition.signal()
  }
  
  public func wait() {
    self.condition.wait(unlocking: self.mutex)
  }
  
  @discardableResult public func wait(_ timeout: TimeInterval) -> Bool {
    return self.condition.wait(timeout, unlocking: self.mutex)
  }
  
  @discardableResult public func wait(until absTime: Double) -> Bool {
    return self.condition.wait(until: absTime, unlocking: self.mutex)
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
  
  @discardableResult public func wait(_ timeout: TimeInterval) -> Bool {
    return self.conditionLock.wait(timeout)
  }
  
  @discardableResult public func wait(until absTime: Double) -> Bool {
    return self.conditionLock.wait(until: absTime)
  }
}
