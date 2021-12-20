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
  private var systemMutex = pthread_mutex_t()
  
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

  private var mutex: EmbeddedMutex
  
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
