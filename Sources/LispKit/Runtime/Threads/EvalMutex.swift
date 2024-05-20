//
//  EvalMutex.swift
//  LispKit
//
//  Created by Matthias Zenger on 01/01/2022.
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

///
/// `EvalMutex` implements objects representing mutexes in LispKit code.
/// 
/// A mutex can be in one of four states: _locked_ (either owned or not owned)
/// and _unlocked_ (either abandoned or not abandoned). An attempt to lock a mutex
/// only succeeds if the mutex is in an unlocked state, otherwise the current thread
/// must wait. A mutex in the locked/owned state has an associated "owner" thread,
/// which by convention is the thread that is responsible for unlocking the mutex
/// A mutex in the locked/not-owned state is not linked to a particular thread.
/// A mutex becomes locked when a thread locks it using the `mutex-lock!` primitive.
/// A mutex becomes unlocked/abandoned when the owner of a locked/owned mutex
/// terminates. A mutex becomes unlocked/not-abandoned when a thread unlocks it
/// using the `mutex-unlock!` primitive.
/// 
public final class EvalMutex: NativeObject, ThreadBlocker {
  
  /// Type representing threads
  public static let type = Type.objectType(Symbol(uninterned: "mutex"))
  
  /// Every mutex has one of the following four states (see above)
  public enum State: Int, CustomStringConvertible {
    case lockedOwned
    case lockedNotOwned
    case unlockedAbandoned
    case unlockedNotAbandoned
    
    public var description: String {
      switch self {
        case .lockedOwned:
          return "locked/owned"
        case .lockedNotOwned:
          return "locked/not-owned"
        case .unlockedAbandoned:
          return "unlocked/abandoned"
        case .unlockedNotAbandoned:
          return "unlocked/not-abandoned"
      }
    }
  }
  
  /// Used to implement mutex semantics
  internal var mutex = EmbeddedConditionLock(recursive: true)
  
  /// The name of the mutex
  public let name: Expr
  
  /// The tag of the mutex
  public let tag: Expr
  
  /// The owner thread, if the mutex is in state "locked/owned"
  internal private(set) weak var owner: EvalThread?
  
  /// The state of the mutex
  public private(set) var state: State
  
  /// Initialize a new mutex.
  init(name: Expr = .false, tag: Expr = .false) {
    self.name = name
    self.tag = tag
    self.owner = nil
    self.state = .unlockedNotAbandoned
  }
  
  deinit {
    self.mutex.release()
  }
  
  public var isLocked: Bool {
    return self.state == .lockedOwned || self.state == .lockedNotOwned
  }
  
  public func tryLock(in thread: EvalThread, for owner: EvalThread?) throws -> Bool {
    self.mutex.lock()
    defer {
      self.mutex.unlock()
    }
    return try self.lockInternal(in: thread, for: owner, deadline: 0.0)
  }
  
  public func lock(in thread: EvalThread,
                   for owner: EvalThread?,
                   timeout: TimeInterval? = nil) throws -> Bool {
    let deadline: Double? = timeout == nil ? nil : Timer.currentTimeInSec + timeout!
    self.mutex.lock()
    defer {
      self.mutex.unlock()
    }
    return try self.lockInternal(in: thread, for: owner, deadline: deadline)
  }
  
  private func lockInternal(in thread: EvalThread,
                            for owner: EvalThread?,
                            deadline: Double? = nil) throws -> Bool {
    thread.waitingOn = self
    do {
      defer {
        thread.waitingOn = nil
      }
      while self.isLocked {
        if thread.aborted {
          throw RuntimeError.abortion()
        }
        if let deadline = deadline {
          if deadline <= 0.0 {
            return false
          } else if !self.mutex.wait(until: deadline) {
            return false
          }
        } else {
          self.mutex.wait()
        }
      }
    }
    let formerState = self.state
    if owner == nil {
      self.owner = nil
      self.state = .lockedNotOwned
    } else if owner!.claimLock(self) {
      self.owner = owner
      self.state = .lockedOwned
    } else {
      self.owner = nil
      self.state = .unlockedAbandoned
      self.mutex.signal()
    }
    if formerState == .unlockedAbandoned {
      throw RuntimeError.eval(.abandonedMutex, .object(self))
    }
    return true
  }
  
  public func unlock(in thread: EvalThread,
                     condition: EvalCondition? = nil,
                     timeout: TimeInterval? = nil) throws -> Bool {
    self.mutex.lock()
    defer {
      self.mutex.unlock()
    }
    if self.isLocked {
      self.owner?.unclaimLock(self)
      self.state = .unlockedNotAbandoned
      self.owner = nil
      self.mutex.signal()
    }
    return try condition?.wait(in: thread, timeout: timeout, unlocking: self) ?? true
  }
  
  public func wait(in thread: EvalThread,
                   for condition: EvalCondition? = nil,
                   timeout: TimeInterval? = nil) throws -> Bool {
    self.mutex.lock()
    defer {
      self.mutex.unlock()
    }
    var owner: EvalThread? = nil
    if self.isLocked {
      owner = self.owner
      self.owner?.unclaimLock(self)
      self.state = .unlockedNotAbandoned
      self.owner = nil
      self.mutex.signal()
    }
    let res = try condition?.wait(in: thread, timeout: timeout, unlocking: self) ?? true
    _ = try self.lockInternal(in: thread, for: owner)
    return res
  }
  
  public func abandon() {
    self.mutex.lock()
    defer {
      self.mutex.unlock()
    }
    if self.isLocked {
      self.owner = nil
      self.state = .unlockedAbandoned
      self.mutex.signal()
    }
  }
  
  public func wakeBlockedThreads() {
    self.mutex.lock()
    defer {
      self.mutex.unlock()
    }
    self.mutex.broadcast()
  }
  
  public override func mark(in gc: GarbageCollector) {
    gc.markLater(self.name)
    gc.markLater(self.tag)
  }
  
  public override var type: Type {
    return Self.type
  }
  
  public override var string: String {
    let nameStr = self.name.isFalse ? self.identityString : self.name.description
    return "#<mutex \(nameStr): \(self.state)>"
  }
  
  public override func unpack(in context: Context) -> Exprs {
    let nameStr = self.name.isFalse ? self.identityString : self.name.description
    return [.makeString(self.identityString),
            .makeString(nameStr),
            .makeNumber(self.state.rawValue),
            self.tag]
  }
}
