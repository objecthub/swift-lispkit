//
//  EvalCondition.swift
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
/// `EvalCondition` implements objects representing condition variables in LispKit code.
/// 
/// A condition variable represents a set of blocked threads. These blocked threads are
/// waiting for a certain condition to become true. When a thread modifies some program
/// state that might make the condition true, the thread unblocks some number of threads
/// (one or all depending on the primitive used) so they can check the value of the
/// condition. This allows complex forms of interthread synchronization to be expressed
/// more conveniently than with mutexes alone.
/// 
public final class EvalCondition: NativeObject, ThreadBlocker {
  
  /// Type representing threads
  public static let type = Type.objectType(Symbol(uninterned: "condition"))
  
  /// The internal condition used to implement the condition variable
  private var condition = EmbeddedCondition()
  
  /// The name of the condition variable
  public let name: Expr
  
  /// The tag of the condition variable
  public let tag: Expr
  
  /// Initialize a new condition variable
  init(name: Expr = .false, tag: Expr = .false) {
    self.name = name
    self.tag = tag
  }
  
  deinit {
    self.condition.release()
  }
  
  public func wait(in thread: EvalThread,
                   timeout: TimeInterval? = nil,
                   unlocking mutex: EvalMutex) throws -> Bool {
    thread.waitingOn = self
    var res = true
    do {
      defer {
        thread.waitingOn = nil
      }
      if let timeout = timeout {
        res = self.condition.wait(timeout, unlocking: mutex.mutex)
      } else {
        self.condition.wait(unlocking: mutex.mutex)
        res = true
      }
    }
    if thread.aborted {
      throw RuntimeError.abortion()
    }
    return res
  }
  
  public func signal() {
    self.condition.signal()
  }
  
  public func broadcast() {
    self.condition.broadcast()
  }
  
  public func wakeBlockedThreads() {
    self.condition.broadcast()
  }
  
  public override var type: Type {
    return Self.type
  }
  
  public override var string: String {
    let nameStr = self.name.isFalse ? self.identityString : self.name.description
    return "#<condition \(nameStr)>"
  }
  
  public override func unpack(in context: Context) -> Exprs {
    let nameStr = self.name.isFalse ? self.identityString : self.name.description
    return [.makeString(self.identityString),
            .makeString(nameStr),
            self.tag]
  }
}
