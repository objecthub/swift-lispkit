//
//  Promise.swift
//  LispKit
//
//  Created by Matthias Zenger on 23/01/2016.
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
/// Class `Promise` is used to represent promises natively in LispKit.
///
public final class Promise: ManagedObject, CustomStringConvertible {
  
  public enum Kind: CustomStringConvertible {
    case promise
    case stream
    
    public var description: String {
      switch self {
        case .promise:
          return "promise"
        case .stream:
          return "stream"
      }
    }
  }
  
  public indirect enum State: CustomStringConvertible {
    case lazy(Procedure)       // Thunk of promises
    case shared(Promise)       // Shared promise
    case value(Expr)           // Evaluated promise
    
    public var description: String {
      switch self {
        case .lazy(let proc):
          return "lazy(\(proc))"
        case .shared(let future):
          return "shared(\(future))"
        case .value(let value):
          return "value(\(value))"
      }
    }
  }

  /// The kind of promise: either a regular promise or a stream.
  public let kind: Kind
  
  /// State of the promise; this state is modified externally.
  public var state: State

  /// Reference to this object in the managed object pool.
  public var managementRef: Int?

  /// Initializes a promise with a `thunk` that yields a promise; this promise's state is
  /// copied over into this promise as part of the protocol to force a promise.
  public init(kind: Kind, thunk: Procedure) {
    self.kind = kind
    self.state = .lazy(thunk)
  }
  
  /// Initializes a promise with a given value; no evaluation will happen.
  public init(kind: Kind, value: Expr) {
    self.kind = kind
    self.state = .value(value)
  }

  /// Returns true if this refers to only "simple" values (i.e. values which won't lead to
  /// cyclic references)
  public var isAtom: Bool {
    switch self.state {
      case .lazy(_):
        return false
      case .shared(let future):
        return future.isAtom
      case .value(let expr):
        return expr.isAtom
    }
  }

  /// Is this promise a stream?
  public var isStream: Bool {
    return self.kind == .stream
  }
  
  /// String representation of the promise.
  public var description: String {
    return "\(self.kind)#\(self.state)"
  }
  
  /// Remove references to expressions from this future.
  public override func clean() {
    self.state = .value(.null)
  }
}
