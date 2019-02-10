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
    case shared(Promise)       // Shared future
    case value(Expr)           // Evaluated future
    case thrown(Error)         // Failed evaluation of future

    public var description: String {
      switch self {
        case .lazy(let proc):
          return "lazy(\(proc))"
        case .shared(let future):
          return "shared(\(future))"
        case .value(let value):
          return "value(\(value))"
        case .thrown(let error):
          return "thrown(\(error))"
      }
    }
  }

  public let kind: Kind

  /// State of the future; this state is modified externally.
  public var state: State

  /// Maintain object statistics.
  internal static let stats = Stats("Promise")

  /// Update object statistics.
  deinit {
    Promise.stats.dealloc()
  }

  /// Initializes a future with a `thunk` that yields a promise; this promise's state is
  /// copied over into this future as part of the protocol to force a promise.
  public init(kind: Kind, thunk: Procedure) {
    self.kind = kind
    self.state = .lazy(thunk)
    super.init(Promise.stats)
  }

  /// Initializes a future with a given value; no evaluation will happen.
  public init(kind: Kind, value: Expr) {
    self.kind = kind
    self.state = .value(value)
    super.init(Promise.stats)
  }

  /// Returns true if this refers to only "simple" values (i.e. values which won't lead to
  /// cyclic references)
  public var isAtom: Bool {
    switch self.state {
      case .lazy(_):
        return true
      case .shared(let future):
        return future.isAtom
      case .value(let expr):
        return expr.isAtom
      case .thrown(_):
        return false
    }
  }

  /// Is this promise a stream?
  public var isStream: Bool {
    return self.kind == .stream
  }

  /// String representation of the future.
  public var description: String {
    return "\(self.kind)#\(self.state)"
  }

  /// Mark the expressions referenced from this future.
  public override func mark(_ tag: UInt8) {
    if self.tag != tag {
      self.tag = tag
      switch self.state {
        case .lazy(let proc):
          proc.mark(tag)
        case .shared(let future):
          future.mark(tag)
        case .value(let expr):
          expr.mark(tag)
        case .thrown(_):
          // TODO: Figure out how to mark errors
          break
      }
    }
  }

  /// Remove references to expressions from this future.
  public override func clean() {
    self.state = .value(.null)
  }
}
