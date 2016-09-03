//
//  Future.swift
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
/// Class `Future` is used to represent promises in LispKit.
///
public final class Future: ManagedObject, CustomStringConvertible {
  
  public indirect enum State {
    case lazy(Procedure)       // Thunk of promises
    case shared(Future)        // Shared future
    case value(Expr)           // Evaluated future
    case thrown(Error)     // Failed evaluation of future
  }
  
  /// State of the future; this state is modified externally.
  public var state: State
  
  /// Maintain object statistics.
  internal static let stats = Stats("Future")
  
  /// Update object statistics.
  deinit {
    Future.stats.dealloc()
  }
  
  /// Initializes a future with a `thunk` that yields a promise; this promise's state is
  /// copied over into this future as part of the protocol to force a promise.
  public init(_ thunk: Procedure) {
    self.state = .lazy(thunk)
    super.init(Future.stats)
  }
  
  /// Initializes a future with a given value; no evaluation will happen.
  public init(_ value: Expr) {
    self.state = .value(value)
    super.init(Future.stats)
  }
  
  /// Returns true if this refers to only "simple" values (i.e. values which won't lead to
  /// cyclic references)
  public var isSimple: Bool {
    switch self.state {
      case .lazy(_):
        return true
      case .shared(let future):
        return future.isSimple
      case .value(let expr):
        return expr.isSimple
      case .thrown(_):
        return false
    }
  }
  
  /// String representation of the future.
  public var description: String {
    switch self.state {
      case .lazy(let proc):
        return "future#lazy(\(proc))"
      case .shared(let future):
        return "future#shared(\(future))"
      case .value(let value):
        return "future#value(\(value))"
      case .thrown(let error):
        return "future#thrown(\(error))"
    }
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
