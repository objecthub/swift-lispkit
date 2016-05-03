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
    case Unevaluated(Procedure)
    case Value(Expr)
    case Thrown(ErrorType)
  }
  
  public var state: State
  
  /// Maintain object statistics.
  internal static let stats = Stats("Future")
  
  /// Update object statistics.
  deinit {
    Future.stats.dealloc()
  }
  
  /// Initializes a future with a `thunk` that yields the result of the future once executed.
  public init(_ thunk: Procedure) {
    self.state = .Unevaluated(thunk)
    super.init(Future.stats)
  }
  
  public var description: String {
    switch self.state {
      case .Unevaluated(let proc):
        return "future#unevaluated(\(proc))"
      case .Value(let value):
        return "future#value(\(value))"
      case .Thrown(let error):
        return "future#thrown(\(error))"
    }
  }
  
  public override func mark(tag: UInt8) {
    if self.tag != tag {
      self.tag = tag
      switch self.state {
        case .Unevaluated(_):
          break
        case .Value(let expr):
          expr.mark(tag)
        case .Thrown(_):
          // TODO: Figure out how to mark errors
          break
      }
    }
  }
  
  public override func clean() {
    self.state = .Value(.Null)
  }
}
