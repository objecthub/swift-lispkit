//
//  Checkpointer.swift
//  LispKit
//
//  Created by Matthias Zenger on 14/05/2016.
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


public class Checkpointer: CustomStringConvertible {
  private var address: UInt
  private var checkpoints: [UInt : Set<CheckpointData>]
  
  public init() {
    self.address = 0
    self.checkpoints = [:]
  }
  
  public func checkpoint() -> UInt {
    self.address += 1
    return self.address
  }
  
  public func reset() {
    self.address = 0
  }
  
  public func associate(data: CheckpointData, with address: UInt) {
    assert(address <= self.address)
    if self.checkpoints[address] == nil {
      self.checkpoints[address] = [data]
    } else {
      self.checkpoints[address]!.insert(data)
    }
  }
  
  private func associations(address: UInt) -> Set<CheckpointData> {
    if let res = self.checkpoints[address] {
      return res
    }
    return []
  }
  
  public func systemDefined(address: UInt) -> Bool {
    for assoc in self.associations(address) {
      if assoc == .SystemDefined {
        return true
      }
    }
    return false
  }
  
  public func fromGlobalEnv(address: UInt) -> Expr? {
    for assoc in self.associations(address) {
      if case .FromGlobalEnv(let expr) = assoc {
        return expr
      }
    }
    return nil
  }
  
  public func isValueBinding(sym: Symbol, at address: UInt) -> Bool {
    for assoc in self.associations(address) {
      if case .ValueBinding(let s) = assoc where s == sym {
        return true
      }
    }
    return false
  }
  
  public func expansion(address: UInt) -> Expr? {
    for assoc in self.associations(address) {
      if case .Expansion(let expr) = assoc {
        return expr
      }
    }
    return nil
  }
  
  public var description: String {
    return "Checkpoints (\(self.address)): \(self.checkpoints)"
  }
}

public enum CheckpointData: Hashable, CustomStringConvertible {
  case SystemDefined
  case FromGlobalEnv(Expr)
  case ValueBinding(Symbol)
  case Expansion(Expr)
  
  public var hashValue: Int {
    switch self {
      case SystemDefined:
        return 1
      case FromGlobalEnv(let expr):
        return expr.hashValue &* 31 &+ 2
      case ValueBinding(let sym):
        return sym.hashValue &* 31 &+ 3
      case Expansion(let expr):
        return expr.hashValue &* 31 &+ 4
    }
  }
  
  public var description: String {
    switch self {
      case SystemDefined:
        return "SystemDefined"
      case FromGlobalEnv(let expr):
        return "FromGlobalEnv(\(expr.description))"
      case ValueBinding(let sym):
        return "ValueBinding(\(sym.description))"
      case Expansion(let expr):
        return "Expansion(\(expr.description))"
    }
  }
}

public func ==(left: CheckpointData, right: CheckpointData) -> Bool {
  switch (left, right) {
    case (.SystemDefined, .SystemDefined):
      return true
    case (.FromGlobalEnv(let lexpr), .FromGlobalEnv(let rexpr)):
      return eqExpr(lexpr, rexpr)
    case (.ValueBinding(let lsym), .ValueBinding(let rsym)):
      return lsym == rsym
    case (.Expansion(let lexpr), .Expansion(let rexpr)):
      return lexpr == rexpr
    default:
      return false
  }
}

