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


open class Checkpointer: CustomStringConvertible {
  fileprivate var address: UInt
  fileprivate var checkpoints: [UInt : Set<CheckpointData>]
  
  public init() {
    self.address = 0
    self.checkpoints = [:]
  }
  
  open func checkpoint() -> UInt {
    self.address += 1
    return self.address
  }
  
  open func reset() {
    self.address = 0
  }
  
  open func associate(_ data: CheckpointData, with address: UInt) {
    assert(address <= self.address)
    if self.checkpoints[address] == nil {
      self.checkpoints[address] = [data]
    } else {
      self.checkpoints[address]!.insert(data)
    }
  }
  
  fileprivate func associations(_ address: UInt) -> Set<CheckpointData> {
    if let res = self.checkpoints[address] {
      return res
    }
    return []
  }
  
  open func systemDefined(_ address: UInt) -> Bool {
    for assoc in self.associations(address) {
      if assoc == .systemDefined {
        return true
      }
    }
    return false
  }
  
  open func fromGlobalEnv(_ address: UInt) -> Expr? {
    for assoc in self.associations(address) {
      if case .fromGlobalEnv(let expr) = assoc {
        return expr
      }
    }
    return nil
  }
  
  open func isValueBinding(_ sym: Symbol, at address: UInt) -> Bool {
    for assoc in self.associations(address) {
      if case .valueBinding(let s) = assoc , s == sym {
        return true
      }
    }
    return false
  }
  
  open func expansion(_ address: UInt) -> Expr? {
    for assoc in self.associations(address) {
      if case .expansion(let expr) = assoc {
        return expr
      }
    }
    return nil
  }
  
  open var description: String {
    return "Checkpoints (\(self.address)): \(self.checkpoints)"
  }
}

public enum CheckpointData: Hashable, CustomStringConvertible {
  case systemDefined
  case fromGlobalEnv(Expr)
  case valueBinding(Symbol)
  case expansion(Expr)
  
  public var hashValue: Int {
    switch self {
      case .systemDefined:
        return 1
      case .fromGlobalEnv(let expr):
        return expr.hashValue &* 31 &+ 2
      case .valueBinding(let sym):
        return sym.hashValue &* 31 &+ 3
      case .expansion(let expr):
        return expr.hashValue &* 31 &+ 4
    }
  }
  
  public var description: String {
    switch self {
      case .systemDefined:
        return "SystemDefined"
      case .fromGlobalEnv(let expr):
        return "FromGlobalEnv(\(expr.description))"
      case .valueBinding(let sym):
        return "ValueBinding(\(sym.description))"
      case .expansion(let expr):
        return "Expansion(\(expr.description))"
    }
  }
}

public func ==(left: CheckpointData, right: CheckpointData) -> Bool {
  switch (left, right) {
    case (.systemDefined, .systemDefined):
      return true
    case (.fromGlobalEnv(let lexpr), .fromGlobalEnv(let rexpr)):
      return eqExpr(rexpr, lexpr)
    case (.valueBinding(let lsym), .valueBinding(let rsym)):
      return lsym == rsym
    case (.expansion(let lexpr), .expansion(let rexpr)):
      return lexpr == rexpr
    default:
      return false
  }
}

