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


public final class Checkpointer: CustomStringConvertible {
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
  
  public func associate(_ data: CheckpointData, with address: UInt) {
    assert(address <= self.address)
    if self.checkpoints[address] == nil {
      self.checkpoints[address] = [data]
    } else {
      self.checkpoints[address]!.insert(data)
    }
  }
  
  private func associations(_ address: UInt) -> Set<CheckpointData> {
    if let res = self.checkpoints[address] {
      return res
    }
    return []
  }
  
  public func systemDefined(_ address: UInt) -> Bool {
    for assoc in self.associations(address) {
      if assoc == .systemDefined {
        return true
      }
    }
    return false
  }
  
  public func imported(_ address: UInt) -> Bool {
    for assoc in self.associations(address) {
      if assoc == .imported {
        return true
      }
    }
    return false
  }
  
  public func fromGlobalEnv(_ address: UInt) -> Expr? {
    for assoc in self.associations(address) {
      if case .fromGlobalEnv(let expr) = assoc {
        return expr
      }
    }
    return nil
  }
  
  public func isValueBinding(_ sym: Symbol, at address: UInt) -> Bool {
    for assoc in self.associations(address) {
      if case .valueBinding(let s) = assoc , s == sym {
        return true
      }
    }
    return false
  }
  
  public func expansion(_ address: UInt) -> Expr? {
    for assoc in self.associations(address) {
      if case .expansion(let expr) = assoc {
        return expr
      }
    }
    return nil
  }
  
  public var description: String {
    return "Checkpoints (\(self.address)): \(self.checkpoints)"
  }
}

public enum CheckpointData: Equatable, Hashable, CustomStringConvertible {
  case systemDefined
  case imported
  case fromGlobalEnv(Expr)
  case valueBinding(Symbol)
  case expansion(Expr)
  
  public var description: String {
    switch self {
      case .systemDefined:
        return "SystemDefined"
      case .imported:
        return "Imported"
      case .fromGlobalEnv(let expr):
        return "FromGlobalEnv(\(expr.description))"
      case .valueBinding(let sym):
        return "ValueBinding(\(sym.description))"
      case .expansion(let expr):
        return "Expansion(\(expr.description))"
    }
  }
  
  public static func ==(left: CheckpointData, right: CheckpointData) -> Bool {
    switch (left, right) {
      case (.systemDefined, .systemDefined):
        return true
      case (.imported, .imported):
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
}
