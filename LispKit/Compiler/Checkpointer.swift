//
//  Checkpointer.swift
//  LispKit
//
//  Created by Matthias Zenger on 14/05/2016.
//  Copyright © 2016 ObjectHub. All rights reserved.
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
  
  public func associations(address: UInt) -> Set<CheckpointData> {
    if let res = self.checkpoints[address] {
      return res
    }
    return []
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
  case FromGlobalEnv(Expr)
  case ValueBinding(Symbol)
  case Expansion(Expr)
  
  public var hashValue: Int {
    switch self {
      case FromGlobalEnv(let expr):
        return expr.hashValue &* 31 + 1
      case ValueBinding(let sym):
        return sym.hashValue &* 31 + 2
      case Expansion(let expr):
        return expr.hashValue &* 31 + 3
    }
  }
  
  public var description: String {
    switch self {
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
    case (.FromGlobalEnv(let lexpr), .FromGlobalEnv(let rexpr)):
      return lexpr == rexpr
    case (.ValueBinding(let lsym), .ValueBinding(let rsym)):
      return lsym == rsym
    case (.Expansion(let lexpr), .Expansion(let rexpr)):
      return lexpr == rexpr
    default:
      return false
  }
}

