//
//  Checkpointer.swift
//  LispKit
//
//  Created by Matthias Zenger on 14/05/2016.
//  Copyright © 2016 ObjectHub. All rights reserved.
//


public class Checkpointer {
  private var address: UInt
  private var checkpoints: [UInt : Set<CheckpointData>]
  
  public init() {
    self.address = 0
    self.checkpoints = [:]
  }
  
  public func checkpoint() {
    self.address += 1
  }
  
  public func reset() {
    self.address = 0
  }
  
  public func associateWith(data: CheckpointData) {
    if self.checkpoints[self.address] == nil {
      self.checkpoints[self.address] = [data]
    } else {
      self.checkpoints[self.address]!.insert(data)
    }
  }
  
  public var associations: Set<CheckpointData> {
    if let res = self.checkpoints[self.address] {
      return res
    }
    return []
  }
  
  public var fromGlobalEnv: Expr? {
    for assoc in self.associations {
      if case .FromGlobalEnv(let expr) = assoc {
        return expr
      }
    }
    return nil
  }
  
  public func isValueBinding(sym: Symbol) -> Bool {
    for assoc in self.associations {
      if case .ValueBinding(let s) = assoc where s == sym {
        return true
      }
    }
    return false
  }
  
  public var expansion: Expr? {
    for assoc in self.associations {
      if case .Expansion(let expr) = assoc {
        return expr
      }
    }
    return nil
  }
}

public enum CheckpointData: Hashable {
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

