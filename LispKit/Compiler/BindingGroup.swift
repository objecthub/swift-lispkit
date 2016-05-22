//
//  BindingGroup.swift
//  LispKit
//
//  Created by Matthias Zenger on 14/02/2016.
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

public class Definition: Reference, CustomStringConvertible {
  
  public enum Kind {
    case Value
    case Variable
    case MutatedVariable
    case Macro(Procedure)
  }
  
  public let index: Int
  public var kind: Kind
  
  private init(index: Int, isVar: Bool = true) {
    self.index = index
    self.kind = isVar ? .Variable : .Value
  }
  
  private init(proc: Procedure) {
    self.index = 0
    self.kind = .Macro(proc)
  }
  
  public var isValue: Bool {
    switch self.kind {
      case .Value:
        return true
      default:
        return false
    }
  }
  
  public var isVariable: Bool {
    switch self.kind {
      case .Variable, .MutatedVariable:
        return true
      default:
        return false
    }
  }
  
  public var isImmutableVariable: Bool {
    switch self.kind {
      case .Variable:
        return true
      default:
        return false
    }
  }
  
  public func wasMutated() {
    switch self.kind {
      case .Value:
        preconditionFailure("cannot declare value mutable")
      case .Variable:
        self.kind = .MutatedVariable
      case .MutatedVariable:
        break
      case .Macro(_):
        preconditionFailure("cannot declare macro mutable")
    }
  }
  
  public var description: String {
    switch self.kind {
      case .Value:
        return "value at index \(self.index)"
      case .Variable:
        return "variable at index \(self.index)"
      case .MutatedVariable:
        return "mutated variable at index \(self.index)"
      case .Macro(let proc):
        return "macro \(proc)"
    }
  }
}

public final class BindingGroup: Reference, CustomStringConvertible {
  public unowned var owner: Compiler
  public let parent: Env
  private var bindings: [Symbol : Definition]
  private let nextIndex: () -> Int
  internal let checkpoint: UInt
  public private(set) var box: WeakBox<BindingGroup>!
  
  public init(owner: Compiler, parent: Env, nextIndex: (() -> Int)? = nil) {
    self.owner = owner
    self.parent = parent
    self.bindings = [Symbol: Definition]()
    self.nextIndex = nextIndex == nil ? owner.nextLocalIndex : nextIndex!
    self.checkpoint = owner.checkpointer.checkpoint()
    super.init()
    self.box = WeakBox(self)
  }
  
  public func defineMacro(sym: Symbol, proc: Procedure) -> Definition {
    let def = Definition(proc: proc)
    self.bindings[sym] = def
    return def
  }
  
  public func allocBindingFor(sym: Symbol) -> Definition {
    let variable = !owner.checkpointer.isValueBinding(sym, at: self.checkpoint)
    if let binding = self.bindings[sym] {
      return binding
    }
    let binding = Definition(index: self.nextIndex(), isVar: variable)
    self.bindings[sym] = binding
    return binding
  }
  
  public func bindingFor(sym: Symbol) -> Definition? {
    return self.bindings[sym]
  }
  
  public func symbolAtIndex(index: Int) -> Symbol? {
    for (sym, bind) in self.bindings {
      if bind.index == index {
        return sym
      }
    }
    return nil
  }
  
  public func covers(group: BindingGroup) -> Bool {
    var env: Env = .Local(self)
    while case .Local(let this) = env {
      if this === group {
        return true
      }
      env = this.parent
    }
    return false
  }
  
  public var count: Int {
    return self.bindings.count
  }
  
  public var symbols: [Symbol?] {
    var seq = [Symbol?](count: self.bindings.count, repeatedValue: nil)
    for (sym, bind) in self.bindings {
      var extendBy = 1 + bind.index - seq.count
      while extendBy > 0 {
        seq.append(nil)
        extendBy -= 1
      }
      seq[bind.index] = sym
    }
    return seq
  }
    
  public func macroGroup() -> BindingGroup {
    var env = self.parent
    while case .Local(let group) = env {
      env = group.parent
    }
    let macroGroup = BindingGroup(owner: self.owner, parent: env, nextIndex: { return 0 })
    env = .Local(self)
    while case .Local(let group) = env {
      for (sym, bind) in group.bindings {
        if case .Macro(_) = bind.kind where macroGroup.bindingFor(sym) == nil {
          macroGroup.bindings[sym] = bind
        }
      }
      env = group.parent
    }
    return macroGroup
  }
  
  public func finalize() {
    for (sym, binding) in self.bindings {
      if binding.isImmutableVariable {
        owner.checkpointer.associate(.ValueBinding(sym), with: self.checkpoint)
      }
    }
  }
  
  public var description: String {
    var seq = self.symbols
    var builder = StringBuilder()
    for index in seq.indices {
      builder.append(index, width: 5, alignRight: true)
      builder.append(": ")
      builder.append(seq[index]?.description ?? "<undef>")
      builder.appendNewline()
    }
    return builder.description
  }
}
