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

public final class BindingGroup: Reference, CustomStringConvertible {
  public unowned var owner: Compiler
  public let parent: Env
  private var bindings: [Symbol : Definition]
  private let nextIndex: (() -> Int)?
  internal let checkpoint: UInt
  public private(set) var box: WeakBox<BindingGroup>!
  
  public init(owner: Compiler, parent: Env, nextIndex: (() -> Int)? = nil) {
    self.owner = owner
    self.parent = parent
    self.bindings = [Symbol: Definition]()
    self.nextIndex = nextIndex
    self.checkpoint = owner.checkpointer.checkpoint()
    super.init()
    self.box = WeakBox(self)
  }
  
  @discardableResult public func defineMacro(_ sym: Symbol, proc: Procedure) -> Definition {
    let def = Definition(proc: proc)
    self.bindings[sym] = def
    return def
  }
  
  @discardableResult public func allocBindingFor(_ sym: Symbol) -> Definition {
    let variable = !owner.checkpointer.isValueBinding(sym, at: self.checkpoint)
    if let binding = self.bindings[sym] {
      return binding
    }
    let binding = Definition(index: self.nextIndex?() ?? owner.nextLocalIndex(), isVar: variable)
    self.bindings[sym] = binding
    return binding
  }
  
  public func bindingFor(_ sym: Symbol) -> Definition? {
    return self.bindings[sym]
  }
  
  public func symbol(at index: Int) -> Symbol? {
    for (sym, bind) in self.bindings {
      if bind.index == index {
        return sym
      }
    }
    return nil
  }
  
  public func covers(_ group: BindingGroup) -> Bool {
    var env: Env = .local(self)
    while case .local(let this) = env {
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
    var seq = [Symbol?](repeating: nil, count: self.bindings.count)
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
    while case .local(let group) = env {
      env = group.parent
    }
    let macroGroup = BindingGroup(owner: self.owner, parent: env, nextIndex: { return 0 })
    env = .local(self)
    while case .local(let group) = env {
      for (sym, bind) in group.bindings {
        if case .macro(_) = bind.kind , macroGroup.bindingFor(sym) == nil {
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
        owner.checkpointer.associate(.valueBinding(sym), with: self.checkpoint)
      }
    }
  }
  
  public var description: String {
    let seq = self.symbols
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

public final class Definition: Reference, CustomStringConvertible {
  
  public enum Kind {
    case value
    case variable
    case mutatedVariable
    case macro(Procedure)
  }
  
  public let index: Int
  public private(set) var kind: Kind
  
  fileprivate init(index: Int, isVar: Bool = true) {
    self.index = index
    self.kind = isVar ? .variable : .value
  }
  
  fileprivate init(proc: Procedure) {
    self.index = 0
    self.kind = .macro(proc)
  }
  
  public var isValue: Bool {
    switch self.kind {
      case .value:
        return true
      default:
        return false
    }
  }
  
  public var isVariable: Bool {
    switch self.kind {
      case .variable, .mutatedVariable:
        return true
      default:
        return false
    }
  }
  
  public var isImmutableVariable: Bool {
    switch self.kind {
      case .variable:
        return true
      default:
        return false
    }
  }
  
  public func wasMutated() {
    switch self.kind {
      case .value:
        preconditionFailure("cannot declare value mutable")
      case .variable:
        self.kind = .mutatedVariable
      case .mutatedVariable:
        break
      case .macro(_):
        preconditionFailure("cannot declare macro mutable")
    }
  }
  
  public var description: String {
    switch self.kind {
      case .value:
        return "value at index \(self.index)"
      case .variable:
        return "variable at index \(self.index)"
      case .mutatedVariable:
        return "mutated variable at index \(self.index)"
      case .macro(let proc):
        return "macro \(proc)"
    }
  }
}
