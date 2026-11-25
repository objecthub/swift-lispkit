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
    if let binding = self.bindings[sym] {
      return binding
    }
    let kind: Definition.Kind
    if owner.checkpointer.isValueBinding(sym, at: self.checkpoint) {
      kind = .value
    } else if owner.checkpointer.isUnboxedBinding(sym, at: self.checkpoint) {
      kind = .unboxedVariable
    } else {
      kind = .variable
    }
    let binding = Definition(index: self.nextIndex?() ?? owner.nextLocalIndex(),
                             kind: kind,
                             scopeStart: owner.instructionCount)
    self.bindings[sym] = binding
    return binding
  }
  
  /// Binds `sym` to the given loop. This is used for compiling named `let` forms into loops.
  @discardableResult public func defineLoop(_ sym: Symbol, loop: Definition.Loop) -> Definition {
    let def = Definition(loop: loop)
    self.bindings[sym] = def
    return def
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
  
  /// Finalizes this binding group at the end of the scope of its bindings. This records in
  /// the checkpointer which bindings can be stored directly on the stack in the second
  /// compilation phase:
  ///
  ///   - Variables that are never mutated are stored as values; closures capture a copy of
  ///     the value.
  ///   - Mutated variables are stored unboxed if they are not captured by a closure and if
  ///     no procedure is called (other than via a tail call) within their scope. A procedure
  ///     call could capture a continuation, and re-entering such a continuation restores the
  ///     stack. With an unboxed variable, this would also restore the value of the variable,
  ///     whereas the location of a boxed variable is shared by all continuations.
  public func finalize() {
    for (sym, binding) in self.bindings {
      switch binding.kind {
        case .variable:
          owner.checkpointer.associate(.valueBinding(sym), with: self.checkpoint)
        case .mutatedVariable:
          if !binding.isCaptured && !owner.containsCalls(from: binding.scopeStart) {
            owner.checkpointer.associate(.unboxedBinding(sym), with: self.checkpoint)
          }
        default:
          break
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
    
    /// An immutable variable stored on the stack
    case value
    
    /// A variable stored in a box which has not been mutated (so far)
    case variable
    
    /// A mutated variable stored in a box
    case mutatedVariable
    
    /// A mutable variable stored on the stack; such variables are never captured
    case unboxedVariable
    
    /// The name of a named `let` which is compiled into a loop
    case loop(Loop)
    
    /// A macro
    case macro(Procedure)
  }
  
  /// Represents a named `let` that is compiled into a loop. Calls of the loop are only
  /// allowed in tail position of the body of the named `let`. They assign the arguments
  /// to `params` and jump to the instruction at index `entryIp`.
  public final class Loop {
    public unowned let owner: Compiler
    public let params: [Definition]
    public internal(set) var entryIp: Int
    
    internal init(owner: Compiler, params: [Definition], entryIp: Int) {
      self.owner = owner
      self.params = params
      self.entryIp = entryIp
    }
  }
  
  /// Collects information about a named `let` in the first compilation phase to find out
  /// if it can be compiled into a loop: this is the case if all references to the name of
  /// the named `let` are calls in tail position of the body with the right number of
  /// arguments.
  internal final class LoopCandidate {
    unowned let owner: Compiler
    let arity: Int
    var references: Int = 0
    var loopCalls: Int = 0
    
    init(owner: Compiler, arity: Int) {
      self.owner = owner
      self.arity = arity
    }
    
    var isLoop: Bool {
      return self.references == self.loopCalls
    }
  }
  
  public let index: Int
  public private(set) var kind: Kind
  
  /// Index of the first instruction in the scope of this definition
  internal let scopeStart: Int
  
  /// Set to true if this definition is captured by a closure
  public internal(set) var isCaptured: Bool = false
  
  /// Set if this definition is the name of a named `let` that might be compiled into a loop
  internal var loopCandidate: LoopCandidate? = nil
  
  fileprivate init(index: Int, kind: Kind, scopeStart: Int) {
    self.index = index
    self.kind = kind
    self.scopeStart = scopeStart
  }
  
  fileprivate init(proc: Procedure) {
    self.index = 0
    self.kind = .macro(proc)
    self.scopeStart = 0
  }
  
  fileprivate init(loop: Loop) {
    self.index = 0
    self.kind = .loop(loop)
    self.scopeStart = 0
  }
  
  /// Returns true if this is an immutable variable stored on the stack.
  public var isValue: Bool {
    switch self.kind {
      case .value:
        return true
      default:
        return false
    }
  }
  
  /// Returns true if this variable is stored on the stack (instead of in a box).
  public var isUnboxed: Bool {
    switch self.kind {
      case .value, .unboxedVariable:
        return true
      default:
        return false
    }
  }
  
  public var isVariable: Bool {
    switch self.kind {
      case .variable, .mutatedVariable, .unboxedVariable:
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
      case .mutatedVariable, .unboxedVariable:
        break
      case .loop(_):
        preconditionFailure("cannot declare loop mutable")
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
      case .unboxedVariable:
        return "unboxed variable at index \(self.index)"
      case .loop(let loop):
        return "loop at ip \(loop.entryIp)"
      case .macro(let proc):
        return "macro \(proc)"
    }
  }
}
