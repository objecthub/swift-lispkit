//
//  Env.swift
//  LispKit
//
//  Created by Matthias Zenger on 08/11/2015.
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
/// Enumeration `Env` represents a lexical environment. Environments are either of type
///    - `Expired`,
///    - `System`,
///    - `Interaction`, or
///    - `Local`,
/// where `Local` represents bindings that are not at the bottom/interaction level.
///
public enum Env: CustomStringConvertible {
  case Expired
  case System
  case Interaction
  case Local(BindingGroup)
  
  /// Initializes a local environment
  public init(_ group: BindingGroup?) {
    if group == nil {
      self = Interaction
    } else {
      self = Local(group!)
    }
  }
  
  /// Is this an environment for the system scope?
  public var isSystem: Bool {
    if case System = self {
      return true
    }
    return false
  }
  
  /// Is this an environment for the interaction scope?
  public var isInteraction: Bool {
    if case Interaction = self {
      return true
    }
    return false
  }
  
  /// Is this an expired environment? Expired environments play a role for environments
  /// derived from weak environments (which are needed for making the macro mechanism work).
  public var isExpired: Bool {
    if case Expired = self {
      return true
    }
    return false
  }
  
  /// Is this a local environment?
  public var isLocal: Bool {
    if case Local(_) = self {
      return true
    }
    return false
  }
  
  /// Is this symbol defined in the system scope?
  public func systemDefined(sym: Symbol, in context: Context) -> Bool {
    var env = self
    while case .Local(let group) = env {
      if group.bindingFor(sym) != nil {
        return false
      }
      env = group.parent
    }
    if let (lexicalSym, lexicalEnv) = sym.lexical {
      return lexicalEnv.systemDefined(lexicalSym, in: context)
    }
    return env.scope(context).scopeWithBindingFor(sym) === context.systemScope
  }
  
  /// Is this environment in scope of environment `env`? An environment is in scope of
  /// another environment is an "outer" environment for the other environment. As a consequence,
  /// the system environment is in scope of all other environments.
  public func inScopeOf(env: Env) -> Bool {
    switch (self, env) {
      case (System, _):
        return true
      case (Interaction, Interaction), (Interaction, Expired), (Interaction, Local(_)):
        return true
      case (Local(let g1), Local(let g2)):
        return g2.covers(g1)
      default:
        return false
    }
  }
  
  /// Returns the binding group of a local environment.
  public var bindingGroup: BindingGroup? {
    guard case .Local(let group) = self else {
      return nil
    }
    return group
  }
  
  /// Returns the scope for global environments.
  public func scope(context: Context) -> Scope {
    switch self {
      case Expired, Local(_):
        preconditionFailure()
      case System:
        return context.systemScope
      case Interaction:
        return context.userScope
    }
  }
  
  /// Returns a weak environment that matches this environment. Only weak environments can be
  /// used for persisting an environment, e.g. in an expression or a generated symbol.
  /// Converting weak environments back into regular environments is possible, but is subject
  /// to expirations if the binding group of a local environment gets out of scope (i.e.
  /// compilation has passed on).
  public var weakEnv: WeakEnv {
    switch self {
      case Expired:
        return .Local(WeakBox(nil))
      case System:
        return .System
      case Interaction:
        return .Interaction
      case Local(let group):
        return .Local(group.box)
    }
  }
  
  /// Creates a "meta environment" from a compilation environment for executing syntax
  /// transformers.
  public var syntacticalEnv: Env {
    switch self {
      case Expired:
        return .Expired
      case System:
        return .System
      case Interaction:
        return .Interaction
      case Local(let group):
        return .Local(group.macroGroup())
    }
  }
  
  /// Returns a unique identifier for this symbol in the given environment. This can,
  /// for instance, be used to print symbols such that generated and interned symbols
  /// can be distinguished.
  public func rename(sym: Symbol) -> String {
    switch self {
      case Expired:
        return sym.identifier + "@x"
      case System:
        return sym.identifier + "@s"
      case Interaction:
        return sym.identifier + "@i"
      case Local(let group):
        return sym.identifier + "@" + String(group.identity, radix: 16)
    }
  }
  
  /// Returns a string representation of this environment.
  public var description: String {
    switch self {
      case Expired:
        return "expired"
      case System:
        return "system"
      case Interaction:
        return "interaction"
      case Local(let group):
        return "local " + String(group.identity, radix: 16)
    }
  }
}

/// Weak environments are used to persist environments during the macro expansion process,
/// for instance in generated symbols. They can be mapped back to regular environments via the
/// `env` method. If this happens outside of the compilation context, the original environment
/// has expired and `env` returns an expired environment.
public enum WeakEnv: Hashable {
  case System
  case Interaction
  case Local(WeakBox<BindingGroup>)
  
  /// Returns a regular environment for this weak environment.
  public var env: Env {
    switch self {
      case System:
        return .System
      case Interaction:
        return .Interaction
      case Local(let box):
        return box.value == nil ? .Expired : .Local(box.value!)
    }
  }
  
  /// Hash value of this weak environment
  public var hashValue: Int {
    switch self {
      case System:
        return 0
      case Interaction:
        return 1
      case Local(let box):
        return ObjectIdentifier(box).hashValue
    }
  }
}

/// Compares two weak environments. Two weak environments are the same if they have the same
/// type (i.e. same enumeration case) and for local environments, the bindings groups are
/// identical (same object).
public func ==(lhs: WeakEnv, rhs: WeakEnv) -> Bool {
  switch (lhs, rhs) {
    case (.System, .System):
      return true
    case (.Interaction, .Interaction):
      return true
    case (.Local(let lbox), .Local(let rbox)):
      return lbox === rbox
    default:
      return false
  }
}
