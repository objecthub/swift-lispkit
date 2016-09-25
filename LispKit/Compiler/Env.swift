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
  case expired
  case system
  case interaction
  case local(BindingGroup)
  
  /// Initializes a local environment
  public init(_ group: BindingGroup?) {
    if group == nil {
      self = .interaction
    } else {
      self = .local(group!)
    }
  }
  
  /// Is this an environment for the system scope?
  public var isSystem: Bool {
    if case .system = self {
      return true
    }
    return false
  }
  
  /// Is this an environment for the interaction scope?
  public var isInteraction: Bool {
    if case .interaction = self {
      return true
    }
    return false
  }
  
  /// Is this an expired environment? Expired environments play a role for environments
  /// derived from weak environments (which are needed for making the macro mechanism work).
  public var isExpired: Bool {
    if case .expired = self {
      return true
    }
    return false
  }
  
  /// Is this a local environment?
  public var isLocal: Bool {
    if case .local(_) = self {
      return true
    }
    return false
  }
  
  /// Is this symbol defined in the system scope?
  public func systemDefined(_ sym: Symbol, in context: Context) -> Bool {
    var env = self
    while case .local(let group) = env {
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
  public func inScopeOf(_ env: Env) -> Bool {
    switch (self, env) {
      case (.system, _):
        return true
      case (.interaction, .interaction), (.interaction, .expired), (.interaction, .local(_)):
        return true
      case (.local(let g1), .local(let g2)):
        return g2.covers(g1)
      default:
        return false
    }
  }
  
  /// Returns the binding group of a local environment.
  public var bindingGroup: BindingGroup? {
    guard case .local(let group) = self else {
      return nil
    }
    return group
  }
  
  /// Returns the scope for global environments.
  public func scope(_ context: Context) -> Scope {
    switch self {
      case .expired, .local(_):
        preconditionFailure()
      case .system:
        return context.systemScope
      case .interaction:
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
      case .expired:
        return .local(WeakBox(nil))
      case .system:
        return .system
      case .interaction:
        return .interaction
      case .local(let group):
        return .local(group.box)
    }
  }
  
  /// Creates a "meta environment" from a compilation environment for executing syntax
  /// transformers.
  public var syntacticalEnv: Env {
    switch self {
      case .expired:
        return .expired
      case .system:
        return .system
      case .interaction:
        return .interaction
      case .local(let group):
        return .local(group.macroGroup())
    }
  }
  
  /// Returns a unique identifier for this symbol in the given environment. This can,
  /// for instance, be used to print symbols such that generated and interned symbols
  /// can be distinguished.
  public func rename(_ sym: Symbol) -> String {
    switch self {
      case .expired:
        return sym.identifier + "@x"
      case .system:
        return sym.identifier + "@s"
      case .interaction:
        return sym.identifier + "@i"
      case .local(let group):
        return sym.identifier + "@" + group.identityString
    }
  }
  
  /// Returns a string representation of this environment.
  public var description: String {
    switch self {
      case .expired:
        return "expired"
      case .system:
        return "system"
      case .interaction:
        return "interaction"
      case .local(let group):
        return "local " + group.identityString
    }
  }
}

/// Weak environments are used to persist environments during the macro expansion process,
/// for instance in generated symbols. They can be mapped back to regular environments via the
/// `env` method. If this happens outside of the compilation context, the original environment
/// has expired and `env` returns an expired environment.
public enum WeakEnv: Hashable {
  case system
  case interaction
  case local(WeakBox<BindingGroup>)
  
  /// Returns a regular environment for this weak environment.
  public var env: Env {
    switch self {
      case .system:
        return .system
      case .interaction:
        return .interaction
      case .local(let box):
        return box.value == nil ? .expired : .local(box.value!)
    }
  }
  
  /// Hash value of this weak environment
  public var hashValue: Int {
    switch self {
      case .system:
        return 0
      case .interaction:
        return 1
      case .local(let box):
        return ObjectIdentifier(box).hashValue
    }
  }
}

/// Compares two weak environments. Two weak environments are the same if they have the same
/// type (i.e. same enumeration case) and for local environments, the bindings groups are
/// identical (same object).
public func ==(lhs: WeakEnv, rhs: WeakEnv) -> Bool {
  switch (lhs, rhs) {
    case (.system, .system):
      return true
    case (.interaction, .interaction):
      return true
    case (.local(let lbox), .local(let rbox)):
      return lbox === rbox
    default:
      return false
  }
}
