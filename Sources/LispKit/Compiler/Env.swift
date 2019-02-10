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
///    - `Global`, or
///    - `Local`,
/// where `Local` represents bindings that are context specific. The context is described
/// in form of a `BindingGroup` object.
///
public enum Env: CustomStringConvertible, Equatable {
  case expired
  case global(Environment)
  case local(BindingGroup)

  /// Initializes a global environment
  public init(_ environment: Environment) {
    self = .global(environment)
  }

  /// Initializes a local environment
  public init(_ group: BindingGroup) {
    self = .local(group)
  }

  /// Is this an expired environment? Expired environments play a role for environments
  /// derived from weak environments (which are needed for making the macro mechanism work).
  public var isExpired: Bool {
    if case .expired = self {
      return true
    }
    return false
  }

  /// Is this an environment for the system scope?
  public var isGlobal: Bool {
    if case .global(_) = self {
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

  /// Is this symbol bound in an immutable fashion?
  public func isImmutable(_ sym: Symbol) -> Bool {
    var env = self
    while case .local(let group) = env {
      if group.bindingFor(sym) != nil {
        return false
      }
      env = group.parent
    }
    if let (lexicalSym, lexicalEnv) = sym.lexical {
      return lexicalEnv.isImmutable(lexicalSym)
    }
    guard case .global(let environment) = env else {
      return false
    }
    return environment.locationRef(for: sym).isImmutable
  }

  /// Is this environment in scope of environment `env`? An environment is in scope of
  /// another environment is an "outer" environment for the other environment. As a consequence,
  /// the global environment is in scope of all other environments.
  public func inScopeOf(_ env: Env) -> Bool {
    switch (self, env) {
      case (.global(let e), _):
        return e == env.environment
      case (.local(let g1), .local(let g2)):
        return g2.covers(g1)
      default:
        return false
    }
  }

  /// Returns the global environment
  public var environment: Environment? {
    switch self {
      case .expired:
        return nil
      case .global(let e):
        return e
      case .local(let group):
        return group.parent.environment
    }
  }

  /// Returns the global env
  public var global: Env {
    switch self {
      case .local(let group):
        return group.parent.global
      default:
        return self
    }
  }

  /// Returns the binding group of a local environment.
  public var bindingGroup: BindingGroup? {
    guard case .local(let group) = self else {
      return nil
    }
    return group
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
      case .global(let environment):
        return .global(environment.box)
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
      case .global(let environment):
        return .global(environment)
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
      case .global(let environment):
        return sym.identifier + "@g" + environment.identityString
      case .local(let group):
        return sym.identifier + "@l" + group.identityString
    }
  }

  /// Returns a string representation of this environment.
  public var description: String {
    switch self {
      case .expired:
        return "expired"
      case .global(let environment):
        return "global " + environment.identityString
      case .local(let group):
        return "local " + group.identityString
    }
  }


  /// Compares two env objects. Two weak environments are the same if they have the same
  /// type (i.e. same enumeration case) and for local environments, the bindings groups are
  /// identical (same object).
  public static func ==(lhs: Env, rhs: Env) -> Bool {
    switch (lhs, rhs) {
      case (.expired, .expired):
        return true
      case (.global(let env1), .global(let env2)):
        return env1 === env2
      case (.local(let group1), .local(let group2)):
        return group1 === group2
      default:
        return false
    }
  }
}

/// Weak environments are used to persist environments during the macro expansion process,
/// for instance in generated symbols. They can be mapped back to regular environments via the
/// `env` method. If this happens outside of the compilation context, the original environment
/// has expired and `env` returns an expired environment.
public enum WeakEnv: Hashable {
  case global(WeakBox<Environment>)
  case local(WeakBox<BindingGroup>)

  /// Returns a regular environment for this weak environment.
  public var env: Env {
    switch self {
      case .global(let box):
        return box.value == nil ? .expired : .global(box.value!)
      case .local(let box):
        return box.value == nil ? .expired : .local(box.value!)
    }
  }

  /// Hash value of this weak environment
  public func hash(into hasher: inout Hasher) {
    switch self {
      case .global(let box):
        return hasher.combine(ObjectIdentifier(box))
      case .local(let box):
        return hasher.combine(ObjectIdentifier(box))
    }
  }

  /// Compares two weak environments. Two weak environments are the same if they have the same
  /// type (i.e. same enumeration case) and for local environments, the bindings groups are
  /// identical (same object).
  public static func ==(lhs: WeakEnv, rhs: WeakEnv) -> Bool {
    switch (lhs, rhs) {
      case (.global(let lbox), .global(let rbox)):
        return lbox === rbox
      case (.local(let lbox), .local(let rbox)):
        return lbox === rbox
      default:
        return false
    }
  }
}
