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
  
  public init(_ group: BindingGroup?) {
    if group == nil {
      self = Interaction
    } else {
      self = Local(group!)
    }
  }
  
  public var isSystem: Bool {
    if case System = self {
      return true
    }
    return false
  }
  
  public var isInteraction: Bool {
    if case System = self {
      return true
    }
    return false
  }
  
  public var isExpired: Bool {
    if case Expired = self {
      return true
    }
    return false
  }
  
  public var isLocal: Bool {
    if case Local(_) = self {
      return true
    }
    return false
  }
  
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
  
  public var bindingGroup: BindingGroup? {
    guard case .Local(let group) = self else {
      return nil
    }
    return group
  }
  
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
  
  public func rename(sym: Symbol) -> String {
    switch self {
      case Expired:
        return sym.description + "@x"
      case System:
        return sym.description + "@s"
      case Interaction:
        return sym.description + "@i"
      case Local(let group):
        return sym.description + "@" + String(ObjectIdentifier(group).uintValue, radix: 16)
    }
  }
  
  public var description: String {
    switch self {
      case Expired:
        return "expired"
      case System:
        return "system"
      case Interaction:
        return "interaction"
      case Local(let group):
        return "local " + String(group.hashValue, radix: 16)
    }
  }
}

public enum WeakEnv: Hashable {
  case System
  case Interaction
  case Local(WeakBox<BindingGroup>)
  
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
  
  public var hashValue: Int {
    switch self {
      case System:
        return 0
      case Interaction:
        return 1
      case Local(let box):
        return unsafeAddressOf(box).hashValue
    }
  }
}

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
