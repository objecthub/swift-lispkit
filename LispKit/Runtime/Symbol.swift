//
//  Symbol.swift
//  LispKit
//
//  Created by Matthias Zenger on 03/01/2016.
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
/// Class `Symbol` represents symbols. Symbols have reference semantics, i.e. two symbols
/// are considered equal if they have the same identity. LispKit supports interned symbols and
/// generated symbols. Generated symbols are composites, consisting of a lexical symbol and
/// corresponding lexical environment.
///
public final class Symbol: Reference, CustomStringConvertible {
  
  private enum Kind {
    case Interned(String)
    case Generated(Symbol, WeakEnv)
  }
  
  private let kind: Kind
  
  internal init(_ ident: String) {
    self.kind = .Interned(ident)
  }
  
  public init(_ sym: Symbol, _ env: Env) {
    self.kind = .Generated(sym, env.weakEnv)
  }
  
  public var identifier: String {
    return self.interned.description
  }
  
  public var isGenerated: Bool {
    switch self.kind {
      case .Interned(_):
        return false
      case .Generated(_, _):
        return true
    }
  }
    
  public var lexical: (Symbol, Env)? {
    switch self.kind {
      case .Interned(_):
        return nil
      case .Generated(let sym, let weakEnv):
        return (sym, weakEnv.env)
    }
  }
  
  public var interned: Symbol {
    switch self.kind {
      case .Interned(_):
        return self
      case .Generated(let sym, _):
        return sym.interned
    }
  }
  
  public var description: String {
    switch self.kind {
      case .Interned(let ident):
        return ident
      case .Generated(let sym, let weakEnv):
        return "[\(sym.interned.description) \(weakEnv.env.description)]"
    }
  }
}
