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

import Foundation

///
/// Class `Symbol` represents symbols. Symbols have reference semantics, i.e. two symbols
/// are considered equal if they have the same identity. LispKit supports interned symbols and
/// generated symbols. Generated symbols are composites, consisting of a lexical symbol and
/// corresponding lexical environment.
///
public final class Symbol: Reference, CustomStringConvertible {
  
  private enum Kind {
    case interned(String)
    case uninterned(String)
    case generated(Symbol, WeakEnv)
  }
  
  private let kind: Kind

  internal init(_ ident: String) {
    self.kind = .interned(ident)
  }
  
  public init(uninterned ident: String) {
    self.kind = .uninterned(ident)
  }
  
  public init(_ sym: Symbol, _ env: Env) {
    self.kind = .generated(sym, env.weakEnv)
  }
  
  public var identifier: String {
    return self.root.rawIdentifier
  }
  
  public var rawIdentifier: String {
    switch self.root.kind {
      case .interned(let ident):
        return ident
      case .uninterned(let ident):
        return ident
      default:
        preconditionFailure("no interned or uninterned symbol")
    }
  }

  public var isInterned: Bool {
    switch self.kind {
      case .interned(_):
        return true
      default:
        return false
    }
  }
  
  public var isGenerated: Bool {
    switch self.kind {
      case .interned(_), .uninterned(_):
        return false
      case .generated(_, _):
        return true
    }
  }

  public var lexical: (Symbol, Env)? {
    switch self.kind {
      case .interned(_), .uninterned(_):
        return nil
      case .generated(let sym, let weakEnv):
        return (sym, weakEnv.env)
    }
  }
  
  public var root: Symbol {
    switch self.kind {
      case .interned(_), .uninterned(_):
        return self
      case .generated(let sym, _):
        return sym.root
    }
  }
  
  private static let escapeChars = { () -> CharacterSet in
    let mcs = NSMutableCharacterSet()
    mcs.formUnion(with: CharacterSet.whitespacesAndNewlines)
    mcs.formUnion(with: CharacterSet.controlCharacters)
    mcs.addCharacters(in: ",\"\'")
    return mcs.copy() as! CharacterSet
  }()
  
  internal static func escapingNeeded(_ ident: String) -> Bool {
    if ident.rangeOfCharacter(from: Symbol.escapeChars) != nil {
      return true
    }
    let lowident = ident.lowercased()
    var index = lowident.startIndex
    func next() -> Character? {
      guard index < lowident.endIndex else {
        return nil
      }
      let res = lowident[index]
      index = lowident.index(after: index)
      return res
    }
    guard let fst = next() else {
      return false
    }
    switch fst {
      case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9":
        return true
      case ".":
        guard let snd = next() else {
          return true
        }
        switch snd {
          case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9":
            return true
          default:
            return false
        }
      case "+", "-":
        guard let snd = next() else {
          return false
        }
        switch snd {
          case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9":
            return true
          case ".":
            return true
          case "n":
            return next() == "a" && next() == "n"
          case "i":
            guard let trd = next() else {
              return true
            }
            return trd == "n" && next() == "f"
          default:
            return false
        }
      default:
        return false
    }
  }
  
  public var description: String {
    switch self.kind {
      case .interned(let ident):
        if Symbol.escapingNeeded(ident) {
          return "|\(Expr.escapeStr(ident))|"
        } else {
          return ident
        }
      case .uninterned(let ident):
        if Symbol.escapingNeeded(ident) {
          return "|\(Expr.escapeStr(ident))|"
        } else {
          return ident
        }
      case .generated(let sym, let weakEnv):
        return "[\(sym.root.description) \(sym.identity) \(weakEnv.env.description)]"
    }
  }
}
