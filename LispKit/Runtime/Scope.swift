//
//  Scope.swift
//  LispKit
//
//  Created by Matthias Zenger on 25/01/2016.
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
/// A scope contains bindings mapping symbol ids to expressions. Scopes are nested. Each
/// scope is either a top-level scope or it has an outer/parent scope.
///
public final class Scope: TrackedObject {
  internal let outer: Scope?
  private var bindings: [Symbol : Expr]
  
  public init(_ outer: Scope? = nil) {
    self.outer = outer
    self.bindings = [Symbol : Expr]()
  }
  
  open subscript(sym: Symbol) -> Expr? {
    get {
      return self.bindingFor(sym)?.0
    }
    set {
      self.bindings[sym] = newValue
    }
  }
  
  open func lookupLocal(_ sym: Symbol) -> Expr? {
    return self.bindings[sym]
  }
  
  open func scopeWithBindingFor(_ sym: Symbol) -> Scope? {
    return self.bindingFor(sym)?.1
  }
  
  open func bindingFor(_ sym: Symbol) -> (Expr, Scope)? {
    var scope: Scope? = self
    while let sc = scope {
      if let res = sc.bindings[sym] {
        return (res, sc)
      }
      scope = sc.outer
    }
    return nil
  }
  
  open func forAll(inclOuter all: Bool = false, proc: (Symbol, Expr) -> Void) {
    var scope: Scope? = self
    while let sc = scope {
      for (sym, expr) in sc.bindings {
        proc(sym, expr)
      }
      scope = all ? sc.outer : nil
    }
  }
  
  open override func mark(_ tag: UInt8) {
    for (_, expr) in self.bindings {
      expr.mark(tag)
    }
    self.outer?.mark(tag)
  }
}
