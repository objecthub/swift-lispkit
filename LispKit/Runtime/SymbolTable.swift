//
//  SymbolTable.swift
//  LispKit
//
//  Created by Matthias Zenger on 23/01/2016.
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
/// Class `Symbols` represents a symbol table. It is used for managing symbols, symbol tags,
/// and their textual representations. Instances of `Symbols` provide functionality for
/// creating interned symbols for a given identifier and for looking up the identifier
/// of a given symbol tag.
///
open class SymbolTable: Sequence {
  fileprivate var symTable = [String : Symbol]()
  fileprivate var gensymCounter: UInt64 = 0
  
  open let UNDEF           = Symbol("<undef>")
  open let ELLIPSIS        = Symbol("...")
  open let WILDCARD        = Symbol("_")
  open let APPEND          = Symbol("append")
  open let CONS            = Symbol("cons")
  open let LIST            = Symbol("list")
  open let QUOTE           = Symbol("quote")
  open let QUASIQUOTE      = Symbol("quasiquote")
  open let UNQUOTE         = Symbol("unquote")
  open let UNQUOTESPLICING = Symbol("unquote-splicing")
  open let DOUBLEARROW     = Symbol("=>")
  open let ELSE            = Symbol("else")
  open let IF              = Symbol("if")
  open let LAMBDA          = Symbol("lambda")
  open let LET             = Symbol("let")
  open let LETSTAR         = Symbol("let*")
  open let LETREC          = Symbol("letrec")
  open let DEFINE          = Symbol("define")
  open let MAKEPROMISE     = Symbol("make-promise")
  
  public init() {
    func register(_ sym: Symbol) {
      self.symTable[sym.description] = sym
    }
    register(self.UNDEF)
    register(self.ELLIPSIS)
    register(self.WILDCARD)
    register(self.APPEND)
    register(self.CONS)
    register(self.LIST)
    register(self.QUOTE)
    register(self.QUASIQUOTE)
    register(self.UNQUOTE)
    register(self.UNQUOTESPLICING)
    register(self.DOUBLEARROW)
    register(self.ELSE)
    register(self.IF)
    register(self.LAMBDA)
    register(self.LET)
    register(self.LETSTAR)
    register(self.LETREC)
    register(self.DEFINE)
    register(self.MAKEPROMISE)
  }
  
  open func exists(_ ident: String) -> Bool {
    return self.symTable[ident] != nil
  }
  
  open func intern(_ ident: String) -> Symbol {
    if let sym = self.symTable[ident] {
      return sym
    } else {
      let sym = Symbol(ident)
      self.symTable[ident] = sym
      return sym
    }
  }
  
  open func gensym(_ basename: String) -> Symbol {
    var ident: String
    repeat {
      ident = basename + String(self.gensymCounter)
      self.gensymCounter += 1
    } while self.exists(ident)
    return self.intern(ident)
  }
  
  /// Returns a generator for iterating over all symbols of this symbol table.
  open func makeIterator() -> AnyIterator<Symbol> {
    var generator = self.symTable.values.makeIterator()
    return AnyIterator { return generator.next() }
  }
}
