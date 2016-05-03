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
public class SymbolTable: SequenceType {
  private var symTable = [String : Symbol]()
  private var gensymCounter: UInt64 = 0
  
  public let UNDEF           = Symbol("<undef>")
  public let ELLIPSIS        = Symbol("...")
  public let WILDCARD        = Symbol("_")
  public let APPEND          = Symbol("append")
  public let CONS            = Symbol("cons")
  public let LIST            = Symbol("list")
  public let QUOTE           = Symbol("quote")
  public let QUASIQUOTE      = Symbol("quasiquote")
  public let UNQUOTE         = Symbol("unquote")
  public let UNQUOTESPLICING = Symbol("unquote-splicing")
  public let DOUBLEARROW     = Symbol("=>")
  public let ELSE            = Symbol("else")
  public let IF              = Symbol("if")
  public let LAMBDA          = Symbol("lambda")
  public let LET             = Symbol("let")
  public let LETSTAR         = Symbol("let*")
  public let LETREC          = Symbol("letrec")
  
  public init() {
    func register(sym: Symbol) {
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
  }
  
  public func exists(ident: String) -> Bool {
    return self.symTable[ident] != nil
  }
  
  public func intern(ident: String) -> Symbol {
    if let sym = self.symTable[ident] {
      return sym
    } else {
      let sym = Symbol(ident)
      self.symTable[ident] = sym
      return sym
    }
  }
  
  public func gensym(basename: String) -> Symbol {
    var ident: String
    repeat {
      ident = basename + String(self.gensymCounter)
      self.gensymCounter += 1
    } while self.exists(ident)
    return self.intern(ident)
  }
  
  /// Returns a generator for iterating over all symbols of this symbol table.
  public func generate() -> AnyGenerator<Symbol> {
    var generator = self.symTable.values.generate()
    return AnyGenerator { return generator.next() }
  }
}
