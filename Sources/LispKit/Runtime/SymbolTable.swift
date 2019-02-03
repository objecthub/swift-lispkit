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
public final class SymbolTable: Sequence {
  private var symTable = [String : Symbol]()
  private var gensymCounter: UInt64 = 0
  
  public let undef           = Symbol("<undef>")
  public let ellipsis        = Symbol("...")
  public let wildcard        = Symbol("_")
  public let append          = Symbol("append")
  public let cons            = Symbol("cons")
  public let list            = Symbol("list")
  public let quote           = Symbol("quote")
  public let quasiquote      = Symbol("quasiquote")
  public let unquote         = Symbol("unquote")
  public let unquoteSplicing = Symbol("unquote-splicing")
  public let doubleArrow     = Symbol("=>")
  public let `else`          = Symbol("else")
  public let `if`            = Symbol("if")
  public let lambda          = Symbol("lambda")
  public let `let`           = Symbol("let")
  public let letStar         = Symbol("let*")
  public let letrec          = Symbol("letrec")
  public let define          = Symbol("define")
  public let makePromise     = Symbol("make-promise")
  public let makeStream      = Symbol("make-stream")
  public let begin           = Symbol("begin")
  public let `import`        = Symbol("import")
  public let export          = Symbol("export")
  public let exportOpen      = Symbol("export-open")
  public let extern          = Symbol("extern")
  public let rename          = Symbol("rename")
  public let only            = Symbol("only")
  public let except          = Symbol("except")
  public let prefix          = Symbol("prefix")
  public let library         = Symbol("library")
  public let and             = Symbol("and")
  public let or              = Symbol("or")
  public let not             = Symbol("not")
  public let condExpand      = Symbol("cond-expand")
  public let include         = Symbol("include")
  public let includeCi       = Symbol("include-ci")
  public let includeLibDecls = Symbol("include-library-declarations")
  public let scheme          = Symbol("scheme")
  public let r5rs            = Symbol("r5rs")
  public let r5rsSyntax      = Symbol("r5rs-syntax")
  public let starOne         = Symbol("*1")
  public let starTwo         = Symbol("*2")
  public let starThree       = Symbol("*3")
  
  public init() {
    func register(_ sym: Symbol) {
      self.symTable[sym.identifier] = sym
    }
    register(self.undef)
    register(self.ellipsis)
    register(self.wildcard)
    register(self.append)
    register(self.cons)
    register(self.list)
    register(self.quote)
    register(self.quasiquote)
    register(self.unquote)
    register(self.unquoteSplicing)
    register(self.doubleArrow)
    register(self.else)
    register(self.if)
    register(self.lambda)
    register(self.let)
    register(self.letStar)
    register(self.letrec)
    register(self.define)
    register(self.makePromise)
    register(self.makeStream)
    register(self.begin)
    register(self.`import`)
    register(self.export)
    register(self.exportOpen)
    register(self.extern)
    register(self.rename)
    register(self.only)
    register(self.except)
    register(self.prefix)
    register(self.library)
    register(self.and)
    register(self.or)
    register(self.not)
    register(self.condExpand)
    register(self.include)
    register(self.includeCi)
    register(self.includeLibDecls)
    register(self.scheme)
    register(self.r5rs)
    register(self.r5rsSyntax)
    register(self.starOne)
    register(self.starTwo)
    register(self.starThree)
  }
  
  public func exists(_ ident: String) -> Bool {
    return self.symTable[ident] != nil
  }
  
  public func intern(_ ident: String) -> Symbol {
    if let sym = self.symTable[ident] {
      return sym
    } else {
      let sym = Symbol(ident)
      self.symTable[ident] = sym
      return sym
    }
  }
  
  public func gensym(_ basename: String) -> Symbol {
    var ident: String
    repeat {
      ident = basename + String(self.gensymCounter)
      self.gensymCounter += 1
    } while self.exists(ident)
    return self.intern(ident)
  }
  
  public func prefix(_ sym: Symbol, with prefix: Symbol) -> Symbol {
    return self.intern(prefix.identifier + sym.identifier)
  }
  
  /// Returns a generator for iterating over all symbols of this symbol table.
  public func makeIterator() -> AnyIterator<Symbol> {
    var generator = self.symTable.values.makeIterator()
    return AnyIterator { return generator.next() }
  }
}
