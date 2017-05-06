//
//  ImportSet.swift
//  LispKit
//
//  Created by Matthias Zenger on 26/09/2016.
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
/// An import set specifies what bindings are being imported into an environment and whether
/// they need to be renamed. An import set specifies the imports implicitly. It can be expanded
/// in a context into an explicit list of potentially renamed symbol references for a library.
///
public indirect enum ImportSet: Equatable, CustomStringConvertible {
  case library(Expr)
  case only([Symbol], ImportSet)
  case except(Set<Symbol>, ImportSet)
  case prefix(Symbol, ImportSet)
  case rename([Symbol : Symbol], ImportSet)
  
  /// Constructs an import set from an expression for a given context.
  public init?(_ importSet: Expr, in context: Context) {
    switch importSet {
      case .pair(.symbol(context.symbols.only), .pair(let baseSet, let idents)):
        var identList = idents
        var inclSym = [Symbol]()
        while case .pair(.symbol(let sym), let next) = identList {
          inclSym.append(sym)
          identList = next
        }
        guard identList.isNull else {
          return nil
        }
        if let importSet = ImportSet(baseSet, in: context) {
          self = .only(inclSym, importSet)
          return
        }
      case .pair(.symbol(context.symbols.except), .pair(let baseSet, let idents)):
        var identList = idents
        var exclSym = Set<Symbol>()
        while case .pair(.symbol(let sym), let next) = identList {
          exclSym.insert(sym)
          identList = next
        }
        guard identList.isNull else {
          return nil
        }
        if let root = ImportSet(baseSet, in: context) {
          self = .except(exclSym, root)
          return
        }
      case .pair(.symbol(context.symbols.prefix),
                 .pair(let baseSet, .pair(.symbol(let ident), .null))):
        if let root = ImportSet(baseSet, in: context) {
          self = .prefix(ident, root)
          return
        }
      case .pair(.symbol(context.symbols.rename), .pair(let baseSet, let idents)):
        var renameList = idents
        var renamings = [Symbol : Symbol]()
        while case .pair(let renaming, let next) = renameList {
          switch renaming {
            case .symbol(let sym):
              renamings[sym] = sym
            case .pair(.symbol(let from), .pair(.symbol(let to), .null)):
              renamings[to] = from
            default:
              return nil
          }
          renameList = next
        }
        guard renameList.isNull else {
          return nil
        }
        if let root = ImportSet(baseSet, in: context) {
          self = .rename(renamings, root)
          return
        }
      case .pair(_, _):
        var libraryName = importSet
        while case .pair(let component, let next) = libraryName {
          switch component {
            case .symbol(_), .fixnum(_), .flonum(_):
              break
            default:
              return nil
          }
          libraryName = next
        }
        guard libraryName.isNull else {
          return nil
        }
        self = .library(importSet)
        return
      default:
        break
    }
    return nil
  }
  
  /// `expand` returns for the import set a reference to the library from which definitions
  /// are imported. In addition, a mapping is returned that maps renamed definitions to the
  /// definitions as exported by the library.
  public func expand(in context: Context) -> (Library, [Symbol : Symbol])? {
    switch self {
      case .library(let name):
        guard let library = context.libraries.lookup(name) else {
          return nil
        }
        _ = library.allocate()
        var imports = [Symbol : Symbol]()
        for export in library.exported {
          imports[export] = export
        }
        return (library, imports)
      case .only(let restricted, let importSet):
        guard let (library, currentImports) = importSet.expand(in: context) else {
          return nil
        }
        var imports = [Symbol : Symbol]()
        for restrict in restricted {
          guard let export = currentImports[restrict] else {
            return nil
          }
          imports[restrict] = export
        }
        return (library, imports)
      case .except(let excluded, let importSet):
        guard let (library, currentImports) = importSet.expand(in: context) else {
          return nil
        }
        var imports = [Symbol : Symbol]()
        for currentImport in currentImports.keys {
          if !excluded.contains(currentImport) {
            imports[currentImport] = currentImports[currentImport]
          }
        }
        return (library, imports)
      case .prefix(let prefix, let importSet):
        guard let (library, currentImports) = importSet.expand(in: context) else {
          return nil
        }
        var imports = [Symbol : Symbol]()
        for currentImport in currentImports.keys {
          imports[context.symbols.prefix(currentImport, with: prefix)] =
            currentImports[currentImport]
        }
        return (library, imports)
      case .rename(let renamings, let importSet):
        guard let (library, currentImports) = importSet.expand(in: context) else {
          return nil
        }
        var imports = [Symbol : Symbol]()
        for currentImport in currentImports.keys {
          imports[renamings[currentImport] ?? currentImport] = currentImports[currentImport]
        }
        return (library, imports)
    }
  }
  
  public var description: String {
    switch self {
      case .library(let expr):
        return expr.description
      case .only(let symbols, let importSet):
        return "(only \(symbols) from \(importSet))"
      case .except(let symbols, let importSet):
        return "(except \(symbols) from \(importSet))"
      case .prefix(let sym, let importSet):
        return "(prefix \(sym) for \(importSet))"
      case .rename(let map, let importSet):
        return "(rename \(map) from \(importSet))"
    }
  }
}

public func ==(_ left: ImportSet, _ right: ImportSet) -> Bool {
  switch (left, right) {
    case (.library(let e1), .library(let e2)):
      return e1 == e2
    case (.only(let s1, let is1), .only(let s2, let is2)):
      return s1 == s2 && is1 == is2
    case (.except(let s1, let is1), .except(let s2, let is2)):
      return s1 == s2 && is1 == is2
    case (.prefix(let s1, let is1), .prefix(let s2, let is2)):
      return s1 == s2 && is1 == is2
    case (.rename(let m1, let is1), .rename(let m2, let is2)):
      return m1 == m2 && is1 == is2
    default:
      return false
  }
}

