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
public indirect enum ImportSet {
  case library(Expr)
  case only(ImportSet, [Symbol])
  case except(ImportSet, Set<Symbol>)
  case prefix(ImportSet, Symbol)
  case rename(ImportSet, [Symbol : Symbol])
  
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
          self = .only(importSet, inclSym)
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
          self = .except(root, exclSym)
          return
        }
      case .pair(.symbol(context.symbols.prefix),
                 .pair(let baseSet, .pair(.symbol(let ident), .null))):
        if let root = ImportSet(baseSet, in: context) {
          self = .prefix(root, ident)
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
          self = .rename(root, renamings)
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
        guard let library = context.libraries[name] else {
          return nil
        }
        var imports = [Symbol : Symbol]()
        for export in library.exported {
          imports[export] = export
        }
        return (library, imports)
      case .only(let importSet, let restricted):
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
      case .except(let importSet, let excluded):
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
      case .prefix(let importSet, let prefix):
        guard let (library, currentImports) = importSet.expand(in: context) else {
          return nil
        }
        var imports = [Symbol : Symbol]()
        for currentImport in currentImports.keys {
          imports[context.symbols.prefix(currentImport, with: prefix)] =
            currentImports[currentImport]
        }
        return (library, imports)
      case .rename(let importSet, let renamings):
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
}
