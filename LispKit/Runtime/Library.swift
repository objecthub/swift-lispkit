//
//  Library.swift
//  LispKit
//
//  Created by Matthias Zenger on 11/09/2016.
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
/// `Library` represents a library specification, listing exported identifiers, imported
/// identifiers, as well as the definition of the library in terms of import, export,
/// and initialization declarations.
/// 
public final class Library: Reference, Trackable, CustomStringConvertible {
  
  public enum State {
    case loaded
    case allocated
    case wired
    case initialized
    
    var isAllocated: Bool {
      switch self {
        case .allocated, .wired, .initialized:
          return true
        case .loaded:
          return false
      }
    }
  }
  
  public enum InternalLocationRef {
    case mutable(Int)
    case immutable(Int)
    
    var isMutable: Bool {
      switch self {
        case .mutable(_):
          return true
        default:
          return false
        }
    }
    
    var location: Int {
      switch self {
        case .mutable(let loc):
          return loc
        case .immutable(let loc):
          return loc
      }
    }
    
    func unify(with that: InternalLocationRef?) -> InternalLocationRef? {
      guard let other = that else {
        return self
      }
      switch (self, other) {
        case (.mutable(let loc1), .mutable(let loc2)):
          guard loc1 == loc2 else {
            return nil
          }
          return .mutable(loc1)
        case (.mutable(let loc1), .immutable(let loc2)):
          guard loc1 == loc2 else {
            return nil
          }
          return .mutable(loc1)
        case (.immutable(let loc1), .mutable(let loc2)):
          guard loc1 == loc2 else {
            return nil
          }
          return .mutable(loc1)
        case (.immutable(let loc1), .immutable(let loc2)):
          guard loc1 == loc2 else {
            return nil
          }
          return .immutable(loc1)
      }
    }
  }
  
  public enum InternalIdent: Hashable {
    case mutable(Symbol)
    case immutable(Symbol)
    
    var isMutable: Bool {
      switch self {
        case .mutable(_):
          return true
        default:
          return false
      }
    }
    
    var identifier: Symbol {
      switch self {
        case .mutable(let sym):
          return sym
        case .immutable(let sym):
          return sym
      }
    }
    
    func located(at location: Int) -> InternalLocationRef {
      switch self {
        case .mutable(_):
          return .mutable(location)
        case .immutable(_):
          return .immutable(location)
      }
    }
    
    public var hashValue: Int {
      return self.identifier.hashValue
    }
  }
  
  /// The name of a library is a list of symbols
  public let name: Expr
  
  /// Maps exported identifiers to location references (immutable/mutable)
  public private(set) var exports: [Symbol : InternalLocationRef]

  /// Maps imported internal identifiers to location references (immutable/mutable)
  public private(set) var imports: [Symbol : InternalLocationRef]
  
  /// Maps imported internal identifiers to a set of (library, identifier) pairs
  internal var imported: MultiMap<Symbol, (Library, Symbol)>
  
  /// Libraries on which this library is dependent on
  private var libraries: Set<Library>
  
  /// Parsed export declarations, mapping exported identifiers to internal identifiers
  /// (with mutable/immutable annotations)
  internal var exportDecls: [Symbol : InternalIdent]
  
  /// Parsed import declarations
  private var importDecls: [ImportSet]
  
  /// Parsed initialization declarations
  private var initDecls: Exprs
  
  /// State of the library
  private var state: State
  
  /// Initialize a new library based on its definition
  public init(name: Expr, declarations: Expr, in context: Context) throws {
    self.name = name
    self.exports = [:]
    self.imports = [:]
    self.imported = MultiMap()
    self.libraries = Set<Library>()
    self.exportDecls = [:]
    self.importDecls = []
    self.initDecls = []
    self.state = .loaded
    super.init()
    try self.parseLibraryDefinition(declarations, in: context)
  }
  
  /// Initialize a new native library
  public convenience init(name: Expr,
                          exports: [InternalIdent : Expr],
                          imports: [ImportSet],
                          initDecls: Exprs,
                          in context: Context) throws {
    try self.init(name: name, declarations: .null, in: context)
    for (ident, decl) in exports {
      self.exports[ident.identifier] = ident.located(at: context.allocateLocation(for: decl))
      self.exportDecls[ident.identifier] = ident
    }
    self.importDecls = imports
    self.initDecls = initDecls
  }
  
  public var exported: [Symbol] {
    return [Symbol](self.exportDecls.keys)
  }
  
  public func exportLocation(_ ident: Symbol) -> InternalLocationRef? {
    assert(self.state.isAllocated, "calling exportLocation on not allocated library")
    // If an export location is known already, return it
    if let locationRef = self.exports[ident] {
      return locationRef
    }
    // Check if this identifier does get exported and determine internal identifier
    guard let internalIdent = self.exportDecls[ident] else {
      // ERRROR: Identifier not exported by library
      return nil
    }
    // Compute the location of the internal identifier
    let locationRef = self.importLocation(internalIdent.identifier)
    // If imported identifier is mutable, let the export decide whether it's a mutable export
    if locationRef.isMutable {
      self.exports[ident] = internalIdent.located(at: locationRef.location)
    // The imported identifier is immutable, make the export immutable as well
    } else {
      self.exports[ident] = .immutable(locationRef.location)
    }
    // Return the now guaranteed export location
    return self.exports[ident]!
  }
  
  public func importLocation(_ ident: Symbol) -> InternalLocationRef {
    // Return location if the internal identifier has a known location already
    if let locationRef = self.imports[ident] {
      return locationRef
    }
    // Determine all imports for the internal identifier
    let libraryReferences = self.imported.values(for: ident)
    for (library, expIdent) in libraryReferences {
      // Get a location from the library
      if let locationRef = library.exportLocation(expIdent) {
        // Unify the new location with the existing location
        let currentLocationRef = self.imports[ident]
        if let unifiedLocationRef = locationRef.unify(with: currentLocationRef) {
          self.imports[ident] = unifiedLocationRef
        } else {
          // ERROR: inconsistent import of `import` involving `library` and one of the
          // previous library values (just pick the first from libraryReferences?) since
          // a unification wasn't possible
        }
      } else {
        // ERROR: signal cyclic dependency since the library isn't able to return a location
      }
    }
    // Return the now guaranteed import location
    return self.imports[ident]!
  }
  
  public func allocate(in context: Context) {
    guard case .loaded = self.state else {
      return
    }
    // Mark the state of the library as allocated
    self.state = .allocated
    // Collect imported identifiers and determine where they are imported from
    for importDecl in self.importDecls {
      if let (library, importSpec) = importDecl.expand(in: context) {
        libraries.insert(library)
        for (impIdent, expIdent) in importSpec {
          self.imported.insert(impIdent, mapsTo: (library, expIdent))
        }
      }
    }
    // Allocate locations for the exported identifiers which are not imported
    for (extIdent, intIdent) in self.exportDecls {
      if !self.imported.hasValues(for: intIdent.identifier) {
        self.exports[extIdent] = intIdent.located(at: context.allocateLocation())
      }
    }
    // Allocate all libraries from which identifiers are imported
    for library in self.libraries {
      library.allocate(in: context)
    }
  }
  
  public func wire(in context: Context) {
    self.allocate(in: context)
    guard case .wired = self.state else {
      return
    }
    // Mark the state of the library as wired
    self.state = .wired
    // Resolve dependencies for the imported identifiers
    for importedIdent in self.imported.keys {
      _ = self.importLocation(importedIdent)
    }
    // Backfill dependencies for the exported identifiers which are imported
    for exportedIdent in self.exportDecls.keys {
      _ = self.exportLocation(exportedIdent)
    }
  }
  
  public func initialize(in context: Context) {
    self.wire(in: context)
    guard case .initialized = self.state else {
      return
    }
    // Mark the state of the library as initialized
    self.state = .initialized
    // Initialize libraries on which this library depends
    for library in self.libraries {
      library.initialize(in: context)
    }
    // Compile and run
    let environment = Environment(in: context, for: self)
  }
  
  private func parseLibraryDefinition(_ def: Expr, in context: Context) throws {
    var decls = def
    while case .pair(let decl, let next) = decls {
      switch decl {
        case .pair(.symbol(context.symbols.export), let spec):
          var exportList = spec
          while case .pair(let export, let next) = exportList {
            switch export {
              case .symbol(let sym):
                self.exportDecls[sym] = .mutable(sym)
              case .pair(.symbol(context.symbols.rename),
                         .pair(.symbol(let intSym), .pair(.symbol(let extSym), .null))):
                self.exportDecls[extSym] = .mutable(intSym)
              default:
                throw EvalError.malformedLibraryDefinition(decls: decls)
            }
            exportList = next
          }
          guard exportList.isNull else {
            throw EvalError.malformedLibraryDefinition(decls: decls)
          }
        case .pair(.symbol(context.symbols.exportImmutable), let spec):
          var exportList = spec
          while case .pair(let export, let next) = exportList {
            switch export {
              case .symbol(let sym):
                self.exportDecls[sym] = .immutable(sym)
              case .pair(.symbol(context.symbols.rename),
                         .pair(.symbol(let intSym), .pair(.symbol(let extSym), .null))):
                self.exportDecls[extSym] = .immutable(intSym)
              default:
                throw EvalError.malformedLibraryDefinition(decls: decls)
            }
            exportList = next
          }
          guard exportList.isNull else {
            throw EvalError.malformedLibraryDefinition(decls: decls)
          }
        case .pair(.symbol(context.symbols.`import`), let spec):
          guard let importSet = ImportSet(spec, in: context) else {
            throw EvalError.malformedLibraryDefinition(decls: decls)
          }
          importDecls.append(importSet)
        case .pair(.symbol(context.symbols.begin), let exprs):
          var initExprs = exprs
          while case .pair(let initExpr, let next) = initExprs {
            initDecls.append(initExpr)
            initExprs = next
          }
          guard initExprs.isNull else {
            throw EvalError.malformedLibraryDefinition(decls: decls)
          }
        default:
          throw EvalError.malformedLibraryDefinition(decls: decls)
      }
      decls = next
    }
    guard decls.isNull else {
      throw EvalError.malformedLibraryDefinition(decls: decls)
    }
  }
  
  /// Libraries do not mark other referenced libraries; this is assuming that all libraries
  /// are tracked individually.
  public func mark(_ tag: UInt8) {
    // TODO: implement library marking
  }
  
  public var description: String {
    return "<library \(self.name) exporting \(self.exports.keys)>"
  }
}

public func ==(left: Library.InternalIdent, right: Library.InternalIdent) -> Bool {
  return left.identifier == right.identifier
}
