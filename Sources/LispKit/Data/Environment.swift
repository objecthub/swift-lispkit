//
//  Environment.swift
//  LispKit
//
//  Created by Matthias Zenger on 16/09/2016.
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
/// `Environment` implements global environments which map symbols to location
/// references (i.e. references into the locations array).
///
public final class Environment: Reference, CustomStringConvertible {
  
  /// Three different kinds of environments are supported. Some operations behave differently
  /// depending on the kind of the environment.
  ///    1. Libraries
  ///    2. Programs
  ///    3. REPL
  public enum Kind: Equatable {
    case library(Expr)
    case program(String)
    case repl
    case custom
  }
  
  /// Tagged reference to a location
  public enum LocationRef: CustomStringConvertible {
    case undefined
    case mutable(Int)
    case mutableImport(Int)
    case immutableImport(Int)
    
    public var isUndefined: Bool {
      guard case .undefined = self else {
        return false
      }
      return true
    }
    
    public var isImmutable: Bool {
      guard case .immutableImport(_) = self else {
        return false
      }
      return true
    }
    
    public var location: Int? {
      switch self {
        case .undefined:
          return nil
        case .mutable(let loc):
          return loc
        case .mutableImport(let loc):
          return loc
        case .immutableImport(let loc):
          return loc
      }
    }
    
    public var description: String {
      switch self {
        case .undefined:
          return "?"
        case .mutable(let loc):
          return String(loc)
        case .mutableImport(let loc):
          return "@\(loc)"
        case .immutableImport(let loc):
          return "^\(loc)"
      }
    }
  }
  
  /// The environment kind.
  public let kind: Kind
  
  /// The context in which this environment is defined in. This is an unowned reference only
  /// since the context is the most long-lived object in a LispKit session.
  public unowned let context: Context
  
  /// The bindings are associations between symbols and locations. Each association is tagged
  /// with information on mutability and whether it's an imported binding.
  internal private(set) var bindings: [Symbol : LocationRef]
  
  /// A weak box object pointing at this environment
  public private(set) var box: WeakBox<Environment>!
  
  /// Initializes an empty interactive environment for read-eval-print loops.
  public init(in context: Context) {
    self.kind = .repl
    self.context = context
    self.bindings = [:]
    super.init()
    self.box = WeakBox(self)
    self.define(context.symbols.`import`,
                as: .special(SpecialForm(context.symbols.`import`.description,
                                         Environment.compileImport)))
  }
  
  /// Initializes an empty environment for executing a program read from `filename`.
  public init(in context: Context, for filename: String) {
    self.kind = .program(filename)
    self.context = context
    self.bindings = [:]
    super.init()
    self.box = WeakBox(self)
  }
  
  /// Initializes an environment for executing the declarations of a library.
  public init(in context: Context, for library: Library) throws {
    // Set up empty environment
    self.kind = .library(library.name)
    self.context = context
    self.bindings = [:]
    super.init()
    self.box = WeakBox(self)
    // Wire the library
    _ = try library.wire()
    // Set up environment based on wired library
    // Include imports in the environment
    for (ident, importLocation) in library.imports {
      switch importLocation {
        case .mutable(let loc):
          self.bind(ident, to: .mutableImport(loc))
        case .immutable(let loc):
          self.bind(ident, to: .immutableImport(loc))
      }
    }
    // Include excludes in the environment
    for (extIdent, intIdent) in library.exportDecls {
      if !library.imported.hasValues(for: intIdent.identifier) {
        let iref = library.exports[extIdent]!
        self.bind(intIdent.identifier, to: .mutable(iref.location))
      }
    }
  }
  
  /// Initializes a custom environment with the given import sets.
  public init(in context: Context, importing importSets: [ImportSet]) throws {
    self.kind = .custom
    self.context = context
    self.bindings = [:]
    super.init()
    self.box = WeakBox(self)
    for importSet in importSets {
      _ = try self.import(from: importSet)
    }
  }
  
  /// Returns the number of bindings in this environment.
  public var count: Int {
    return self.bindings.count
  }
  
  /// Returns an array of all the symbols bound in this environment.
  public var boundSymbols: [Symbol] {
    return [Symbol](self.bindings.keys)
  }
  
  /// Looks up value associated with `sym` in this environment.
  public subscript(sym: Symbol) -> Expr? {
    guard let loc = self.bindings[sym]?.location else {
      return nil
    }
    return self.context.heap.locations[loc]
  }
  
  /// Looks up documentation associated with `sym` in this environment.
  public func documentation(_ sym: Symbol) -> String? {
    guard let loc = self.bindings[sym]?.location else {
      return nil
    }
    return self.context.heap.documentation[loc]
  }
  
  /// Binds symbol `sym` to the given location reference `loc`.
  internal func bind(_ sym: Symbol, to loc: LocationRef) {
    switch self.kind {
      case .repl:
        let new = self.isUndefined(sym)
        self.bindings[sym] = loc
        if new {
          self.context.delegate?.bound(symbol: sym, in: self)
        }
      default:
        self.bindings[sym] = loc
    }
  }
  
  /// Returns true if the given symbol is not defined in this environment.
  public func isUndefined(_ sym: Symbol) -> Bool {
    return self.bindings[sym]?.isUndefined ?? true
  }
  
  /// Returns true if the given symbol is imported in an immutable fashion.
  public func isImmutable(_ sym: Symbol) -> Bool {
    return self.bindings[sym]?.isImmutable ?? false
  }
  
  /// Returns true if the given symbol is imported
  public func isImported(_ sym: Symbol, immutable: Bool? = nil) -> Bool {
    guard let locRef = self.bindings[sym] else {
      return false
    }
    switch locRef {
      case .mutableImport(_):
        return immutable == nil ? true : !(immutable!)
      case .immutableImport(_):
        return immutable == nil ? true : immutable!
      default:
        return false
    }
  }
  
  /// Returns the name of library if this is an environment for a library, `nil` otherwise.
  public var libraryName: Expr? {
    switch self.kind {
      case .library(let lib):
        return lib
      default:
        return nil
    }
  }
  
  /// Returns the location reference associated with `sym`.
  public func locationRef(for sym: Symbol) -> LocationRef {
    return self.bindings[sym] ?? .undefined
  }
  
  /// Returns the location reference associated with `sym`. If the location reference is
  /// undefined, this method will reserve a location, guaranteeing that this method never
  /// returns `LocationRef.undefined`.
  public func forceDefinedLocationRef(for sym: Symbol) -> LocationRef {
    if let locRef = self.bindings[sym] {
      switch locRef {
        case .undefined, .immutableImport(_):
          break;
        default:
          return locRef
      }
    }
    let locRef = LocationRef.mutable(self.context.heap.allocateLocation(for: .uninit(sym)))
    self.bind(sym, to: locRef)
    return locRef
  }
  
  /// Sets the location reference of `sym` to `lref`.
  internal func setLocationRef(for sym: Symbol, to lref: LocationRef) {
    if case .undefined = lref {
      preconditionFailure("cannot set binding in environment to undefined")
    }
    self.bind(sym, to: lref)
  }
  
  /// Defines a new binding in this environment from `sym` to `expr`. This function returns
  /// false only if this is an environment for a program or a library and the symbol was
  /// previously bound already.
  @discardableResult public func define(_ sym: Symbol, as expr: Expr) -> Bool {
    let lref = self.bindings[sym] ?? .undefined
    switch lref {
      case .undefined:
        self.bind(sym, to: .mutable(self.context.heap.allocateLocation(for: expr)))
        return true
      case .mutable(_), .mutableImport(_), .immutableImport(_):
        switch self.kind {
          case .library, .program:          // illegal redefinition of a binding
            return false
          default:                          // override existing binding
            self.bind(sym, to: .mutable(self.context.heap.allocateLocation(for: expr)))
            return true
        }
    }
  }
  
  /// Redefines a binding in this environment from `sym` to `expr`. This function returns
  /// false if there either was no previous binding, or the previous binding was immutable.
  @discardableResult public func set(_ sym: Symbol, to expr: Expr) -> Bool {
    let lref = self.bindings[sym] ?? .undefined
    switch lref {
      case .undefined:
        return false
      case .mutable(let loc):
        self.context.heap.locations[loc] = expr
        return true
      case .mutableImport(let loc):
        self.context.heap.locations[loc] = expr
        return true
      case .immutableImport(_):
        return false
    }
  }
  
  /// Assigns a documentation string to symbol `sym` if `sym` is bound to a value.
  @discardableResult public func assignDoc(of sym: Symbol, to str: String) -> Bool {
    let lref = self.bindings[sym] ?? .undefined
    switch lref {
      case .undefined:
        return false
      case .mutable(let loc):
        self.context.heap.documentation[loc] = str
        return true
      case .mutableImport(let loc):
        self.context.heap.documentation[loc] = str
        return true
      case .immutableImport(_):
        return false
    }
  }
  
  /// Imports all the bindings defined by the library identified by the name `library` into this
  /// environment. Environments of libraries do not support imports. This method forces the
  /// library to get initialized.
  public func `import`(_ library: [String]) throws {
    _ = try self.`import`(.library(self.context.libraries.name(library)))
  }
  
  /// Imports the bindings defined by `importSet` into this environment. Environments of
  /// libraries do not support imports. This method forces the library to be initialized.
  @discardableResult public func `import`(_ importSet: ImportSet) throws -> Library {
    // Cannot import into libraries
    if case .library(let lib) = self.kind {
      throw RuntimeError.eval(.importInLibrary, lib)
    }
    return try self.import(from: importSet)
  }
  
  /// Imports the bindings defined by `importSet` into this environment. Environments of
  /// libraries do not support imports. This method does not force the library to be initialized.
  private func `import`(from importSet: ImportSet) throws -> Library {
    // Expand the import set
    guard let (library, importSpec) = try importSet.expand(in: self.context) else {
      // Could not expand import set
      throw RuntimeError.eval(.cannotExpandImportSet, .makeString(importSet.description))
    }
    // Make sure the library from which symbols are imported is initialized
    _ = try library.initialize()
    // Check that bindings can be imported
    for impIdent in importSpec.keys {
      switch self.bindings[impIdent] {
        case .some(.mutable(_)),
             .some(.mutableImport(_)),
             .some(.immutableImport(_)):
          guard case .repl = self.kind else {
            // Cannot redefine a local binding with an import in a program
            throw RuntimeError.eval(.erroneousRedefinition,
                                    .makeString(impIdent.description),
                                    .makeString(library.description))
          }
        default:
          break
      }
    }
    // Import the bindings
    for (impIdent, expIdent) in importSpec {
      switch library.exports[expIdent] {
        case .some(.mutable(let loc)):
          // print("  import \(expIdent) from \(library.name) as \(impIdent) using " +
          //      "[\(loc)] \(self.context.locations[loc])")
          self.bind(impIdent, to: .mutableImport(loc))
        case .some(.immutable(let loc)):
          // print("  import \(expIdent) from \(library.name) as immutable \(impIdent) using " +
          //       "[\(loc)] \(self.context.locations[loc])")
          self.bind(impIdent, to: .immutableImport(loc))
        default:
          // Should never happen
          throw RuntimeError.eval(.corruptLibrary,
                                  library.name,
                                  .makeString("import \(impIdent) as \(expIdent) using " +
                                              String(describing: library.exports[expIdent])))
      }
    }
    return library
  }
  
  /// A description of the bindings in this environment.
  public var description: String {
    var type: String = ""
    switch self.kind {
      case .library(let name):
        type = " " + name.description
      case .program(let filename):
        type = " " + filename
      case .repl:
        type = " interaction"
      case .custom:
        type = " "
    }
    var builder = StringBuilder(prefix: "«env",
                                postfix: "»",
                                separator: ", ",
                                initial: type + ": ")
    for (sym, locref) in self.bindings {
      builder.append(sym.description, " -> ", locref.description)
    }
    return builder.description
  }
  
  /// Implements `import` for programs and REPLs.
  private static func compileImport(_ compiler: Compiler,
                                    expr: Expr,
                                    env: Env,
                                    tail: Bool) throws -> Bool {
    guard case .pair(_, let args) = expr else {
      preconditionFailure()
    }
    // Check that import is not executed in a local environment
    if case .local(_) = env {
      throw RuntimeError.eval(.importInLocalEnv, args)
    }
    // Collect all import sets
    var importSets: [ImportSet] = []
    var imports = args
    while case .pair(let importSpec, let rest) = imports {
      // Map import spec into an import set
      guard let importSet = ImportSet(importSpec, in: compiler.context) else {
        throw RuntimeError.eval(.malformedImportSet, importSpec)
      }
      importSets.append(importSet)
      imports = rest
    }
    // Import definitions into global environment and initialize the library; do this only
    // once (i.e. skip this step if its done in the second optimization compiler run)
    let cp = compiler.checkpointer.checkpoint()
    if !compiler.checkpointer.imported(cp) {
      compiler.checkpointer.associate(.imported, with: cp)
      for importSet in importSets {
        _ = try env.environment?.`import`(importSet)
      }
    }
    compiler.emit(.pushVoid)
    return false
  }
}
