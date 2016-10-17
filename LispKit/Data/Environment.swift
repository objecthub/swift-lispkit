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
  public enum Kind {
    case library(Expr)
    case program(String)
    case repl
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
        case .mutable(let location):
          return String(location)
        case .mutableImport(let location):
          return "@\(location)"
        case .immutableImport(let location):
          return "^\(location)"
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
  private var bindings: [Symbol : LocationRef]
  
  /// Initializes an empty interactive environment (typically used for read-eval-print loops).
  public init(in context: Context) {
    self.kind = .repl
    self.context = context
    self.bindings = [:]
  }
  
  /// Initializes an empty environment for executing a program read from `filename`.
  public init(in context: Context, for filename: String) {
    self.kind = .program(filename)
    self.context = context
    self.bindings = [:]
  }
  
  /// Initializes an environment for executing the declarations of a library.
  public init(in context: Context, for library: Library) {
    // Set up empty environment
    self.kind = .library(library.name)
    self.context = context
    self.bindings = [:]
    // Wire the library
    library.wire()
    // Set up environment based on wired library
    for (ident, importLocation) in library.imports {
      switch importLocation {
        case .mutable(let loc):
          self.bindings[ident] = .mutableImport(loc)
        case .immutable(let loc):
          self.bindings[ident] = .immutableImport(loc)
      }
    }
    for (extIdent, intIdent) in library.exportDecls {
      if !library.imported.hasValues(for: intIdent.identifier) {
        self.bindings[intIdent.identifier] = .mutable(library.exports[extIdent]!.location)
      }
    }
  }
  
  /// Returns an array of all the symbols bound in this environment.
  public var boundSymbols: [Symbol] {
    return [Symbol](self.bindings.keys)
  }
  
  /// Looks up value associated with `sym` in this environment.
  public subscript(sym: Symbol) -> Expr {
    guard let loc = self.bindings[sym.interned]?.location else {
      return .undef
    }
    return self.context.locations[loc]
  }
  
  /// Returns true if the given symbol is not defined in this environment.
  public func isUndefined(_ sym: Symbol) -> Bool {
    return self.bindings[sym.interned]?.isUndefined ?? true
  }
  
  /// Returns the location reference associated with `sym`.
  public func locationRef(for sym: Symbol) -> LocationRef {
    return self.bindings[sym.interned] ?? .undefined
  }
  
  /// Sets the location reference of `sym` to `lref`.
  internal func setLocationRef(for sym: Symbol, to lref: LocationRef) {
    if case .undefined = lref {
      preconditionFailure("cannot set binding in environment to undefined")
    }
    self.bindings[sym.interned] = lref
  }
  
  /// Defines a new binding in this environment from `sym` to `expr`. This function returns
  /// false only if this is an environment for a program or a library and the symbol was
  /// previously bound already.
  public func define(_ sym: Symbol, as expr: Expr) -> Bool {
    let interned = sym.interned
    let lref = self.bindings[interned] ?? .undefined
    switch lref {
      case .undefined:
        self.bindings[interned] = .mutable(self.context.allocateLocation(for: expr))
        return true
      case .mutable(let loc):
        switch self.kind {
          case .library, .program: // illegal redefinition of a binding
            return false
          case .repl:              // change existing binding
            self.context.locations[loc] = expr
            return true
        }
      case .mutableImport(_), .immutableImport(_):
        switch self.kind {
          case .library, .program: // illegal redefinition of a binding
            return false
          case .repl:              // override existing binding
            self.bindings[interned] = .mutable(self.context.allocateLocation(for: expr))
            return true
        }
    }
  }
  
  /// Redefines a binding in this environment from `sym` to `expr`. This function returns
  /// false if there either was no previous binding, or the previous binding was immutable.
  public func set(_ sym: Symbol, to expr: Expr) -> Bool {
    let interned = sym.interned
    let lref = self.bindings[interned] ?? .undefined
    switch lref {
      case .undefined:
        return false
      case .mutable(let loc), .mutableImport(let loc):
        self.context.locations[loc] = expr
        return true
      case .immutableImport(_):
        return false
    }
  }
  
  /// Imports the bindings defined by `importSet` into this environment. Environments of
  /// libraries do not support imports.
  public func `import`(_ importSet: ImportSet) -> Library? {
    // Cannot import into libraries
    if case .library(_) = self.kind {
      return nil
    }
    // Expand the import set
    guard let (library, importSpec) = importSet.expand(in: self.context) else {
      // Could not expand import set
      return nil
    }
    // Make sure the library from which symbols are imported is wired
    library.wire()
    // Check that bindings can be imported
    for impIdent in importSpec.keys {
      switch self.bindings[impIdent] {
        case .some(.mutable(_)), .some(.mutableImport(_)), .some(.immutableImport(_)):
          guard case .repl = self.kind else {
            // Cannot redefine a local binding with an import in a program
            return nil
          }
        default:
          break
      }
    }
    // Import the bindings
    for (impIdent, expIdent) in importSpec {
      switch library.exports[expIdent] {
        case .some(.mutable(let loc)):
          self.bindings[impIdent] = .mutableImport(loc)
        case .some(.immutable(let loc)):
          self.bindings[impIdent] = .immutableImport(loc)
        default:
          // Should nerver happen
          return nil
      }
    }
    return library
  }
  
  /// A description of the bindings in this environment.
  public var description: String {
    var builder = StringBuilder(prefix: "<env", postfix: ">", separator: ", ", initial: ": ")
    for (sym, locref) in self.bindings {
      builder.append(sym.description, " -> ", locref.description)
    }
    return builder.description
  }
}
