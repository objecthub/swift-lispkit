//
//  LibraryManager.swift
//  LispKit
//
//  Created by Matthias Zenger on 16/10/2016.
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
/// Class `LibraryManager` manages libraries that are loaded into a LispKit context. This
/// component doesn't need to be a `TrackedObject` because it only stores expressions
/// consisting of pairs, symbols, and integers.
/// 
/// These are the concepts supported by class `LibraryManager`:
///   - `load` tries to load an unknown library definition from disk
///   - `register` is called when either a native library is defined, or as a consequence
///     of `load` when the `define-library` syntax is being compiled. This will just insert
///     the library definition into the dictionary of loaded libraries, but will not further
///     trigger any evaluation of the library.
///   - `lookup` determines if a library is available already, and if that is not the case,
///     it will trigger a `load` to automatically load a library.
/// 
public final class LibraryManager: TrackedObject, CustomStringConvertible {

  /// The owner of this library manager.
  private unowned let context: Context
  
  /// Table of loaded libraries
  public private(set) var libraries: [Expr : Library]
  
  /// Set of unknown libraries
  private var unknownLibraries: Set<Expr>

  /// Initialize a new library manager for the given context.
  internal init(for context: Context) {
    self.context = context
    self.libraries = [:]
    self.unknownLibraries = []
  }
  
  /// Returns the libraries loaded by this library manager.
  public var loaded: AnySequence<Library> {
    return AnySequence(self.libraries.values)
  }
  
  /// Checks if there is a library available to be loaded (if not loaded already)
  public func isAvailable(_ name: Expr) -> Bool {
    if self.unknownLibraries.contains(name) {
      return false
    } else if self.libraries[name] == nil {
      if let filename = self.filename(name),
         self.context.fileHandler.libraryFilePath(forFile: filename) != nil {
        return true
      } else {
        return false
      }
    } else {
      return true
    }
  }
  
  /// Returns the library loaded by this library manager with the given name.
  public func lookup(_ name: [String]) throws -> Library? {
    return try self.lookup(self.name(name))
  }
  
  /// Returns the library loaded by this library manager with the given name.
  public func lookup(_ name: Expr) throws -> Library? {
    guard let library = self.libraries[name] else {
      try self.load(name: name)
      return self.libraries[name]
    }
    return library
  }
  
  /// Returns the native library loaded by this library manager for the given native
  /// library type.
  public func lookup<T: NativeLibrary>(_ impl: T.Type) throws -> T? {
    if let lib = try self.lookup(impl.name) {
      return lib as? T
    } else {
      return nil
    }
  }
  
  /// Returns the library loaded by this library manager with the given name.
  public func lookup(_ nameComponents: String...) throws -> Library? {
    return try self.lookup(self.name(nameComponents))
  }
  
  /// Attempt to load library `name` from disk. This method only loads a library definition
  /// if the library isn't loaded yet and hasn't been tried loading previously.
  private func load(name: Expr) throws {
    guard self.libraries[name] == nil && !self.unknownLibraries.contains(name) else {
      return
    }
    if let filename = self.filename(name) {
      do {
        _ = try self.context.evaluator.machine.eval(
                  file: self.context.fileHandler.libraryFilePath(forFile: filename) ?? filename,
                  in: self.context.global)
      } catch let error as RuntimeError {
        throw error.attach(library: name)
      } catch let error {
        throw RuntimeError.os(error).attach(library: name)
      }
    }
  }
  
  /// Register library with the given name and library declarations.
  public func register(name: Expr, declarations: Expr, origin: String) throws {
    let library = try Library(name: name,
                              declarations: declarations,
                              origin: origin,
                              in: self.context)
    self.libraries[name] = library
    self.context.delegate?.loaded(library: library, by: self)
  }
  
  /// Register native library.
  public func register(libraryType: NativeLibrary.Type) throws {
    let library = try libraryType.init(in: self.context)
    self.libraries[self.name(libraryType.name)] = library
    self.context.delegate?.loaded(library: library, by: self)
  }
  
  /// Returns the library name for the given string components. Strings that can be converted
  /// to an integer are represented as fixnum values, all other strings are converted to symbols.
  public func name(_ components: String...) -> Expr {
    return self.name(components)
  }
  
  /// Returns the library name for the given string components. Strings that can be converted
  /// to an integer are represented as fixnum values, all other strings are converted to symbols.
  public func name(_ components: [String]) -> Expr {
    var res = Expr.null
    for component in components.reversed() {
      if let num = Int64(component) {
        res = .pair(.fixnum(num), res)
      } else {
        res = .pair(.symbol(context.symbols.intern(component)), res)
      }
    }
    return res
  }
  
  /// Returns a filename from which a definition of the library `name` can be loaded.
  public func filename(_ name: Expr) -> String? {
    var components: [String] = []
    var expr = name
    while case .pair(let component, let next) = expr {
      switch component {
        case .symbol(let sym):
          components.append(sym.description)
        case .fixnum(let num):
          components.append(String(num))
        default:
          break
      }
      expr = next
    }
    if components.count > 0 {
      components[components.count - 1] = components[components.count - 1] + ".sld"
    }
    return NSURL.fileURL(withPathComponents: components)?.relativePath
  }
  
  /// Returns a textual description of all the libraries and their current state.
  public var description: String {
    var builder = StringBuilder(prefix: "{", postfix: "}", separator: ", ")
    for name in self.libraries.keys {
      builder.append(name.description)
    }
    return builder.description
  }
  
  /// Mark all registered libraries
  public override func mark(in gc: GarbageCollector) {
    for library in self.libraries.values {
      library.mark(in: gc)
    }
  }
  
  /// Reset library manager
  public func release() {
    for library in self.libraries.values {
      library.release()
    }
    self.libraries.removeAll()
    self.unknownLibraries.removeAll()
  }
}
