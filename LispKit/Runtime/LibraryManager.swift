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
public final class LibraryManager: TrackedObject, CustomStringConvertible {

  /// The owner of this library manager.
  private unowned let context: Context
  
  /// Table of loaded libraries
  private var libraries: [Expr : Library]
  
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
  
  /// Returns the libraries loaded by this library manager.
  public var loadedLibraryNames: AnySequence<Expr> {
    return AnySequence(self.libraries.keys)
  }
  
  /// Returns the library loaded by this library manager with the given name.
  public func lookup(_ name: Expr) -> Library? {
    guard let library = self.libraries[name] else {
      self.load(name: name)
      return self.libraries[name]
    }
    return library
  }
  
  /// Returns the library loaded by this library manager with the given name.
  public func lookup(_ name: [String]) -> Library? {
    return self.lookup(self.name(name))
  }
  
  /// Returns the library loaded by this library manager with the given name.
  public func lookup(_ nameComponents: String...) -> Library? {
    return self.lookup(self.name(nameComponents))
  }
  
  /// Attempt to load library `name` from disk. This method only loads a library definition
  /// if the library isn't loaded yet and hasn't been tried loading previously.
  public func load(name: Expr) {
    guard self.libraries[name] == nil && !self.unknownLibraries.contains(name) else {
      return
    }
    if let filename = self.filename(name) {
      // Evaluate file
      do {
        _ = try self.context.machine.eval(
                  file: self.context.fileHandler.libraryFilePath(forFile: filename) ?? filename,
                  in: self.context.global)
      } catch let error {
        Swift.print("error = \(error.localizedDescription)")
        // TODO: figure out how to best propagate an error in such a case
        // ignore
      }
    }
  }
  
  /// Load library with the given name and library declarations.
  public func load(name: Expr, declarations: Expr) throws {
    let library = try Library(name: name, declarations: declarations, in: self.context)
    self.libraries[name] = library
    self.context.delegate?.loaded(library: library, by: self)
  }
  
  /// Load native library.
  public func load(libraryType: NativeLibrary.Type) throws {
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
          if components.count == 0 {
            components.append(String(num))
          } else {
            components[components.count - 1] = components[components.count - 1] + "_\(num)"
          }
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
  public override func mark(_ tag: UInt8) {
    for library in self.libraries.values {
      library.mark(tag)
    }
  }
}
