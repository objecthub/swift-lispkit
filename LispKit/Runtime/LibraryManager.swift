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

public final class LibraryManager: CustomStringConvertible {

  /// The owner of this library manager.
  private unowned let context: Context
  
  /// Table of loaded libraries
  private var libraries: [Expr : Library]

  /// Initialize a new library manager for the given context.
  internal init(for context: Context) {
    self.context = context
    self.libraries = [:]
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
    return self.libraries[name]
  }
  
  /// Returns the library loaded by this library manager with the given name.
  public func lookup(_ name: [String]) -> Library? {
    return self.libraries[self.name(name)]
  }
  
  /// Returns the library loaded by this library manager with the given name.
  public func lookup(_ nameComponents: String...) -> Library? {
    return self.libraries[self.name(nameComponents)]
  }
  
  /// Load library with the given name and library declarations.
  public func load(name: Expr, declarations: Expr) throws {
    self.libraries[name] = try Library(name: name, declarations: declarations, in: self.context)
  }
  
  /// Load native library.
  public func load(libraryType: NativeLibrary.Type) throws {
    self.libraries[self.name(libraryType.name)] = try libraryType.init(in: self.context)
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
  
  /// Returns a textual description of all the libraries and their current state.
  public var description: String {
    var builder = StringBuilder(prefix: "{", postfix: "}", separator: ", ")
    for name in self.libraries.keys {
      builder.append(name.description)
    }
    return builder.description
  }
}
