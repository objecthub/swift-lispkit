//
//  Library.swift
//  LispKit
//
//  Created by Matthias Zenger on 22/01/2016.
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
/// `Library` defines a framework for defining built-in functionality for LispKit in a modular
/// fashion. Concrete implementations subclass `Library` and override the `export` method with
/// declarations of constants, procedures, and special forms.
///
open class NativeLibrary: Library {
  
  /// Initialize native library by providing a hook that programmatically sets up the
  /// declarations of this library.
  public required init(in context: Context) throws {
    try super.init(name: Library.name(type(of: self).name, in: context), in: context)
    self.declarations()
  }
  
  /// This method overrides `wire` from the `Library` initialization protocol to provide a hook
  /// for getting access to imported definitions.
  public override func wire() {
    super.wire()
    self.dependencies()
  }
  
  /// This is the name of the native library. This needs to be overridden in subclasses.
  public class var name: [String] {
    preconditionFailure("native library missing name")
  }
  
  /// The `declarations` method needs to be overridden in subclasses of `NativeLibrary`. These
  /// overriding implementations declare all bindings that are imported, exported, or used
  /// internally by the library.
  public func declarations() {
    // This method needs to be overridden for concrete native libraries.
  }
  
  /// The `dependencies` method needs to be overrideen in subclasses of `NativeLibrary` if the
  /// subclass needs access to the locations of imported methods.
  public func dependencies() {
    // This method needs to be overridden for concrete native libraries.
  }
  
  /// Imports the definitions described by the given import set. This method can only be used in
  /// the `declarations` method of a subclass.
  public func `import`(_ importSet: ImportSet) {
    self.importDecls.append(importSet)
  }
  
  /// Imports all the definitions from `library`.
  public func include(_ library: [String]) {
    self.importDecls.append(.library(Library.name(library, in: self.context)))
  }
  
  /// Imports the definitions `idents` from `library`. This method can only be used in the
  /// `declarations` method of a subclass.
  public func `import`(from library: [String], _ idents: String...) {
    var syms = [Symbol]()
    for ident in idents {
      syms.append(self.context.symbols.intern(ident))
    }
    self.importDecls.append(.only(syms, .library(Library.name(library, in: self.context))))
  }
  
  /// Imports the definitions `idents` from `library`. This method can only be used in the
  /// `declarations` method of a subclass.
  public func `import`(from library: [String], _ syms: Symbol...) {
    self.importDecls.append(.only(syms, .library(Library.name(library, in: self.context))))
  }
  
  /// Declares a new definition for name `name` assigned to `expr`. The optional parameter
  /// `mutable` determines whether the definition can be mutated. `export` determines whether
  /// the definition is exported or internal.
  @discardableResult public func define(_ name: String,
                                        as expr: Expr = .undef,
                                        mutable: Bool = false,
                                        export: Bool = true) -> Int {
    let location = self.context.allocateLocation(for: expr)
    let ident = self.context.symbols.intern(name)
    if export {
      if mutable {
        self.exports[ident] = .mutable(location)
        self.exportDecls[ident] = .mutable(ident)
      } else {
        self.exports[ident] = .immutable(location)
        self.exportDecls[ident] = .immutable(ident)
      }
    } else {
      if mutable {
        self.imports[ident] = .mutable(location)
      } else {
        self.imports[ident] = .immutable(location)
      }
    }
    return location
  }
    
  public func define(_ proc: Procedure, export: Bool = true) {
    self.define(proc.name, as: .procedure(proc), export: export)
  }
  
  public func define(_ name: String, as proc: Procedure, export: Bool = true) {
    self.define(proc.name, as: .procedure(proc), export: export)
  }
  
  public func define(_ name: String, as special: SpecialForm, export: Bool = true) {
    self.define(name, as: .special(special), export: export)
  }
  
  public func define(_ name: String, via: String...) {
    self.define(name, export: true)
    self.execute(code: via.reduce("", +))
  }
  
  /// Adds the given expression to the initialization declarations of this library.
  public func execute(expr: Expr) {
    self.initDecls.append(expr)
  }
  
  /// Parses the given string and adds the resulting expression to the initialization
  /// declarations of this library.
  public func execute(code: String) {
    do {
      let parser = Parser(symbols: self.context.symbols, src: code)
      while !parser.finished {
        self.execute(expr: try parser.parse())
      }
    } catch let error as LispError { // handle Lisp-related issues
      preconditionFailure("compilation failure: " + error.description)
    } catch { // handle internal issues
      preconditionFailure()
    }
  }
  
  public func execute(_ source: String...) {
    self.execute(code: source.reduce("", +))
  }
  
  /// Returns the location of the imported symbol. This method can only be used in the
  /// `dependencies` method of subclasses.
  public func imported(_ symbol: Symbol) -> Int {
    return self.imports[symbol]!.location
  }
  
  /// Returns the location of the imported symbol, identified by string `str`. This method can
  /// only be used in the `dependencies` method of subclasses.
  public func imported(_ str: String) -> Int {
    return self.imported(self.context.symbols.intern(str))
  }
  
  /// Returns the procedure at the given location. This method fails if there is no procedure
  /// at the given location. This method can only be used in native procedure implementations to
  /// refer to other procedures (that are imported, or defined internally).
  public func procedure(_ location: Int) -> Procedure {
    let expr = self.context.locations[location]
    guard case .procedure(let proc) = expr else {
      preconditionFailure("predefined procedure not a procedure")
    }
    return proc
  }
  
  /// Invokes the given instruction `instr` with parameter `expr` in environment `env` for
  /// compiler `compiler`. This method must only be used in native function implementations.
  public func invoke(_ instr: Instruction,
                     with expr: Expr,
                     in env: Env,
                     for compiler: Compiler) throws -> Bool {
    guard case .pair(_, .pair(let arg, .null)) = expr else {
      throw EvalError.argumentCountError(formals: 1, args: expr)
    }
    try compiler.compile(arg, in: env, inTailPos: false)
    compiler.emit(instr)
    return false
  }
}
