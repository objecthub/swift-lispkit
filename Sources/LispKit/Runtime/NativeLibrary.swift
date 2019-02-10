//
//  NativeLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 01/10/2016.
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
/// `NativeLibrary` defines a framework for defining built-in functionality for LispKit in
/// a modular fashion. Concrete implementations subclass `NativeLibrary` and override the
/// `export` method with declarations of constants, procedures, and special forms.
///
open class NativeLibrary: Library {

  /// Maps internal identifiers of internal mutable definitions to locations
  public internal(set) var internals: [Symbol : Int]

  /// Initialize native library by providing a hook that programmatically sets up the
  /// declarations of this library.
  public required init(in context: Context) throws {
    self.internals = [:]
    try super.init(name: Library.name(type(of: self).name, in: context), in: context)
    self.declarations()
  }

  /// This method overrides `initializationEnvironment` to also include the internal definitions
  /// in the environment.
  internal override func initializationEnvironment() throws -> Environment {
    let environment = try Environment(in: self.context, for: self)
    for (ident, loc) in self.internals {
      environment.bind(ident, to: .mutable(loc))
    }
    return environment
  }

  /// This method overrides `allocate` from the `Library` initialization protocol to provide
  /// a hook for importing definitions used later during initialization.
  public override func allocate() throws -> Bool {
    if self.state == .loaded {
      self.dependencies()
      if try super.allocate() {
        try self.reexports()
        return true
      }
    } else if try super.allocate() {
      try self.reexports()
      return true
    }
    return false
  }

  /// This method overrides `wire` from the `Library` initialization protocol to provide
  /// a hook for getting access to imported definitions.
  public override func wire() throws -> Bool {
    if try super.wire() {
      self.initializations()
      return true
    }
    return false
  }

  /// This is the name of the native library. This needs to be overridden in subclasses.
  open class var name: [String] {
    preconditionFailure("native library missing name")
  }

  /// The `declarations` method needs to be overridden in subclasses of `NativeLibrary`. These
  /// overriding implementations declare all bindings that are exported or used
  /// internally by the library.
  open func declarations() {
    // This method needs to be overridden for concrete native libraries.
  }

  /// The `dependencies` method needs to be overridden in subclasses of `NativeLibrary`.
  /// These overriding implementations import all bindings that are needed in declarations
  /// that are getting compiled during initialization.
  open func dependencies() {
    // This method needs to be overridden for concrete native libraries.
  }

  /// The `exports` method needs to be overridden in subclasses of `NativeLibrary`. It can be
  /// used to declare exported bindings from imports.
  open func reexports() throws {
    // This method needs to be overridden for concrete native libraries.
  }

  /// The `initializations` method needs to be overrideen in subclasses of `NativeLibrary` if the
  /// subclass needs access to the locations of imported methods.
  open func initializations() {
    // This method needs to be overridden for concrete native libraries.
  }

  /// Imports the definitions described by the given import set. This method can only be used in
  /// the `dependencies` method of a subclass.
  public func `import`(_ importSet: ImportSet) {
    self.importDecls.append(importSet)
  }

  /// Imports the definitions `idents` from `library`. This method can only be used in the
  /// `dependencies` method of a subclass.
  public func `import`(from library: [String], _ idents: String...) {
    var syms = [Symbol]()
    for ident in idents {
      syms.append(self.context.symbols.intern(ident))
    }
    if syms.count == 0 {
      self.importDecls.append(.library(Library.name(library, in: self.context)))
    } else {
      self.importDecls.append(.only(syms, .library(Library.name(library, in: self.context))))
    }
  }

  /// Exports all bindings from this library that are also exported by the imported library
  /// `library`. This method fails if the library is unknown or was not imported before.
  /// Bindings whose identifier starts with `_` are not re-exported.
  public func export(from library: Expr) throws {
    guard let lib = try self.context.libraries.lookup(library) else {
      preconditionFailure("cannot export from unknown library \(library)")
    }
    guard self.libraries.contains(lib) else {
      preconditionFailure("cannot export library \(library) without importing it")
    }
    for (sym, internalName) in lib.exportDecls {
      if self.exportDecls[sym] == nil && sym.identifier.first != Character("_") {
        switch internalName {
          case .mutable:
            self.exportDecls[sym] = .mutable(sym)
          case .immutable:
            self.exportDecls[sym] = .immutable(sym)
        }
      }
    }
  }

  /// Exports all bindings from this library that are also exported by the library identified
  /// by `library`. This method fails if the library is unknown or was not imported before.
  /// Bindings whose identifier starts with `_` are not re-exported.
  public func export(from library: [String]) throws {
    try self.export(from: self.context.libraries.name(library))
  }

  /// Exports all bindings from all libraries imported by this library.
  public func exportAll() throws {
    for library in self.libraries {
      try self.export(from: library.name)
    }
  }

  /// Declares a new definition for name `name` assigned to `expr`. The optional parameter
  /// `mutable` determines whether the definition can be mutated. `export` determines whether
  /// the definition is exported or internal.
  @discardableResult public func define(_ name: String,
                                        as expr: Expr? = nil,
                                        mutable: Bool = false,
                                        export: Bool = true) -> Int {
    let ident = self.context.symbols.intern(name)
    let location = self.context.heap.allocateLocation(for: expr ?? .uninit(ident))
    if export {
      if mutable {
        self.exports[ident] = .mutable(location)
        self.exportDecls[ident] = .mutable(ident)
      } else {
        self.exports[ident] = .immutable(location)
        self.exportDecls[ident] = .immutable(ident)
      }
    } else {
      self.internals[ident] = location
    }
    return location
  }

  /// Declares a new definition for the given procedure `proc` using the internal name of `proc`.
  /// The optional parameter `export` determines whether the definition is exported or internal.
  @discardableResult public func define(_ proc: Procedure, export: Bool = true) -> Int {
    return self.define(proc.name, as: .procedure(proc), export: export)
  }

  /// Declares a new special form using the internal name of `spec`.
  /// The optional parameter `export` determines whether the definition is exported or internal.
  @discardableResult public func define(_ spec: SpecialForm, export: Bool = true) -> Int {
    return self.define(spec.name, as: .special(spec), export: export)
  }

  /// Declares a new definition for the given procedure `proc` and name `name`. The optional
  /// parameter `export` determines whether the definition is exported or internal.
  @discardableResult public func define(_ name: String,
                                        as proc: Procedure,
                                        export: Bool = true) -> Int {
    return self.define(name, as: .procedure(proc), export: export)
  }

  /// Declares a new syntactical definition for the given special form `special` and name
  /// `name`. The optional parameter `export` determines whether the definition is exported
  /// or internal.
  @discardableResult public func define(_ name: String,
                                        as special: SpecialForm,
                                        export: Bool = true) -> Int {
    return self.define(name, as: .special(special), export: export)
  }

  /// Declares a new exported definition expressed in terms of source code.
  @discardableResult public func define(_ name: String,
                                        mutable: Bool = false,
                                        export: Bool = true,
                                        via: String...) -> Int {
    let res = self.define(name, as: nil, mutable: mutable, export: export)
    self.execute(code: via.joined())
    return res
  }

  /// Adds the given expression to the initialization declarations of this library.
  public func execute(expr: Expr) {
    if let block = self.initDeclBlocks.last, block.sourceDirectory == nil {
      block.decls.append(expr)
    } else {
      self.initDeclBlocks.append(DeclBlock(decls: [expr]))
    }
  }

  /// Parses the given string and adds the resulting expression to the initialization
  /// declarations of this library.
  public func execute(code: String) {
    do {
      let input = TextInput(string: code,
                            abortionCallback: self.context.machine.isAbortionRequested)
      let parser = Parser(symbols: self.context.symbols, input: input)
      while !parser.finished {
        self.execute(expr: try parser.parse().datum)
      }
    } catch let error as RuntimeError { // handle Lisp-related issues
      preconditionFailure("compilation failure \(error) when compiling: \(code)")
    } catch { // handle internal issues
      preconditionFailure()
    }
  }

  public func execute(_ source: String...) {
    self.execute(code: source.joined())
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

  /// Returns the native library for the given implementation (if imported).
  public func nativeLibrary<T: NativeLibrary>(_: T.Type) -> T? {
    for library in self.libraries {
      if let l = library as? T {
        return l
      }
    }
    return nil
  }

  /// Returns the procedure at the given location. This method fails if there is no procedure
  /// at the given location. This method can only be used in native procedure implementations to
  /// refer to other procedures (that are imported, or defined internally).
  public func procedure(_ location: Int) -> Procedure {
    let expr = self.context.heap.locations[location]
    guard case .procedure(let proc) = expr else {
      preconditionFailure("predefined procedure not a procedure")
    }
    return proc
  }

  /// Invokes the given instruction `instr` with parameter `expr` in environment `env` for
  /// compiler `compiler`. This method must only be used in form compilers.
  public func invoke(_ instrs: Instruction...,
                     with expr: Expr,
                     in env: Env,
                     for compiler: Compiler) throws -> Bool {
    guard case .pair(_, .pair(let arg, .null)) = expr else {
      throw RuntimeError.argumentCount(min: 1, max: 1, expr: expr)
    }
    try compiler.compile(arg, in: env, inTailPos: false)
    for instr in instrs {
      compiler.emit(instr)
    }
    return false
  }
}

