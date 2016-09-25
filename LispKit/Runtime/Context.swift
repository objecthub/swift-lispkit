//
//  Context.swift
//  LispKit
//
//  Created by Matthias Zenger on 14/01/2016.
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
/// Represents a Scheme evaluation context. Evaluation contexts provide
/// access to components shared by all environments.
///
public final class Context {
  
  /// The console window, for reading and writing strings from the default port.
  public let console: Console
  
  /// The managed object pool for freeing up objects with cyclic dependencies.
  public let objects: ManagedObjectPool
  
  /// The symbol table for managing interned symbols.
  public let symbols: SymbolTable
  
  /// Table of global locations
  public var locations: Exprs
  
  /// Table of loaded libraries
  public var libraries: [Expr : Library]
  
  /// The user scope.
  public let userScope: Scope
  
  /// The system scope.
  public var systemScope: Scope {
    return self.userScope.outer!
  }
  
  /// The global environment is typically used for read-eval-print loops.
  public private(set) var environment: Environment! = nil
  
  /// The virtual machine for executing Lisp code.
  public private(set) var machine: VirtualMachine! = nil

  /// The current input port.
  public var inputPort: Port
  
  /// The current output port.
  public var outputPort: Port
  
  /// The current error port.
  public var errorPort: Port
  
  /// Initializes a new object
  public init(console: Console, library: NativeLibrary.Type? = nil) {
    // Initialize global components
    self.console = console
    self.objects = ManagedObjectPool()
    self.symbols = SymbolTable()
    self.locations = Exprs()
    self.libraries = [:]
    self.userScope = Scope(Scope())
    self.inputPort = Port(input: TextInput(source: console))
    self.outputPort = Port(output: TextOutput(target: console, threshold: 0))
    self.errorPort = self.inputPort
    self.environment = Environment(in: self)
    self.machine = VirtualMachine(self)
    // Register tracked objects
    self.objects.track(self.machine)
    self.objects.track(self.userScope)
    // Import libraries
    if let lib = library {
      self.use(lib)
    }
  }
  
  /// Allocates a new global location and initializes it with `expr`.
  public func allocateLocation(for expr: Expr = .undef) -> Int {
    self.locations.append(expr)
    return self.locations.count - 1
  }
  
  /// Registers the given library in the context. This method returns false if there is a
  /// library of the same name registered already.
  public func register(library: Library) -> Bool {
    guard self.libraries[library.name] == nil else {
      return false
    }
    self.libraries[library.name] = library
    return true
  }
  
  /// Import an instantiation of the given library type.
  public func use(_ lib: NativeLibrary.Type) {
    let _ = lib.init(self)
  }
}
