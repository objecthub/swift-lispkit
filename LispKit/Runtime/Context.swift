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
  
  /// A delegate object which receives updates related to the virtual machine managed by
  /// this context. The virtual machine also delegates some functionality to this object.
  public var delegate: ContextDelegate?
  
  /// The console for reading and writing strings from the default port.
  public let console: Console
  
  /// The global expression heap.
  public let heap: Heap
  
  /// A centralized module for handling files.
  public let fileHandler: FileHandler
  
  /// The managed object pool for freeing up objects with cyclic dependencies.
  public let objects: ManagedObjectPool
  
  /// The symbol table for managing interned symbols.
  public let symbols: SymbolTable
  
  /// The library manager of this context.
  public private(set) var libraries: LibraryManager! = nil
  
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
  public init(console: Console, delegate: ContextDelegate? = nil) {
    // Initialize components
    self.delegate = delegate
    self.console = console
    self.heap = Heap()
    self.fileHandler = FileHandler()
    self.objects = ManagedObjectPool()
    self.symbols = SymbolTable()
    self.inputPort = Port(input: TextInput(source: console))
    self.outputPort = Port(output: TextOutput(target: console, threshold: 0))
    self.errorPort = self.inputPort
    self.libraries = LibraryManager(for: self)
    self.environment = Environment(in: self)
    self.machine = VirtualMachine(for: self)
    // Register tracked objects
    self.objects.track(self.machine)
    // Load native libraries
    do {
      for nativeLibrary in LibraryRegistry.nativeLibraries {
        try self.libraries.load(libraryType: nativeLibrary)
      }
    } catch let error {
      preconditionFailure("cannot load native libraries: \(error)")
    }
  }
  
  /// Returns the global environment of this context
  public var global: Env {
    return .global(self.environment)
  }
}
