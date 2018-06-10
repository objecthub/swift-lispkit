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

import Foundation

///
/// Represents a Scheme evaluation context. Evaluation contexts provide
/// access to components shared by all environments.
///
public final class Context {
  
  /// The name of the LispKit interpreter which is defined by this context.
  public let implementationName: String?
  
  /// Version of the LispKit interpreter which is defined by this context.
  public let implementationVersion: String?
  
  /// Command-line arguments
  public let commandLineArguments: [String]
  
  /// A delegate object which receives updates related to the virtual machine managed by
  /// this context. The virtual machine also delegates some functionality to this object.
  public var delegate: ContextDelegate?
  
  /// The console for reading and writing strings from the default port.
  public let console: Console
  
  /// The global expression heap.
  public let heap: Heap
  
  /// A centralized module for handling files.
  public let fileHandler: FileHandler
  
  /// The source manager of this context.
  public let sourceManager: SourceManager
  
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
  public var inputPort: Port!
  
  /// The current output port.
  public var outputPort: Port!
  
  /// The current error port.
  public var errorPort: Port!
  
  /// Bundle of the LispKit module
  public static let bundle = Bundle(identifier: "net.objecthub.LispKit")
  
  /// Name of the LispKit implementation
  public static let implementationName = Context.bundle?.infoDictionary?["CFBundleName"] as? String
  
  /// Version of the LispKit implementation
  public static let implementationVersion =
    Context.bundle?.infoDictionary?["CFBundleShortVersionString"] as? String
  
  /// Path to default prelude file. Set it to the prelude provided by the bundle, if this exists,
  /// or fall back to the LispKit directory contained in the Documents folder.
  public static let defaultPreludePath =
    Context.bundle?.path(forResource: "Prelude", ofType: "scm", inDirectory: "LispKit/Resources") ??
    URL(fileURLWithPath: "LispKit/Prelude.scm",
        relativeTo: URL(fileURLWithPath: NSSearchPathForDirectoriesInDomains(
                                           .documentDirectory,
                                           .userDomainMask,
                                           true)[0])).absoluteURL.path
  
  /// Initializes a new object
  public init(console: Console,
              implementationName: String? = nil,
              implementationVersion: String? = nil,
              commandLineArguments: [String]? = nil,
              includeInternalResources: Bool = true,
              includeDocumentPath: String? = "LispKit",
              delegate: ContextDelegate? = nil) {
    // Initialize components
    self.delegate = delegate
    self.implementationName = implementationName ?? Context.implementationName
    self.implementationVersion = implementationVersion ?? Context.implementationVersion
    self.commandLineArguments = commandLineArguments ?? CommandLine.arguments
    self.console = console
    self.heap = Heap()
    self.fileHandler = FileHandler(includeInternalResources: includeInternalResources,
                                   includeDocumentPath: includeDocumentPath)
    self.sourceManager = SourceManager()
    self.objects = ManagedObjectPool()
    self.symbols = SymbolTable()
    self.libraries = LibraryManager(for: self)
    self.environment = Environment(in: self)
    self.machine = VirtualMachine(for: self)
    self.inputPort = Port(input: TextInput(source: console,
                                           abortionCallback: self.machine.isAbortionRequested))
    self.outputPort = Port(output: TextOutput(target: console, threshold: 0))
    self.errorPort = self.inputPort
    // Register tracked objects
    self.objects.track(self.machine)
    self.objects.track(self.heap)
    self.objects.track(self.libraries)
    // Load native libraries
    do {
      for nativeLibrary in LibraryRegistry.nativeLibraries {
        try self.libraries.load(libraryType: nativeLibrary)
      }
    } catch let error {
      preconditionFailure("cannot load native libraries: \(error)")
    }
  }
  
  /// Returns the global environment of this context.
  public var global: Env {
    return .global(self.environment)
  }
}
