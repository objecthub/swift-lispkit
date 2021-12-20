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
/// Represents a Scheme evaluation context. Evaluation contexts provide access to
/// components shared by all environments. This class should be used if extensions, or
/// customizations of individual components are needed. Class `LispKitContext` can be
/// used if the interpreter is used without customizations.
///
open class Context {
  
  /// The name of the LispKit interpreter which is defined by this context.
  public let implementationName: String?
  
  /// Version of the LispKit interpreter which is defined by this context.
  public let implementationVersion: String?
  
  /// Command-line arguments
  public let commandLineArguments: [String]
  
  /// Initial home path used by the LispKit file system
  public let initialHomePath: String?
  
  /// A delegate object which receives updates related to the virtual machine managed by
  /// this context. The virtual machine also delegates some functionality to this object.
  /// Context delegates are only referenced weakly.
  public weak var delegate: ContextDelegate?
  
  /// The global expression heap.
  public let heap: Heap
  
  /// A centralized module for handling files.
  public let fileHandler: FileHandler
  
  /// The source manager of this context.
  public let sources: SourceManager
  
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
  
  /// The features exposed by the LispKit interpreter defined by this context
  public let features: Set<String>
  
  /// The default input port.
  internal var inputPort: Port!
  
  /// The default output port.
  internal var outputPort: Port!
  
  /// Use simplified descriptions?
  public static var simplifiedDescriptions: Bool = false
  
  /// Initializes a new context.
  public init(delegate: ContextDelegate,
              implementationName: String?,
              implementationVersion: String?,
              commandLineArguments: [String],
              initialHomePath: String?,
              includeInternalResources: Bool,
              includeDocumentPath: String?,
              assetPath: String?,
              gcDelay: Double,
              features: [String]) {
    // Initialize components
    self.delegate = delegate
    self.implementationName = implementationName
    self.implementationVersion = implementationVersion
    self.commandLineArguments = commandLineArguments
    self.initialHomePath = initialHomePath
    self.heap = Heap()
    self.fileHandler = FileHandler(includeInternalResources: includeInternalResources,
                                   includeDocumentPath: includeDocumentPath)
    if let path = assetPath {
      _ = self.fileHandler.addAssetSearchPath(path)
    }
    self.sources = SourceManager()
    self.objects = ManagedObjectPool(marker: GarbageCollector(),
                                     gcDelay: gcDelay,
                                     gcCallback: delegate.garbageCollected)
    self.symbols = SymbolTable()
    var supported = Feature.supported
    for feature in features {
      supported.insert(feature)
    }
    self.features = supported
    self.libraries = LibraryManager(for: self)
    self.environment = Environment(in: self)
    self.machine = VirtualMachine(for: self)
    self.inputPort = Port(input: TextInput(source: delegate,
                                           abortionCallback: self.machine.isAbortionRequested))
    self.outputPort = Port(output: TextOutput(target: delegate, threshold: 0))
    // Register tracked objects
    self.objects.track(self.machine)
    self.objects.track(self.heap)
    self.objects.track(self.libraries)
    // Register native libraries
    do {
      for nativeLibrary in LibraryRegistry.nativeLibraries {
        try self.libraries.register(libraryType: nativeLibrary)
      }
    } catch let error {
      preconditionFailure("cannot load native libraries: \(error)")
    }
  }
  
  /// Prepares context to be ready to execute code. Library `(lispkit core)` and
  /// `(lispkit dynamic)` gets loaded as a result of this. If `forRepl` is set to true,
  /// `bootstrap` will also introduce the variables `*1`, `*2`, and `*3` in the global
  /// environment. These will be used by a REPL to store the last three results.
  public func bootstrap(forRepl: Bool = false) throws {
    // Guarantee that (lispkit dynamic) is imported; this will also load (lispkit core)
    try self.environment.import(["lispkit", "dynamic"])
    // Install error handler
    if let dynamicLib = try self.libraries.lookup("lispkit", "dynamic") as? DynamicControlLibrary,
       let raiseProc = dynamicLib.raiseProc {
      self.machine.raiseProc = raiseProc
    }
    // Install definition procedures
    if let coreLib = try self.libraries.lookup("lispkit", "core") as? CoreLibrary {
      self.machine.loader = coreLib.loader
      self.machine.defineSpecial = coreLib.defineSpecial
      self.machine.defineValuesSpecial = coreLib.defineValuesSpecial
    }
    if forRepl {
      _ = self.environment.define(self.symbols.starOne, as: .undef)
      _ = self.environment.define(self.symbols.starTwo, as: .undef)
      _ = self.environment.define(self.symbols.starThree, as: .undef)
    }
  }
  
  /// This method updates the variables `*1`, `*2`, and `*3` in the global environment
  /// to match the last three results that were being evaluated via a REPL.
  public func update(withReplResult expr: Expr) {
    if let expr = self.environment[self.symbols.starTwo] {
      self.environment.set(self.symbols.starThree, to: expr)
    }
    if let expr = self.environment[self.symbols.starOne] {
      self.environment.set(self.symbols.starTwo, to: expr)
    }
    self.environment.set(self.symbols.starOne, to: expr)
  }
  
  /// Returns the global environment of this context.
  public var global: Env {
    return .global(self.environment)
  }
  
  /// Reset this context
  public func release() {
    self.heap.release()
    self.libraries.release()
    self.sources.release()
    self.symbols.release()
    self.machine.release()
  }
}
