//
//  LispKitRepl.swift
//  LispKitTools
//
//  Created by Matthias Zenger on 18/11/2019.
//  Copyright © 2019 ObjectHub. All rights reserved.
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
import LispKit
import CommandLineKit

///
/// Framework for building simple command-line tools based on LispKit and CommandLineKit.
///
open class LispKitRepl {

  // Tool metadata
  public let toolName: String
  public let toolVersion: String
  public let toolBuild: String
  public let toolCopyright: String

  // Flags
  public let flags: Flags
  public let filePaths: RepeatedArgument<String>
  public let assetPaths: RepeatedArgument<String>
  public let libPaths: RepeatedArgument<String>
  public let roots: RepeatedArgument<String>
  public let searchDocs: SingletonArgument<String>
  public let heapSize: SingletonArgument<Int>
  public let maxStackSize: SingletonArgument<Int>
  public let importLibs: RepeatedArgument<String>
  public let r7rs: Option
  public let lispkit: Option
  public let prelude: SingletonArgument<String>
  public let prompt: SingletonArgument<String>
  public let basic: Option
  public let extended: Option
  public let strict: Option
  public let quiet: Option
  public let help: Option

  // LispKit setup
  public lazy var lineReader: LineReader? = self.basic.wasSet ? nil : LineReader()
  public let terminal: CommandLineDelegate
  public var context: Context?

  /// Initializer of the read-eval-print loop
  public init(name: String,
              version: String,
              build: String,
              copyright: String,
              prompt: String) {
    self.toolName = name
    self.toolVersion = version
    self.toolBuild = build
    self.toolCopyright = copyright
    let f = Flags()
    self.flags = f
    // Declare flags
    self.filePaths  = f.strings("f", "filepath",
                                description: "Adds file paths in which programs are searched for.")
    self.libPaths   = f.strings("l", "libpath",
                                description: "Adds file paths in which libraries are searched for.")
    self.assetPaths = f.strings("a", "assetpath",
                                description: "Adds file paths in which assets are searched for.")
    self.searchDocs = f.string("d", "documents",
                               description: "Search for files and libraries in folder " +
                                            "~/Documents/<value>.")
    self.roots = f.strings("r", "root",
                           description: "Directories in which to search for files and libraries.")
    self.heapSize   = f.int("m", "memsize",
                            description: "Initial capacity of the heap memory.",
                            value: 1000)
    self.maxStackSize = f.int("n", "maxstack",
                              description: "Maximum stack size",
                              value: 10000)
    self.importLibs = f.strings("i", "import",
                                description: "Imports library automatically after startup.")
    self.r7rs       = f.option(nil, "r7rs",
                               description: "Imports library (scheme base).")
    self.lispkit    = f.option(nil, "lispkit",
                               description: "Imports library (lispkit base).")
    self.prelude    = f.string("p", "prelude",
                               description: "Path to prelude file which gets executed after " +
                                            "loading all provided libraries.")
    self.prompt     = f.string("c", "prompt",
                               description: "String used as prompt in REPL.",
                               value: prompt)
    self.basic      = f.option("b", "basic",
                               description: "Use basic line reader only.")
    self.extended   = f.option("e", "extendednames",
                               description: "Use extended procedure names.")
    self.strict     = f.option("s", "strict",
                               description: "In strict mode, initialization warnings terminate " +
                                            "the application.")
    self.quiet      = f.option("q", "quiet",
                               description: "In quiet mode, optional messages are not printed.")
    self.help       = f.option("h", "help",
                               description: "Show description of usage and options of this tools.")
    // Instantiate the terminal
    self.terminal = CommandLineDelegate()
    // Reset context
    self.context = nil
  }

  // Define prompt/read logic based on `terminal`
  open func readCommand(withPrompt: Bool = true) -> String? {
    if let ln = self.lineReader {
      do {
        return try ln.readLine(prompt: withPrompt ? (self.prompt.value ?? "> ") : "",
                               maxCount: 4000,
                               strippingNewline: true,
                               promptProperties: TextProperties(.blue, nil, .bold),
                               readProperties: TextProperties(.default, nil),
                               parenProperties: TextProperties(.red, nil, .bold))
      } catch LineReaderError.CTRLC {
        self.terminal.print("\nterminated\n")
        return nil
      } catch {
        self.terminal.print("\(error.localizedDescription)\n")
        return nil
      }
    } else {
      if withPrompt {
        self.terminal.print(self.prompt.value ?? "> ")
      }
      return self.terminal.read()
    }
  }

  // Define how optional messages and errors are printed
  open func printOpt(_ message: String) {
    if !self.quiet.wasSet {
      print(message)
    }
  }

  // Define how errors are printed if `issue` is `true`.
  open func printError(if issue: Bool = true, _ message: String) -> Bool {
    if issue {
      self.printOpt(message)
      if self.strict.wasSet {
        return false
      }
    }
    return true
  }

  open func flagsValid() -> Bool {
    // Check if there was a flag parsing error?
    if let failure = self.flags.parsingFailure() {
      print(failure)
      return false
    }
    // Are simplified descriptions requested?
    Context.simplifiedDescriptions = !self.extended.wasSet
    return true
  }
  
  open func shouldRunRepl() -> Bool {
    // If help flag was provided, print usage description and exit tool
    if self.help.wasSet {
      print(self.flags.usageDescription(
              usageName: TextStyle.bold.properties.apply(to: "usage:"),
              synopsis: "[<option> ...] [---] [<program> <arg> ...]",
              usageStyle: TextProperties.none,
              optionsName: TextStyle.bold.properties.apply(to: "options:"),
              flagStyle: TextStyle.italic.properties),
            terminator: "")
      return false
    }
    return true
  }

  open func configurationSuccessfull(implementationName: String? = nil,
                                     implementationVersion: String? = nil,
                                     includeInternalResources: Bool = true,
                                     defaultDocDirectory: String? = nil,
                                     assetPath: String? = nil,
                                     features: [String] = [],
                                     initialLibraries: [String] = []) -> Bool {
    // Determine remaining command-line args
    let cmdLineArgs = self.flags.parameters.isEmpty ? [CommandLine.arguments.first!]
                                                    : self.flags.parameters
    // Create LispKit context
    self.context = LispKitContext(delegate: self.terminal,
                                  implementationName: implementationName,
                                  implementationVersion: implementationVersion,
                                  commandLineArguments: cmdLineArgs,
                                  includeInternalResources: includeInternalResources,
                                  includeDocumentPath: self.searchDocs.value ?? defaultDocDirectory,
                                  assetPath: assetPath,
                                  features: features,
                                  limitStack: self.maxStackSize.value! * 1000)
    // Configure heap capacity
    if let capacity = self.heapSize.value {
      self.context?.heap.reserveCapacity(capacity)
    }
    // By default (no internal resources and no roots provided), keep supplemental files in a
    // `lib` directory one level up the binary location.
    if !includeInternalResources && !self.roots.wasSet {
      let base = URL(fileURLWithPath: CommandLine.arguments[0]).absoluteURL
                                        .deletingLastPathComponent()
                                        .deletingLastPathComponent()
                                        .appendingPathComponent("lib", isDirectory: true)
      guard self.setupBinaryBundle(root: base) else {
        return false
      }
    }
    // Set up remaining file paths
    return self.setupRootPaths(includeInternalResources: includeInternalResources) &&
           self.setupPaths() &&
           self.bootstrapContext() &&
           self.importLibraries(initialLibraries) &&
           self.loadPrelude()
  }

  open func setupBinaryBundle(root: URL) -> Bool {
    _ = self.context?.fileHandler.prependSearchPath(root.path)
    _ = self.context?.fileHandler.prependAssetSearchPath(root
                                   .appendingPathComponent("Assets", isDirectory: true).path)
    _ = self.context?.fileHandler.prependLibrarySearchPath(root
                                   .appendingPathComponent("Libraries", isDirectory: true).path)
    if !self.prelude.wasSet {
      let path = root.appendingPathComponent("Prelude.scm", isDirectory: false).path
      if FileManager.default.fileExists(atPath: path) {
        self.prelude.value = path
      }
    }
    return true
  }

  open func setupRootPaths(includeInternalResources: Bool) -> Bool {
    for root in self.roots.value {
      guard self.setupBinaryBundle(root: URL(fileURLWithPath: root, isDirectory: true)) else {
        return false
      }
    }
    return true
  }

  open func setupPaths() -> Bool {
    if let context = self.context {
      for p in self.filePaths.value {
        guard self.printError(if: !context.fileHandler.addSearchPath(p),
                              "cannot add search path: \(p)") else {
          return false
        }
      }
      for p in self.libPaths.value {
        guard self.printError(if: !context.fileHandler.addLibrarySearchPath(p),
                              "cannot add library path: \(p)") else {
          return false
        }
      }
      for p in self.assetPaths.value {
        guard self.printError(if: !context.fileHandler.addAssetSearchPath(p),
                              "cannot add asset path: \(p)") else {
          return false
        }
      }
    }
    return true
  }

  open func bootstrapContext() -> Bool {
    do {
      try self.context?.bootstrap(forRepl: true)
    } catch let error as RuntimeError {
      print("cannot import core lispkit libraries: \(error.message)")
      return false
    } catch let error as NSError {
      print("cannot import core lispkit libraries: \(error.localizedDescription)")
      return false
    } catch {
      print("cannot import core lispkit libraries")
      return false
    }
    return true
  }

  open func importLibraries(_ initialLibraries: [String]) -> Bool {
    var libs: [String] = initialLibraries
    if self.lispkit.wasSet {
      libs.append("lispkit base")
    } else if self.r7rs.wasSet {
      libs.append("scheme base")
    }
    libs.append(contentsOf: self.importLibs.value)
    for lib in libs {
      // Remove outer parenthesis if needed; this is to allow users to provide a more readable
      // library name (same expression as within the repl)
      var initialLib = lib
      if initialLib.first == "(" && initialLib.last == ")" {
        initialLib.removeFirst()
        initialLib.removeLast()
      }
      var name: [String] = []
      for s in initialLib.split(separator: " ") {
        name.append(String(s))
      }
      do {
        try self.context?.environment.import(name)
      } catch let error as RuntimeError {
        guard self.printError("error importing (\(initialLib)): \(error.message)") else {
          return false
        }
      } catch let error as NSError {
        guard self.printError("error importing (\(initialLib)): \(error.localizedDescription)") else {
          return false
        }
      } catch {
        guard self.printError("error importing (\(initialLib))") else {
          return false
        }
      }
    }
    return true
  }

  open func loadPrelude() -> Bool {
    if let context = self.context,
       let ppath = self.prelude.value ??
                   (self.flags.parameters.isEmpty ? LispKitContext.defaultPreludePath : nil) {
      do {
        _ = try context.evaluator.machine.eval(file: ppath, in: context.global)
      } catch let error as RuntimeError {
        guard self.printError("cannot evaluate prelude \(ppath): \(error.message)") else {
          return false
        }
      } catch let error as NSError {
        guard self.printError("cannot evaluate prelude \(ppath): " +
                              error.localizedDescription) else {
          return false
        }
      } catch {
        guard self.printError("cannot evaluate prelude \(ppath)") else {
          return false
        }
      }
    }
    return true
  }

  open func printResult(_ res: Expr) {
    // For multiple values being returned, print each value on a separate line
    if case .values(let expr) = res {
      var next = expr
      while case .pair(let x, let rest) = next {
        self.terminal.print("\(x.description)\n")
        next = rest
      }
    // For errors print the error message
    } else if case .error(let err) = res {
      if let context = context {
        self.terminal.print("\(err.printableDescription(context: context))\n")
      } else {
        self.terminal.print("\(err.description)\n")
      }
    // For non-void results, print result
    } else if res != .void {
      self.terminal.print("\(res.description)\n")
    }
  }
  
  open func execute(command buffer: String) -> Expr {
    guard let context = self.context else {
      return .false
    }
    return context.evaluator.execute { machine in
      return try machine.eval(str: buffer,
                              sourceId: SourceManager.consoleSourceId,
                              in: context.global, as: "<repl>")
    }
  }
  
  open func execute(file path: String) -> Bool {
    guard let context = self.context else {
      return false
    }
    let currentPath = context.fileHandler.currentDirectoryPath
    let filename = context.fileHandler.filePath(forFile: path, relativeTo: currentPath) ??
                   context.fileHandler.libraryFilePath(forFile: path, relativeTo: currentPath) ??
                   context.fileHandler.path(path, relativeTo: currentPath)
    let res = context.evaluator.execute { machine in
      return try machine.eval(file: filename, in: context.global)
    }
    if context.evaluator.exitTriggered {
      if res != .true {
        print("abnormal exit: \(res.description)\n")
        return false
      }
    } else {
      self.printResult(res)
      if case .error(_) = res {
        return false
      }
    }
    return true
  }

  open func printHeader() {
    let props = Terminal.fullColorSupport ? TextStyle.bold.properties : TextProperties.none
    self.printOpt(props.apply(to: "\(self.toolName) \(self.toolVersion)\(self.toolBuild)"))
    self.printOpt(props.apply(to: "\(self.toolCopyright)"))
  }

  open func runRepl() -> Bool {
    guard let context = self.context else {
      return false
    }
    var buffer = ""
    while let line = self.readCommand(withPrompt: buffer.isEmpty) {
      buffer += line + "\n"
      // Execute the command
      let res = self.execute(command: buffer)
      // Exit loop if the machine has executed the `exit` function
      if context.evaluator.exitTriggered {
        if res != .true {
          print("abnormal exit: \(res.description)\n")
          return false
        }
        break
      // If closing parenthesis are missing, keep on reading
      } else if case .error(let err) = res,
            context.sources.consoleIsSource(sourceId: err.pos.sourceId),
            case .syntax(.closingParenthesisMissing) = err.descriptor {
        continue
      // Else print result
      } else {
        self.printResult(res)
        context.update(withReplResult: res)
      }
      // Store buffer in the history of the line reader
      self.lineReader?.addHistory(
        buffer.trimmingCharacters(in: CharacterSet.whitespacesAndNewlines))
      // Empty buffer
      buffer = ""
    }
    return true
  }

  open func run() -> Bool {
    // Distinguish interactive usage (via REPL) from non-interactive usage
    if let program = flags.parameters.first {
      return self.execute(file: program)
    } else {
      // Print the read-eval-print loop header
      self.printHeader()
      // Enter read-eval-print loop
      return self.runRepl()
    }
  }
  
  open func release() {
    self.context?.release()
  }
}
