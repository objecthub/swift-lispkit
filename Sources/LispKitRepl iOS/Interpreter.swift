//
//  Interpreter.swift
//  LispKitRepl iOS
//
//  Created by Matthias Zenger on 24/04/2021.
//  Copyright © 2021 ObjectHub. All rights reserved.
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

final class Interpreter: ContextDelegate, ObservableObject {
  
  /// Maximum number of console slots (not necessarily rows)
  static let maxConsoleEntries = 1000
  
  /// Reading status of console
  enum ReadingStatus: Equatable, CustomStringConvertible {
    case reject
    case accept
    case read(String)
    
    var description: String {
      switch self {
        case .reject:
          return "reject"
        case .accept:
          return "accept"
        case .read(let str):
          return "read(\(str))"
      }
    }
  }
  
  /// Published state
  @Published var consoleContent: [ConsoleOutput] = []
  @Published var isReady: Bool = false
  @Published var readingStatus: ReadingStatus = .reject
  
  /// The context of the interpreter
  var context: Context? = nil
  
  /// Condition synchronizing changes to the published state variables
  let readingCondition = NSCondition()
  
  /// The processing queue for the interpreter
  private let processingQueue: OperationQueue
  
  init() {
    self.processingQueue = OperationQueue()
    self.processingQueue.qualityOfService = .userInitiated
    self.processingQueue.maxConcurrentOperationCount = 1
    self.processingQueue.addOperation(self.initialize)
  }
  
  func append(output: ConsoleOutput) {
    if self.consoleContent.count >= Interpreter.maxConsoleEntries {
      self.consoleContent.removeFirst()
    }
    if let last = self.consoleContent.last,
       case .output = last.kind,
       last.text.last == "\n" {
      self.consoleContent[self.consoleContent.count - 1].text = String(last.text.dropLast())
    }
    self.consoleContent.append(output)
  }
  
  func reset() -> Bool {
    guard self.isReady else {
      return false
    }
    self.context = nil
    self.processingQueue.addOperation(self.initialize)
    return true
  }
  
  var isInitialized: Bool {
    return self.context != nil
  }
  
  func evaluate(_ command: String, reset: @escaping () -> Void) {
    self.readingCondition.lock()
    defer {
      self.readingCondition.signal()
      self.readingCondition.unlock()
    }
    guard self.isReady else {
      if self.readingStatus == .accept {
        self.readingStatus = .read(command)
        self.printInternal(command + "\n")
      }
      return
    }
    self.isReady = false
    self.readingStatus = .reject
    self.processingQueue.addOperation { [weak self] in
      let res = self?.execute { context in
        try context.machine.eval(str: command,
                                 sourceId: SourceManager.consoleSourceId,
                                 in: context.global, as: "<repl>")
      }
      DispatchQueue.main.sync {
        self?.isReady = true
        self?.readingStatus = .accept
        if let res = res {
          if res.kind == .result, res.text.isEmpty {
            // do nothing
          } else {
            self?.append(output: res)
          }
        } else {
          reset()
        }
      }
    }
  }
  
  private func initialize() {
    self.context = nil
    DispatchQueue.main.sync {
      self.isReady = false
      self.readingStatus = .reject
    }
    let context = LispKitContext(delegate: self,
                                 initialHomePath: PortableURL.Base.documents.url?.path,
                                 features: ["lispkitrepl"])
    // Setup search paths
    if let librariesPath = PortableURL.Base.documents.url?.appendingPathComponent("Libraries/").path {
      _ = context.fileHandler.prependLibrarySearchPath(librariesPath)
    }
    if let assetsPath = PortableURL.Base.documents.url?.appendingPathComponent("Assets/").path {
      _ = context.fileHandler.prependAssetSearchPath(assetsPath)
    }
    if let homePath = PortableURL.Base.documents.url?.path {
      _ = context.fileHandler.addSearchPath(homePath)
    }
    // Bootstrap context
    do {
      try context.bootstrap(forRepl: true)
    } catch {
      preconditionFailure("cannot import required LispKit libraries")
    }
    // Evaluate prelude
    self.context = context
    do {
      _ = try context.machine.eval(file: Context.defaultPreludePath,
                                   in: context.global,
                                   as: "<prelude>")
    } catch let error {
      DispatchQueue.main.sync {
        self.append(output: ConsoleOutput(kind: .error("init"), text: error.localizedDescription))
        self.isReady = true
        self.readingStatus = .accept
      }
      return
    }
    // The interpreter is ready now
    DispatchQueue.main.sync {
      self.isReady = true
      self.readingStatus = .accept
    }
  }
  
  private func execute(action: (Context) throws -> Expr) -> ConsoleOutput? {
    guard let context = self.context else {
      return nil
    }
    let res = context.machine.onTopLevelDo {
      try action(context)
    }
    if context.machine.exitTriggered {
      // Check if we should close this interpreter
    }
    switch res {
      case .error(let err):
        if case .syntax(let error) = err.descriptor,
           context.sources.consoleIsSource(sourceId: err.pos.sourceId),
           error == .closingParenthesisMissing || error == .unexpectedClosingParenthesis {
          return nil
        } else {
          return ConsoleOutput(kind: .error(self.errorLocation(err, in: context)),
                               text: self.errorMessage(err, in: context))
        }
      case .void:
        return ConsoleOutput(kind: .result, text: "")
      case .values(let expr):
        var message = ""
        var next = expr
        while case .pair(let x, let rest) = next {
          if message.isEmpty {
            message = x.description
          } else {
            message += "\n"
            message += x.description
          }
          next = rest
        }
        context.update(withReplResult: res)
        return ConsoleOutput(kind: .result, text: message)
      default:
        context.update(withReplResult: res)
        return ConsoleOutput(kind: .result, text: res.description)
    }
  }
  
  private func errorMessage(_ err: RuntimeError, in context: Context) -> String {
    return err.printableDescription(context: context,
                                    typeOpen: "〚",
                                    typeClose: "〛 ",
                                    irritantHeader: "\n     • ",
                                    irritantSeparator: "\n     • ",
                                    positionHeader: nil,
                                    libraryHeader: nil,
                                    stackTraceHeader: nil)
  }
  
  private func errorLocation(_ err: RuntimeError, in context: Context) -> String? {
    guard let stackTrace = err.stackTrace, stackTrace.count > 0 else {
      guard let libraryName = err.library?.description else {
        if !err.pos.isUnknown {
          if let filename = context.sources.sourcePath(for: err.pos.sourceId) {
            return " └─ at: \(err.pos.description):\(filename)"
          } else {
            return " └─ at: \(err.pos.description)"
          }
        }
        return nil
      }
      if !err.pos.isUnknown {
        if let filename = context.sources.sourcePath(for: err.pos.sourceId) {
          return " │  at: \(err.pos.description):\(filename)\n"
        } else {
          return " │  at: \(err.pos.description)\n"
        }
      }
      return " └─ library: \(libraryName)"
    }
    var res = ""
    if !err.pos.isUnknown {
      if let filename = context.sources.sourcePath(for: err.pos.sourceId) {
        res += " │  at: \(err.pos.description):\(filename)\n"
      } else {
        res += " │  at: \(err.pos.description)\n"
      }
    }
    if let libraryName = err.library?.description {
      res += " │  library: \(libraryName)\n"
    }
    res += " └── "
    var sep = ""
    for proc in stackTrace {
      res += sep
      res += proc.name
      sep = " « "
    }
    return res
  }
  
  /// Prints the given string into the console window.
  func print(_ str: String) {
    Swift.print(str, separator: "", terminator: "")
    DispatchQueue.main.async {
      self.printInternal(str)
    }
  }
  
  private func printInternal(_ str: String) {
    if self.consoleContent.isEmpty {
      self.append(output: ConsoleOutput(kind: .output, text: str))
    } else if let last = self.consoleContent.last,
              last.kind == .output {
      if last.text.count < 1000 {
        self.consoleContent[self.consoleContent.count - 1].text += str
      } else if str.first == "\n" {
        self.append(output: ConsoleOutput(kind: .output, text: String(str.dropFirst())))
      } else if last.text.last == "\n" {
        let str = String(self.consoleContent[self.consoleContent.count - 1].text.dropLast())
        self.consoleContent[self.consoleContent.count - 1].text = str
        self.append(output: ConsoleOutput(kind: .output, text: str))
      } else {
        self.append(output: ConsoleOutput(kind: .output, text: str))
      }
    } else {
      self.append(output: ConsoleOutput(kind: .output, text: str))
    }
  }
  
  /// Reads a string from the console window.
  func read() -> String? {
    DispatchQueue.main.sync {
      self.readingStatus = .accept
    }
    self.readingCondition.lock()
    defer {
      self.readingCondition.signal()
      self.readingCondition.unlock()
    }
    // Wait for self.readingStatus turning into .read(...)
    while !self.isReady && self.readingStatus == .accept {
      self.readingCondition.wait()
    }
    if case .read(let text) = self.readingStatus {
      DispatchQueue.main.sync {
        self.readingStatus = .reject
      }
      return text + "\n"
    } else {
      DispatchQueue.main.sync {
        self.readingStatus = .reject
      }
      return nil
    }
  }
  
  /// This is called whenever a new library is loaded
  func loaded(library lib: Library, by: LispKit.LibraryManager) {
  }
  
  /// This is called whenever a symbol is bound in an environment
  func bound(symbol: Symbol, in: Environment) {
  }

  /// This is called whenever garbage collection was called
  func garbageCollected(objectPool: ManagedObjectPool, time: Double, objectsBefore: Int) {
  }

  /// This is called when the execution of the virtual machine got aborted.
  func aborted() {
  }
  
  /// This is called by the `exit` function of LispKit.
  func emergencyExit(obj: Expr?) {
  }
}
