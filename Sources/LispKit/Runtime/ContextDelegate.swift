//
//  ContextDelegate.swift
//  LispKit
//
//  Created by Matthias Zenger on 29/12/2016.
//  Copyright © 2016-2022 ObjectHub. All rights reserved.
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

#if os(iOS) || os(watchOS) || os(tvOS)
import UIKit
#elseif os(macOS)
import Cocoa
#endif

///
/// `ContextDelegate` provides functionality for customizing the behavior of the
/// virtual machine. The most important use case is the interaction with the console window.
/// For this use case, `ContextDelegate` provides methods to set the console window status,
/// to write strings as well as to read strings.
///
public protocol ContextDelegate: AnyObject, TextInputSource, TextOutputTarget {
  
  /// Prints the given string into the console window.
  func print(_ str: String)
  
  /// Reads a string from the console window.
  func read() -> String?
  
  /// Is called whenever a procedure that is being traced is called
  func trace(call: Procedure,
             args: Exprs,
             tailCall: Bool,
             in: VirtualMachine)
  
  /// Is called whenever a procedure that is being traced returns
  func trace(return: Procedure,
             result: Expr,
             tailCall: Bool,
             in: VirtualMachine)
  
  /// This is called whenever a new library is loaded
  func loaded(library: Library, by: LibraryManager)
  
  /// This is called whenever a symbol is bound in an environment
  func bound(symbol: Symbol, in: Environment)

  /// This is called whenever garbage collection was called
  func garbageCollected(objectPool: ManagedObjectPool, time: Double, objectsBefore: Int)

  /// This is called when the execution of the virtual machine got aborted.
  func aborted()
  
  /// This is called by the `exit` function of LispKit.
  func emergencyExit(obj: Expr?)
}

///
/// Default implementations for context delegates. In particular, these defaults implement
/// the `TextInputSource` and `TextOutputTarget` functionality of the delegate via methods
/// `print` and `read`.
///
public extension ContextDelegate {
  
  /// The console always blocks before providing a new string.
  var nextReadMightBlock: Bool {
    return true
  }
  
  /// Read the next string from the console. This operation always blocks.
  func readString() -> String? {
    return self.read()
  }
  
  /// `flush` always succeeds.
  func flush(_ completely: Bool = false) -> Bool {
    return true
  }
  
  /// Print the given string to the console.
  func writeString(_ str: String) -> Bool {
    self.print(str)
    return true
  }
  
  func countTracedProcedures(_ callStack: [Procedure]) -> Int {
    var num = 0
    for proc in callStack {
      if proc.traced {
        num += 1
      }
    }
    return num
  }
  
  func trace(call proc: Procedure,
                    args: Exprs,
                    tailCall: Bool,
                    in machine: VirtualMachine) {
    var builder = StringBuilder()
    var offset = tailCall ? 0 : 1
    let callStack = machine.getStackTrace()
    if machine.context.evaluator.traceCalls == .byProc {
      offset += self.countTracedProcedures(callStack)
    } else {
      offset += callStack.count
    }
    builder.append(tailCall ? "↪︎" : "⟶", width: offset * 2 + 1, alignRight: true)
    builder.append(" (", proc.originalName ?? proc.name)
    for arg in args {
      builder.append(" ", arg.description)
    }
    builder.append(")")
    if let currentProc = callStack.first {
      builder.append(" in ", currentProc.originalName ?? currentProc.name)
    }
    builder.append("\n")
    self.print(builder.description)
  }
  
  func trace(return proc: Procedure,
                    result: Expr,
                    tailCall: Bool,
                    in machine: VirtualMachine) {
    var builder = StringBuilder()
    var offset = tailCall ? 0 : 1
    let callStack = machine.getStackTrace()
    if machine.context.evaluator.traceCalls == .byProc {
      offset += self.countTracedProcedures(callStack)
    } else {
      offset += callStack.count
    }
    builder.append("⟵", width: offset * 2 + 1, alignRight: true)
    builder.append(" ", result.description)
    builder.append(" from ", proc.originalName ?? proc.name)
    if callStack.count > 1 {
      let currentProc = callStack[1]
      builder.append(" in ", currentProc.originalName ?? currentProc.name)
    }
    builder.append("\n")
    self.print(builder.description)
  }
  
  func loaded(library: Library, by: LibraryManager) {
  }
  
  func bound(symbol: Symbol, in: Environment) {
  }

  func garbageCollected(objectPool: ManagedObjectPool, time: Double, objectsBefore: Int) {
  }

  func aborted() {
  }
  
  func emergencyExit(obj: Expr?) {
    #if os(macOS)
    NSApplication.shared.terminate(self)
    #endif
  }
}

///
/// Struct `CommandLineDelegate` implements `ContextDelegate` with Swift's built in `print` and
/// `readLine` functions to allow for a simple read-eval-print loop on a terminal interface.
///
public class CommandLineDelegate: ContextDelegate {
  private let lock: EmbeddedUnfairLock
  
  public init() {
    self.lock = EmbeddedUnfairLock()
  }
  
  deinit {
    self.lock.release()
  }
  
  /// Prints the given string into the console window.
  public func print(_ str: String) {
    self.lock.lock()
    Swift.print(str, separator: "", terminator: "")
    self.lock.unlock()
  }
  
  /// Reads a string from the console window.
  public func read() -> String? {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    return Swift.readLine(strippingNewline: false)
  }
}
