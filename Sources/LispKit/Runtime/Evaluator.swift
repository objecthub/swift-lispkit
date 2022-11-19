//
//  Evaluator.swift
//  LispKit
//
//  Created by Matthias Zenger on 21/12/2021.
//  Copyright © 2021-2022 ObjectHub. All rights reserved.
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
/// Class `Evaluator` implements an API for 
/// 
/// 
public final class Evaluator: TrackedObject {
  
  /// Call tracing modes
  public enum CallTracingMode {
    case on
    case off
    case byProc
  }
  
  /// The context of this virtual machine
  private unowned let context: Context
  
  /// Procedures defined in low-level libraries
  public internal(set) var raiseProc: Procedure? = nil
  public internal(set) var raiseContinuableProc: Procedure? = nil
  public internal(set) var loader: Procedure? = nil
  public internal(set) var defineSpecial: SpecialForm? = nil
  public internal(set) var defineValuesSpecial: SpecialForm? = nil
  public private(set) var setParameterProc: Procedure!
  public private(set) var currentDirectoryProc: Procedure!
  
  /// Will be set to true if the `exit` function was invoked
  public internal(set) var exitTriggered: Bool = false
  
  /// When set to true, will print call and return traces
  public var traceCalls: CallTracingMode = .off
  
  /// Call stack cap
  public var maxCallStack: Int = 20
  
  /// The main virtual machine
  private let main: VirtualMachine
  
  /// The main evaluation thread object (it's just a proxy for the thread executing an expression)
  private var mainThread: NativeThread! = nil
  
  /// All managed threads
  internal var threads = ThreadManager()
  
  init(for context: Context, limitStack: Int = 10000000) {
    self.context = context
    self.main = VirtualMachine(for: context, limitStack: limitStack)
    context.objects.manage(self.main)
    super.init()
    self.mainThread = NativeThread(EvalThread(worker: self))
    self.setParameterProc = Procedure("_set-parameter", self.setParameter, nil)
    self.currentDirectoryProc =
      Procedure(.procedure(Procedure("_valid-current-path", self.validCurrentPath)),
                .makeString(context.initialHomePath ??
                            context.fileHandler.currentDirectoryPath))
  }
  
  /// Returns the virtual machine owned by the active evaluation thread (or the main thread if
  /// there is currently no active managed evaluation thread)
  public var machine: VirtualMachine {
    return self.threads.current?.value.machine ?? self.main
  }
  
  // Evaluation of code
  
  /// Executes an evaluation function at the top-level, i.e. in the main virtual machine
  public func execute(_ eval: (VirtualMachine) throws -> Expr) -> Expr {
    self.mainThread.value.mutex.lock()
    guard self.machine === self.main else {
      self.mainThread.value.mutex.unlock()
      preconditionFailure("executing source code not on main thread")
    }
    self.threads.register(thread: Thread.current, as: self.mainThread)
    self.exitTriggered = false
    self.mainThread.value.mutex.unlock()
    var result = self.main.onTopLevelDo {
      return try eval(self.main)
    }
    self.mainThread.value.mutex.lock()
    if case .error(let err) = result, err.descriptor == .uncaught {
      result = err.irritants[0]
    }
    self.mainThread.value.abandonLocks()
    self.mainThread.value.mutex.unlock()
    // Terminate all threads
    self.threads.abortAll(except: Thread.current)
    if !self.threads.waitForTerminationOfAll(timeout: 1.0) {
      // Not sure why the following line is sometimes needed...
      self.threads.abortAll(except: Thread.current)
      _ = self.threads.waitForTerminationOfAll(timeout: 0.5)
    }
    self.threads.remove(thread: Thread.current)
    return result
  }
  
  /// Aborts the currently running execution
  public func abort() {
    _ = self.mainThread.value.abort()
  }
  
  // Create evaluation threads
  
  public func thread(for proc: Procedure, name: Expr? = nil, tag: Expr? = nil) -> NativeThread {
    return NativeThread(EvalThread(parent: self.machine,
                                   procedure: proc,
                                   name: name ?? .false,
                                   tag: tag ?? .false))
  }
  
  // Abortion of evaluation
  
  /// Returns true if an abortion was requested.
  public func isAbortionRequested() -> Bool {
    return self.main.abortionRequested
  }
  
  // Parsing of expressions
  
  /// Parses the given file and returns a list of parsed expressions.
  public func parse(file path: String, foldCase: Bool = false) throws -> Expr {
    let (sourceId, text) = try self.context.sources.readSource(for: path)
    return try self.parse(str: text, sourceId: sourceId, foldCase: foldCase)
  }
  
  /// Parses the given string and returns a list of parsed expressions.
  public func parse(str: String, sourceId: UInt16, foldCase: Bool = false) throws -> Expr {
    return .makeList(try self.parseExprs(str: str, sourceId: sourceId, foldCase: foldCase))
  }
  
  /// Parses the given string and returns an array of parsed expressions.
  public func parseExprs(file path: String, foldCase: Bool = false) throws -> Exprs {
    let (sourceId, text) = try self.context.sources.readSource(for: path)
    return try self.parseExprs(str: text,
                               sourceId: sourceId,
                               foldCase: foldCase)
  }
  
  /// Parses the given string and returns an array of parsed expressions.
  public func parseExprs(str: String, sourceId: UInt16, foldCase: Bool = false) throws -> Exprs {
    let input = TextInput(string: str,
                          abortionCallback: self.context.evaluator.isAbortionRequested)
    let parser = Parser(symbols: self.context.symbols,
                        input: input,
                        sourceId: sourceId,
                        foldCase: foldCase)
    var exprs = Exprs()
    while !parser.finished {
      exprs.append(try parser.parse().datum) // TODO: remove .datum
    }
    return exprs
  }
  
  // Parameter handling
  
  private var parameters: HashTable {
    return self.machine.parameters
  }
  
  public func getParam(_ param: Procedure) -> Expr? {
    return self.getParameter(.procedure(param))
  }
  
  public func getParameter(_ param: Expr) -> Expr? {
    guard case .some(.pair(_, .box(let cell))) = self.parameters.get(param) else {
      guard case .procedure(let proc) = param,
            case .parameter(let tuple) = proc.kind else {
        return nil
      }
      return tuple.snd
    }
    return cell.value
  }
  
  public func setParam(_ param: Procedure, to value: Expr) -> Expr {
    return self.setParameter(.procedure(param), to: value)
  }
  
  public func setParameter(_ param: Expr, to value: Expr) -> Expr {
    guard case .some(.pair(_, .box(let cell))) = self.parameters.get(param) else {
      guard case .procedure(let proc) = param,
            case .parameter(let tuple) = proc.kind else {
        preconditionFailure("cannot set parameter \(param)")
      }
      tuple.snd = value
      return .void
    }
    cell.value = value
    return .void
  }
  
  internal func bindParameter(_ param: Expr, to value: Expr) -> Expr {
    self.parameters.add(key: param, mapsTo: .box(Cell(value)))
    return .void
  }
  
  public var currentDirectoryPath: String {
    get {
      do {
        return try self.getParam(self.currentDirectoryProc)!.asString()
      } catch {
        preconditionFailure("current directory path not a string")
      }
    }
    set {
      _ = self.setParam(self.currentDirectoryProc, to: .makeString(newValue))
    }
  }
  
  private func validCurrentPath(param: Expr, expr: Expr, setter: Expr) throws -> Expr {
    self.currentDirectoryPath =
      self.context.fileHandler.path(try expr.asPath(), relativeTo: self.currentDirectoryPath)
    return .makeString(self.currentDirectoryPath)
  }
  
  // Tracked object protocol
  
  /// Mark evaluator
  public override func mark(in gc: GarbageCollector) {
    self.main.mark(in: gc)
    self.mainThread.mark(in: gc)
    self.threads.mark(in: gc)
    if let proc = self.raiseProc {
      gc.mark(proc)
    }
    if let proc = self.raiseContinuableProc {
      gc.mark(proc)
    }
  }
  
  /// Reset evaluator
  public func release() {
    self.threads.releaseAll()
    self.main.clean()
    self.mainThread = nil
    self.raiseProc = nil
    self.raiseContinuableProc = nil
    self.loader = nil
    self.defineSpecial = nil
    self.defineValuesSpecial = nil
    self.setParameterProc = nil
    self.exitTriggered = false
    self.traceCalls = .off
  }
}

extension Evaluator: EvalThreadWorker {
  
  public var evalMachine: VirtualMachine {
    return self.main
  }
  
  public func start() {
    // nothing to do here
  }
  
  public func stop() -> Thread? {
    if self.main.executing {
      self.main.requestAbortion()
      self.context.delegate?.aborted()
    }
    return nil
  }
  
  public func guaranteeSecondary() throws {
    throw RuntimeError.eval(.joinWithMainThread)
  }
  
  public func markDelegate(with gc: GarbageCollector) {
    // nothing to do here
  }
  
  public func releaseDelegate() {
    // nothing to do here
  }
}
