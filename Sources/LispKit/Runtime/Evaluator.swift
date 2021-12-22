//
//  Evaluator.swift
//  LispKit
//
//  Created by Matthias Zenger on 21/12/2021.
//  Copyright © 2021 ObjectHub. All rights reserved.
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
  
  /// Threads for evaluating thunks concurrently
  public final class MachineThread: Thread {
    public let machine: VirtualMachine
    private var procedure: Procedure?
    public var result: Expr?
    public var error: Error?
    
    fileprivate init(parent machine: VirtualMachine, procedure: Procedure) {
      self.machine = VirtualMachine(parent: machine)
      self.procedure = procedure
      self.result = nil
      self.error = nil
      super.init()
      self.stackSize = 4 * 256 * 4096 // 4 MByte stack size
      self.qualityOfService = .userInitiated
    }
    
    public var isInitial: Bool {
      return self.result == nil &&
             self.error == nil &&
             !self.isActive
    }
    
    public var isActive: Bool {
      return self.machine.context.evaluator.activeThreads.contains(self)
    }
    
    public var hasCompleted: Bool {
      return self.result != nil || self.error != nil
    }
    
    override public func main() {
      guard let proc = self.procedure else {
        return
      }
      do {
        self.machine.context.evaluator.activeThreads.insert(self)
        defer {
          self.machine.context.evaluator.activeThreads.remove(self)
        }
        self.result = try machine.apply(.procedure(proc), to: .null)
      } catch let err {
        self.error = err
      }
    }
    
    public func mark(in gc: GarbageCollector) {
      self.machine.mark(in: gc)
      if let proc = self.procedure {
        gc.mark(proc)
      }
      if let result = self.result {
        gc.markLater(result)
      }
      // TODO: garbage collect self.error?
    }
    
    public func release() {
      self.machine.release()
      self.procedure = nil
      self.result = nil
      self.error = nil
    }
  }
  
  /// The context of this virtual machine
  private unowned let context: Context
  
  /// Procedures defined in low-level libraries
  public internal(set) var raiseProc: Procedure? = nil
  public internal(set) var loader: Procedure? = nil
  public internal(set) var defineSpecial: SpecialForm? = nil
  public internal(set) var defineValuesSpecial: SpecialForm? = nil
  public private(set) var setParameterProc: Procedure!
  public private(set) var currentDirectoryProc: Procedure!
  
  /// Will be set to true if the `exit` function was invoked
  public internal(set) var exitTriggered: Bool = false
  
  /// When set to true, it will trigger an abortion of the evaluator as soon as possible
  private var abortionRequested: Bool = false
  
    /// When set to true, will print call and return traces
  public var traceCalls: CallTracingMode = .off
  
  /// The main virtual machine
  private let main: VirtualMachine
  
  /// Active threads
  private var activeThreads: Set<MachineThread> = []
  
  init(for context: Context) {
    self.context = context
    self.main = VirtualMachine(for: context)
    super.init()
    self.setParameterProc = Procedure("_set-parameter", self.setParameter, nil)
    self.currentDirectoryProc =
      Procedure(.procedure(Procedure("_valid-current-path", self.validCurrentPath)),
                .makeString(context.initialHomePath ??
                            context.fileHandler.currentDirectoryPath))
  }
  
  /// Returns the virtual machine owned by the active machine thread (or the main thread if
  /// there is currently no active machine thread)
  public var machine: VirtualMachine {
    if activeThreads.count == 0 {
      return self.main
    } else if let machineThread = Thread.current as? MachineThread {
      return machineThread.machine
    } else {
      return self.main
    }
  }
  
  // Evaluation of code
  
  /// Executes an evaluation function at the top-level, i.e. in the main virtual machine
  public func execute(_ eval: (VirtualMachine) throws -> Expr) -> Expr {
    guard self.machine === self.main else {
      preconditionFailure("executing source code not on main thread")
    }
    for thread in self.activeThreads {
      thread.machine.requestAbortion()
    }
    self.exitTriggered = false
    self.abortionRequested = false
    self.activeThreads.removeAll()
    return self.main.onTopLevelDo {
      return try eval(self.main)
    }
  }
  
  // Create evaluation threads
  
  public func thread(for proc: Procedure) -> MachineThread {
    return MachineThread(parent: self.machine, procedure: proc)
  }
  
  // Abortion of evaluation
  
  /// Requests abortion of the machine evaluator.
  public func abort() {
    if self.main.executing {
      self.abortionRequested = true
      self.context.delegate?.aborted()
      self.main.requestAbortion()
      for thread in self.activeThreads {
        thread.machine.requestAbortion()
      }
    }
  }
  
  /// Returns true if an abortion was requested.
  public func isAbortionRequested() -> Bool {
    return self.abortionRequested
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
    for thread in self.activeThreads {
      thread.mark(in: gc)
    }
  }
  
  /// Reset evaluator
  public func release() {
    for thread in self.activeThreads {
      thread.cancel()
      thread.release()
    }
    self.activeThreads.removeAll()
    self.main.release()
    self.raiseProc = nil
    self.loader = nil
    self.defineSpecial = nil
    self.defineValuesSpecial = nil
    self.setParameterProc = nil
    self.raiseProc = nil
    self.exitTriggered = false
    self.traceCalls = .off
  }
}
