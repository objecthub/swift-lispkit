//
//  EvalThread.swift
//  LispKit
//
//  Created by Matthias Zenger on 23/12/2021.
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
import Atomics

///
/// `NativeThread` implements the `NativeObject` protocol, wrapping an `EvalThread`
/// object (which itself implements the `ManagedObject` protocol). This approach allows
/// for the reuse of native objects for threads, while making them managed (as they can
/// introduce cyclic dependencies via their `result` property).
/// 
public final class NativeThread: AnyNativeObject<EvalThread> {
  
  /// Type representing threads
  public static let type = Type.objectType(Symbol(uninterned: "thread"))
  
  /// Returns the type of this native object.
  public override var type: Type {
    return Self.type
  }
  
  /// Returns a string representation of this native object.
  public override var string: String {
    let nameStr = self.value.name.isFalse ? self.identityString : self.value.name.description
    return "#<thread \(nameStr): \(self.value.state)>"
  }
  
  /// Unpack this native object.
  public override func unpack(in context: Context) -> Exprs {
    let nameStr = self.value.name.isFalse ? self.identityString : self.value.name.description
    return [.makeString(self.identityString),
            .makeString(nameStr),
            .makeNumber(self.value.state.rawValue),
            self.value.threadTag]
  }
  
  /// Implements the mark phase of the garbage collector for this native object.
  public override func mark(in gc: GarbageCollector) {
    self.value.mark(in: gc)
  }
}

/// 
/// Class `EvalThreads` implements first-class threads for evaluating thunks concurrently.
/// The threading model and thread interface is based on SRFI 18. Each `EvalThread` object
/// has the following properties:
/// 
///   - A name (an arbitrary expression identifying the tread)
///   - A fixed tag (defined at thread creation time)
///   - A state (using the SRFI 18 thread state model)
///   - A result (which is optional and only provided for threads that are terminated)
///   - Owned locks and objects a thread is blocked on
///   - A worker thread object representing the virtual machine and the code to execute.
///     It gets generated from a configuration object at thread start time.
/// 
/// A "running" thread is a thread that is currently executing. There can be more than one
/// running thread. A "runnable" thread is a thread that is ready to execute or running.
/// A thread is "blocked" if it is waiting for a mutex to become unlocked, the end of a
/// "sleep" period, etc. A "new" thread is a thread that has not yet become runnable.
/// A new thread becomes runnable when it is started. A "terminated" thread is a thread
/// that can no longer become runnable (but "deadlocked" threads are not considered
/// terminated).
/// 
public final class EvalThread: ManagedObject, ThreadBlocker, CustomStringConvertible {
  
  /// EvalThreads track their state explicitly. State changes happen according to the
  /// following state transition diagram:
  /// 
  ///                         unblock
  ///       start            <-------
  ///  NEW -------> RUNNABLE -------> BLOCKED
  ///    \             |      block  /
  ///     \            v            /
  ///      +-----> TERMINATED <----+
  /// 
  public enum State: Int, CustomStringConvertible {
    case new
    case runnable
    case blocked
    case terminated
    
    public var description: String {
      switch self {
        case .new:
          return "new"
        case .runnable:
          return "runnable"
        case .blocked:
          return "blocked"
        case .terminated:
          return "terminated"
      }
    }
  }
  
  /// Maximum number of user-created threads (across all contexts)
  public static let maxThreads = Sysctl.maxThreads == nil
                               ? 256
                               : (Sysctl.maxThreads! - (Sysctl.maxThreads! > 1000 ? 500 : 100))
  
  /// Atomic counter for counting the number of allocated threads
  public static let allocated = ManagedAtomic<Int>(0)
  
  /// The name of the thread.
  public let name: Expr
  
  /// The tag of the thread.
  public let threadTag: Expr
  
  /// Mutex for synchronizing thread state changes.
  internal let mutex = EmbeddedConditionLock(recursive: true)
  
  /// The state of the thread.
  public internal(set) var state: EvalThread.State
  
  /// The result of the thread execution.
  fileprivate var result: Expr? = nil
  
  /// A set of locks owned by the thread during the execution.
  fileprivate var locks: Set<EvalMutex> = []
  
  /// Mutex this thread is blocked by.
  internal var waitingOn: ThreadBlocker? = nil {
    didSet {
      if self.state == .runnable && self.waitingOn != nil {
        self.state = .blocked
      } else if self.state == .blocked && self.waitingOn == nil {
        self.state = .runnable
      }
    }
  }
  
  /// A native operation is currently running which can be aborted via `blocker`
  private var blocker: Abortable? = nil
  
  /// The components needed to create a worker thread.
  private var config: (VirtualMachine, Int, QualityOfService, Procedure)?
  
  /// The delegate object for performing the concurrent evaluation.
  private weak var worker: EvalThreadWorker?
  
  /// Initializer for representing main (= primary) threads.
  internal init(worker: EvalThreadWorker) {
    self.name = .symbol(Symbol(uninterned: "main"))
    self.threadTag = .box(Cell(.false))
    self.state = .runnable
    self.config = nil
    self.worker = worker
    EvalThread.allocated.wrappingIncrement(ordering: .relaxed)
  }
  
  /// Initializer for representing secondary threads.
  internal init(parent machine: VirtualMachine,
                stacksize: Int = 2_097_152,  // 2 MByte stack size
                qos: QualityOfService = .default, 
                procedure proc: Procedure,
                name: Expr,
                tag: Expr) {
    self.name = name
    self.threadTag = tag
    self.state = .new
    self.config = (machine, stacksize, qos, proc)
    self.worker = nil
    EvalThread.allocated.wrappingIncrement(ordering: .relaxed)
  }
  
  /// Deallocate the mutex
  deinit {
    self.mutex.release()
    EvalThread.allocated.wrappingDecrement(ordering: .relaxed)
  }
  
  /// The virtual machine executing this thread (if available).
  internal var machine: VirtualMachine? {
    return self.worker?.evalMachine
  }
  
  /// Has this thread been aborted?
  internal var aborted: Bool {
    guard let worker = self.worker else {
      if case .some(.error(let err)) = self.result {
        switch err.descriptor {
          case .abortion:
            return true
          case .eval(.threadTerminated):
            return true
          default:
            return false
        }
      }
      return false
    }
    return worker.evalMachine.abortionRequested
  }
  
  /// Starts the thread. This operation throws an exception if the state of this thread is
  /// different from "new".
  public func start() throws {
    self.mutex.lock()
    defer {
      self.mutex.unlock()
    }
    // Only new threads can be started
    guard self.state == .new,
          let (machine, stacksize, qos, proc) = self.config else {
      throw RuntimeError.eval(.attemptedToStartNonNewThread, .object(NativeThread(self)))
    }
    // Limit number of runnable threads
    guard machine.context.evaluator.threads.count < EvalThread.maxThreads else {
      throw RuntimeError.eval(.tooManyThreads)
    }
    // Create worker thread
    let mt = MachineThread(thread: self, parent: machine, procedure: proc)
    if name.isTrue {
      let str = name.description
      mt.name = str.count > 19 ? str.prefix(19) + "…" : str
    }
    mt.stackSize = stacksize
    mt.qualityOfService = qos
    // Update thread state
    self.worker = mt
    self.config = nil
    self.state = .runnable
    // Register worker to not loose the object (due to the weak reference)
    machine.context.evaluator.threads.register(thread: mt, as: NativeThread(self))
    // Start worker
    mt.start()
  }
  
  /// Aborts the current thread. This aborts the running virtual machine and will result
  /// in the thread being terminated with a "terminated thread exception".
  public func abort() -> Thread? {
    self.mutex.lock()
    let res = self.worker?.stop()
    let waitingOn = self.waitingOn
    let blocker = self.blocker
    self.mutex.unlock()
    waitingOn?.wakeBlockedThreads()
    blocker?.abort()
    return res
  }
  
  /// Makes the thread sleep. This must be called only on thread objects that are the current
  /// thread.
  public func sleep(_ timeout: TimeInterval) throws {
    let deadline: Double = Timer.currentTimeInSec + timeout
    self.mutex.lock()
    defer {
      self.mutex.unlock()
    }
    self.waitingOn = self
    while !(self.machine?.abortionRequested ?? true) {
      if !self.mutex.wait(until: deadline) {
        self.waitingOn = nil
        return
      }
    }
    self.waitingOn = nil
    throw RuntimeError.abortion()
  }
  
  /// Returns the result of this thread execution. This is a blocking operation, waiting until
  /// the thread terminates or the timeout expires. If the thread unblocks due to a timeout,
  /// `nil` is being returned. If the thread terminates by raising an exception, the exception
  /// is packed up as an "uncaught exception" and returned by `getResult` as a result.
  public func getResult(in thread: EvalThread, _ timeout: TimeInterval? = nil) throws -> Expr? {
    let deadline: Double? = timeout == nil ? nil : Timer.currentTimeInSec + timeout!
    self.mutex.lock()
    defer {
      self.mutex.unlock()
    }
    try self.worker?.guaranteeSecondary()
    guard thread !== self, thread.state == .runnable else {
      throw RuntimeError.eval(.joinWithItself, .object(NativeThread(self)))
    }
    thread.waitingOn = self
    do {
      defer {
        thread.waitingOn = nil
      }
      while self.result == nil {
        if thread.machine?.abortionRequested ?? true {
          throw RuntimeError.abortion()
        }
        if let deadline = deadline {
          if !self.mutex.wait(until: deadline) {
            return nil
          }
        } else {
          self.mutex.wait()
        }
      }
    }
    if case .some(.error(let err)) = self.result {
      switch err.descriptor {
        case .uncaught, .eval(.threadTerminated):
          throw err
        default:
          return self.result
      }
    } else {
      return self.result
    }
  }
  
  /// Let this thread own the given mutex.
  func claimLock(_ mutex: EvalMutex) -> Bool {
    self.mutex.lock()
    defer {
      self.mutex.unlock()
    }
    guard self.state != .terminated else {
      return false
    }
    self.locks.insert(mutex)
    return true
  }
  
  /// Let this thread give up ownership of the given mutex.
  func unclaimLock(_ mutex: EvalMutex) {
    self.mutex.lock()
    defer {
      self.mutex.unlock()
    }
    self.locks.remove(mutex)
  }
  
  /// Abandon all locks.
  public func abandonLocks() {
    for mutex in self.locks {
      mutex.abandon()
    }
    self.locks.removeAll()
    self.mutex.broadcast()
  }
  
  /// Used to wake all threads blocked on this thread.
  public func wakeBlockedThreads() {
    self.mutex.lock()
    defer {
      self.mutex.unlock()
    }
    self.mutex.broadcast()
  }
  
  /// Execute long running operation which can be aborted via `blocker`
  public func execute<B: Abortable, R>(_ blocker: B, _ operation: (B) throws -> R) rethrows -> R {
    self.mutex.lock()
    self.blocker = blocker
    self.mutex.unlock()
    defer {
      self.mutex.lock()
      self.blocker = nil
      self.mutex.unlock()
    }
    return try operation(blocker)
  }
  
  /// Helps deallocating a thread.
  public override func clean() {
    self.state = .terminated
    self.result = nil
    self.locks.removeAll()
    self.waitingOn = nil
    self.config = nil
    self.worker?.releaseDelegate()
    self.worker = nil
  }
  
  /// Implements the mark phase of the garbage collector for this native object.
  public func mark(in gc: GarbageCollector) {
    if self.tag != gc.tag {
      self.tag = gc.tag
      gc.markLater(self.name)
      gc.markLater(self.threadTag)
      if let expr = self.result {
        gc.markLater(expr)
      }
      for lock in self.locks {
        lock.mark(in: gc)
      }
      self.waitingOn?.mark(in: gc)
      if let (machine, _, _, proc) = self.config {
        machine.mark(in: gc)
        gc.mark(proc)
      }
      self.worker?.markDelegate(with: gc)
    }
  }
  
  /// For debugging purposes.
  public var description: String {
    let nameStr = self.name.isFalse ? self.identityString : self.name.description
    return "[eval thread \(nameStr): \(self.state)]"
  }
  
  public var string: String {
    return self.description
  }
}

///
/// `EvalThreadDelegate` objects encapsulate the code to execute as well as the virtual
/// machine evaluating the code.
/// 
public protocol EvalThreadWorker: AnyObject {
  var evalMachine: VirtualMachine { get }
  func start()
  func stop() -> Thread?
  func guaranteeSecondary() throws
  func markDelegate(with gc: GarbageCollector)
  func releaseDelegate()
}

///
/// `ThreadBlocker` is implemented by all objects which make other threads potentially wait.
/// 
public protocol ThreadBlocker {
  func wakeBlockedThreads()
  func mark(in gc: GarbageCollector)
  var string: String { get }
}

///
/// `Abortable` is implemented by long-running, native operations that can block threads
///
public protocol Abortable {
  func abort()
}

///
/// `MachineThread` objects act as `EvalThreadDelegates` for threads evaluating a
/// thunk concurrently. All threads except for the main thread are represented by a
/// `MachineThread` delegates.
/// 
fileprivate final class MachineThread: Thread, EvalThreadWorker {
  private weak var thread: EvalThread?
  public let evalMachine: VirtualMachine
  private var procedure: Procedure?
  
  fileprivate init(thread: EvalThread, parent machine: VirtualMachine, procedure: Procedure) {
    self.thread = thread
    self.procedure = procedure
    self.evalMachine = VirtualMachine(parent: machine)
    machine.context.objects.manage(self.evalMachine)
  }
  
  override public func main() {
    guard let thread = self.thread else {
      return
    }
    thread.mutex.lock()
    guard let proc = self.procedure else {
      thread.mutex.unlock()
      return
    }
    self.procedure = nil
    thread.mutex.unlock()
    let result = self.evalMachine.onTopLevelDo {
      return try self.evalMachine.apply(.procedure(proc), to: .null)
    }
    thread.mutex.lock()
    if case .error(let err) = result {
      if err.descriptor == .abortion {
        thread.result = .error(RuntimeError.eval(.threadTerminated))
      } else if err.descriptor == .uncaught,
                case .some(.error(let uerr)) = err.irritants.first,
                uerr.descriptor == .abortion {
        thread.result = .error(RuntimeError.eval(.threadTerminated))
      } else {
        thread.result = result
      }
    } else {
      thread.result = result
    }
    if !(thread.result?.isAtom ?? true) {
      self.evalMachine.context.objects.manage(thread)
    }
    thread.state = .terminated
    thread.abandonLocks()
    thread.mutex.unlock()
    self.evalMachine.context.evaluator.threads.remove(thread: self)
  }
  
  public func stop() -> Thread? {
    if self.procedure == nil {
      self.evalMachine.requestAbortion()
    } else {
      self.procedure = nil
      self.thread?.result = .error(RuntimeError.eval(.threadTerminated))
      self.thread?.state = .terminated
      self.thread?.abandonLocks()
      self.evalMachine.context.evaluator.threads.remove(thread: self)
    }
    return self
  }
  
  public func guaranteeSecondary() throws {
  }
  
  public func markDelegate(with gc: GarbageCollector) {
    self.evalMachine.mark(in: gc)
    if let proc = self.procedure {
      gc.mark(proc)
    }
  }
  
  public func releaseDelegate() {
    self.evalMachine.clean()
    self.procedure = nil
  }
}
