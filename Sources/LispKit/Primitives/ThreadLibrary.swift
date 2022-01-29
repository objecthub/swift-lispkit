//
//  ThreadLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 22/12/2021.
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
import SwiftUI

public final class ThreadLibrary: NativeLibrary {
  
  // Mutex states
  private let notOwned: Symbol
  private let abandoned: Symbol
  private let notAbandoned: Symbol
  
  /// Initialize symbols
  public required init(in context: Context) throws {
    self.notOwned = context.symbols.intern("not-owned")
    self.abandoned = context.symbols.intern("abandoned")
    self.notAbandoned = context.symbols.intern("not-abandoned")
    try super.init(in: context)
  }

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "thread"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"], "define", "define-syntax", "syntax-rules", "lambda",
                                             "apply", "values")
    self.`import`(from: ["lispkit", "control"], "begin", "let")
    self.`import`(from: ["lispkit", "dynamic"], "dynamic-wind")
    self.`import`(from: ["lispkit", "system"], "current-second")
    self.`import`(from: ["lispkit", "math"], "+", "-")
    self.`import`(from: ["lispkit", "list"], "cons", "map")
  }

  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("current-thread", self.currentThread))
    self.define(Procedure("thread?", self.isThread))
    self.define(SpecialForm("go", self.compileGo))
    self.define(SpecialForm("thread", self.compileThread))
    self.define(Procedure("spawn", self.spawn))
    self.define(Procedure("make-thread", self.makeThread))
    self.define(Procedure("thread-name", self.threadName))
    self.define(Procedure("thread-tag", self.threadTag))
    self.define(Procedure("thread-runnable?", self.isThreadRunnable))
    self.define(Procedure("thread-blocked?", self.isThreadBlocked))
    self.define(Procedure("thread-terminated?", self.isThreadTerminated))
    self.define(Procedure("thread-start!", self.threadStart))
    self.define(Procedure("thread-yield!", self.threadYield))
    self.define(Procedure("thread-sleep!", self.threadSleep))
    self.define(Procedure("thread-terminate!", self.threadTerminate))
    self.define(Procedure("thread-join!", self.threadJoin))
    self.define(Procedure("mutex?", self.isMutex))
    self.define(Procedure("make-mutex", self.makeMutex))
    self.define(Procedure("mutex-name", self.mutexName))
    self.define(Procedure("mutex-tag", self.mutexTag))
    self.define(Procedure("mutex-state", self.mutexState))
    self.define(Procedure("mutex-lock!", self.mutexLock))
    self.define(Procedure("mutex-try-lock!", self.mutexTryLock))
    self.define(Procedure("mutex-unlock!", self.mutexUnlock))
    self.define(Procedure("condition-variable?", self.isConditionVariable))
    self.define(Procedure("make-condition-variable", self.makeConditionVariable))
    self.define(Procedure("condition-variable-name", self.conditionVariableName))
    self.define(Procedure("condition-variable-tag", self.conditionVariableTag))
    self.define(Procedure("condition-variable-wait!", self.conditionVariableWait))
    self.define(Procedure("condition-variable-signal!", self.conditionVariableSignal))
    self.define(Procedure("condition-variable-broadcast!", self.conditionVariableBroadcast))
    self.define(Procedure("join-timeout-exception?", self.isJoinTimeoutException))
    self.define(Procedure("abandoned-mutex-exception?", self.isAbandonedMutexException))
    self.define(Procedure("terminated-thread-exception?", self.isTerminatedThreadException))
    self.define(Procedure("uncaught-exception?", self.isUncaughtException))
    self.define(Procedure("uncaught-exception-reason", self.uncaughtExceptionReason))
    self.define(Procedure("processor-count", self.processorCount))
    self.define(Procedure("runnable-thread-count", self.runnableThreadCount))
    self.define(Procedure("allocated-thread-count", self.allocatedThreadCount))
    self.define("with-mutex", via:
      "(define-syntax with-mutex",
      "  (syntax-rules ()",
      "    ((_ mtx e0 e1 ...)",
      "       (let ((m mtx))",
      "         (dynamic-wind",
      "           (lambda () (mutex-lock! m))",
      "           (lambda () (begin e0 e1 ...))",
      "           (lambda () (mutex-unlock! m)))))))")
    self.define("parallel", via:
      "(define (parallel thnk . thunks)",
      "  (let ((threads (spawn thunks)))",
      "    (apply values (cons (thnk) (map thread-join! threads)))))")
    self.define("parallel/timeout", via:
      "(define (parallel/timeout timeout def . thunks)",
      "  (let ((t (+ (current-second) timeout))",
      "        (threads (spawn thunks)))",
      "    (apply values (map (lambda (th)",
      "                         (thread-join! th (- t (current-second)) def)) threads))))")
  }
  
  private func thread(from expr: Expr) throws -> NativeThread {
    guard case .object(let obj) = expr, let nt = obj as? NativeThread else {
      throw RuntimeError.type(expr, expected: [NativeThread.type])
    }
    return nt
  }
  
  private func mutex(from expr: Expr) throws -> EvalMutex {
    guard case .object(let obj) = expr, let mutex = obj as? EvalMutex else {
      throw RuntimeError.type(expr, expected: [EvalMutex.type])
    }
    return mutex
  }
  
  private func condvar(from expr: Expr) throws -> EvalCondition {
    guard case .object(let obj) = expr, let condvar = obj as? EvalCondition else {
      throw RuntimeError.type(expr, expected: [EvalCondition.type])
    }
    return condvar
  }
  
  private func currentThread() -> Expr {
    if let et = self.context.evaluator.threads.current {
      return .object(et)
    } else {
      return .false
    }
  }
  
  private func isThread(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is NativeThread {
      return .true
    }
    return .false
  }
  
  private func compileGo(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, let body) = expr else {
      throw RuntimeError.argumentCount(of: "go", min: 0, expr: expr)
    }
    try compiler.compileLambda(nil, .null, body, env)
    compiler.emit(.makeThread(true))
    return false
  }
  
  private func compileThread(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, let body) = expr else {
      throw RuntimeError.argumentCount(of: "thread", min: 0, expr: expr)
    }
    try compiler.compileLambda(nil, .null, body, env)
    compiler.emit(.makeThread(false))
    return false
  }
  
  private func spawn(thunk: Expr, name: Expr?, tag: Expr?) throws -> Expr {
    if case .null = thunk, name == nil && tag == nil {
      return .null
    } else if case .pair(_, _) = thunk, name == nil && tag == nil {
      var lst = thunk
      var res: Exprs = []
      while case .pair(let th, let next) = lst {
        let thread = self.context.evaluator.thread(for: try th.asProcedure(),
                                                   name: name,
                                                   tag: tag)
        try thread.value.start()
        res.append(.object(thread))
        lst = next
      }
      return .makeList(res)
    } else {
      let thread = self.context.evaluator.thread(for: try thunk.asProcedure(),
                                                 name: name,
                                                 tag: tag)
      try thread.value.start()
      return .object(thread)
    }
  }
  
  private func makeThread(thunk: Expr, name: Expr?, tag: Expr?) throws -> Expr {
    return .object(self.context.evaluator.thread(for: try thunk.asProcedure(),
                                                 name: name,
                                                 tag: tag))
  }
  
  private func threadName(th: Expr) throws -> Expr {
    return try self.thread(from: th).value.name
  }
  
  private func threadTag(th: Expr) throws -> Expr {
    return try self.thread(from: th).value.threadTag
  }
  
  private func isThreadRunnable(th: Expr) throws -> Expr {
    return .makeBoolean(try self.thread(from: th).value.state == .runnable)
  }
  
  private func isThreadBlocked(th: Expr) throws -> Expr {
    return .makeBoolean(try self.thread(from: th).value.state == .blocked)
  }
  
  private func isThreadTerminated(th: Expr) throws -> Expr {
    return .makeBoolean(try self.thread(from: th).value.state == .terminated)
  }
  
  private func threadStart(th: Expr) throws -> Expr {
    let thread = try self.thread(from: th)
    try thread.value.start()
    return .object(thread)
  }
  
  private func threadYield() -> Expr {
    pthread_yield_np()
    return .void
  }
  
  private func threadSleep(timeout: Expr) throws -> Expr {
    if let et = self.context.evaluator.threads.current {
      try et.value.sleep(try timeout.asDouble(coerce: true))
    }
    return .void
  }
  
  private func threadTerminate(th: Expr) throws -> Expr {
    let thread = try self.thread(from: th)
    if let t = thread.value.abort(), Thread.current !== t {
      self.context.evaluator.threads.waitForTermination(of: t)
    }
    return .void
  }
  
  private func threadJoin(args: Arguments) throws -> (Procedure, Exprs) {
    guard args.count > 0 && args.count < 4,
          let (th, tout, def) = args.optional(.false, .false, .undef) else {
      throw RuntimeError.argumentCount(of: "thread-join!", min: 1, max: 3, args: .makeList(args))
    }
    let thread = try self.thread(from: th)
    let timeout = tout.isFalse ? nil : try tout.asDouble(coerce: true)
    guard let current = self.context.evaluator.threads.current else {
      throw RuntimeError.eval(.threadJoinInInvalidContext, th)
    }
    var result: Expr? = nil
    do {
      result = try thread.value.getResult(in: current.value, timeout)
    } catch let error as RuntimeError {
      if let proc = self.context.evaluator.raiseContinuableProc {
        var args = Exprs()
        args.append(.error(error))
        return (proc, args)
      } else {
        throw error
      }
    }
    if let result = result {
      var args = Exprs()
      args.append(result)
      return (CoreLibrary.idProc, args)
    } else if def.isUndef {
      throw RuntimeError.eval(.joinTimedOut, th)
    } else {
      var args = Exprs()
      args.append(def)
      return (CoreLibrary.idProc, args)
    }
  }
  
  private func isMutex(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is EvalMutex {
      return .true
    }
    return .false
  }
  
  private func makeMutex(name: Expr?, tag: Expr?) -> Expr {
    return .object(EvalMutex(name: name ?? .false, tag: tag ?? .false))
  }
  
  private func mutexName(mt: Expr) throws -> Expr {
    return try self.mutex(from: mt).name
  }
  
  private func mutexTag(mt: Expr) throws -> Expr {
    return try self.mutex(from: mt).tag
  }
  
  private func mutexState(mt: Expr) throws -> Expr {
    let mutex = try self.mutex(from: mt)
    switch mutex.state {
      case .unlockedNotAbandoned:
        return .symbol(self.notAbandoned)
      case .unlockedAbandoned:
        return .symbol(self.abandoned)
      case .lockedNotOwned:
        return .symbol(self.notOwned)
      case .lockedOwned:
        guard let owner = mutex.owner else {
          return .symbol(self.notOwned)
        }
        return .object(NativeThread(owner))
    }
  }
  
  private func mutexLock(mt: Expr, timeout: Expr?, th: Expr?) throws -> Expr {
    let mutex = try self.mutex(from: mt)
    let timeout = timeout == nil || timeout! == .false ? nil : try timeout!.asDouble(coerce: true)
    guard let current = self.context.evaluator.threads.current else {
      throw RuntimeError.eval(.mutexUseInInvalidContext, mt)
    }
    let thread = th == nil ? current : (th! == .false ? nil : try self.thread(from: th!))
    return .makeBoolean(try mutex.lock(in: current.value, for: thread?.value, timeout: timeout))
  }
  
  private func mutexTryLock(mt: Expr, th: Expr?) throws -> Expr {
    let mutex = try self.mutex(from: mt)
    guard let current = self.context.evaluator.threads.current else {
      throw RuntimeError.eval(.mutexUseInInvalidContext, mt)
    }
    let thread = th == nil ? current : (th! == .false ? nil : try self.thread(from: th!))
    return .makeBoolean(try mutex.tryLock(in: current.value, for: thread?.value))
  }
  
  private func mutexUnlock(mt: Expr, cv: Expr?, timeout: Expr?) throws -> Expr {
    let mutex = try self.mutex(from: mt)
    let condvar = cv == nil || cv! == .false ? nil : try self.condvar(from: cv!)
    let timeout = timeout == nil || timeout! == .false ? nil : try timeout!.asDouble(coerce: true)
    guard let current = self.context.evaluator.threads.current else {
      throw RuntimeError.eval(.mutexUseInInvalidContext, mt)
    }
    return .makeBoolean(try mutex.unlock(in: current.value, condition: condvar, timeout: timeout))
  }
  
  private func isConditionVariable(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is EvalCondition {
      return .true
    }
    return .false
  }
  
  private func makeConditionVariable(name: Expr?, tag: Expr?) -> Expr {
    return .object(EvalCondition(name: name ?? .false, tag: tag ?? .false))
  }
  
  private func conditionVariableName(cv: Expr) throws -> Expr {
    return try self.condvar(from: cv).name
  }
  
  private func conditionVariableTag(cv: Expr) throws -> Expr {
    return try self.condvar(from: cv).tag
  }
  
  private func conditionVariableWait(cv: Expr, mt: Expr, timeout: Expr?) throws -> Expr {
    let condition = try self.condvar(from: cv)
    let timeout = timeout == nil || timeout! == .false ? nil : try timeout!.asDouble(coerce: true)
    guard let current = self.context.evaluator.threads.current else {
      throw RuntimeError.eval(.mutexUseInInvalidContext, mt)
    }
    return .makeBoolean(try self.mutex(from: mt).wait(in: current.value,
                                                      for: condition,
                                                      timeout: timeout))
  }
  
  private func conditionVariableSignal(cv: Expr) throws -> Expr {
    try self.condvar(from: cv).signal()
    return .void
  }
  
  private func conditionVariableBroadcast(cv: Expr) throws -> Expr {
    try self.condvar(from: cv).broadcast()
    return .void
  }
  
  private func isJoinTimeoutException(expr: Expr) -> Expr {
    guard case .error(let err) = expr,
          case .eval(.joinTimedOut) = err.descriptor else {
      return .false
    }
    return .true
  }
  
  private func isAbandonedMutexException(expr: Expr) -> Expr {
    guard case .error(let err) = expr,
          case .eval(.abandonedMutex) = err.descriptor else {
      return .false
    }
    return .true
  }
  
  private func isTerminatedThreadException(expr: Expr) -> Expr {
    guard case .error(let err) = expr,
          case .eval(.threadTerminated) = err.descriptor else {
      return .false
    }
    return .true
  }
  
  private func isUncaughtException(expr: Expr) -> Expr {
    guard case .error(let err) = expr, err.descriptor == .uncaught else {
      return .false
    }
    return .true
  }
  
  private func uncaughtExceptionReason(expr: Expr) throws -> Expr {
    guard case .error(let err) = expr, err.descriptor == .uncaught else {
      throw RuntimeError.eval(.expectedUncaughtException, expr)
    }
    return err.irritants[0]
  }
  
  private func processorCount(active: Expr?) -> Expr {
    if active?.isTrue ?? false {
      return .makeNumber(ProcessInfo.processInfo.activeProcessorCount)
    } else {
      return .makeNumber(ProcessInfo.processInfo.processorCount)
    }
  }
  
  private func runnableThreadCount() -> Expr {
    return .makeNumber(self.context.evaluator.threads.count)
  }
  
  private func allocatedThreadCount() -> Expr {
    return .makeNumber(EvalThread.allocated.load(ordering: .relaxed))
  }
}
