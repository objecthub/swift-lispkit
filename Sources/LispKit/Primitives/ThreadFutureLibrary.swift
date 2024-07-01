//
//  ThreadFutureLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 22/06/2024.
//  Copyright © 2024 ObjectHub. All rights reserved.
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

public final class ThreadFutureLibrary: NativeLibrary {
  
  /// Initialize symbols
  public required init(in context: Context) throws {
    try super.init(in: context)
  }
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "thread", "future"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"], "define", "define-syntax", "syntax-rules", "lambda",
                                             "case-lambda")
    self.`import`(from: ["lispkit", "control"], "let", "if")
    self.`import`(from: ["lispkit", "dynamic"], "try", "raise", "error")
    self.`import`(from: ["lispkit", "list"], "cdr", "car")
    self.`import`(from: ["lispkit", "thread"], "spawn")
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define("future-type-tag", as: Future.type.objectTypeTag())
    self.define(Procedure("future?", self.isFuture))
    self.define(Procedure("_make-future", self.makeFuture))
    self.define(Procedure("_future-get", self.futureGet))
    self.define(Procedure("_future-set!", self.futureSet))
    self.define(Procedure("future-done?", self.futureDone))
    self.define("make-future", via: """
      (define (make-future thunk)
        (let ((future (_make-future)))
          (spawn (lambda ()
                    (try (lambda () (_future-set! future (thunk) #f))
                         (lambda (e) (_future-set! future e #t)))))
          future))
    """)
    self.define("make-evaluated-future", via: """
      (define (make-evaluated-future x)
        (let ((future (_make-future)))
          (_future-set! future x #f)
          future))
    """)
    self.define("make-failing-future", via: """
      (define (make-failing-future x)
        (let ((future (_make-future)))
          (_future-set! future x #t)
          future))
    """)
    self.define("future", via: """
      (define-syntax future
        (syntax-rules ()
          ((_ expr ...)
            (make-future (lambda () expr ...)))))
    """)
    self.define("future-get", via: """
      (define future-get
        (case-lambda
          ((future)
            (let ((result (_future-get future)))
              (if (cdr result) (raise (car result)) (car result))))
          ((future timeout)
            (let ((result (_future-get future timeout)))
              (if result
                  (if (cdr result) (raise (car result)) (car result))
                  (error "future-get timed out for $0" future))))
          ((future timeout default)
            (let ((result (_future-get future timeout)))
              (if result
                  (if (cdr result) (raise (car result)) (car result))
                  default)))
        )
      )
    """)
    self.define("touch", via: """
      (define (touch future)
        (let ((result (_future-get future)))
          (if (cdr result) (raise (car result)) (car result))))
    """)
  }
  
  private func future(from expr: Expr) throws -> Future {
    guard case .object(let obj) = expr, let future = obj as? Future else {
      throw RuntimeError.type(expr, expected: [Future.type])
    }
    return future
  }
  
  private func isFuture(expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is Future else {
      return .false
    }
    return .true
  }
  
  private func makeFuture() throws -> Expr {
    return .object(Future(external: true))
  }
  
  private func futureDone(expr: Expr) throws -> Expr {
    return .makeBoolean(try self.future(from: expr).resultAvailable(in: self.context))
  }
  
  private func futureGet(expr: Expr, timeout: Expr?) throws -> Expr {
    let timeout = try timeout?.asDouble(coerce: true)
    if let (expr, error) =
        try self.future(from: expr).getResult(in: self.context, timeout: timeout) {
      return .pair(expr, .makeBoolean(error))
    } else {
      return .false
    }
  }
  
  private func futureSet(expr: Expr, value: Expr, error: Expr) throws -> Expr {
    guard try self.future(from: expr).setResult(in: self.context,
                                                to: value,
                                                raise: error.isTrue) else {
      throw RuntimeError.eval(.settingFutureValueTwice, expr, value)
    }
    return .void
  }
}

enum FutureSync {
  case `external`(mutex: EvalMutex, condition: EvalCondition)
  case `internal`(condition: NSCondition)
  
  func lock(in context: Context) throws {
    switch self {
      case .external(let mutex, _):
        guard let current = context.evaluator.threads.current,
              try mutex.lock(in: current.value, for: current.value) else {
          throw RuntimeError.eval(.mutexUseInInvalidContext, .object(mutex))
        }
      case .internal(let condition):
        condition.lock()
    }
  }
  
  func unlock(in context: Context) throws {
    switch self {
      case .external(let mutex, _):
        guard let current = context.evaluator.threads.current else {
          throw RuntimeError.eval(.mutexUseInInvalidContext, .object(mutex))
        }
        _ = try mutex.unlock(in: current.value)
      case .internal(let condition):
        condition.unlock()
    }
  }
  
  func wait(in context: Context, timeout: TimeInterval? = nil) throws {
    switch self {
      case .external(let mutex, let condition):
        guard let current = context.evaluator.threads.current else {
          throw RuntimeError.eval(.mutexUseInInvalidContext, .object(mutex))
        }
        _ = try mutex.unlock(in: current.value, condition: condition, timeout: timeout)
      case .internal(let condition):
        if let timeout {
          var target = Date()
          target.addTimeInterval(timeout)
          while Date() < target && !context.evaluator.isAbortionRequested() {
            var nextCheckpoint = Date()
            nextCheckpoint.addTimeInterval(0.5)
            if condition.wait(until: nextCheckpoint < target ? nextCheckpoint : target) {
              break
            }
          }
        } else {
          while !context.evaluator.isAbortionRequested() {
            var nextCheckpoint = Date()
            nextCheckpoint.addTimeInterval(0.5)
            if condition.wait(until: nextCheckpoint) {
              break
            }
          }
        }
    }
  }
  
  func signal() {
    switch self {
      case .external(_, let condition):
        condition.signal()
      case .internal(let condition):
        condition.signal()
    }
  }
}

public final class Future: NativeObject {

  /// Type representing zip archives
  public static let type = Type.objectType(Symbol(uninterned: "future"))
  
  /// Condition to protect the result
  private let sync: FutureSync
  
  /// The result once computed
  public var result: (value: Expr, error: Bool)? = nil
  
  /// Initializer
  public init(external: Bool) {
    self.sync = external ? .external(mutex: EvalMutex(), condition: EvalCondition())
                         : .internal(condition: NSCondition())
  }
  
  public func resultAvailable(in context: Context) throws -> Bool {
    try self.sync.lock(in: context)
    defer {
      try? self.sync.unlock(in: context)
    }
    return self.result != nil
  }
  
  public func setResult(in context: Context, to result: Expr, raise: Bool = false) throws -> Bool {
    try self.sync.lock(in: context)
    defer {
      try? self.sync.unlock(in: context)
    }
    guard self.result == nil else {
      return false
    }
    self.result = (result, raise)
    self.sync.signal()
    return true
  }
  
  public func getResult(in context: Context, timeout: TimeInterval? = nil) throws -> (value: Expr, error: Bool)? {
    try self.sync.lock(in: context)
    defer {
      try? self.sync.unlock(in: context)
    }
    if self.result == nil {
      try self.sync.wait(in: context, timeout: timeout)
    }
    guard let result = self.result else {
      return nil
    }
    return result
  }
  
  public override var type: Type {
    return Self.type
  }
  
  public override var string: String {
    return "#<\(self.tagString)>"
  }
  
  public override var tagString: String {
    switch self.result {
      case .none:
        return "\(Self.type) \(self.identityString)"
      case .some((value: let expr, error: false)):
        return "\(Self.type) \(self.identityString): \(expr)"
      case .some((value: let expr, error: true)):
        return "\(Self.type) \(self.identityString)! \(expr)"
    }
  }
  
  public override func mark(in gc: GarbageCollector) {
    // nothing to collect for the objects stored in sync
    if let expr = self.result?.value {
      gc.markLater(expr)
    }
  }
  
  public override func unpack(in context: Context) -> Exprs {
    switch self.result {
      case .none:
        return [.makeString(identityString)]
      case .some((value: let expr, error: false)):
        return [.makeString(identityString), expr, .false]
      case .some((value: let expr, error: true)):
        return [.makeString(identityString), expr, .true]
    }
  }
}
