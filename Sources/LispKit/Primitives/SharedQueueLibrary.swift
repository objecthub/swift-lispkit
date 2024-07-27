//
//  ThreadSharedQueueLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 24/07/2024.
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

public final class SharedQueueLibrary: NativeLibrary {
  
  /// Initialize symbols
  public required init(in context: Context) throws {
    try super.init(in: context)
  }
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "thread", "shared-queue"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define("shared-queue-type-tag", as: SharedQueue.type.objectTypeTag())
    self.define(Procedure("shared-queue?", self.isSharedQueue))
    self.define(Procedure("make-shared-queue", self.makeSharedQueue))
    self.define(Procedure("shared-queue-copy", self.sharedQueueCopy))
    self.define(Procedure("list->shared-queue", self.listToSharedQueue))
    self.define(Procedure("shared-queue->list", self.sharedQueueToList))
    self.define(Procedure("shared-queue-closed?", self.isSharedQueueClosed))
    self.define(Procedure("shared-queue-empty?", self.isSharedQueueEmpty))
    self.define(Procedure("shared-queue-length", self.sharedQueueLength))
    self.define(Procedure("shared-queue-room", self.sharedQueueRoom))
    self.define(Procedure("shared-queue-front", self.sharedQueueFront))
    self.define(Procedure("shared-queue-rear", self.sharedQueueRear))
    self.define(Procedure("shared-queue-num-waiting", self.sharedQueueNumWaiting))
    self.define(Procedure("shared-queue-enqueue!", self.sharedQueueEnqueue))
    self.define(Procedure("shared-queue-dequeue!", self.sharedQueueDequeue))
    self.define(Procedure("shared-queue-dequeue-all!", self.sharedQueueDequeueAll))
    self.define(Procedure("shared-queue-push!", self.sharedQueuePush))
    self.define(Procedure("shared-queue-pop!", self.sharedQueuePop))
    self.define(Procedure("shared-queue-enqueue/wait!", self.sharedQueueEnqueueWait))
    self.define(Procedure("shared-queue-dequeue/wait!", self.sharedQueueDequeueWait))
    self.define(Procedure("shared-queue-push/wait!", self.sharedQueuePushWait))
    self.define(Procedure("shared-queue-pop/wait!", self.sharedQueuePopWait))
  }
  
  private func sharedQueue(from expr: Expr) throws -> SharedQueue {
    guard case .object(let obj) = expr, let queue = obj as? SharedQueue else {
      throw RuntimeError.type(expr, expected: [SharedQueue.type])
    }
    return queue
  }
  
  private func isSharedQueue(expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is SharedQueue else {
      return .false
    }
    return .true
  }
  
  private func makeSharedQueue(maxLength: Expr?, capacity: Expr?) throws -> Expr {
    return .object(SharedQueue(external: true,
                               maxLength: try maxLength?.asInt(above: 0, below: Int.max),
                               capacity: try capacity?.asInt(above: 2, below: 100000)))
  }
  
  private func sharedQueueCopy(expr: Expr) throws -> Expr {
    return .object(try self.sharedQueue(from: expr).copy(in: self.context))
  }
  
  private func listToSharedQueue(expr: Expr, maxLength: Expr?, capacity: Expr?) throws -> Expr {
    let maxLength = try maxLength?.asInt(above: 0, below: Int.max)
    let queue = SharedQueue(external: true,
                            maxLength: maxLength,
                            capacity: try capacity?.asInt(above: 2, below: 100000))
    var list = expr
    while case .pair(let car, let cdr) = list {
      if queue.queue.exprs.count < (maxLength ?? Int.max) {
        queue.queue.exprs.append(car)
      } else {
        throw RuntimeError.eval(.insertIntoMaxQueue, car, .object(queue))
      }
      list = cdr
    }
    guard case .null = list else {
      throw RuntimeError.type(expr, expected: [.properListType])
    }
    return .object(queue)
  }
  
  private func sharedQueueToList(expr: Expr) throws -> Expr {
    return try self.sharedQueue(from: expr).toList(in: self.context)
  }
  
  private func isSharedQueueClosed(expr: Expr) throws -> Expr {
    return .makeBoolean(try self.sharedQueue(from: expr).isClosed(in: self.context))
  }
  
  private func isSharedQueueEmpty(expr: Expr) throws -> Expr {
    return .makeBoolean(try self.sharedQueue(from: expr).isEmpty(in: self.context))
  }
  
  private func sharedQueueLength(expr: Expr) throws -> Expr {
    return .makeNumber(try self.sharedQueue(from: expr).length(in: self.context))
  }
  
  private func sharedQueueRoom(expr: Expr) throws -> Expr {
    return .makeNumber(try self.sharedQueue(from: expr).roomLeft(in: self.context))
  }
  
  private func sharedQueueFront(expr: Expr, fallback: Expr?) throws -> Expr {
    if let result = try self.sharedQueue(from: expr).front(in: self.context) ?? fallback {
      return result
    } else {
      throw RuntimeError.eval(.queueIsEmpty, expr)
    }
  }
  
  private func sharedQueueRear(expr: Expr, fallback: Expr?) throws -> Expr {
    if let result = try self.sharedQueue(from: expr).rear(in: self.context) ?? fallback {
      return result
    } else {
      throw RuntimeError.eval(.queueIsEmpty, expr)
    }
  }
  
  private func sharedQueueNumWaiting(expr: Expr) throws -> Expr {
    let (readers, writers) = try self.sharedQueue(from: expr).waiting(in: self.context)
    return .values(.pair(.makeNumber(readers), .pair(.makeNumber(writers), .null)))
  }
  
  private func sharedQueueEnqueue(expr: Expr, args: Arguments) throws -> Expr {
    guard try self.sharedQueue(from: expr).enqueue(in: self.context,
                                                   values: Exprs(args),
                                                   timeout: nil,
                                                   push: false,
                                                   wait: false,
                                                   close: false) else {
      throw RuntimeError.eval(.insertIntoMaxQueue, .makeList(args), expr)
    }
    return .void
  }
  
  private func sharedQueueDequeue(expr: Expr, fallback: Expr?) throws -> Expr {
    if let res = try self.sharedQueue(from: expr).dequeue(
                   in: self.context,
                   timeout: nil,
                   wait: false,
                   close: false,
                   callback: { $0.queue.exprs.removeLast() }) ?? fallback {
      return res
    }
    throw RuntimeError.eval(.queueIsEmpty, expr)
  }
  
  private func sharedQueueDequeueAll(expr: Expr) throws -> Expr {
    return try self.sharedQueue(from: expr).dequeue(
                 in: self.context,
                 timeout: nil,
                 wait: false,
                 close: false,
                 callback: {
                   let res = Expr.makeList(fromStack: Array($0.queue.exprs))
                   $0.queue.exprs.removeAll(keepingCapacity: false)
                   return res
                 }) ?? .null
  }
  
  private func sharedQueuePush(expr: Expr, args: Arguments) throws -> Expr {
    guard try self.sharedQueue(from: expr).enqueue(in: self.context,
                                                   values: Exprs(args),
                                                   timeout: nil,
                                                   push: true,
                                                   wait: false,
                                                   close: false) else {
      throw RuntimeError.eval(.insertIntoMaxQueue, .makeList(args), expr)
    }
    return .void
  }
  
  private func sharedQueuePop(expr: Expr, fallback: Expr?) throws -> Expr {
    if let res = try self.sharedQueue(from: expr).dequeue(
                   in: self.context,
                   timeout: nil,
                   wait: false,
                   close: false,
                   callback: { $0.queue.exprs.removeLast() }) ?? fallback {
      return res
    }
    throw RuntimeError.eval(.queueIsEmpty, expr)
  }
  
  private func sharedQueueEnqueueWait(expr: Expr, obj: Expr, args: Arguments) throws -> Expr {
    guard let (timeout, fallback, close) = args.optional(.false, .false, .false) else {
      throw RuntimeError.argumentCount(of: "shared-queue-enqueue/wait!",
                                       min: 2,
                                       max: 5,
                                       args: .pair(expr, .pair(obj, .makeList(args))))
    }
    let timeo = timeout.isFalse ? nil : try timeout.asDouble(coerce: true)
    if try self.sharedQueue(from: expr).enqueue(in: self.context,
                                                values: [obj],
                                                timeout: timeo,
                                                push: false,
                                                wait: true,
                                                close: close.isTrue) {
      return .true
    } else {
      return fallback
    }
  }
  
  private func sharedQueueDequeueWait(expr: Expr, args: Arguments) throws -> Expr {
    guard let (timeout, fallback, close) = args.optional(.false, .false, .false) else {
      throw RuntimeError.argumentCount(of: "shared-queue-dequeue/wait!",
                                       min: 1,
                                       max: 4,
                                       args: .pair(expr, .makeList(args)))
    }
    let timeo = timeout.isFalse ? nil : try timeout.asDouble(coerce: true)
    return try self.sharedQueue(from: expr).dequeue(
                   in: self.context,
                   timeout: timeo,
                   wait: true,
                   close: close.isTrue,
                   callback: { $0.queue.exprs.removeLast() }) ?? fallback
  }
  
  private func sharedQueuePushWait(expr: Expr, obj: Expr, args: Arguments) throws -> Expr {
    guard let (timeout, fallback, close) = args.optional(.false, .false, .false) else {
      throw RuntimeError.argumentCount(of: "shared-queue-push/wait!",
                                       min: 2,
                                       max: 5,
                                       args: .pair(expr, .pair(obj, .makeList(args))))
    }
    let timeo = timeout.isFalse ? nil : try timeout.asDouble(coerce: true)
    if try self.sharedQueue(from: expr).enqueue(in: self.context,
                                                values: [obj],
                                                timeout: timeo,
                                                push: true,
                                                wait: true,
                                                close: close.isTrue) {
      return .true
    } else {
      return fallback
    }
  }
  
  private func sharedQueuePopWait(expr: Expr, args: Arguments) throws -> Expr {
    guard let (timeout, fallback, close) = args.optional(.false, .false, .false) else {
      throw RuntimeError.argumentCount(of: "shared-queue-pop/wait!",
                                       min: 1,
                                       max: 4,
                                       args: .pair(expr, .makeList(args)))
    }
    let timeo = timeout.isFalse ? nil : try timeout.asDouble(coerce: true)
    return try self.sharedQueue(from: expr).dequeue(
                   in: self.context,
                   timeout: timeo,
                   wait: true,
                   close: close.isTrue,
                   callback: { $0.queue.exprs.removeLast() }) ?? fallback
  }
}

public final class SharedQueue: NativeObject {

  /// Type representing zip archives
  public static let type = Type.objectType(Symbol(uninterned: "shared-queue"))
  
  /// Condition to protect the result
  private let sync: FutureSync
  
  /// The actual queue as a collection
  public var queue: Collection
  
  /// The maximum length of the queue
  public var maxLength: Int
  
  /// To make sure we still garbage collect while waiting to insert
  public var parked: [Int : Expr]
  public var nextParkingId: Int
  
  /// Blocked threads
  public var waitingReaders: Int
  public var waitingWriters: Int
  
  /// Is the queue closed?
  public var closed: Bool

  /// Is this queue aborted?
  public var aborted: Bool
  
  /// Initializer
  public init(external: Bool, maxLength: Int? = nil, capacity: Int? = nil) {
    self.sync = external ? .external(mutex: EvalMutex(), condition: EvalCondition())
                         : .internal(condition: NSCondition())
    self.queue = Collection(kind: .growableVector)
    self.maxLength = (maxLength ?? Int.max) < 0 ? 0 : (maxLength ?? Int.max)
    self.parked = [:]
    self.nextParkingId = 0
    if let capacity {
      self.queue.exprs.reserveCapacity(capacity)
    }
    self.waitingReaders = 0
    self.waitingWriters = 0
    self.closed = false
    self.aborted = false
  }
  
  public func copy(in context: Context) throws -> SharedQueue {
    try self.sync.lock(in: context)
    defer {
      try? self.sync.unlock(in: context)
    }
    let res = SharedQueue(external: true,
                          maxLength: self.maxLength,
                          capacity: self.queue.exprs.capacity)
    res.queue.exprs = self.queue.exprs
    return res
  }
  
  public func isClosed(in context: Context) throws -> Bool {
    try self.sync.lock(in: context)
    defer {
      try? self.sync.unlock(in: context)
    }
    return self.closed
  }
  
  public func isAborted(in context: Context) throws -> Bool {
    try self.sync.lock(in: context)
    defer {
      try? self.sync.unlock(in: context)
    }
    return self.aborted
  }
  
  public func abort(in context: Context) throws {
    try self.sync.lock(in: context)
    defer {
      try? self.sync.unlock(in: context)
    }
    guard !self.aborted else {
      return
    }
    self.aborted = true
    self.sync.broadcast()
  }
  
  public func isEmpty(in context: Context) throws -> Bool {
    try self.sync.lock(in: context)
    defer {
      try? self.sync.unlock(in: context)
    }
    return self.queue.exprs.isEmpty
  }
  
  public func length(in context: Context) throws -> Int {
    try self.sync.lock(in: context)
    defer {
      try? self.sync.unlock(in: context)
    }
    return self.queue.exprs.count
  }
  
  public func roomLeft(in context: Context) throws -> Int {
    try self.sync.lock(in: context)
    defer {
      try? self.sync.unlock(in: context)
    }
    return self.maxLength - self.queue.exprs.count
  }
  
  public func front(in context: Context) throws -> Expr? {
    try self.sync.lock(in: context)
    defer {
      try? self.sync.unlock(in: context)
    }
    return self.queue.exprs.last
  }
  
  public func rear(in context: Context) throws -> Expr? {
    try self.sync.lock(in: context)
    defer {
      try? self.sync.unlock(in: context)
    }
    return self.queue.exprs.first
  }
  
  public func waiting(in context: Context) throws -> (readers: Int, writers: Int) {
    try self.sync.lock(in: context)
    defer {
      try? self.sync.unlock(in: context)
    }
    return (readers: self.waitingReaders, writers: self.waitingWriters)
  }
  
  public func toList(in context: Context) throws -> Expr {
    try self.sync.lock(in: context)
    defer {
      try? self.sync.unlock(in: context)
    }
    return .makeList(fromStack: Array(self.queue.exprs))
  }
  
  public func enqueue(in context: Context,
                      values: Exprs,
                      timeout: TimeInterval? = nil,
                      push: Bool = false,
                      wait: Bool = false,
                      close: Bool = false) throws -> Bool {
    try self.sync.lock(in: context)
    defer {
      try? self.sync.unlock(in: context)
    }
    if wait && !self.aborted && !self.closed {
      var index = self.nextParkingId
      for value in values {
        self.parked[self.nextParkingId] = value
        self.nextParkingId = self.nextParkingId &+ 1
      }
      let target = self.targetDate(timeout: timeout)
      self.waitingWriters += 1
      defer {
        self.waitingWriters -= 1
        for _ in 0..<values.count {
          self.parked.removeValue(forKey: index)
          index = index &+ 1
        }
      }
      while self.notTimedOutYet(target: target) &&
              self.queue.exprs.count >= self.maxLength &&
              !self.aborted &&
              !context.evaluator.isAbortionRequested() {
        let remaining = self.remainingTimeout(target: target)
        if remaining == nil || remaining! > 0 {
          try self.sync.wait(in: context, timeout: remaining)
        }
      }
    }
    if self.closed || self.aborted {
      throw RuntimeError.eval(.insertIntoClosedQueue, .makeList(values), .object(self))
    } else if self.queue.exprs.count + values.count > self.maxLength ||
                context.evaluator.isAbortionRequested() {
      return false
    }
    // Guarantee that the referenced cell is managed by a managed object pool if needed
    if push {
      for value in values {
        (value.isAtom ? self.queue : context.objects.manage(self.queue)).exprs.append(value)
      }
    } else {
      for value in values {
        (value.isAtom ? self.queue : context.objects.manage(self.queue)).exprs.insert(value, at: 0)
      }
    }
    if close {
      self.closed = true
    }
    self.sync.broadcast()
    return true
  }
  
  public func dequeue<T>(in context: Context,
                         timeout: TimeInterval? = nil,
                         wait: Bool = false,
                         close: Bool = false,
                         callback: (SharedQueue) -> T) throws -> T? {
    try self.sync.lock(in: context)
    defer {
      try? self.sync.unlock(in: context)
    }
    if wait && !self.aborted {
      self.waitingReaders += 1
      defer {
        self.waitingReaders -= 1
      }
      let target = self.targetDate(timeout: timeout)
      while self.notTimedOutYet(target: target) &&
              self.queue.exprs.isEmpty &&
              !self.closed &&
              !self.aborted &&
              !context.evaluator.isAbortionRequested() {
        let remaining = self.remainingTimeout(target: target)
        if remaining == nil || remaining! > 0 {
          try self.sync.wait(in: context, timeout: remaining)
        }
      }
    }
    if self.queue.exprs.isEmpty || self.aborted || context.evaluator.isAbortionRequested() {
      return nil
    }
    let result = callback(self)
    if close {
      self.closed = true
    }
    self.sync.broadcast()
    return result
  }
  
  public override var type: Type {
    return Self.type
  }
  
  public override var string: String {
    return "#<\(self.tagString)>"
  }
  
  public override var tagString: String {
    var res = "\(Self.type) \(self.identityString)"
    var sep = ": "
    for i in 0..<min(self.queue.exprs.count, 10) {
      res += sep + self.queue.exprs[i].description
      sep = " "
    }
    return res
  }
  
  public var isAtom: Bool {
    return false
  }
  
  public override func mark(in gc: GarbageCollector) {
    gc.mark(self.queue)
    for expr in self.parked.values {
      gc.markLater(expr)
    }
  }
  
  public override func unpack(in context: Context) -> Exprs {
    var res: Exprs = [.makeString(identityString)]
    for i in 0..<min(self.queue.exprs.count, 10) {
      res.append(self.queue.exprs[i])
    }
    return res
  }
  
  private func targetDate(timeout: TimeInterval?) -> Date? {
    guard let timeout else {
      return nil
    }
    var target: Date = .now
    target.addTimeInterval(timeout)
    return target
  }
  
  private func remainingTimeout(target: Date?) -> TimeInterval? {
    guard let target else {
      return nil
    }
    return Date.now.distance(to: target)
  }
  
  private func notTimedOutYet(target: Date?) -> Bool {
    return (target == nil) || (target! > .now)
  }
}
