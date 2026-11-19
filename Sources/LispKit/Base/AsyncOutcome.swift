//
//  AsyncOutcome.swift
//  LispKit
//
//  Created by Matthias Zenger on 19/11/2026.
//  Copyright © 2026 ObjectHub. All rights reserved.
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
/// `AsyncOutcome` holds the result of a computation that is performed asynchronously, e.g.
/// by a Swift `Task` or on a dispatch queue, and lets a LispKit thread wait for it. Waiting
/// can be aborted (e.g. via ctrl-C), in which case `RuntimeError.abortion()` is thrown.
///
public final class AsyncOutcome<Value, Failure: Error>: @unchecked Sendable {
  private let lock = NSLock()
  private let semaphore = DispatchSemaphore(value: 0)
  private var result: Result<Value, Failure>?

  public init() {}

  /// Stores the result. Only the first result is stored.
  public func store(_ result: Result<Value, Failure>) {
    self.lock.lock()
    let stored = self.result == nil
    if stored {
      self.result = result
    }
    self.lock.unlock()
    if stored {
      self.semaphore.signal()
    }
  }

  /// The result, if available.
  public var current: Result<Value, Failure>? {
    self.lock.lock()
    defer { self.lock.unlock() }
    return self.result
  }

  /// Waits at most `seconds` for the result.
  public func wait(upTo seconds: TimeInterval) {
    _ = self.semaphore.wait(timeout: .now() + seconds)
  }

  /// Waits for the result until it is available (the result of the function), the
  /// evaluation on the current LispKit thread gets aborted (an error is thrown), or the
  /// `deadline` has passed (`nil` is returned). There is no time limit if `deadline` is `nil`.
  /// If this is the main thread, the run loop is kept running while waiting, since some
  /// frameworks deliver their results on the main thread.
  public func waitForResult(in context: Context?,
                            until deadline: Date? = nil) throws -> Result<Value, Failure>? {
    while self.current == nil {
      if context?.evaluator.threads.current?.value.machine?.abortionRequested ?? false {
        throw RuntimeError.abortion()
      }
      if let deadline, Date() > deadline {
        return nil
      }
      if Thread.isMainThread {
        if !RunLoop.current.run(mode: .default, before: Date(timeIntervalSinceNow: 0.05)) {
          Thread.sleep(forTimeInterval: 0.005)
        }
      } else {
        self.wait(upTo: 0.05)
      }
    }
    return self.current
  }
}
