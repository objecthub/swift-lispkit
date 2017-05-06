//
//  TextOutput.swift
//  LispKit
//
//  Created by Matthias Zenger on 07/06/2016.
//  Copyright © 2016 ObjectHub. All rights reserved.
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
/// `TextOutput` represents a textual output port. This class manages a character buffer of a
/// configurable capacity, and an optional binary output to which all data is eventually
/// written. If there is no output object attached, all data will be accumulated in the buffer.
///
open class TextOutput {
  
  /// Internal character buffer.
  private var buffer: [UniChar]
  
  /// Index of the next character to write.
  private var next: Int
  
  /// Soft threshold for triggering a `flush`. Invariant: `threshold` <= `buffer.count`.
  private let threshold: Int
  
  /// Text output target object to which the character sequences are written as a stream of bytes.
  private var target: TextOutputTarget?
  
  /// The URL of this text output.
  open var url: URL?
  
  /// Returns the characters that are currently in the buffer as a string.
  open var currentBuffer: String {
    return String(utf16CodeUnits: self.buffer, count: self.next)
  }

  internal init() {
    self.buffer = []
    self.next = 0
    self.threshold = 0
    self.target = nil
    self.url = nil
  }
  
  public convenience init(output: BinaryOutput, capacity: Int = 4096) {
    self.init(target: UTF8EncodedTarget(output: output),
              url: output.url as URL?,
              capacity: capacity,
              threshold: capacity)
  }
  
  public init(target: TextOutputTarget,
              url: URL? = nil,
              capacity: Int = 4096,
              threshold: Int = 4096) {
    guard threshold <= capacity else {
      preconditionFailure("TextOutput threshold above capacity")
    }
    self.buffer = Array<UniChar>(repeating: 0, count: capacity)
    self.next = 0
    self.threshold = threshold
    self.target = target
    self.url = url
  }
  
  /// Closes the output object at garbage collection time.
  deinit {
    self.close()
  }
  
  /// Closes the output object. From that point on, writing to the `TextOutput` is still
  /// possible and will be accumulated in the internal buffer.
  open func close() {
    self.flush()
    self.target = nil
  }
  
  @discardableResult open func flush(_ completely: Bool = false) -> Bool {
    if self.target != nil && self.next > 0 {
      let str = String(utf16CodeUnits: self.buffer, count: self.next)
      guard self.target!.writeString(str) else {
        return false
      }
      self.next = 0
    }
    return true
  }
  
  open func write(_ ch: UniChar) -> Bool {
    guard self.writeToBuffer(ch) else {
      return false
    }
    guard self.target == nil || self.next < self.threshold else {
      return self.flush()
    }
    return true
  }
  
  open func writeString(_ str: String) -> Bool {
    for ch in str.utf16 {
      guard self.writeToBuffer(ch) else {
        return false
      }
    }
    guard self.target == nil || self.next < self.threshold else {
      return self.flush()
    }
    return true
  }
  
  fileprivate func writeToBuffer(_ ch: UniChar) -> Bool {
    if self.next < self.buffer.count {
      self.buffer[self.next] = ch
    } else if self.target != nil {
      guard self.flush() else {
        return false
      }
      self.buffer[self.next] = ch
    } else {
      // This happens only for `TextOutput` objects that are not backed by an output stream
      self.buffer.append(ch)
    }
    self.next += 1
    return true
  }
}
