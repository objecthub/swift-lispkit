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
public class TextOutput {
  
  /// Internal character buffer.
  private var buffer: [UniChar]
  
  /// Index of the next character to write.
  private var next: Int
  
  /// Binary output object to which text is written as a UTF8 encoded stream of bytes.
  private var output: BinaryOutput?
  
  /// The URL of this text output.
  public var url: NSURL? {
    return self.output?.url
  }
  
  /// Returns the characters that are currently in the buffer as a string.
  public var currentBuffer: String {
    return String(utf16CodeUnits: self.buffer, count: self.next)
  }

  internal init() {
    self.buffer = []
    self.next = 0
    self.output = nil
  }
  
  public init?(output: BinaryOutput, capacity: Int = 4096) {
    self.buffer = Array<UniChar>(count: capacity, repeatedValue: 0)
    self.next = 0
    self.output = output
  }
  
  /// Closes the output object at garbage collection time.
  deinit {
    self.close()
  }
  
  /// Closes the output object. From that point on, writing to the `TextOutput` is still
  /// possible and will be accumulated in the internal buffer.
  public func close() {
    self.flush()
    self.output = nil
  }
  
  private func writeable() -> Bool {
    guard self.output == nil || self.next < self.buffer.count else {
      return self.flush()
    }
    return true
  }
  
  public func flush() -> Bool {
    if let output = self.output where self.next > 0 {
      let str = String(utf16CodeUnits: self.buffer, count: self.next)
      for byte in str.utf8 {
        guard output.write(byte) else {
          return false
        }
      }
      self.next = 0
    }
    return true
  }

  public func write(ch: UniChar) -> Bool {
    guard self.writeable() else {
      return false
    }
    if self.next < self.buffer.count {
      self.buffer[self.next] = ch
    } else {
      // This happens only for `TextOutput` objects that are not backed by an output stream
      self.buffer.append(ch)
    }
    self.next += 1
    return true
  }
  
  public func writeString(str: String) -> Bool {
    for ch in str.utf16 {
      guard self.write(ch) else {
        return false
      }
    }
    return true
  }
}
