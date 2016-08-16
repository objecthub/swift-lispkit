//
//  TextInput.swift
//  LispKit
//
//  Created by Matthias Zenger on 18/06/2016.
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
/// `TextInput` implements a buffered textual input port. Textual input ports are
/// not stream-based. They either get initialized with a string, or their content is read
/// from a file when the `TextInput` object is initialized.
///
public class TextInput {
  
  /// Internal character buffer.
  private var buffer: String.UTF16View
  
  /// Index into the buffer pointing at the next character to read.
  private var next: String.UTF16View.Index
  
  /// Eof is true if all bytes have been consumed.
  public private(set) var eof: Bool = false
  
  /// Text provider. If this property is nil, only the content in the buffer is relevant.
  private var source: TextInputSource?
  
  /// The URL of this text input object.
  public var url: NSURL?
  
  public init(string: String) {
    self.buffer = string.utf16
    self.next = self.buffer.startIndex
    self.source = nil
    self.url = nil
  }
  
  public convenience init(input: BinaryInput, capacity: Int = 4096) {
    self.init(source: UTF8EncodedSource(input: input, length: capacity), url: input.url)
    self.eof = input.eof
  }
  
  public init(source: TextInputSource, url: NSURL? = nil) {
    self.buffer = "".utf16
    self.next = self.buffer.startIndex
    self.eof = false
    self.source = source
    self.url = url
  }
  
  /// Makes sure that the input object is closed at garbage collection time.
  deinit {
    self.close()
  }
  
  /// Closes the `TextInput` object.
  public func close() {
    self.source = nil
  }
  
  public func read() -> UniChar? {
    guard self.readable() else {
      return nil
    }
    let res = self.buffer[self.next]
    self.next = self.next.successor()
    return res
  }
  
  public func peek() -> UniChar? {
    guard self.readable() else {
      return nil
    }
    return self.buffer[self.next]
  }
  
  public func readString(n: Int) -> String? {
    guard self.readable() else {
      return nil
    }
    var res: [UniChar] = []
    for _ in 0..<n {
      res.append(self.buffer[self.next])
      self.next = self.next.successor()
      guard self.readable() else {
        return self.eof ? String(utf16CodeUnits: res, count: res.count) : nil
      }
    }
    return String(utf16CodeUnits: res, count: res.count)
  }
  
  public func readLine() -> String? {
    guard self.readable() else {
      return nil
    }
    var res: [UniChar] = []
    var last: UniChar = 0
    repeat {
      let ch = self.buffer[self.next]
      self.next = self.next.successor()
      if (ch == EOL_CH) {
        return String(utf16CodeUnits: res, count: (last == RET_CH) ? res.count - 1 : res.count)
      }
      res.append(ch)
      last = ch
    } while self.readable()
    return self.eof ? String(utf16CodeUnits: res, count: res.count) : nil
  }
  
  public var readMightBlock: Bool {
    if self.eof {
      return false
    } else if self.next >= self.buffer.endIndex {
      return self.source?.nextReadMightBlock ?? false
    } else {
      return false
    }
  }
  
  private func readable() -> Bool {
    if self.eof {
      return false
    } else if self.next >= self.buffer.endIndex {
      guard self.source != nil else {
        self.buffer = "".utf16
        self.next = self.buffer.startIndex
        self.eof = true
        return false
      }
      guard let str = self.source!.readString() else {
        return false
      }
      self.buffer = str.utf16
      self.next = self.buffer.startIndex
      if self.next >= self.buffer.endIndex {
        self.eof = true
        return false
      }
    }
    return true
  }
}
