//
//  BinaryOutput.swift
//  LispKit
//
//  Created by Matthias Zenger on 07/06/2016.
//  Copyright © 2016-2017 ObjectHub. All rights reserved.
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
/// `BinaryOutput` represents a binary output port. This class manages a byte buffer of a
/// configurable capacity, and an optional output stream to which all data is eventually
/// written. If there is no output stream, all data will be accumulated in the buffer.
///
open class BinaryOutput {
  
  /// Synchronize access to the buffer
  private var lock = EmbeddedUnfairLock()
  
  /// Buffer into which bytes are written first before they are written into an output stream.
  private var buffer: [UInt8]
  
  /// Index into the buffer indicating the next byte to write.
  private var next: Int
  
  /// The output stream into which content in the buffer is flushed.
  private var output: OutputStream?
  
  /// The URL for the output stream. `url` is nil whenever `output` is nil.
  public let url: URL?
  
  /// Relative paths are relative to the documents folder
  private static let documentsUrl =
    URL(fileURLWithPath:
      NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0])

  /// Initializes a new `BinaryOutput` that is not backed by an output stream.
  public init() {
    self.buffer = [UInt8]()
    self.next = 0
    self.output = nil
    self.url = nil
  }
  
  /// Initializes a new `BinaryOutput` that is backed by an output steam to a file at
  /// a given path. Relative paths are relative to the documents folder. `append` determines
  /// if the content should be appended in case the output stream refers to an existing file.
  /// `capacity` indicates the size of the buffer in terms of the number of bytes.
  public convenience init?(path: String, append: Bool = false, capacity: Int = 4096) {
    self.init(url: URL(fileURLWithPath: path, relativeTo: BinaryOutput.documentsUrl),
              capacity: capacity)
  }
  
  /// Initializes a new `BinaryOutput` that is backed by an output steam to a file at the
  /// given URL. `append` determines if the content should be appended in case the output
  /// stream refers to an existing file. `capacity` indicates the size of the buffer in terms
  /// of the number of bytes.
  public init?(url: URL, append: Bool = false, capacity: Int = 4096) {
    // Create a new output stream
    guard let output = OutputStream(url: url, append: append) else {
      return nil
    }
    // Open the output stream
    output.open()
    // Check that there is space to write data
    guard output.hasSpaceAvailable else {
      output.close()
      return nil
    }
    // Initialize properties
    self.buffer = [UInt8](repeating: 0, count: capacity)
    self.next = 0
    self.output = output
    self.url = url
  }
  
  /// Closes the output stream when the object gets garbage collected.
  deinit {
    self.close()
    self.lock.release()
  }
  
  /// Closes the output stream. From that point on, writing to the `BinaryOutput` is still
  /// possible and will be accumulated in the internal buffer.
  open func close() {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    _ = self.flushBuffer()
    if let output = self.output {
      self.output = nil
      output.close()
    }
  }
  
  /// Returns true if there is at least one byte that can be written into the buffer.
  private func writeable() -> Bool {
    guard self.output == nil || self.next < self.buffer.count else {
      return self.flushBuffer()
    }
    return true
  }
  
  /// Flushes the buffer by writing it into the output steam. For `BinaryOutput` objects that
  /// are not backed by an output stream, flush does nothing.
  @discardableResult open func flush(_ completely: Bool = false) -> Bool {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    return self.flushBuffer(completely)
  }
  
  /// Writes the given byte into the `BinaryOutput`.
  open func write(_ byte: UInt8) -> Bool {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    return self.writeByte(byte)
  }
  
  /// Writes the given sequence of bytes into the `BinaryOutput`.
  open func writeFrom(_ source: [UInt8], start: Int, end: Int) -> Bool {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    guard start < source.count && start <= end else {
      return true
    }
    let to = end > source.count ? source.count : end
    for i in start..<to {
      guard self.writeByte(source[i]) else {
        return false
      }
    }
    return true
  }
  
  /// Returns the bytes that are currently in the buffer as a new byte array.
  open var currentBuffer: [UInt8] {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    return [UInt8](self.buffer[0..<self.next])
  }
  
  /// Writes the given byte into the `BinaryOutput`.
  private func writeByte(_ byte: UInt8) -> Bool {
    guard self.writeable() else {
      return false
    }
    if self.next < self.buffer.count {
      self.buffer[self.next] = byte
    } else {
      // This happens only for `BinaryOutput` objects that are not backed by an output stream
      self.buffer.append(byte)
    }
    self.next += 1
    return true
  }
  
  /// Flushes the buffer by writing it into the output steam. For `BinaryOutput` objects that
  /// are not backed by an output stream, flush does nothing.
  private func flushBuffer(_ completely: Bool = false) -> Bool {
    if let output = self.output , self.next > 0 {
      let result = output.write(&self.buffer, maxLength: self.next * MemoryLayout<UInt8>.size)
      if result < 0 {
        return false
      }
      self.next = 0
    }
    return true
  }
}
