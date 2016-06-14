//
//  BinaryInput.swift
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
/// `BinaryInput` represents a binary input port. This struct manages a byte buffer, an
/// end of file indicator and an optional input stream from which all data is read. If there is
/// no input stream, all data is contained in the buffer which gets initialized when the
/// `BinaryInput` object is getting created.
///
public class BinaryInput: GeneratorType {
  
  /// Buffer for fetching bigger chunks of data at once.
  private var buffer: [UInt8]
  
  /// Index into buffer pointing at the next byte to read.
  private var index: Int = 0
  
  /// Eof is true if all bytes have been consumed.
  public private(set) var eof: Bool = false
  
  /// Input stream. If this property is nil, only the content in the buffer is relevant.
  private var input: NSInputStream?
  
  /// The URL for the input stream. `url` is nil whenever `input` is nil.
  public let url: NSURL?
  
  /// Relative paths are relative to the documents folder
  private static let documentsUrl =
    NSURL(fileURLWithPath:
      NSSearchPathForDirectoriesInDomains(.DocumentDirectory, .UserDomainMask, true)[0])
  
  /// Initializes a binary input from a byte array
  public init(data: [UInt8]) {
    self.buffer = data
    self.input = nil
    self.url = nil
  }
  
  /// Initializes a binary input from a binary file given in terms of a local/absolute path.
  /// It is assumed that local paths are local to the documents folder. `capacity` determines
  /// the number of bytes used for caching data.
  public convenience init?(path: String, capacity: Int = 4096) {
    self.init(url: NSURL(fileURLWithPath: path, relativeToURL: BinaryInput.documentsUrl),
              capacity: capacity)
  }
  
  /// Initializes a binary input from a binary file at the given URL. `capacity` determines
  /// the number of bytes used for caching data.
  public init?(url: NSURL, capacity: Int = 4096) {
    // Create a new input stream
    guard let input = NSInputStream(URL: url) else {
      return nil
    }
    // Set up `BinaryInput` object
    self.buffer = [UInt8](count: capacity, repeatedValue: 0)
    self.input = input
    self.url = url
    // Open the stream
    input.open()
    // Read data into the buffer
    if input.hasBytesAvailable {
      let result = input.read(&self.buffer, maxLength: self.buffer.count * sizeof(UInt8))
      if result < 0 {
        self.input = nil
        input.close()
        return nil
      } else if result == 0 {
        self.eof = true
      }
    } else {
      self.eof = true
    }
  }
  
  /// Makes sure that the input stream is closed when the object gets deallocated.
  deinit {
    self.close()
  }
  
  /// Closes the `BinaryInput`.
  public func close() {
    if let input = self.input {
      self.input = nil
      input.close()
    }
  }
  
  /// Guarantees that there is at least one byte to read from the buffer. Returns false if the
  /// end of file is reached or there has been a read error.
  private func readable() -> Bool {
    if self.eof {
      return false
    } else if self.index >= self.buffer.count {
      if let input = self.input where input.hasBytesAvailable {
        let result = input.read(&self.buffer, maxLength: self.buffer.count * sizeof(UInt8))
        guard result >= 0 else {
          return false
        }
        self.index = 0
        if result == 0 {
          self.eof = true
          return false
        }
      } else {
        self.index = 0
        self.eof = true
        return false
      }
    }
    return true
  }
  
  /// Reads the next byte. Returns nil if all input is consumed or an error was encountered.
  /// Clients can disambiguate the current state by checking property `eof`.
  public func read() -> UInt8? {
    return self.next()
  }
  
  /// Reads the next byte. Returns nil if all input is consumed or an error was encountered.
  /// Clients can disambiguate the current state by checking property `eof`.
  public func next() -> UInt8? {
    guard self.readable() else {
      return nil
    }
    self.index += 1
    return self.buffer[self.index - 1]
  }
  
  /// Returns the next byte without consuming it. Returns nil if all input is consumed or an
  /// error was encountered. Clients can disambiguate the current state by checking property `eof`.
  public func peek() -> UInt8? {
    guard self.readable() else {
      return nil
    }
    return self.buffer[self.index]
  }
  
  /// Reads up to `n` bytes into a new byte array. Returns nil if all input is consumed or an
  /// error was encountered. Clients can disambiguate the current state by checking property `eof`.
  public func readMany(n: Int) -> [UInt8]? {
    guard self.readable() else {
      return nil
    }
    var res = [UInt8]()
    for _ in 0..<n {
      res.append(self.buffer[self.index])
      self.index += 1
      guard self.readable() else {
        return self.eof ? res : nil
      }
    }
    return res
  }
  
  /// Reads bytes into `target` starting from `start` up to and excluding `end`. Returns nil if
  /// all input is consumed or an error was encountered. Clients can disambiguate the current
  /// state by checking property `eof`.
  public func readInto(inout target: [UInt8], start: Int = 0, end: Int = Int.max) -> Int? {
    guard self.readable() else {
      return nil
    }
    guard start < target.count && start < end else {
      return 0
    }
    let to = end > target.count ? target.count : end
    for i in start..<to {
      target[i] = self.buffer[self.index]
      self.index += 1
      guard self.readable() else {
        return self.eof ? i - start + 1 : nil
      }
    }
    return to - start
  }
  
  /// Returns a generator for all the bytes of this `BinaryInput` object.
  public func generate() -> AnyGenerator<UInt8> {
    return AnyGenerator(body: self.read)
  }
}
