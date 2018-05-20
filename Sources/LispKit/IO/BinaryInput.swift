//
//  BinaryInput.swift
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
/// `BinaryInput` represents a binary input port. This class manages a byte buffer, an
/// end of file indicator and an optional binary input source from which all data is read.
/// If there is no input source, all data is contained in the buffer which gets initialized
/// when the `BinaryInput` object is getting created.
///
open class BinaryInput: IteratorProtocol {
  
  /// Buffer for fetching bigger chunks of data at once.
  private var buffer: [UInt8]
  
  /// Size of the buffer; invariant `bufferSize` <= `buffer.count`
  private var bufferSize: Int = 0
  
  /// Index into buffer pointing at the next byte to read.
  private var index: Int = 0
  
  /// Eof is true if all bytes have been consumed.
  public private(set) var eof: Bool = false
  
  /// Input stream. If this property is nil, only the content in the buffer is relevant.
  private var input: BinaryInputSource?
  
  /// The URL for the input stream. `url` is nil whenever `input` is nil.
  public let url: URL?
  
  /// Callback to check if the binary input should abort its operation.
  public let isAborted: () -> Bool
  
  /// Relative paths are relative to the documents folder
  private static let documentsUrl =
    URL(fileURLWithPath:
      NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0])
  
  /// A default abortion callback
  public static func neverAbort() -> Bool {
    return false
  }
  
  /// Initializes a binary input from a byte array
  public init(data: [UInt8],
              abortionCallback: @escaping () -> Bool = BinaryInput.neverAbort) {
    self.buffer = data
    self.bufferSize = data.count
    self.input = nil
    self.url = nil
    self.isAborted = abortionCallback
  }
  
  /// Initializes a binary input from a binary file given in terms of a local/absolute path.
  /// It is assumed that local paths are local to the documents folder. `capacity` determines
  /// the number of bytes used for caching data.
  public convenience init?(path: String,
                           capacity: Int = 4096,
                           abortionCallback: @escaping () -> Bool = BinaryInput.neverAbort) {
    self.init(url: URL(fileURLWithPath: path, relativeTo: BinaryInput.documentsUrl),
              capacity: capacity,
              abortionCallback: abortionCallback)
  }
  
  /// Initializes a binary input from a binary file at the given URL. `capacity` determines
  /// the number of bytes used for caching data.
  public convenience init?(url: URL,
                           capacity: Int = 4096,
                           abortionCallback: @escaping () -> Bool = BinaryInput.neverAbort) {
    switch url.scheme {
      case "http"?, "https"?:
        // Create a new HTTP input stream
        guard let input = HTTPInputStream(url: url) else {
          return nil
        }
        self.init(source: input, url: url, capacity: capacity, abortionCallback: abortionCallback)
      default:
        // Check if the file at the given URL exists
        guard (try? url.checkResourceIsReachable()) ?? false else {
          return nil
        }
        // Create a new file input stream
        guard let input = InputStream(url: url) else {
          return nil
        }
        self.init(source: input, url: url, capacity: capacity, abortionCallback: abortionCallback)
    }
  }
  
  /// Initializes a binary input from a binary file at the given URL. `capacity` determines
  /// the number of bytes used for caching data.
  public init?(source: BinaryInputSource,
               url: URL,
               capacity: Int = 4096,
               abortionCallback: @escaping () -> Bool = BinaryInput.neverAbort) {
    // Set up `BinaryInput` object
    self.buffer = [UInt8](repeating: 0, count: capacity)
    self.input = source
    self.url = url
    self.isAborted = abortionCallback
    // Open the stream
    source.open()
    // Read data into the buffer
    if source.hasBytesAvailable {
      let result = source.read(&self.buffer,
                               maxLength: self.buffer.count * MemoryLayout<UInt8>.size)
      if result < 0 {
        self.input = nil
        source.close()
        return nil
      } else if result == 0 {
        self.eof = true
      } else {
        self.bufferSize = result
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
  open func close() {
    if let input = self.input {
      self.input = nil
      input.close()
    }
  }
  
  /// Reads the next byte. Returns nil if all input is consumed or an error was encountered.
  /// Clients can disambiguate the current state by checking property `eof`.
  open func read() -> UInt8? {
    return self.next()
  }
  
  /// Reads the next byte. Returns nil if all input is consumed or an error was encountered.
  /// Clients can disambiguate the current state by checking property `eof`.
  open func next() -> UInt8? {
    guard self.readable() else {
      return nil
    }
    self.index += 1
    return self.buffer[self.index - 1]
  }
  
  /// Returns the next byte without consuming it. Returns nil if all input is consumed or an
  /// error was encountered. Clients can disambiguate the current state by checking property `eof`.
  open func peek() -> UInt8? {
    guard self.readable() else {
      return nil
    }
    return self.buffer[self.index]
  }
  
  /// Reads up to `n` bytes into a new byte array. Returns nil if all input is consumed or an
  /// error was encountered. Clients can disambiguate the current state by checking property `eof`.
  open func readMany(_ n: Int) -> [UInt8]? {
    assert(n >= 0, "BinaryInput.readMany called with negative count")
    guard n > 0 else {
      return [UInt8]()
    }
    var res = [UInt8]()
    for _ in 0..<n {
      guard self.readable() else {
        return (res.count == 0 || !self.eof) ? nil : res
      }
      res.append(self.buffer[self.index])
      self.index += 1
    }
    return res
  }
  
  /// Reads bytes into `target` starting from `start` up to and excluding `end`. Returns nil if
  /// all input is consumed or an error was encountered. Clients can disambiguate the current
  /// state by checking property `eof`.
  open func readInto(_ target: inout [UInt8], start: Int = 0, end: Int = Int.max) -> Int? {
    guard start < target.count && start < end else {
      return 0
    }
    let to = end > target.count ? target.count : end
    for i in start..<to {
      guard self.readable() else {
        return (i == start || !self.eof) ? nil : (i - start + 1)
      }
      target[i] = self.buffer[self.index]
      self.index += 1
    }
    return to - start
  }
  
  open var readMightBlock: Bool {
    if self.eof {
      return false
    } else if self.index >= self.bufferSize {
      return self.input?.hasBytesAvailable ?? false
    } else {
      return false
    }
  }
  
  /// Guarantees that there is at least one byte to read from the buffer. Returns false if the
  /// end of file is reached or there has been a read error.
  private func readable() -> Bool {
    if self.eof || self.isAborted() {
      return false
    } else if self.index >= self.bufferSize {
      if let input = self.input, input.hasBytesAvailable {
        let result = input.read(&self.buffer,
                                maxLength: self.buffer.count * MemoryLayout<UInt8>.size)
        guard result >= 0 else {
          return false
        }
        self.bufferSize = result
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
  
  /// Returns a function that decodes the binary input as UTF8 and returns strings of at most
  /// `length` characters (where a character is a unicode scalar).
  public func textProvider(_ length: Int) -> () -> String? {
    var codec = UTF8()
    var this = self
    return {
      var str = ""
      for _ in 0..<length {
        switch codec.decode(&this) {
          case .scalarValue (let scalar):
            str.append(String(scalar))
          case .emptyInput:
            return str
          case .error:
            return nil
        }
      }
      return str
    }
  }
}
