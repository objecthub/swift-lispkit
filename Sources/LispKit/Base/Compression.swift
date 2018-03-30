//
//  Compression.swift
//  LispKit
//
//  Created by Matthias Zenger on 01/12/2017.
//
//  Based on DataCompression by Markus Wanke:
//
//  libcompression wrapper as an extension for the `Data` type
//  (ZLIB, LZFSE, LZMA, LZ4, deflate, RFC-1950, RFC-1951)
//
//  Created by Markus Wanke, 2016/12/05
//
//                Apache License, Version 2.0
//
//  Copyright 2016, Markus Wanke
//
//  Licensed under the Apache License, Version 2.0 (the "License");
//  you may not use this file except in compliance with the License.
//  You may obtain a copy of the License at
//
//  http://www.apache.org/licenses/LICENSE-2.0
//
//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS,
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  See the License for the specific language governing permissions and
//  limitations under the License.
//

import Foundation
import Compression

/// Extensions providing compression and decompression functionality.
public extension Data {
  
  /// Returns a new `Data` object containing the same data compressed via zlib's deflate
  /// algorithm fixed at compression level 5.
  public func deflate() -> Data? {
    return self.withUnsafeBytes { (ptr: UnsafePointer<UInt8>) -> Data? in
      return Data.process(operation: COMPRESSION_STREAM_ENCODE,
                          algorithm: COMPRESSION_ZLIB,
                          ptr: ptr,
                          count: count)
    }
  }
  
  /// Returns a new `Data` object containing the decompressed data of this object, which is
  /// assumed to be a raw deflate stream. Decompression is done using the zlib's deflate
  /// algorithm.
  public func inflate() -> Data? {
    return self.withUnsafeBytes { (ptr: UnsafePointer<UInt8>) -> Data? in
      return Data.process(operation: COMPRESSION_STREAM_DECODE,
                          algorithm: COMPRESSION_ZLIB,
                          ptr: ptr,
                          count: count)
    }
  }
  
  /// Returns a new `Data` object containing the same data compressed via zlib's deflate
  /// algorithm fixed at compression level 5. This method is packaging up the raw deflate
  /// stream in zip format.
  public func zip() -> Data? {
    var crc = self.adler32Checksum().bigEndian
    var res = Data(bytes: [0x78, 0x5e])
    guard let deflated = self.deflate() else {
      return nil
    }
    res.append(deflated)
    res.append(Data(bytes: &crc, count: MemoryLayout<UInt32>.size))
    return res
  }
  
  /// Returns a new `Data` object containing the decompressed data of this object, which is
  /// assumed to be a raw deflate stream in zip format. Decompression is done using the zlib's
  /// deflate algorithm.
  public func unzip(skipCheckSumValidation: Bool = true) -> Data? {
    // 2 byte header + 4 byte adler32 checksum
    let overhead = 2 + MemoryLayout<UInt32>.size
    guard count > overhead else {
      return nil
    }
    let header: UInt16 = withUnsafeBytes { (ptr: UnsafePointer<UInt16>) -> UInt16 in
      return ptr.pointee.bigEndian
    }
    // check for the deflate stream bit and the header checksum
    guard header >> 8 & 0b1111 == 0b1000, header % 31 == 0 else {
      return nil
    }
    let cresult: Data? = withUnsafeBytes { (ptr: UnsafePointer<UInt8>) -> Data? in
      return Data.process(operation: COMPRESSION_STREAM_DECODE,
                          algorithm: COMPRESSION_ZLIB,
                          ptr: ptr.advanced(by: 2),
                          count: count - overhead)
    }
    guard let inflated = cresult else {
      return nil
    }
    if skipCheckSumValidation {
      return inflated
    } else {
      let cksum: UInt32 = withUnsafeBytes { (ptr: UnsafePointer<UInt8>) -> UInt32 in
        let last = ptr.advanced(by: count - MemoryLayout<UInt32>.size)
        return last.withMemoryRebound(to: UInt32.self, capacity: 1) { (intPtr) -> UInt32 in
          return intPtr.pointee.bigEndian
        }
      }
      return cksum == inflated.adler32Checksum() ? inflated : nil
    }
  }
  
  /// Handles all low level compression and decompression operations.
  private static func process(operation: compression_stream_operation,
                              algorithm: compression_algorithm,
                              ptr: UnsafePointer<UInt8>,
                              count: Int) -> Data? {
    guard operation == COMPRESSION_STREAM_ENCODE || count > 0 else {
      return nil
    }
    let streamBase = UnsafeMutablePointer<compression_stream>.allocate(capacity: 1)
    defer {
      streamBase.deallocate()
    }
    var stream = streamBase.pointee
    let status = compression_stream_init(&stream, operation, algorithm)
    guard status != COMPRESSION_STATUS_ERROR else {
      return nil
    }
    defer {
      compression_stream_destroy(&stream)
    }
    let bufferSize = count < 64 ? 64 : count > 32768 ? 32768 : count
    let buffer = UnsafeMutablePointer<UInt8>.allocate(capacity: bufferSize)
    defer {
      buffer.deallocate()
    }
    stream.dst_ptr  = buffer
    stream.dst_size = bufferSize
    stream.src_ptr  = ptr
    stream.src_size = count
    var res = Data()
    let flags: Int32 = Int32(COMPRESSION_STREAM_FINALIZE.rawValue)
    while true {
      switch compression_stream_process(&stream, flags) {
        case COMPRESSION_STATUS_OK:
          guard stream.dst_size == 0 else {
            return nil
          }
          res.append(buffer, count: stream.dst_ptr - buffer)
          stream.dst_ptr = buffer
          stream.dst_size = bufferSize
        case COMPRESSION_STATUS_END:
          res.append(buffer, count: stream.dst_ptr - buffer)
          return res
        default:
          return nil
      }
    }
  }
  
  private func adler32Checksum() -> UInt32 {
    let modAdler: UInt32 = 65521
    var a: UInt32 = 1
    var b: UInt32 = 0
    for byte in self {
      a += UInt32(byte)
      if a >= modAdler {
        a = a % modAdler
      }
      b += a
      if b >= modAdler {
        b = b % modAdler
      }
    }
    return (b << 16) | a
  }
}

