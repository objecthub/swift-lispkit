//
//  String.swift
//  LispKit
//
//  Created by Matthias Zenger on 06/03/2022.
//  Copyright © 2022 ObjectHub. All rights reserved.
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

extension String {
  
  enum TruncPosition {
    case front
    case middle
    case end
  }
  
  func truncated(limit: Int, pos: TruncPosition = .end, terminator: String = "…") -> String {
    guard self.count > limit else {
      return self
    }
    switch pos {
      case .front:
        return terminator + self.suffix(limit)
      case .middle:
        let numFrontChars = Int(ceil(Float(limit - terminator.count)/2.0))
        let numTailChars = Int(floor(Float(limit - terminator.count)/2.0))
        return "\(self.prefix(numFrontChars))\(terminator)\(self.suffix(numTailChars))"
      case .end:
        return self.prefix(limit) + terminator
    }
  }
  
  func hexDecodedData() -> Data? {
    // Verify that the string only consists of hex numbers
    var count = 0
    for ch in self {
      if let ascii = ch.asciiValue {
        guard (ascii >= 48 && ascii <= 57) ||
              (ascii >= 65 && ascii <= 70) ||
              (ascii >= 97 && ascii <= 102) else {
          return nil
        }
        count += 1
      }
    }
    // The length of the string needs to be even (two hex digits encode one byte)
    guard count % 2 == 0 else {
      return nil
    }
    // Extract the UTF8 characters of this string
    let chars = Array(utf8)
    // Intermediate buffer
    var bytes = [UInt8]()
    bytes.reserveCapacity(count / 2)
    // Initialize lookup map
    let map: [UInt8] = [
      0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, // 01234567
      0x08, 0x09, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // 89:;<=>?
      0x00, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x00, // @ABCDEFG
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // HIJKLMNO
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // PQRSTUVW
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // XYZ[\]^_
      0x00, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x00  // `abcdefg
    ]
    // Extract two characters at a time, map them and turn the result into a byte
    for i in stride(from: 0, to: count, by: 2) {
      let index1 = Int(chars[i] & 0x1F ^ 0x10)
      let index2 = Int(chars[i + 1] & 0x1F ^ 0x10)
      bytes.append(map[index1] << 4 | map[index2])
    }
    return Data(bytes)
  }
}
