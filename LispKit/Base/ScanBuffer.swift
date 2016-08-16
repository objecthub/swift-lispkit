//
//  ScanBuffer.swift
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


public struct ScanBuffer {
  private var buffer: [UniChar]
  public private(set) var index: Int
  
  public init(capacity: Int = 256) {
    self.buffer = Array<UniChar>(count: capacity, repeatedValue: 0)
    self.index = 0
  }
  
  public mutating func reset() {
    if self.index > 0 {
      self.buffer[0] = self.buffer[self.index - 1]
      self.index = 1
    }
  }
  
  public mutating func append(ch: UniChar) {
    if self.index < self.buffer.count {
      self.buffer[self.index] = ch
    } else {
      self.buffer.append(ch)
    }
    self.index += 1
  }
  
  public var stringValue: String {
    return self.index > 0 ? String(utf16CodeUnits: self.buffer, count: self.index - 1) : ""
  }
  
  public func stringStartingAt(start: Int) -> String {
    guard self.index > start else {
      return ""
    }
    let temp = Array(self.buffer[start..<self.index - 1])
    return String(utf16CodeUnits: temp, count: temp.count)
  }
}
