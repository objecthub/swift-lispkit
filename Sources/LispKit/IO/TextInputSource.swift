//
//  TextInputSource.swift
//  LispKit
//
//  Created by Matthias Zenger on 19/06/2016.
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


public protocol TextInputSource {
  var nextReadMightBlock: Bool { get }
  mutating func readString() -> String?
}

public struct UTF8EncodedSource: TextInputSource {
  private var input: BinaryInput
  private let length: Int
  private var codec: UTF8
  
  public init(input: BinaryInput, length: Int) {
    self.input = input
    self.length = length
    self.codec = UTF8()
  }
  
  public var nextReadMightBlock: Bool {
    return self.input.readMightBlock
  }
  
  /// Decodes the binary input as UTF8 and returns strings of at most
  /// `length` characters (where a character is a unicode scalar).
  public mutating func readString() -> String? {
    var str = ""
    for _ in 0..<length {
      switch self.codec.decode(&self.input) {
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
