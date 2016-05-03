//
//  StringBuilder.swift
//  LispKit
//
//  Created by Matthias Zenger on 12/04/2016.
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

///
/// Utility class for building strings incrementally.
///
public struct StringBuilder: CustomStringConvertible {
  private var buffer: String
  
  public init(_ initial: String = "") {
    self.buffer = initial
  }
  
  public var description: String {
    return buffer
  }
  
  public mutating func append(str: String) {
    self.buffer += str
  }
  
  public mutating func append(str: String, width: Int, alignRight: Bool = false) {
    let pad = width - str.characters.count
    if alignRight {
      self.appendSpaces(pad)
      self.buffer += str
    } else {
      self.buffer += str
      self.appendSpaces(pad)
    }
  }
  
  public mutating func append(num: Int) {
    self.buffer += String(num)
  }
  
  public mutating func append(num: Int, width: Int, alignRight: Bool = false) {
    self.append(String(num), width: width, alignRight: alignRight)
  }
  
  public mutating func appendSpaces(width: Int) {
    if width > 0 {
      self.buffer.appendContentsOf(StringBuilder.padding(width))
    }
  }
  
  public mutating func appendNewline() {
    self.buffer += "\n"
  }
  
  private static let SPACE: Character = " "

  /// Returns a string with `n` spaces.
  private static func padding(n: Int) -> String {
    switch n {
      case 0:
        return ""
      case 1:
        return " "
      case 2:
        return "  "
      case 3:
        return "   "
      case 4:
        return "    "
      case 5:
        return "     "
      case 6:
        return "      "
      default:
        return String(count: n, repeatedValue: StringBuilder.SPACE)
    }
  }
}