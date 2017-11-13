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
  private let postfix: String
  private let separator: String?
  private var initial: String?
  private var requiresSeparator: Bool
  
  public init(prefix: String = "",
              postfix: String = "",
              separator: String? = nil,
              initial: String? = nil) {
    self.buffer = prefix
    self.postfix = postfix
    self.separator = separator
    self.initial = initial
    self.requiresSeparator = false
  }
  
  public var description: String {
    return buffer + postfix
  }
  
  public mutating func append(_ strs: String...) {
    if self.requiresSeparator {
      self.buffer += self.separator!
    } else {
      self.requiresSeparator = self.separator != nil
      if self.initial != nil {
        self.buffer += self.initial!
        self.initial = nil
      }
    }
    for str in strs {
      self.buffer += str
    }
  }
  
  public mutating func append(_ str: String, width: Int, alignRight: Bool = false) {
    let pad = width - str.count
    if alignRight {
      self.append(StringBuilder.padding(pad), str)
    } else {
      self.append(str, StringBuilder.padding(pad))
    }
  }
  
  public mutating func append(_ num: Int) {
    self.append(String(num))
  }
  
  public mutating func append(_ num: Int, width: Int, alignRight: Bool = false) {
    self.append(String(num), width: width, alignRight: alignRight)
  }
  
  public mutating func appendNewline() {
    if self.requiresSeparator {
      self.buffer += separator!
      self.requiresSeparator = false
    }
    self.buffer += "\n"
  }

  /// Returns a string with `n` spaces.
  private static func padding(_ n: Int) -> String {
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
      case 7:
        return "       "
      case 8:
        return "        "
      default:
        return String(repeating: " ", count: n)
    }
  }
}
