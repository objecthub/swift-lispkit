//
//  SyntaxError.swift
//  LispKit
//
//  Created by Matthias Zenger on 01/03/2018.
//  Copyright © 2018 ObjectHub. All rights reserved.
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
/// Enumeration `SyntaxError` represents syntactical errors emitted by the parser.
///
public enum SyntaxError: Int, Hashable {
  case empty
  case closingParenthesisMissing
  case unexpectedClosingParenthesis
  case unexpectedDot
  case notAByteValue
  case syntaxNotYetSupported

  public var message: String {
    switch self {
      case .empty:
        return "empty input"
      case .closingParenthesisMissing:
        return "closing parenthesis missing"
      case .unexpectedClosingParenthesis:
        return "unexpected closing parenthesis"
      case .unexpectedDot:
        return "unexpected dot"
      case .notAByteValue:
        return "bytevector element not a byte"
      case .syntaxNotYetSupported:
        return "syntax not yet supported"
    }
  }

  public static func ==(_ lhs: SyntaxError, _ rhs: SyntaxError) -> Bool {
    return lhs.rawValue == rhs.rawValue
  }
}

