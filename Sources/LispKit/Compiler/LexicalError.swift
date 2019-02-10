//
//  LexicalError.swift
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
/// Enumeration `LexicalError` represents lexical parsing errors emitted by the
/// scanner.
///
public enum LexicalError: Int, Hashable {
  case empty
  case malformedIdentifier
  case brokenIdentifierEncoding
  case brokenNumberEncoding
  case numberExpected
  case malformedFloatLiteral
  case malformedComplexLiteral
  case malformedStringLiteral
  case malformedCharacterLiteral
  case unknownCharacterLiteral
  case incompleteCharacterLiteral
  case illegalCharacter
  case illegalHexCharacter
  case illegalEscapeSequence
  case illegalEndOfLine
  case tokenNotYetSupported
  case divisionByZero
  case exactComplexNumbersUnsupported
  case unknownDirective

  public var message: String {
    switch self {
      case .empty:
        return "no input available"
      case .malformedIdentifier:
        return "malformed identifier"
      case .brokenIdentifierEncoding:
        return "broken identifier encoding"
      case .brokenNumberEncoding:
        return "broken number encoding"
      case .numberExpected:
        return "expected a number"
      case .malformedFloatLiteral:
        return "malformed floating point number literal"
      case .malformedComplexLiteral:
        return "malformed complex number literal"
      case .malformedStringLiteral:
        return "malformed string literal"
      case .malformedCharacterLiteral:
        return "malformed character literal"
      case .unknownCharacterLiteral:
        return "unknown character literal"
      case .incompleteCharacterLiteral:
        return "incomplete character literal"
      case .illegalCharacter:
        return "illegal character"
      case .illegalHexCharacter:
        return "illegal hex character"
      case .illegalEscapeSequence:
        return "illegal escape sequence"
      case .illegalEndOfLine:
        return "illegal end of line"
      case .tokenNotYetSupported:
        return "token not yet supported"
      case .divisionByZero:
        return "division by zero"
      case .exactComplexNumbersUnsupported:
        return "exact complex numnbers are not supported"
      case .unknownDirective:
        return "unknown directive"
    }
  }

  public static func ==(_ lhs: LexicalError, _ rhs: LexicalError) -> Bool {
    return lhs.rawValue == rhs.rawValue
  }
}

