//
//  CharacterLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 23/01/2016.
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


public final class CharacterLibrary: Library {
  
  public override func export() {
    define("char?", Procedure(isChar))
    define("char=?", Procedure(charEquals))
    define("char<?", Procedure(charLessThan))
    define("char>?", Procedure(charGreaterThan))
    define("char<=?", Procedure(charLessThanEquals))
    define("char>=?", Procedure(charGreaterThanEquals))
    define("char-ci=?", Procedure(charCiEquals))
    define("char-ci<?", Procedure(charCiLessThan))
    define("char-ci>?", Procedure(charCiGreaterThan))
    define("char-ci<=?", Procedure(charCiLessThanEquals))
    define("char-ci>=?", Procedure(charCiGreaterThanEquals))
    define("char-alphabetic?", Procedure(charIsAlphabetic))
    define("char-numeric?", Procedure(charIsNumeric))
    define("char-whitespace?", Procedure(charIsWhitespace))
    define("char-upper-case?", Procedure(charIsUpperCase))
    define("char-lower-case?", Procedure(charIsLowerCase))
    define("char->integer", Procedure(charToInteger))
    define("integer->char", Procedure(integerToChar))
    define("char-upcase", Procedure(charUpcase))
    define("char-downcase", Procedure(charDowncase))
  }
  
  func isChar(expr: Expr) -> Expr {
    if case .Char(_) = expr {
      return .True
    }
    return .False
  }
  
  func charEquals(fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asChar() == snd.asChar())
  }
  
  func charLessThan(fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asChar() < snd.asChar())
  }
  
  func charLessThanEquals(fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asChar() <= snd.asChar())
  }
  
  func charGreaterThan(fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asChar() > snd.asChar())
  }
  
  func charGreaterThanEquals(fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asChar() >= snd.asChar())
  }
  
  func charCiEquals(fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asCharStr().lowercaseString == snd.asCharStr().lowercaseString)
  }
  
  func charCiLessThan(fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asCharStr().lowercaseString < snd.asCharStr().lowercaseString)
  }
  
  func charCiLessThanEquals(fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asCharStr().lowercaseString <= snd.asCharStr().lowercaseString)
  }
  
  func charCiGreaterThan(fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asCharStr().lowercaseString > snd.asCharStr().lowercaseString)
  }
  
  func charCiGreaterThanEquals(fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asCharStr().lowercaseString >= snd.asCharStr().lowercaseString)
  }
  
  func charIsAlphabetic(expr: Expr) throws -> Expr {
    return .Boolean(LETTERS.characterIsMember(try expr.asChar()))
  }
  
  func charIsNumeric(expr: Expr) throws -> Expr {
    return .Boolean(DIGITS.characterIsMember(try expr.asChar()))
  }
  
  func charIsWhitespace(expr: Expr) throws -> Expr {
    return .Boolean(WHITESPACES.characterIsMember(try expr.asChar()))
  }
  
  func charIsUpperCase(expr: Expr) throws -> Expr {
    return .Boolean(UPPER_LETTERS.characterIsMember(try expr.asChar()))
  }
  
  func charIsLowerCase(expr: Expr) throws -> Expr {
    return .Boolean(LOWER_LETTERS.characterIsMember(try expr.asChar()))
  }
  
  func charToInteger(expr: Expr) throws -> Expr {
    return .Fixnum(Int64(try expr.asChar()))
  }
  
  func integerToChar(expr: Expr) throws -> Expr {
    return .Char(UInt16(try expr.asInteger()))
  }
  
  func charUpcase(expr: Expr) throws -> Expr {
    return .Char(try expr.asCharStr().uppercaseString.utf16.first!)
  }
  
  func charDowncase(expr: Expr) throws -> Expr {
    return .Char(try expr.asCharStr().lowercaseString.utf16.first!)
  }
}
