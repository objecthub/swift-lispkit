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


public final class CharacterLibrary: NativeLibrary {
  
  public override func export() {
    define(Procedure("char?", isChar))
    define(Procedure("char=?", charEquals))
    define(Procedure("char<?", charLessThan))
    define(Procedure("char>?", charGreaterThan))
    define(Procedure("char<=?", charLessThanEquals))
    define(Procedure("char>=?", charGreaterThanEquals))
    define(Procedure("char-ci=?", charCiEquals))
    define(Procedure("char-ci<?", charCiLessThan))
    define(Procedure("char-ci>?", charCiGreaterThan))
    define(Procedure("char-ci<=?", charCiLessThanEquals))
    define(Procedure("char-ci>=?", charCiGreaterThanEquals))
    define(Procedure("char-alphabetic?", charIsAlphabetic))
    define(Procedure("char-numeric?", charIsNumeric))
    define(Procedure("char-whitespace?", charIsWhitespace))
    define(Procedure("char-upper-case?", charIsUpperCase))
    define(Procedure("char-lower-case?", charIsLowerCase))
    define(Procedure("char->integer", charToInteger))
    define(Procedure("integer->char", integerToChar))
    define(Procedure("char-upcase", charUpcase))
    define(Procedure("char-downcase", charDowncase))
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
