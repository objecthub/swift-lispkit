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
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "character"]
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("char?", isChar))
    self.define(Procedure("char=?", charEquals))
    self.define(Procedure("char<?", charLessThan))
    self.define(Procedure("char>?", charGreaterThan))
    self.define(Procedure("char<=?", charLessThanEquals))
    self.define(Procedure("char>=?", charGreaterThanEquals))
    self.define(Procedure("char-ci=?", charCiEquals))
    self.define(Procedure("char-ci<?", charCiLessThan))
    self.define(Procedure("char-ci>?", charCiGreaterThan))
    self.define(Procedure("char-ci<=?", charCiLessThanEquals))
    self.define(Procedure("char-ci>=?", charCiGreaterThanEquals))
    self.define(Procedure("char-alphabetic?", charIsAlphabetic))
    self.define(Procedure("char-numeric?", charIsNumeric))
    self.define(Procedure("char-whitespace?", charIsWhitespace))
    self.define(Procedure("char-upper-case?", charIsUpperCase))
    self.define(Procedure("char-lower-case?", charIsLowerCase))
    self.define(Procedure("char->integer", charToInteger))
    self.define(Procedure("integer->char", integerToChar))
    self.define(Procedure("char-upcase", charUpcase))
    self.define(Procedure("char-downcase", charDowncase))
  }
  
  func isChar(_ expr: Expr) -> Expr {
    if case .char(_) = expr {
      return .true
    }
    return .false
  }
  
  func charEquals(_ fst: Expr, _ snd: Expr) throws -> Expr {
    return .makeBoolean(try fst.asUniChar() == snd.asUniChar())
  }
  
  func charLessThan(_ fst: Expr, _ snd: Expr) throws -> Expr {
    return .makeBoolean(try fst.asUniChar() < snd.asUniChar())
  }
  
  func charLessThanEquals(_ fst: Expr, _ snd: Expr) throws -> Expr {
    return .makeBoolean(try fst.asUniChar() <= snd.asUniChar())
  }
  
  func charGreaterThan(_ fst: Expr, _ snd: Expr) throws -> Expr {
    return .makeBoolean(try fst.asUniChar() > snd.asUniChar())
  }
  
  func charGreaterThanEquals(_ fst: Expr, _ snd: Expr) throws -> Expr {
    return .makeBoolean(try fst.asUniChar() >= snd.asUniChar())
  }
  
  func charCiEquals(_ fst: Expr, _ snd: Expr) throws -> Expr {
    return .makeBoolean(try fst.charAsString().lowercased() == snd.charAsString().lowercased())
  }
  
  func charCiLessThan(_ fst: Expr, _ snd: Expr) throws -> Expr {
    return .makeBoolean(try fst.charAsString().lowercased() < snd.charAsString().lowercased())
  }
  
  func charCiLessThanEquals(_ fst: Expr, _ snd: Expr) throws -> Expr {
    return .makeBoolean(try fst.charAsString().lowercased() <= snd.charAsString().lowercased())
  }
  
  func charCiGreaterThan(_ fst: Expr, _ snd: Expr) throws -> Expr {
    return .makeBoolean(try fst.charAsString().lowercased() > snd.charAsString().lowercased())
  }
  
  func charCiGreaterThanEquals(_ fst: Expr, _ snd: Expr) throws -> Expr {
    return .makeBoolean(try fst.charAsString().lowercased() >= snd.charAsString().lowercased())
  }
  
  func charIsAlphabetic(_ expr: Expr) throws -> Expr {
    return .makeBoolean(LETTERS.contains(unicodeScalar(try expr.asUniChar())))
  }
  
  func charIsNumeric(_ expr: Expr) throws -> Expr {
    return .makeBoolean(DIGITS.contains(unicodeScalar(try expr.asUniChar())))
  }
  
  func charIsWhitespace(_ expr: Expr) throws -> Expr {
    return .makeBoolean(WHITESPACES.contains(unicodeScalar(try expr.asUniChar())))
  }
  
  func charIsUpperCase(_ expr: Expr) throws -> Expr {
    return .makeBoolean(UPPER_LETTERS.contains(unicodeScalar(try expr.asUniChar())))
  }
  
  func charIsLowerCase(_ expr: Expr) throws -> Expr {
    return .makeBoolean(LOWER_LETTERS.contains(unicodeScalar(try expr.asUniChar())))
  }
  
  func charToInteger(_ expr: Expr) throws -> Expr {
    return .fixnum(Int64(try expr.asUniChar()))
  }
  
  func integerToChar(_ expr: Expr) throws -> Expr {
    return .char(UInt16(try expr.asInt64()))
  }
  
  func charUpcase(_ expr: Expr) throws -> Expr {
    return .char(try expr.charAsString().uppercased().utf16.first!)
  }
  
  func charDowncase(_ expr: Expr) throws -> Expr {
    return .char(try expr.charAsString().lowercased().utf16.first!)
  }
}
