//
//  StringLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 22/01/2016.
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


public final class StringLibrary: Library {
  
  public override func export() {
    define("string?", Procedure(isString))
    define("string", Procedure(string))
    define("string-ref", Procedure(stringRef))
    define("string-length", Procedure(stringLength))
    define("string-append", Procedure(stringAppend))
    define("string=?", Procedure(stringEquals))
    define("string<?", Procedure(stringLessThan))
    define("string>?", Procedure(stringLessThanEquals))
    define("string<=?", Procedure(stringGreaterThan))
    define("string>=?", Procedure(stringGreaterThanEquals))
    define("string-ci=?", Procedure(stringCiEquals))
    define("string-ci<?", Procedure(stringCiLessThan))
    define("string-ci>?", Procedure(stringCiLessThanEquals))
    define("string-ci<=?", Procedure(stringCiGreaterThan))
    define("string-ci>=?", Procedure(stringCiGreaterThanEquals))
    define("string-contains?", Procedure(stringContains))
    define("string-upcase", Procedure(stringUpcase))
    define("string-downcase", Procedure(stringDowncase))
    define("list->string", Procedure(listToString))
    define("string->list", Procedure(stringToList))
    define("substring", Procedure(substring))
  }

  func isString(expr: Expr) -> Expr {
    if case .Str(_) = expr {
      return .True
    }
    return .False
  }
  
  func string(exprs: Arguments) throws -> Expr {
    var uniChars: [UniChar] = []
    for expr in exprs {
      uniChars.append(try expr.asChar())
    }
    return .Str(Box(String(utf16CodeUnits: uniChars, count: uniChars.count)))
  }
  
  func stringLength(expr: Expr) throws -> Expr {
    return .Fixnum(Int64(try expr.asStr().utf16.count))
  }
  
  func stringRef(expr: Expr, _ index: Expr) throws -> Expr {
    let str = try expr.asStr().utf16
    let k = try index.asInt()
    let i = str.startIndex.advancedBy(k)
    guard i < str.endIndex else {
      throw EvalError.IndexOutOfBounds(Int64(k), Int64(str.count - 1), expr)
    }
    return .Char(str[i])
  }
  
  func stringAppend(exprs: Arguments) throws -> Expr {
    var res = ""
    for expr in exprs {
      res.appendContentsOf(try expr.asStr())
    }
    return .Str(Box(res))
  }
  
  func stringEquals(fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asStr() == snd.asStr())
  }
  
  func stringLessThan(fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asStr() < snd.asStr())
  }
  
  func stringLessThanEquals(fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asStr() <= snd.asStr())
  }
  
  func stringGreaterThan(fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asStr() > snd.asStr())
  }
  
  func stringGreaterThanEquals(fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asStr() >= snd.asStr())
  }
  
  func stringCiEquals(fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asStr().lowercaseString == snd.asStr().lowercaseString)
  }
  
  func stringCiLessThan(fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asStr().lowercaseString < snd.asStr().lowercaseString)
  }
  
  func stringCiLessThanEquals(fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asStr().lowercaseString <= snd.asStr().lowercaseString)
  }
  
  func stringCiGreaterThan(fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asStr().lowercaseString > snd.asStr().lowercaseString)
  }
  
  func stringCiGreaterThanEquals(fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asStr().lowercaseString >= snd.asStr().lowercaseString)
  }
  
  func stringContains(expr: Expr, _ other: Expr) throws -> Expr {
    return .Boolean(try expr.asStr().containsString(try other.asStr()))
  }
  
  func stringUpcase(expr: Expr) throws -> Expr {
    return .Str(Box(try expr.asStr().uppercaseString))
  }
  
  func stringDowncase(expr: Expr) throws -> Expr {
    return .Str(Box(try expr.asStr().lowercaseString))
  }
  
  func stringToList(expr: Expr) throws -> Expr {
    var res = Expr.Null
    let str = try expr.asStr().utf16
    for ch in str.reverse() {
      res = .Pair(.Char(ch), res)
    }
    return res
  }
  
  func listToString(expr: Expr) throws -> Expr {
    var list = expr
    var uniChars: [UniChar] = []
    while case .Pair(let ch, let next) = list {
      uniChars.append(try ch.asChar())
      list = next
    }
    guard list.isNull else {
      throw EvalError.TypeError(expr, [.ProperListType])
    }
    return .Str(Box(String(utf16CodeUnits: uniChars, count: uniChars.count)))
  }
  
  func substring(expr: Expr, _ start: Expr, _ end: Expr?) throws -> Expr {
    let str = try expr.asStr().utf16
    let s = str.startIndex.advancedBy(try start.asInt())
    guard s < str.endIndex else {
      throw EvalError.IndexOutOfBounds(try start.asInteger(), Int64(str.count), expr)
    }
    let e = end == nil ? str.endIndex : str.startIndex.advancedBy(try end!.asInt())
    guard e <= str.endIndex && s <= e else {
      // TODO: Fix error (should define [s..str.count] as bounds for e
      throw EvalError.IndexOutOfBounds(try end!.asInteger(), Int64(str.count), expr)
    }
    var uniChars: [UniChar] = []
    for ch in str[s..<e] {
      uniChars.append(ch)
    }
    return .Str(Box(String(utf16CodeUnits: uniChars, count: uniChars.count)))
  }
}
