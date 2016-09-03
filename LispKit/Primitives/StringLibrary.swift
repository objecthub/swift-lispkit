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


public final class StringLibrary: NativeLibrary {
  
  public override func export() {
    define(Procedure("string?", isString))
    define(Procedure("make-string", makeString))
    define(Procedure("string", string))
    define(Procedure("string-ref", stringRef))
    define(Procedure("string-length", stringLength))
    define(Procedure("string-append", stringAppend))
    define(Procedure("string=?", stringEquals))
    define(Procedure("string<?", stringLessThan))
    define(Procedure("string>?", stringLessThanEquals))
    define(Procedure("string<=?", stringGreaterThan))
    define(Procedure("string>=?", stringGreaterThanEquals))
    define(Procedure("string-ci=?", stringCiEquals))
    define(Procedure("string-ci<?", stringCiLessThan))
    define(Procedure("string-ci>?", stringCiLessThanEquals))
    define(Procedure("string-ci<=?", stringCiGreaterThan))
    define(Procedure("string-ci>=?", stringCiGreaterThanEquals))
    define(Procedure("string-contains?", stringContains))
    define(Procedure("string-upcase", stringUpcase))
    define(Procedure("string-downcase", stringDowncase))
    define(Procedure("list->string", listToString))
    define(Procedure("string->list", stringToList))
    define(Procedure("substring", substring))
  }
  
  func isString(_ expr: Expr) -> Expr {
    if case .string(_) = expr {
      return .true
    }
    return .false
  }
  
  func makeString(_ k: Expr, ch: Expr?) throws -> Expr {
    let uniChars = Array<UniChar>(repeating: try ch?.asChar() ?? UniChar(" "),
                                  count: try k.asInt())
    return .string(NSMutableString(string: String(utf16CodeUnits: uniChars, count: uniChars.count)))
  }
  
  func string(_ exprs: Arguments) throws -> Expr {
    var uniChars: [UniChar] = []
    for expr in exprs {
      uniChars.append(try expr.asChar())
    }
    return .string(NSMutableString(string: String(utf16CodeUnits: uniChars, count: uniChars.count)))
  }
  
  func stringLength(_ expr: Expr) throws -> Expr {
    return .fixnum(Int64(try expr.asStr().utf16.count))
  }
  
  func stringRef(_ expr: Expr, _ index: Expr) throws -> Expr {
    let str = try expr.asStr().utf16
    let k = try index.asInt()
    let i = str.index(str.startIndex, offsetBy: k)
    guard i < str.endIndex else {
      throw EvalError.indexOutOfBounds(Int64(k), Int64(str.count - 1), expr)
    }
    return .char(str[i])
  }
  
  func stringAppend(_ exprs: Arguments) throws -> Expr {
    var res = ""
    for expr in exprs {
      res.append(try expr.asStr())
    }
    return .string(NSMutableString(string: res))
  }
  
  func stringEquals(_ fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asStr() == snd.asStr())
  }
  
  func stringLessThan(_ fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asStr() < snd.asStr())
  }
  
  func stringLessThanEquals(_ fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asStr() <= snd.asStr())
  }
  
  func stringGreaterThan(_ fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asStr() > snd.asStr())
  }
  
  func stringGreaterThanEquals(_ fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asStr() >= snd.asStr())
  }
  
  func stringCiEquals(_ fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asStr().lowercased() == snd.asStr().lowercased())
  }
  
  func stringCiLessThan(_ fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asStr().lowercased() < snd.asStr().lowercased())
  }
  
  func stringCiLessThanEquals(_ fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asStr().lowercased() <= snd.asStr().lowercased())
  }
  
  func stringCiGreaterThan(_ fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asStr().lowercased() > snd.asStr().lowercased())
  }
  
  func stringCiGreaterThanEquals(_ fst: Expr, _ snd: Expr) throws -> Expr {
    return .Boolean(try fst.asStr().lowercased() >= snd.asStr().lowercased())
  }
  
  func stringContains(_ expr: Expr, _ other: Expr) throws -> Expr {
    return .Boolean(try expr.asStr().contains(try other.asStr()))
  }
  
  func stringUpcase(_ expr: Expr) throws -> Expr {
    return .string(NSMutableString(string: try expr.asMutableStr().uppercased))
  }
  
  func stringDowncase(_ expr: Expr) throws -> Expr {
    return .string(NSMutableString(string: try expr.asStr().lowercased()))
  }
  
  func stringToList(_ expr: Expr) throws -> Expr {
    var res = Expr.null
    let str = try expr.asStr().utf16
    for ch in str.reversed() {
      res = .pair(.char(ch), res)
    }
    return res
  }
  
  func listToString(_ expr: Expr) throws -> Expr {
    var list = expr
    var uniChars: [UniChar] = []
    while case .pair(let ch, let next) = list {
      uniChars.append(try ch.asChar())
      list = next
    }
    guard list.isNull else {
      throw EvalError.typeError(expr, [.properListType])
    }
    return .string(NSMutableString(string: String(utf16CodeUnits: uniChars, count: uniChars.count)))
  }
  
  func substring(_ expr: Expr, _ start: Expr, _ end: Expr?) throws -> Expr {
    let str = try expr.asStr().utf16
    let s = str.index(str.startIndex, offsetBy: try start.asInt())
    guard s < str.endIndex else {
      throw EvalError.indexOutOfBounds(try start.asInteger(), Int64(str.count), expr)
    }
    let e = end == nil ? str.endIndex : str.index(str.startIndex, offsetBy: try end!.asInt())
    guard e <= str.endIndex && s <= e else {
      // TODO: Fix error (should define [s..str.count] as bounds for e
      throw EvalError.indexOutOfBounds(try end!.asInteger(), Int64(str.count), expr)
    }
    var uniChars: [UniChar] = []
    for ch in str[s..<e] {
      uniChars.append(ch)
    }
    return .string(NSMutableString(string: String(utf16CodeUnits: uniChars, count: uniChars.count)))
  }
}
