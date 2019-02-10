//
//  CharSetLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 01/01/2019.
//  Copyright © 2019 ObjectHub. All rights reserved.
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

public final class CharSetLibrary: NativeLibrary {

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "char-set"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"],    "define", "and", "or", "not", "lambda", "set!")
    self.`import`(from: ["lispkit", "control"], "if", "let", "do")
    self.`import`(from: ["lispkit", "list"],    "car", "null?")
    self.`import`(from: ["lispkit", "math"],    "fx1+")
  }

  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("char-set", charSet))
    self.define(Procedure("immutable-char-set", immutableCharSet))
    self.define(Procedure("list->char-set", listToCharSet))
    self.define(Procedure("string->char-set", stringToCharSet))
    self.define(Procedure("ucs-range->char-set", ucsRangeToCharSet))
    self.define(Procedure("list->char-set!", listToCharSetDestructive))
    self.define(Procedure("string->char-set!", stringToCharSetDestructive))
    self.define(Procedure("ucs-range->char-set!", ucsRangeToCharSetDestructive))
    self.define(Procedure("->char-set", coerceToCharSet))
    self.define(Procedure("char-set-copy", charSetCopy))
    self.define(Procedure("char-set?", isCharSet))
    self.define(Procedure("char-set=?", charSetEqual))
    self.define(Procedure("char-set<=?", charSetIncluded))
    self.define(Procedure("char-set-hash", charSetHash))
    self.define(Procedure("char-set-disjoint?", charSetDisjoint))
    self.define(Procedure("char-set-empty?", isCharSetEmpty))
    self.define(Procedure("char-set-contains?", charSetContains))
    self.define(Procedure("char-set-size", charSetSize))
    self.define(Procedure("char-set-cursor", charSetCursor))
    self.define(Procedure("char-set-ref", charSetRef))
    self.define(Procedure("char-set-cursor-next", charSetCursorNext))
    self.define(Procedure("end-of-char-set?", endOfCharSet))
    self.define(Procedure("char-set->list", charSetToList))
    self.define(Procedure("char-set->string", charSetToString))
    self.define(Procedure("char-set-complement", charSetComplement))
    self.define(Procedure("char-set-adjoin", charSetAdjoin))
    self.define(Procedure("char-set-delete", charSetDelete))
    self.define(Procedure("char-set-union", charSetUnion))
    self.define(Procedure("char-set-intersection", charSetIntersection))
    self.define(Procedure("char-set-difference", charSetDifference))
    self.define(Procedure("char-set-xor", charSetXor))
    self.define(Procedure("char-set-diff+intersection", charSetDiffIntersection))
    self.define(Procedure("char-set-complement!", charSetComplementDestructive))
    self.define(Procedure("char-set-adjoin!", charSetAdjoinDestructive))
    self.define(Procedure("char-set-delete!", charSetDeleteDestructive))
    self.define(Procedure("char-set-union!", charSetUnionDestructive))
    self.define(Procedure("char-set-intersection!", charSetIntersectionDestructive))
    self.define(Procedure("char-set-difference!", charSetDifferenceDestructive))
    self.define(Procedure("char-set-xor!", charSetXorDestructive))
    self.define(Procedure("char-set-diff+intersection!", charSetDiffIntersectionDestructive))
    self.define("char-set-every?", via: """
      (define (char-set-every? pred cs)
        (do ((cursor (char-set-cursor cs) (char-set-cursor-next cs cursor)))
            ((or (end-of-char-set? cursor) (not (pred (char-set-ref cs cursor))))
             (end-of-char-set? cursor))))
    """)
    self.define("char-set-any?", via: """
      (define (char-set-any? pred cs)
        (do ((cursor (char-set-cursor cs) (char-set-cursor-next cs cursor)))
            ((or (end-of-char-set? cursor) (pred (char-set-ref cs cursor)))
             (not (end-of-char-set? cursor)))))
    """)
    self.define("char-set-count", via: """
      (define (char-set-count pred cs)
        (do ((cursor (char-set-cursor cs) (char-set-cursor-next cs cursor))
             (count 0))
            ((end-of-char-set? cursor) count)
          (if (pred (char-set-ref cs cursor)) (set! count (fx1+ count)))))
    """)
    self.define("char-set-for-each", via: """
      (define (char-set-for-each proc cs)
        (do ((cursor (char-set-cursor cs) (char-set-cursor-next cs cursor)))
            ((end-of-char-set? cursor))
          (proc (char-set-ref cs cursor))))
    """)
    self.define("char-set-map", via: """
      (define (char-set-map proc cs)
        (do ((cursor (char-set-cursor cs) (char-set-cursor-next cs cursor))
             (res (char-set)))
            ((end-of-char-set? cursor) res)
          (char-set-adjoin! res (proc (char-set-ref cs cursor)))))
    """)
    self.define("char-set-fold", via: """
      (define (char-set-fold proc z cs)
        (do ((cursor (char-set-cursor cs) (char-set-cursor-next cs cursor))
             (acc z (proc (char-set-ref cs cursor) acc)))
            ((end-of-char-set? cursor) acc)))
    """)
    self.define("char-set-unfold!", via: """
      (define (char-set-unfold! p f g seed bcs)
        (let lp ((seed seed) (cs bcs))
          (if (p seed) cs (lp (g seed) (char-set-adjoin! cs (f seed))))))
    """)
    self.define("char-set-unfold", via: """
      (define (char-set-unfold p f g seed . args)
        (char-set-unfold! p f g seed (if (null? args) (char-set) (char-set-copy (car args)))))
    """)
    self.define("char-set-filter", via: """
      (define (char-set-filter pred cs . args)
        (do ((cursor (char-set-cursor cs) (char-set-cursor-next cs cursor))
             (res (if (null? args) (char-set) (char-set-copy (car args)))))
            ((end-of-char-set? cursor) res)
          (if (pred (char-set-ref cs cursor))
              (char-set-adjoin! res (char-set-ref cs cursor)))))
    """)
    self.define("char-set-filter!", via: """
      (define (char-set-filter! pred cs res)
        (do ((cursor (char-set-cursor cs) (char-set-cursor-next cs cursor)))
            ((end-of-char-set? cursor) res)
          (if (pred (char-set-ref cs cursor))
              (char-set-adjoin! res (char-set-ref cs cursor)))))
    """)
    self.define("char-set:lower-case",
                as: .object(CharSet.lowercaseLetters()), mutable: false, export: true)
    self.define("char-set:upper-case",
                as: .object(CharSet.uppercaseLetters()), mutable: false, export: true)
    self.define("char-set:title-case",
                as: .object(CharSet.titlecaseLetters()), mutable: false, export: true)
    self.define("char-set:letter",
                as: .object(CharSet.letters()), mutable: false, export: true)
    self.define("char-set:digit",
                as: .object(CharSet.digits()), mutable: false, export: true)
    self.define("char-set:letter+digit",
                as: .object(CharSet.lettersAndDigits()), mutable: false, export: true)
    self.define("char-set:graphic",
                as: .object(CharSet.graphics()), mutable: false, export: true)
    self.define("char-set:printing",
                as: .object(CharSet.printing()), mutable: false, export: true)
    self.define("char-set:whitespace",
                as: .object(CharSet.whitespaces()), mutable: false, export: true)
    self.define("char-set:iso-control",
                as: .object(CharSet.controls()), mutable: false, export: true)
    self.define("char-set:punctuation",
                as: .object(CharSet.punctuations()), mutable: false, export: true)
    self.define("char-set:symbol",
                as: .object(CharSet.symbols()), mutable: false, export: true)
    self.define("char-set:hex-digit",
                as: .object(CharSet.hexdigits()), mutable: false, export: true)
    self.define("char-set:blank",
                as: .object(CharSet.blanks()), mutable: false, export: true)
    self.define("char-set:ascii",
                as: .object(CharSet.ascii()), mutable: false, export: true)
    self.define("char-set:empty",
                as: .object(CharSet()), mutable: false, export: true)
    self.define("char-set:full",
                as: .object(CharSet.full()), mutable: false, export: true)
  }

  private func charSet(_ args: Arguments) throws -> Expr {
    let cs = CharSet()
    for arg in args {
      cs.insert(try arg.asUniChar())
    }
    return .object(cs)
  }

  private func immutableCharSet(_ args: Arguments) throws -> Expr {
    let cs = CharSet(immutable: true)
    for arg in args {
      cs.insert(try arg.asUniChar())
    }
    return .object(cs)
  }

  private func listToCharSet(_ expr: Expr, _ charset: Expr?) throws -> Expr {
    let cs = try self.characterSetCopy(charset)
    var lst = expr
    while case .pair(let car, let cdr) = lst {
      cs.insert(try car.asUniChar())
      lst = cdr
    }
    return .object(cs)
  }

  private func stringToCharSet(_ expr: Expr, _ charset: Expr?) throws -> Expr {
    let cs = try self.characterSetCopy(charset)
    cs.insert(charsIn: try expr.asString())
    return .object(cs)
  }

  private func ucsRangeToCharSet(_ lower: Expr,
                                 _ upper: Expr,
                                 _ fst: Expr?,
                                 _ snd: Expr?) throws -> Expr {
    let upperLimit: Int
    let cs: CharSet
    if let charset = snd {
      upperLimit = (fst?.isTrue ?? false) ? Int(UInt16.max) + 1 : Int.max
      cs = try self.characterSetCopy(charset)
    } else {
      upperLimit = Int.max
      cs = try self.characterSetCopy(fst)
    }
    let hiArg = try upper.asInt(below: upperLimit)
    let loArg = try lower.asInt(below: hiArg + 1)
    let hi = min(hiArg, Int(UInt16.max) + 1)
    let lo = min(loArg, Int(UInt16.max) + 1)
    for ch in lo ..< hi {
      cs.insert(UInt16(ch))
    }
    return .object(cs)
  }

  private func listToCharSetDestructive(_ expr: Expr, _ charset: Expr) throws -> Expr {
    let cs = try self.characterSet(charset)
    guard !cs.immutable else {
      throw RuntimeError.eval(.attemptToModifyImmutableData, charset)
    }
    var lst = expr
    while case .pair(let car, let cdr) = lst {
      cs.insert(try car.asUniChar())
      lst = cdr
    }
    return .object(cs)
  }

  private func stringToCharSetDestructive(_ expr: Expr, _ charset: Expr) throws -> Expr {
    let cs = try self.characterSet(charset)
    guard !cs.immutable else {
      throw RuntimeError.eval(.attemptToModifyImmutableData, charset)
    }
    cs.insert(charsIn: try expr.asString())
    return .object(cs)
  }

  private func ucsRangeToCharSetDestructive(_ lower: Expr,
                                            _ upper: Expr,
                                            _ fst: Expr?,
                                            _ snd: Expr?) throws -> Expr {
    let upperLimit: Int
    let cs: CharSet
    if let charset = snd {
      upperLimit = (fst?.isTrue ?? false) ? Int(UInt16.max) + 1 : Int.max
      cs = try self.characterSet(charset)
      guard !cs.immutable else {
        throw RuntimeError.eval(.attemptToModifyImmutableData, charset)
      }
    } else {
      upperLimit = Int.max
      cs = try self.characterSet(fst)
      guard !cs.immutable else {
        throw RuntimeError.eval(.attemptToModifyImmutableData, fst!)
      }
    }
    let hiArg = try upper.asInt(below: upperLimit)
    let loArg = try lower.asInt(below: hiArg + 1)
    let hi = min(hiArg, Int(UInt16.max) + 1)
    let lo = min(loArg, Int(UInt16.max) + 1)
    for ch in lo ..< hi {
      cs.insert(UInt16(ch))
    }
    return .object(cs)
  }

  private func coerceToCharSet(_ expr: Expr) throws -> Expr {
    switch expr {
      case .string:
        return try self.stringToCharSet(expr, nil)
      case .char:
        return try self.listToCharSet(.pair(expr, .null), nil)
      case .object(let ref):
        guard ref is CharSet else {
          throw RuntimeError.type(expr, expected: [.strType, .charType, .charSetType])
        }
        return expr
      default:
        throw RuntimeError.type(expr, expected: [.strType, .charType, .charSetType])
    }
  }

  private func charSetCopy(_ expr: Expr, _ mutable: Expr?) throws -> Expr {
    guard case .object(let obj) = expr, let res = obj as? CharSet else {
      throw RuntimeError.type(expr, expected: [.charSetType])
    }
    return .object(CharSet(copy: res, immutable: mutable?.isFalse ?? false))
  }

  private func isCharSet(_ expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is CharSet else {
      return .false
    }
    return .true
  }

  private func isCharSetEmpty(_ expr: Expr) throws -> Expr {
    return .makeBoolean(try self.characterSet(expr).isEmpty)
  }

  private func charSetEqual(_ args: Arguments) throws -> Expr {
    var first: CharSet? = nil
    for arg in args {
      let other = try self.characterSet(arg)
      if first == nil {
        first = other
      } else if !first!.isEqual(to: other) {
        return .false
      }
    }
    return .true
  }

  private func charSetIncluded(_ args: Arguments) throws -> Expr {
    var left: CharSet? = nil
    for arg in args {
      let other = try self.characterSet(arg)
      if left == nil || left!.isSubset(of: other) {
        left = other
      } else {
        return .false
      }
    }
    return .true
  }

  private func charSetDisjoint(_ first: Expr, _ second: Expr) throws -> Expr {
    return .makeBoolean(try self.characterSet(first).isDisjoint(with:
                          try self.characterSet(second)))
  }

  private func charSetHash(_ expr: Expr, _ bound: Expr?) throws -> Expr {
    let cs = try self.characterSet(expr)
    let bnd = try bound?.asInt64() ?? 0
    if bnd <= 0 {
      return .fixnum(Int64(cs.charSetHashValue))
    } else {
      return .fixnum(Int64(cs.charSetHashValue) %% bnd)
    }
  }

  private func charSetContains(_ expr: Expr, _ char: Expr) throws -> Expr {
    let cs = try self.characterSet(expr)
    return .makeBoolean(cs.contains(try char.asUniChar()))
  }

  private func charSetSize(_ expr: Expr) throws -> Expr {
    return .fixnum(Int64(try self.characterSet(expr).count))
  }

  private func charSetCursor(_ expr: Expr) throws -> Expr {
    guard let cursor = try self.characterSet(expr).first else {
      return .false
    }
    return .char(cursor)
  }

  private func charSetRef(_ expr: Expr, _ cursor: Expr) throws -> Expr {
    _ = try self.characterSet(expr)
    guard case .char = cursor else {
      throw RuntimeError.type(cursor, expected: [.charType])
    }
    return cursor
  }

  private func charSetCursorNext(_ expr: Expr, _ cursor: Expr) throws -> Expr {
    let cs = try self.characterSet(expr)
    if cursor.isFalse {
      return .false
    }
    guard case .char(let current) = cursor else {
      throw RuntimeError.type(cursor, expected: [.charType])
    }
    guard let new = cs.next(current) else {
      return .false
    }
    return .char(new)
  }

  private func endOfCharSet(_ cursor: Expr) throws -> Expr {
    if cursor.isFalse {
      return .true
    }
    guard case .char = cursor else {
      throw RuntimeError.type(cursor, expected: [.charType])
    }
    return .false
  }

  private func charSetToList(_ expr: Expr) throws -> Expr {
    let cs = try self.characterSet(expr)
    var res: Expr = .null
    cs.forEach { ch in
      res = .pair(.char(ch), res)
    }
    return res
  }

  private func charSetToString(_ expr: Expr) throws -> Expr {
    let uniChars = try self.characterSet(expr).array
    return .makeString(String(utf16CodeUnits: uniChars, count: uniChars.count))
  }

  private func charSetComplement(_ expr: Expr) throws -> Expr {
    return .object(try self.characterSet(expr).inverted)
  }

  private func charSetAdjoin(_ expr: Expr, _ args: Arguments) throws -> Expr {
    let cs = try self.characterSetCopy(expr)
    for arg in args {
      cs.insert(try arg.asUniChar())
    }
    return .object(cs)
  }

  private func charSetDelete(_ expr: Expr, _ args: Arguments) throws -> Expr {
    let cs = try self.characterSetCopy(expr)
    for arg in args {
      cs.remove(try arg.asUniChar())
    }
    return .object(cs)
  }

  private func charSetUnion(_ args: Arguments) throws -> Expr {
    let res = CharSet()
    for arg in args {
      res.formUnion(try self.characterSet(arg))
    }
    return .object(res)
  }

  private func charSetIntersection(_ args: Arguments) throws -> Expr {
    var res: CharSet? = nil
    for arg in args {
      if res == nil {
        res = try self.characterSetCopy(arg)
      } else {
        res!.formIntersection(try self.characterSet(arg))
      }
    }
    if res == nil {
      return .object(CharSet().inverted)
    } else {
      return .object(res!)
    }
  }

  private func charSetDifference(_ first: Expr, _ args: Arguments) throws -> Expr {
    let cs = try self.characterSetCopy(first)
    for arg in args {
      cs.subtract(try self.characterSet(arg))
    }
    return .object(cs)
  }

  private func charSetXor(_ args: Arguments) throws -> Expr {
    let cs = CharSet()
    for arg in args {
      cs.formSymmetricDifference(try self.characterSet(arg))
    }
    return .object(cs)
  }

  private func charSetDiffIntersection(_ first: Expr, _ args: Arguments) throws -> Expr {
    let one = try self.characterSetCopy(first)
    let union = CharSet()
    for arg in args {
      let cs = try self.characterSet(arg)
      one.subtract(cs)
      union.formUnion(cs)
    }
    let two = try self.characterSetCopy(first)
    two.formIntersection(union)
    return .values(.pair(.object(one), .pair(.object(two), .null)))
  }

  private func charSetComplementDestructive(_ expr: Expr) throws -> Expr {
    let cs = try self.characterSet(expr)
    guard !cs.immutable else {
      throw RuntimeError.eval(.attemptToModifyImmutableData, expr)
    }
    cs.invert()
    return .object(cs)
  }

  private func charSetAdjoinDestructive(_ expr: Expr, _ args: Arguments) throws -> Expr {
    let cs = try self.characterSet(expr)
    guard !cs.immutable else {
      throw RuntimeError.eval(.attemptToModifyImmutableData, expr)
    }
    for arg in args {
      cs.insert(try arg.asUniChar())
    }
    return .object(cs)
  }

  private func charSetDeleteDestructive(_ expr: Expr, _ args: Arguments) throws -> Expr {
    let cs = try self.characterSet(expr)
    guard !cs.immutable else {
      throw RuntimeError.eval(.attemptToModifyImmutableData, expr)
    }
    for arg in args {
      cs.remove(try arg.asUniChar())
    }
    return .object(cs)
  }

  private func charSetUnionDestructive(_ first: Expr, _ args: Arguments) throws -> Expr {
    let res = try self.characterSet(first)
    guard !res.immutable else {
      throw RuntimeError.eval(.attemptToModifyImmutableData, first)
    }
    for arg in args {
      res.formUnion(try self.characterSet(arg))
    }
    return .object(res)
  }

  private func charSetIntersectionDestructive(_ first: Expr, _ args: Arguments) throws -> Expr {
    let res = try self.characterSet(first)
    guard !res.immutable else {
      throw RuntimeError.eval(.attemptToModifyImmutableData, first)
    }
    for arg in args {
      res.formIntersection(try self.characterSet(arg))
    }
    return .object(res)
  }

  private func charSetDifferenceDestructive(_ first: Expr, _ args: Arguments) throws -> Expr {
    let res = try self.characterSet(first)
    guard !res.immutable else {
      throw RuntimeError.eval(.attemptToModifyImmutableData, first)
    }
    for arg in args {
      res.subtract(try self.characterSet(arg))
    }
    return .object(res)
  }

  private func charSetXorDestructive(_ first: Expr, _ args: Arguments) throws -> Expr {
    let res = try self.characterSet(first)
    guard !res.immutable else {
      throw RuntimeError.eval(.attemptToModifyImmutableData, first)
    }
    for arg in args {
      res.formSymmetricDifference(try self.characterSet(arg))
    }
    return .object(res)
  }

  private func charSetDiffIntersectionDestructive(_ first: Expr,
                                                  _ second: Expr,
                                                  _ args: Arguments) throws -> Expr {
    let one = try self.characterSet(first)
    guard !one.immutable else {
      throw RuntimeError.eval(.attemptToModifyImmutableData, first)
    }
    let fst = CharSet(copy: one)
    let two = try self.characterSet(second)
    guard !two.immutable else {
      throw RuntimeError.eval(.attemptToModifyImmutableData, second)
    }
    one.subtract(two)
    for arg in args {
      let cs = try self.characterSet(arg)
      one.subtract(cs)
      two.formUnion(cs)
    }
    two.formIntersection(fst)
    return .values(.pair(.object(one), .pair(.object(two), .null)))
  }

  private func characterSet(_ expr: Expr?) throws -> CharSet {
    guard let expr = expr else {
      return CharSet()
    }
    guard case .object(let obj) = expr, let res = obj as? CharSet else {
      throw RuntimeError.type(expr, expected: [.charSetType])
    }
    return res
  }

  private func characterSetCopy(_ expr: Expr?) throws -> CharSet {
    guard let expr = expr else {
      return CharSet()
    }
    guard case .object(let obj) = expr, let res = obj as? CharSet else {
      throw RuntimeError.type(expr, expected: [.charSetType])
    }
    return CharSet(copy: res)
  }
}
