//
//  EnumLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 02/03/2022.
//  Copyright © 2022 ObjectHub. All rights reserved.
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

public final class EnumLibrary: NativeLibrary {
  
  /// Initialize symbols
  public required init(in context: Context) throws {
    try super.init(in: context)
  }

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "enum"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"], "define", "define-syntax", "syntax-rules", "lambda",
                                             "quote", "and", "or", "not", "eq?")
    self.`import`(from: ["lispkit", "control"], "do", "if", "cond", "begin", "let")
    self.`import`(from: ["lispkit", "list"], "cons", "reverse", "map")
    self.`import`(from: ["lispkit", "math"], "fx1+")
  }
  
  /// Declarations of the library.
  public override func declarations() {
    // Enum types
    self.define(Procedure("enum-type?", self.isEnumType))
    self.define(Procedure("make-enum-type", self.makeEnumType))
    self.define(Procedure("enum-type-size", self.enumTypeSize))
    self.define(Procedure("enum-min", self.enumMin))
    self.define(Procedure("enum-max", self.enumMax))
    self.define(Procedure("enum-type-enums", self.enumTypeEnums))
    self.define(Procedure("enum-type-names", self.enumTypeNames))
    self.define(Procedure("enum-type-tags", self.enumTypeTags))
    self.define(Procedure("enum-type-contains?", self.enumTypeContains))
    self.define(Procedure("enum-name->enum", self.enumNameToEnum))
    self.define(Procedure("enum-name->ordinal", self.enumNameToOrdinal))
    self.define(Procedure("enum-name->tag", self.enumNameToTag))
    self.define(Procedure("enum-ordinal->enum", self.enumOrdinalToEnum))
    self.define(Procedure("enum-ordinal->name", self.enumOrdinalToName))
    self.define(Procedure("enum-ordinal->tag", self.enumOrdinalToTag)) 
    
    // Enum values
    self.define(Procedure("enum?", self.isEnum))
    self.define(Procedure("enum-type", self.getEnumType))
    self.define(Procedure("enum-name", self.enumName))
    self.define(Procedure("enum-ordinal", self.enumOrdinal))
    self.define(Procedure("enum-tag", self.enumTag))
    self.define(Procedure("enum-next", self.enumNext))
    self.define(Procedure("enum-prev", self.enumPrev))
    self.define(Procedure("enum=?", self.enumEqual))
    self.define(Procedure("enum<?", self.enumLessThan))
    self.define(Procedure("enum<=?", self.enumLessThanEqual))
    self.define(Procedure("enum>?", self.enumGreaterThan))
    self.define(Procedure("enum>=?", self.enumGreaterThanEqual))
    
    // Enum sets
    self.define(Procedure("enum-set?", self.isEnumSet))
    self.define(Procedure("enum-set", self.enumSet))
    self.define(Procedure("list->enum-set", self.listToEnumSet))
    self.define(Procedure("enum-type->enum-set", self.enumTypeToEnumSet))
    self.define(Procedure("enum-set-projection", self.enumSetProjection))
    self.define(Procedure("enum-set-copy", self.enumSetCopy))
    self.define(Procedure("enum-set-empty?", self.isEnumSetEmpty))
    self.define(Procedure("enum-set-contains?", self.enumSetContains))
    self.define(Procedure("enum-set-disjoint?", self.enumSetDisjoint))
    self.define(Procedure("enum-set-subset?", self.enumSetSubset))
    self.define(Procedure("enum-set=?", self.enumSetEqual))
    self.define(Procedure("enum-set<?", self.enumSetLessThan))
    self.define(Procedure("enum-set<=?", self.enumSetLessThanEqual))
    self.define(Procedure("enum-set>?", self.enumSetGreaterThan))
    self.define(Procedure("enum-set>=?", self.enumSetGreaterThanEqual))
    self.define(Procedure("enum-set->list", self.enumSetToList))
    self.define(Procedure("enum-set->enum-list", self.enumSetToEnumList))
    self.define(Procedure("enum-set-next", self.enumSetNext))
    self.define(Procedure("enum-set-type", self.enumSetType))
    self.define(Procedure("enum-set-bitset", self.enumSetBitset))
    self.define(Procedure("enum-set-size", self.enumSetSize))
    self.define(Procedure("enum-set-adjoin!", self.enumSetAdjoin))
    self.define(Procedure("enum-set-adjoin-all!", self.enumSetAdjoinAll))
    self.define(Procedure("enum-set-delete!", self.enumSetDelete))
    self.define(Procedure("enum-set-delete-all!", self.enumSetDeleteAll))
    self.define(Procedure("enum-set-union", self.enumSetUnionNew))
    self.define(Procedure("enum-set-union!", self.enumSetUnion))
    self.define(Procedure("enum-set-intersection", self.enumSetIntersectionNew))
    self.define(Procedure("enum-set-intersection!", self.enumSetIntersection))
    self.define(Procedure("enum-set-difference", self.enumSetDifferenceNew))
    self.define(Procedure("enum-set-difference!", self.enumSetDifference))
    self.define(Procedure("enum-set-xor", self.enumSetXorNew))
    self.define(Procedure("enum-set-xor!", self.enumSetXor))
    self.define(Procedure("enum-set-complement", self.enumSetComplementNew))
    self.define(Procedure("enum-set-complement!", self.enumSetComplement))
    
    // Backward compatibility
    self.define(Procedure("make-enumeration", self.makeEnumeration))
    self.define(Procedure("enum-set-universe", self.enumSetUniverse))
    self.define(Procedure("enum-set-member?", self.enumSetMember))
    
    // Higher-order procedures
    self.define("enum-type-test-predicate", via:
      "(define (enum-type-test-predicate et)" +
      "  (lambda (x) (and (enum? x) (enum-type-contains? et x))))")
    self.define("enum-set-type-test-predicate", via:
      "(define (enum-set-type-test-predicate et)" +
      "  (lambda (x) (and (enum-set? x) (eq? (enum-set-type x) et))))")
    self.define("enum-constructor", via:
      "(define (enum-constructor et)" +
      "  (lambda (x) (enum-name->enum (enum-set-type et) x)))")
    self.define("enum-set-constructor", via:
      "(define (enum-set-constructor et)" +
      "  (lambda (x) (list->enum-set (enum-set-type et) x)))")
    self.define("enum-set-indexer", via:
      "(define (enum-set-indexer eset)" +
      "  (let ((type (enum-set-type eset)))" +
      "    (lambda (name)" +
      "      (cond ((enum-name->enum type name) => enum-ordinal) (else #f)))))")
    self.define("enum-set-any?", via: """
      (define (enum-set-any? f bs)
        (do ((i (enum-set-next bs (enum-min (enum-set-type bs))) (enum-set-next bs (enum-next i))))
            ((or (not i) (f i)) i)))
    """)
    self.define("enum-set-every?", via: """
      (define (enum-set-every? f bs)
        (do ((i (enum-set-next bs (enum-min (enum-set-type bs))) (enum-set-next bs (enum-next i))))
            ((or (not i) (not (f i))) (not i))))
    """)
    self.define("enum-set-count", via: """
      (define (enum-set-count f bs)
        (do ((i (enum-set-next bs (enum-min (enum-set-type bs))) (enum-set-next bs (enum-next i)))
             (x 0 (if (f i) (fx1+ x) x)))
            ((not i) x)))
    """)
    self.define("enum-set-map->list", via: """
      (define (enum-set-map->list f bs)
        (do ((i (enum-set-next bs (enum-min (enum-set-type bs))) (enum-set-next bs (enum-next i)))
             (x '() (cons (f i) x)))
            ((not i) (reverse x))))
    """)
    self.define("enum-set-for-each", via: """
      (define (enum-set-for-each f bs)
        (do ((i (enum-set-next bs (enum-min (enum-set-type bs))) (enum-set-next bs (enum-next i))))
            ((not i))
          (f i)))
    """)
    self.define("enum-set-fold", via: """
      (define (enum-set-fold f z bs)
        (do ((i (enum-set-next bs (enum-min (enum-set-type bs))) (enum-set-next bs (enum-next i)))
             (acc z (f i acc)))
            ((not i) acc)))
    """)
    self.define("enum-set-filter", via: """
      (define (enum-set-filter f bs)
        (do ((i (enum-set-next bs (enum-min (enum-set-type bs))) (enum-set-next bs (enum-next i)))
             (res (enum-set (enum-set-type bs))))
            ((not i) res)
          (if (f i) (enum-set-adjoin! res i))))
    """)
    self.define("enum-set-remove", via: """
      (define (enum-set-remove f bs)
        (do ((i (enum-set-next bs (enum-min (enum-set-type bs))) (enum-set-next bs (enum-next i)))
             (res (enum-set (enum-set-type bs))))
            ((not i) res)
          (if (not (f i)) (enum-set-adjoin! res i))))
    """)
    self.define("define-enum", via: """
      (define-syntax define-enum
        (syntax-rules ()
          ((_ type-name (name-val ...) constructor)
            (begin
              (define etype (make-enum-type '(name-val ...)))
              (define-syntax type-name
                (syntax-rules ()
                  ((_ name)
                    (enum-name->enum etype 'name))))
              (define-syntax constructor
                (syntax-rules ()
                  ((_ . ns)
                    (list->enum-set etype (map (lambda (s) (enum-name->enum etype s)) 'ns)))))))))
    """)
    self.define("define-enumeration", via: """
      (define-syntax define-enumeration
        (syntax-rules ()
          ((_ type-name (name-val ...) constructor)
            (begin
              (define etype (make-enum-type '(name-val ...)))
              (define-syntax type-name
                (syntax-rules ()
                  ((_ name)
                    (and (enum-name->enum etype 'name) 'name))))
              (define-syntax constructor
                (syntax-rules ()
                  ((_ . ns)
                    (list->enum-set etype (map (lambda (s) (enum-name->enum etype s)) 'ns)))))))))
    """)
  }
  
  private func enumType(from expr: Expr) throws -> EnumType {
    guard case .object(let obj) = expr, let enumType = obj as? EnumType else {
      throw RuntimeError.type(expr, expected: [EnumType.type])
    }
    return enumType
  }
  
  private func enumSetMemberOrdinal(_ expr: Expr, for estype: EnumType) throws -> Int {
    switch expr {
      case .fixnum(let i):
        guard i >= 0 && i < estype.enumCount else {
          throw RuntimeError.eval(.enumNotMatchingEnumSetType, expr, .object(estype))
        }
        return Int(i)
      case .symbol(let sym):
        guard let e = estype.enum(for: sym) else {
          throw RuntimeError.eval(.enumNotMatchingEnumSetType, expr, .object(estype))
        }
        return e.ordinal
      default:
        let (etype, i) = try self.enumComponents(from: expr)
        guard etype == estype else {
          throw RuntimeError.eval(.enumNotMatchingEnumSetType, expr, .object(estype))
        }
        return i
    }
  }
  
  private func enumSetComponents(from expr: Expr) throws -> (EnumType, NativeBitset) {
    guard case .tagged(.object(let obj), .object(let bsobj)) = expr,
          let enumType = obj as? EnumType,
          let nbs = bsobj as? NativeBitset else {
      throw RuntimeError.eval(.invalidEnumSet, expr)
    }
    return (enumType, nbs)
  }
  
  private func enumComponents(from expr: Expr) throws -> (EnumType, Int) {
    guard case .tagged(.object(let obj), .fixnum(let index)) = expr,
          let enumType = obj as? EnumType,
          index >= 0 && index < enumType.enumCount else {
      throw RuntimeError.eval(.invalidEnumValue, expr)
    }
    return (enumType, Int(index))
  }
  
  private func `enum`(from expr: Expr) throws -> EnumType.Enum {
    let (enumType, index) = try self.enumComponents(from: expr)
    return enumType.`enum`(at: index)!
  }
  
  private func enumSetAsExpr(_ bitset: NativeBitset, for type: EnumType) -> Expr {
    return .tagged(.object(type), .object(bitset))
  }
  
  private func enumAsExpr(_ name: Symbol, for type: EnumType) -> Expr? {
    if let e = type.`enum`(for: name) {
      return .tagged(.object(type), .fixnum(Int64(e.ordinal)))
    } else {
      return nil
    }
  }
  
  private func enumAsExpr(_ index: Int, for type: EnumType) -> Expr {
    return .tagged(.object(type), .fixnum(Int64(index)))
  }
  
  private func isEnumType(expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is EnumType else {
      return .false
    }
    return .true
  }
  
  private func makeEnumType(fst: Expr, snd: Expr?, trd: Expr?) throws -> Expr {
    let name: Expr? = snd == nil ? nil : fst
    var lst = snd ?? fst
    var entries: [(Symbol, Expr?)] = []
    while case .pair(let car, let cdr) = lst {
      switch car {
        case .symbol(let sym):
          entries.append((sym, nil))
        case .pair(.symbol(let sym), .pair(let tag, .null)):
          entries.append((sym, tag))
        default:
          throw RuntimeError.eval(.invalidEnumSpecifier, car)
      }
      lst = cdr
    }
    guard entries.count > 0 else {
      throw RuntimeError.eval(.enumTypeEmpty)
    }
    return .object(EnumType(name: name, entries: entries, tag: trd ?? .false))
  }
  
  private func enumTypeSize(expr: Expr) throws -> Expr {
    return .fixnum(Int64(try self.enumType(from: expr).enumCount))
  }
  
  private func enumMin(expr: Expr) throws -> Expr {
    return self.enumAsExpr(0, for: try self.enumType(from: expr))
  }
  
  private func enumMax(expr: Expr) throws -> Expr {
    let etype = try self.enumType(from: expr)
    return self.enumAsExpr(etype.enumCount - 1, for: etype)
  }
  
  private func enumTypeEnums(expr: Expr) throws -> Expr {
    let etype = try self.enumType(from: expr)
    var res = Expr.null
    var i = etype.enumCount
    while i > 0 {
      i -= 1
      res = .pair(self.enumAsExpr(i, for: etype), res)
    }
    return res
  }
  
  private func enumTypeNames(expr: Expr) throws -> Expr {
    let etype = try self.enumType(from: expr)
    var res = Expr.null
    var i = etype.enumCount
    while i > 0 {
      i -= 1
      res = .pair(.symbol(etype.`enum`(at: i)!.name), res)
    }
    return res
  }
  
  private func enumTypeTags(expr: Expr) throws -> Expr {
    let etype = try self.enumType(from: expr)
    var res = Expr.null
    var i = etype.enumCount
    while i > 0 {
      i -= 1
      res = .pair(etype.`enum`(at: i)!.tag, res)
    }
    return res
  }
  
  private func enumTypeContains(etype: Expr, expr: Expr) throws -> Expr {
    let enumType = try self.enumType(from: etype)
    guard case .tagged(.object(let obj), .fixnum(_)) = expr,
          let exprType = obj as? EnumType else {
      throw RuntimeError.eval(.invalidEnumValue, expr)
    }
    return .makeBoolean(enumType == exprType)
  }
  
  private func enumNameToEnum(expr: Expr, name: Expr) throws -> Expr {
    let etype = try self.enumType(from: expr)
    return self.enumAsExpr(try name.asSymbol(), for: etype) ?? .false
  }
  
  private func enumNameToOrdinal(expr: Expr, name: Expr) throws -> Expr {
    guard let e = try self.enumType(from: expr).`enum`(for: try name.asSymbol()) else {
      throw RuntimeError.eval(.unknownEnumValue, expr, name)
    }
    return .fixnum(Int64(e.ordinal))
  }
  
  private func enumNameToTag(expr: Expr, name: Expr) throws -> Expr {
    guard let e = try self.enumType(from: expr).`enum`(for: try name.asSymbol()) else {
      throw RuntimeError.eval(.unknownEnumValue, expr, name)
    }
    return e.tag
  }
  
  private func enumOrdinalToEnum(expr: Expr, ord: Expr) throws -> Expr {
    let etype = try self.enumType(from: expr)
    guard let e = etype.`enum`(at: try ord.asInt()) else {
      return .false
    }
    return self.enumAsExpr(e.ordinal, for: etype)
  }
  
  private func enumOrdinalToName(expr: Expr, ord: Expr) throws -> Expr {
    guard let e = try self.enumType(from: expr).`enum`(at: try ord.asInt()) else {
      throw RuntimeError.eval(.unknownEnumValue, expr, name)
    }
    return .symbol(e.name)
  }
  
  private func enumOrdinalToTag(expr: Expr, ord: Expr) throws -> Expr {
    guard let e = try self.enumType(from: expr).`enum`(at: try ord.asInt()) else {
      throw RuntimeError.eval(.unknownEnumValue, expr, name)
    }
    return e.tag
  }
  
  private func isEnum(expr: Expr) -> Expr {
    guard case .tagged(.object(let obj), .fixnum(_)) = expr, obj is EnumType else {
      return .false
    }
    return .true
  }
  
  private func getEnumType(expr: Expr) throws -> Expr {
    return .object(try self.enumComponents(from: expr).0)
  }
  
  private func enumName(expr: Expr) throws -> Expr {
    return .symbol(try self.`enum`(from: expr).name)
  }
  
  private func enumOrdinal(expr: Expr) throws -> Expr {
    return .fixnum(Int64(try self.`enum`(from: expr).ordinal))
  }
  
  private func enumTag(expr: Expr) throws -> Expr {
    return try self.`enum`(from: expr).tag
  }
  
  private func enumNext(expr: Expr) throws -> Expr {
    let (etype, index) = try self.enumComponents(from: expr)
    guard index >= 0 && index < etype.enumCount - 1 else {
      return .false
    }
    return self.enumAsExpr(index + 1, for: etype)
  }
  
  private func enumPrev(expr: Expr) throws -> Expr {
    let (etype, index) = try self.enumComponents(from: expr)
    guard index > 0 && index < etype.enumCount else {
      return .false
    }
    return self.enumAsExpr(index - 1, for: etype)
  }
  
  private func enumEqual(expr: Expr, args: Arguments) throws -> Expr {
    let (etype, index) = try self.enumComponents(from: expr)
    for arg in args {
      let (etype2, index2) = try self.enumComponents(from: arg)
      guard etype == etype2 && index == index2 else {
        return .false
      }
    }
    return .true
  }
  
  private func enumLessThan(expr: Expr, args: Arguments) throws -> Expr {
    var (etype, index) = try self.enumComponents(from: expr)
    for arg in args {
      let (etype2, index2) = try self.enumComponents(from: arg)
      guard etype2 == etype && index2 > index else {
        return .false
      }
      index = index2
    }
    return .true
  }
  
  private func enumLessThanEqual(expr: Expr, args: Arguments) throws -> Expr {
    var (etype, index) = try self.enumComponents(from: expr)
    for arg in args {
      let (etype2, index2) = try self.enumComponents(from: arg)
      guard etype2 == etype && index2 >= index else {
        return .false
      }
      index = index2
    }
    return .true
  }
  
  private func enumGreaterThan(expr: Expr, args: Arguments) throws -> Expr {
    var (etype, index) = try self.enumComponents(from: expr)
    for arg in args {
      let (etype2, index2) = try self.enumComponents(from: arg)
      guard etype2 == etype && index2 < index else {
        return .false
      }
      index = index2
    }
    return .true
  }
  
  private func enumGreaterThanEqual(expr: Expr, args: Arguments) throws -> Expr {
    var (etype, index) = try self.enumComponents(from: expr)
    for arg in args {
      let (etype2, index2) = try self.enumComponents(from: arg)
      guard etype2 == etype && index2 <= index else {
        return .false
      }
      index = index2
    }
    return .true
  }
  
  private func isEnumSet(expr: Expr) -> Expr {
    guard case .tagged(.object(let obj), .object(let bsobj)) = expr,
          obj is EnumType,
          bsobj is NativeBitset else {
      return .false
    }
    return .true
  }
  
  private func enumSet(expr: Expr, args: Arguments) throws -> Expr {
    let estype = try self.enumType(from: expr)
    let bitset = Bitset()
    for arg in args {
      bitset.add(try self.enumSetMemberOrdinal(arg, for: estype))
    }
    return self.enumSetAsExpr(NativeBitset(bitset), for: estype)
  }
  
  private func listToEnumSet(expr: Expr, list: Expr) throws -> Expr {
    let estype = try self.enumType(from: expr)
    let bitset = Bitset()
    var lst = list
    while case .pair(let car, let cdr) = lst {
      bitset.add(try self.enumSetMemberOrdinal(car, for: estype))
      lst = cdr
    }
    return self.enumSetAsExpr(NativeBitset(bitset), for: estype)
  }
  
  private func enumTypeToEnumSet(expr: Expr) throws -> Expr {
    let estype = try self.enumType(from: expr)
    let bitset = Bitset()
    for i in 0..<estype.enumCount {
      bitset.add(i)
    }
    return self.enumSetAsExpr(NativeBitset(bitset), for: estype)
  }
  
  private func enumSetProjection(expr: Expr, eset: Expr) throws -> Expr {
    let etype: EnumType
    switch expr {
      case .object(_):
        etype = try self.enumType(from: expr)
      default:
        let (et, _) = try self.enumSetComponents(from: expr)
        etype = et
    }
    let (estype, nbs) = try self.enumSetComponents(from: eset)
    let bitset = Bitset()
    for i in nbs.bitset {
      if let name = estype.`enum`(at: i)?.name,
         let j = etype.`enum`(for: name)?.ordinal {
        bitset.add(j)
      }
    }
    return self.enumSetAsExpr(NativeBitset(bitset), for: etype)
  }
  
  private func enumSetCopy(expr: Expr) throws -> Expr {
    let (etype, nbs) = try self.enumSetComponents(from: expr)
    return self.enumSetAsExpr(NativeBitset(copy: nbs.bitset), for: etype)
  }
  
  private func isEnumSetEmpty(eset: Expr) throws -> Expr {
    return .makeBoolean(try self.enumSetComponents(from: eset).1.bitset.isEmpty())
  }
  
  private func enumSetContains(eset: Expr, e: Expr) throws -> Expr {
    let (etype, nbs) = try self.enumSetComponents(from: eset)
    let i = try self.enumSetMemberOrdinal(e, for: etype)
    return .makeBoolean(nbs.bitset.contains(i))
  }

  private func enumSetDisjoint(eset1: Expr, eset2: Expr) throws -> Expr {
    let (etype1, nbs1) = try self.enumSetComponents(from: eset1)
    let (etype2, nbs2) = try self.enumSetComponents(from: eset2)
    guard etype1 == etype2 else {
      throw RuntimeError.eval(.incompatibleEnumSetTypes, eset1, eset2)
    }
    return .makeBoolean(nbs1.bitset.intersectionCount(nbs2.bitset) == 0)
  }
  
  private func enumSetSubset(eset1: Expr, eset2: Expr) throws -> Expr {
    let (etype1, nbs1) = try self.enumSetComponents(from: eset1)
    let (etype2, nbs2) = try self.enumSetComponents(from: eset2)
    for i in nbs1.bitset {
      if let name = etype1.enum(at: i)?.name {
        if let j = etype2.enum(for: name)?.ordinal, nbs2.bitset.contains(j) {
          // ok
        } else {
          return .false
        }
      }
    }
    return .true
  }
  
  private func enumSetEqual(eset1: Expr, eset2: Expr) throws -> Expr {
    let (etype1, nbs1) = try self.enumSetComponents(from: eset1)
    let (etype2, nbs2) = try self.enumSetComponents(from: eset2)
    guard etype1 == etype2 else {
      throw RuntimeError.eval(.incompatibleEnumSetTypes, eset1, eset2)
    }
    return .makeBoolean(nbs1.bitset == nbs2.bitset)
  }
  
  private func enumSetLessThan(eset1: Expr, eset2: Expr) throws -> Expr {
    let (etype1, nbs1) = try self.enumSetComponents(from: eset1)
    let (etype2, nbs2) = try self.enumSetComponents(from: eset2)
    guard etype1 == etype2 else {
      throw RuntimeError.eval(.incompatibleEnumSetTypes, eset1, eset2)
    }
    let nbs1Count = nbs1.bitset.count()
    return .makeBoolean(nbs1.bitset.intersectionCount(nbs2.bitset) == nbs1Count &&
                        nbs1Count < nbs2.bitset.count())
  }
  
  private func enumSetLessThanEqual(eset1: Expr, eset2: Expr) throws -> Expr {
    let (etype1, nbs1) = try self.enumSetComponents(from: eset1)
    let (etype2, nbs2) = try self.enumSetComponents(from: eset2)
    guard etype1 == etype2 else {
      throw RuntimeError.eval(.incompatibleEnumSetTypes, eset1, eset2)
    }
    return .makeBoolean(nbs1.bitset.intersectionCount(nbs2.bitset) == nbs1.bitset.count())
  }
  
  private func enumSetGreaterThan(eset1: Expr, eset2: Expr) throws -> Expr {
    let (etype2, nbs2) = try self.enumSetComponents(from: eset1)
    let (etype1, nbs1) = try self.enumSetComponents(from: eset2)
    guard etype1 == etype2 else {
      throw RuntimeError.eval(.incompatibleEnumSetTypes, eset1, eset2)
    }
    let nbs1Count = nbs1.bitset.count()
    return .makeBoolean(nbs1.bitset.intersectionCount(nbs2.bitset) == nbs1Count &&
                        nbs1Count < nbs2.bitset.count())
  }
  
  private func enumSetGreaterThanEqual(eset1: Expr, eset2: Expr) throws -> Expr {
    let (etype2, nbs2) = try self.enumSetComponents(from: eset1)
    let (etype1, nbs1) = try self.enumSetComponents(from: eset2)
    guard etype1 == etype2 else {
      throw RuntimeError.eval(.incompatibleEnumSetTypes, eset1, eset2)
    }
    return .makeBoolean(nbs1.bitset.intersectionCount(nbs2.bitset) == nbs1.bitset.count())
  }
  
  private func enumSetToList(eset: Expr) throws -> Expr {
    let (etype, nbs) = try self.enumSetComponents(from: eset)
    var res = Expr.null
    for i in (0..<nbs.bitset.count()).reversed() {
      if let name = etype.enum(at: i)?.name {
        res = .pair(.symbol(name), res)
      }
    }
    return res
  }
  
  private func enumSetToEnumList(eset: Expr) throws -> Expr {
    let (etype, nbs) = try self.enumSetComponents(from: eset)
    var res = Expr.null
    for i in (0..<nbs.bitset.count()).reversed() {
      res = .pair(self.enumAsExpr(i, for: etype), res)
    }
    return res
  }
  
  private func enumSetNext(eset: Expr, e: Expr) throws -> Expr {
    let (etype, nbs) = try self.enumSetComponents(from: eset)
    guard e.isTrue else {
      return .false
    }
    if let i = nbs.bitset.next(current: try self.enumSetMemberOrdinal(e, for: etype)) {
      return self.enumAsExpr(i, for: etype)
    } else {
      return .false
    }
  }
  
  private func enumSetType(eset: Expr) throws -> Expr {
    if case .object(let obj) = eset, obj is EnumType {
      return eset
    } else {
      return .object(try self.enumSetComponents(from: eset).0)
    }
  }
  
  private func enumSetBitset(eset: Expr) throws -> Expr {
    return .object(NativeBitset(copy: try self.enumSetComponents(from: eset).1.bitset))
  }
  
  private func enumSetSize(eset: Expr) throws -> Expr {
    return .fixnum(Int64(try self.enumSetComponents(from: eset).1.bitset.count()))
  }
  
  private func enumSetAdjoin(eset: Expr, args: Arguments) throws -> Expr {
    let (etype, nbs) = try self.enumSetComponents(from: eset)
    for arg in args {
      nbs.bitset.add(try self.enumSetMemberOrdinal(arg, for: etype))
    }
    return eset
  }
  
  private func enumSetAdjoinAll(eset: Expr, list: Expr) throws -> Expr {
    let (etype, nbs) = try self.enumSetComponents(from: eset)
    var lst = list
    while case .pair(let arg, let cdr) = lst {
      nbs.bitset.add(try self.enumSetMemberOrdinal(arg, for: etype))
      lst = cdr
    }
    return eset
  }
  
  private func enumSetDelete(eset: Expr, args: Arguments) throws -> Expr {
    let (etype, nbs) = try self.enumSetComponents(from: eset)
    for arg in args {
      nbs.bitset.remove(try self.enumSetMemberOrdinal(arg, for: etype))
    }
    return eset
  }
  
  private func enumSetDeleteAll(eset: Expr, list: Expr) throws -> Expr {
    let (etype, nbs) = try self.enumSetComponents(from: eset)
    var lst = list
    while case .pair(let arg, let cdr) = lst {
      nbs.bitset.remove(try self.enumSetMemberOrdinal(arg, for: etype))
      lst = cdr
    }
    return eset
  }
  
  private func enumSetUnion(eset1: Expr, eset2: Expr) throws -> Expr {
    let (etype1, nbs1) = try self.enumSetComponents(from: eset1)
    let (etype2, nbs2) = try self.enumSetComponents(from: eset2)
    guard etype1 == etype2 else {
      throw RuntimeError.eval(.incompatibleEnumSetTypes, eset1, eset2)
    }
    nbs1.bitset.union(nbs2.bitset)
    return eset1
  }
  
  private func enumSetUnionNew(eset1: Expr, eset2: Expr) throws -> Expr {
    let (etype1, nbs1) = try self.enumSetComponents(from: eset1)
    let (etype2, nbs2) = try self.enumSetComponents(from: eset2)
    guard etype1 == etype2 else {
      throw RuntimeError.eval(.incompatibleEnumSetTypes, eset1, eset2)
    }
    let res = Bitset(nbs1.bitset)
    res.union(nbs2.bitset)
    return self.enumSetAsExpr(NativeBitset(res), for: etype1)
  }
  
  private func enumSetIntersection(eset1: Expr, eset2: Expr) throws -> Expr {
    let (etype1, nbs1) = try self.enumSetComponents(from: eset1)
    let (etype2, nbs2) = try self.enumSetComponents(from: eset2)
    guard etype1 == etype2 else {
      throw RuntimeError.eval(.incompatibleEnumSetTypes, eset1, eset2)
    }
    nbs1.bitset.intersection(nbs2.bitset)
    return eset1
  }
  
  private func enumSetIntersectionNew(eset1: Expr, eset2: Expr) throws -> Expr {
    let (etype1, nbs1) = try self.enumSetComponents(from: eset1)
    let (etype2, nbs2) = try self.enumSetComponents(from: eset2)
    guard etype1 == etype2 else {
      throw RuntimeError.eval(.incompatibleEnumSetTypes, eset1, eset2)
    }
    let res = Bitset(nbs1.bitset)
    res.intersection(nbs2.bitset)
    return self.enumSetAsExpr(NativeBitset(res), for: etype1)
  }
  
  private func enumSetDifference(eset1: Expr, eset2: Expr) throws -> Expr {
    let (etype1, nbs1) = try self.enumSetComponents(from: eset1)
    let (etype2, nbs2) = try self.enumSetComponents(from: eset2)
    guard etype1 == etype2 else {
      throw RuntimeError.eval(.incompatibleEnumSetTypes, eset1, eset2)
    }
    nbs1.bitset.difference(nbs2.bitset)
    return eset1
  }
  
  private func enumSetDifferenceNew(eset1: Expr, eset2: Expr) throws -> Expr {
    let (etype1, nbs1) = try self.enumSetComponents(from: eset1)
    let (etype2, nbs2) = try self.enumSetComponents(from: eset2)
    guard etype1 == etype2 else {
      throw RuntimeError.eval(.incompatibleEnumSetTypes, eset1, eset2)
    }
    let res = Bitset(nbs1.bitset)
    res.difference(nbs2.bitset)
    return self.enumSetAsExpr(NativeBitset(res), for: etype1)
  }
  
  private func enumSetXor(eset1: Expr, eset2: Expr) throws -> Expr {
    let (etype1, nbs1) = try self.enumSetComponents(from: eset1)
    let (etype2, nbs2) = try self.enumSetComponents(from: eset2)
    guard etype1 == etype2 else {
      throw RuntimeError.eval(.incompatibleEnumSetTypes, eset1, eset2)
    }
    nbs1.bitset.symmetricDifference(nbs2.bitset)
    return eset1
  }
  
  private func enumSetXorNew(eset1: Expr, eset2: Expr) throws -> Expr {
    let (etype1, nbs1) = try self.enumSetComponents(from: eset1)
    let (etype2, nbs2) = try self.enumSetComponents(from: eset2)
    guard etype1 == etype2 else {
      throw RuntimeError.eval(.incompatibleEnumSetTypes, eset1, eset2)
    }
    let res = Bitset(nbs1.bitset)
    res.symmetricDifference(nbs2.bitset)
    return self.enumSetAsExpr(NativeBitset(res), for: etype1)
  }
  
  private func enumSetComplement(eset: Expr) throws -> Expr {
    let (etype, nbs) = try self.enumSetComponents(from: eset)
    for i in 0..<etype.enumCount {
      if nbs.bitset.contains(i) {
        nbs.bitset.remove(i)
      } else {
        nbs.bitset.add(i)
      }
    }
    return eset
  }
  
  private func enumSetComplementNew(eset: Expr) throws -> Expr {
    let (etype, nbs) = try self.enumSetComponents(from: eset)
    let bitset = Bitset(nbs.bitset)
    for i in 0..<etype.enumCount {
      if bitset.contains(i) {
        bitset.remove(i)
      } else {
        bitset.add(i)
      }
    }
    return self.enumSetAsExpr(NativeBitset(bitset), for: etype)
  }
  
  private func makeEnumeration(list: Expr) throws -> Expr {
    var lst = list
    var entries: [(Symbol, Expr?)] = []
    let bitset = Bitset()
    var i = 0
    while case .pair(let car, let cdr) = lst {
      switch car {
        case .symbol(let sym):
          entries.append((sym, .symbol(sym)))
        case .pair(.symbol(let sym), .pair(let tag, .null)):
          entries.append((sym, tag))
        default:
          throw RuntimeError.eval(.invalidEnumSpecifier, car)
      }
      bitset.add(i)
      i += 1
      lst = cdr
    }
    guard entries.count > 0 else {
      throw RuntimeError.eval(.enumTypeEmpty)
    }
    return self.enumSetAsExpr(NativeBitset(bitset),
                              for: EnumType(name: name, entries: entries, tag: .false))
  }
  
  private func enumSetUniverse(eset: Expr) throws -> Expr {
    let (etype, _) = try self.enumSetComponents(from: eset)
    let bitset = Bitset()
    for i in 0..<etype.enumCount {
      bitset.add(i)
    }
    return self.enumSetAsExpr(NativeBitset(bitset), for: etype)
  }
  
  private func enumSetMember(s: Expr, eset: Expr) throws -> Expr {
    let sym = try s.asSymbol()
    let (etype, nbs) = try self.enumSetComponents(from: eset)
    if let i = etype.`enum`(for: sym)?.ordinal {
      return .makeBoolean(nbs.bitset.contains(i))
    } else {
      return .false
    }
  }
}

public final class EnumType: NativeObject {

  /// Representation of an enum value
  public struct Enum {
    
    /// The name of the enum value
    public let name: Symbol
    
    /// The ordinal of the enum value
    public let ordinal: Int
    
    /// The tag of the enum value
    public let tag: Expr
  }
  
  /// Type representing enum types
  public static let type = Type.objectType(Symbol(uninterned: "enum-type"))
  
  /// The enum type name
  public let name: Expr?
  
  /// The enum values
  public let enums: [Enum]
  
  /// The name table
  public let nameTable: [Symbol : Int]
  
  /// The tag of the enum type
  public let tag: Expr
  
  /// Make sure we don't collect garbage more than once
  internal var gctag: UInt8 = 0
  
  /// Initializer
  public init(name: Expr?, entries: [(Symbol, Expr?)], tag: Expr) {
    var enums: [Enum] = []
    var nameTable: [Symbol : Int] = [:]
    var i = 0
    for (sym, tag) in entries {
      enums.append(Enum(name: sym, ordinal: i, tag: tag ?? .fixnum(Int64(i))))
      nameTable[sym] = i
      i += 1
    }
    self.name = name
    self.enums = enums
    self.nameTable = nameTable
    self.tag = tag
  }
  
  public var enumCount: Int {
    return self.enums.count
  }
  
  public func `enum`(for name: Symbol) -> Enum? {
    if let i = self.nameTable[name] {
      return self.enums[i]
    } else {
      return nil
    }
  }
  
  public func `enum`(at index: Int) -> Enum? {
    if index >= 0 && index < self.enums.count {
      return self.enums[index]
    } else {
      return nil
    }
  }
  
  public override var type: Type {
    return Self.type
  }
  
  public override var string: String {
    var enumNames = self.enums.prefix(12).map {
      /* $0.tag.isTrue ? "\($0.name)(\($0.tag))" : */
      $0.name.description
    }.joined(separator: ", ")
    if self.enums.count > 12 {
      enumNames.append(", …")
    }
    if let name = self.name {
      return "#<enum-type \(name): \(enumNames)>"
    } else {
      return "#<enum-type \(self.identityString): \(enumNames)>"
    }
  }
  
  public override var tagString: String {
    if let name = self.name {
      return "enum \(name)"
    } else {
      return "enum \(self.identityString)"
    }
  }
  
  public override func mark(in gc: GarbageCollector) {
    if self.gctag != gc.tag {
      self.gctag = gc.tag
      if let name = self.name {
        gc.markLater(name)
      }
      gc.markLater(self.tag)
      for e in self.enums {
        gc.markLater(e.tag)
      }
    }
  }
}
