//
//  BitsetLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 05/03/2022.
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

///
/// Bitset library: fast native growable bitsets
///
public final class BitsetLibrary: NativeLibrary {
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "bitset"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"],    "define", "not")
    self.`import`(from: ["lispkit", "control"], "do", "if")
    self.`import`(from: ["lispkit", "math"],    "fx1+")
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define("bitset-type-tag", as: NativeBitset.type.objectTypeTag())
    self.define(Procedure("bitset?", isBitset))
    self.define(Procedure("bitset", bitset))
    self.define(Procedure("list->bitset", listToBitset))
    self.define(Procedure("bitset-copy", bitsetCopy))
    self.define(Procedure("bitset-size", bitsetSize))
    self.define(Procedure("bitset-next", bitsetNext))
    self.define(Procedure("bitset-empty?", bitsetEmpty))
    self.define(Procedure("bitset-disjoint?", bitsetDisjoint))
    self.define(Procedure("bitset-subset?", bitsetSubset))
    self.define(Procedure("bitset-contains?", bitsetContains))
    self.define(Procedure("bitset-adjoin!", bitsetAdjoin))
    self.define(Procedure("bitset-adjoin-all!", bitsetAdjoinAll))
    self.define(Procedure("bitset-delete!", bitsetDelete))
    self.define(Procedure("bitset-delete-all!", bitsetDeleteAll))
    self.define(Procedure("bitset-union!", bitsetUnion))
    self.define(Procedure("bitset-intersection!", bitsetIntersection))
    self.define(Procedure("bitset-difference!", bitsetDifference))
    self.define(Procedure("bitset-xor!", bitsetXor))
    self.define(Procedure("bitset=?", bitsetEqual))
    self.define(Procedure("bitset<?", bitsetLessThan))
    self.define(Procedure("bitset<=?", bitsetLessThanEqual))
    self.define(Procedure("bitset>?", bitsetGreaterThan))
    self.define(Procedure("bitset>=?", bitsetGreaterThanEqual))
    self.define(Procedure("bitset->list", bitsetToList))
    self.define("bitset-for-each", via: """
      (define (bitset-for-each f bs)
        (do ((i (bitset-next bs 0) (bitset-next bs (fx1+ i))))
            ((not i))
          (f i)))
    """)
    self.define("bitset-fold", via: """
      (define (bitset-fold f z bs)
        (do ((i (bitset-next bs 0) (bitset-next bs (fx1+ i)))
             (acc z (f i acc)))
            ((not i) acc)))
    """)
    self.define("bitset-any?", via: """
      (define (bitset-any? f bs)
        (do ((i (bitset-next bs 0) (bitset-next bs (fx1+ i))))
            ((or (not i) (f i)) i)))
    """)
    self.define("bitset-every?", via: """
      (define (bitset-every? f bs)
        (do ((i (bitset-next bs 0) (bitset-next bs (fx1+ i))))
            ((or (not i) (not (f i))) (not i))))
    """)
    self.define("bitset-filter", via: """
      (define (bitset-filter f bs)
        (do ((i (bitset-next bs 0) (bitset-next bs (fx1+ i)))
             (res (bitset)))
            ((not i) res)
          (if (f i) (bitset-adjoin! res i))))
    """)
    self.define("bitset-filter!", via: """
      (define (bitset-filter! f bs)
        (do ((i (bitset-next bs 0) (bitset-next bs (fx1+ i))))
            ((not i))
          (if (not (f i)) (bitset-delete! bs i))))
    """)
  }
  
  private func asBitset(expr: Expr) throws -> NativeBitset {
    guard case .object(let obj) = expr, let nbs = obj as? NativeBitset else {
      throw RuntimeError.type(expr, expected: [NativeBitset.type])
    }
    return nbs
  }
  
  private func isBitset(expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is NativeBitset else {
      return .false
    }
    return .true
  }
  
  private func bitset(args: Arguments) throws -> Expr {
    let bs = Bitset()
    for arg in args {
      bs.add(try arg.asInt(above: 0, below: 100000))
    }
    return .object(NativeBitset(bs))
  }
  
  private func listToBitset(expr: Expr) throws -> Expr {
    let bs = Bitset()
    var lst = expr
    while case .pair(let car, let cdr) = lst {
      bs.add(try car.asInt(above: 0, below: 100000))
      lst = cdr
    }
    return .object(NativeBitset(bs))
  }
  
  private func bitsetCopy(expr: Expr) throws -> Expr {
    return .object(NativeBitset(copy: try self.asBitset(expr: expr).bitset))
  }
  
  private func bitsetSize(expr: Expr) throws -> Expr {
    return .fixnum(Int64(try self.asBitset(expr: expr).bitset.count()))
  }
  
  private func bitsetNext(expr: Expr, bit: Expr?) throws -> Expr {
    if let value = try bit?.asInt(above: 0, below: 100000),
       let next = try self.asBitset(expr: expr).bitset.next(current: value) {
      return .fixnum(Int64(next))
    } else {
      return .false
    }
  }
  
  private func bitsetEmpty(expr: Expr) throws -> Expr {
    return .makeBoolean(try self.asBitset(expr: expr).bitset.isEmpty())
  }
  
  private func bitsetDisjoint(bs1: Expr, bs2: Expr) throws -> Expr {
    return .makeBoolean(try self.asBitset(expr: bs1).bitset.intersectionCount(
                              try self.asBitset(expr: bs2).bitset) == 0)
  }
  
  private func bitsetSubset(bs1: Expr, bs2: Expr) throws -> Expr {
    let bitset1 = try self.asBitset(expr: bs1).bitset
    let bitset2 = try self.asBitset(expr: bs2).bitset
    return .makeBoolean(bitset1.intersectionCount(bitset2) == bitset1.count())
  }
  
  private func bitsetContains(bs: Expr, args: Arguments) throws -> Expr {
    let bitset = try self.asBitset(expr: bs).bitset
    for arg in args {
      if !bitset.contains(try arg.asInt(above: 0, below: 1000000)) {
        return .false
      }
    }
    return .true
  }
  
  private func bitsetAdjoin(bs: Expr, args: Arguments) throws -> Expr {
    let nbs = try self.asBitset(expr: bs)
    for arg in args {
      nbs.bitset.add(try arg.asInt(above: 0, below: 1000000))
    }
    return .object(nbs)
  }
  
  private func bitsetAdjoinAll(bs: Expr, expr: Expr) throws -> Expr {
    let nbs = try self.asBitset(expr: bs)
    var lst = expr
    while case .pair(let car, let cdr) = lst {
      nbs.bitset.add(try car.asInt(above: 0, below: 1000000))
      lst = cdr
    }
    return .object(nbs)
  }
  
  private func bitsetDelete(bs: Expr, args: Arguments) throws -> Expr {
    let nbs = try self.asBitset(expr: bs)
    for arg in args {
      nbs.bitset.remove(try arg.asInt(above: 0, below: 1000000))
    }
    return .object(nbs)
  }
  
  private func bitsetDeleteAll(bs: Expr, expr: Expr) throws -> Expr {
    let nbs = try self.asBitset(expr: bs)
    var lst = expr
    while case .pair(let car, let cdr) = lst {
      nbs.bitset.remove(try car.asInt(above: 0, below: 1000000))
      lst = cdr
    }
    return .object(nbs)
  }
  
  private func bitsetUnion(bs: Expr, args: Arguments) throws -> Expr {
    let nbs = try self.asBitset(expr: bs)
    for arg in args {
      nbs.bitset.union(try self.asBitset(expr: arg).bitset)
    }
    return .object(nbs)
  }
  
  private func bitsetIntersection(bs: Expr, args: Arguments) throws -> Expr {
    let nbs = try self.asBitset(expr: bs)
    for arg in args {
      nbs.bitset.intersection(try self.asBitset(expr: arg).bitset)
    }
    return .object(nbs)
  }
  
  private func bitsetDifference(bs: Expr, args: Arguments) throws -> Expr {
    let nbs = try self.asBitset(expr: bs)
    for arg in args {
      nbs.bitset.difference(try self.asBitset(expr: arg).bitset)
    }
    return .object(nbs)
  }
  
  private func bitsetXor(bs: Expr, args: Arguments) throws -> Expr {
    let nbs = try self.asBitset(expr: bs)
    for arg in args {
      nbs.bitset.symmetricDifference(try self.asBitset(expr: arg).bitset)
    }
    return .object(nbs)
  }
  
  private func bitsetEqual(bs: Expr, args: Arguments) throws -> Expr {
    let bitset = try self.asBitset(expr: bs).bitset
    for arg in args {
      if try !(bitset == self.asBitset(expr: arg).bitset) {
        return .false
      }
    }
    return .true
  }
  
  private func bitsetLessThan(bs: Expr, args: Arguments) throws -> Expr {
    var lhs = try self.asBitset(expr: bs).bitset
    var lhsCount = lhs.count()
    for arg in args {
      let rhs = try self.asBitset(expr: arg).bitset
      let rhsCount = rhs.count()
      if !(lhs.intersectionCount(rhs) == lhsCount && rhsCount > lhsCount) {
        return .false
      }
      lhs = rhs
      lhsCount = rhsCount
    }
    return .true
  }
  
  private func bitsetLessThanEqual(bs: Expr, args: Arguments) throws -> Expr {
    var lhs = try self.asBitset(expr: bs).bitset
    var lhsCount = lhs.count()
    for arg in args {
      let rhs = try self.asBitset(expr: arg).bitset
      let rhsCount = rhs.count()
      if lhs.intersectionCount(rhs) != lhsCount {
        return .false
      }
      lhs = rhs
      lhsCount = rhsCount
    }
    return .true
  }
  
  private func bitsetGreaterThan(bs: Expr, args: Arguments) throws -> Expr {
    var lhs = try self.asBitset(expr: bs).bitset
    var lhsCount = lhs.count()
    for arg in args {
      let rhs = try self.asBitset(expr: arg).bitset
      let rhsCount = rhs.count()
      if !(lhs.intersectionCount(rhs) == rhsCount && lhsCount > rhsCount) {
        return .false
      }
      lhs = rhs
      lhsCount = rhsCount
    }
    return .true
  }
  
  private func bitsetGreaterThanEqual(bs: Expr, args: Arguments) throws -> Expr {
    var lhs = try self.asBitset(expr: bs).bitset
    for arg in args {
      let rhs = try self.asBitset(expr: arg).bitset
      if lhs.intersectionCount(rhs) != rhs.count() {
        return .false
      }
      lhs = rhs
    }
    return .true
  }
  
  private func bitsetToList(bs: Expr) throws -> Expr {
    let bitset = try self.asBitset(expr: bs).bitset
    let arr = Array(bitset).reversed()
    var res = Expr.null
    for x in arr {
      res = .pair(.fixnum(Int64(x)), res)
    }
    return res
  }
}

public final class NativeBitset: NativeObject {
  
  /// Type representing enum sets
  public static let type = Type.objectType(Symbol(uninterned: "bitset"))
  
  /// Internal bitset representation
  public let bitset: Bitset
  
  init(_ bs: Bitset) {
    self.bitset = bs
  }
  
  init(copy: Bitset? = nil) {
    self.bitset = copy == nil ? Bitset() : Bitset(copy!)
  }
  
  public override var type: Type {
    return Self.type
  }
  
  public override var string: String {
    var res = self.bitset.prefix(50).map { $0.description }.joined(separator: ", ")
    if self.bitset.count() > 50 {
      res.append(", …")
    }
    return "#<bitset: \(res)>"
  }
  
  public override func unpack() -> Exprs {
    var bits: Exprs = []
    for bit in self.bitset {
      bits.append(.fixnum(Int64(bit)))
    }
    return [.makeString(self.identityString),
            .vector(Collection(kind: .immutableVector, exprs: bits))]
  }
  
  public override var tagString: String {
    var res = self.bitset.prefix(40).map { $0.description }.joined(separator: ", ")
    if self.bitset.count() > 40 {
      res.append(", …")
    }
    return res
  }
}
