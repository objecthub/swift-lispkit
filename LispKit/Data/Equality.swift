//
//  Equality.swift
//  LispKit
//
//  Created by Matthias Zenger on 30/01/2016.
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

import Foundation
import NumberKit


//-------- MARK: - Equals

struct Equality: Hashable {
  let ref1: Reference
  let ref2: Reference
  
  init(_ ref1: Reference, _ ref2: Reference) {
    self.ref1 = ref1
    self.ref2 = ref2
  }
  
  var hashValue: Int {
    return ref1.hashValue &+ ref2.hashValue
  }
}

func ==(lhs: Equality, rhs: Equality) -> Bool {
  return lhs.ref1 == rhs.ref1 && lhs.ref2 == rhs.ref2 ||
         lhs.ref1 == rhs.ref2 && lhs.ref2 == rhs.ref1
}

func equalExpr(this: Expr, _ that: Expr) -> Bool {
  var equalities = Set<Equality>()
  
  func equals(lhs: Expr, _ rhs: Expr) -> Bool {
    switch (lhs, rhs) {
      case (.Undef, .Undef),
           (.Void, .Void),
           (.Eof, .Eof),
           (.Null, .Null),
           (.True, .True),
           (.False, .False):
        return true
      case (.Sym(let sym1), .Sym(let sym2)):
        return sym1 === sym2
      case (.Fixnum(_), _),
           (.Bignum(_), _),
           (.Rat(_), _),
           (.Bigrat(_), _),
           (.Flonum(_), _),
           (.Complexnum(_), _):
        return lhs.isExactNumber == rhs.isExactNumber && compare(lhs, with: rhs) == 0
      case (.Char(let ch1), .Char(let ch2)):
        return ch1 == ch2
      case (.Str(let str1), .Str(let str2)):
        return str1 == str2
      case (.Bytes(let bvector1), .Bytes(let bvector2)):
        guard bvector1.value.count == bvector2.value.count else {
          return false
        }
        for i in bvector1.value.indices {
          guard bvector1.value[i] == bvector2.value[i] else {
            return false
          }
        }
        return true
      case (.Pair(let car1, let cdr1), .Pair(let car2, let cdr2)):
        return equals(car1, car2) && equals(cdr1, cdr2)
      case (.Box(let cell1), .Box(let cell2)):
        guard cell1 !== cell2 else {
          return true
        }
        let equality = Equality(cell1, cell2)
        guard !equalities.contains(equality) else {
          return true
        }
        equalities.insert(equality)
        return cell1.value == cell2.value
      case (.MutablePair(let tuple1), .MutablePair(let tuple2)):
        guard tuple1 !== tuple2 else {
          return true
        }
        let equality = Equality(tuple1, tuple2)
        guard !equalities.contains(equality) else {
          return true
        }
        equalities.insert(equality)
        return tuple1.fst == tuple2.fst && tuple1.snd == tuple2.snd
      case (.Vector(let vector1), .Vector(let vector2)):
        guard vector1 !== vector2 else {
          return true
        }
        let equality = Equality(vector1, vector2)
        guard !equalities.contains(equality) else {
          return true
        }
        guard vector1.exprs.count == vector2.exprs.count else {
          return false
        }
        equalities.insert(equality)
        for i in vector1.exprs.indices {
          guard equals(vector1.exprs[i], vector2.exprs[i]) else {
            return false
          }
        }
        return true
      case (.Record(let record1), .Record(let record2)):
        guard record1 !== record2 else {
          return true
        }
        let equality = Equality(record1, record2)
        guard !equalities.contains(equality) else {
          return true
        }
        guard record1.sameKindAs(record2) && record1.exprs.count == record2.exprs.count else {
          return false
        }
        equalities.insert(equality)
        for i in record1.exprs.indices {
          guard equals(record1.exprs[i], record2.exprs[i]) else {
            return false
          }
        }
        return true
      case (.Map(let map1), .Map(let map2)):
        // Identical maps are also equals
        guard map1 !== map2 else {
          return true
        }
        // Maps with incompatible hashing and equality functions are not equals
        switch (map1.equiv, map2.equiv) {
          case (.Eq, .Eq), (.Eqv, .Eqv), (.Equal, .Equal):
            break;
          case (.Custom(let procs1), .Custom(let procs2)):
            guard procs1.eql == procs2.eql && procs1.hsh == procs2.hsh else {
              return false
            }
          default:
            return false
        }
        // Assume equality (to handle recursive dependencies)
        let equality = Equality(map1, map2)
        guard !equalities.contains(equality) else {
          return true
        }
        equalities.insert(equality)
        // Check for structural equality of all mappings
        // TODO: Consider optimizing this; the algorithm currently has complexity O(n*n)
        let mappings1 = map1.mappings
        var count2 = 0
        outer:
        for (key2, value2) in map2.mappings {
          count2 += 1
          for (key1, value1) in mappings1 {
            if equals(key1, key2) && equals(value1, value2) {
              continue outer
            }
          }
          return false
        }
        return count2 == mappings1.count
      case (.Promise(let promise1), .Promise(let promise2)):
        return promise1 == promise2
      case (.Proc(let e1), .Proc(let e2)):
        return e1 == e2
      case (.Special(let e1), .Special(let e2)):
        return e1 == e2
      case (.Prt(let p1), .Prt(let p2)):
        return p1 == p2
      case (.Error(let e1), .Error(let e2)):
        return e1 == e2
      default:
        return false
    }
  }
  
  return equals(this, that)
}


//-------- MARK: - Eqv

func eqvExpr(lhs: Expr, _ rhs: Expr) -> Bool {
  switch (lhs, rhs) {
    case (.Undef, .Undef),
         (.Void, .Void),
         (.Eof, .Eof),
         (.Null, .Null),
         (.True, .True),
         (.False, .False):
      return true
    case (.Sym(let sym1), .Sym(let sym2)):
      return sym1 === sym2
    case (.Fixnum(_), _),
         (.Bignum(_), _),
         (.Rat(_), _),
         (.Bigrat(_), _),
         (.Flonum(_), _),
         (.Complexnum(_), _):
      return lhs.isExactNumber == rhs.isExactNumber && compare(lhs, with: rhs) == 0
    case (.Char(let ch1), .Char(let ch2)):
      return ch1 == ch2
    case (.Str(let str1), .Str(let str2)):
      return str1 === str2
    case (.Bytes(let bvector1), .Bytes(let bvector2)):
      return bvector1 === bvector2
    case (.Pair(let car1, let cdr1), .Pair(let car2, let cdr2)):
      return eqvExpr(car1, car2) && eqvExpr(cdr1, cdr2)
    case (.Box(let c1), .Box(let c2)):
      return c1 === c2
    case (.MutablePair(let t1), .MutablePair(let t2)):
      return t1 === t2
    case (.Vector(let vector1), .Vector(let vector2)):
      return vector1 === vector2
    case (.Record(let record1), .Record(let record2)):
      return record1 === record2
    case (.Map(let map1), .Map(let map2)):
      return map1 === map2
    case (.Promise(let promise1), .Promise(let promise2)):
      return promise1 === promise2
    case (.Proc(let e1), .Proc(let e2)):
      return e1 === e2
    case (.Special(let e1), .Special(let e2)):
      return e1 === e2
    case (.Error(let e1), .Error(let e2)):
      return e1 === e2
    default:
      return false
  }
}


//-------- MARK: - Eq

func eqExpr(lhs: Expr, _ rhs: Expr) -> Bool {
  switch (lhs, rhs) {
    case (.Undef, .Undef),
         (.Void, .Void),
         (.Eof, .Eof),
         (.Null, .Null),
         (.True, .True),
         (.False, .False):
      return true
    case (.Sym(let sym1), .Sym(let sym2)):
      return sym1 === sym2
    case (.Fixnum(let num1), .Fixnum(let num2)):
      return num1 == num2
    case (.Bignum(let num1), .Bignum(let num2)):
      return num1 == num2
    case (.Rat(let num1), .Rat(let num2)):
      return num1 == num2
    case (.Bigrat(let num1), .Bigrat(let num2)):
      return num1 == num2
    case (.Flonum(let num1), .Flonum(let num2)):
      return num1 == num2
    case (.Complexnum(let num1), .Complexnum(let num2)):
      return num1 == num2
    case (.Char(let ch1), .Char(let ch2)):
      return ch1 == ch2
    case (.Str(let str1), .Str(let str2)):
      return str1 === str2
    case (.Bytes(let bvector1), .Bytes(let bvector2)):
      return bvector1 === bvector2
    case (.Pair(let car1, let cdr1), .Pair(let car2, let cdr2)):
      return eqvExpr(car1, car2) && eqvExpr(cdr1, cdr2)
    case (.Box(let c1), .Box(let c2)):
      return c1 === c2
    case (.MutablePair(let t1), .MutablePair(let t2)):
      return t1 === t2
    case (.Vector(let vector1), .Vector(let vector2)):
      return vector1 === vector2
    case (.Record(let record1), .Vector(let record2)):
      return record1 === record2
    case (.Map(let map1), .Map(let map2)):
      return map1 === map2
    case (.Promise(let promise1), .Promise(let promise2)):
      return promise1 === promise2
    case (.Proc(let e1), .Proc(let e2)):
      return e1 === e2
    case (.Special(let e1), .Special(let e2)):
      return e1 === e2
    case (.Error(let e1), .Error(let e2)):
      return e1 === e2
    default:
      return false
  }
}


//-------- MARK: - Numeric comparisons

enum NumberPair {
  case FixnumPair(Int64, Int64)
  case BignumPair(BigInt, BigInt)
  case RationalPair(Rational<Int64>, Rational<Int64>)
  case BigRationalPair(Rational<BigInt>, Rational<BigInt>)
  case FlonumPair(Double, Double)
  case ComplexPair(Complex<Double>, Complex<Double>)
  
  init(_ fst: Expr, _ snd: Expr) throws {
    guard let res = NumberPair(fst, and: snd) else {
      try snd.assertTypeOf(.NumberType)
      try fst.assertTypeOf(.NumberType)
      preconditionFailure()
    }
    self = res
  }
  
  init?(_ fst: Expr, and snd: Expr) {
    switch (fst, snd) {
      case (.Fixnum(let lhs), .Fixnum(let rhs)):
        self = FixnumPair(lhs, rhs)
      case (.Fixnum(let lhs), .Bignum(let rhs)):
        self = BignumPair(BigInt(lhs), rhs)
      case (.Fixnum(let lhs), .Rat(let rhs)):
        self = RationalPair(Rational(lhs), rhs)
      case (.Fixnum(let lhs), .Bigrat(let rhs)):
        self = BigRationalPair(Rational(BigInt(lhs)), rhs.value)
      case (.Fixnum(let lhs), .Flonum(let rhs)):
        self = FlonumPair(Double(lhs), rhs)
      case (.Fixnum(let lhs), .Complexnum(let rhs)):
        self = ComplexPair(Complex(Double(lhs)), rhs)
      case (.Bignum(let lhs), .Fixnum(let rhs)):
        self = BignumPair(lhs, BigInt(rhs))
      case (.Bignum(let lhs), .Bignum(let rhs)):
        self = BignumPair(lhs, rhs)
      case (.Bignum(let lhs), .Rat(let rhs)):
        self = BigRationalPair(Rational(lhs),
                               Rational(BigInt(rhs.numerator), BigInt(rhs.denominator)))
      case (.Bignum(let lhs), .Bigrat(let rhs)):
        self = BigRationalPair(Rational(lhs), rhs.value)
      case (.Bignum(let lhs), .Flonum(let rhs)):
        self = FlonumPair(lhs.doubleValue, rhs)
      case (.Bignum(let lhs), .Complexnum(let rhs)):
        self = ComplexPair(Complex(lhs.doubleValue), rhs)
      case (.Rat(let lhs), .Fixnum(let rhs)):
        self = RationalPair(lhs, Rational(rhs))
      case (.Rat(let lhs), .Bignum(let rhs)):
        self = BigRationalPair(Rational(BigInt(lhs.numerator), BigInt(lhs.denominator)),
                               Rational(rhs))
      case (.Rat(let lhs), .Rat(let rhs)):
        self = RationalPair(lhs, rhs)
      case (.Rat(let lhs), .Bigrat(let rhs)):
        self = BigRationalPair(Rational(BigInt(lhs.numerator), BigInt(lhs.denominator)), rhs.value)
      case (.Rat(let lhs), .Flonum(let rhs)):
        self = FlonumPair(Double(lhs.numerator) / Double(lhs.numerator), rhs)
      case (.Rat(let lhs), .Complexnum(let rhs)):
        self = ComplexPair(Complex(Double(lhs.numerator) / Double(lhs.numerator)), rhs)
      case (.Bigrat(let lhs), .Fixnum(let rhs)):
        self = BigRationalPair(lhs.value, Rational(BigInt(rhs)))
      case (.Bigrat(let lhs), .Bignum(let rhs)):
        self = BigRationalPair(lhs.value, Rational(rhs))
      case (.Bigrat(let lhs), .Rat(let rhs)):
        self = BigRationalPair(lhs.value, Rational(BigInt(rhs.numerator), BigInt(rhs.denominator)))
      case (.Bigrat(let lhs), .Bigrat(let rhs)):
        self = BigRationalPair(lhs.value, rhs.value)
      case (.Bigrat(let lhs), .Flonum(let rhs)):
        self = FlonumPair(lhs.value.numerator.doubleValue / lhs.value.denominator.doubleValue, rhs)
      case (.Bigrat(let lhs), .Complexnum(let rhs)):
        self = ComplexPair(
          Complex(lhs.value.numerator.doubleValue / lhs.value.denominator.doubleValue), rhs)
      case (.Flonum(let lhs), .Fixnum(let rhs)):
        self = FlonumPair(lhs, Double(rhs))
      case (.Flonum(let lhs), .Bignum(let rhs)):
        self = FlonumPair(lhs, rhs.doubleValue)
      case (.Flonum(let lhs), .Rat(let rhs)):
        self = FlonumPair(lhs, Double(rhs.numerator) / Double(rhs.denominator))
      case (.Flonum(let lhs), .Bigrat(let rhs)):
        self = FlonumPair(lhs, rhs.value.numerator.doubleValue / rhs.value.denominator.doubleValue)
      case (.Flonum(let lhs), .Flonum(let rhs)):
        self = FlonumPair(lhs, rhs)
      case (.Flonum(let lhs), .Complexnum(let rhs)):
        self = ComplexPair(Complex(lhs), rhs)
      case (.Complexnum(let lhs), .Fixnum(let rhs)):
        self = ComplexPair(lhs, Complex(Double(rhs)))
      case (.Complexnum(let lhs), .Bignum(let rhs)):
        self = ComplexPair(lhs, Complex(rhs.doubleValue))
      case (.Complexnum(let lhs), .Rat(let rhs)):
        self = ComplexPair(lhs, Complex(Double(rhs.numerator) / Double(rhs.denominator)))
      case (.Complexnum(let lhs), .Bigrat(let rhs)):
        self = ComplexPair(lhs,
          Complex(rhs.value.numerator.doubleValue / rhs.value.denominator.doubleValue))
      case (.Complexnum(let lhs), .Flonum(let rhs)):
        self = ComplexPair(lhs, Complex(rhs))
      case (.Complexnum(let lhs), .Complexnum(let rhs)):
        self = ComplexPair(lhs, rhs)
      default:
        return nil
    }
  }
}

func compareNumber(lhs: Expr, with rhs: Expr) throws -> Int {
  guard let res = compare(lhs, with: rhs) else {
    try lhs.assertTypeOf(.RealType)
    try rhs.assertTypeOf(.RealType)
    preconditionFailure()
  }
  return res
}

func compare(lhs: Expr, with rhs: Expr) -> Int? {
  guard let pair = NumberPair(lhs, and: rhs) else {
    return nil
  }
  switch pair {
    case .FixnumPair(let lnum, let rnum):
      return lnum < rnum ? -1 : (lnum > rnum ? 1 : 0)
    case .BignumPair(let lnum, let rnum):
      return lnum < rnum ? -1 : (lnum > rnum ? 1 : 0)
    case .RationalPair(let lnum, let rnum):
      return lnum < rnum ? -1 : (lnum > rnum ? 1 : 0)
    case .BigRationalPair(let lnum, let rnum):
      return lnum < rnum ? -1 : (lnum > rnum ? 1 : 0)
    case .FlonumPair(let lnum, let rnum):
      return lnum < rnum ? -1 : (lnum > rnum ? 1 : 0)
    default:
      return nil
  }
}
