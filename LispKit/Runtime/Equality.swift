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

struct Equality<T: Hashable>: Hashable {
  let vec1: T
  let vec2: T
  
  init(_ vec1: T, _ vec2: T) {
    self.vec1 = vec1
    self.vec2 = vec2
  }
  
  var hashValue: Int {
    return vec1.hashValue &+ vec2.hashValue
  }
}

func ==<T: Hashable>(lhs: Equality<T>, rhs: Equality<T>) -> Bool {
  return lhs.vec1 == rhs.vec1 && lhs.vec2 == rhs.vec2 ||
         lhs.vec1 == rhs.vec2 && lhs.vec2 == rhs.vec1
}

func equalExpr(this: Expr, _ that: Expr) -> Bool {
  var vectorEqualities = Set<Equality<Vector>>()
  
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
        return str1.value == str2.value
      case (.Pair(let car1, let cdr1), .Pair(let car2, let cdr2)):
        return equals(car1, car2) && equals(cdr1, cdr2)
      case (.Vec(let vector1), .Vec(let vector2)):
        let equality = Equality(vector1, vector2)
        guard !vectorEqualities.contains(equality) else {
          return true
        }
        guard vector1.exprs.count == vector2.exprs.count else {
          return false
        }
        vectorEqualities.insert(Equality(vector1, vector2))
        for i in vector1.exprs.indices {
          guard equals(vector1.exprs[i], vector2.exprs[i]) else {
            return false
          }
        }
        return true
      case (.ByteVec(let bvector1), .ByteVec(let bvector2)):
        guard bvector1.value.count == bvector2.value.count else {
          return false
        }
        for i in bvector1.value.indices {
          guard bvector1.value[i] == bvector2.value[i] else {
            return false
          }
        }
        return true
      case (.Promise(let promise1), .Promise(let promise2)):
        return promise1 == promise2
      case (.Special(let e1), .Special(let e2)):
        return e1 == e2
      case (.Proc(let e1), .Proc(let e2)):
        return e1 == e2
      case (.Var(let v1), .Var(let v2)):
        return v1 === v2
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
    case (.Pair(let car1, let cdr1), .Pair(let car2, let cdr2)):
      return eqvExpr(car1, car2) && eqvExpr(cdr1, cdr2)
    case (.Vec(let vector1), .Vec(let vector2)):
      return vector1 === vector2
    case (.ByteVec(let bvector1), .ByteVec(let bvector2)):
      return bvector1 === bvector2
    case (.Promise(let promise1), .Promise(let promise2)):
      return promise1 === promise2
    case (.Special(let e1), .Special(let e2)):
      return e1 === e2
    case (.Proc(let e1), .Proc(let e2)):
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
    case (.Pair(let car1, let cdr1), .Pair(let car2, let cdr2)):
      return eqvExpr(car1, car2) && eqvExpr(cdr1, cdr2)
    case (.Vec(let vector1), .Vec(let vector2)):
      return vector1 === vector2
    case (.ByteVec(let bvector1), .ByteVec(let bvector2)):
      return bvector1 === bvector2
    case (.Promise(let promise1), .Promise(let promise2)):
      return promise1 === promise2
    case (.Special(let e1), .Special(let e2)):
      return e1 === e2
    case (.Proc(let e1), .Proc(let e2)):
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
        self = BigRationalPair(Rational(BigInt(lhs)), rhs)
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
        self = BigRationalPair(Rational(lhs), rhs)
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
        self = BigRationalPair(Rational(BigInt(lhs.numerator), BigInt(lhs.denominator)), rhs)
      case (.Rat(let lhs), .Flonum(let rhs)):
        self = FlonumPair(Double(lhs.numerator) / Double(lhs.numerator), rhs)
      case (.Rat(let lhs), .Complexnum(let rhs)):
        self = ComplexPair(Complex(Double(lhs.numerator) / Double(lhs.numerator)), rhs)
      case (.Bigrat(let lhs), .Fixnum(let rhs)):
        self = BigRationalPair(lhs, Rational(BigInt(rhs)))
      case (.Bigrat(let lhs), .Bignum(let rhs)):
        self = BigRationalPair(lhs, Rational(rhs))
      case (.Bigrat(let lhs), .Rat(let rhs)):
        self = BigRationalPair(lhs, Rational(BigInt(rhs.numerator), BigInt(rhs.denominator)))
      case (.Bigrat(let lhs), .Bigrat(let rhs)):
        self = BigRationalPair(lhs, rhs)
      case (.Bigrat(let lhs), .Flonum(let rhs)):
        self = FlonumPair(lhs.numerator.doubleValue / lhs.denominator.doubleValue, rhs)
      case (.Bigrat(let lhs), .Complexnum(let rhs)):
        self = ComplexPair(Complex(lhs.numerator.doubleValue / lhs.denominator.doubleValue), rhs)
      case (.Flonum(let lhs), .Fixnum(let rhs)):
        self = FlonumPair(lhs, Double(rhs))
      case (.Flonum(let lhs), .Bignum(let rhs)):
        self = FlonumPair(lhs, rhs.doubleValue)
      case (.Flonum(let lhs), .Rat(let rhs)):
        self = FlonumPair(lhs, Double(rhs.numerator) / Double(rhs.denominator))
      case (.Flonum(let lhs), .Bigrat(let rhs)):
        self = FlonumPair(lhs, rhs.numerator.doubleValue / rhs.denominator.doubleValue)
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
        self = ComplexPair(lhs, Complex(rhs.numerator.doubleValue / rhs.denominator.doubleValue))
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
