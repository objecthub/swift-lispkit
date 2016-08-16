//
//  MathLibrary.swift
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

import NumberKit


public final class MathLibrary: Library {
  
  public override func export() {
    define(Procedure("number?", isNumber))
    define(Procedure("real?", isReal))
    define(Procedure("integer?", isInteger))
    define(Procedure("rational?", isRational))
    define(Procedure("complex?", isComplex))
    define(Procedure("exact?", isExact))
    define(Procedure("inexact?", isInexact))
    define(Procedure("positive?", isPositive))
    define(Procedure("negative?", isNegative))
    define(Procedure("zero?", isZero))
    define(Procedure("even?", isEven))
    define(Procedure("odd?", isOdd))
    define(Procedure("exact->inexact", exactToInexact))
    define(Procedure("inexact->exact", inexactToExact))
    define(Procedure("rationalize", rationalize))
    define(Procedure("floor", floor))
    define(Procedure("ceiling", ceiling))
    define(Procedure("truncate", truncate))
    define(Procedure("round", round))
    define(Procedure("+", plus))
    define(Procedure("-", minus))
    define(Procedure("*", mult))
    define(Procedure("/", div))
    define(Procedure("=", equals))
    define(Procedure("<", lessThan))
    define(Procedure(">", biggerThan))
    define(Procedure("<=", lessThanEquals))
    define(Procedure(">=", biggerThanEquals))
    define(Procedure("max", max))
    define(Procedure("min", min))
    define(Procedure("abs", absolute))
    define(Procedure("sqrt", sqrt))
    define(Procedure("expt", expt))
    define(Procedure("exp", exp))
    define(Procedure("log", log))
    define(Procedure("sin", sin))
    define(Procedure("cos", cos))
    define(Procedure("tan", tan))
    define(Procedure("asin", asin))
    define(Procedure("acos", acos))
    define(Procedure("atan", atan))
    define(Procedure("number->string", numberToString))
    define(Procedure("string->number", stringToNumber))
    define(Procedure("make-rectangular", makeRectangular))
    define(Procedure("make-polar", makePolar))
    define(Procedure("real-part", realPart))
    define(Procedure("imag-part", imagPart))
    define(Procedure("magnitude", magnitude))
    define(Procedure("angle", angle))
    define(Procedure("numerator", numerator))
    define(Procedure("denominator", denominator))
    define(Procedure("gcd", gcd))
    define(Procedure("lcm", lcm))
    define(Procedure("quotient", quotient))
    define(Procedure("remainder", remainder))
    define(Procedure("modulo", modulo))
    define(Procedure("fx+", fxPlus, compileFxPlus))
    define(Procedure("fx-", fxMinus, compileFxMinus))
    define(Procedure("fx*", fxMult, compileFxMult))
    define(Procedure("fx/", fxDiv, compileFxDiv))
  }
  
  
  //-------- MARK: - Classification primitives

  func isNumber(expr: Expr) -> Expr {
    return .Boolean(Type.NumberType.includes(expr.type))
  }

  func isReal(expr: Expr) -> Expr {
    return .Boolean(Type.RealType.includes(expr.type))
  }

  func isInteger(expr: Expr) throws -> Expr {
    return .Boolean(Type.IntegerType.includes(expr.type))
  }

  func isRational(expr: Expr) throws -> Expr {
    return .Boolean(Type.IntegerType.includes(expr.type) || Type.RationalType.includes(expr.type))
  }

  func isComplex(expr: Expr) -> Expr {
    return .Boolean(Type.NumberType.includes(expr.type))
  }

  func isExact(expr: Expr) throws -> Expr {
    switch expr {
      case .Fixnum(_),
           .Bignum(_),
           .Rat(_),
           .Bigrat(_):
        return .True
      case .Flonum(_),
           .Complexnum(_):
        return .False
      default:
        throw EvalError.TypeError(expr, [.NumberType])
    }
  }

  func isInexact(expr: Expr) throws -> Expr {
    switch expr {
      case .Fixnum(_),
           .Bignum(_),
           .Rat(_),
           .Bigrat(_):
        return .False
      case .Flonum(_),
           .Complexnum(_):
        return .True
      default:
        throw EvalError.TypeError(expr, [.NumberType])
    }
  }

  func isPositive(expr: Expr) throws -> Expr {
    switch expr {
      case .Fixnum(let num):
        return .Boolean(num >= 0)
      case .Bignum(let num):
        return .Boolean(!num.isNegative)
      case .Rat(let num):
        return .Boolean(!num.value.isNegative)
      case .Bigrat(let num):
        return .Boolean(!num.value.isNegative)
      case .Flonum(let num):
        return .Boolean(!num.isSignMinus)
      case .Complexnum(let num):
        return num.value.isReal ? .Boolean(num.value.re >= 0) : .False
      default:
        throw EvalError.TypeError(expr, [.NumberType])
    }
  }

  func isNegative(expr: Expr) throws -> Expr {
    switch expr {
      case .Fixnum(let num):
        return .Boolean(num < 0)
      case .Bignum(let num):
        return .Boolean(num.isNegative)
      case .Rat(let num):
        return .Boolean(num.value.isNegative)
      case .Bigrat(let num):
        return .Boolean(num.value.isNegative)
      case .Flonum(let num):
        return .Boolean(num.isSignMinus)
      case .Complexnum(let num):
        return num.value.isReal ? .Boolean(num.value.re < 0) : .False
      default:
        throw EvalError.TypeError(expr, [.NumberType])
    }
  }

  func isZero(expr: Expr) throws -> Expr {
    switch expr {
      case .Fixnum(let num):
        return .Boolean(num == 0)
      case .Bignum(let num):
        return .Boolean(num.isZero)
      case .Rat(let num):
        return .Boolean(num.value.isZero)
      case .Bigrat(let num):
        return .Boolean(num.value.isZero)
      case .Flonum(let num):
        return .Boolean(num.isZero)
      case .Complexnum(let num):
        return num.value.isReal ? .Boolean(num.value.re >= 0) : .False
      default:
        throw EvalError.TypeError(expr, [.NumberType])
    }
  }

  func isEven(expr: Expr) throws -> Expr {
    switch expr.normalized {
      case .Fixnum(let num):
        return .Boolean(num % 2 == 0)
      case .Bignum(let num):
        return .Boolean(num % 2 == 0)
      default:
        throw EvalError.TypeError(expr, [.IntegerType])
    }
  }

  func isOdd(expr: Expr) throws -> Expr {
    switch expr.normalized {
      case .Fixnum(let num):
        return .Boolean(num % 2 != 0)
      case .Bignum(let num):
        return .Boolean(num % 2 != 0)
      default:
        throw EvalError.TypeError(expr, [.IntegerType])
    }
  }


  //-------- MARK: - Conversion primitives

  func exactToInexact(expr: Expr) throws -> Expr {
    switch expr {
      case .Fixnum(let num):
        return .Number(Double(num))
      case .Bignum(let num):
        return .Number(num.doubleValue)
      case .Rat(let num):
        return .Number(Double(num.value.numerator) / Double(num.value.denominator))
      case .Bigrat(let num):
        return .Number(num.value.numerator.doubleValue / num.value.denominator.doubleValue)
      case .Flonum(_),
           .Complexnum(_):
        return expr
      default:
        throw EvalError.TypeError(expr, [.NumberType])
    }
  }

  func inexactToExact(expr: Expr) throws -> Expr {
    switch expr {
      case .Fixnum(_),
           .Bignum(_),
           .Rat(_),
           .Bigrat(_):
        return expr
      case .Flonum(let num):
        return .Number(MathLibrary.approximate(num))
      default:
        throw EvalError.TypeError(expr, [.RealType])
    }
  }

  static func approximate(x: Double, tolerance: Double = 1.0e-9) -> Rational<Int64> {
    let mx = x * tolerance
    var y = x
    var (n1, d1) = (Int64(1), Int64(0))
    var (n2, d2) = (Int64(0), Int64(1))
    repeat {
      let fy = Int64(Foundation.floor(y))
      (n1, n2) = (fy * n1 + n2, n1)
      (d1, d2) = (fy * d1 + d2, d1)
      y = 1.0 / (y - Foundation.floor(y))
    } while abs(x - Double(n1) / Double(d1)) > mx
    return Rational(n1, d1)
  }


  //-------- MARK: - Rounding primitives

  func floor(expr: Expr) throws -> Expr {
    switch expr {
      case .Fixnum(_), .Bignum(_):
        return expr
      case .Rat(let num):
        return .Number(Int64(Foundation.floor(Double(num.value.numerator) /
                       Double(num.value.denominator))))
      case .Bigrat(let num):
        return .Number(Int64(Foundation.floor(num.value.numerator.doubleValue /
                       num.value.denominator.doubleValue)))
      case .Flonum(let num):
        return .Number(Foundation.floor(num))
      default:
        throw EvalError.TypeError(expr, [.RealType])
    }
  }

  func ceiling(expr: Expr) throws -> Expr {
    switch expr {
      case .Fixnum(_), .Bignum(_):
        return expr
      case .Rat(let num):
        return .Number(Int64(ceil(Double(num.value.numerator) / Double(num.value.denominator))))
      case .Bigrat(let num):
        return .Number(Int64(ceil(num.value.numerator.doubleValue /
                       num.value.denominator.doubleValue)))
      case .Flonum(let num):
        return .Number(ceil(num))
      default:
        throw EvalError.TypeError(expr, [.RealType])
    }
  }

  func truncate(expr: Expr) throws -> Expr {
    switch expr {
      case .Fixnum(_), .Bignum(_):
        return expr
      case .Rat(let num):
        return .Number(Int64(trunc(Double(num.value.numerator) / Double(num.value.denominator))))
      case .Bigrat(let num):
        return .Number(Int64(trunc(num.value.numerator.doubleValue /
                       num.value.denominator.doubleValue)))
      case .Flonum(let num):
        return .Number(trunc(num))
      default:
        throw EvalError.TypeError(expr, [.RealType])
    }
  }

  func round(expr: Expr) throws -> Expr {
    switch expr {
      case .Fixnum(_), .Bignum(_):
        return expr
      case .Rat(let num):
        return .Number(Int64(Foundation.round(Double(num.value.numerator) /
                       Double(num.value.denominator))))
      case .Bigrat(let num):
        return .Number(Int64(Foundation.round(num.value.numerator.doubleValue /
                       num.value.denominator.doubleValue)))
      case .Flonum(let num):
        return .Number(Foundation.round(num))
      default:
        throw EvalError.TypeError(expr, [.RealType])
    }
  }


  //-------- MARK: - Arithmetic primitives

  func plus(exprs: Arguments) throws -> Expr {
    var acc = Expr.Fixnum(0)
    for expr in exprs {
      switch try NumberPair(acc, expr) {
        case .FixnumPair(let lhs, let rhs):
          let (res, overflow) = Int64.addWithOverflow(lhs, rhs)
          acc = overflow ? .Number(BigInt(lhs) + BigInt(rhs)) : .Number(res)
        case .BignumPair(let lhs, let rhs):
          acc = .Number(lhs + rhs)
        case .RationalPair(let lhs, let rhs):
          let (res, overflow) = Rational.addWithOverflow(lhs, rhs)
          acc = overflow ? .Number(Rational(BigInt(lhs.numerator), BigInt(lhs.denominator)) +
                                   Rational(BigInt(rhs.numerator), BigInt(rhs.denominator)))
                         : .Number(res)
        case .BigRationalPair(let lhs, let rhs):
          acc = .Number(lhs + rhs)
        case .FlonumPair(let lhs, let rhs):
          acc = .Number(lhs + rhs)
        case .ComplexPair(let lhs, let rhs):
          acc = .Number(lhs + rhs)
      }
    }
    return acc
  }

  func minus(first: Expr, _ exprs: Arguments) throws -> Expr {
    var acc = first.normalized
    if exprs.isEmpty {
      switch acc {
        case .Fixnum(let res):
          return .Number(-res)
        case .Bignum(let res):
          return .Number(res.negate)
        case .Rat(let res):
          return .Number(res.value.negate)
        case .Bigrat(let res):
          return .Number(res.value.negate)
        case .Flonum(let res):
          return .Number(-res)
        case .Complexnum(let res):
          return .Number(res.value.negate)
        default:
          throw EvalError.TypeError(first, [.NumberType])
      }
    }
    for expr in exprs {
      switch try NumberPair(acc, expr) {
        case .FixnumPair(let lhs, let rhs):
          let (res, overflow) = Int64.subtractWithOverflow(lhs, rhs)
          acc = overflow ? .Number(BigInt(lhs) - BigInt(rhs)) : .Number(res)
        case .BignumPair(let lhs, let rhs):
          acc = .Number(lhs - rhs)
        case .RationalPair(let lhs, let rhs):
          let (res, overflow) = Rational.subtractWithOverflow(lhs, rhs)
          acc = overflow ? .Number(Rational(BigInt(lhs.numerator), BigInt(lhs.denominator)) -
                                   Rational(BigInt(rhs.numerator), BigInt(rhs.denominator)))
                         : .Number(res)
        case .BigRationalPair(let lhs, let rhs):
          acc = .Number(lhs - rhs)
        case .FlonumPair(let lhs, let rhs):
          acc = .Number(lhs - rhs)
        case .ComplexPair(let lhs, let rhs):
          acc = .Number(lhs - rhs)
      }
    }
    return acc
  }

  func mult(exprs: Arguments) throws -> Expr {
    var acc = Expr.Fixnum(1)
    for expr in exprs {
      switch try NumberPair(acc, expr) {
        case .FixnumPair(let lhs, let rhs):
          let (res, overflow) = Int64.multiplyWithOverflow(lhs, rhs)
          acc = overflow ? .Number(BigInt(lhs) * BigInt(rhs)) : .Number(res)
        case .BignumPair(let lhs, let rhs):
          acc = .Number(lhs * rhs)
        case .RationalPair(let lhs, let rhs):
          let (res, overflow) = Rational.multiplyWithOverflow(lhs, rhs)
          acc = overflow ? .Number(Rational(BigInt(lhs.numerator), BigInt(lhs.denominator)) *
                                   Rational(BigInt(rhs.numerator), BigInt(rhs.denominator)))
                         : .Number(res)
        case .BigRationalPair(let lhs, let rhs):
          acc = .Number(lhs * rhs)
        case .FlonumPair(let lhs, let rhs):
          acc = .Number(lhs * rhs)
        case .ComplexPair(let lhs, let rhs):
          acc = .Number(lhs * rhs)
      }
    }
    return acc
  }

  func div(first: Expr, _ exprs: Arguments) throws -> Expr {
    var acc = first.normalized
    if exprs.isEmpty {
      switch acc {
        case .Fixnum(let res):
          guard res != 0 else {
            throw EvalError.DivisionByZero
          }
          return .Number(Rational(1, res))
        case .Bignum(let res):
          guard !res.isZero else {
            throw EvalError.DivisionByZero
          }
          return .Number(Rational(BigInt(1), res))
        case .Rat(let res):
          guard !res.value.isZero else {
            throw EvalError.DivisionByZero
          }
          return .Number(Rational(res.value.denominator, res.value.numerator))
        case .Bigrat(let res):
          guard !res.value.isZero else {
            throw EvalError.DivisionByZero
          }
          return .Number(Rational(res.value.denominator, res.value.numerator))
        case .Flonum(let res):
          return .Number(1.0 / res)
        case .Complexnum(let res):
          return .Number(1.0 / res.value)
        default:
          throw EvalError.TypeError(first, [.NumberType])
      }
    }
    for expr in exprs {
      switch try NumberPair(acc, expr) {
        case .FixnumPair(let lhs, let rhs):
          guard rhs != 0 else {
            throw EvalError.DivisionByZero
          }
          let (res, overflow) = Rational.rationalWithOverflow(lhs, rhs)
          acc = overflow ? .Number(Rational(BigInt(lhs), BigInt(rhs))) : .Number(res)
        case .BignumPair(let lhs, let rhs):
          acc = .Number(Rational(lhs, rhs))
        case .RationalPair(let lhs, let rhs):
          let (res, overflow) = Rational.divideWithOverflow(lhs, rhs)
          acc = overflow ? .Number(Rational(BigInt(lhs.numerator), BigInt(lhs.denominator)) /
                                   Rational(BigInt(rhs.numerator), BigInt(rhs.denominator)))
                         : .Number(res)
        case .BigRationalPair(let lhs, let rhs):
          acc = .Number(lhs / rhs)
        case .FlonumPair(let lhs, let rhs):
          acc = .Number(lhs / rhs)
        case .ComplexPair(let lhs, let rhs):
          acc = .Number(lhs / rhs)
      }
    }
    return acc
  }
  
  
  //-------- MARK: - Comparison primitives
  
  func equals(first: Expr, _ exprs: Arguments) throws -> Expr {
    try first.assertTypeOf(.NumberType)
    var last = first
    for expr in exprs {
      guard try compareNumber(last, with: expr) == 0 else {
        return Expr.False
      }
      last = expr
    }
    return Expr.True
  }

  func lessThan(first: Expr, _ exprs: Arguments) throws -> Expr {
    try first.assertTypeOf(.NumberType)
    var last = first
    for expr in exprs {
      guard try compareNumber(last, with: expr) < 0 else {
        return Expr.False
      }
      last = expr
    }
    return Expr.True
  }

  func lessThanEquals(first: Expr, _ exprs: Arguments) throws -> Expr {
    try first.assertTypeOf(.NumberType)
    var last = first
    for expr in exprs {
      guard try compareNumber(last, with: expr) <= 0 else {
        return Expr.False
      }
      last = expr
    }
    return Expr.True
  }

  func biggerThan(first: Expr, _ exprs: Arguments) throws -> Expr {
    try first.assertTypeOf(.NumberType)
    var last = first
    for expr in exprs {
      guard try compareNumber(last, with: expr) > 0 else {
        return Expr.False
      }
      last = expr
    }
    return Expr.True
  }

  func biggerThanEquals(first: Expr, _ exprs: Arguments) throws -> Expr {
    try first.assertTypeOf(.NumberType)
    var last = first
    for expr in exprs {
      guard try compareNumber(last, with: expr) >= 0 else {
        return Expr.False
      }
      last = expr
    }
    return Expr.True
  }

  func max(first: Expr, _ exprs: Arguments) throws -> Expr {
    try first.assertTypeOf(.NumberType)
    var res = first
    for expr in exprs {
      if try compareNumber(res, with: expr) < 0 {
        res = expr
      }
    }
    return res
  }

  func min(first: Expr, _ exprs: Arguments) throws -> Expr {
    try first.assertTypeOf(.NumberType)
    var res = first
    for expr in exprs {
      if try compareNumber(res, with: expr) > 0 {
        res = expr
      }
    }
    return res
  }


  //-------- MARK: - Numeric functions

  func absolute(expr: Expr) throws -> Expr {
    switch expr {
      case .Fixnum(let num):
        return .Number(num < 0 ? -num : num)
      case .Bignum(let num):
        return .Number(num.isNegative ? -num : num)
      case .Rat(let num):
        return .Number(num.value.isNegative ? -num.value : num.value)
      case .Bigrat(let num):
        return .Number(num.value.isNegative ? -num.value : num.value)
      case .Flonum(let num):
        return .Flonum(num.isSignMinus ? -num : num)
      default:
        throw EvalError.TypeError(expr, [.RealType])
    }
  }

  func sqrt(expr: Expr) throws -> Expr {
    switch expr {
      case .Fixnum(_), .Bignum(_), .Rat(_), .Bigrat(_), .Flonum(_):
        let dbl = try expr.asFloat(coerce: true)
        let res = Foundation.sqrt(dbl)
        return res.isNaN ? .Number(Complex(dbl).sqrt) : .Number(res)
      case .Complexnum(let num):
        return .Number(num.value.sqrt)
      default:
        throw EvalError.TypeError(expr, [.NumberType])
    }
  }
  
  func expt(expr: Expr, _ exp: Expr) throws -> Expr {
    switch try NumberPair(expr, exp) {
      case .FixnumPair(let x, let y):
        return .Number(BigInt(x) ** BigInt(y))
      case .BignumPair(let x, let y):
        return .Number(x ** y)
      case .RationalPair(let x, let y):
        return .Number(Foundation.exp(y.doubleValue * Foundation.log(x.doubleValue)))
      case .BigRationalPair(let x, let y):
        return .Number(Foundation.exp(y.doubleValue * Foundation.log(x.doubleValue)))
      case .FlonumPair(let x, let y):
        return .Number(Foundation.exp(y * Foundation.log(x)))
      case .ComplexPair(let x, let y):
        return .Number((y * x.log).exp)
    }
  }

  func exp(expr: Expr) throws -> Expr {
    switch expr {
      case .Fixnum(_), .Bignum(_), .Rat(_), .Bigrat(_), .Flonum(_):
        let dbl = try expr.asFloat(coerce: true)
        let res = Foundation.exp(dbl)
        return res.isNaN ? .Number(Complex(dbl).exp) : .Number(res)
      case .Complexnum(let num):
        return .Number(num.value.exp)
      default:
        throw EvalError.TypeError(expr, [.NumberType])
    }
  }

  func log(expr: Expr) throws -> Expr {
    switch expr {
      case .Fixnum(_), .Bignum(_), .Rat(_), .Bigrat(_), .Flonum(_):
        let dbl = try expr.asFloat(coerce: true)
        let res = Foundation.log(dbl)
        return res.isNaN ? .Number(Complex(dbl).log) : .Number(res)
      case .Complexnum(let num):
        return .Number(num.value.log)
      default:
        throw EvalError.TypeError(expr, [.NumberType])
    }
  }

  func sin(expr: Expr) throws -> Expr {
    return .Number(Foundation.sin(try expr.asFloat(coerce: true)))
  }

  func cos(expr: Expr) throws -> Expr {
    return .Number(Foundation.cos(try expr.asFloat(coerce: true)))
  }

  func tan(expr: Expr) throws -> Expr {
    return .Number(Foundation.tan(try expr.asFloat(coerce: true)))
  }

  func asin(expr: Expr) throws -> Expr {
    return .Number(Foundation.asin(try expr.asFloat(coerce: true)))
  }

  func acos(expr: Expr) throws -> Expr {
    return .Number(Foundation.acos(try expr.asFloat(coerce: true)))
  }

  func atan(fst: Expr, _ snd: Expr?) throws -> Expr {
    let y = try fst.asFloat(coerce: true)
    if let snd = snd {
      return .Number(Foundation.atan2(y, try snd.asFloat(coerce: true)))
    } else {
      return .Number(Foundation.atan(y))
    }
  }
  
  func numberToString(expr: Expr, _ rad: Expr?) throws -> Expr {
    var radix = 10
    if let base = try rad?.asInt() {
      if base == 2 || base == 8 || base == 10 || base == 16 {
        radix = base
      } else {
        throw EvalError.IllegalRadix(rad!)
      }
    }
    switch expr {
      case .Fixnum(let num):
        return .Str(NSMutableString(string: String(num, radix: radix)))
      case .Bignum(let num):
        return .Str(NSMutableString(string: num.toString(base: BigInt.base(radix))))
      case .Rat(let num):
        return .Str(NSMutableString(string: String(num.value.numerator, radix: radix) + "/" +
                                            String(num.value.denominator, radix: radix)))
      case .Bigrat(let num):
        return .Str(
          NSMutableString(string: num.value.numerator.toString(base: BigInt.base(radix)) + "/" +
                          num.value.denominator.toString(base: BigInt.base(radix))))
      case .Flonum(let num):
        if radix != 10 {
          throw EvalError.IllegalRadix(rad!)
        }
        return .Str(NSMutableString(string: String(num)))
      case .Complexnum(let num):
        if radix != 10 {
          throw EvalError.IllegalRadix(rad!)
        }
        return .Str(NSMutableString(string: num.value.description))
      default:
        throw EvalError.TypeError(expr, [.NumberType])
    }
  }
  
  func stringToNumber(expr: Expr, _ rad: Expr?) throws -> Expr {
    var radix = 10
    if let base = try rad?.asInt() {
      if base == 2 || base == 8 || base == 10 || base == 16 {
        radix = base
      } else {
        throw EvalError.IllegalRadix(rad!)
      }
    }
    let scanner = Scanner(string: try expr.asStr(), prescan: false)
    scanner.skipSpace()
    guard scanner.ch != EOF_CH else {
      throw EvalError.TypeError(expr, [.NumberType])
    }
    scanner.scanSignedNumber(radix)
    let token = scanner.token
    scanner.skipSpace()
    guard scanner.ch == EOF_CH else {
      throw EvalError.TypeError(expr, [.NumberType])
    }
    switch token.kind {
      case .INT:
        return .Fixnum(token.intVal)
      case .BIGINT:
        return .Bignum(token.bigIntVal)
      case .RAT:
        return .Rat(ImmutableBox(token.ratVal))
      case .BIGRAT:
        return .Bigrat(ImmutableBox(token.bigRatVal))
      case .FLOAT:
        return .Flonum(token.floatVal)
      case .COMPLEX:
        return .Complexnum(ImmutableBox(token.complexVal))
      default:
        throw EvalError.TypeError(expr, [.NumberType])
    }
  }

  private static func findBestRat(t: Double, _ l: Int64) -> (Double, Int64, Int64) {
    precondition(l >= 1)
    if t <= 0.0 {
      return (0.0, 0, 1)
    }
    var (n1, n2) = (Int64(1), Int64(0))
    var (d1, d2) = (Int64(0), Int64(1))
    var x = t
    var ai = Int64(Foundation.floor(x))
    while d1 * ai + d2 <= l {
      (n1, n2) = (n1 * ai + n2, n1)
      (d1, d2) = (d1 * ai + d2, d1)
      if x == Foundation.floor(x) {
        break
      }
      x = Double(1.0) / (x - Double(ai))
      ai = Int64(Foundation.floor(x))
    }
    let err1 = t - Double(n1) / Double(d1)
    ai = Int64(Foundation.floor(Double(l - d2) / Double(d1)))
    (n2, d2) = (n1 * ai + n2, d1 * ai + d2)
    let err2 = t - Double(n2) / Double(d2)
    return abs(err1) <= abs(err2) ? (err1, n1, d1) : (err2, n2, d2)
  }
  
  func rationalize(x: Expr, delta: Expr) throws -> Expr {
    var l_curr: Int64 = 1
    let t = try x.asFloat(coerce: true)
    let err = try delta.asFloat(coerce: true)
    var (actual, n, d) = MathLibrary.findBestRat(t, l_curr)
    while abs(actual) > err && l_curr < (Int64.max / 1000) {
      l_curr *= 10
      (actual, n, d) = MathLibrary.findBestRat(t, l_curr)
    }
    return .Rat(ImmutableBox(Rational(n, d)))
  }
  
  func makeRectangular(re: Expr, _ imag: Expr) throws -> Expr {
    return .Complexnum(ImmutableBox(Complex(try re.asFloat(coerce: true),
                                            try imag.asFloat(coerce: true))))
  }
  
  func makePolar(abs: Expr, _ arg: Expr) throws -> Expr {
    return .Complexnum(ImmutableBox(Complex(abs: try abs.asFloat(coerce: true),
                                            arg: try arg.asFloat(coerce: true))))
  }
  
  func realPart(expr: Expr) throws -> Expr {
    return .Flonum(try expr.asComplex(coerce: true).re)
  }
  
  func imagPart(expr: Expr) throws -> Expr {
    return .Flonum(try expr.asComplex(coerce: true).im)
  }
  
  func magnitude(expr: Expr) throws -> Expr {
    return .Flonum(try expr.asComplex(coerce: true).abs)
  }
  
  func angle(expr: Expr) throws -> Expr {
    return .Flonum(try expr.asComplex(coerce: true).arg)
  }
  
  func numerator(expr: Expr) throws -> Expr {
    switch expr {
      case .Fixnum(_), .Bignum(_):
        return expr
      case .Rat(let num):
        return .Fixnum(num.value.numerator)
      case .Bigrat(let num):
        return .Number(num.value.numerator)
      case .Flonum(let num):
        return .Flonum(Double(MathLibrary.approximate(num).numerator))
      default:
        throw EvalError.TypeError(expr, [.ExactNumberType])
    }
  }
  
  func denominator(expr: Expr) throws -> Expr {
    switch expr {
      case .Fixnum(_), .Bignum(_):
        return .Fixnum(1)
      case .Rat(let num):
        return .Fixnum(num.value.denominator)
      case .Bigrat(let num):
        return .Number(num.value.denominator)
      case .Flonum(let num):
        return .Flonum(Double(MathLibrary.approximate(num).denominator))
      default:
        throw EvalError.TypeError(expr, [.RealType])
    }
  }
  
  func gcd(exprs: Arguments) throws -> Expr {
    var acc = Expr.Fixnum(0)
    for expr in exprs {
      var e = expr
      if case .Flonum(let num) = expr {
        e = .Number(MathLibrary.approximate(num))
      }
      switch try NumberPair(acc, e) {
        case .FixnumPair(let lhs, let rhs):
          let (res, overflow) = Rational.gcdWithOverflow(lhs, rhs)
          acc = overflow ? .Number(Rational.gcd(BigInt(lhs), BigInt(rhs))) : .Number(res)
        case .BignumPair(let lhs, let rhs):
          acc = .Number(Rational.gcd(lhs, rhs))
        case .RationalPair(let lhs, let rhs):
          let (res, overflow) = Rational.gcdWithOverflow(lhs, rhs)
          acc = overflow ?
              .Number(Rational.gcd(Rational(BigInt(lhs.numerator), BigInt(lhs.denominator)),
                                   Rational(BigInt(rhs.numerator), BigInt(rhs.denominator))))
            : .Number(res)
        case .BigRationalPair(let lhs, let rhs):
          acc = .Number(Rational.gcd(lhs, rhs))
        default:
          throw EvalError.TypeError(expr, [.RealType])
      }
    }
    return acc
  }
  
  func lcm(exprs: Arguments) throws -> Expr {
    var acc = Expr.Fixnum(1)
    for expr in exprs {
      var e = expr
      if case .Flonum(let num) = expr {
        e = .Number(MathLibrary.approximate(num))
      }
      switch try NumberPair(acc, e) {
        case .FixnumPair(let lhs, let rhs):
          let (res, overflow) = Rational.lcmWithOverflow(lhs, rhs)
          acc = overflow ? .Number(Rational.lcm(BigInt(lhs), BigInt(rhs))) : .Number(res)
        case .BignumPair(let lhs, let rhs):
          acc = .Number(Rational.lcm(lhs, rhs))
        case .RationalPair(let lhs, let rhs):
          let (res, overflow) = Rational.lcmWithOverflow(lhs, rhs)
          acc = overflow ?
              .Number(Rational.lcm(Rational(BigInt(lhs.numerator), BigInt(lhs.denominator)),
                                   Rational(BigInt(rhs.numerator), BigInt(rhs.denominator))))
            : .Number(res)
        case .BigRationalPair(let lhs, let rhs):
          acc = .Number(Rational.lcm(lhs, rhs))
        default:
          throw EvalError.TypeError(expr, [.RealType])
      }
    }
    return acc
  }
  
  func quotient(x: Expr, _ y: Expr) throws -> Expr {
    switch try NumberPair(x, y) {
      case .FixnumPair(let lhs, let rhs):
        return .Number(lhs / rhs)
      case .BignumPair(let lhs, let rhs):
        return .Number(lhs / rhs)
      default:
        try x.assertTypeOf(.IntegerType)
        try y.assertTypeOf(.IntegerType)
        preconditionFailure()
    }
  }
  
  func remainder(x: Expr, _ y: Expr) throws -> Expr {
    switch try NumberPair(x, y) {
      case .FixnumPair(let lhs, let rhs):
        return .Number(lhs % rhs)
      case .BignumPair(let lhs, let rhs):
        return .Number(lhs % rhs)
      default:
        try x.assertTypeOf(.IntegerType)
        try y.assertTypeOf(.IntegerType)
        preconditionFailure()
    }
  }
  
  func modulo(x: Expr, _ y: Expr) throws -> Expr {
    switch try NumberPair(x, y) {
      case .FixnumPair(let lhs, let rhs):
        let res = lhs % rhs
        return .Number((res < 0) == (rhs < 0) ? res : res + rhs)
      case .BignumPair(let lhs, let rhs):
        let res = lhs % rhs
        return .Number(res.isNegative == rhs.isNegative ? res : res + rhs)
      default:
        try x.assertTypeOf(.IntegerType)
        try y.assertTypeOf(.IntegerType)
        preconditionFailure()
    }
  }
  
  func fxPlus(x: Expr, _ y: Expr) throws -> Expr {
    return .Fixnum(try x.asInteger() &+ y.asInteger())
  }
  
  func compileFxPlus(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .Pair(_, .Pair(let x, .Pair(let y, .Null))) = expr else {
      throw EvalError.ArgumentCountError(formals: 2, args: expr)
    }
    try compiler.compile(x, in: env, inTailPos: false)
    try compiler.compile(y, in: env, inTailPos: false)
    compiler.emit(.FxPlus)
    return false
  }
  
  func fxMinus(x: Expr, _ y: Expr) throws -> Expr {
    return .Fixnum(try x.asInteger() &- y.asInteger())
  }
  
  func compileFxMinus(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .Pair(_, .Pair(let x, .Pair(let y, .Null))) = expr else {
      throw EvalError.ArgumentCountError(formals: 2, args: expr)
    }
    try compiler.compile(x, in: env, inTailPos: false)
    try compiler.compile(y, in: env, inTailPos: false)
    compiler.emit(.FxMinus)
    return false
  }

  func fxMult(x: Expr, _ y: Expr) throws -> Expr {
    return .Fixnum(try x.asInteger() &* y.asInteger())
  }
  
  func compileFxMult(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .Pair(_, .Pair(let x, .Pair(let y, .Null))) = expr else {
      throw EvalError.ArgumentCountError(formals: 2, args: expr)
    }
    try compiler.compile(x, in: env, inTailPos: false)
    try compiler.compile(y, in: env, inTailPos: false)
    compiler.emit(.FxMult)
    return false
  }
  
  func fxDiv(x: Expr, _ y: Expr) throws -> Expr {
    return .Fixnum(try x.asInteger() / y.asInteger())
  }
  
  func compileFxDiv(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .Pair(_, .Pair(let x, .Pair(let y, .Null))) = expr else {
      throw EvalError.ArgumentCountError(formals: 2, args: expr)
    }
    try compiler.compile(x, in: env, inTailPos: false)
    try compiler.compile(y, in: env, inTailPos: false)
    compiler.emit(.FxDiv)
    return false
  }
}
