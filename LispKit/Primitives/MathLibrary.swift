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

///
/// Math library: based on R7RS spec.
///
public final class MathLibrary: NativeLibrary {
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "math"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "base"], "define", "lambda")
    self.`import`(from: ["lispkit", "control"], "letrec", "let", "cond", "if")
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("number?", isNumber))
    self.define(Procedure("real?", isReal))
    self.define(Procedure("integer?", isInteger))
    self.define(Procedure("rational?", isRational))
    self.define(Procedure("complex?", isComplex))
    self.define(Procedure("fixnum?", isFixnum))
    self.define(Procedure("ratnum?", isRatnum))
    self.define(Procedure("bignum?", isBignum))
    self.define(Procedure("flonum?", isFlonum))
    self.define(Procedure("cflonum?", isCflonum))
    self.define(Procedure("exact?", isExact))
    self.define(Procedure("inexact?", isInexact))
    self.define(Procedure("exact-integer?", isExactInteger))
    self.define(Procedure("finite?", isFinite))
    self.define(Procedure("infinite?", isInfinite))
    self.define(Procedure("nan?", isNaN))
    self.define(Procedure("positive?", isPositive))
    self.define(Procedure("negative?", isNegative))
    self.define(Procedure("zero?", isZero))
    self.define(Procedure("even?", isEven))
    self.define(Procedure("odd?", isOdd))
    self.define(Procedure("inexact", inexact))
    self.define(Procedure("exact", exact))
    self.define(Procedure("approximate", approximate))
    self.define("rationalize", via:
      "(define (rationalize x e)",
      "  (letrec ((simplest (lambda (x y return)",
      "                       (let ((fx (floor x)) (fy (floor y)))",
      "                         (cond ((>= fx x) (return fx 1))",
      "                               ((= fx fy) (simplest (/ (- y fy)) (/ (- x fx))",
      "                                            (lambda (n d) (return (+ d (* fx n)) n))))",
      "                               (else      (return (+ fx 1) 1))))))",
      "           (ax (abs x))",
      "           (ae (abs e)))",
      "  (simplest (- ax ae) (+ ax ae)",
      "    (if (negative? x) (lambda (num den) (/ (- num) den)) /))))")
    self.define(Procedure("floor", floor))
    self.define(Procedure("ceiling", ceiling))
    self.define(Procedure("truncate", truncate))
    self.define(Procedure("round", round))
    self.define(Procedure("+", plus))
    self.define(Procedure("-", minus))
    self.define(Procedure("*", mult))
    self.define(Procedure("/", div))
    self.define(Procedure("=", equals))
    self.define(Procedure("<", lessThan))
    self.define(Procedure(">", biggerThan))
    self.define(Procedure("<=", lessThanEquals))
    self.define(Procedure(">=", biggerThanEquals))
    self.define(Procedure("max", max))
    self.define(Procedure("min", min))
    self.define(Procedure("abs", absolute))
    self.define(Procedure("square", square))
    self.define(Procedure("sqrt", sqrt))
    self.define(Procedure("expt", expt))
    self.define(Procedure("exp", exp))
    self.define(Procedure("log", log))
    self.define(Procedure("sin", sin))
    self.define(Procedure("cos", cos))
    self.define(Procedure("tan", tan))
    self.define(Procedure("asin", asin))
    self.define(Procedure("acos", acos))
    self.define(Procedure("atan", atan))
    self.define(Procedure("number->string", numberToString))
    self.define(Procedure("string->number", stringToNumber))
    self.define(Procedure("make-rectangular", makeRectangular))
    self.define(Procedure("make-polar", makePolar))
    self.define(Procedure("real-part", realPart))
    self.define(Procedure("imag-part", imagPart))
    self.define(Procedure("magnitude", magnitude))
    self.define(Procedure("angle", angle))
    self.define(Procedure("numerator", numerator))
    self.define(Procedure("denominator", denominator))
    self.define(Procedure("gcd", gcd))
    self.define(Procedure("lcm", lcm))
    self.define(Procedure("truncate-quotient", truncateQuotient))
    self.define(Procedure("truncate-remainder", truncateRemainder))
    self.define(Procedure("floor-quotient", floorQuotient))
    self.define(Procedure("floor-remainder", floorRemainder))
    self.define(Procedure("quotient", truncateQuotient))
    self.define(Procedure("remainder", truncateRemainder))
    self.define(Procedure("modulo", floorRemainder))
    self.define(Procedure("fx+", fxPlus, compileFxPlus))
    self.define(Procedure("fx-", fxMinus, compileFxMinus))
    self.define(Procedure("fx*", fxMult, compileFxMult))
    self.define(Procedure("fx/", fxDiv, compileFxDiv))
    self.define(Procedure("fx=", fxEq, compileFxEq))
    self.define(Procedure("fx<", fxLt, compileFxLt))
    self.define(Procedure("fx>", fxGt, compileFxGt))
    self.define(Procedure("fx<=", fxLtEq, compileFxLtEq))
    self.define(Procedure("fx>=", fxGtEq, compileFxGtEq))
    self.define(Procedure("fx1+", fx1Plus, compileFx1Plus))
    self.define(Procedure("fx1-", fx1Minus, compileFx1Minus))
    self.define(Procedure("fxzero?", fxIsZero, compileFxIsZero))
    self.define(Procedure("fxpositive?", fxIsPositive))
    self.define(Procedure("fxnegative?", fxIsNegative))
    self.define(Procedure("fxremainder", fxRemainder))
    self.define(Procedure("fxmodulo", fxModulo))
    self.define(Procedure("fxabs", fxAbs))
    self.define(Procedure("fxand", fxAnd))
    self.define(Procedure("fxior", fxIor))
    self.define(Procedure("fxxor", fxXor))
    self.define(Procedure("fxnot", fxNot))
    self.define(Procedure("fxlshift", fxLshift))
    self.define(Procedure("fxrshift", fxRshift))
    self.define(Procedure("fxlrshift", fxLrshift))
    self.define(Procedure("fxmin", fxMin))
    self.define(Procedure("fxmax", fxMax))
    self.define("max-fixnum", via: "(define max-fixnum \(Int64.max))")
    self.define("min-fixnum", via: "(define min-fixnum \(Int64.min))")
    self.define(Procedure("fl+", flPlus, compileFlPlus))
    self.define(Procedure("fl-", flMinus, compileFlMinus))
    self.define(Procedure("fl*", flMult, compileFlMult))
    self.define(Procedure("fl/", flDiv, compileFlDiv))
    self.define(Procedure("flzero?", flIsZero))
    self.define(Procedure("flpositive?", flIsPositive))
    self.define(Procedure("flnegative?", flIsNegative))
    self.define(Procedure("fl=", flEq, compileFlEq))
    self.define(Procedure("fl<", flLt, compileFlLt))
    self.define(Procedure("fl>", flGt, compileFlGt))
    self.define(Procedure("fl<=", flLtEq, compileFlLtEq))
    self.define(Procedure("fl>=", flGtEq, compileFlGtEq))
    self.define(Procedure("flabs", flAbs))
    self.define(Procedure("flmin", flMin))
    self.define(Procedure("flmax", flMax))
    self.define("pi", via: "(define pi \(Double.pi))")
    self.define("e", via: "(define e \(M_E))")
  }
  
  
  //-------- MARK: - Classification primitives

  func isNumber(_ expr: Expr) -> Expr {
    return .makeBoolean(Type.numberType.includes(expr.type))
  }

  func isReal(_ expr: Expr) -> Expr {
    return .makeBoolean(Type.realType.includes(expr.type))
  }

  func isInteger(_ expr: Expr) throws -> Expr {
    return .makeBoolean(Type.integerType.includes(expr.type))
  }

  func isRational(_ expr: Expr) throws -> Expr {
    return .makeBoolean(Type.integerType.includes(expr.type) || Type.rationalType.includes(expr.type))
  }

  func isComplex(_ expr: Expr) -> Expr {
    return .makeBoolean(Type.numberType.includes(expr.type))
  }
  
  func isFixnum(_ expr: Expr) -> Expr {
    switch expr {
      case .fixnum(_):
        return .true
      default:
        return .false
    }
  }
  
  func isRatnum(_ expr: Expr) -> Expr {
    switch expr {
      case .rational(_),
           .bigrat(_):
        return .true
      default:
        return .false
    }
  }
  
  func isBignum(_ expr: Expr) -> Expr {
    switch expr {
      case .bignum(_):
        return .true
      default:
        return .false
    }
  }
  
  func isFlonum(_ expr: Expr) -> Expr {
    switch expr {
      case .flonum(_):
        return .true
      default:
        return .false
    }
  }
  
  func isCflonum(_ expr: Expr) -> Expr {
    switch expr {
      case .complex(_):
        return .true
      default:
        return .false
    }
  }
  
  func isExact(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_),
           .bignum(_),
           .rational(_),
           .bigrat(_):
        return .true
      default:
        return .false
    }
  }

  func isInexact(_ expr: Expr) throws -> Expr {
    switch expr {
      case .flonum(_),
           .complex(_):
        return .true
      default:
        return .false
    }
  }

  func isExactInteger(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_):
        return .true
      default:
        return .false
    }
  }
  
  func isFinite(_ expr: Expr) throws -> Expr {
    switch expr {
      case .flonum(let num):
        return .makeBoolean(num.isFinite)
      case .complex(let box):
        return .makeBoolean(!box.value.isInfinite)
      default:
        return .true
    }
  }

  func isInfinite(_ expr: Expr) throws -> Expr {
    switch expr {
      case .flonum(let num):
        return .makeBoolean(num.isInfinite)
      case .complex(let box):
        return .makeBoolean(box.value.isInfinite)
      default:
        return .false
    }
  }
  
  func isNaN(_ expr: Expr) throws -> Expr {
    switch expr {
      case .flonum(let num):
        return .makeBoolean(num.isNaN)
      case .complex(let box):
        return .makeBoolean(box.value.re.isNaN || box.value.im.isNaN)
      default:
        return .false
    }
  }
  
  func isPositive(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(let num):
        return .makeBoolean(num > 0)
      case .bignum(let num):
        return .makeBoolean(!num.isNegative)
      case .rational(let num):
        return .makeBoolean(!num.value.isNegative)
      case .bigrat(let num):
        return .makeBoolean(!num.value.isNegative)
      case .flonum(let num):
        return .makeBoolean(num > 0.0)
      case .complex(let num):
        return num.value.isReal ? .makeBoolean(num.value.re > 0) : .false
      default:
        throw EvalError.typeError(expr, [.numberType])
    }
  }

  func isNegative(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(let num):
        return .makeBoolean(num < 0)
      case .bignum(let num):
        return .makeBoolean(num.isNegative)
      case .rational(let num):
        return .makeBoolean(num.value.isNegative)
      case .bigrat(let num):
        return .makeBoolean(num.value.isNegative)
      case .flonum(let num):
        return .makeBoolean(num < 0.0)
      case .complex(let num):
        return num.value.isReal ? .makeBoolean(num.value.re < 0) : .false
      default:
        throw EvalError.typeError(expr, [.numberType])
    }
  }

  func isZero(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(let num):
        return .makeBoolean(num == 0)
      case .bignum(let num):
        return .makeBoolean(num.isZero)
      case .rational(let num):
        return .makeBoolean(num.value.isZero)
      case .bigrat(let num):
        return .makeBoolean(num.value.isZero)
      case .flonum(let num):
        return .makeBoolean(num.isZero)
      case .complex(let num):
        return num.value.isReal ? .makeBoolean(num.value.re >= 0) : .false
      default:
        throw EvalError.typeError(expr, [.numberType])
    }
  }

  func isEven(_ expr: Expr) throws -> Expr {
    switch expr.normalized {
      case .fixnum(let num):
        return .makeBoolean(num % 2 == 0)
      case .bignum(let num):
        return .makeBoolean(num % 2 == 0)
      default:
        throw EvalError.typeError(expr, [.integerType])
    }
  }

  func isOdd(_ expr: Expr) throws -> Expr {
    switch expr.normalized {
      case .fixnum(let num):
        return .makeBoolean(num % 2 != 0)
      case .bignum(let num):
        return .makeBoolean(num % 2 != 0)
      default:
        throw EvalError.typeError(expr, [.integerType])
    }
  }


  //-------- MARK: - Conversion primitives

  func inexact(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(let num):
        return .makeNumber(Double(num))
      case .bignum(let num):
        return .makeNumber(num.doubleValue)
      case .rational(let num):
        return .makeNumber(Double(num.value.numerator) / Double(num.value.denominator))
      case .bigrat(let num):
        return .makeNumber(num.value.numerator.doubleValue / num.value.denominator.doubleValue)
      case .flonum(_), .complex(_):
        return expr
      default:
        throw EvalError.typeError(expr, [.numberType])
    }
  }

  func exact(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_), .rational(_), .bigrat(_):
        return expr
      case .flonum(let num):
        return .makeNumber(MathLibrary.approximate(num))
      default:
        throw EvalError.typeError(expr, [.realType])
    }
  }

  static func approximate(_ x: Double, tolerance: Double = 1.0e-16) -> Rational<Int64> {
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

  func floor(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_):
        return expr
      case .rational(let num):
        return .makeNumber(Int64(Foundation.floor(Double(num.value.numerator) /
                       Double(num.value.denominator))))
      case .bigrat(let num):
        return .makeNumber(Int64(Foundation.floor(num.value.numerator.doubleValue /
                       num.value.denominator.doubleValue)))
      case .flonum(let num):
        return .makeNumber(Foundation.floor(num))
      default:
        throw EvalError.typeError(expr, [.realType])
    }
  }

  func ceiling(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_):
        return expr
      case .rational(let num):
        return .makeNumber(Int64(ceil(Double(num.value.numerator) / Double(num.value.denominator))))
      case .bigrat(let num):
        return .makeNumber(Int64(ceil(num.value.numerator.doubleValue /
                       num.value.denominator.doubleValue)))
      case .flonum(let num):
        return .makeNumber(ceil(num))
      default:
        throw EvalError.typeError(expr, [.realType])
    }
  }

  func truncate(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_):
        return expr
      case .rational(let num):
        return .makeNumber(Int64(trunc(Double(num.value.numerator) / Double(num.value.denominator))))
      case .bigrat(let num):
        return .makeNumber(Int64(trunc(num.value.numerator.doubleValue /
                       num.value.denominator.doubleValue)))
      case .flonum(let num):
        return .makeNumber(trunc(num))
      default:
        throw EvalError.typeError(expr, [.realType])
    }
  }

  func round(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_):
        return expr
      case .rational(let num):
        return .makeNumber(Int64(Foundation.round(Double(num.value.numerator) /
                       Double(num.value.denominator))))
      case .bigrat(let num):
        return .makeNumber(Int64(Foundation.round(num.value.numerator.doubleValue /
                       num.value.denominator.doubleValue)))
      case .flonum(let num):
        return .makeNumber(Foundation.round(num))
      default:
        throw EvalError.typeError(expr, [.realType])
    }
  }


  //-------- MARK: - Arithmetic primitives

  func plus(_ exprs: Arguments) throws -> Expr {
    var acc = Expr.fixnum(0)
    for expr in exprs {
      switch try NumberPair(acc, expr) {
        case .fixnumPair(let lhs, let rhs):
          let (res, overflow) = Int64.addWithOverflow(lhs, rhs)
          acc = overflow ? .makeNumber(BigInt(lhs) + BigInt(rhs)) : .makeNumber(res)
        case .bignumPair(let lhs, let rhs):
          acc = .makeNumber(lhs + rhs)
        case .rationalPair(let lhs, let rhs):
          let (res, overflow) = Rational.addWithOverflow(lhs, rhs)
          acc = overflow ? .makeNumber(Rational(BigInt(lhs.numerator), BigInt(lhs.denominator)) +
                                   Rational(BigInt(rhs.numerator), BigInt(rhs.denominator)))
                         : .makeNumber(res)
        case .bigRationalPair(let lhs, let rhs):
          acc = .makeNumber(lhs + rhs)
        case .flonumPair(let lhs, let rhs):
          acc = .makeNumber(lhs + rhs)
        case .complexPair(let lhs, let rhs):
          acc = .makeNumber(lhs + rhs)
      }
    }
    return acc
  }

  func minus(_ first: Expr, _ exprs: Arguments) throws -> Expr {
    var acc = first.normalized
    if exprs.isEmpty {
      switch acc {
        case .fixnum(let res):
          return .makeNumber(-res)
        case .bignum(let res):
          return .makeNumber(res.negate)
        case .rational(let res):
          return .makeNumber(res.value.negate)
        case .bigrat(let res):
          return .makeNumber(res.value.negate)
        case .flonum(let res):
          return .makeNumber(-res)
        case .complex(let res):
          return .makeNumber(res.value.negate)
        default:
          throw EvalError.typeError(first, [.numberType])
      }
    }
    for expr in exprs {
      switch try NumberPair(acc, expr) {
        case .fixnumPair(let lhs, let rhs):
          let (res, overflow) = Int64.subtractWithOverflow(lhs, rhs)
          acc = overflow ? .makeNumber(BigInt(lhs) - BigInt(rhs)) : .makeNumber(res)
        case .bignumPair(let lhs, let rhs):
          acc = .makeNumber(lhs - rhs)
        case .rationalPair(let lhs, let rhs):
          let (res, overflow) = Rational.subtractWithOverflow(lhs, rhs)
          acc = overflow ? .makeNumber(Rational(BigInt(lhs.numerator), BigInt(lhs.denominator)) -
                                   Rational(BigInt(rhs.numerator), BigInt(rhs.denominator)))
                         : .makeNumber(res)
        case .bigRationalPair(let lhs, let rhs):
          acc = .makeNumber(lhs - rhs)
        case .flonumPair(let lhs, let rhs):
          acc = .makeNumber(lhs - rhs)
        case .complexPair(let lhs, let rhs):
          acc = .makeNumber(lhs - rhs)
      }
    }
    return acc
  }

  func mult(_ exprs: Arguments) throws -> Expr {
    var acc = Expr.fixnum(1)
    for expr in exprs {
      switch try NumberPair(acc, expr) {
        case .fixnumPair(let lhs, let rhs):
          let (res, overflow) = Int64.multiplyWithOverflow(lhs, rhs)
          acc = overflow ? .makeNumber(BigInt(lhs) * BigInt(rhs)) : .makeNumber(res)
        case .bignumPair(let lhs, let rhs):
          acc = .makeNumber(lhs * rhs)
        case .rationalPair(let lhs, let rhs):
          let (res, overflow) = Rational.multiplyWithOverflow(lhs, rhs)
          acc = overflow ? .makeNumber(Rational(BigInt(lhs.numerator), BigInt(lhs.denominator)) *
                                   Rational(BigInt(rhs.numerator), BigInt(rhs.denominator)))
                         : .makeNumber(res)
        case .bigRationalPair(let lhs, let rhs):
          acc = .makeNumber(lhs * rhs)
        case .flonumPair(let lhs, let rhs):
          acc = .makeNumber(lhs * rhs)
        case .complexPair(let lhs, let rhs):
          acc = .makeNumber(lhs * rhs)
      }
    }
    return acc
  }

  func div(_ first: Expr, _ exprs: Arguments) throws -> Expr {
    var acc = first.normalized
    if exprs.isEmpty {
      switch acc {
        case .fixnum(let res):
          guard res != 0 else {
            throw EvalError.divisionByZero
          }
          return .makeNumber(Rational(1, res))
        case .bignum(let res):
          guard !res.isZero else {
            throw EvalError.divisionByZero
          }
          return .makeNumber(Rational(BigInt(1), res))
        case .rational(let res):
          guard !res.value.isZero else {
            throw EvalError.divisionByZero
          }
          return .makeNumber(Rational(res.value.denominator, res.value.numerator))
        case .bigrat(let res):
          guard !res.value.isZero else {
            throw EvalError.divisionByZero
          }
          return .makeNumber(Rational(res.value.denominator, res.value.numerator))
        case .flonum(let res):
          return .makeNumber(1.0 / res)
        case .complex(let res):
          return .makeNumber(1.0 / res.value)
        default:
          throw EvalError.typeError(first, [.numberType])
      }
    }
    for expr in exprs {
      switch try NumberPair(acc, expr) {
        case .fixnumPair(let lhs, let rhs):
          guard rhs != 0 else {
            throw EvalError.divisionByZero
          }
          let (res, overflow) = Rational.rationalWithOverflow(lhs, rhs)
          acc = overflow ? .makeNumber(Rational(BigInt(lhs), BigInt(rhs))) : .makeNumber(res)
        case .bignumPair(let lhs, let rhs):
          acc = .makeNumber(Rational(lhs, rhs))
        case .rationalPair(let lhs, let rhs):
          let (res, overflow) = Rational.divideWithOverflow(lhs, rhs)
          acc = overflow ? .makeNumber(Rational(BigInt(lhs.numerator), BigInt(lhs.denominator)) /
                                   Rational(BigInt(rhs.numerator), BigInt(rhs.denominator)))
                         : .makeNumber(res)
        case .bigRationalPair(let lhs, let rhs):
          acc = .makeNumber(lhs / rhs)
        case .flonumPair(let lhs, let rhs):
          acc = .makeNumber(lhs / rhs)
        case .complexPair(let lhs, let rhs):
          acc = .makeNumber(lhs / rhs)
      }
    }
    return acc
  }
  
  
  //-------- MARK: - Comparison primitives
  
  func equals(_ first: Expr, _ exprs: Arguments) throws -> Expr {
    try first.assertType(.numberType)
    var last = first
    for expr in exprs {
      guard try compareNumber(last, with: expr) == 0 else {
        return Expr.false
      }
      last = expr
    }
    return Expr.true
  }

  func lessThan(_ first: Expr, _ exprs: Arguments) throws -> Expr {
    try first.assertType(.numberType)
    var last = first
    for expr in exprs {
      guard try compareNumber(last, with: expr) < 0 else {
        return Expr.false
      }
      last = expr
    }
    return Expr.true
  }

  func lessThanEquals(_ first: Expr, _ exprs: Arguments) throws -> Expr {
    try first.assertType(.numberType)
    var last = first
    for expr in exprs {
      guard try compareNumber(last, with: expr) <= 0 else {
        return Expr.false
      }
      last = expr
    }
    return Expr.true
  }

  func biggerThan(_ first: Expr, _ exprs: Arguments) throws -> Expr {
    try first.assertType(.numberType)
    var last = first
    for expr in exprs {
      guard try compareNumber(last, with: expr) > 0 else {
        return Expr.false
      }
      last = expr
    }
    return Expr.true
  }

  func biggerThanEquals(_ first: Expr, _ exprs: Arguments) throws -> Expr {
    try first.assertType(.numberType)
    var last = first
    for expr in exprs {
      guard try compareNumber(last, with: expr) >= 0 else {
        return Expr.false
      }
      last = expr
    }
    return Expr.true
  }

  func max(_ first: Expr, _ exprs: Arguments) throws -> Expr {
    try first.assertType(.numberType)
    var res = first
    for expr in exprs {
      if try compareNumber(res, with: expr) < 0 {
        res = expr
      }
    }
    return res
  }

  func min(_ first: Expr, _ exprs: Arguments) throws -> Expr {
    try first.assertType(.numberType)
    var res = first
    for expr in exprs {
      if try compareNumber(res, with: expr) > 0 {
        res = expr
      }
    }
    return res
  }


  //-------- MARK: - Numeric functions

  func absolute(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(let num):
        return .makeNumber(num < 0 ? -num : num)
      case .bignum(let num):
        return .makeNumber(num.isNegative ? -num : num)
      case .rational(let num):
        return .makeNumber(num.value.isNegative ? -num.value : num.value)
      case .bigrat(let num):
        return .makeNumber(num.value.isNegative ? -num.value : num.value)
      case .flonum(let num):
        return .flonum((num.sign == .minus) ? -num : num)
      default:
        throw EvalError.typeError(expr, [.realType])
    }
  }
  
  func square(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(let num):
        let (res, overflow) = Int64.multiplyWithOverflow(num, num)
        return overflow ? .makeNumber(BigInt(num) * BigInt(num)) : .makeNumber(res)
      case .bignum(let num):
        return .makeNumber(num * num)
      case .rational(let num):
        let (res, overflow) = Rational.multiplyWithOverflow(num.value, num.value)
        return overflow ? .makeNumber(Rational(BigInt(num.value.numerator), BigInt(num.value.denominator)) *
                                  Rational(BigInt(num.value.numerator), BigInt(num.value.denominator)))
                        : .makeNumber(res)
      case .bigrat(let num):
        return .makeNumber(num.value * num.value)
      case .flonum(let num):
        return .makeNumber(num * num)
      case .complex(let  num):
        return .makeNumber(num.value * num.value)
      default:
        throw EvalError.typeError(expr, [.numberType])
    }
  }

  func sqrt(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_), .rational(_), .bigrat(_), .flonum(_):
        let dbl = try expr.asDouble(coerce: true)
        let res = Foundation.sqrt(dbl)
        return res.isNaN ? .makeNumber(Complex(dbl).sqrt) : .makeNumber(res)
      case .complex(let num):
        return .makeNumber(num.value.sqrt)
      default:
        throw EvalError.typeError(expr, [.numberType])
    }
  }
  
  func expt(_ expr: Expr, _ exp: Expr) throws -> Expr {
    switch try NumberPair(expr, exp) {
      case .fixnumPair(let x, let y):
        return .makeNumber(BigInt(x) ** BigInt(y))
      case .bignumPair(let x, let y):
        return .makeNumber(x ** y)
      case .rationalPair(let x, let y):
        return .makeNumber(Foundation.exp(y.doubleValue * Foundation.log(x.doubleValue)))
      case .bigRationalPair(let x, let y):
        return .makeNumber(Foundation.exp(y.doubleValue * Foundation.log(x.doubleValue)))
      case .flonumPair(let x, let y):
        return .makeNumber(Foundation.exp(y * Foundation.log(x)))
      case .complexPair(let x, let y):
        return .makeNumber((y * x.log).exp)
    }
  }

  func exp(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_), .rational(_), .bigrat(_), .flonum(_):
        let dbl = try expr.asDouble(coerce: true)
        let res = Foundation.exp(dbl)
        return res.isNaN ? .makeNumber(Complex(dbl).exp) : .makeNumber(res)
      case .complex(let num):
        return .makeNumber(num.value.exp)
      default:
        throw EvalError.typeError(expr, [.numberType])
    }
  }

  func log(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_), .rational(_), .bigrat(_), .flonum(_):
        let dbl = try expr.asDouble(coerce: true)
        let res = Foundation.log(dbl)
        return res.isNaN ? .makeNumber(Complex(dbl).log) : .makeNumber(res)
      case .complex(let num):
        return .makeNumber(num.value.log)
      default:
        throw EvalError.typeError(expr, [.numberType])
    }
  }

  func sin(_ expr: Expr) throws -> Expr {
    return .makeNumber(Foundation.sin(try expr.asDouble(coerce: true)))
  }

  func cos(_ expr: Expr) throws -> Expr {
    return .makeNumber(Foundation.cos(try expr.asDouble(coerce: true)))
  }

  func tan(_ expr: Expr) throws -> Expr {
    return .makeNumber(Foundation.tan(try expr.asDouble(coerce: true)))
  }

  func asin(_ expr: Expr) throws -> Expr {
    return .makeNumber(Foundation.asin(try expr.asDouble(coerce: true)))
  }

  func acos(_ expr: Expr) throws -> Expr {
    return .makeNumber(Foundation.acos(try expr.asDouble(coerce: true)))
  }

  func atan(_ fst: Expr, _ snd: Expr?) throws -> Expr {
    let y = try fst.asDouble(coerce: true)
    if let snd = snd {
      return .makeNumber(Foundation.atan2(y, try snd.asDouble(coerce: true)))
    } else {
      return .makeNumber(Foundation.atan(y))
    }
  }
  
  func numberToString(_ expr: Expr, _ rad: Expr?) throws -> Expr {
    var radix = 10
    if let base = try rad?.asInt() {
      if base == 2 || base == 8 || base == 10 || base == 16 {
        radix = base
      } else {
        throw EvalError.illegalRadix(rad!)
      }
    }
    switch expr {
      case .fixnum(let num):
        return .string(NSMutableString(string: String(num, radix: radix)))
      case .bignum(let num):
        return .string(NSMutableString(string: num.toString(base: BigInt.base(of: radix))))
      case .rational(let num):
        return .string(NSMutableString(string: String(num.value.numerator, radix: radix) + "/" +
                                            String(num.value.denominator, radix: radix)))
      case .bigrat(let num):
        return .string(
          NSMutableString(string: num.value.numerator.toString(base: BigInt.base(of: radix)) +
                          "/" + num.value.denominator.toString(base: BigInt.base(of: radix))))
      case .flonum(let num):
        if radix != 10 {
          throw EvalError.illegalRadix(rad!)
        }
        return .string(NSMutableString(string: String(num)))
      case .complex(let num):
        if radix != 10 {
          throw EvalError.illegalRadix(rad!)
        }
        return .string(NSMutableString(string: num.value.description))
      default:
        throw EvalError.typeError(expr, [.numberType])
    }
  }
  
  func stringToNumber(_ expr: Expr, _ rad: Expr?) throws -> Expr {
    var radix = 10
    if let base = try rad?.asInt() {
      if base == 2 || base == 8 || base == 10 || base == 16 {
        radix = base
      } else {
        throw EvalError.illegalRadix(rad!)
      }
    }
    let scanner = Scanner(string: try expr.asString(), prescan: false)
    scanner.skipSpace()
    guard scanner.ch != EOF_CH else {
      throw EvalError.typeError(expr, [.numberType])
    }
    scanner.scanSignedNumber(radix)
    let token = scanner.token
    scanner.skipSpace()
    guard scanner.ch == EOF_CH else {
      throw EvalError.typeError(expr, [.numberType])
    }
    switch token.kind {
      case .int:
        return .fixnum(token.intVal)
      case .bigint:
        return .bignum(token.bigIntVal)
      case .rat:
        return .rational(ImmutableBox(token.ratVal))
      case .bigrat:
        return .bigrat(ImmutableBox(token.bigRatVal))
      case .float:
        return .flonum(token.floatVal)
      case .complex:
        return .complex(ImmutableBox(token.complexVal))
      default:
        throw EvalError.typeError(expr, [.numberType])
    }
  }

  fileprivate static func findBestRat(_ t: Double, _ l: Int64) -> (Double, Int64, Int64) {
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
  
  func approximate(_ x: Expr, delta: Expr) throws -> Expr {
    var l_curr: Int64 = 1
    let t = try x.asDouble(coerce: true)
    let err = try delta.asDouble(coerce: true)
    var (actual, n, d) = MathLibrary.findBestRat(t, l_curr)
    while abs(actual) > err && l_curr < (Int64.max / 1000) {
      l_curr *= 10
      (actual, n, d) = MathLibrary.findBestRat(t, l_curr)
    }
    (actual, n, d) = MathLibrary.findBestRat(t, l_curr)
    return .rational(ImmutableBox(Rational(n, d)))
  }
  
  func makeRectangular(_ re: Expr, _ imag: Expr) throws -> Expr {
    return .complex(ImmutableBox(Complex(try re.asDouble(coerce: true),
                                            try imag.asDouble(coerce: true))))
  }
  
  func makePolar(_ abs: Expr, _ arg: Expr) throws -> Expr {
    return .complex(ImmutableBox(Complex(abs: try abs.asDouble(coerce: true),
                                            arg: try arg.asDouble(coerce: true))))
  }
  
  func realPart(_ expr: Expr) throws -> Expr {
    return .flonum(try expr.asComplex(coerce: true).re)
  }
  
  func imagPart(_ expr: Expr) throws -> Expr {
    return .flonum(try expr.asComplex(coerce: true).im)
  }
  
  func magnitude(_ expr: Expr) throws -> Expr {
    return .flonum(try expr.asComplex(coerce: true).abs)
  }
  
  func angle(_ expr: Expr) throws -> Expr {
    return .flonum(try expr.asComplex(coerce: true).arg)
  }
  
  func numerator(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_):
        return expr
      case .rational(let num):
        return .fixnum(num.value.numerator)
      case .bigrat(let num):
        return .makeNumber(num.value.numerator)
      case .flonum(let num):
        return .flonum(Double(MathLibrary.approximate(num).numerator))
      default:
        throw EvalError.typeError(expr, [.numberType])
    }
  }
  
  func denominator(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_):
        return .fixnum(1)
      case .rational(let num):
        return .fixnum(num.value.denominator)
      case .bigrat(let num):
        return .makeNumber(num.value.denominator)
      case .flonum(let num):
        return .flonum(Double(MathLibrary.approximate(num).denominator))
      default:
        throw EvalError.typeError(expr, [.numberType])
    }
  }
  
  func gcd(_ exprs: Arguments) throws -> Expr {
    var acc = Expr.fixnum(0)
    for expr in exprs {
      var e = expr
      if case .flonum(let num) = expr {
        e = .makeNumber(MathLibrary.approximate(num))
      }
      switch try NumberPair(acc, e) {
        case .fixnumPair(let lhs, let rhs):
          let (res, overflow) = Rational.gcdWithOverflow(lhs, rhs)
          acc = overflow ? .makeNumber(Rational.gcd(BigInt(lhs), BigInt(rhs))) : .makeNumber(res)
        case .bignumPair(let lhs, let rhs):
          acc = .makeNumber(Rational.gcd(lhs, rhs))
        case .rationalPair(let lhs, let rhs):
          let (res, overflow) = Rational.gcdWithOverflow(lhs, rhs)
          acc = overflow ?
              .makeNumber(Rational.gcd(Rational(BigInt(lhs.numerator), BigInt(lhs.denominator)),
                                   Rational(BigInt(rhs.numerator), BigInt(rhs.denominator))))
            : .makeNumber(res)
        case .bigRationalPair(let lhs, let rhs):
          acc = .makeNumber(Rational.gcd(lhs, rhs))
        default:
          throw EvalError.typeError(expr, [.realType])
      }
    }
    return acc
  }
  
  func lcm(_ exprs: Arguments) throws -> Expr {
    var acc = Expr.fixnum(1)
    for expr in exprs {
      var e = expr
      if case .flonum(let num) = expr {
        e = .makeNumber(MathLibrary.approximate(num))
      }
      switch try NumberPair(acc, e) {
        case .fixnumPair(let lhs, let rhs):
          let (res, overflow) = Rational.lcmWithOverflow(lhs, rhs)
          acc = overflow ? .makeNumber(Rational.lcm(BigInt(lhs), BigInt(rhs))) : .makeNumber(res)
        case .bignumPair(let lhs, let rhs):
          acc = .makeNumber(Rational.lcm(lhs, rhs))
        case .rationalPair(let lhs, let rhs):
          let (res, overflow) = Rational.lcmWithOverflow(lhs, rhs)
          acc = overflow ?
              .makeNumber(Rational.lcm(Rational(BigInt(lhs.numerator), BigInt(lhs.denominator)),
                                   Rational(BigInt(rhs.numerator), BigInt(rhs.denominator))))
            : .makeNumber(res)
        case .bigRationalPair(let lhs, let rhs):
          acc = .makeNumber(Rational.lcm(lhs, rhs))
        default:
          throw EvalError.typeError(expr, [.realType])
      }
    }
    return acc
  }
  
  func truncateQuotient(_ x: Expr, _ y: Expr) throws -> Expr {
    switch try NumberPair(x, y) {
      case .fixnumPair(let lhs, let rhs):
        return .makeNumber(lhs / rhs)
      case .bignumPair(let lhs, let rhs):
        return .makeNumber(lhs / rhs)
      default:
        try x.assertType(.integerType)
        try y.assertType(.integerType)
        preconditionFailure()
    }
  }
  
  func truncateRemainder(_ x: Expr, _ y: Expr) throws -> Expr {
    switch try NumberPair(x, y) {
      case .fixnumPair(let lhs, let rhs):
        return .makeNumber(lhs % rhs)
      case .bignumPair(let lhs, let rhs):
        return .makeNumber(lhs % rhs)
      default:
        try x.assertType(.integerType)
        try y.assertType(.integerType)
        preconditionFailure()
    }
  }
  
  func floorQuotient(_ x: Expr, _ y: Expr) throws -> Expr {
    switch try NumberPair(x, y) {
      case .fixnumPair(let lhs, let rhs):
        let res = lhs % rhs
        return .makeNumber(((res < 0) == (rhs < 0) ? (lhs - res) : (lhs - res - rhs)) / rhs)
      case .bignumPair(let lhs, let rhs):
        let res = lhs % rhs
        return .makeNumber((res.isNegative == rhs.isNegative ? (lhs - res) : (lhs - res - rhs)) / rhs)
      default:
        try x.assertType(.integerType)
        try y.assertType(.integerType)
        preconditionFailure()
    }
  }
  
  func floorRemainder(_ x: Expr, _ y: Expr) throws -> Expr {
    switch try NumberPair(x, y) {
      case .fixnumPair(let lhs, let rhs):
        let res = lhs % rhs
        return .makeNumber((res < 0) == (rhs < 0) ? res : res + rhs)
      case .bignumPair(let lhs, let rhs):
        let res = lhs % rhs
        return .makeNumber(res.isNegative == rhs.isNegative ? res : res + rhs)
      default:
        try x.assertType(.integerType)
        try y.assertType(.integerType)
        preconditionFailure()
    }
  }
  
  @inline(__always) private func compileBinOp(_ compiler: Compiler, expr: Expr, env: Env) throws {
    guard case .pair(_, .pair(let x, .pair(let y, .null))) = expr else {
      throw EvalError.argumentCountError(formals: 2, args: expr)
    }
    try compiler.compile(x, in: env, inTailPos: false)
    try compiler.compile(y, in: env, inTailPos: false)
  }
  
  func fxPlus(_ x: Expr, _ y: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() &+ y.asInt64())
  }
  
  func compileFxPlus(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let x, .pair(let y, .null))) = expr else {
      throw EvalError.argumentCountError(formals: 2, args: expr)
    }
    if case .fixnum(1) = x {
      try compiler.compile(y, in: env, inTailPos: false)
      compiler.emit(.fxInc)
    } else if case .fixnum(1) = y {
      try compiler.compile(x, in: env, inTailPos: false)
      compiler.emit(.fxInc)
    } else if case .fixnum(-1) = x {
      try compiler.compile(y, in: env, inTailPos: false)
      compiler.emit(.fxDec)
    } else if case .fixnum(-1) = y {
      try compiler.compile(x, in: env, inTailPos: false)
      compiler.emit(.fxDec)
    } else {
      try compiler.compile(x, in: env, inTailPos: false)
      try compiler.compile(y, in: env, inTailPos: false)
      compiler.emit(.fxPlus)
    }
    return false
  }
  
  func fxMinus(_ x: Expr, _ y: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() &- y.asInt64())
  }
  
  func compileFxMinus(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let x, .pair(let y, .null))) = expr else {
      throw EvalError.argumentCountError(formals: 2, args: expr)
    }
    if case .fixnum(1) = y {
      try compiler.compile(x, in: env, inTailPos: false)
      compiler.emit(.fxDec)
    } else if case .fixnum(-1) = y {
      try compiler.compile(x, in: env, inTailPos: false)
      compiler.emit(.fxInc)
    } else {
      try compiler.compile(x, in: env, inTailPos: false)
      try compiler.compile(y, in: env, inTailPos: false)
      compiler.emit(.fxMinus)
    }
    return false
  }
  
  func fxMult(_ x: Expr, _ y: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() &* y.asInt64())
  }
  
  func compileFxMult(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.fxMult)
    return false
  }
  
  func fxDiv(_ x: Expr, _ y: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() / y.asInt64())
  }
  
  func compileFxDiv(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.fxDiv)
    return false
  }
  
  func fx1Plus(_ x: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() &+ 1)
  }
  
  func compileFx1Plus(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let x, .null)) = expr else {
      throw EvalError.argumentCountError(formals: 2, args: expr)
    }
    try compiler.compile(x, in: env, inTailPos: false)
    compiler.emit(.fxInc)
    return false
  }
  
  func fx1Minus(_ x: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() &- 1)
  }
  
  func compileFx1Minus(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let x, .null)) = expr else {
      throw EvalError.argumentCountError(formals: 2, args: expr)
    }
    try compiler.compile(x, in: env, inTailPos: false)
    compiler.emit(.fxDec)
    return false
  }
  
  func fxIsZero(_ x: Expr) throws -> Expr {
    return .makeBoolean(try x.asInt64() == 0)
  }
  
  func compileFxIsZero(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let x, .null)) = expr else {
      throw EvalError.argumentCountError(formals: 2, args: expr)
    }
    try compiler.compile(x, in: env, inTailPos: false)
    compiler.emit(.fxIsZero)
    return false
  }
  
  func fxIsPositive(_ x: Expr) throws -> Expr {
    return .makeBoolean(try x.asInt64() > 0)
  }
  
  func fxIsNegative(_ x: Expr) throws -> Expr {
    return .makeBoolean(try x.asInt64() < 0)
  }
  
  func fxEq(_ x: Expr, _ y: Expr) throws -> Expr {
    return .makeBoolean(try x.asInt64() == y.asInt64())
  }
  
  func compileFxEq(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let x, .pair(let y, .null))) = expr else {
      throw EvalError.argumentCountError(formals: 2, args: expr)
    }
    if case .fixnum(0) = x {
      try compiler.compile(y, in: env, inTailPos: false)
      compiler.emit(.fxIsZero)
    } else if case .fixnum(0) = y {
      try compiler.compile(x, in: env, inTailPos: false)
      compiler.emit(.fxIsZero)
    } else {
      try compiler.compile(x, in: env, inTailPos: false)
      try compiler.compile(y, in: env, inTailPos: false)
      compiler.emit(.fxEq)
    }
    return false
  }
  
  func fxLt(_ x: Expr, _ y: Expr) throws -> Expr {
    return .makeBoolean(try x.asInt64() < y.asInt64())
  }
  
  func compileFxLt(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.fxLt)
    return false
  }
  
  func fxGt(_ x: Expr, _ y: Expr) throws -> Expr {
    return .makeBoolean(try x.asInt64() > y.asInt64())
  }
  
  func compileFxGt(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.fxGt)
    return false
  }
  
  func fxLtEq(_ x: Expr, _ y: Expr) throws -> Expr {
    return .makeBoolean(try x.asInt64() <= y.asInt64())
  }
  
  func compileFxLtEq(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.fxLtEq)
    return false
  }

  func fxGtEq(_ x: Expr, _ y: Expr) throws -> Expr {
    return .makeBoolean(try x.asInt64() >= y.asInt64())
  }
  
  func compileFxGtEq(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.fxLtEq)
    return false
  }
  
  func fxRemainder(_ x: Expr, _ y: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() % y.asInt64())
  }
  
  func fxModulo(_ x: Expr, _ y: Expr) throws -> Expr {
    let rhs = try y.asInt64()
    let res = try x.asInt64() % rhs
    return .fixnum((res < 0) == (rhs < 0) ? res : res + rhs)
  }
  
  func fxAbs(_ x: Expr) throws -> Expr {
    let res = try x.asInt64()
    return .fixnum(res < 0 ? -res : res)
  }
  
  func fxAnd(_ x: Expr, _ y: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() & y.asInt64())
  }
  
  func fxIor(_ x: Expr, _ y: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() | y.asInt64())
  }
  
  func fxXor(_ x: Expr, _ y: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() ^ y.asInt64())
  }
  
  func fxNot(_ x: Expr) throws -> Expr {
    return .fixnum(try ~x.asInt64())
  }
  
  func fxLshift(_ x: Expr, _ y: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() << Int64(y.asInt(below: 64)))
  }
  
  func fxRshift(_ x: Expr, _ y: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() >> Int64(y.asInt(below: 64)))
  }
  
  func fxLrshift(_ x: Expr, _ y: Expr) throws -> Expr {
    return .fixnum(
      Int64(bitPattern: UInt64(bitPattern: try x.asInt64()) >> UInt64(try y.asInt(below: 64))))
  }
  
  func fxMin(_ x: Expr, _ y: Expr) throws -> Expr {
    let xint = try x.asInt64()
    let yint = try y.asInt64()
    return .fixnum(xint < yint ? xint : yint)
  }
  
  func fxMax(_ x: Expr, _ y: Expr) throws -> Expr {
    let xint = try x.asInt64()
    let yint = try y.asInt64()
    return .fixnum(xint > yint ? xint : yint)
  }
  
  func flPlus(_ x: Expr, _ y: Expr) throws -> Expr {
    return .flonum(try x.asDouble() + y.asDouble())
  }
  
  func compileFlPlus(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.flPlus)
    return false
  }
  
  func flMinus(_ x: Expr, _ y: Expr) throws -> Expr {
    return .flonum(try x.asDouble() - y.asDouble())
  }
  
  func compileFlMinus(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.flMinus)
    return false
  }

  func flMult(_ x: Expr, _ y: Expr) throws -> Expr {
    return .flonum(try x.asDouble() * y.asDouble())
  }
  
  func compileFlMult(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.flMult)
    return false
  }
  
  func flDiv(_ x: Expr, _ y: Expr) throws -> Expr {
    return .flonum(try x.asDouble() / y.asDouble())
  }
  
  func compileFlDiv(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.flDiv)
    return false
  }
  
  func flIsZero(_ x: Expr) throws -> Expr {
    return .makeBoolean(try x.asDouble() == 0.0)
  }
  
  func flIsPositive(_ x: Expr) throws -> Expr {
    return .makeBoolean(try x.asDouble() > 0.0)
  }
  
  func flIsNegative(_ x: Expr) throws -> Expr {
    return .makeBoolean(try x.asDouble() < 0.0)
  }
  
  func flEq(_ x: Expr, _ y: Expr) throws -> Expr {
    return .makeBoolean(try x.asDouble() == y.asDouble())
  }
  
  func compileFlEq(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.flEq)
    return false
  }
  
  func flLt(_ x: Expr, _ y: Expr) throws -> Expr {
    return .makeBoolean(try x.asDouble() < y.asDouble())
  }
  
  func compileFlLt(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.flLt)
    return false
  }
  
  func flGt(_ x: Expr, _ y: Expr) throws -> Expr {
    return .makeBoolean(try x.asDouble() > y.asDouble())
  }
  
  func compileFlGt(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.flGt)
    return false
  }
  
  func flLtEq(_ x: Expr, _ y: Expr) throws -> Expr {
    return .makeBoolean(try x.asDouble() <= y.asDouble())
  }
  
  func compileFlLtEq(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.flLtEq)
    return false
  }

  func flGtEq(_ x: Expr, _ y: Expr) throws -> Expr {
    return .makeBoolean(try x.asDouble() >= y.asDouble())
  }
  
  func compileFlGtEq(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.flLtEq)
    return false
  }
  
  func flAbs(_ x: Expr) throws -> Expr {
    let xint = try x.asDouble()
    return .flonum(xint < 0 ? -xint : xint)
  }
  
  func flMin(_ x: Expr, _ y: Expr) throws -> Expr {
    let xint = try x.asDouble()
    let yint = try y.asDouble()
    return .flonum(xint < yint ? xint : yint)
  }
  
  func flMax(_ x: Expr, _ y: Expr) throws -> Expr {
    let xint = try x.asDouble()
    let yint = try y.asDouble()
    return .flonum(xint > yint ? xint : yint)
  }
}
