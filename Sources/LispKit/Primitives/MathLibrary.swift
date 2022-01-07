//
//  MathLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 23/01/2016.
//  Copyright © 2016-2022 ObjectHub. All rights reserved.
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
import AppKit

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
    self.`import`(from: ["lispkit", "core"],    "define", "lambda")
    self.`import`(from: ["lispkit", "control"], "letrec", "let", "cond", "if")
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("number?", self.isNumber))
    self.define(Procedure("real?", self.isReal))
    self.define(Procedure("integer?", self.isInteger))
    self.define(Procedure("rational?", self.isRational))
    self.define(Procedure("complex?", self.isComplex))
    self.define(Procedure("fixnum?", self.isFixnum))
    self.define(Procedure("ratnum?", self.isRatnum))
    self.define(Procedure("bignum?", self.isBignum))
    self.define(Procedure("flonum?", self.isFlonum))
    self.define(Procedure("cflonum?", self.isCflonum))
    self.define(Procedure("exact?", self.isExact))
    self.define(Procedure("inexact?", self.isInexact))
    self.define(Procedure("exact-integer?", self.isExactInteger))
    self.define(Procedure("finite?", self.isFinite))
    self.define(Procedure("infinite?", self.isInfinite))
    self.define(Procedure("nan?", self.isNaN))
    self.define(Procedure("positive?", self.isPositive))
    self.define(Procedure("negative?", self.isNegative))
    self.define(Procedure("zero?", self.isZero))
    self.define(Procedure("even?", self.isEven))
    self.define(Procedure("odd?", self.isOdd))
    self.define(Procedure("inexact", self.inexact))
    self.define(Procedure("exact", self.exact))
    self.define(Procedure("approximate", self.approximate))
    self.define("rationalize", via:
      """
       (define (rationalize x e)
         (letrec ((simplest (lambda (x y return)
                              (let ((fx (floor x)) (fy (floor y)))
                                (cond ((>= fx x) (return fx 1))
                                      ((= fx fy) (simplest (/ (- y fy)) (/ (- x fx))
                                                   (lambda (n d) (return (+ d (* fx n)) n))))
                                      (else      (return (+ fx 1) 1))))))
                  (ax (abs x))
                  (ae (abs e)))
         (simplest (- ax ae) (+ ax ae)
           (if (negative? x) (lambda (num den) (/ (- num) den)) /))))
       """)
    self.define(Procedure("floor", self.floor))
    self.define(Procedure("ceiling", self.ceiling))
    self.define(Procedure("truncate", self.truncate))
    self.define(Procedure("round", self.round))
    self.define(Procedure("random", self.random))
    self.define(Procedure("+", self.plus))
    self.define(Procedure("-", self.minus))
    self.define(Procedure("*", self.mult))
    self.define(Procedure("/", self.div))
    self.define(Procedure("=", self.equals))
    self.define(Procedure("<", self.lessThan))
    self.define(Procedure(">", self.biggerThan))
    self.define(Procedure("<=", self.lessThanEquals))
    self.define(Procedure(">=", self.biggerThanEquals))
    self.define(Procedure("max", self.max))
    self.define(Procedure("min", self.min))
    self.define(Procedure("abs", self.absolute))
    self.define(Procedure("square", self.square))
    self.define(Procedure("sqrt", self.sqrt))
    self.define(Procedure("exact-integer-sqrt", self.exactIntegerSqrt))
    self.define(Procedure("expt", self.expt))
    self.define(Procedure("exp", self.exp))
    self.define(Procedure("log", self.log))
    self.define(Procedure("sin", self.sin))
    self.define(Procedure("cos", self.cos))
    self.define(Procedure("tan", self.tan))
    self.define(Procedure("asin", self.asin))
    self.define(Procedure("acos", self.acos))
    self.define(Procedure("atan", self.atan))
    self.define(Procedure("number->string", self.numberToString))
    self.define(Procedure("string->number", self.stringToNumber))
    self.define(Procedure("make-rectangular", self.makeRectangular))
    self.define(Procedure("make-polar", self.makePolar))
    self.define(Procedure("real-part", self.realPart))
    self.define(Procedure("imag-part", self.imagPart))
    self.define(Procedure("magnitude", self.magnitude))
    self.define(Procedure("angle", self.angle))
    self.define(Procedure("numerator", self.numerator))
    self.define(Procedure("denominator", self.denominator))
    self.define(Procedure("gcd", self.gcd))
    self.define(Procedure("lcm", self.lcm))
    self.define(Procedure("truncate/", self.truncateDiv))
    self.define(Procedure("truncate-quotient", self.truncateQuotient))
    self.define(Procedure("truncate-remainder", self.truncateRemainder))
    self.define(Procedure("floor/", self.floorDiv))
    self.define(Procedure("floor-quotient", self.floorQuotient))
    self.define(Procedure("floor-remainder", self.floorRemainder))
    self.define(Procedure("quotient", self.truncateQuotient))
    self.define(Procedure("remainder", self.truncateRemainder))
    self.define(Procedure("modulo", self.floorRemainder))
    self.define(Procedure("fx+", self.fxPlus, self.compileFxPlus))
    self.define(Procedure("fx-", self.fxMinus, self.compileFxMinus))
    self.define(Procedure("fx*", self.fxMult, self.compileFxMult))
    self.define(Procedure("fx/", self.fxDiv, self.compileFxDiv))
    self.define(Procedure("fx=", self.fxEq, self.compileFxEq))
    self.define(Procedure("fx<", self.fxLt, self.compileFxLt))
    self.define(Procedure("fx>", self.fxGt, self.compileFxGt))
    self.define(Procedure("fx<=", self.fxLtEq, self.compileFxLtEq))
    self.define(Procedure("fx>=", self.fxGtEq, self.compileFxGtEq))
    self.define(Procedure("fx1+", self.fx1Plus, self.compileFx1Plus))
    self.define(Procedure("fx1-", self.fx1Minus, self.compileFx1Minus))
    self.define(Procedure("fxzero?", self.fxIsZero, self.compileFxIsZero))
    self.define(Procedure("fxpositive?", self.fxIsPositive))
    self.define(Procedure("fxnegative?", self.fxIsNegative))
    self.define(Procedure("fxodd?", self.fxIsOdd))
    self.define(Procedure("fxeven?", self.fxIsEven))
    self.define(Procedure("fxremainder", self.fxRemainder))
    self.define(Procedure("fxmodulo", self.fxModulo))
    self.define(Procedure("fxabs", self.fxAbs))
    self.define(Procedure("fxnot", self.fxNot))
    self.define(Procedure("fxand", self.fxAnd))
    self.define(Procedure("fxior", self.fxIor))
    self.define(Procedure("fxxor", self.fxXor))
    self.define(Procedure("fxif", self.fxIf))
    self.define(Procedure("fxlshift", self.fxLshift))
    self.define(Procedure("fxrshift", self.fxRshift))
    self.define(Procedure("fxlrshift", self.fxLrshift))
    self.define(Procedure("fxarithmetic-shift", self.fxShift))
    self.define(Procedure("fxarithmetic-shift-left", self.fxLshift))
    self.define(Procedure("fxarithmetic-shift-right", self.fxRshift))
    self.define(Procedure("fxlogical-shift-right", self.fxRshift))
    self.define(Procedure("fxbit-count", self.fxBitCount))
    self.define(Procedure("fxlength", self.fxLength))
    self.define(Procedure("fxfirst-bit-set", self.fxFirstBitSet))
    self.define(Procedure("fxbit-set?", self.fxIsBitSet))
    self.define(Procedure("fxcopy-bit", self.fxCopyBit))
    self.define(Procedure("fxmin", self.fxMin))
    self.define(Procedure("fxmax", self.fxMax))
    self.define(Procedure("fxrandom", self.fxRandom))
    self.define(Procedure("fxsqrt", self.fxSqrt))
    self.define(Procedure("integer->fixnum", self.integerToFx))
    self.define(Procedure("fixnum-width", self.fixnumWidth))
    self.define(Procedure("least-fixnum", self.leastFixnum))
    self.define(Procedure("greatest-fixnum", self.greatestFixnum))
    self.define(Procedure("real->flonum", self.realToFlonum))
    self.define(Procedure("make-flonum", self.makeFlonum))
    self.define(Procedure("flexponent", self.flExponent))
    self.define(Procedure("flsignificand", self.flSignificand))
    self.define(Procedure("fl+", self.flPlus, self.compileFlPlus))
    self.define(Procedure("fl-", self.flMinus, self.compileFlMinus))
    self.define(Procedure("fl*", self.flMult, self.compileFlMult))
    self.define(Procedure("fl/", self.flDiv, self.compileFlDiv))
    self.define(Procedure("flzero?", self.flIsZero))
    self.define(Procedure("flpositive?", self.flIsPositive))
    self.define(Procedure("flnegative?", self.flIsNegative))
    self.define(Procedure("fl=", self.flEq, self.compileFlEq))
    self.define(Procedure("fl<", self.flLt, self.compileFlLt))
    self.define(Procedure("fl>", self.flGt, self.compileFlGt))
    self.define(Procedure("fl<=", self.flLtEq, self.compileFlLtEq))
    self.define(Procedure("fl>=", self.flGtEq, self.compileFlGtEq))
    self.define(Procedure("flabs", self.flAbs))
    self.define(Procedure("flmin", self.flMin))
    self.define(Procedure("flmax", self.flMax))
    self.define(Procedure("flrandom", self.flRandom))
    self.define(Procedure("flsqrt", self.flSqrt))
    self.define(Procedure("flnext", self.flNext))
    self.define(Procedure("flprev", self.flPrev))
    self.define(Procedure("bitwise-not", self.bitwiseNot))
    self.define(Procedure("bitwise-and", self.bitwiseAnd))
    self.define(Procedure("bitwise-ior", self.bitwiseIor))
    self.define(Procedure("bitwise-xor", self.bitwiseXor))
    self.define("bitwise-if", via:
      """
       (define (bitwise-if x y z)
         (bitwise-ior (bitwise-and x y) (bitwise-and (bitwise-not x) z)))
      """)
    self.define(Procedure("bit-count", self.bitCount))
    self.define(Procedure("integer-length", self.integerLength))
    self.define(Procedure("first-bit-set", self.firstBitSet))
    self.define(Procedure("bit-set?", self.isBitSet))
    self.define(Procedure("copy-bit", self.copyBit))
    self.define(Procedure("arithmetic-shift", self.arithmeticShift))
    self.define(Procedure("arithmetic-shift-left", self.arithmeticShiftLeft))
    self.define(Procedure("arithmetic-shift-right", self.arithmeticShiftRight))
    self.define("pi", via: "(define pi \(Double.pi))")
    self.define("e", via: "(define e \(M_E))")
    self.define("fx-width", via: "(define fx-width \(Int64.bitWidth))")
    self.define("fx-greatest", via: "(define fx-greatest \(Int64.max))")
    self.define("fx-least", via: "(define fx-least \(Int64.min))")
  }
  
  
  //-------- MARK: - Classification primitives

  private func isNumber(_ expr: Expr) -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_), .rational(_, _), .flonum(_), .complex(_):
        return .true
      default:
        return .false
    }
  }
  
  private func isComplex(_ expr: Expr) -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_), .rational(_, _), .flonum(_), .complex(_):
        return .true
      default:
        return .false
    }
  }
  
  private func isReal(_ expr: Expr) -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_), .rational(_, _), .flonum(_):
        return .true
      case .complex(let num):
        return .makeBoolean(num.value.isReal)
      default:
        return .false
    }
  }
  
  private func isInteger(_ expr: Expr) -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_):
        return .true
      case .rational(_, .fixnum(let d)):
        return .makeBoolean(d == 1)
      case .rational(_, .bignum(let d)):
        return .makeBoolean(d == 1)
      case .flonum(let num):
        return .makeBoolean(Foundation.trunc(num) == num)
      default:
        return .false
    }
  }
  
  private func isRational(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_), .rational(_, _):
        return .true
      case .flonum(let num):
        return .makeBoolean(!num.isInfinite && !num.isNaN)
      case .complex(let num):
        return .makeBoolean(!num.value.re.isInfinite && !num.value.re.isNaN &&
                            !num.value.im.isInfinite && !num.value.im.isNaN)
      default:
        return .false
    }
  }
  
  private func isFixnum(_ expr: Expr) -> Expr {
    switch expr {
      case .fixnum(_):
        return .true
      default:
        return .false
    }
  }
  
  private func isRatnum(_ expr: Expr) -> Expr {
    switch expr {
      case .rational(_, _):
        return .true
      default:
        return .false
    }
  }
  
  private func isBignum(_ expr: Expr) -> Expr {
    switch expr {
      case .bignum(_):
        return .true
      default:
        return .false
    }
  }
  
  private func isFlonum(_ expr: Expr) -> Expr {
    switch expr {
      case .flonum(_):
        return .true
      default:
        return .false
    }
  }
  
  private func isCflonum(_ expr: Expr) -> Expr {
    switch expr {
      case .complex(_):
        return .true
      default:
        return .false
    }
  }
  
  private func isExact(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_), .rational(_, _):
        return .true
      default:
        return .false
    }
  }

  private func isInexact(_ expr: Expr) throws -> Expr {
    switch expr {
      case .flonum(_), .complex(_):
        return .true
      default:
        return .false
    }
  }

  private func isExactInteger(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_):
        return .true
      case .rational(_, .fixnum(let d)):
        return .makeBoolean(d == 1)
      case .rational(_, .bignum(let d)):
        return .makeBoolean(d == 1)
      default:
        return .false
    }
  }
  
  private func isFinite(_ expr: Expr) throws -> Expr {
    switch expr {
      case .flonum(let num):
        return .makeBoolean(num.isFinite)
      case .complex(let box):
        return .makeBoolean(!box.value.isInfinite)
      default:
        return .true
    }
  }

  private func isInfinite(_ expr: Expr) throws -> Expr {
    switch expr {
      case .flonum(let num):
        return .makeBoolean(num.isInfinite)
      case .complex(let box):
        return .makeBoolean(box.value.isInfinite)
      default:
        return .false
    }
  }
  
  private func isNaN(_ expr: Expr) throws -> Expr {
    switch expr {
      case .flonum(let num):
        return .makeBoolean(num.isNaN)
      case .complex(let box):
        return .makeBoolean(box.value.re.isNaN || box.value.im.isNaN)
      default:
        return .false
    }
  }
  
  private func isPositive(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(let num):
        return .makeBoolean(num > 0)
      case .bignum(let num):
        return .makeBoolean(!num.isNegative)
      case .rational(.fixnum(let n), _):
        return .makeBoolean(n > 0)
      case .rational(.bignum(let n), _):
        return .makeBoolean(n > 0)
      case .flonum(let num):
        return .makeBoolean(num > 0.0)
      case .complex(let num):
        return num.value.isReal ? .makeBoolean(num.value.re > 0) : .false
      default:
        throw RuntimeError.type(expr, expected: [.numberType])
    }
  }

  private func isNegative(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(let num):
        return .makeBoolean(num < 0)
      case .bignum(let num):
        return .makeBoolean(num.isNegative)
      case .rational(.fixnum(let n), _):
        return .makeBoolean(n < 0)
      case .rational(.bignum(let n), _):
        return .makeBoolean(n < 0)
      case .flonum(let num):
        return .makeBoolean(num < 0.0)
      case .complex(let num):
        return num.value.isReal ? .makeBoolean(num.value.re < 0) : .false
      default:
        throw RuntimeError.type(expr, expected: [.numberType])
    }
  }
  
  private func isZero(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(let num):
        return .makeBoolean(num == 0)
      case .bignum(let num):
        return .makeBoolean(num.isZero)
      case .rational(.fixnum(let n), _):
        return .makeBoolean(n == 0)
      case .rational(.bignum(let n), _):
        return .makeBoolean(n.isZero)
      case .flonum(let num):
        return .makeBoolean(num.isZero)
      case .complex(let num):
        return num.value.isReal ? .makeBoolean(num.value.re.isZero) : .false
      default:
        throw RuntimeError.type(expr, expected: [.numberType])
    }
  }

  private func isEven(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(let num):
        return .makeBoolean(num % 2 == 0)
      case .bignum(let num):
        return .makeBoolean(num % 2 == 0)
      case .flonum(let num):
        guard Foundation.trunc(num) == num else {
          throw RuntimeError.type(expr, expected: [.integerType])
        }
        return .makeBoolean(num.truncatingRemainder(dividingBy: 2) == 0.0)
      default:
        throw RuntimeError.type(expr, expected: [.integerType])
    }
  }

  private func isOdd(_ expr: Expr) throws -> Expr {
    switch expr.normalized {
      case .fixnum(let num):
        return .makeBoolean(num % 2 != 0)
      case .bignum(let num):
        return .makeBoolean(num % 2 != 0)
      case .flonum(let num):
        guard Foundation.trunc(num) == num else {
          throw RuntimeError.type(expr, expected: [.integerType])
        }
        return .makeBoolean(num.truncatingRemainder(dividingBy: 2) != 0.0)
      default:
        throw RuntimeError.type(expr, expected: [.integerType])
    }
  }


  //-------- MARK: - Conversion primitives

  private func inexact(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(let num):
        return .makeNumber(Double(num))
      case .bignum(let num):
        return .makeNumber(num.doubleValue)
      case .rational(.fixnum(let n), .fixnum(let d)):
        return .makeNumber(Double(n) / Double(d))
      case .rational(.bignum(let n), .bignum(let d)):
        return .makeNumber(n.doubleValue / d.doubleValue)
      case .flonum(_), .complex(_):
        return expr
      default:
        throw RuntimeError.type(expr, expected: [.numberType])
    }
  }
  
  private func exact(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_), .rational(_, _):
        return expr
      case .flonum(let num):
        return MathLibrary.approximateNumber(num)
      default:
        throw RuntimeError.type(expr, expected: [.realType])
    }
  }
  
  static func approximateNumber(_ x: Double, tolerance: Double = 1.0e-15) -> Expr {
    if let rat = MathLibrary.approximate(x, tolerance: tolerance) {
      return .makeNumber(rat)
    } else if let rat = MathLibrary.approximateBigRat(x, tolerance: tolerance) {
      return .makeNumber(rat)
    } else {
      return .false
    }
  }
  
  static func approximate(_ x: Double, tolerance: Double = 1.0e-15) -> Rational<Int64>? {
    var y: Double = abs(x)
    let mx = y * tolerance
    var (n1, d1) = (Int64(1), Int64(0))
    var (n2, d2) = (Int64(0), Int64(1))
    repeat {
      guard y.isFinite else {
        return nil
      }
      if let fy = Int64(exactly: Foundation.floor(y)) {
        (n1, n2) = (fy * n1 + n2, n1)
        (d1, d2) = (fy * d1 + d2, d1)
        y = 1.0 / (y - Foundation.floor(y))
      } else {
        return nil
      }
    } while abs(abs(x) - Double(n1) / Double(d1)) > mx
    return Rational(n1 * (x < 0.0 ? -1 : 1), d1)
  }
  
  static func approximateBigRat(_ x: Double, tolerance: Double = 1.0e-15) -> Rational<BigInt>? {
    var y: Double = abs(x)
    let mx = y * tolerance
    var (n1, d1) = (BigInt(1), BigInt(0))
    var (n2, d2) = (BigInt(0), BigInt(1))
    repeat {
      guard y.isFinite else {
        return nil
      }
      let fy = BigInt(Foundation.floor(y))
      (n1, n2) = (fy * n1 + n2, n1)
      (d1, d2) = (fy * d1 + d2, d1)
      y = 1.0 / (y - Foundation.floor(y))
    } while abs(abs(x) - n1.doubleValue / d1.doubleValue) > mx
    if x < 0.0 {
      return Rational(n1.negate, d1)
    } else {
      return Rational(n1, d1)
    }
  }
  
  
  //-------- MARK: - Rounding primitives

  private func floor(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_):
        return expr
      case .rational(.fixnum(let n), .fixnum(let d)):
        return .makeNumber(Int64(Foundation.floor(Double(n) / Double(d))))
      case .rational(.bignum(let n), .bignum(let d)):
        let (quotient, remainder) = n.divided(by: d)
        if remainder.isZero {
          return .makeNumber(quotient)
        } else if quotient.isNegative {
          return .makeNumber(quotient.minus(1))
        } else {
          return .makeNumber(quotient)
        }
      case .flonum(let num):
        return .makeNumber(Foundation.floor(num))
      default:
        throw RuntimeError.type(expr, expected: [.realType])
    }
  }

  private func ceiling(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_):
        return expr
      case .rational(.fixnum(let n), .fixnum(let d)):
        return .makeNumber(Int64(Foundation.ceil(Double(n) / Double(d))))
      case .rational(.bignum(let n), .bignum(let d)):
        let (quotient, remainder) = n.divided(by: d)
        if remainder.isZero || quotient.isNegative {
          return .makeNumber(quotient)
        } else {
          return .makeNumber(quotient.plus(1))
        }
      case .flonum(let num):
        return .makeNumber(ceil(num))
      default:
        throw RuntimeError.type(expr, expected: [.realType])
    }
  }

  private func truncate(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_):
        return expr
      case .rational(.fixnum(let n), .fixnum(let d)):
        return .makeNumber(Int64(Foundation.trunc(Double(n) / Double(d))))
      case .rational(.bignum(let n), .bignum(let d)):
        return .makeNumber(n.divided(by: d).quotient)
      case .flonum(let num):
        return .makeNumber(trunc(num))
      default:
        throw RuntimeError.type(expr, expected: [.realType])
    }
  }

  private func round(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_),
           .bignum(_):
        return expr
      case .rational(.fixnum(let n), .fixnum(let d)):
        return .makeNumber(Int64((Double(n) / Double(d)).rounded(.toNearestOrEven)))
      case .rational(.bignum(let n), .bignum(let d)):
        let (quotient, remainder) = n.divided(by: d)
        if remainder.isZero {
          return .makeNumber(quotient)
        } else {
          let doubleQuotient = remainder.abs.times(2)
          let denom = d.abs
          if doubleQuotient == denom {
            if quotient.isOdd {
              if quotient.isNegative {
                return .makeNumber(quotient.minus(1))
              } else {
                return .makeNumber(quotient.plus(1))
              }
            } else {
              return .makeNumber(quotient)
            }
          } else if doubleQuotient > denom {
            if quotient.isNegative {
              return .makeNumber(quotient.minus(1))
            } else {
              return .makeNumber(quotient.plus(1))
            }
          } else {
            return .makeNumber(quotient)
          }
        }
      case .flonum(let num):
        return .makeNumber(num.rounded(.toNearestOrEven))
      default:
        throw RuntimeError.type(expr, expected: [.realType])
    }
  }
  
  private func random(_ fst: Expr?, _ snd: Expr?) throws -> Expr {
    guard let fst = fst else {
      return .flonum(Double.random(in: 0.0..<1.0))
    }
    guard let snd = snd else {
      switch fst {
        case .fixnum(let max):
          guard max > 0 else {
            throw RuntimeError.eval(.firstArgOfProcViolation,
                                    fst,
                                    .makeString("random"),
                                    .makeString("> 0"))
          }
          return .fixnum(Int64.random(in: 0..<max))
        case .bignum(let max):
          guard max > 0 else {
            throw RuntimeError.eval(.firstArgOfProcViolation,
                                    fst,
                                    .makeString("random"),
                                    .makeString("> 0"))
          }
          return .makeNumber(BigInt.random(below: max))
        default:
          let max = try fst.asDouble(coerce: true)
          guard max > 0.0 else {
            throw RuntimeError.eval(.firstArgOfProcViolation,
                                    fst,
                                    .makeString("random"),
                                    .makeString("> 0.0"))
          }
          return .makeNumber(Double.random(in: 0.0..<max))
      }
    }
    switch (fst, snd) {
      case (.fixnum(let min), .fixnum(let max)):
        guard max > min else {
          throw RuntimeError.eval(.secondArgOfProcViolation,
                                  snd,
                                  .makeString("random"),
                                  .makeString("> \(min)"))
        }
        return .fixnum(Int64.random(in: min..<max))
      case (.fixnum(let m), .bignum(let max)):
        let min = BigInt(m)
        guard max > min else {
          throw RuntimeError.eval(.secondArgOfProcViolation,
                                  snd,
                                  .makeString("random"),
                                  .makeString("> \(min)"))
        }
        return .makeNumber(BigInt.random(below: max - min) + min)
      case (.bignum(let min), .fixnum(let m)):
        let max = BigInt(m)
        guard max > min else {
          throw RuntimeError.eval(.secondArgOfProcViolation,
                                  snd,
                                  .makeString("random"),
                                  .makeString("> \(min)"))
        }
        return .makeNumber(BigInt.random(below: max - min) + min)
      case (.bignum(let min), .bignum(let max)):
        guard max > min else {
          throw RuntimeError.eval(.secondArgOfProcViolation,
                                  snd,
                                  .makeString("random"),
                                  .makeString("> \(min)"))
        }
        return .makeNumber(BigInt.random(below: max - min) + min)
      default:
        let min = try fst.asDouble(coerce: true)
        let max = try snd.asDouble(coerce: true)
        guard max > min else {
          throw RuntimeError.eval(.secondArgOfProcViolation,
                                  snd,
                                  .makeString("random"),
                                  .makeString("> \(fst)"))
        }
        return .makeNumber(Double.random(in: min..<max))
    }
  }

  //-------- MARK: - Arithmetic primitives

  private func plus(_ exprs: Arguments) throws -> Expr {
    var acc = Expr.fixnum(0)
    for expr in exprs {
      switch try NumberPair(acc, expr) {
        case .fixnumPair(let lhs, let rhs):
          let (res, overflow) = lhs.addingReportingOverflow(rhs)
          acc = overflow ? .makeNumber(BigInt(lhs) + BigInt(rhs)) : .makeNumber(res)
        case .bignumPair(let lhs, let rhs):
          acc = .makeNumber(lhs + rhs)
        case .rationalPair(let lhs, let rhs):
          let (res, overflow) = lhs.addingReportingOverflow(rhs)
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

  private func minus(_ first: Expr, _ exprs: Arguments) throws -> Expr {
    var acc = first.normalized
    if exprs.isEmpty {
      switch acc {
        case .fixnum(let res):
          return .makeNumber(-res)
        case .bignum(let res):
          return .makeNumber(res.negate)
        case .rational(.fixnum(let n), let d):
          return .rational(.fixnum(-n), d)
        case .rational(.bignum(let n), let d):
          return .rational(.bignum(-n), d)
        case .flonum(let res):
          if res.isNaN {
            return .makeNumber(res.sign == .minus ? Double.nan : Scanner.negativeNaN)
          } else {
            return .makeNumber(-res)
          }
        case .complex(let res):
          return .makeNumber(res.value.negate)
        default:
          throw RuntimeError.type(first, expected: [.numberType])
      }
    }
    for expr in exprs {
      switch try NumberPair(acc, expr) {
        case .fixnumPair(let lhs, let rhs):
          let (res, overflow) = lhs.subtractingReportingOverflow(rhs)
          acc = overflow ? .makeNumber(BigInt(lhs) - BigInt(rhs)) : .makeNumber(res)
        case .bignumPair(let lhs, let rhs):
          acc = .makeNumber(lhs - rhs)
        case .rationalPair(let lhs, let rhs):
          let (res, overflow) = lhs.subtractingReportingOverflow(rhs)
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

  private func mult(_ exprs: Arguments) throws -> Expr {
    var acc = Expr.fixnum(1)
    for expr in exprs {
      switch try NumberPair(acc, expr) {
        case .fixnumPair(let lhs, let rhs):
          let (res, overflow) = lhs.multipliedReportingOverflow(by: rhs)
          acc = overflow ? .makeNumber(BigInt(lhs) * BigInt(rhs)) : .makeNumber(res)
        case .bignumPair(let lhs, let rhs):
          acc = .makeNumber(lhs * rhs)
        case .rationalPair(let lhs, let rhs):
          let (res, overflow) = lhs.multipliedReportingOverflow(by: rhs)
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

  private func div(_ first: Expr, _ exprs: Arguments) throws -> Expr {
    var acc = first.normalized
    if exprs.isEmpty {
      switch acc {
        case .fixnum(let res):
          guard res != 0 else {
            throw RuntimeError.eval(.divisionByZero)
          }
          return .makeNumber(Rational(1, res))
        case .bignum(let res):
          guard !res.isZero else {
            throw RuntimeError.eval(.divisionByZero)
          }
          return .makeNumber(Rational(BigInt(1), res))
        case .rational(.fixnum(let n), .fixnum(let d)):
          guard n != 0 else {
            throw RuntimeError.eval(.divisionByZero)
          }
          return .makeNumber(Rational(d, n))
        case .rational(.bignum(let n), .bignum(let d)):
          guard !n.isZero else {
            throw RuntimeError.eval(.divisionByZero)
          }
          return .makeNumber(Rational(d, n))
        case .flonum(let res):
          return .makeNumber(1.0 / res)
        case .complex(let res):
          return .makeNumber(1.0 / res.value)
        default:
          throw RuntimeError.type(first, expected: [.numberType])
      }
    }
    for expr in exprs {
      switch try NumberPair(acc, expr) {
        case .fixnumPair(let lhs, let rhs):
          guard rhs != 0 else {
            throw RuntimeError.eval(.divisionByZero)
          }
          let (res, overflow) = Rational.rationalWithOverflow(lhs, rhs)
          acc = overflow ? .makeNumber(Rational(BigInt(lhs), BigInt(rhs))) : .makeNumber(res)
        case .bignumPair(let lhs, let rhs):
          acc = .makeNumber(Rational(lhs, rhs))
        case .rationalPair(let lhs, let rhs):
          let (res, overflow) = lhs.dividedReportingOverflow(by: rhs)
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
  
  private func equals(_ first: Expr, _ exprs: Arguments) throws -> Expr {
    try first.assertType(.numberType)
    var last = first
    for expr in exprs {
      guard try compareNumber(last, with: expr) == .equal else {
        return Expr.false
      }
      last = expr
    }
    return Expr.true
  }

  private func lessThan(_ first: Expr, _ exprs: Arguments) throws -> Expr {
    try first.assertType(.numberType)
    var last = first
    for expr in exprs {
      guard try compareNumber(last, with: expr) == .less else {
        return Expr.false
      }
      last = expr
    }
    return Expr.true
  }

  private func lessThanEquals(_ first: Expr, _ exprs: Arguments) throws -> Expr {
    try first.assertType(.numberType)
    var last = first
    for expr in exprs {
      let res = try compareNumber(last, with: expr)
      guard res == .less || res == .equal else {
        return Expr.false
      }
      last = expr
    }
    return Expr.true
  }

  private func biggerThan(_ first: Expr, _ exprs: Arguments) throws -> Expr {
    try first.assertType(.numberType)
    var last = first
    for expr in exprs {
      guard try compareNumber(last, with: expr) == .greater else {
        return Expr.false
      }
      last = expr
    }
    return Expr.true
  }

  private func biggerThanEquals(_ first: Expr, _ exprs: Arguments) throws -> Expr {
    try first.assertType(.numberType)
    var last = first
    for expr in exprs {
      let res = try compareNumber(last, with: expr)
      guard res == .greater || res == .equal else {
        return Expr.false
      }
      last = expr
    }
    return Expr.true
  }

  private func max(_ first: Expr, _ exprs: Arguments) throws -> Expr {
    try first.assertType(.numberType)
    var res = first
    var inexact = first.isInexactNumber
    for expr in exprs {
      inexact = inexact || expr.isInexactNumber
      if try compareNumber(res, with: expr) == .less {
        res = expr
      }
    }
    return inexact ? try self.inexact(res) : res
  }

  private func min(_ first: Expr, _ exprs: Arguments) throws -> Expr {
    try first.assertType(.numberType)
    var res = first
    var inexact = first.isInexactNumber
    for expr in exprs {
      inexact = inexact || expr.isInexactNumber
      if try compareNumber(res, with: expr) == .greater {
        res = expr
      }
    }
    return inexact ? try self.inexact(res) : res
  }


  //-------- MARK: - Numeric functions

  private func absolute(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(let num):
        return .makeNumber(abs(num))
      case .bignum(let num):
        return .makeNumber(num.isNegative ? -num : num)
      case .rational(.fixnum(let n), .fixnum(let d)):
        return n < 0 ? .rational(.fixnum(-n), .fixnum(d)) : expr
      case .rational(.bignum(let n), .bignum(let d)):
        return n.isNegative ? .rational(.bignum(-n), .bignum(d)) : expr
      case .flonum(let num):
        return .flonum(num.magnitude)
      default:
        throw RuntimeError.type(expr, expected: [.realType])
    }
  }
  
  private func square(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(let num):
        let (res, overflow) = num.multipliedReportingOverflow(by: num)
        if overflow {
          let n = BigInt(num)
          return .makeNumber(n * n)
        } else {
          return .makeNumber(res)
        }
      case .bignum(let num):
        return .makeNumber(num * num)
      case .rational(.fixnum(let n), .fixnum(let d)):
        let num = Rational(n, d)
        let (res, overflow) = num.multipliedReportingOverflow(by: num)
        return overflow ? .makeNumber(Rational(BigInt(n), BigInt(d)) *
                                      Rational(BigInt(n), BigInt(d)))
                        : .makeNumber(res)
      case .rational(.bignum(let n), .bignum(let d)):
        let num = Rational(n, d)
        return .makeNumber(num * num)
      case .flonum(let num):
        return .makeNumber(num * num)
      case .complex(let  num):
        return .makeNumber(num.value * num.value)
      default:
        throw RuntimeError.type(expr, expected: [.numberType])
    }
  }

  private func sqrt(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_), .rational(_, _), .flonum(_):
        let dbl = try expr.asDouble(coerce: true)
        let res = Foundation.sqrt(dbl)
        return res.isNaN ? .makeNumber(Complex(dbl).sqrt) : .makeNumber(res)
      case .complex(let num):
        return .makeNumber(num.value.sqrt)
      default:
        throw RuntimeError.type(expr, expected: [.numberType])
    }
  }
  
  private func exactIntegerSqrt(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(let x):
        let res = Foundation.sqrt(Double(x))
        if res.isNaN {
          let x = Complex(try expr.asDouble(coerce: true))
          let sr = x.sqrt
          return .values(.pair(.makeNumber(sr), .pair(.makeNumber(x - sr * sr), .null)))
        } else {
          let sr = Int64(res)
          return .values(.pair(.fixnum(sr), .pair(.fixnum(x - sr * sr), .null)))
        }
      case .bignum(let x):
        if x.isNegative {
          let x = Complex(try expr.asDouble(coerce: true))
          let sr = x.sqrt
          return .values(.pair(.makeNumber(sr), .pair(.makeNumber(x - sr * sr), .null)))
        } else {
          let sr = x.sqrt
          return .values(.pair(.makeNumber(sr), .pair(.makeNumber(x - sr * sr), .null)))
        }
      default:
        throw RuntimeError.type(expr, expected: [.exactIntegerType])
    }
  }
  
  private func expt(_ expr: Expr, _ exp: Expr) throws -> Expr {
    switch try NumberPair(expr, exp) {
      case .fixnumPair(let x, let y):
        if y == 0 {
          return .fixnum(1)
        } else if y >= 0 {
          return .makeNumber(BigInt(x) ** BigInt(y))
        } else {
          return .makeNumber(Rational(BigInt.one, BigInt(x) ** BigInt(y).negate))
        }
      case .bignumPair(let x, let y):
        if y.isZero {
          return .fixnum(1)
        } else if y.isNegative {
          return .makeNumber(Rational(BigInt.one, x ** y.negate))
        } else {
          return .makeNumber(x ** y)
        }
      case .rationalPair(let x, let y):
        if y.isZero {
          return .fixnum(1)
        } else if y.denominator == 1 {
          return .makeNumber(x.toPower(of: y.numerator))
        } else {
          return .makeNumber(Foundation.pow(x.doubleValue, y.doubleValue))
        }
      case .bigRationalPair(let x, let y):
        if y.isZero {
          return .fixnum(1)
        } else if y.denominator.isOne {
          return .makeNumber(x.toPower(of: y.numerator))
        } else {
          return .makeNumber(Foundation.pow(x.doubleValue, y.doubleValue))
        }
      case .flonumPair(let x, let y):
        return .makeNumber(Foundation.pow(x, y))
      case .complexPair(let x, let y):
        return .makeNumber((y * x.log).exp)
    }
  }

  private func exp(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_), .rational(_, _), .flonum(_):
        let dbl = try expr.asDouble(coerce: true)
        let res = Foundation.exp(dbl)
        return res.isNaN ? .makeNumber(Complex(dbl).exp) : .makeNumber(res)
      case .complex(let num):
        return .makeNumber(num.value.exp)
      default:
        throw RuntimeError.type(expr, expected: [.numberType])
    }
  }
  
  private func logNat(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_), .rational(_, _), .flonum(_):
        let dbl = try expr.asDouble(coerce: true)
        let res = Foundation.log(dbl)
        return res.isNaN ? .makeNumber(Complex(dbl).log) : .makeNumber(res)
      case .complex(let num):
        return .makeNumber(num.value.log)
      default:
        throw RuntimeError.type(expr, expected: [.numberType])
    }
  }

  private func log(_ expr: Expr, base: Expr?) throws -> Expr {
    let numerator = try self.logNat(expr)
    guard let base = base else {
      return numerator
    }
    let denominator = try self.logNat(base)
    return try self.div(numerator, [denominator])
  }
  
  private func sin(_ expr: Expr) throws -> Expr {
    return .makeNumber(Foundation.sin(try expr.asDouble(coerce: true)))
  }

  private func cos(_ expr: Expr) throws -> Expr {
    return .makeNumber(Foundation.cos(try expr.asDouble(coerce: true)))
  }

  private func tan(_ expr: Expr) throws -> Expr {
    return .makeNumber(Foundation.tan(try expr.asDouble(coerce: true)))
  }

  private func asin(_ expr: Expr) throws -> Expr {
    return .makeNumber(Foundation.asin(try expr.asDouble(coerce: true)))
  }

  private func acos(_ expr: Expr) throws -> Expr {
    return .makeNumber(Foundation.acos(try expr.asDouble(coerce: true)))
  }

  private func atan(_ fst: Expr, _ snd: Expr?) throws -> Expr {
    let y = try fst.asDouble(coerce: true)
    if let snd = snd {
      return .makeNumber(Foundation.atan2(y, try snd.asDouble(coerce: true)))
    } else {
      return .makeNumber(Foundation.atan(y))
    }
  }
  
  private func numberToString(_ expr: Expr, _ args: Arguments) throws -> Expr {
    guard let (rad, len, prec, noexp) = args.optional(.fixnum(10),
                                                      .fixnum(1),
                                                      .false,
                                                      .false) else {
      throw RuntimeError.argumentCount(of: "number->string",
                                       min: 1,
                                       max: 5,
                                       args: .pair(expr, .makeList(args)))
    }
    let radix = try rad.asInt()
    guard radix == 2 || radix == 8 || radix == 10 || radix == 16 else {
      throw RuntimeError.eval(.illegalRadix, rad)
    }
    let l = try len.asInt(above: Int.min, below: Int.max)
    let align = l < 0 ? "-" : ""
    let pre = prec.isFalse ? 16 : try prec.asInt(above: 0, below: 100)
    var res: NSMutableString
    switch expr {
      case .fixnum(let num):
        res = NSMutableString(string: String(num, radix: radix))
      case .bignum(let num):
        res = NSMutableString(string: num.toString(base: BigInt.base(of: radix)))
      case .rational(.fixnum(let n), .fixnum(let d)):
        res = NSMutableString(string: String(n, radix: radix) + "/" + String(d, radix: radix))
      case .rational(.bignum(let n), .bignum(let d)):
        res = NSMutableString(string: n.toString(base: BigInt.base(of: radix)) + "/" +
                                      d.toString(base: BigInt.base(of: radix)))
      case .flonum(let num):
        guard radix == 10 else {
          throw RuntimeError.eval(.illegalRadix, rad)
        }
        if prec.isFalse {
          res = NSMutableString(string: String(num))
        } else {
          let fstr = "%\(align)\(l == 0 ? "" : String(abs(l))).\(pre)\(noexp.isTrue ? "f" : "g")"
          res = NSMutableString(string: String(format: fstr, num))
          return .string(res)
        }
      case .complex(let num):
        guard radix == 10 else {
          throw RuntimeError.eval(.illegalRadix, rad)
        }
        if prec.isFalse {
          res = num.value.re.isZero ?
            NSMutableString() : NSMutableString(string: String(num.value.re) +
                                                        (num.value.im >= 0.0 ? "+" : ""))
          res.append(String(num.value.im) + "i")
        } else {
          let fstr = "%1.\(pre)\(noexp.isTrue ? "f" : "g")"
          res = num.value.re.isZero ?
            NSMutableString() :
            NSMutableString(string: String(format: fstr + (num.value.im >= 0.0 ? "+" : ""),
                                           num.value.re))
          res.appendFormat((fstr + "i") as NSString, num.value.im)
        }
      default:
        throw RuntimeError.type(expr, expected: [.numberType])
    }
    if res.length < abs(l) {
      if l < 0 {
        res.append(String(repeating: " ", count: abs(l) - res.length))
      } else {
        res.insert(String(repeating: " ", count: abs(l) - res.length), at: 0)
      }
    }
    return .string(res)
  }
  
  private func stringToNumber(_ expr: Expr, _ rad: Expr?) throws -> Expr {
    var radix = 10
    if let base = try rad?.asInt() {
      if base == 2 || base == 8 || base == 10 || base == 16 {
        radix = base
      } else {
        throw RuntimeError.eval(.illegalRadix, rad!)
      }
    }
    let input = TextInput(string: try expr.asString(),
                          abortionCallback: self.context.evaluator.isAbortionRequested)
    let scanner = Scanner(input: input, prescan: false)
    scanner.skipSpace()
    guard scanner.ch != EOF_CH else {
      throw RuntimeError.type(expr, expected: [.numberType])
    }
    scanner.scanSignedNumber(radix)
    let token = scanner.token
    scanner.skipSpace()
    guard scanner.ch == EOF_CH else {
      return .false
    }
    switch token.kind {
      case .int:
        return .fixnum(token.intVal)
      case .bigint:
        return .bignum(token.bigIntVal)
      case .rat:
        return .rational(.fixnum(token.ratVal.numerator), .fixnum(token.ratVal.denominator))
      case .bigrat:
        return .rational(.bignum(token.bigRatVal.numerator), .bignum(token.bigRatVal.denominator))
      case .float:
        return .flonum(token.floatVal)
      case .complex:
        return .complex(ImmutableBox(token.complexVal))
      default:
        return .false
    }
  }

  private static func findBestRat(_ t: Double, _ l: Int64) -> (Double, Int64, Int64) {
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
  
  private func approximate(_ x: Expr, delta: Expr) throws -> Expr {
    var l_curr: Int64 = 1
    let t = try x.asDouble(coerce: true)
    let err = try delta.asDouble(coerce: true)
    var (actual, n, d) = MathLibrary.findBestRat(t, l_curr)
    while abs(actual) > err && l_curr < (Int64.max / 1000) {
      l_curr *= 10
      (actual, n, d) = MathLibrary.findBestRat(t, l_curr)
    }
    (actual, n, d) = MathLibrary.findBestRat(t, l_curr)
    let res = Rational(n, d)
    if res.denominator == 1 {
      return .fixnum(res.numerator)
    } else {
      return .rational(.fixnum(res.numerator), .fixnum(res.denominator))
    }
  }
  
  private func makeRectangular(_ re: Expr, _ imag: Expr) throws -> Expr {
    return .complex(ImmutableBox(Complex(try re.asDouble(coerce: true),
                                            try imag.asDouble(coerce: true))))
  }
  
  private func makePolar(_ abs: Expr, _ arg: Expr) throws -> Expr {
    return .complex(ImmutableBox(Complex(abs: try abs.asDouble(coerce: true),
                                            arg: try arg.asDouble(coerce: true))))
  }
  
  private func realPart(_ expr: Expr) throws -> Expr {
    return .flonum(try expr.asComplex(coerce: true).re)
  }
  
  private func imagPart(_ expr: Expr) throws -> Expr {
    return .flonum(try expr.asComplex(coerce: true).im)
  }
  
  private func magnitude(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(let num):
        return .makeNumber(abs(num))
      case .bignum(let num):
        return .makeNumber(num.isNegative ? -num : num)
      case .rational(.fixnum(let n), .fixnum(let d)):
        return n < 0 ? .rational(.fixnum(-n), .fixnum(d)) : expr
      case .rational(.bignum(let n), .bignum(let d)):
        return n.isNegative ? .rational(.bignum(-n), .bignum(d)) : expr
      case .flonum(let num):
        return .flonum(num.magnitude)
      default:
        return .flonum(try expr.asComplex(coerce: true).abs)
    }
  }
  
  private func angle(_ expr: Expr) throws -> Expr {
    return .flonum(try expr.asComplex(coerce: true).arg)
  }
  
  private func numerator(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_):
        return expr
      case .rational(let numerator, _):
        return numerator
      case .flonum(let num):
        return try self.inexact(try self.numerator(MathLibrary.approximateNumber(num)))
      default:
        throw RuntimeError.type(expr, expected: [.numberType])
    }
  }
  
  private func denominator(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_), .bignum(_):
        return .fixnum(1)
      case .rational(_, let denominator):
        return denominator
      case .flonum(let num):
        return try self.inexact(try self.denominator(MathLibrary.approximateNumber(num)))
      default:
        throw RuntimeError.type(expr, expected: [.numberType])
    }
  }
  
  private func gcd(_ exprs: Arguments) throws -> Expr {
    var acc = Expr.fixnum(0)
    var inexact = false
    for expr in exprs {
      var e = try self.absolute(expr)
      if case .flonum(let num) = expr {
        inexact = true
        e = MathLibrary.approximateNumber(num)
      }
      switch try NumberPair(acc, e) {
        case .fixnumPair(let lhs, let rhs):
          let (res, overflow) = Rational.gcdWithOverflow(lhs, rhs)
          acc = overflow ? .makeNumber(Rational.gcd(BigInt(lhs), BigInt(rhs))) : .makeNumber(res)
        case .bignumPair(let lhs, let rhs):
          acc = .makeNumber(Rational.gcd(lhs, rhs))
        case .rationalPair(let lhs, let rhs):
          let (res, overflow) = lhs.gcdReportingOverflow(with: rhs)
          acc = overflow ?
              .makeNumber(Rational.gcd(Rational(BigInt(lhs.numerator), BigInt(lhs.denominator)),
                                   Rational(BigInt(rhs.numerator), BigInt(rhs.denominator))))
            : .makeNumber(res)
        case .bigRationalPair(let lhs, let rhs):
          acc = .makeNumber(Rational.gcd(lhs, rhs))
        default:
          throw RuntimeError.type(expr, expected: [.realType])
      }
    }
    return inexact ? try self.inexact(acc) : acc
  }
  
  private func lcm(_ exprs: Arguments) throws -> Expr {
    var acc = Expr.fixnum(1)
    var inexact = false
    for expr in exprs {
      var e = try self.absolute(expr)
      if case .flonum(let num) = expr {
        inexact = true
        e = MathLibrary.approximateNumber(num)
      }
      switch try NumberPair(acc, e) {
        case .fixnumPair(let lhs, let rhs):
          let (res, overflow) = Rational.lcmWithOverflow(lhs, rhs)
          acc = overflow ? .makeNumber(Rational.lcm(BigInt(lhs), BigInt(rhs))) : .makeNumber(res)
        case .bignumPair(let lhs, let rhs):
          acc = .makeNumber(Rational.lcm(lhs, rhs))
        case .rationalPair(let lhs, let rhs):
          let (res, overflow) = lhs.lcmReportingOverflow(with: rhs)
          acc = overflow ?
              .makeNumber(Rational.lcm(Rational(BigInt(lhs.numerator), BigInt(lhs.denominator)),
                                   Rational(BigInt(rhs.numerator), BigInt(rhs.denominator))))
            : .makeNumber(res)
        case .bigRationalPair(let lhs, let rhs):
          acc = .makeNumber(Rational.lcm(lhs, rhs))
        default:
          throw RuntimeError.type(expr, expected: [.realType])
      }
    }
    return inexact ? try self.inexact(acc) : acc
  }
  
  private func truncateDiv(_ x: Expr, _ y: Expr) throws -> Expr {
    switch try NumberPair(x, y) {
      case .fixnumPair(let lhs, let rhs):
        return .values(.pair(.makeNumber(lhs / rhs), .pair(.makeNumber(lhs % rhs), .null)))
      case .bignumPair(let lhs, let rhs):
        return .values(.pair(.makeNumber(lhs / rhs), .pair(.makeNumber(lhs % rhs), .null)))
      case .flonumPair(let lhs, let rhs):
        guard Foundation.trunc(lhs) == lhs else {
          throw RuntimeError.type(x, expected: [.integerType])
        }
        guard Foundation.trunc(rhs) == rhs else {
          throw RuntimeError.type(y, expected: [.integerType])
        }
        return .values(.pair(.makeNumber(Foundation.trunc(lhs / rhs)),
                             .pair(.makeNumber(lhs.truncatingRemainder(dividingBy: rhs)), .null)))
      default:
        try x.assertType(.integerType)
        try y.assertType(.integerType)
        return .false
    }
  }
  
  private func truncateQuotient(_ x: Expr, _ y: Expr) throws -> Expr {
    switch try NumberPair(x, y) {
      case .fixnumPair(let lhs, let rhs):
        return .makeNumber(lhs / rhs)
      case .bignumPair(let lhs, let rhs):
        return .makeNumber(lhs / rhs)
      case .flonumPair(let lhs, let rhs):
        guard Foundation.trunc(lhs) == lhs else {
          throw RuntimeError.type(x, expected: [.integerType])
        }
        guard Foundation.trunc(rhs) == rhs else {
          throw RuntimeError.type(y, expected: [.integerType])
        }
        return .makeNumber(Foundation.trunc(lhs / rhs))
      default:
        try x.assertType(.integerType)
        try y.assertType(.integerType)
        return .false
    }
  }
  
  private func truncateRemainder(_ x: Expr, _ y: Expr) throws -> Expr {
    switch try NumberPair(x, y) {
      case .fixnumPair(let lhs, let rhs):
        return .makeNumber(lhs % rhs)
      case .bignumPair(let lhs, let rhs):
        return .makeNumber(lhs % rhs)
      case .flonumPair(let lhs, let rhs):
        guard Foundation.trunc(lhs) == lhs else {
          throw RuntimeError.type(x, expected: [.integerType])
        }
        guard Foundation.trunc(rhs) == rhs else {
          throw RuntimeError.type(y, expected: [.integerType])
        }
        return .makeNumber(lhs.truncatingRemainder(dividingBy: rhs))
      default:
        try x.assertType(.integerType)
        try y.assertType(.integerType)
        return .false
    }
  }
  
  private func floorDiv(_ x: Expr, _ y: Expr) throws -> Expr {
    switch try NumberPair(x, y) {
      case .fixnumPair(let lhs, let rhs):
        let res = lhs % rhs
        if (res < 0) == (rhs < 0) {
          return .values(.pair(.makeNumber((lhs - res) / rhs), .pair(.makeNumber(res), .null)))
        } else {
          return .values(.pair(.makeNumber((lhs - res - rhs) / rhs),
                               .pair(.makeNumber(res + rhs), .null)))
        }
      case .bignumPair(let lhs, let rhs):
        let res = lhs % rhs
        if res.isNegative == rhs.isNegative {
          return .values(.pair(.makeNumber((lhs - res) / rhs), .pair(.makeNumber(res), .null)))
        } else {
          return .values(.pair(.makeNumber((lhs - res - rhs) / rhs),
                               .pair(.makeNumber(res + rhs), .null)))
        }
      case .flonumPair(let lhs, let rhs):
        guard Foundation.trunc(lhs) == lhs else {
          throw RuntimeError.type(x, expected: [.integerType])
        }
        guard Foundation.trunc(rhs) == rhs else {
          throw RuntimeError.type(y, expected: [.integerType])
        }
        let res = lhs.truncatingRemainder(dividingBy: rhs)
        if (res < 0.0) == (rhs < 0.0) {
          return .values(.pair(.makeNumber((lhs - res) / rhs), .pair(.makeNumber(res), .null)))
        } else {
          return .values(.pair(.makeNumber((lhs - res - rhs) / rhs),
                               .pair(.makeNumber(res + rhs), .null)))
        }
      default:
        try x.assertType(.integerType)
        try y.assertType(.integerType)
        return .false
    }
  }
  
  private func floorQuotient(_ x: Expr, _ y: Expr) throws -> Expr {
    switch try NumberPair(x, y) {
      case .fixnumPair(let lhs, let rhs):
        let res = lhs % rhs
        return .makeNumber(((res < 0) == (rhs < 0) ? (lhs - res) : (lhs - res - rhs)) / rhs)
      case .bignumPair(let lhs, let rhs):
        let res = lhs % rhs
        return .makeNumber((res.isNegative == rhs.isNegative ? (lhs - res)
                                                             : (lhs - res - rhs)) / rhs)
      case .flonumPair(let lhs, let rhs):
        guard Foundation.trunc(lhs) == lhs else {
          throw RuntimeError.type(x, expected: [.integerType])
        }
        guard Foundation.trunc(rhs) == rhs else {
          throw RuntimeError.type(y, expected: [.integerType])
        }
        let res = lhs.truncatingRemainder(dividingBy: rhs)
        if (res < 0.0) == (rhs < 0.0) {
          return .makeNumber((lhs - res) / rhs)
        } else {
          return .makeNumber((lhs - res - rhs) / rhs)
        }
      default:
        try x.assertType(.integerType)
        try y.assertType(.integerType)
        return .false
    }
  }
  
  private func floorRemainder(_ x: Expr, _ y: Expr) throws -> Expr {
    switch try NumberPair(x, y) {
      case .fixnumPair(let lhs, let rhs):
        let res = lhs % rhs
        return .makeNumber((res < 0) == (rhs < 0) ? res : res + rhs)
      case .bignumPair(let lhs, let rhs):
        let res = lhs % rhs
        return .makeNumber(res.isNegative == rhs.isNegative ? res : res + rhs)
      case .flonumPair(let lhs, let rhs):
        guard Foundation.trunc(lhs) == lhs else {
          throw RuntimeError.type(x, expected: [.integerType])
        }
        guard Foundation.trunc(rhs) == rhs else {
          throw RuntimeError.type(y, expected: [.integerType])
        }
        let res = lhs.truncatingRemainder(dividingBy: rhs)
        if (res < 0.0) == (rhs < 0.0) {
          return .makeNumber(res)
        } else {
          return .makeNumber(res + rhs)
        }
      default:
        try x.assertType(.integerType)
        try y.assertType(.integerType)
        return .false
    }
  }
  
  private func compileBinOp(_ compiler: Compiler, expr: Expr, zero: Expr? = nil, env: Env) throws {
    switch expr {
      case .pair(_, .pair(let x, .null)):
        if let zero = zero {
          try compiler.compile(zero, in: env, inTailPos: false)
          try compiler.compile(x, in: env, inTailPos: false)
        } else {
          throw RuntimeError.argumentCount(min: 2, max: 2, expr: expr)
        }
      case .pair(_, .pair(let x, .pair(let y, .null))):
        try compiler.compile(x, in: env, inTailPos: false)
        try compiler.compile(y, in: env, inTailPos: false)
      default:
        throw RuntimeError.argumentCount(min: (zero == nil ? 2 : 1), max: 2, expr: expr)
    }
  }
  
  private func fxPlus(_ x: Expr, _ y: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() &+ y.asInt64())
  }
  
  private func compileFxPlus(_ compiler: Compiler,
                             expr: Expr,
                             env: Env,
                             tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let x, .pair(let y, .null))) = expr else {
      throw RuntimeError.argumentCount(min: 2, max: 2, expr: expr)
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
  
  private func fxMinus(_ x: Expr, _ y: Expr?) throws -> Expr {
    if let y = y {
      return .fixnum(try x.asInt64() &- y.asInt64())
    } else {
      return .fixnum(-(try x.asInt64()))
    }
  }
  
  private func compileFxMinus(_ compiler: Compiler,
                              expr: Expr,
                              env: Env,
                              tail: Bool) throws -> Bool {
    switch expr {
      case .pair(_, .pair(let x, .null)):
        try compiler.compile(.fixnum(0), in: env, inTailPos: false)
        try compiler.compile(x, in: env, inTailPos: false)
        compiler.emit(.fxMinus)
      case .pair(_, .pair(let x, .pair(let y, .null))):
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
      default:
        throw RuntimeError.argumentCount(min: 1, max: 2, expr: expr)
    }
    return false
  }
  
  private func fxMult(_ x: Expr, _ y: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() &* y.asInt64())
  }
  
  private func compileFxMult(_ compiler: Compiler,
                             expr: Expr,
                             env: Env,
                             tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.fxMult)
    return false
  }
  
  private func fxDiv(_ x: Expr, _ y: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() / y.asInt64())
  }
  
  private func compileFxDiv(_ compiler: Compiler,
                            expr: Expr,
                            env: Env,
                            tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.fxDiv)
    return false
  }
  
  private func fx1Plus(_ x: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() &+ 1)
  }
  
  private func compileFx1Plus(_ compiler: Compiler,
                              expr: Expr,
                              env: Env,
                              tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let x, .null)) = expr else {
      throw RuntimeError.argumentCount(min: 1, max: 1, expr: expr)
    }
    try compiler.compile(x, in: env, inTailPos: false)
    compiler.emit(.fxInc)
    return false
  }
  
  private func fx1Minus(_ x: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() &- 1)
  }
  
  private func compileFx1Minus(_ compiler: Compiler,
                               expr: Expr,
                               env: Env,
                               tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let x, .null)) = expr else {
      throw RuntimeError.argumentCount(min: 1, max: 1, expr: expr)
    }
    try compiler.compile(x, in: env, inTailPos: false)
    compiler.emit(.fxDec)
    return false
  }
  
  private func fxIsZero(_ x: Expr) throws -> Expr {
    return .makeBoolean(try x.asInt64() == 0)
  }
  
  private func compileFxIsZero(_ compiler: Compiler,
                               expr: Expr,
                               env: Env,
                               tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let x, .null)) = expr else {
      throw RuntimeError.argumentCount(min: 1, max: 1, expr: expr)
    }
    try compiler.compile(x, in: env, inTailPos: false)
    compiler.emit(.fxIsZero)
    return false
  }
  
  private func fxIsPositive(_ x: Expr) throws -> Expr {
    return .makeBoolean(try x.asInt64() > 0)
  }
  
  private func fxIsNegative(_ x: Expr) throws -> Expr {
    return .makeBoolean(try x.asInt64() < 0)
  }
  
  private func fxIsOdd(_ x: Expr) throws -> Expr {
    return .makeBoolean(try x.asInt64().isOdd)
  }
  
  private func fxIsEven(_ x: Expr) throws -> Expr {
    return .makeBoolean(try !x.asInt64().isOdd)
  }
  
  private func fxEq(_ x: Expr, _ y: Expr) throws -> Expr {
    return .makeBoolean(try x.asInt64() == y.asInt64())
  }
  
  private func compileFxEq(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let x, .pair(let y, .null))) = expr else {
      throw RuntimeError.argumentCount(min: 2, max: 2, expr: expr)
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
  
  private func fxLt(_ x: Expr, _ y: Expr) throws -> Expr {
    return .makeBoolean(try x.asInt64() < y.asInt64())
  }
  
  private func compileFxLt(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.fxLt)
    return false
  }
  
  private func fxGt(_ x: Expr, _ y: Expr) throws -> Expr {
    return .makeBoolean(try x.asInt64() > y.asInt64())
  }
  
  private func compileFxGt(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.fxGt)
    return false
  }
  
  private func fxLtEq(_ x: Expr, _ y: Expr) throws -> Expr {
    return .makeBoolean(try x.asInt64() <= y.asInt64())
  }
  
  private func compileFxLtEq(_ compiler: Compiler,
                             expr: Expr,
                             env: Env,
                             tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.fxLtEq)
    return false
  }

  private func fxGtEq(_ x: Expr, _ y: Expr) throws -> Expr {
    return .makeBoolean(try x.asInt64() >= y.asInt64())
  }
  
  private func compileFxGtEq(_ compiler: Compiler,
                             expr: Expr,
                             env: Env,
                             tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.fxGtEq)
    return false
  }
  
  private func fxRemainder(_ x: Expr, _ y: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() % y.asInt64())
  }
  
  private func fxModulo(_ x: Expr, _ y: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() %% y.asInt64())
  }
  
  private func fxAbs(_ x: Expr) throws -> Expr {
    let res = try x.asInt64()
    return .fixnum(res < 0 ? -res : res)
  }
  
  private func fxAnd(_ x: Expr, _ y: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() & y.asInt64())
  }
  
  private func fxIor(_ x: Expr, _ y: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() | y.asInt64())
  }
  
  private func fxXor(_ x: Expr, _ y: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() ^ y.asInt64())
  }
  
  private func fxIf(_ x: Expr, _ y: Expr, _ z: Expr) throws -> Expr {
    let v = try x.asInt64()
    return .fixnum((try v & y.asInt64()) | (try ~v & z.asInt64()))
  }
  
  private func fxNot(_ x: Expr) throws -> Expr {
    return .fixnum(try ~x.asInt64())
  }
  
  private func fxLshift(_ x: Expr, _ y: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() << Int64(y.asInt(below: Int64.bitWidth)))
  }
  
  private func fxRshift(_ x: Expr, _ y: Expr) throws -> Expr {
    return .fixnum(try x.asInt64() >> Int64(y.asInt(below: Int64.bitWidth)))
  }
  
  private func fxLrshift(_ x: Expr, _ y: Expr) throws -> Expr {
    return .fixnum(
      Int64(bitPattern: UInt64(bitPattern: try x.asInt64()) >>
                        UInt64(try y.asInt(below: Int64.bitWidth))))
  }
  
  private func fxShift(_ x: Expr, _ y: Expr) throws -> Expr {
    let n = try y.asInt64()
    guard n > -Int64(Int64.bitWidth) && n < Int64(Int64.bitWidth) else {
      throw RuntimeError.range(parameter: 2,
                               of: "fxarithmetic-shift",
                               y,
                               min: -Int64(Int64.bitWidth),
                               max: Int64(Int64.bitWidth))
    }
    if n < 0 {
      return .fixnum(try x.asInt64() >> -n)
    } else {
      return .fixnum(try x.asInt64() << n)
    }
  }
  
  private func fxBitCount(_ x: Expr) throws -> Expr {
    let v = try x.asInt64()
    if v < 0 {
      return .fixnum(~Int64(bitcount(~v)))
    } else {
      return .fixnum(Int64(bitcount(v)))
    }
  }
  
  private func fxLength(_ x: Expr) throws -> Expr {
    let v = try x.asInt64()
    var len = 0
    var bits = v < 0 ? ~v : v
    while bits != 0 {
      bits >>= 1
      len += 1
    }
    return .fixnum(Int64(len))
  }
  
  private func fxFirstBitSet(_ x: Expr) throws -> Expr {
    let v = try x.asInt64()
    return v == 0 ? .fixnum(-1) : .fixnum(Int64(v.trailingZeroBitCount))
  }
  
  private func fxIsBitSet(_ x: Expr, _ y: Expr) throws -> Expr {
    return .makeBoolean((try (x.asInt64() >> y.asInt(below: Int64.bitWidth)) & 1) == 1)
  }
  
  private func fxCopyBit(_ x: Expr, _ y: Expr, _ z: Expr) throws -> Expr {
    let v = try x.asInt64()
    let pos = try y.asInt(below: Int64.bitWidth)
    if try z.asInt(below: 2) == 0 {
      return .fixnum(v & ~(1 << pos))
    } else {
      return .fixnum(v | (1 << pos))
    }
  }
  
  private func fxMin(_ x: Expr, _ args: Arguments) throws -> Expr {
    var res = try x.asInt64()
    for arg in args {
      let y = try arg.asInt64()
      if y < res {
        res = y
      }
    }
    return .fixnum(res)
  }
  
  private func fxMax(_ x: Expr, _ args: Arguments) throws -> Expr {
    var res = try x.asInt64()
    for arg in args {
      let y = try arg.asInt64()
      if y > res {
        res = y
      }
    }
    return .fixnum(res)
  }
  
  private func fxRandom(_ fst: Expr?, _ snd: Expr?) throws -> Expr {
    if fst == nil {
      return .fixnum(Int64.random(in: Int64.min...Int64.max))
    } else if snd == nil {
      let max = try fst!.asInt64()
      guard max > 0 else {
        throw RuntimeError.eval(.firstArgOfProcViolation,
                                fst!,
                                .makeString("rxrandom"),
                                .makeString("> 0"))
      }
      return .fixnum(Int64.random(in: 0..<max))
    } else {
      let min = try fst!.asInt64()
      let max = try snd!.asInt64()
      guard max > min else {
        throw RuntimeError.eval(.secondArgOfProcViolation,
                                snd!,
                                .makeString("rxrandom"),
                                .makeString("> \(min)"))
      }
      return .fixnum(Int64.random(in: min..<max))
    }
  }
  
  private func fxSqrt(_ expr: Expr) throws -> Expr {
    let n = try expr.asInt64()
    guard n < 3037000499 * 3037000499 else {
      return .makeNumber(3037000499)
    }
    var res = Int64(Double(n).squareRoot())
    if res * res > n {
      res -= 1
    } else if (res + 1) * (res + 1) <= n {
      res += 1
    }
    return .makeNumber(res)
  }
  
  private static let maxFx = BigInt(Int64.max) + 1
  
  private func integerToFx(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(_):
        return expr
      case .bignum(let num):
        return .fixnum((num % MathLibrary.maxFx).intValue!)
      default:
        throw RuntimeError.type(expr, expected: [.exactIntegerType])
    }
  }
  
  private func fixnumWidth() -> Expr {
    return .fixnum(Int64(Int64.bitWidth))
  }
  
  private func leastFixnum() -> Expr {
    return .fixnum(Int64.min)
  }
  
  private func greatestFixnum() -> Expr {
    return .fixnum(Int64.max)
  }

  private func realToFlonum(_ expr: Expr) throws -> Expr {
    switch expr {
      case .fixnum(let num):
        return .makeNumber(Double(num))
      case .bignum(let num):
        return .makeNumber(num.doubleValue)
      case .rational(.fixnum(let n), .fixnum(let d)):
        return .makeNumber(Double(n) / Double(d))
      case .rational(.bignum(let n), .bignum(let d)):
        return .makeNumber(n.doubleValue / d.doubleValue)
      case .flonum(_):
        return expr
      default:
        throw RuntimeError.type(expr, expected: [.realType])
    }
  }
  
  private func makeFlonum(_ significand: Expr, _ exponent: Expr) throws -> Expr {
    return .flonum(Double(sign: .plus,
                          exponent: try exponent.asInt(above: -100000000, below: 100000000),
                          significand: try significand.asDouble()))
  }
  
  private func flExponent(_ expr: Expr) throws -> Expr {
    return .makeNumber(try expr.asDouble().exponent)
  }
  
  private func flSignificand(_ expr: Expr) throws -> Expr {
    return .flonum(try expr.asDouble().significand)
  }
  
  private func flPlus(_ x: Expr, _ y: Expr) throws -> Expr {
    return .flonum(try x.asDouble() + y.asDouble())
  }
  
  private func compileFlPlus(_ compiler: Compiler,
                             expr: Expr,
                             env: Env,
                             tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.flPlus)
    return false
  }
  
  private func flMinus(_ x: Expr, _ y: Expr?) throws -> Expr {
    if let y = y {
      return .flonum(try x.asDouble() - y.asDouble())
    } else {
      return .flonum(-(try x.asDouble()))
    }
  }
  
  private func compileFlMinus(_ compiler: Compiler,
                              expr: Expr,
                              env: Env,
                              tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, zero: .flonum(0.0), env: env)
    compiler.emit(.flMinus)
    return false
  }

  private func flMult(_ x: Expr, _ y: Expr) throws -> Expr {
    return .flonum(try x.asDouble() * y.asDouble())
  }
  
  private func compileFlMult(_ compiler: Compiler,
                             expr: Expr,
                             env: Env,
                             tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.flMult)
    return false
  }
  
  private func flDiv(_ x: Expr, _ y: Expr?) throws -> Expr {
    if let y = y {
      return .flonum(try x.asDouble() / y.asDouble())
    } else {
      return .flonum(1 / (try x.asDouble()))
    }
  }
  
  private func compileFlDiv(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, zero: .flonum(1.0), env: env)
    compiler.emit(.flDiv)
    return false
  }
  
  private func flIsZero(_ x: Expr) throws -> Expr {
    return .makeBoolean(try x.asDouble() == 0.0)
  }
  
  private func flIsPositive(_ x: Expr) throws -> Expr {
    return .makeBoolean(try x.asDouble() > 0.0)
  }
  
  private func flIsNegative(_ x: Expr) throws -> Expr {
    return .makeBoolean(try x.asDouble() < 0.0)
  }
  
  private func flEq(_ x: Expr, _ y: Expr) throws -> Expr {
    return .makeBoolean(try x.asDouble().isEqual(to: y.asDouble()))
  }
  
  private func compileFlEq(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.flEq)
    return false
  }
  
  private func flLt(_ x: Expr, _ y: Expr) throws -> Expr {
    return .makeBoolean(try x.asDouble() < y.asDouble())
  }
  
  private func compileFlLt(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.flLt)
    return false
  }
  
  private func flGt(_ x: Expr, _ y: Expr) throws -> Expr {
    return .makeBoolean(try x.asDouble() > y.asDouble())
  }
  
  private func compileFlGt(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.flGt)
    return false
  }
  
  private func flLtEq(_ x: Expr, _ y: Expr) throws -> Expr {
    return .makeBoolean(try x.asDouble() <= y.asDouble())
  }
  
  private func compileFlLtEq(_ compiler: Compiler,
                             expr: Expr,
                             env: Env,
                             tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.flLtEq)
    return false
  }

  private func flGtEq(_ x: Expr, _ y: Expr) throws -> Expr {
    return .makeBoolean(try x.asDouble() >= y.asDouble())
  }
  
  private func compileFlGtEq(_ compiler: Compiler,
                             expr: Expr,
                             env: Env,
                             tail: Bool) throws -> Bool {
    try self.compileBinOp(compiler, expr: expr, env: env)
    compiler.emit(.flLtEq)
    return false
  }
  
  private func flAbs(_ x: Expr) throws -> Expr {
    return .flonum(try x.asDouble().magnitude)
  }
  
  private func flMin(_ x: Expr, _ args: Arguments) throws -> Expr {
    var res = try x.asDouble()
    for arg in args {
      res = Double.minimum(res, try arg.asDouble())
    }
    return .flonum(res)
  }
  
  private func flMax(_ x: Expr, _ args: Arguments) throws -> Expr {
    var res = try x.asDouble()
    for arg in args {
      res = Double.maximum(res, try arg.asDouble())
    }
    return .flonum(res)
  }
  
  private func flRandom(_ fst: Expr?, _ snd: Expr?) throws -> Expr {
    if let max = try fst?.asDouble(), snd == nil {
      guard max > 0.0, !max.isNaN, !max.isInfinite else {
        throw RuntimeError.eval(.firstArgOfProcViolation,
                                fst!,
                                .makeString("flrandom"),
                                .makeString("> 0.0"))
      }
      return .flonum(Double.random(in: 0.0..<max))
    } else {
      let min = try fst?.asDouble(coerce: true) ?? 0.0
      let max = try snd?.asDouble(coerce: true) ?? 1.0
      guard !max.isNaN, !min.isNaN, !max.isInfinite, !min.isInfinite, max > min else {
        throw RuntimeError.eval(.secondArgOfProcViolation,
                                .makeNumber(max),
                                .makeString("flrandom"),
                                .makeString("> \(min)"))
      }
      return .flonum(Double.random(in: min..<max))
    }
  }
  
  private func flSqrt(_ x: Expr) throws -> Expr {
    return .flonum(try x.asDouble().squareRoot())
  }
  
  private func flNext(_ x: Expr) throws -> Expr {
    return .flonum(try x.asDouble().nextUp)
  }
  
  private func flPrev(_ x: Expr) throws -> Expr {
    return .flonum(try x.asDouble().nextDown)
  }
  
  private func bitwiseNot(_ num: Expr) throws -> Expr {
    switch num {
      case .fixnum(let x):
        return .fixnum(~x)
      case .bignum(let x):
        return .bignum(x.not)
      default:
        throw RuntimeError.type(num, expected: [.integerType])
    }
  }
  
  private func bitwiseAnd(_ exprs: Arguments) throws -> Expr {
    var acc = Expr.fixnum(-1)
    for expr in exprs {
      switch try NumberPair(acc, expr) {
        case .fixnumPair(let lhs, let rhs):
          acc = .fixnum(lhs & rhs)
        case .bignumPair(let lhs, let rhs):
          acc = .makeNumber(lhs & rhs)
        default:
          throw RuntimeError.type(expr, expected: [.integerType])
      }
    }
    return acc
  }
  
  private func bitwiseIor(_ exprs: Arguments) throws -> Expr {
    var acc = Expr.fixnum(0)
    for expr in exprs {
      switch try NumberPair(acc, expr) {
      case .fixnumPair(let lhs, let rhs):
        acc = .fixnum(lhs | rhs)
      case .bignumPair(let lhs, let rhs):
        acc = .makeNumber(lhs | rhs)
      default:
        throw RuntimeError.type(expr, expected: [.integerType])
      }
    }
    return acc
  }
  
  private func bitwiseXor(_ exprs: Arguments) throws -> Expr {
    var acc = Expr.fixnum(0)
    for expr in exprs {
      switch try NumberPair(acc, expr) {
        case .fixnumPair(let lhs, let rhs):
          acc = .fixnum(lhs ^ rhs)
        case .bignumPair(let lhs, let rhs):
          acc = .makeNumber(lhs ^ rhs)
        default:
          throw RuntimeError.type(expr, expected: [.integerType])
      }
    }
    return acc
  }
  
  private func bitCount(_ num: Expr) throws -> Expr {
    switch num {
      case .fixnum(let x):
        if x < 0 {
          return .fixnum(Int64(bitcount(~x)))
        } else {
          return .fixnum(Int64(bitcount(x)))
        }
      case .bignum(let x):
        if x.isNegative {
          return .fixnum(Int64(x.not.bitCount))
        } else {
          return .fixnum(Int64(x.bitCount))
        }
      default:
        throw RuntimeError.type(num, expected: [.integerType])
    }
  }
  
  private func integerLength(_ num: Expr) throws -> Expr {
    switch num {
      case .fixnum(let x):
        return .fixnum(Int64(UInt64.bitWidth - (x < 0 ? ~x : x).leadingZeroBitCount))
      case .bignum(let x):
        return .fixnum(Int64(x.lastBitSet))
      default:
        throw RuntimeError.type(num, expected: [.integerType])
    }
  }
  
  private func firstBitSet(_ num: Expr) throws -> Expr {
    switch num {
      case .fixnum(let x):
        return x == 0 ? .fixnum(-1) : .fixnum(Int64(x.trailingZeroBitCount))
      case .bignum(let x):
        return .fixnum(Int64(x.firstBitSet))
      default:
        throw RuntimeError.type(num, expected: [.integerType])
    }
  }
  
  private func isBitSet(_ num: Expr, _ n: Expr) throws -> Expr {
    let y = try n.asInt()
    switch num {
      case .fixnum(let x):
        return .makeBoolean(y < 63 ? (x & (1 << y)) != 0 : BigInt(x).isBitSet(y))
      case .bignum(let x):
        return .makeBoolean(x.isBitSet(y))
      default:
        throw RuntimeError.type(num, expected: [.integerType])
    }
  }
  
  private func copyBit(_ num: Expr, _ n: Expr, _ bit: Expr) throws -> Expr {
    let y = try n.asInt()
    let z: Int
    switch bit {
      case .true:
        z = 1
      case .false:
        z = 0
      case .fixnum(let b):
        guard b >= 0 && b < 2 else {
          throw RuntimeError.range(parameter: 3, of: "copy-bit", bit, min: 0, max: 1)
        }
        z = Int(b)
      default:
        throw RuntimeError.type(bit, expected: [.exactIntegerType])
    }
    switch num {
      case .fixnum(let x):
        if y < 63 {
          return .fixnum(z == 0 ? x & ~(1 << y) : x | (1 << y))
        } else {
          return .makeNumber(BigInt(x).set(bit: y, to: z != 0))
        }
      case .bignum(let x):
        return .makeNumber(x.set(bit: y, to: z != 0))
      default:
        throw RuntimeError.type(num, expected: [.integerType])
    }
  }
  
  private func arithmeticShift(_ num: Expr, _ n: Expr) throws -> Expr {
    let y = try n.asInt64()
    guard y >= Int.min && y <= Int.max else {
      throw RuntimeError.range(
        parameter: 2, of: "arithmetic-shift", n, min: Int64(Int.min), max: Int64(Int.max))
    }
    switch num {
      case .fixnum(let x):
        if y <= 0 {
          return .fixnum(x >> (-y))
        } else if x == 0 {
          return .fixnum(0)
        } else if x > 0 {
          if y < (x.leadingZeroBitCount - 1) {
            return .fixnum(x << y)
          } else {
            return .makeNumber(BigInt(x).shift(Int(y)))
          }
        } else {
          if y < ((~x).leadingZeroBitCount - 1) {
            return .fixnum(x << y)
          } else {
            return .makeNumber(BigInt(x).shift(Int(y)))
          }
        }
      case .bignum(let x):
        return .makeNumber(x.shift(Int(y)))
      default:
        throw RuntimeError.type(num, expected: [.integerType])
    }
  }
  
  private func arithmeticShiftLeft(_ num: Expr, _ n: Expr) throws -> Expr {
    let y = try n.asInt()
    switch num {
      case .fixnum(let x):
        if x == 0 {
          return .fixnum(0)
        } else if y == 0 {
          return .fixnum(x)
        } else if x > 0 {
          if y < (x.leadingZeroBitCount - 1) {
            return .fixnum(x << y)
          } else {
            return .makeNumber(BigInt(x).shift(y))
          }
        } else {
          if y < ((~x).leadingZeroBitCount - 1) {
            return .fixnum(x << y)
          } else {
            return .makeNumber(BigInt(x).shift(y))
          }
        }
      case .bignum(let x):
        return .makeNumber(x.shift(y))
      default:
        throw RuntimeError.type(num, expected: [.integerType])
    }
  }
  
  private func arithmeticShiftRight(_ num: Expr, _ n: Expr) throws -> Expr {
    let y = try n.asInt()
    switch num {
      case .fixnum(let x):
        return .fixnum(x >> y)
      case .bignum(let x):
        return .makeNumber(x.shift(-y))
      default:
        throw RuntimeError.type(num, expected: [.integerType])
    }
  }
}

