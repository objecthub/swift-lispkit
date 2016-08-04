//
//  Expr.swift
//  LispKit
//
//  Created by Matthias Zenger on 08/11/2015.
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
/// `Expr` represents LispKit expressions in form of an enumeration with associated values.
///
public enum Expr: Trackable, Hashable {
  case Undef
  case Void
  case Eof
  case Null
  case True
  case False
  case Sym(Symbol)
  case Fixnum(Int64)
  case Bignum(BigInt)
  case Rat(Rational<Int64>)
  case Bigrat(Rational<BigInt>)
  case Flonum(Double)
  case Complexnum(Complex<Double>)
  case Char(UniChar)
  case Str(NSMutableString)
  case Bytes(ByteVector)
  indirect case Pair(Expr, Expr)
  case Box(Cell)
  case MPair(Tuple)
  case Vec(Vector)
  case Map(HashMap)
  case Promise(Future)
  case Proc(Procedure)
  case Special(SpecialForm)
  case Prt(Port)
  case Error(AnyError)
  
  /// Returns the type of this expression.
  public var type: Type {
    switch self {
      case Undef:
        return .UndefinedType
      case Void:
        return .VoidType
      case Eof:
        return .EofType
      case Null:
        return .NullType
      case True:
        return .BooleanType
      case False:
        return .BooleanType
      case Sym(_):
        return .SymbolType
      case Fixnum(_):
        return .IntegerType
      case Bignum(_):
        return .IntegerType
      case Rat(_):
        return .RationalType
      case Bigrat(_):
        return .RationalType
      case Flonum(_):
        return .FloatType
      case Complexnum(_):
        return .ComplexType
      case Char(_):
        return .CharType
      case Str(_):
        return .StrType
      case Bytes(_):
        return .ByteVectorType
      case Pair(_, _):
        return .PairType
      case Box(_):
        return .BoxType
      case MPair(_):
        return .MPairType
      case Vec(_):
        return .VectorType
      case Map(_):
        return .MapType
      case Promise(_):
        return .PromiseType
      case Proc(_):
        return .ProcedureType
      case Special(_):
        return .SpecialType
      case Prt(_):
        return .PortType
      case Error(_):
        return .ErrorType
    }
  }
  
  // Predicate methods
  
  /// Returns true if this expression is null.
  public var isNull: Bool {
    switch self {
      case Null:
        return true
      default:
        return false
    }
  }
  
  /// Returns true if this is not the `#f` value.
  public var isTrue: Bool {
    switch self {
      case False:
        return false
      default:
        return true
    }
  }
  
  /// Returns true if this is the `#f` value.
  public var isFalse: Bool {
    switch self {
      case False:
        return true
      default:
        return false
    }
  }
  
  /// Returns true if this is an exact number.
  public var isExactNumber: Bool {
    switch self {
      case Fixnum(_), Bignum(_), Rat(_), Bigrat(_):
        return true
      default:
        return false
    }
  }
  
  /// Normalizes the representation (relevant for numeric datatypes)
  public var normalized: Expr {
    switch self {
      case Bignum(let num):
        if let fn = num.intValue {
          return Fixnum(fn)
        }
        return self
      case Rat(let num):
        if let fn = num.intValue {
          return Fixnum(fn)
        }
        return self
      case Bigrat(let num):
        if let bn = num.intValue {
          if let fn = bn.intValue {
            return Fixnum(fn)
          }
          return Bignum(bn)
        }
        if let fnnumer = num.numerator.intValue, fndenom = num.denominator.intValue {
          return Rat(Rational(fnnumer, fndenom)).normalized
        }
        return self
      case Complexnum(let num):
        return num.isReal ? Flonum(num.re) : self
      default:
        return self
    }
  }
  
  /// Returns the given expression with all symbols getting interned.
  public var datum: Expr {
    switch self {
      case Sym(let sym):
        return Sym(sym.interned)
      case Pair(let car, let cdr):
        return .Pair(car.datum, cdr.datum)
      default:
        return self
    }
  }
  
  /// Maps a list into an array of expressions and a tail (= null for proper lists)
  public func toExprs() -> (Exprs, Expr) {
    var exprs = Exprs()
    var expr = self
    while case Pair(let car, let cdr) = expr {
      exprs.append(car)
      expr = cdr
    }
    return (exprs, expr)
  }
  
  /// The length of this expression (all non-pair expressions have length 1)
  var length: Int {
    var expr = self
    var len: Int = 0
    while case .Pair(_, let cdr) = expr {
      len += 1
      expr = cdr
    }
    return len
  }
  
  public var requiresTracking: Bool {
    switch self {
      case Pair(let car, let cdr):
        return car.requiresTracking || cdr.requiresTracking
      case Box(_), MPair(_), Vec(_), Map(_), Promise(_), Proc(_), Special(_), Error(_):
        return true
      default:
        return false
    }
  }
  
  public func mark(tag: UInt8) {
    switch self {
      case Pair(let car, let cdr):
        car.mark(tag)
        cdr.mark(tag)
      case Box(let cell):
        cell.mark(tag)
      case MPair(let tuple):
        tuple.mark(tag)
      case Vec(let vector):
        vector.mark(tag)
      case Map(let map):
        map.mark(tag)
      case Promise(let future):
        future.mark(tag)
      case Proc(let proc):
        proc.mark(tag)
      case Special(let special):
        special.mark(tag)
      case Error(_):
        break
      default:
        break
    }
  }
  
  public var hashValue: Int {
    return equalHash(self)
  }
}


/// Extension adding static factory methods to `Expr`.
///
extension Expr {
  
  public static func Boolean(val: Bool) -> Expr {
    return val ? True : False
  }
  
  public static func Number(num: Int) -> Expr {
    return Fixnum(Int64(num))
  }
  
  public static func Number(num: Int64) -> Expr {
    return Fixnum(num)
  }
  
  public static func Number(num: BigInt) -> Expr {
    return Bignum(num).normalized
  }
  
  public static func Number(num: Rational<Int64>) -> Expr {
    return Rat(num).normalized
  }
  
  public static func Number(num: Rational<BigInt>) -> Expr {
    return Bigrat(num).normalized
  }
  
  public static func Number(num: Double) -> Expr {
    return Flonum(num)
  }
  
  public static func Number(num: Complex<Double>) -> Expr {
    return Complexnum(num).normalized
  }
  
  public static func List(expr: Expr...) -> Expr {
    return List(expr)
  }
  
  public static func List(exprs: Exprs, append: Expr = Expr.Null) -> Expr {
    return Expr.Stack(exprs.reverse(), append: append)
  }
  
  public static func List(exprs: Arguments, append: Expr = Expr.Null) -> Expr {
    return Expr.Stack(exprs.reverse(), append: append)
  }
  
  public static func Stack(exprs: Exprs, append: Expr = Expr.Null) -> Expr {
    var res = append
    for expr in exprs {
      res = Pair(expr, res)
    }
    return res
  }
  
  public static func StringFor(str: String) -> Expr {
    return .Str(NSMutableString(string: str))
  }
}


/// This extension adds projections to `Expr`.
///
extension Expr {
  public func assertTypeOf(types: Type...) throws {
    for type in types {
      for subtype in type.included {
        if self.type == subtype {
          return
        }
      }
    }
    throw EvalError.TypeError(self, Set(types))
  }
  
  public func asInteger() throws -> Int64 {
    guard case Fixnum(let res) = self else {
      throw EvalError.TypeError(self, [.IntegerType])
    }
    return res
  }
  
  public func asInt(below below: Int = Int.max) throws -> Int {
    guard case Fixnum(let res) = self else {
      throw EvalError.TypeError(self, [.IntegerType])
    }
    guard res >= 0 && res < Int64(below) else {
      throw EvalError.IndexOutOfBounds(res, Int64(below), self)
    }
    return Int(res)
  }
  
  public func asByte() throws -> UInt8 {
    guard case Fixnum(let number) = self where number >= 0 && number <= 255 else {
      throw EvalError.TypeError(self, [.ByteType])
    }
    return UInt8(number)
  }
  
  public func asFloat(coerce coerce: Bool = false) throws -> Double {
    if !coerce {
      if case Flonum(let num) = self {
        return num
      }
      throw EvalError.TypeError(self, [.FloatType])
    }
    switch self.normalized {
      case Fixnum(let num):
        return Double(num)
      case .Bignum(let num):
        return num.doubleValue
      case .Rat(let num):
        return Double(num.numerator) / Double(num.denominator)
      case .Bigrat(let num):
        return num.numerator.doubleValue / num.denominator.doubleValue
      case .Flonum(let num):
        return num
      default:
        throw EvalError.TypeError(self, [.RealType])
    }
  }
    
  public func asComplex(coerce coerce: Bool = false) throws -> Complex<Double> {
    if !coerce {
      switch self {
        case Flonum(let num):
          return Complex(num, 0.0)
        case Complexnum(let complex):
          return complex
        default:
          throw EvalError.TypeError(self, [.ComplexType])
      }
    }
    switch self.normalized {
      case Fixnum(let num):
        return Complex(Double(num), 0.0)
      case .Bignum(let num):
        return Complex(num.doubleValue, 0.0)
      case .Rat(let num):
        return Complex(Double(num.numerator) / Double(num.denominator), 0.0)
      case .Bigrat(let num):
        return Complex(num.numerator.doubleValue / num.denominator.doubleValue, 0.0)
      case .Flonum(let num):
        return Complex(num, 0.0)
      case .Complexnum(let num):
        return num
      default:
        throw EvalError.TypeError(self, [.ComplexType])
    }
  }
  
  public func asSymbol() throws -> Symbol {
    guard let symid = self.toSymbol() else {
      throw EvalError.TypeError(self, [.SymbolType])
    }
    return symid
  }
  
  public func toSymbol() -> Symbol? {
    switch self {
    case Sym(let sym):
      return sym
    default:
      return nil
    }
  }
  
  public func asChar() throws -> UniChar {
    guard case Char(let res) = self else {
      throw EvalError.TypeError(self, [.CharType])
    }
    return res
  }
  
  public func asCharStr() throws -> String {
    guard case Char(let res) = self else {
      throw EvalError.TypeError(self, [.CharType])
    }
    return String(UnicodeScalar(res))
  }
  
  public func asStr() throws -> String {
    guard case Str(let res) = self else {
      throw EvalError.TypeError(self, [.StrType])
    }
    return res as String
  }
  
  public func asMutableStr() throws -> NSMutableString {
    guard case Str(let res) = self else {
      throw EvalError.TypeError(self, [.StrType])
    }
    return res
  }
  
  public func asByteVector() throws -> ByteVector {
    guard case Bytes(let bvector) = self else {
      throw EvalError.TypeError(self, [.ByteVectorType])
    }
    return bvector
  }
  
  public func asPair() throws -> (Expr, Expr) {
    guard case Pair(let res) = self else {
      throw EvalError.TypeError(self, [.PairType])
    }
    return res
  }
  
  public func asVector() throws -> Vector {
    guard case Vec(let res) = self else {
      throw EvalError.TypeError(self, [.VectorType])
    }
    return res
  }
  
  public func asMap() throws -> HashMap {
    guard case Map(let map) = self else {
      throw EvalError.TypeError(self, [.MapType])
    }
    return map
  }
  
  public func asProc() throws -> Procedure {
    guard case Proc(let proc) = self else {
      throw EvalError.TypeError(self, [.ProcedureType])
    }
    return proc
  }
  
  public func asPort() throws -> Port {
    guard case Prt(let port) = self else {
      throw EvalError.TypeError(self, [.PortType])
    }
    return port
  }
}


/// This extension makes `Expr` implement the `CustomStringConvertible`.
///
extension Expr: CustomStringConvertible {
  
  public var description: String {
    return self.toString()
  }
  
  public var unescapedDescription: String {
    return self.toString(escape: false)
  }
  
  public func toString(escape escape: Bool = true) -> String {
    var enclObjs = Set<Reference>()
    var objId = [Reference: Int]()
    
    func objIdString(ref: Reference) -> String? {
      if let id = objId[ref] {
        return "#\(id)#"
      } else if enclObjs.contains(ref) {
        objId[ref] = objId.count
        return "#\(objId.count - 1)#"
      } else {
        return nil
      }
    }
    
    func fixString(ref: Reference, _ str: String) -> String {
      if let id = objId[ref] {
        return "#\(id)=\(str)"
      } else {
        return str
      }
    }
    
    func stringReprOf(expr: Expr) -> String {
      switch expr {
        case .Undef:
          return "#<undef>"
        case .Void:
          return "#<void>"
        case .Eof:
          return "#<eof>"
        case .Null:
          return "()"
        case .True:
          return "#t"
        case .False:
          return "#f"
        case .Sym(let sym):
          guard escape else {
            return sym.rawIdentifier
          }
          return sym.description
        case .Fixnum(let val):
          return String(val)
        case .Bignum(let val):
          return val.description
        case .Rat(let val):
          return val.description
        case .Bigrat(let val):
          return val.description
        case .Flonum(let val):
          if val.isInfinite {
            return val.isSignMinus ? "-inf.0" : "+inf.0"
          } else if val.isNaN {
            return "+nan.0"
          } else {
            return String(val)
          }
        case .Complexnum(let val):
          return val.description
        case .Char(let ch):
          guard escape else {
            return String(UnicodeScalar(ch))
          }
          switch ch {
            case   7: return "#\\alarm"
            case   8: return "#\\backspace"
            case 127: return "#\\delete"
            case  27: return "#\\escape"
            case  10: return "#\\newline"
            case   0: return "#\\null"
            case  12: return "#\\page"
            case  13: return "#\\return"
            case  32: return "#\\space"
            case   9: return "#\\tab"
            case  11: return "#\\vtab"
            default :
              if WHITESPACES_NL.characterIsMember(ch) ||
                 CONTROL_CHARS.characterIsMember(ch) ||
                 ILLEGAL_CHARS.characterIsMember(ch) ||
                 MODIFIER_CHARS.characterIsMember(ch) ||
                 ch > 0xd7ff {
                return "#\\u\(String(ch, radix:16))"
              } else {
                return "#\\\(Character(UnicodeScalar(ch)))"
              }
          }
        case .Str(let str):
          guard escape else {
            return str as String
          }
          return "\"\(Expr.escapeStr(str as String))\""
        case .Bytes(let boxedVec):
          var res = "#u8("
          var sep = ""
          for byte in boxedVec.value {
            res = res + sep + String(byte)
            sep = " "
          }
          return res + ")"
        case .Pair(let head, let tail):
          var res = "(" + stringReprOf(head)
          var expr = tail
          while case .Pair(let car, let cdr) = expr {
            res += " "
            res += stringReprOf(car)
            expr = cdr
          }
          return res + (expr.isNull ? ")" : " . \(expr))")
        case .Box(let cell):
          if let res = objIdString(cell) {
            return res
          } else {
            enclObjs.insert(cell)
            let res = "#<box \(String(cell.identity, radix: 16)): \(stringReprOf(cell.value))>"
            enclObjs.remove(cell)
            return fixString(cell, res)
          }
        case .MPair(let tuple):
          if let res = objIdString(tuple) {
            return res
          } else {
            enclObjs.insert(tuple)
            let res = "#<tuple \(String(tuple.identity, radix: 16)): " +
                      "\(stringReprOf(tuple.fst)), \(stringReprOf(tuple.snd))>"
            enclObjs.remove(tuple)
            return fixString(tuple, res)
          }
        case .Vec(let vector):
          if let res = objIdString(vector) {
            return res
          } else if vector.exprs.count == 0 {
            return "#()"
          } else {
            enclObjs.insert(vector)
            var res = ""
            var sep = "#("
            for expr in vector.exprs {
              res = res + sep + stringReprOf(expr)
              sep = " "
            }
            res += ")"
            enclObjs.remove(vector)
            return fixString(vector, res)
          }
        case .Map(let map):
          if let res = objIdString(map) {
            return res
          } else {
            enclObjs.insert(map)
            var res = "#<hashtable \(String(map.identity, radix: 16)):"
            var sep = " "
            for (key, value) in map.mappings {
              res = res + sep + stringReprOf(key) + " -> " + stringReprOf(value)
              sep = ", "
            }
            res += ">"
            enclObjs.remove(map)
            return fixString(map, res)
          }
        case .Promise(let promise):
          return "#<promise \(String(promise.identity, radix: 16))>"
        case .Proc(let proc):
          return "#<procedure \(proc.name)>"
        case .Special(let special):
          return "#<special \(String(special.identity, radix: 16))>"
        case .Prt(let port):
          return "#<\(port.typeDescription) \(port.identDescription)>"
        case .Error(let error):
          return error.description
      }
    }
    return stringReprOf(self)
  }
  
  internal static func escapeStr(str: String) -> String {
    var res = ""
    for c in str.characters {
      switch c {
      case "\u{7}":  res += "\\a"
      case "\u{8}":  res += "\\b"
      case "\t":     res += "\\t"
      case "\n":     res += "\\n"
      case "\u{11}": res += "\\v"
      case "\u{12}": res += "\\f"
      case "\r":     res += "\\r"
      case "\u{27}": res += "\\e"
      case "\"":     res += "\\\""
      case "\\":     res += "\\\\"
      default:       res.append(c)
      }
    }
    return res
  }
}

public func ==(lhs: Expr, rhs: Expr) -> Bool {
  return equalExpr(lhs, rhs)
}

public typealias ByteVector = MutableBox<[UInt8]>

