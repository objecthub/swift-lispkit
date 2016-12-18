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
  case undef
  case void
  case eof
  case null
  case `true`
  case `false`
  case uninit(Symbol)
  case symbol(Symbol)
  case fixnum(Int64)
  case bignum(BigInt)
  case rational(FixedRational)
  case bigrat(BigRational)
  case flonum(Double)
  case complex(DoubleComplex)
  case char(UniChar)
  case string(NSMutableString)
  case bytes(ByteVector)
  indirect case pair(Expr, Expr)
  case box(Cell)
  case mpair(Tuple)
  case vector(Collection)
  case record(Collection)
  case table(HashTable)
  case promise(Promise)
  case procedure(Procedure)
  case special(SpecialForm)
  case env(Environment)
  case port(Port)
  case error(AnyError)
  
  /// Returns the type of this expression.
  public var type: Type {
    switch self {
      case .undef,
           .uninit(_):
        return .undefinedType
      case .void:
        return .voidType
      case .eof:
        return .eofType
      case .null:
        return .nullType
      case .true:
        return .booleanType
      case .false:
        return .booleanType
      case .symbol(_):
        return .symbolType
      case .fixnum(_):
        return .integerType
      case .bignum(_):
        return .integerType
      case .rational(_):
        return .rationalType
      case .bigrat(_):
        return .rationalType
      case .flonum(_):
        return .floatType
      case .complex(_):
        return .complexType
      case .char(_):
        return .charType
      case .string(_):
        return .strType
      case .bytes(_):
        return .byteVectorType
      case .pair(_, _):
        return .pairType
      case .box(_):
        return .boxType
      case .mpair(_):
        return .mpairType
      case .vector(_):
        return .vectorType
      case .record(_):
        return .recordType
      case .table(_):
        return .tableType
      case .promise(_):
        return .promiseType
      case .procedure(_):
        return .procedureType
      case .special(_):
        return .specialType
      case .env(_):
        return .envType
      case .port(_):
        return .portType
      case .error(_):
        return .errorType
    }
  }
  
  // Predicate methods
  
  /// Returns true if this expression is undefined.
  public var isUndef: Bool {
    switch self {
      case .undef, .uninit(_):
        return true
      default:
        return false
    }
  }
  
  /// Returns true if this expression is null.
  public var isNull: Bool {
    switch self {
      case .null:
        return true
      default:
        return false
    }
  }
  
  /// Returns true if this is not the `#f` value.
  public var isTrue: Bool {
    switch self {
      case .false:
        return false
      default:
        return true
    }
  }
  
  /// Returns true if this is the `#f` value.
  public var isFalse: Bool {
    switch self {
      case .false:
        return true
      default:
        return false
    }
  }
  
  /// Returns true if this is an exact number.
  public var isExactNumber: Bool {
    switch self {
      case .fixnum(_), .bignum(_), .rational(_), .bigrat(_):
        return true
      default:
        return false
    }
  }
  
  /// Normalizes the representation (relevant for numeric datatypes)
  public var normalized: Expr {
    switch self {
      case .bignum(let num):
        if let fn = num.intValue {
          return .fixnum(fn)
        }
        return self
      case .rational(let num):
        if let fn = num.value.intValue {
          return .fixnum(fn)
        }
        return self
      case .bigrat(let num):
        if let bn = num.value.intValue {
          if let fn = bn.intValue {
            return .fixnum(fn)
          }
          return .bignum(bn)
        }
        if let fnnumer = num.value.numerator.intValue,
           let fndenom = num.value.denominator.intValue {
          let num: Rational<Int64> = Rational(fnnumer, fndenom)
          return Expr.rational(ImmutableBox(num)).normalized
        }
        return self
      case .complex(let num):
        return num.value.isReal ? .flonum(num.value.re) : self
      default:
        return self
    }
  }
  
  /// Returns the given expression with all symbols getting interned.
  public var datum: Expr {
    switch self {
      case .symbol(let sym):
        return .symbol(sym.interned)
      case .pair(let car, let cdr):
        return .pair(car.datum, cdr.datum)
      default:
        return self
    }
  }
  
  /// Maps a list into an array of expressions and a tail (= null for proper lists)
  public func toExprs() -> (Exprs, Expr) {
    var exprs = Exprs()
    var expr = self
    while case .pair(let car, let cdr) = expr {
      exprs.append(car)
      expr = cdr
    }
    return (exprs, expr)
  }
  
  /// The length of this expression (all non-pair expressions have length 1).
  var length: Int {
    var expr = self
    var len = 0
    while case .pair(_, let cdr) = expr {
      len += 1
      expr = cdr
    }
    return len
  }
  
  /// Returns true if the expression isn't referring to other expressions directly or
  /// indirectly.
  public var isAtom: Bool {
    switch self {
      case .undef, .void, .eof, .null, .true, .false, .uninit(_), .symbol(_),
           .fixnum(_), .bignum(_), .rational(_), .bigrat(_), .flonum(_), .complex(_),
           .char(_), .string(_), .bytes(_), .env(_), .port(_):
        return true
      default:
        return false
    }
  }
  
  public var requiresTracking: Bool {
    switch self {
      case .pair(let car, let cdr):
        return car.requiresTracking || cdr.requiresTracking
      case .box(_), .mpair(_), .vector(_), .record(_), .table(_), .promise(_),
           .procedure(_), .special(_), .error(_):
        return true
      default:
        return false
    }
  }
  
  public func mark(_ tag: UInt8) {
    switch self {
      case .pair(let car, let cdr):
        car.mark(tag)
        cdr.mark(tag)
      case .box(let cell):
        cell.mark(tag)
      case .mpair(let tuple):
        tuple.mark(tag)
      case .vector(let vector):
        vector.mark(tag)
      case .record(let record):
        record.mark(tag)
      case .table(let map):
        map.mark(tag)
      case .promise(let future):
        future.mark(tag)
      case .procedure(let proc):
        proc.mark(tag)
      case .special(let special):
        special.mark(tag)
      case .error(_):
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
  
  public static func makeBoolean(_ val: Bool) -> Expr {
    return val ? .true : .false
  }
  
  public static func makeNumber(_ num: Int) -> Expr {
    return .fixnum(Int64(num))
  }
  
  public static func makeNumber(_ num: Int64) -> Expr {
    return .fixnum(num)
  }
  
  public static func makeNumber(_ num: BigInt) -> Expr {
    return Expr.bignum(num).normalized
  }
  
  public static func makeNumber(_ num: Rational<Int64>) -> Expr {
    return Expr.rational(ImmutableBox(num)).normalized
  }
  
  public static func makeNumber(_ num: Rational<BigInt>) -> Expr {
    return Expr.bigrat(ImmutableBox(num)).normalized
  }
  
  public static func makeNumber(_ num: Double) -> Expr {
    return .flonum(num)
  }
  
  public static func makeNumber(_ num: Complex<Double>) -> Expr {
    return Expr.complex(ImmutableBox(num)).normalized
  }
  
  public static func makeList(_ expr: Expr...) -> Expr {
    return Expr.makeList(expr)
  }
  
  public static func makeList(_ exprs: Exprs, append: Expr = Expr.null) -> Expr {
    return Expr.makeList(fromStack: exprs.reversed(), append: append)
  }
  
  public static func makeList(_ exprs: Arguments, append: Expr = Expr.null) -> Expr {
    return Expr.makeList(fromStack: exprs.reversed(), append: append)
  }
  
  public static func makeList(fromStack exprs: Exprs, append: Expr = Expr.null) -> Expr {
    var res = append
    for expr in exprs {
      res = pair(expr, res)
    }
    return res
  }
  
  public static func makeString(_ str: String) -> Expr {
    return .string(NSMutableString(string: str))
  }
}


/// This extension adds projections to `Expr`.
///
extension Expr {
  public func assertType(_ types: Type...) throws {
    for type in types {
      for subtype in type.included {
        if self.type == subtype {
          return
        }
      }
    }
    throw EvalError.typeError(self, Set(types))
  }
  
  public func asInt64() throws -> Int64 {
    guard case .fixnum(let res) = self else {
      throw EvalError.typeError(self, [.integerType])
    }
    return res
  }
  
  public func asInt(below: Int = Int.max) throws -> Int {
    guard case .fixnum(let res) = self else {
      throw EvalError.typeError(self, [.integerType])
    }
    guard res >= 0 && res < Int64(below) else {
      throw EvalError.indexOutOfBounds(res, Int64(below), self)
    }
    return Int(res)
  }
  
  public func asUInt8() throws -> UInt8 {
    guard case .fixnum(let number) = self , number >= 0 && number <= 255 else {
      throw EvalError.typeError(self, [.byteType])
    }
    return UInt8(number)
  }
  
  public func asDouble(coerce: Bool = false) throws -> Double {
    if !coerce {
      if case .flonum(let num) = self {
        return num
      }
      throw EvalError.typeError(self, [.floatType])
    }
    switch self.normalized {
      case .fixnum(let num):
        return Double(num)
      case .bignum(let num):
        return num.doubleValue
      case .rational(let num):
        return Double(num.value.numerator) / Double(num.value.denominator)
      case .bigrat(let num):
        return num.value.numerator.doubleValue / num.value.denominator.doubleValue
      case .flonum(let num):
        return num
      default:
        throw EvalError.typeError(self, [.realType])
    }
  }
    
  public func asComplex(coerce: Bool = false) throws -> Complex<Double> {
    if !coerce {
      switch self {
        case .flonum(let num):
          return Complex(num, 0.0)
        case .complex(let complex):
          return complex.value
        default:
          throw EvalError.typeError(self, [.complexType])
      }
    }
    switch self.normalized {
      case .fixnum(let num):
        return Complex(Double(num), 0.0)
      case .bignum(let num):
        return Complex(num.doubleValue, 0.0)
      case .rational(let num):
        return Complex(Double(num.value.numerator) / Double(num.value.denominator), 0.0)
      case .bigrat(let num):
        return Complex(num.value.numerator.doubleValue / num.value.denominator.doubleValue, 0.0)
      case .flonum(let num):
        return Complex(num, 0.0)
      case .complex(let num):
        return num.value
      default:
        throw EvalError.typeError(self, [.complexType])
    }
  }
  
  public func asSymbol() throws -> Symbol {
    guard let symid = self.toSymbol() else {
      throw EvalError.typeError(self, [.symbolType])
    }
    return symid
  }
  
  public func toSymbol() -> Symbol? {
    switch self {
      case .symbol(let sym):
        return sym
      default:
        return nil
    }
  }
  
  public func asUniChar() throws -> UniChar {
    guard case .char(let res) = self else {
      throw EvalError.typeError(self, [.charType])
    }
    return res
  }
  
  public func charAsString() throws -> String {
    guard case .char(let res) = self else {
      throw EvalError.typeError(self, [.charType])
    }
    return String(unicodeScalar(res))
  }
  
  public func asString() throws -> String {
    guard case .string(let res) = self else {
      throw EvalError.typeError(self, [.strType])
    }
    return res as String
  }
  
  public func asMutableStr() throws -> NSMutableString {
    guard case .string(let res) = self else {
      throw EvalError.typeError(self, [.strType])
    }
    return res
  }
  
  public func asPath() throws -> String {
    guard case .string(let res) = self else {
      throw EvalError.typeError(self, [.strType])
    }
    return res.expandingTildeInPath
  }
  
  public func asByteVector() throws -> ByteVector {
    guard case .bytes(let bvector) = self else {
      throw EvalError.typeError(self, [.byteVectorType])
    }
    return bvector
  }
  
  public func vectorAsCollection() throws -> Collection {
    guard case .vector(let res) = self else {
      throw EvalError.typeError(self, [.vectorType])
    }
    return res
  }
  
  public func recordAsCollection() throws -> Collection {
    guard case .record(let res) = self else {
      throw EvalError.typeError(self, [.recordType])
    }
    return res
  }
  
  public func asHashTable() throws -> HashTable {
    guard case .table(let map) = self else {
      throw EvalError.typeError(self, [.tableType])
    }
    return map
  }
  
  public func asProcedure() throws -> Procedure {
    guard case .procedure(let proc) = self else {
      throw EvalError.typeError(self, [.procedureType])
    }
    return proc
  }
  
  public func asEnvironment() throws -> Environment {
    guard case .env(let environment) = self else {
      throw EvalError.typeError(self, [.envType])
    }
    return environment
  }
  
  public func asPort() throws -> Port {
    guard case .port(let port) = self else {
      throw EvalError.typeError(self, [.portType])
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
  
  public func toString(escape: Bool = true) -> String {
    var enclObjs = Set<Reference>()
    var objId = [Reference: Int]()
    
    func objIdString(_ ref: Reference) -> String? {
      if let id = objId[ref] {
        return "#\(id)#"
      } else if enclObjs.contains(ref) {
        objId[ref] = objId.count
        return "#\(objId.count - 1)#"
      } else {
        return nil
      }
    }
    
    func fixString(_ ref: Reference, _ str: String) -> String {
      if let id = objId[ref] {
        return "#\(id)=\(str)"
      } else {
        return str
      }
    }
    
    func doubleString(_ val: Double) -> String {
      if val.isInfinite {
        return (val.sign == .minus) ? "-inf.0" : "+inf.0"
      } else if val.isNaN {
        return "+nan.0"
      } else {
        return String(val)
      }
    }
    
    func stringReprOf(_ expr: Expr) -> String {
      switch expr {
        case .undef:
          return "#<undef>"
        case .void:
          return "#<void>"
        case .eof:
          return "#<eof>"
        case .null:
          return "()"
        case .true:
          return "#t"
        case .false:
          return "#f"
        case .uninit(let sym):
          guard escape else {
            return "#<uninit \(sym.rawIdentifier)>"
          }
          return "#<uninit \(sym.description)>"
        case .symbol(let sym):
          guard escape else {
            return sym.rawIdentifier
          }
          return sym.description
        case .fixnum(let val):
          return String(val)
        case .bignum(let val):
          return val.description
        case .rational(let val):
          return val.value.description
        case .bigrat(let val):
          return val.value.description
        case .flonum(let val):
          return doubleString(val)
        case .complex(let val):
          var res = doubleString(val.value.re)
          if val.value.im.isNaN || val.value.im.isInfinite || val.value.im < 0.0 {
            res += doubleString(val.value.im)
          } else {
            res += "+" + doubleString(val.value.im)
          }
          return res + "i"
        case .char(let ch):
          guard escape else {
            return String(unicodeScalar(ch))
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
              if WHITESPACES_NL.contains(unicodeScalar(ch)) ||
                 CONTROL_CHARS.contains(unicodeScalar(ch)) ||
                 ILLEGAL_CHARS.contains(unicodeScalar(ch)) ||
                 MODIFIER_CHARS.contains(unicodeScalar(ch)) ||
                 ch > 0xd7ff {
                return "#\\u\(String(ch, radix:16))"
              } else {
                return "#\\\(Character(UnicodeScalar(ch)!))"
              }
          }
        case .string(let str):
          guard escape else {
            return str as String
          }
          return "\"\(Expr.escapeStr(str as String))\""
        case .bytes(let boxedVec):
          var builder = StringBuilder(prefix: "#u8(", postfix: ")", separator: " ")
          for byte in boxedVec.value {
            builder.append(String(byte))
          }
          return builder.description
        case .pair(let head, let tail):
          var builder = StringBuilder(prefix: "(", separator: " ")
          builder.append(stringReprOf(head))
          var expr = tail
          while case .pair(let car, let cdr) = expr {
            builder.append(stringReprOf(car))
            expr = cdr
          }
          return builder.description + (expr.isNull ? ")" : " . \(stringReprOf(expr)))")
        case .box(let cell):
          if let res = objIdString(cell) {
            return res
          } else {
            enclObjs.insert(cell)
            let res = "#<box \(cell.identityString): \(stringReprOf(cell.value))>"
            enclObjs.remove(cell)
            return fixString(cell, res)
          }
        case .mpair(let tuple):
          if let res = objIdString(tuple) {
            return res
          } else {
            enclObjs.insert(tuple)
            let res = "#<tuple \(tuple.identityString): " +
                      "\(stringReprOf(tuple.fst)), \(stringReprOf(tuple.snd))>"
            enclObjs.remove(tuple)
            return fixString(tuple, res)
          }
        case .vector(let vector):
          if let res = objIdString(vector) {
            return res
          } else if vector.exprs.count == 0 {
            return "#()"
          } else {
            enclObjs.insert(vector)
            var builder = StringBuilder(prefix: "#(", postfix: ")", separator: " ")
            for expr in vector.exprs {
              builder.append(stringReprOf(expr))
            }
            enclObjs.remove(vector)
            return fixString(vector, builder.description)
          }
        case .record(let record):
          guard case .record(let type) = record.kind else {
            guard case .string(let name) = record.exprs[0] else {
              preconditionFailure("incorrect encoding of record type")
            }
            return "#<record-type \(name)>"
          }
          if let res = objIdString(record) {
            return res
          } else {
            guard case .string(let name) = type.exprs[0] else {
              preconditionFailure("incorrect encoding of record type")
            }
            enclObjs.insert(record)
            var builder = StringBuilder(prefix: "#<record \(name)",
                                        postfix: ">",
                                        separator: ", ",
                                        initial: ": ")
            var fields = type.exprs[2]
            for expr in record.exprs {
              guard case .pair(let sym, let nextFields) = fields else {
                preconditionFailure("incorrect encoding of record \(type.exprs[0])")
              }
              builder.append(sym.description, "=", stringReprOf(expr))
              fields = nextFields
            }
            enclObjs.remove(record)
            return fixString(record, builder.description)
          }
        case .table(let map):
          if let res = objIdString(map) {
            return res
          } else {
            enclObjs.insert(map)
            var builder = StringBuilder(prefix: "#<hashtable \(map.identityString)",
                                        postfix: ">",
                                        separator: ", ",
                                        initial: ": ")
            for (key, value) in map.mappings {
              builder.append(stringReprOf(key), " -> ", stringReprOf(value))
            }
            enclObjs.remove(map)
            return fixString(map, builder.description)
          }
        case .promise(let promise):
          return "#<promise \(promise.identityString)>"
        case .procedure(let proc):
          switch proc.kind {
            case .parameter(let tuple):
              if let res = objIdString(proc) {
                return res
              } else {
                enclObjs.insert(proc)
                let res = "#<parameter \(proc.name): \(stringReprOf(tuple.snd))>"
                enclObjs.remove(proc)
                return fixString(proc, res)
              }
            case .continuation(_):
              return "#<continuation \(proc.name)>"
            default:
              return "#<procedure \(proc.name)>"
          }
        case .special(let special):
          return "#<special \(special.identityString)>"
        case .env(let environment):
          var type: String = ""
          switch environment.kind {
            case .library(let name):
              type = " " + name.description
            case .program(let filename):
              type = " " + filename
            case .repl:
              type = " interaction"
            case .custom:
              type = ""
          }
          var builder = StringBuilder(prefix: "#<env",
                                      postfix: ">",
                                      separator: ", ",
                                      initial: type + ": ")
          for sym in environment.boundSymbols {
            builder.append(sym.description)
          }
          return builder.description
        case .port(let port):
          return "#<\(port.typeDescription) \(port.identDescription)>"
        case .error(let error):
          return error.description
      }
    }
    return stringReprOf(self)
  }
  
  internal static func escapeStr(_ str: String) -> String {
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
  return equalExpr(rhs, lhs)
}

public typealias ByteVector = MutableBox<[UInt8]>
public typealias FixedRational = ImmutableBox<Rational<Int64>>
public typealias BigRational = ImmutableBox<Rational<BigInt>>
public typealias DoubleComplex = ImmutableBox<Complex<Double>>
