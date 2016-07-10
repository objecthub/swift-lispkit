//
//  ByteVectorLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 03/06/2016.
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


public final class BytevectorLibrary: Library {
  
  public override func export() {
    define(Procedure("bytevector?", isBytevector))
    define(Procedure("bytevector", bytevector))
    define(Procedure("make-bytevector", makeBytevector))
    define(Procedure("bytevector-length", bytevectorLength))
    define(Procedure("bytevector-u8-ref", bytevectorU8Ref))
    define(Procedure("bytevector-u8-set!", bytevectorU8Set))
    define(Procedure("bytevector-copy", bytevectorCopy))
    define(Procedure("bytevector-copy!", bytevectorCopyInto))
    define(Procedure("bytevector-append", bytevectorAppend))
    define(Procedure("utf8->string", utf8ToString))
    define(Procedure("string->utf8", stringToUtf8))
  }
  
  func isBytevector(expr: Expr) -> Expr {
    switch expr {
      case .ByteVec(_):
        return .True
      default:
        return .False
    }
  }
  
  func bytevector(args: Arguments) throws -> Expr {
    var res = [UInt8]()
    for arg in args {
      res.append(try arg.asByte())
    }
    return .ByteVec(MutableBox(res))
  }
  
  func makeBytevector(len: Expr, byte: Expr?) throws -> Expr {
    return .ByteVec(MutableBox([UInt8](count: try len.asInt(),
                                       repeatedValue: try byte?.asByte() ?? 0)))
  }
  
  func bytevectorLength(expr: Expr) throws -> Expr {
    return .Number(try expr.asBytevector().value.count)
  }
  
  func bytevectorU8Ref(bvec: Expr, index: Expr) throws -> Expr {
    let bvector = try bvec.asBytevector()
    let i = try index.asInt()
    guard i >= 0 && i < bvector.value.count else {
      throw EvalError.IndexOutOfBounds(Int64(i), Int64(bvector.value.count - 1), bvec)
    }
    return .Fixnum(Int64(bvector.value[i]))
  }
  
  func bytevectorU8Set(bvec: Expr, index: Expr, expr: Expr) throws -> Expr {
    let bvector = try bvec.asBytevector()
    let i = try index.asInt()
    guard i >= 0 && i < bvector.value.count else {
      throw EvalError.IndexOutOfBounds(Int64(i), Int64(bvector.value.count - 1), expr)
    }
    bvector.value[i] = try expr.asByte()
    return .Void
  }
  
  func bytevectorCopy(bvec: Expr, args: Arguments) throws -> Expr {
    let bvector = try bvec.asBytevector()
    guard let (s, e) = args.optional(Expr.Number(0), Expr.Number(bvector.value.count)) else {
      throw EvalError.ArgumentCountError(formals: 2, args: .Pair(bvec, .List(args)))
    }
    let (start, end) = (try s.asInt(), try e.asInt())
    guard start >= 0 && start < bvector.value.count else {
      throw EvalError.ParameterOutOfBounds(
        "bytevector-copy", 2, Int64(start), Int64(0), Int64(bvector.value.count - 1))
    }
    guard end >= start && end <= bvector.value.count else {
      throw EvalError.ParameterOutOfBounds(
        "bytevector-copy", 3, Int64(end), Int64(start), Int64(bvector.value.count))
    }
    var res = [UInt8](count: end - start, repeatedValue: 0)
    for i in start..<end {
      res[i - start] = bvector.value[i]
    }
    return .ByteVec(MutableBox(res))
  }
  
  func bytevectorCopyInto(to: Expr, at: Expr, from: Expr, args: Arguments) throws -> Expr {
    let toVec = try to.asBytevector()
    let toStart = try at.asInt()
    guard toStart >= 0 && toStart < toVec.value.count else {
      throw EvalError.ParameterOutOfBounds(
        "bytevector-copy!", 2, Int64(toStart), Int64(0), Int64(toVec.value.count - 1))
    }
    let fromVec = try from.asBytevector()
    guard let (s, e) = args.optional(Expr.Number(0), Expr.Number(fromVec.value.count)) else {
      throw EvalError.ArgumentCountError(
        formals: 2, args: .Pair(to, .Pair(at, .Pair(from, .List(args)))))
    } 
    let (start, end) = (try s.asInt(), try e.asInt())
    guard start >= 0 && start < fromVec.value.count else {
      throw EvalError.ParameterOutOfBounds(
        "bytevector-copy!", 4, Int64(start), Int64(0), Int64(fromVec.value.count - 1))
    }
    guard end >= start && end <= fromVec.value.count else {
      throw EvalError.ParameterOutOfBounds(
        "bytevector-copy!", 5, Int64(end), Int64(start), Int64(fromVec.value.count))
    }
    guard toStart + end - start - 1 < toVec.value.count else {
      throw EvalError.TargetBytevectorTooSmall(to)
    }
    for i in start..<end {
      toVec.value[toStart + i - start] = fromVec.value[i]
    }
    return .Void
  }
  
  func bytevectorAppend(exprs: Arguments) throws -> Expr {
    var res = [UInt8]()
    for expr in exprs {
      res.appendContentsOf(try expr.asBytevector().value)
    }
    return .ByteVec(MutableBox(res))
  }
  
  func utf8ToString(bvec: Expr, args: Arguments) throws -> Expr {
    let bvector = try bvec.asBytevector()
    guard let (s, e) = args.optional(Expr.Number(0), Expr.Number(bvector.value.count)) else {
      throw EvalError.ArgumentCountError(formals: 2, args: .Pair(bvec, .List(args)))
    }
    let (start, end) = (try s.asInt(), try e.asInt())
    guard start >= 0 && start < bvector.value.count else {
      throw EvalError.ParameterOutOfBounds(
        "utf8->string", 2, Int64(start), Int64(0), Int64(bvector.value.count - 1))
    }
    guard end >= start && end <= bvector.value.count else {
      throw EvalError.ParameterOutOfBounds(
        "utf8->string", 3, Int64(end), Int64(start), Int64(bvector.value.count))
    }
    var str = ""
    var decoder = UTF8()
    var generator = bvector.value.generate()
    while case .Result(let scalar) = decoder.decode(&generator) {
      str.append(scalar)
    }
    return .Str(NSMutableString(string: str))
  }
  
  func stringToUtf8(string: Expr, args: Arguments) throws -> Expr {
    let str = try string.asStr().utf16
    guard let (s, e) = args.optional(Expr.Number(0), Expr.Number(str.count)) else {
      throw EvalError.ArgumentCountError(formals: 2, args: .Pair(string, .List(args)))
    }
    let (start, end) = (try s.asInt(), try e.asInt())
    let sidx = str.startIndex.advancedBy(start)
    guard sidx < str.endIndex else {
      throw EvalError.IndexOutOfBounds(Int64(start), Int64(str.count), s)
    }
    let eidx = str.startIndex.advancedBy(end)
    guard eidx <= str.endIndex && sidx <= eidx else {
      throw EvalError.IndexOutOfBounds(Int64(end), Int64(str.count), e)
    }
    var uniChars: [UniChar] = []
    for ch in str[sidx..<eidx] {
      uniChars.append(ch)
    }
    let substr = String(utf16CodeUnits: uniChars, count: uniChars.count)
    var res = [UInt8]()
    for byte in substr.utf8 {
      res.append(byte)
    }
    return .ByteVec(MutableBox(res))
  }
}
