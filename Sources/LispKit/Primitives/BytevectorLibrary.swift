//
//  BytevectorLibrary.swift
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

import Foundation

///
/// Bytevector library: based on R7RS spec.
///
public final class BytevectorLibrary: NativeLibrary {
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "bytevector"]
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("bytevector?", isBytevector))
    self.define(Procedure("bytevector", bytevector))
    self.define(Procedure("make-bytevector", makeBytevector))
    self.define(Procedure("bytevector-length", bytevectorLength))
    self.define(Procedure("bytevector-u8-ref", bytevectorU8Ref))
    self.define(Procedure("bytevector-u8-set!", bytevectorU8Set))
    self.define(Procedure("bytevector-copy", bytevectorCopy))
    self.define(Procedure("bytevector-copy!", bytevectorCopyInto))
    self.define(Procedure("bytevector-append", bytevectorAppend))
    self.define(Procedure("utf8->string", utf8ToString))
    self.define(Procedure("string->utf8", stringToUtf8))
    self.define(Procedure("bytevector->base64", bytevectorToBase64))
    self.define(Procedure("base64->bytevector", base64ToBytevector))
    self.define(Procedure("bytevector-deflate", bytevectorDeflate))
    self.define(Procedure("bytevector-inflate", bytevectorInflate))
  }
  
  func isBytevector(_ expr: Expr) -> Expr {
    switch expr {
      case .bytes(_):
        return .true
      default:
        return .false
    }
  }
  
  func bytevector(_ args: Arguments) throws -> Expr {
    var res = [UInt8]()
    for arg in args {
      res.append(try arg.asUInt8())
    }
    return .bytes(MutableBox(res))
  }
  
  func makeBytevector(_ len: Expr, byte: Expr?) throws -> Expr {
    return .bytes(MutableBox([UInt8](repeating: try byte?.asUInt8() ?? 0,
                                     count: try len.asInt())))
  }
  
  func bytevectorLength(_ expr: Expr) throws -> Expr {
    return .makeNumber(try expr.asByteVector().value.count)
  }
  
  func bytevectorU8Ref(_ bvec: Expr, index: Expr) throws -> Expr {
    let bvector = try bvec.asByteVector()
    let i = try index.asInt()
    guard i >= 0 && i < bvector.value.count else {
      throw RuntimeError.range(parameter: 2,
                               of: "bytevector-u8-ref",
                               index,
                               min: Int64(i),
                               max: Int64(bvector.value.count - 1))
    }
    return .fixnum(Int64(bvector.value[i]))
  }
  
  func bytevectorU8Set(_ bvec: Expr, index: Expr, expr: Expr) throws -> Expr {
    let bvector = try bvec.asByteVector()
    let i = try index.asInt()
    guard i >= 0 && i < bvector.value.count else {
      throw RuntimeError.range(parameter: 2,
                               of: "bytevector-u8-set!",
                               index,
                               min: Int64(i),
                               max: Int64(bvector.value.count - 1))
    }
    bvector.value[i] = try expr.asUInt8()
    return .void
  }
  
  func bytevectorCopy(_ bvec: Expr, args: Arguments) throws -> Expr {
    let bvector = try bvec.asByteVector()
    guard let (s, e) = args.optional(Expr.makeNumber(bvector.value.count),
                                     Expr.makeNumber(0)) else {
      throw RuntimeError.argumentCount(of: "bytevector-copy",
                                       min: 2,
                                       max: 2,
                                       args: .pair(bvec, .makeList(args)))
    }
    let (start, end) = (try s.asInt(), try e.asInt())
    guard start >= 0 && start < bvector.value.count else {
      throw RuntimeError.range(parameter: 2,
                               of: "bytevector-copy",
                               s,
                               min: 0,
                               max: Int64(bvector.value.count - 1))
    }
    guard end >= start && end <= bvector.value.count else {
      throw RuntimeError.range(parameter: 3,
                               of: "bytevector-copy",
                               e,
                               min: Int64(start),
                               max: Int64(bvector.value.count))
    }
    var res = [UInt8](repeating: 0, count: end - start)
    for i in start..<end {
      res[i - start] = bvector.value[i]
    }
    return .bytes(MutableBox(res))
  }
  
  func bytevectorCopyInto(_ to: Expr, at: Expr, from: Expr, args: Arguments) throws -> Expr {
    let toVec = try to.asByteVector()
    let toStart = try at.asInt()
    guard toStart >= 0 && toStart < toVec.value.count else {
      throw RuntimeError.range(parameter: 2,
                               of: "bytevector-copy!",
                               at,
                               min: 0,
                               max: Int64(toVec.value.count - 1))
    }
    let fromVec = try from.asByteVector()
    guard let (s, e) = args.optional(Expr.makeNumber(fromVec.value.count),
                                     Expr.makeNumber(0)) else {
      throw RuntimeError.argumentCount(of: "bytevector-copy!",
                                       min: 3,
                                       max: 5,
                                       args: .pair(to, .pair(at, .pair(from, .makeList(args)))))
    }
    let (start, end) = (try s.asInt(), try e.asInt())
    guard start >= 0 && start < fromVec.value.count else {
      throw RuntimeError.range(parameter: 4,
                               of: "bytevector-copy!",
                               s,
                               min: 0,
                               max: Int64(fromVec.value.count - 1))
    }
    guard end >= start && end <= fromVec.value.count else {
      throw RuntimeError.range(parameter: 5,
                               of: "bytevector-copy!",
                               e,
                               min: Int64(start),
                               max: Int64(fromVec.value.count))
    }
    guard toStart + end - start - 1 < toVec.value.count else {
      throw RuntimeError.eval(.targetBytevectorTooSmall, to)
    }
    if toStart <= start {
      for i in start..<end {
        toVec.value[toStart + i - start] = fromVec.value[i]
      }
    } else if toStart > start {
      for i in (start..<end).reversed() {
        toVec.value[toStart + i - start] = fromVec.value[i]
      }
    }
    return .void
  }
  
  func bytevectorAppend(_ exprs: Arguments) throws -> Expr {
    var res = [UInt8]()
    for expr in exprs {
      res.append(contentsOf: try expr.asByteVector().value)
    }
    return .bytes(MutableBox(res))
  }
  
  func utf8ToString(_ bvec: Expr, args: Arguments) throws -> Expr {
    let subvec = try self.subVector("utf8->string", bvec, args)
    var generator = subvec.makeIterator()
    var str = ""
    var decoder = UTF8()
    while case .scalarValue(let scalar) = decoder.decode(&generator) {
      str.append(String(scalar))
    }
    return .makeString(str)
  }
  
  func stringToUtf8(_ string: Expr, args: Arguments) throws -> Expr {
    let substr = try self.subString("string->utf8", string, args)
    var res = [UInt8]()
    for byte in substr.utf8 {
      res.append(byte)
    }
    return .bytes(MutableBox(res))
  }
  
  func bytevectorToBase64(_ bvec: Expr, args: Arguments) throws -> Expr {
    return .makeString(Data(bytes:
      try self.subVector("bytevector->base64", bvec, args)).base64EncodedString())
  }
  
  func base64ToBytevector(_ string: Expr, args: Arguments) throws -> Expr {
    let substr = try self.subString("base64->bytevector", string, args)
    guard let data = Data(base64Encoded: substr, options: []) else {
      throw RuntimeError.eval(.cannotDecodeBytevector, .makeString(substr))
    }
    let count = data.count
    var res = [UInt8](repeating: 0, count: count)
    data.copyBytes(to: &res, count: count)
    return .bytes(MutableBox(res))
  }
  
  private func bytevectorDeflate(_ bvec: Expr, args: Arguments) throws -> Expr {
    let subvec = try self.subVector("bytevector-deflate", bvec, args)
    guard let data = Data(bytes: subvec).deflate() else {
      throw RuntimeError.eval(.cannotEncodeBytevector, .bytes(MutableBox(subvec)))
    }
    let count = data.count
    var res = [UInt8](repeating: 0, count: count)
    data.copyBytes(to: &res, count: count)
    return .bytes(MutableBox(res))
  }
  
  private func bytevectorInflate(_ bvec: Expr, args: Arguments) throws -> Expr {
    let subvec = try self.subVector("bytevector-inflate", bvec, args)
    guard let data = Data(bytes: subvec).inflate() else {
      throw RuntimeError.eval(.cannotDecodeBytevector, .bytes(MutableBox(subvec)))
    }
    let count = data.count
    var res = [UInt8](repeating: 0, count: count)
    data.copyBytes(to: &res, count: count)
    return .bytes(MutableBox(res))
  }
  
  private func subString(_ procname: String, _ string: Expr, _ args: Arguments) throws -> String {
    let st = try string.asString()
    let str = st.utf16
    guard let (s, e) = args.optional(Expr.makeNumber(0), Expr.makeNumber(str.count)) else {
      throw RuntimeError.argumentCount(of: procname,
                                       min: 2,
                                       max: 4,
                                       args: .pair(string, .makeList(args)))
    }
    let (start, end) = (try s.asInt(), try e.asInt())
    if start == 0 && end == str.count {
      return st
    }
    let sidx = str.index(str.startIndex, offsetBy: start)
    guard sidx <= str.endIndex else {
      throw RuntimeError.range(parameter: 3, of: procname, s, min: 0, max: Int64(str.count))
    }
    let eidx = str.index(str.startIndex, offsetBy: end)
    guard eidx <= str.endIndex && sidx <= eidx else {
      throw RuntimeError.range(parameter: 4,
                               of: procname,
                               e,
                               min: Int64(end),
                               max: Int64(str.count))
    }
    var uniChars: [UniChar] = []
    for ch in str[sidx..<eidx] {
      uniChars.append(ch)
    }
    return String(utf16CodeUnits: uniChars, count: uniChars.count)
  }
  
  private func subVector(_ name: String, _ bvec: Expr, _ args: Arguments) throws -> [UInt8] {
    let bvector = try bvec.asByteVector()
    guard let (s, e) = args.optional(Expr.makeNumber(0),
                                     Expr.makeNumber(bvector.value.count)) else {
      throw RuntimeError.argumentCount(of: name, min: 2, max: 2, args: .pair(bvec, .makeList(args)))
    }
    let (start, end) = (try s.asInt(), try e.asInt())
    guard start >= 0 && start <= bvector.value.count else {
      throw RuntimeError.range(parameter: 2, of: name, s, min: 0, max: Int64(bvector.value.count))
    }
    guard end >= start && end <= bvector.value.count else {
      throw RuntimeError.range(parameter: 3,
                               of: name,
                               e,
                               min: Int64(start),
                               max: Int64(bvector.value.count))
    }
    var subvec = [UInt8](repeating: 0, count: end - start)
    for i in start..<end {
      subvec[i - start] = bvector.value[i]
    }
    return subvec
  }
}

