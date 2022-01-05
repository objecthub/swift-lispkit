//
//  InternalLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 25/08/2019.
//  Copyright © 2019 ObjectHub. All rights reserved.
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
/// Internal library
///
public final class InternalLibrary: NativeLibrary {

  public static let deconstructionTag = Symbol(uninterned: "deconstruct-datatype")

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "internal"]
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("datatype-deconstruction?", self.isDatatypeDeconstruction))
    self.define(Procedure("deconstruct-datatype", self.deconstructDatatype))
    self.define(Procedure("make-nan", self.makeNan))
    self.define(Procedure("nan-quiet?", self.nanQuiet))
    self.define(Procedure("nan-payload", self.nanPayload))
    self.define(Procedure("flbits=?", self.doubleBitsEquals))
  }

  private func isDatatypeDeconstruction(args: Expr) -> Expr {
    guard case .pair(.symbol(InternalLibrary.deconstructionTag), .pair(_, .null)) = args else {
      return .false
    }
    return .true
  }

  private func deconstructDatatype(args: Arguments) throws -> (Procedure, Exprs) {
    guard args.count == 2 else {
      throw RuntimeError.argumentCount(num: 2, args: .makeList(args))
    }
    guard case .procedure(let proc) = args.first! else {
      throw RuntimeError.type(args.first!, expected: [.procedureType])
    }
    return (proc, [.symbol(InternalLibrary.deconstructionTag), args[args.startIndex + 1]])
  }
  
  private func makeNan(neg: Expr, quiet: Expr, payload: Expr) throws -> Expr {
    // This is not portable code; I couldn't figure out a different way to implement this
    // function
    let pload = try payload.asInt64()
    guard pload >= 0 && pload <= (Int64(1) << 50) else {
      return .false
    }
    let num = Double(nan: UInt64(bitPattern: pload), signaling: quiet.isFalse)
    if neg.isTrue {
      return .makeNumber(Double(bitPattern: num.bitPattern | (UInt64(1) << 63)))
    } else {
      return .makeNumber(num)
    }
  }
  
  private func nanQuiet(expr: Expr) throws -> Expr {
    let num = try expr.asDouble(coerce: true)
    return .makeBoolean(num.isNaN && !num.isSignalingNaN)
  }
  
  private func nanPayload(expr: Expr) throws -> Expr {
    let num = try expr.asDouble(coerce: true)
    return .makeNumber(num.bitPattern & ((UInt64(1) << 50) - 1))
  }
  
  private func doubleBitsEquals(fst: Expr, snd: Expr) throws -> Expr {
    let num1 = try fst.asDouble(coerce: true)
    let num2 = try snd.asDouble(coerce: true)
    return .makeBoolean(num1.bitPattern == num2.bitPattern)
  }
}
