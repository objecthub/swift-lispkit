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
  }

  private func isDatatypeDeconstruction(_ args: Expr) -> Expr {
    guard case .pair(.symbol(InternalLibrary.deconstructionTag), .pair(_, .null)) = args else {
      return .false
    }
    return .true
  }

  private func deconstructDatatype(_ args: Arguments) throws -> (Procedure, Exprs) {
    guard args.count == 2 else {
      throw RuntimeError.argumentCount(num: 2, args: .makeList(args))
    }
    guard case .procedure(let proc) = args.first! else {
      throw RuntimeError.type(args.first!, expected: [.procedureType])
    }
    return (proc, [.symbol(InternalLibrary.deconstructionTag), args[args.startIndex + 1]])
  }
}
