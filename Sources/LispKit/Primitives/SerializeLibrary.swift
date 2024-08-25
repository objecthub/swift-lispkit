//
//  SerializeLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 24/08/2024.
//  Copyright © 2024 ObjectHub. All rights reserved.
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

public final class SerializeLibrary: NativeLibrary {
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "serialize"]
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("serializable?", isSerializable))
    self.define(Procedure("deserializable?", isDeserializable))
    self.define(Procedure("serialize", serialize))
    self.define(Procedure("deserialize", deserialize))
  }
  
  private func isSerializable(_ expr: Expr) -> Expr {
    return .makeBoolean(expr.isSerializable)
  }
  
  private func isDeserializable(_ expr: Expr, _ args: Arguments) -> Expr {
    do {
      let subvec = try BytevectorLibrary.subVector("deserialize", expr, args)
      _ = try Serialization(data: Data(subvec)).deserialize(in: self.context)
      return .true
    } catch {
      return .false
    }
  }
  
  private func serialize(_ expr: Expr, _ default: Expr?) throws -> Expr {
    if let `default` {
      return try BytevectorLibrary.bytevector(from:
                   expr.serialization(unserializable: `default`).serialize())
    } else {
      return try BytevectorLibrary.bytevector(from:
                   expr.serialization().serialize())
    }
  }
  
  private func deserialize(_ expr: Expr, _ args: Arguments) throws -> Expr {
    let subvec = try BytevectorLibrary.subVector("deserialize", expr, args)
    return try Serialization(data: Data(subvec)).deserialize(in: self.context)
  }
}
