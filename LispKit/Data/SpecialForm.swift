//
//  SpecialForm.swift
//  LispKit
//
//  Created by Matthias Zenger on 21/01/2016.
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

///
/// `SpecialForm` objects represent special forms which define new expression
/// types.
///
public final class SpecialForm: Reference, CustomStringConvertible {
  
  /// There are two kinds of special forms:
  ///    1. Primitives: These are built-in special forms
  ///    2. Macros: These are user-defined and expressed in terms of a macro transformer
  internal enum Kind {
    case primitive(FormCompiler)
    case macro(Procedure)
  }
  
  /// Special form kind
  internal let kind: Kind
  
  /// Initializer for primitive special forms
  public init(_ compiler: FormCompiler) {
    self.kind = .primitive(compiler)
  }
  
  /// Initializer for special forms based on macro transformers
  public init(_ transformer: Procedure) {
    self.kind = .macro(transformer)
  }
  
  public func mark(_ tag: UInt8) {
    switch self.kind {
      case .primitive(_):
        break
      case .macro(let proc):
        proc.mark(tag)
    }
  }
  
  /// A textual description
  public var description: String {
    return "special:\(self.identityString)"
  }
}
