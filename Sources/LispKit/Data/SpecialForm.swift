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
  
  /// Original name of the special form, if available; this is the name of the special
  /// form at definition time.
  public let originalName: String?
  
  /// Initializer for primitive special forms
  public init(_ name: String?, _ compiler: @escaping FormCompiler) {
    self.kind = .primitive(compiler)
    self.originalName = name
  }
  
  /// Initializer for special forms based on macro transformers
  public init(_ name: String?, _ transformer: Procedure) {
    self.kind = .macro(transformer)
    self.originalName = name
  }
  
  /// Returns the name of this special form. This method either returns the name of a primitive
  /// special form or a macro transformer. If the name isn't available, the identity is returned
  /// as a hex string.
  public var name: String {
    guard let originalName = self.originalName else {
      return self.identityString
    }
    switch self.kind {
      case .primitive(_):
        return originalName
      case .macro(_):
        return "\(originalName)@\(self.identityString)"
    }
  }
  
  /// A textual description
  public var description: String {
    return "«special \(self.name)»"
  }
}
