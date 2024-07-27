//
//  HTTPServer.swift
//  LispKit
//
//  Created by Matthias Zenger on 23/07/2024.
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
import NanoHTTP

public final class HTTPServerLibrary: NativeLibrary {
  
  // Flow identifiers
  // private let codeGrant: Symbol
  
  /// Initialize symbols.
  public required init(in context: Context) throws {
    // self.codeGrant = context.symbols.intern("code-grant")
    try super.init(in: context)
  }
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "http", "server"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
  }

  /// Declarations of the library.
  public override func declarations() {
    
  }
  
  /// Initializations of the library.
  public override func initializations() {
  }
  
  
}

public final class HTTPServer: NanoHTTPServer, CustomExpr {
  public static let type = Type.objectType(Symbol(uninterned: "http-server"))
  
  public var type: Type {
    return Self.type
  }
  
  public final var identity: UInt {
    return UInt(bitPattern: ObjectIdentifier(self))
  }
  
  public final var identityString: String {
    return String(self.identity, radix: 16)
  }
  
  public final func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(self))
  }
  
  public var hash: Int {
    return self.hashValue
  }
  
  public var tagString: String {
    return "\(Self.type) \(self.identityString)"
  }
  
  public func equals(to expr: Expr) -> Bool {
    guard case .object(let obj) = expr,
          let other = obj as? HTTPServer else {
      return false
    }
    return self == other
  }
  
  public func unpack(in context: Context) -> Exprs {
    return [.makeString(self.identityString)]
  }
  
  public static func ==(lhs: HTTPServer, rhs: HTTPServer) -> Bool {
    return lhs === rhs
  }
}
