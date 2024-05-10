//
//  CustomExpr.swift
//  LispKit
//
//  Created by Matthias Zenger on 05/05/2024.
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

///
/// Protocol implementing custom values.
///
public protocol CustomExpr: Hashable {
  var type: Type { get }
  var typeDescription: String { get }
  var string: String { get }
  var tagString: String { get }
  var hash: Int { get }
  func equals(to expr: Expr) -> Bool
  func eqv(to expr: Expr) -> Bool
  func eq(to expr: Expr) -> Bool
  func mark(in gc: GarbageCollector)
  func unpack() -> Exprs
}

extension CustomExpr {
  public var type: Type {
    preconditionFailure("NativeObject.type undefined: NativeObject instantiated")
  }
  
  public var string: String {
    return "#<\(self.tagString)>"
  }
  
  public var typeDescription: String {
    return self.type.description
  }
  
  public func eqv(to expr: Expr) -> Bool {
    return self.equals(to: expr)
  }
  
  public func eq(to expr: Expr) -> Bool {
    return self.equals(to: expr)
  }
  
  public func mark(in gc: GarbageCollector) {
    // do nothing by default
  }
}
