//
//  Exprs.swift
//  LispKit
//
//  Created by Matthias Zenger on 24/01/2016.
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


/// A sequence of expressions, represented as an array.
public typealias Exprs = [Expr]

/// An immutable empty set of expressions
public let NO_EXPRS = Exprs()

/// Add evaluation capabilities to arrays implementing the `Evaluatable` and the
/// `CustomStringConvertible` protocols
extension Array where Element: CustomStringConvertible {
    
  /// Returns a description of this array of expressions with the given prefix and separator
  public func descriptionWithPrefix(_ prefix: String, andSeparator separator: String) -> String {
    var res = prefix, sep = ""
    for expr in self {
      res += sep
      res += expr.description
      sep = separator
    }
    return res
  }
}

/// Equality function for sequences of expressions.
public func ==(lhs: Exprs, rhs: Exprs) -> Bool {
  guard lhs.count == rhs.count else {
    return false
  }
  for i in 0..<lhs.count {
    guard lhs[i] == rhs[i] else {
      return false
    }
  }
  return true
}
