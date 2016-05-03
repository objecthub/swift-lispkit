//
//  Variable.swift
//  LispKit
//
//  Created by Matthias Zenger on 07/02/2016.
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
/// `Variable` implements a mutable expression.
///
public final class Variable: ManagedObject, CustomStringConvertible {
  
  /// The current value of the variable.
  public var value: Expr
  
  /// Maintain object statistics.
  internal static let stats = Stats("Variable")

  /// Update object statistics.
  deinit {
    Variable.stats.dealloc()
  }
  
  /// Create a new variable with a given initial value.
  public init(_ value: Expr) {
    self.value = value
    super.init(Variable.stats)
  }
  
  /// Mark variable object.
  public override func mark(tag: UInt8) {
    if self.tag != tag {
      self.tag = tag
      self.value.mark(tag)
    }
  }
  
  /// Clear variable value
  public override func clean() {
    self.value = .Undef
  }
  
  /// A string representation of this variable.
  public var description: String {
    return "«\(self.value)»"
  }
}
