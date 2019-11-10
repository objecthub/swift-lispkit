//
//  Cell.swift
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
/// `Cell` implements a mutable expression.
///
public final class Cell: ManagedObject, CustomStringConvertible {
  
  /// The current value of the cell.
  public var value: Expr

  /// Create a new cell with a given initial value.
  public init(_ value: Expr) {
    self.value = value
  }
  
  /// Clear cell value.
  public override func clean() {
    self.value = .undef
  }
  
  /// A string representation of this cell.
  public var description: String {
    return "«cell \(self.value)»"
  }
}
