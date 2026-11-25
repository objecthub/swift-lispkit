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
public class Cell: ManagedObject, CustomStringConvertible {
  
  /// The current value of the cell.
  ///
  /// Dynamic exclusivity checks are disabled for `value` since the virtual machine accesses
  /// cells constantly (e.g. for mutable local variables). These checks only detect overlapping
  /// accesses within a single thread; they never detected races between threads, so this
  /// attribute does not change thread safety: concurrent accesses must be synchronized
  /// explicitly, as `AtomicBox` does. Without the checks, the following becomes undefined
  /// behavior instead of a runtime trap: passing `value` as an `inout` argument, or mutating it
  /// in place, while code that is called during this access (e.g. a Scheme procedure)
  /// accesses the same cell. Don't do this; copy `value` first and assign the result.
  @exclusivity(unchecked) public var value: Expr

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
