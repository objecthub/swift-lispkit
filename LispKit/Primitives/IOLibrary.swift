//
//  IOLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 23/01/2016.
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


public final class IOLibrary: Library {
  
  public override func export() {
    define("display", Procedure(display))
    define("newline", Procedure(newline))
  }
  
  func display(expr: Expr) -> Expr {
    switch expr {
      case .Str(let str):
        context.console.print(str.value)
      default:
        context.console.print(expr.description)
    }
    return .Void
  }
  
  func newline() -> Expr {
    context.console.print("\n")
    return .Void
  }
}
