//
//  DrawingLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 06/07/2018.
//  Copyright © 2018 ObjectHub. All rights reserved.
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

public final class DrawingLibrary: NativeLibrary {
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "draw"]
  }
  
  /// Declarations of the library.
  public override func declarations() {
    // Colors
    self.define(Procedure("color?", isColor))
    self.define(Procedure("make-color", makeColor))
    self.define(Procedure("color", color))
    self.define(Procedure("color-red", colorRed))
    self.define(Procedure("color-green", colorGreen))
    self.define(Procedure("color-blue", colorBlue))
    self.define(Procedure("color-alpha", colorAlpha))
  }
  
  func isColor(_ expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is ImmutableBox<Color> {
      return .true
    }
    return .false
  }
  
  func makeColor(red: Expr, green: Expr, blue: Expr, alpha: Expr?) throws -> Expr {
    return .object(ImmutableBox(Color(red: try red.asDouble(coerce: true),
                                      green: try green.asDouble(coerce: true),
                                      blue: try blue.asDouble(coerce: true),
                                      alpha: try (alpha ?? .flonum(0.0)).asDouble(coerce: true))))
  }
  
  func color(_ expr: Expr) -> Expr {
    if case .char(_) = expr {
      return .true
    }
    return .false
  }
  
  func colorRed(_ expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let colorRef = obj as? ImmutableBox<Color> else {
      throw RuntimeError.type(expr, expected: [.colorType])
    }
    return .flonum(colorRef.value.red)
  }
  
  func colorGreen(_ expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let colorRef = obj as? ImmutableBox<Color> else {
      throw RuntimeError.type(expr, expected: [.colorType])
    }
    return .flonum(colorRef.value.green)
  }
  
  func colorBlue(_ expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let colorRef = obj as? ImmutableBox<Color> else {
      throw RuntimeError.type(expr, expected: [.colorType])
    }
    return .flonum(colorRef.value.blue)
  }
  
  func colorAlpha(_ expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let colorRef = obj as? ImmutableBox<Color> else {
      throw RuntimeError.type(expr, expected: [.colorType])
    }
    return .flonum(colorRef.value.alpha)
  }
}
