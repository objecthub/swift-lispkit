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
    // Transformations
    self.define(Procedure("transformation?", isTransformation))
    self.define(Procedure("make-transformation", makeTransformation))
    self.define(Procedure("invert", invert))
    self.define(Procedure("translate", translate))
    self.define(Procedure("scale", scale))
    self.define(Procedure("rotate", rotate))
    
    // Colors
    self.define(Procedure("color?", isColor))
    self.define(Procedure("make-color", makeColor))
    self.define(Procedure("color", color))
    self.define(Procedure("color-red", colorRed))
    self.define(Procedure("color-green", colorGreen))
    self.define(Procedure("color-blue", colorBlue))
    self.define(Procedure("color-alpha", colorAlpha))
    
    // Points/sizes
    self.define(Procedure("point", point))
    self.define(Procedure("size", size))
    self.define(Procedure("rect", rect))
  }
  
  
  // Transformations
  
  private func isTransformation(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is Transformation {
      return .true
    }
    return .false
  }
  
  private func makeTransformation(args: Arguments) throws -> Expr {
    var transform = AffineTransform()
    for arg in args {
      transform.append(try self.affineTransform(arg))
    }
    return .object(Transformation(transform))
  }
  
  private func invert(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let tf = obj as? Transformation else {
      throw RuntimeError.type(expr, expected: [.transformationType])
    }
    var affineTransform = tf.affineTransform
    affineTransform.invert()
    return .object(Transformation(affineTransform))
  }
  
  private func translate(dx: Expr, dy: Expr, expr: Expr?) throws -> Expr {
    var transform = try self.affineTransform(expr)
    transform.translate(x: CGFloat(try dx.asDouble(coerce: true)),
                        y: CGFloat(try dy.asDouble(coerce: true)))
    return .object(Transformation(transform))
  }
  
  private func scale(dx: Expr, dy: Expr, expr: Expr?) throws -> Expr {
    var transform = try self.affineTransform(expr)
    transform.scale(x: CGFloat(try dx.asDouble(coerce: true)),
                    y: CGFloat(try dy.asDouble(coerce: true)))
    return .object(Transformation(transform))
  }
  
  private func rotate(angle: Expr, expr: Expr?) throws -> Expr {
    var transform = try self.affineTransform(expr)
    transform.rotate(byRadians: CGFloat(try angle.asDouble(coerce: true)))
    return .object(Transformation(transform))
  }
  
  private func affineTransform(_ expr: Expr?) throws -> AffineTransform {
    guard let tf = expr else {
      return AffineTransform()
    }
    guard case .object(let obj) = tf, let transform = obj as? Transformation else {
      throw RuntimeError.type(tf, expected: [.transformationType])
    }
    return transform.affineTransform
  }
  
  // Colors
  
  private func isColor(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is ImmutableBox<Color> {
      return .true
    }
    return .false
  }
  
  private func makeColor(red: Expr, green: Expr, blue: Expr, alpha: Expr?) throws -> Expr {
    return .object(ImmutableBox(Color(red: try red.asDouble(coerce: true),
                                      green: try green.asDouble(coerce: true),
                                      blue: try blue.asDouble(coerce: true),
                                      alpha: try (alpha ?? .flonum(0.0)).asDouble(coerce: true))))
  }
  
  private func color(_ expr: Expr) -> Expr {
    if case .char(_) = expr {
      return .true
    }
    return .false
  }
  
  private func colorRed(_ expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let colorRef = obj as? ImmutableBox<Color> else {
      throw RuntimeError.type(expr, expected: [.colorType])
    }
    return .flonum(colorRef.value.red)
  }
  
  private func colorGreen(_ expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let colorRef = obj as? ImmutableBox<Color> else {
      throw RuntimeError.type(expr, expected: [.colorType])
    }
    return .flonum(colorRef.value.green)
  }
  
  private func colorBlue(_ expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let colorRef = obj as? ImmutableBox<Color> else {
      throw RuntimeError.type(expr, expected: [.colorType])
    }
    return .flonum(colorRef.value.blue)
  }
  
  private func colorAlpha(_ expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let colorRef = obj as? ImmutableBox<Color> else {
      throw RuntimeError.type(expr, expected: [.colorType])
    }
    return .flonum(colorRef.value.alpha)
  }
  
  // Points/sizes
  
  private func point(x: Expr, y: Expr) throws -> Expr {
    return .pair(.flonum(try x.asDouble(coerce: true)),
                 .flonum(try y.asDouble(coerce: true)))
  }
  
  private func size(w: Expr, h: Expr) throws -> Expr {
    return .pair(.flonum(try w.asDouble(coerce: true)),
                 .flonum(try h.asDouble(coerce: true)))
  }
  
  private func rect(p: Expr, s: Expr) throws -> Expr {
    guard case .pair(let x, let y) = p else {
      throw RuntimeError.type(p, expected: [.pairType])
    }
    guard case .pair(let w, let h) = s else {
      throw RuntimeError.type(s, expected: [.pairType])
    }
    guard case .flonum(_) = x else {
      throw RuntimeError.type(x, expected: [.floatType])
    }
    guard case .flonum(_) = y else {
      throw RuntimeError.type(y, expected: [.floatType])
    }
    guard case .flonum(_) = w else {
      throw RuntimeError.type(w, expected: [.floatType])
    }
    guard case .flonum(_) = h else {
      throw RuntimeError.type(h, expected: [.floatType])
    }
    return .pair(p, s)
  }
}
