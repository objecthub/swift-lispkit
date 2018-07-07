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
import Cocoa

public final class DrawingLibrary: NativeLibrary {
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "draw"]
  }
  
  /// Declarations of the library.
  public override func declarations() {
    
    // Shapes
    self.define(Procedure("shape?", isShape))
    self.define(Procedure("make-shape", makeShape))
    self.define(Procedure("copy-shape", copyShape))
    self.define(Procedure("make-polygon", makePolygon))
    self.define(Procedure("make-rect", makeRect))
    self.define(Procedure("make-oval", makeOval))
    self.define(Procedure("make-arc", makeArc))
    self.define(Procedure("transform-shape", transformShape))
    self.define(Procedure("interpolate", interpolate))
    self.define(Procedure("interpolate-catmull/rom", interpolateCatmullRom))
    self.define(Procedure("move-to", moveTo))
    self.define(Procedure("line-to", lineTo))
    self.define(Procedure("curve-to", curveTo))
    self.define(Procedure("add-glyphs", addGlyphs))
    self.define(Procedure("add-shape", addShape))
    
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
    self.define(Procedure("font?", isFont))
    self.define(Procedure("font", font))
    self.define(Procedure("font-name", fontName))
    self.define(Procedure("font-size", fontSize))
    self.define(Procedure("point", point))
    self.define(Procedure("size", size))
  }
  
  // Shapes
  
  private func isShape(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is Shape {
      return .true
    }
    return .false
  }
  
  private func makeShape(expr: Expr?) throws -> Expr {
    if let prototype = expr {
      return .object(Shape(.shape(try self.shape(from: prototype)), flipped: true))
    }
    return .object(Shape(flipped: true))
  }
  
  private func copyShape(expr: Expr) throws -> Expr {
    return .object(Shape(copy: try self.shape(from: expr)))
  }
  
  //  (define (make-shape) ...)
  //  (define (make-polygon points) ...)
  //  (define (make-rect point size [xradius] [yradius]) ...)
  //  (define (make-oval point size) ...)
  //  (define (make-arc point radius [start end]) ...)
  //  (define (copy-shape shape) ...)
  //  (define (transform-shape shape transformation) ...)
  //  (define (interpolate points [closed] [alpha]) ...)
  //  (define (interpolate-catmull/rom points [closed] [alpha]) ...)
  //  (define (move-to shape point) ...)
  //  (define (line-to shape point) ...)
  //  (define (curve-to shape point cntrl1 cntrl2) ...)
  //  (define (add-glyphs shape text font size) ...)
  //  (define (add-shape shape other) ...)
  
  private func shape(from expr: Expr) throws -> Shape {
    guard case .object(let obj) = expr, let shape = obj as? Shape else {
      throw RuntimeError.type(expr, expected: [.shapeType])
    }
    return shape
  }
  
  private func pointList(_ args: Arguments) -> Expr {
    var first = true
    var res: Expr = .null
    for arg in args.reversed() {
      if first {
        switch arg {
          case .null, .pair(.pair(.flonum(_), .flonum(_)), _):
            res = arg
          default:
            res = .pair(arg, .null)
        }
        first = false
      } else {
        res = .pair(arg, res)
      }
    }
    return res
  }
  
  private func makePolygon(args: Arguments) throws -> Expr {
    let shape = Shape(flipped: true)
    var pointList = self.pointList(args)
    while case .pair(let point, let rest) = pointList {
      guard case .pair(.flonum(let x), .flonum(let y)) = point else {
        throw RuntimeError.eval(.invalidPoint, point)
      }
      if shape.isEmpty {
        shape.append(.move(to: NSPoint(x: x, y: y)))
      } else {
        shape.append(.line(to: NSPoint(x: x, y: y)))
      }
      pointList = rest
    }
    return .object(shape)
  }
  
  private func makeRect(point: Expr, size: Expr, xradius: Expr?, yradius: Expr?) throws -> Expr {
    guard case .pair(.flonum(let x), .flonum(let y)) = point else {
      throw RuntimeError.eval(.invalidPoint, point)
    }
    guard case .pair(.flonum(let w), .flonum(let h)) = size else {
      throw RuntimeError.eval(.invalidSize, size)
    }
    if let xrad = try xradius?.asDouble(coerce: true) {
      if let yrad = try yradius?.asDouble(coerce: true) {
        return .object(Shape(.roundedRect(NSRect(x: x, y: y, width: w, height: h),
                                          xradius: xrad,
                                          yradius: yrad), flipped: true))
      }
      return .object(Shape(.roundedRect(NSRect(x: x, y: y, width: w, height: h),
                                        xradius: xrad,
                                        yradius: xrad), flipped: true))
    }
    return .object(Shape(.rect(NSRect(x: x, y: y, width: w, height: h)), flipped: true))
  }
  
  private func makeOval(point: Expr, size: Expr) throws -> Expr {
    guard case .pair(.flonum(let x), .flonum(let y)) = point else {
      throw RuntimeError.eval(.invalidPoint, point)
    }
    guard case .pair(.flonum(let w), .flonum(let h)) = size else {
      throw RuntimeError.eval(.invalidSize, size)
    }
    return .object(Shape(.oval(NSRect(x: x, y: y, width: w, height: h)), flipped: true))
  }
  
  private func makeArc(point: Expr,
                       radius: Expr,
                       start: Expr,
                       end: Expr?,
                       clockwise: Expr?) throws -> Expr {
    guard case .pair(.flonum(let x), .flonum(let y)) = point else {
      throw RuntimeError.eval(.invalidPoint, point)
    }
    let rad = try radius.asDouble(coerce: true)
    let start = try start.asDouble(coerce: true)
    if let end = try end?.asDouble(coerce: true) {
      return .object(Shape(.arc(center: NSPoint(x: x, y: y),
                                radius: rad,
                                startAngle: start,
                                endAngle: end,
                                clockwise: clockwise?.isTrue ?? true), flipped: true))
    }
    return .object(Shape(.arc(center: NSPoint(x: x, y: y),
                              radius: rad,
                              startAngle: 0,
                              endAngle: Double.pi * 2.0,
                              clockwise: clockwise?.isTrue ?? true), flipped: true))
  }
  
  private func transformShape(shape: Expr, transformation: Expr) throws -> Expr {
    return .object(Shape(.transformed(try self.shape(from: shape),
                                      try self.transformation(from: transformation)),
                         flipped: true))
  }
  
  private func interpolate(points: Expr, closed: Expr?, alpha: Expr?) throws -> Expr {
    let nspoints = try self.nsPoints(from: points)
    return .object(Shape(.interpolated(nspoints,
                     method: .hermite(closed: closed?.isTrue ?? false,
                                      alpha: try alpha?.asDouble(coerce: true) ?? 1.0/3.0)),
             flipped: true))
  }
  
  private func interpolateCatmullRom(points: Expr, closed: Expr?, alpha: Expr?) throws -> Expr {
    let nspoints = try self.nsPoints(from: points)
    return .object(Shape(.interpolated(nspoints,
                     method: .catmullRom(closed: closed?.isTrue ?? false,
                                         alpha: try alpha?.asDouble(coerce: true) ?? 1.0/3.0)),
             flipped: true))
  }
  
  private func nsPoints(from expr: Expr) throws -> [NSPoint] {
    var nspoints: [NSPoint] = []
    var pointList = expr
    while case .pair(let point, let rest) = pointList {
      guard case .pair(.flonum(let x), .flonum(let y)) = point else {
        throw RuntimeError.eval(.invalidPoint, point)
      }
      nspoints.append(NSPoint(x: x, y: y))
      pointList = rest
    }
    return nspoints
  }
  
  private func moveTo(shape: Expr, point: Expr) throws -> Expr {
    guard case .pair(.flonum(let x), .flonum(let y)) = point else {
      throw RuntimeError.eval(.invalidPoint, point)
    }
    (try self.shape(from: shape)).append(.move(to: NSPoint(x: x, y: y)))
    return .void
  }
  
  private func lineTo(shape: Expr, args: Arguments) throws -> Expr {
    let shape = try self.shape(from: shape)
    var points = self.pointList(args)
    while case .pair(let point, let rest) = points {
      guard case .pair(.flonum(let x), .flonum(let y)) = point else {
        throw RuntimeError.eval(.invalidPoint, point)
      }
      shape.append(.line(to: NSPoint(x: x, y: y)))
      points = rest
    }
    return .void
  }
  
  private func curveTo(shape: Expr, point: Expr, ctrl1: Expr, ctrl2: Expr) throws -> Expr {
    guard case .pair(.flonum(let x), .flonum(let y)) = point else {
      throw RuntimeError.eval(.invalidPoint, point)
    }
    guard case .pair(.flonum(let c1x), .flonum(let c1y)) = ctrl1 else {
      throw RuntimeError.eval(.invalidPoint, ctrl1)
    }
    guard case .pair(.flonum(let c2x), .flonum(let c2y)) = ctrl2 else {
      throw RuntimeError.eval(.invalidPoint, ctrl2)
    }
    (try self.shape(from: shape)).append(
      .curve(to: NSPoint(x: x, y: y),
             controlCurrent: NSPoint(x: c1x, y: c1y),
             controlTarget: NSPoint(x: c2x, y: c2y)))
    return .void
  }
  
  private func addGlyphs(shape: Expr, text: Expr, font: Expr, size: Expr) throws -> Expr {
    guard case .object(let obj) = font, let fontBox = obj as? ImmutableBox<NSFont> else {
      throw RuntimeError.type(font, expected: [.fontType])
    }
    guard case .pair(.flonum(let w), .flonum(let h)) = size else {
      throw RuntimeError.eval(.invalidSize, size)
    }
    (try self.shape(from: shape)).append(
      .text(try text.asString(), in: fontBox.value, width: w, height: h))
    return .void
  }
  
  private func addShape(shape: Expr, other: Expr) throws -> Expr {
    (try self.shape(from: shape)).append(.include((try self.shape(from: other))))
    return .void
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
  
  private func transformation(from expr: Expr) throws -> Transformation {
    guard case .object(let obj) = expr, let transform = obj as? Transformation else {
      throw RuntimeError.type(expr, expected: [.transformationType])
    }
    return transform
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
  
  // Points/sizes/fonts
  
  private func point(x: Expr, y: Expr) throws -> Expr {
    return .pair(.flonum(try x.asDouble(coerce: true)),
                 .flonum(try y.asDouble(coerce: true)))
  }
  
  private func size(w: Expr, h: Expr) throws -> Expr {
    return .pair(.flonum(try w.asDouble(coerce: true)),
                 .flonum(try h.asDouble(coerce: true)))
  }
  
  private func isFont(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is ImmutableBox<NSFont> {
      return .true
    }
    return .false
  }
  
  private func font(font: Expr, size: Expr) throws -> Expr {
    guard let nsfont = NSFont(name: try font.asString(),
                              size: CGFloat(try size.asDouble(coerce: true))) else {
      throw RuntimeError.eval(.unknownFont, font, size)
    }
    return .object(ImmutableBox(nsfont))
  }
  
  private func fontName(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let fontBox = obj as? ImmutableBox<NSFont> else {
      throw RuntimeError.type(expr, expected: [.fontType])
    }
    return .makeString(fontBox.value.fontName)
  }
  
  private func fontSize(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let fontBox = obj as? ImmutableBox<NSFont> else {
      throw RuntimeError.type(expr, expected: [.fontType])
    }
    return .flonum(Double(fontBox.value.pointSize))
  }
}
