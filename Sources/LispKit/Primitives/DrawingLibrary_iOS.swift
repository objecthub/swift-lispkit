//
//  DrawingLibrary_iOS.swift
//  LispKit
//
//  Created by Matthias Zenger on 24/04/2021.
//  Copyright © 2021 ObjectHub. All rights reserved.
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
import MarkdownKit
import UIKit

///
/// This class implements the library `(lispkit draw)`. 
///
public final class DrawingLibrary: NativeLibrary {
  
  /// Exported parameter objects
  public let drawingParam: Procedure
  public let shapeParam: Procedure
  
  /// Symbols used in enumeration values
  
  // Bitmap file types
  private let formatPDF: Symbol
  private let formatPNG: Symbol
  private let formatJPG: Symbol
  private let formatGIF: Symbol
  private let formatBMP: Symbol
  private let formatTIFF: Symbol
  
  // Shape flip orientation
  private let orientationHorizontal: Symbol
  private let orientationVertical: Symbol
  private let orientationMirror: Symbol
  
  // Interpolation algorithm
  private let interpolateHermite: Symbol
  private let interpolateCatmullRom: Symbol
  
  // Image composition operation
  private let compositionClear: Symbol
  private let compositionCopy: Symbol
  private let compositionMultiply: Symbol
  private let compositionOverlay: Symbol
  private let compositionSourceOver: Symbol
  private let compositionSourceIn: Symbol
  private let compositionSourceOut: Symbol
  private let compositionSourceAtop: Symbol
  private let compositionDestinationOver: Symbol
  private let compositionDestinationIn: Symbol
  private let compositionDestinationOut: Symbol
  private let compositionDestinationAtop: Symbol
  
  /// Initialize drawing library, in particular its parameter objects.
  public required init(in context: Context) throws {
    self.drawingParam = Procedure(.null, .false)
    self.shapeParam = Procedure(.null, .false)
    self.formatPDF = context.symbols.intern("pdf")
    self.formatPNG = context.symbols.intern("png")
    self.formatJPG = context.symbols.intern("jpg")
    self.formatGIF = context.symbols.intern("gif")
    self.formatBMP = context.symbols.intern("bmp")
    self.formatTIFF = context.symbols.intern("tiff")
    self.compositionClear = context.symbols.intern("clear")
    self.compositionCopy = context.symbols.intern("copy")
    self.compositionMultiply = context.symbols.intern("multiply")
    self.compositionOverlay = context.symbols.intern("overlay")
    self.compositionSourceOver = context.symbols.intern("source-over")
    self.compositionSourceIn = context.symbols.intern("source-in")
    self.compositionSourceOut = context.symbols.intern("source-out")
    self.compositionSourceAtop = context.symbols.intern("source-atop")
    self.compositionDestinationOver = context.symbols.intern("destination-over")
    self.compositionDestinationIn = context.symbols.intern("destination-in")
    self.compositionDestinationOut = context.symbols.intern("destination-out")
    self.compositionDestinationAtop = context.symbols.intern("destination-atop")
    self.orientationHorizontal = context.symbols.intern("horizontal")
    self.orientationVertical = context.symbols.intern("vertical")
    self.orientationMirror = context.symbols.intern("mirror")
    self.interpolateHermite = context.symbols.intern("hermite")
    self.interpolateCatmullRom = context.symbols.intern("catmull-rom")
    try super.init(in: context)
  }
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "draw"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"],    "define", "define-syntax", "syntax-rules")
    self.`import`(from: ["lispkit", "control"], "let")
    self.`import`(from: ["lispkit", "dynamic"], "parameterize")
  }
  
  /// Declarations of the library.
  public override func declarations() {
    // Parameter objects
    self.define("current-drawing", as: self.drawingParam)
    self.define("current-shape", as: self.shapeParam)
    
    // Drawings
    self.define("drawing-type-tag", as: Drawing.type.objectTypeTag())
    self.define(Procedure("drawing?", isDrawing))
    self.define(Procedure("make-drawing", makeDrawing))
    self.define(Procedure("copy-drawing", copyDrawing))
    self.define(Procedure("clear-drawing", clearDrawing))
    self.define(Procedure("set-color", setColor))
    self.define(Procedure("set-fill-color", setFillColor))
    self.define(Procedure("set-line-width", setLineWidth))
    self.define(Procedure("set-shadow", setShadow))
    self.define(Procedure("remove-shadow", removeShadow))
    self.define(Procedure("enable-transformation", enableTransformation))
    self.define(Procedure("disable-transformation", disableTransformation))
    self.define(Procedure("draw", draw))
    self.define(Procedure("draw-dashed", drawDashed))
    self.define(Procedure("fill", fill))
    self.define(Procedure("fill-gradient", fillGradient))
    self.define(Procedure("draw-line", drawLine))
    self.define(Procedure("draw-rect", drawRect))
    self.define(Procedure("fill-rect", fillRect))
    self.define(Procedure("draw-ellipse", drawEllipse))
    self.define(Procedure("fill-ellipse", fillEllipse))
    self.define(Procedure("draw-text", drawText))
    self.define(Procedure("draw-styled-text", drawStyledText))
    self.define(Procedure("draw-html", drawHtml))
    self.define(Procedure("draw-image", drawImage))
    self.define(Procedure("draw-drawing", drawDrawing))
    self.define(Procedure("clip-drawing", clipDrawing))
    self.define(Procedure("inline-drawing", inlineDrawing))
    self.define(Procedure("save-drawing", saveDrawing))
    self.define(Procedure("save-drawings", saveDrawings))
    
    // Images/bitmaps
    self.define("image-type-tag", as: NativeImage.type.objectTypeTag())
    self.define(Procedure("image?", isImage))
    self.define(Procedure("load-image", loadImage))
    self.define(Procedure("load-image-asset", loadImageAsset))
    self.define(Procedure("save-image", saveImage))
    self.define(Procedure("bytevector->image", bytevectorToImage))
    self.define(Procedure("image->bytevector", imageToBytevector))
    self.define(Procedure("image-size", imageSize))
    self.define(Procedure("set-image-size!", setImageSize))
    self.define(Procedure("bitmap?", isBitmap))
    self.define(Procedure("bitmap-size", bitmapSize))
    self.define(Procedure("bitmap-pixels", bitmapPixels))
    self.define(Procedure("bitmap-ppi", bitmapPpi))
    self.define(Procedure("bitmap-exif-data", bitmapExifData))
    // self.define(Procedure("set-bitmap-exif-data!", setBitmapExifData))
    self.define(Procedure("make-bitmap", makeBitmap))
    self.define(Procedure("bitmap-crop", bitmapCrop))
    self.define(Procedure("bitmap-blur", bitmapBlur))
    self.define(Procedure("save-bitmap", saveBitmap))
    self.define(Procedure("bitmap->bytevector", bitmapToBytevector))
    
    // Shapes
    self.define("shape-type-tag", as: Shape.type.objectTypeTag())
    self.define(Procedure("shape?", isShape))
    self.define(Procedure("make-shape", makeShape))
    self.define(Procedure("copy-shape", copyShape))
    self.define(Procedure("line", line))
    self.define(Procedure("polygon", polygon))
    self.define(Procedure("closed-polygon", closedPolygon))
    self.define(Procedure("rectangle", rectangle))
    self.define(Procedure("circle", circle))
    self.define(Procedure("oval", oval))
    self.define(Procedure("arc", arc))
    self.define(Procedure("glyphs", glyphs))
    self.define(Procedure("move-shape", moveShape))
    self.define(Procedure("scale-shape", scaleShape))
    self.define(Procedure("transform-shape", transformShape))
    self.define(Procedure("flip-shape", flipShape))
    self.define(Procedure("interpolate", interpolate))
    self.define(Procedure("move-to", moveTo))
    self.define(Procedure("line-to", lineTo))
    self.define(Procedure("curve-to", curveTo))
    self.define(Procedure("relative-move-to", relativeMoveTo))
    self.define(Procedure("relative-line-to", relativeLineTo))
    self.define(Procedure("relative-curve-to", relativeCurveTo))
    self.define(Procedure("add-shape", addShape))
    self.define(Procedure("shape-bounds", shapeBounds))
    self.define(Procedure("shape-contains?", shapeContains))
    
    // Transformations
    self.define("transformation-type-tag", as: Transformation.type.objectTypeTag())
    self.define(Procedure("transformation?", isTransformation))
    self.define(Procedure("transformation", transformation))
    self.define(Procedure("invert", invert))
    self.define(Procedure("translate", translate))
    self.define(Procedure("scale", scale))
    self.define(Procedure("rotate", rotate))
    
    // Colors
    self.define("color-type-tag", as: Color.type.objectTypeTag())
    self.define(Procedure("color?", isColor))
    self.define(Procedure("color", color))
    self.define(Procedure("color->hex", colorToHex))
    self.define(Procedure("color-red", colorRed))
    self.define(Procedure("color-green", colorGreen))
    self.define(Procedure("color-blue", colorBlue))
    self.define(Procedure("color-alpha", colorAlpha))
    
    // Fonts
    self.define("font-type-tag", as: NativeFont.type.objectTypeTag())
    self.define(Procedure("font?", isFont))
    self.define(Procedure("font", font))
    self.define(Procedure("font-name", fontName))
    self.define(Procedure("font-family-name", fontFamilyName))
    self.define(Procedure("font-weight", fontWeight))
    self.define(Procedure("font-traits", fontTraits))
    self.define(Procedure("font-has-traits", fontHasTraits))
    self.define(Procedure("font-size", fontSize))
    
    // Points/sizes
    self.define(Procedure("available-fonts", availableFonts))
    self.define(Procedure("available-font-families", availableFontFamilies))
    self.define(Procedure("point?", isPoint))
    self.define(Procedure("point", point))
    self.define(Procedure("point-x", pointX))
    self.define(Procedure("point-y", pointY))
    self.define(Procedure("move-point", movePoint))
    self.define(Procedure("scale-point", scalePoint))
    self.define(Procedure("transform-point", transformPoint))
    self.define(Procedure("size?", isSize))
    self.define(Procedure("size", size))
    self.define(Procedure("size-width", sizeWidth))
    self.define(Procedure("size-height", sizeHeight))
    self.define(Procedure("size-ratio", sizeRatio))
    self.define(Procedure("increase-size", increaseSize))
    self.define(Procedure("scale-size", scaleSize))
    
    // Rects
    self.define(Procedure("rect?", isRect))
    self.define(Procedure("rect", rect))
    self.define(Procedure("rect-point", rectPoint))
    self.define(Procedure("rect-size", rectSize))
    self.define(Procedure("rect-x", rectX))
    self.define(Procedure("rect-y", rectY))
    self.define(Procedure("rect-width", rectWidth))
    self.define(Procedure("rect-height", rectHeight))
    self.define(Procedure("rect-max-point", rectMaxPoint))
    self.define(Procedure("rect-max-x", rectMaxX))
    self.define(Procedure("rect-max-y", rectMaxY))
    self.define(Procedure("rect-mid-point", rectMidPoint))
    self.define(Procedure("rect-mid-x", rectMidX))
    self.define(Procedure("rect-mid-y", rectMidY))
    self.define(Procedure("move-rect", moveRect))
    self.define(Procedure("scale-rect", scaleRect))
    self.define(Procedure("inset-rect", insetRect))
    self.define(Procedure("transform-rect", transformRect))
    self.define(Procedure("intersect-rect", intersectRect))
    self.define(Procedure("union-rect", unionRect))
    self.define(Procedure("rect-contains?", rectContains))
    
    // Utilities
    self.define(Procedure("text-size", textSize))
    self.define(Procedure("styled-text-size", styledTextSize))
    self.define(Procedure("html-size", htmlSize))
    self.define(Procedure("transpose", transpose))
    
    // Define constants
    self.define("zero-point", via: "(define zero-point (point 0 0))")
    self.define("zero-size", via: "(define zero-size (size 0 0))")
    self.define("black", via: "(define black (color 0 0 0))")
    self.define("gray", via: "(define gray (color 0.5 0.5 0.5))")
    self.define("white", via: "(define white (color 1 1 1))")
    self.define("red", via: "(define red (color 1 0 0))")
    self.define("green", via: "(define green (color 0 1 0))")
    self.define("blue", via: "(define blue (color 0 0 1))")
    self.define("yellow", via: "(define yellow (color 1 1 0))")
    self.define("italic", via: "(define italic \(FontTraitModifier.italic.rawValue))")
    self.define("boldface", via: "(define boldface \(FontTraitModifier.boldface.rawValue))")
    self.define("unitalic", via: "(define unitalic \(FontTraitModifier.unitalic.rawValue))")
    self.define("unboldface", via: "(define unboldface \(FontTraitModifier.unboldface.rawValue))")
    self.define("narrow", via: "(define narrow \(FontTraitModifier.narrow.rawValue))")
    self.define("expanded", via: "(define expanded \(FontTraitModifier.expanded.rawValue))")
    self.define("condensed", via: "(define condensed \(FontTraitModifier.condensed.rawValue))")
    self.define("small-caps", via: "(define small-caps \(FontTraitModifier.smallCaps.rawValue))")
    self.define("poster", via: "(define poster \(FontTraitModifier.poster.rawValue))")
    self.define("compressed", via: "(define compressed \(FontTraitModifier.compressed.rawValue))")
    self.define("monospace", via: "(define monospace \(FontTraitModifier.monospace.rawValue))")
    self.define("ultralight", via: "(define ultralight \(UIFont.Weight.ultraLight.rawValue))")
    self.define("thin", via: "(define thin \(UIFont.Weight.thin.rawValue))")
    self.define("light", via: "(define light \(UIFont.Weight.light.rawValue))")
    self.define("book", via: "(define book \(UIFont.Weight.light.rawValue/2.0))")
    self.define("normal", via: "(define normal \(UIFont.Weight.regular.rawValue))")
    self.define("medium", via: "(define medium \(UIFont.Weight.medium.rawValue))")
    self.define("demi", via: "(define demi 0.265)")
    self.define("semi", via: "(define semi \(UIFont.Weight.semibold.rawValue))")
    self.define("bold", via: "(define bold \(UIFont.Weight.bold.rawValue))")
    self.define("extra", via: "(define extra 0.48)")
    self.define("heavy", via: "(define heavy \(UIFont.Weight.heavy.rawValue))")
    self.define("super", via: "(define super 0.59)")
    self.define("ultra", via: "(define ultra \(UIFont.Weight.black.rawValue))")
    self.define("extrablack", via: "(define extrablack 0.7)")
    
    // Syntax definitions
    self.define("drawing", via: """
      (define-syntax drawing
        (syntax-rules ()
          ((_ body ...)
            (let ((d (make-drawing)))
              (parameterize ((current-drawing d)) body ...)
              d))))
    """)
    self.define("with-drawing", via: """
      (define-syntax with-drawing
        (syntax-rules ()
          ((_ d body ...)
            (parameterize ((current-drawing d)) body ...))))
    """)
    self.define("transform", via: """
      (define-syntax transform
        (syntax-rules ()
          ((_ tf body ...)
            (let ((t tf))
              (enable-transformation t)
              body ...
              (disable-transformation t)))))
    """)
    self.define("shape", via: """
      (define-syntax shape
        (syntax-rules ()
          ((_ body ...)
            (let ((s (make-shape)))
              (parameterize ((current-shape s)) body ...)
              s))))
    """)
    self.define("with-shape", via: """
      (define-syntax with-shape
        (syntax-rules ()
          ((_ s body ...)
            (parameterize ((current-shape s)) body ...))))
    """)
  }
  
  private func drawing(from expr: Expr?) throws -> Drawing {
    if let expr = expr {
      guard case .object(let obj) = expr, let drawing = obj as? Drawing else {
        throw RuntimeError.type(expr, expected: [Drawing.type])
      }
      return drawing
    }
    guard let value = self.context.evaluator.getParam(self.drawingParam) else {
      throw RuntimeError.eval(.invalidDefaultDrawing, .false)
    }
    guard case .object(let obj) = value, let drawing = obj as? Drawing else {
      throw RuntimeError.eval(.invalidDefaultDrawing, value)
    }
    return drawing
  }
  
  private func shape(from expr: Expr?) throws -> Shape {
    if let expr = expr {
      guard case .object(let obj) = expr, let shape = obj as? Shape else {
        throw RuntimeError.type(expr, expected: [Shape.type])
      }
      return shape
    }
    guard let value = self.context.evaluator.getParam(self.shapeParam) else {
      throw RuntimeError.eval(.invalidDefaultShape, .false)
    }
    guard case .object(let obj) = value, let shape = obj as? Shape else {
      throw RuntimeError.eval(.invalidDefaultShape, value)
    }
    return shape
  }
  
  private func shape(from args: Arguments) throws -> (Shape, Bool) {
    if case .some(.object(let obj)) = args.last, let shape = obj as? Shape {
      return (shape, true)
    }
    guard let value = self.context.evaluator.getParam(self.shapeParam) else {
      throw RuntimeError.eval(.invalidDefaultShape, .false)
    }
    guard case .object(let obj) = value, let shape = obj as? Shape else {
      throw RuntimeError.eval(.invalidDefaultShape, value)
    }
    return (shape, false)
  }
  
  private func image(from expr: Expr) throws -> UIImage {
    guard case .object(let obj) = expr,
          let imageBox = obj as? NativeImage else {
      throw RuntimeError.type(expr, expected: [NativeImage.type])
    }
    return imageBox.value
  }
  
  // Drawings
  
  private func isDrawing(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is Drawing {
      return .true
    }
    return .false
  }
  
  private func makeDrawing() -> Expr {
    return .object(Drawing())
  }
  
  private func copyDrawing(drawing: Expr) throws -> Expr {
    return .object(Drawing(copy: try self.drawing(from: drawing)))
  }
  
  private func clearDrawing(drawing: Expr) throws -> Expr {
    try self.drawing(from: drawing).clear()
    return .void
  }
  
  private func setColor(color: Expr, drawing: Expr?) throws -> Expr {
    try self.drawing(from: drawing).append(.setStrokeColor(try self.color(from: color)))
    return .void
  }
  
  private func setFillColor(color: Expr, drawing: Expr?) throws -> Expr {
    try self.drawing(from: drawing).append(.setFillColor(try self.color(from: color)))
    return .void
  }
  
  private func setLineWidth(width: Expr, drawing: Expr?) throws -> Expr {
    try self.drawing(from: drawing).append(.setStrokeWidth(try width.asDouble(coerce: true)))
    return .void
  }
  
  private func setShadow(color: Expr, size: Expr, r: Expr, drawing: Expr?) throws -> Expr {
    guard case .pair(.flonum(let w), .flonum(let h)) = size else {
      throw RuntimeError.eval(.invalidSize, size)
    }
    try self.drawing(from: drawing).append(.setShadow(try self.color(from: color),
                                                      dx: w,
                                                      dy: h,
                                                      blurRadius: try r.asDouble(coerce: true)))
    return .void
  }
  
  private func removeShadow(drawing: Expr?) throws -> Expr {
    try self.drawing(from: drawing).append(.removeShadow)
    return .void
  }
  
  private func enableTransformation(tf: Expr, drawing: Expr?) throws -> Expr {
    try self.drawing(from: drawing).append(.concatTransformation(try self.tformation(from: tf)))
    return .void
  }
  
  private func disableTransformation(tf: Expr, drawing: Expr?) throws -> Expr {
    try self.drawing(from: drawing).append(.undoTransformation(try self.tformation(from: tf)))
    return .void
  }
  
  private func draw(shape: Expr, width: Expr?, drawing: Expr?) throws -> Expr {
    let width = try width?.asDouble(coerce: true) ?? 1.0
    try self.drawing(from: drawing).append(.stroke(try self.shape(from: shape), width: width))
    return .void
  }
  
  private func drawDashed(shape: Expr,
                          lengths: Expr,
                          phase: Expr,
                          width: Expr?,
                          drawing: Expr?) throws -> Expr {
    let shape = try self.shape(from: shape)
    var dashLengths: [Double] = []
    var list = lengths
    while case .pair(let len, let rest) = list {
      dashLengths.append(try len.asDouble(coerce: true))
      list = rest
    }
    guard list.isNull else {
      throw RuntimeError.type(lengths, expected: [.properListType])
    }
    let phase = try phase.asDouble(coerce: true)
    let width = try width?.asDouble(coerce: true) ?? 1.0
    try self.drawing(from: drawing).append(
      .strokeDashed(shape, width: width, lengths: dashLengths, phase: phase))
    return .void
  }
  
  private func fill(shape: Expr, drawing: Expr?) throws -> Expr {
    try self.drawing(from: drawing).append(.fill(try self.shape(from: shape)))
    return .void
  }
  
  private func fillGradient(shape: Expr,
                            cols: Expr,
                            gradient: Expr?,
                            drawing: Expr?) throws -> Expr {
    var colors: [Color] = []
    var colorList = cols
    while case .pair(let color, let rest) = colorList {
      colors.append(try self.color(from: color))
      colorList = rest
    }
    guard colors.count > 0 else {
      throw RuntimeError.eval(.unsupportedGradientColorSpec, cols)
    }
    if let gradient = gradient {
      switch gradient {
        case .pair(.flonum(let x), .flonum(let y)):
          try self.drawing(from: drawing).append(
            .fillRadialGradient(try self.shape(from: shape),
                                colors,
                                relativeCenter: CGPoint(x: x, y: y)))
        default:
          let angle = try gradient.asDouble(coerce: true)
          try self.drawing(from: drawing).append(
            .fillLinearGradient(try self.shape(from: shape), colors, angle: angle))
      }
    } else {
      try self.drawing(from: drawing).append(
        .fillLinearGradient(try self.shape(from: shape), colors, angle: 0.0))
    }
    return .void
  }
  
  private func drawLine(start: Expr, end: Expr, drawing: Expr?) throws -> Expr {
    guard case .pair(.flonum(let sx), .flonum(let sy)) = start else {
      throw RuntimeError.eval(.invalidPoint, start)
    }
    guard case .pair(.flonum(let ex), .flonum(let ey)) = end else {
      throw RuntimeError.eval(.invalidPoint, end)
    }
    try self.drawing(from: drawing).append(.strokeLine(CGPoint(x: sx, y: sy),
                                                       CGPoint(x: ex, y: ey)))
    return .void
  }
  
  private func drawRect(expr: Expr, drawing: Expr?) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                     .pair(.flonum(let w), .flonum(let h))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    try self.drawing(from: drawing).append(.strokeRect(CGRect(x: x, y: y, width: w, height: h)))
    return .void
  }
  
  private func fillRect(expr: Expr, drawing: Expr?) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                     .pair(.flonum(let w), .flonum(let h))) = expr else {
                      throw RuntimeError.eval(.invalidRect, expr)
    }
    try self.drawing(from: drawing).append(.fillRect(CGRect(x: x, y: y, width: w, height: h)))
    return .void
  }
  
  private func drawEllipse(expr: Expr, drawing: Expr?) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                     .pair(.flonum(let w), .flonum(let h))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    try self.drawing(from: drawing).append(.strokeEllipse(CGRect(x: x, y: y, width: w, height: h)))
    return .void
  }
  
  private func fillEllipse(expr: Expr, drawing: Expr?) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                     .pair(.flonum(let w), .flonum(let h))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    try self.drawing(from: drawing).append(.fillEllipse(CGRect(x: x, y: y, width: w, height: h)))
    return .void
  }
  
  private func asObjectLocation(_ location: Expr) throws -> ObjectLocation {
    switch location {
      case .pair(.flonum(let x), .flonum(let y)):
        return .position(CGPoint(x: x, y: y))
      case .pair(.pair(.flonum(let x), .flonum(let y)), .pair(.flonum(let w), .flonum(let h))):
        return .boundingBox(CGRect(x: x, y: y, width: w, height: h))
      default:
        throw RuntimeError.eval(.invalidPoint, location)
    }
  }
  
  private func drawText(text: Expr,
                        location: Expr,
                        font: Expr,
                        color: Expr?,
                        drawing: Expr?) throws -> Expr {
    guard case .object(let obj) = font, let fnt = (obj as? NativeFont)?.value else {
      throw RuntimeError.type(font, expected: [NativeFont.type])
    }
    let color = color ?? .object(Color.black)
    guard case .object(let obj2) = color, let clr = obj2 as? Color else {
      throw RuntimeError.type(color, expected: [Color.type])
    }
    let loc = try self.asObjectLocation(location)
    try self.drawing(from: drawing).append(.text(try text.asString(),
                                                 font: fnt,
                                                 color: clr,
                                                 style: nil,
                                                 at: loc))
    return .void
  }
  
  private func drawStyledText(text: Expr,
                              location: Expr,
                              drawing: Expr?) throws -> Expr {
    guard case .object(let obj) = text, let attribStr = (obj as? StyledText)?.value else {
      throw RuntimeError.type(text, expected: [StyledText.type])
    }
    let loc = try self.asObjectLocation(location)
    try self.drawing(from: drawing).append(.attributedText(attribStr, at: loc))
    return .void
  }
  
  private func drawHtml(text: Expr,
                        location: Expr,
                        drawing: Expr?) throws -> Expr {
    let http = Data(try text.asString().utf8)
    let loc = try self.asObjectLocation(location)
    let attribStr =
      try NSAttributedString(data: http,
                             options: [.documentType: NSAttributedString.DocumentType.html,
                                       .characterEncoding: String.Encoding.utf8.rawValue],
                             documentAttributes: nil)
    try self.drawing(from: drawing).append(.attributedText(attribStr, at: loc))
    return .void
  }
  
  private func drawImage(bitmap: Expr,
                         location: Expr,
                         args: Arguments) throws -> Expr {
    guard let (opacity, composition, drawing) =
                args.optional(.flonum(1.0), .symbol(self.compositionCopy), .false) else {
      throw RuntimeError.argumentCount(
        min: 2, max: 5, args: .pair(bitmap, .pair(location, .makeList(args))))
    }
    let loc: ObjectLocation
    switch location {
      case .pair(.flonum(let x), .flonum(let y)):
        loc = .position(CGPoint(x: x, y: y))
      case .pair(.pair(.flonum(let x), .flonum(let y)), .pair(.flonum(let w), .flonum(let h))):
        loc = .boundingBox(CGRect(x: x, y: y, width: w, height: h))
      default:
        throw RuntimeError.eval(.invalidPoint, location)
    }
    let opcty = try opacity.asDouble(coerce: true)
    guard opcty >= 0.0 && opcty <= 1.0 else {
      throw RuntimeError.range(parameter: 4,
                               of: "draw-bitmap",
                               opacity,
                               min: 0,
                               max: 1,
                               at: SourcePosition.unknown)
    }
    guard case .symbol(let sym) = composition else {
      throw RuntimeError.eval(.invalidCompositionOperation, composition)
    }
    let compositionOperation: CGBlendMode
    switch sym {
      case self.compositionClear:
        compositionOperation = .clear
      case self.compositionCopy:
        compositionOperation = .copy
      case self.compositionMultiply:
        compositionOperation = .multiply
      case self.compositionOverlay:
        compositionOperation = .overlay
      case self.compositionSourceOver:
        compositionOperation = .normal
      case self.compositionSourceIn:
        compositionOperation = .sourceIn
      case self.compositionSourceOut:
        compositionOperation = .sourceOut
      case self.compositionSourceAtop:
        compositionOperation = .sourceAtop
      case self.compositionDestinationOver:
        compositionOperation = .destinationOver
      case self.compositionDestinationIn:
        compositionOperation = .destinationIn
      case self.compositionDestinationOut:
        compositionOperation = .destinationOut
      case self.compositionDestinationAtop:
        compositionOperation = .destinationAtop
      default:
        throw RuntimeError.eval(.invalidCompositionOperation, composition)
    }
    try self.drawing(from: drawing.isFalse ? nil : drawing)
          .append(.image(try self.image(from: bitmap),
                                        loc,
                                        operation: compositionOperation,
                                        opacity: opcty))
    return .void
  }
  
  private func drawDrawing(other: Expr, drawing: Expr?) throws -> Expr {
    try self.drawing(from: drawing).append(.include(try self.drawing(from: other), clippedTo: nil))
    return .void
  }
  
  private func clipDrawing(other: Expr, clip: Expr, drawing: Expr?) throws -> Expr {
    try self.drawing(from: drawing).append(
      .include(try self.drawing(from: other), clippedTo: try self.shape(from: clip)))
    return .void
  }
  
  private func inlineDrawing(other: Expr, drawing: Expr?) throws -> Expr {
    try self.drawing(from: drawing).append(.inline(try self.drawing(from: other)))
    return .void
  }
  
  private func saveDrawing(path: Expr,
                           drawing: Expr,
                           size: Expr,
                           title: Expr?,
                           author: Expr?) throws -> Expr {
    let url = URL(fileURLWithPath:
      self.context.fileHandler.path(try path.asPath(),
                                    relativeTo: self.context.evaluator.currentDirectoryPath))
    guard case .pair(.flonum(let w), .flonum(let h)) = size,
          w > 0.0 && w <= 1000000 && h > 0.0 && h <= 1000000 else {
      throw RuntimeError.eval(.invalidSize, size)
    }
    return .makeBoolean(
      try self.drawing(from: drawing).saveAsPDF(url: url,
                                                width: Int(w),
                                                height: Int(h),
                                                flipped: true,
                                                title: try title?.asString(),
                                                author: try author?.asString()))
  }
  
  private func saveDrawings(path: Expr, pages: Expr, title: Expr?, author: Expr?) throws -> Expr {
    let url = URL(fileURLWithPath:
      self.context.fileHandler.path(try path.asPath(),
                                    relativeTo: self.context.evaluator.currentDirectoryPath))
    let document = DrawingDocument(title: try title?.asString(), author: try author?.asString())
    var pageList = pages
    while case .pair(let page, let next) = pageList {
      guard case .pair(let drawing, .pair(let size, .null)) = page else {
        break
      }
      guard case .pair(.flonum(let w), .flonum(let h)) = size,
            w > 0.0 && w <= 1000000 && h > 0.0 && h <= 1000000 else {
        throw RuntimeError.eval(.invalidSize, size)
      }
      document.append(try self.drawing(from: drawing), flipped: true, width: Int(w), height: Int(h))
      pageList = next
    }
    return .makeBoolean(document.saveAsPDF(url: url))
  }
  
  // Images/bitmaps
  
  private func isImage(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is NativeImage {
      return .true
    }
    return .false
  }
  
  private func loadImage(filename: Expr, forceBM: Expr?) throws -> Expr {
    let path = self.context.fileHandler.path(try filename.asPath(),
                                             relativeTo: self.context.evaluator.currentDirectoryPath)
    return try self.loadImage(path: path, forceBM: forceBM)
  }
  
  private func loadImageAsset(name: Expr, type: Expr, dir: Expr?, forceBM: Expr?) throws -> Expr {
    let dir = ((dir?.isTrue ?? false) ? try dir?.asPath() : nil) ?? "Images"
    if let path = self.context.fileHandler.assetFilePath(
                    forFile: try name.asString(),
                    ofType: try type.asString(),
                    inFolder: dir,
                    relativeTo: self.context.evaluator.currentDirectoryPath) {
      return try self.loadImage(path: path, forceBM: forceBM)
    } else {
      throw RuntimeError.eval(.cannotLoadImageAsset, name, type, .makeString(dir))
    }
  }
  
  private func loadImage(path: String, forceBM: Expr?) throws -> Expr {
    if let image = UIImage(contentsOfFile: path) {
      return .object(NativeImage(image))
    } else {
      let url = URL(fileURLWithPath: path)
      if let document = CGPDFDocument(url as CFURL),
         let page = document.page(at: 1) {
        let pageRect = page.getBoxRect(.mediaBox)
        let renderer = UIGraphicsImageRenderer(size: pageRect.size)
        let proc: (UIGraphicsImageRendererContext) -> Void = { ctx in
          ctx.cgContext.translateBy(x: 0.0, y: pageRect.size.height)
          ctx.cgContext.scaleBy(x: 1.0, y: -1.0)
          ctx.cgContext.drawPDFPage(page)
        }
        if forceBM?.isTrue ?? false {
          if let image = UIImage(data: renderer.pngData(actions: proc), scale: 2.0) {
            return .object(NativeImage(image))
          }
        } else {
          return .object(NativeImage(renderer.image(actions: proc)))
        }
      }
    }
    throw RuntimeError.eval(.cannotLoadImage, .makeString(path))
  }
  
  private func saveImage(filename: Expr, expr: Expr, format: Expr, quality: Expr?) throws -> Expr {
    guard let data = try self.imageData(image: try self.image(from: expr),
                                        format: format,
                                        quality: quality) else {
      return .false
    }
    let url = URL(fileURLWithPath:
      self.context.fileHandler.path(try filename.asPath(),
                                    relativeTo: self.context.evaluator.currentDirectoryPath))
    do {
      try data.write(to: url, options: .atomic)
      return .true
    } catch {
      return .false
    }
  }
  
  private func bytevectorToImage(expr: Expr, args: Arguments) throws -> Expr {
    let data = Data(try BytevectorLibrary.subVector("bytevector->image", expr, args))
    if let image = UIImage(data: data) {
      return .object(NativeImage(image))
    }
    if let dataProvider = CGDataProvider(data: data as CFData),
       let document = CGPDFDocument(dataProvider),
       let page = document.page(at: 1) {
      let pageRect = page.getBoxRect(.mediaBox)
      let renderer = UIGraphicsImageRenderer(size: pageRect.size)
      let proc: (UIGraphicsImageRendererContext) -> Void = { ctx in
        ctx.cgContext.translateBy(x: 0.0, y: pageRect.size.height)
        ctx.cgContext.scaleBy(x: 1.0, y: -1.0)
        ctx.cgContext.drawPDFPage(page)
      }
      return .object(NativeImage(renderer.image(actions: proc)))
    }
    throw RuntimeError.eval(.cannotCreateImage, expr)
  }
  
  private func imageToBytevector(expr: Expr, format: Expr, quality: Expr?) throws -> Expr {
    let image = try self.image(from: expr)
    guard let data = try self.imageData(image: image, format: format, quality: quality) else {
      return .false
    }
    let count = data.count
    var res = [UInt8](repeating: 0, count: count)
    data.copyBytes(to: &res, count: count)
    return .bytes(MutableBox(res))
  }
  
  private func imageData(image: UIImage, format: Expr, quality: Expr?) throws -> Data? {
    guard case .symbol(let sym) = format else {
      throw RuntimeError.eval(.invalidImageFileType, format)
    }
    if sym == self.formatPDF {
      var mediaBox = CGRect(x: 0, y: 0, width: image.size.width, height: image.size.height)
      let pdfData = NSMutableData()
      guard let cgImage =  image.cgImage,
            let pdfConsumer = CGDataConsumer(data: pdfData as CFMutableData),
            let pdfContext = CGContext(consumer: pdfConsumer, mediaBox: &mediaBox, nil) else {
        return nil
      }
      let pageInfo: NSDictionary = [
        kCGPDFContextMediaBox : NSData(bytes: &mediaBox,
                                       length: MemoryLayout.size(ofValue: mediaBox))
      ]
      pdfContext.beginPDFPage(pageInfo)
      pdfContext.draw(cgImage, in: mediaBox)
      pdfContext.endPage()
      pdfContext.closePDF()
      return pdfData as Data
    } else {
      let fileType: BitmapImageFileType
      switch sym {
        case self.formatPNG:
          fileType = .png
        case self.formatJPG:
          fileType = .jpeg
        case self.formatGIF:
          fileType = .gif
        case self.formatBMP:
          fileType = .bmp
        case self.formatTIFF:
          fileType = .tiff
        default:
          throw RuntimeError.eval(.invalidImageFileType, format)
      }
      let qualityFactor: Double?
      if let qf = try quality?.asDouble(coerce: true) {
        if qf < 0.0 {
          qualityFactor = 0.0
        } else if qf > 1.0 {
          qualityFactor = 1.0
        } else {
          qualityFactor = qf
        }
      } else {
        qualityFactor = nil
      }
      return fileType.data(for: image, qualityFactor: qualityFactor)
    }
  }
  
  private func imageSize(image: Expr) throws -> Expr {
    let size = try self.image(from: image).size
    if size.width == 0.0 && size.height == 0.0 {
      return .false
    } else {
      return .pair(.flonum(Double(size.width)), .flonum(Double(size.height)))
    }
  }

  private func setImageSize(image: Expr, size: Expr) throws -> Expr {
    guard case .pair(.flonum(let w), .flonum(let h)) = size, w > 0.0, h > 0.0 else {
      throw RuntimeError.eval(.invalidSize, size)
    }
    guard case .object(let obj) = image,
          let imageBox = obj as? NativeImage else {
      throw RuntimeError.type(image, expected: [NativeImage.type])
    }
    let img = imageBox.value
    let resizedImg = UIGraphicsImageRenderer(size: CGSize(width: w, height: h)).image { _ in
      img.draw(in: CGRect(origin: .zero, size: CGSize(width: w, height: h)))
    }
    imageBox.value = resizedImg
    return .void
  }

  private func isBitmap(expr: Expr) -> Expr {
    if case .object(let obj) = expr, let image = (obj as? NativeImage)?.value {
      return .makeBoolean(image.cgImage != nil)
    }
    return .false
  }

  private func bitmapSize(expr: Expr) throws -> Expr {
    if case .object(let obj) = expr, let image = (obj as? NativeImage)?.value {
      return .pair(.flonum(Double(image.size.width)), .flonum(Double(image.size.height)))
    }
    return .false
  }

  private func bitmapPixels(expr: Expr) throws -> Expr {
    if case .object(let obj) = expr, let image = (obj as? NativeImage)?.value {
      let size = CGSize(width: image.size.width * image.scale,
                        height: image.size.height * image.scale)
      return .pair(.flonum(Double(size.width)), .flonum(Double(size.height)))
    }
    return .false
  }
  
  private func bitmapPpi(expr: Expr) throws -> Expr {
    if case .object(let obj) = expr, let image = (obj as? NativeImage)?.value {
      return .makeNumber(image.scale * 72.0)
    }
    return .false
  }
  
  /*
  private func setBitmapExifData(image: Expr, expr: Expr) throws -> Expr {
    for repr in try self.image(from: image).representations {
      if let bitmapRepr = repr as? NSBitmapImageRep,
         bitmapRepr.size.width > 0.0 {
        var exifDict: [AnyHashable : Any] = [:]
        var lst = expr
        while case .pair(let binding, let rest) = lst {
          if case .pair(.symbol(let sym), let value) = binding,
             let v = self.exifValue(from: value) {
            exifDict[sym.rawIdentifier] = v
          }
          lst = rest
        }
        bitmapRepr.setProperty(.exifData, withValue: NSDictionary(dictionary: exifDict))
        return .void
      }
    }
    throw RuntimeError.eval(.cannotInsertExif, image)
  }
  */
  
  private func exifValue(from expr: Expr) -> Any? {
    switch expr {
      case .fixnum(let num):
        return NSNumber(value: num)
      case .flonum(let num):
        return NSNumber(value: num)
      case .string(let str):
        return NSString(string: str)
      case .null:
        return NSArray()
      case .pair(_, _):
        let res = NSMutableArray()
        var lst = expr
        while case .pair(let head, let tail) = lst {
          if let v = self.exifValue(from: head) {
            res.add(v)
          }
          lst = tail
        }
        return NSArray(array: res)
      default:
        return nil
    }
  }

  private func bitmapExifData(image: Expr) throws -> Expr {
    let image = try self.image(from: image)
    if image.cgImage != nil,
       let exifDict: NSDictionary = image.getExifData() {
      var res: Expr = .null
      for (key, value) in exifDict {
        if let k = key as? String,
           let v = self.expr(from: value) {
          res = .pair(.pair(.symbol(self.context.symbols.intern(k)), v), res)
        }
      }
      return res
    }
    return .false
  }

  private func expr(from value: Any) -> Expr? {
    if let num = value as? Int64 {
      return .fixnum(num)
    } else if let num = value as? Double {
      return .flonum(num)
    } else if let str = value as? String {
      return .makeString(str)
    } else if let a = value as? NSArray {
      var res = Expr.null
      var i = a.count
      while i > 0 {
        i -= 1
        if let v = self.expr(from: a.object(at: i)) {
          res = .pair(v, res)
        }
      }
      return res
    } else {
      return nil
    }
  }
  
  private func makeBitmap(drawing: Expr, size: Expr, dpi: Expr?) throws -> Expr {
    guard case .pair(.flonum(let w), .flonum(let h)) = size, w > 0.0 && h > 0.0 else {
      throw RuntimeError.eval(.invalidSize, size)
    }
    let scale = (try dpi?.asDouble(coerce: true) ?? 72.0)/72.0
    guard scale > 0.0 && scale <= 10.0 else {
      throw RuntimeError.range(parameter: 3,
                               of: "make-bitmap",
                               dpi ?? .fixnum(72),
                               min: 0,
                               max: 720,
                               at: SourcePosition.unknown)
    }
    // Create a bitmap suitable for storing the image
    guard let context = CGContext(data: nil,
                                  width: Int(w * scale),
                                  height: Int(h * scale),
                                  bitsPerComponent: 8,
                                  bytesPerRow: 0,
                                  space: Color.colorSpaceName,
                                  bitmapInfo: CGBitmapInfo(rawValue:
                                                CGImageAlphaInfo.premultipliedFirst.rawValue)
                                              .union(.byteOrder32Little).rawValue) else {
      throw RuntimeError.eval(.cannotCreateBitmap,
                                .pair(drawing, .pair(size, .pair(dpi ?? .fixnum(72), .null))))
    }
    // Configure the graphics context
    context.translateBy(x: 0.0, y: CGFloat(h * scale))
    context.scaleBy(x: CGFloat(scale), y: CGFloat(-scale))
    // Push the context
    UIGraphicsPushContext(context)
    defer {
      UIGraphicsPopContext()
    }
    // Draw into the bitmap
    try self.drawing(from: drawing).draw()
    // Create an image
    guard let cgImage = context.makeImage() else {
      throw RuntimeError.eval(.cannotCreateBitmap,
                                .pair(drawing, .pair(size, .pair(dpi ?? .fixnum(72), .null))))
    }
    let uiImage = UIImage(cgImage: cgImage, scale: CGFloat(scale), orientation: .up)
    return .object(NativeImage(uiImage))
  }
  
  private lazy var coreImageContext = CIContext()
  
  private func bitmapCrop(bitmap: Expr, rect: Expr) throws -> Expr {
    let image = try self.image(from: bitmap)
    let pixels = CGSize(width: image.size.width * image.scale,
                       height: image.size.height * image.scale)
    guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                     .pair(.flonum(let w), .flonum(let h))) = rect else {
      throw RuntimeError.eval(.invalidRect,rect)
    }
    let bounds = CGRect(x: x, y: y, width: w, height: h)
                   .intersection(CGRect(x: 0, y: 0, width: pixels.width, height: pixels.height))
    guard bounds.width > 0, bounds.height > 0,
          let newImage = CIImage(image: image)?.cropped(to: bounds),
          let res = self.coreImageContext.createCGImage(newImage, from: newImage.extent) else {
      return .false
    }
    return .object(NativeImage(UIImage(cgImage: res, scale: image.scale, orientation: .up)))
  }
  
  private func bitmapBlur(bitmap: Expr, radius: Expr) throws -> Expr {
    let orig = try self.image(from: bitmap)
    guard let image = CIImage(image: orig) else {
      return .false
    }
    let radius = CGFloat(try radius.asDouble(coerce: true))
    let blurredImage = image.clampedToExtent()
                            .applyingFilter(
                                "CIGaussianBlur",
                                parameters: [ kCIInputRadiusKey: radius ])
                            .cropped(to: image.extent)
    guard let res = self.coreImageContext.createCGImage(blurredImage,
                                                        from: blurredImage.extent) else {
      return .false
    }
    return .object(NativeImage(UIImage(cgImage: res, scale: orig.scale, orientation: .up)))
  }
  
  private func saveBitmap(filename: Expr, bitmap: Expr, format: Expr, quality: Expr?) throws -> Expr {
    guard case .symbol(let sym) = format else {
      throw RuntimeError.eval(.invalidImageFileType, format)
    }
    let fileType: BitmapImageFileType
    switch sym {
      case self.formatPNG:
        fileType = .png
      case self.formatJPG:
        fileType = .jpeg
      case self.formatGIF:
        fileType = .gif
      case self.formatBMP:
        fileType = .bmp
      case self.formatTIFF:
        fileType = .tiff
      default:
        throw RuntimeError.eval(.invalidImageFileType, format)
    }
    let qualityFactor: Double?
    if let qf = try quality?.asDouble(coerce: true) {
      if qf < 0.0 {
        qualityFactor = 0.0
      } else if qf > 1.0 {
        qualityFactor = 1.0
      } else {
        qualityFactor = qf
      }
    } else {
      qualityFactor = nil
    }
    return self.saveInFile(try self.image(from: bitmap),
                           try filename.asPath(),
                           fileType,
                           qualityFactor) ? .true : .false
  }
  
  private func saveInFile(_ image: UIImage,
                          _ filename: String,
                          _ filetype: BitmapImageFileType,
                          _ qf: Double?) -> Bool {
    let qualityFactor: Double?
    if let qf = qf {
      if qf < 0.0 {
        qualityFactor = 0.0
      } else if qf > 1.0 {
        qualityFactor = 1.0
      } else {
        qualityFactor = qf
      }
    } else {
      qualityFactor = nil
    }
    let url = URL(fileURLWithPath:
      self.context.fileHandler.path(filename,
                                    relativeTo: self.context.evaluator.currentDirectoryPath))
    guard let data = filetype.data(for: image, qualityFactor: qualityFactor) else {
      return false
    }
    do {
      try data.write(to: url, options: .atomic)
      return true
    } catch {
      return false
    }
  }
  
  private func bitmapToBytevector(bitmap: Expr, format: Expr, quality: Expr?) throws -> Expr {
    let image = try self.image(from: bitmap)
    guard case .symbol(let sym) = format else {
      throw RuntimeError.eval(.invalidImageFileType, format)
    }
    let fileType: BitmapImageFileType
    switch sym {
      case self.formatPNG:
        fileType = .png
      case self.formatJPG:
        fileType = .jpeg
      case self.formatGIF:
        fileType = .gif
      case self.formatBMP:
        fileType = .bmp
      case self.formatTIFF:
        fileType = .tiff
      default:
        throw RuntimeError.eval(.invalidImageFileType, format)
    }
    let qualityFactor: Double?
    if let qf = try quality?.asDouble(coerce: true) {
      if qf < 0.0 {
        qualityFactor = 0.0
      } else if qf > 1.0 {
        qualityFactor = 1.0
      } else {
        qualityFactor = qf
      }
    } else {
      qualityFactor = nil
    }
    guard let data = fileType.data(for: image, qualityFactor: qualityFactor) else {
      return .false
    }
    let count = data.count
    var res = [UInt8](repeating: 0, count: count)
    data.copyBytes(to: &res, count: count)
    return .bytes(MutableBox(res))
  }
  
  
  // Shapes
  
  private func isShape(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is Shape {
      return .true
    }
    return .false
  }
  
  private func makeShape(expr: Expr?, freeze: Expr?) throws -> Expr {
    if let prototype = expr {
      if freeze?.isTrue ?? false {
        return .object(Shape(.path(try self.shape(from: prototype).compile())))
      } else {
        return .object(Shape(.shape(try self.shape(from: prototype))))
      }
    }
    return .object(Shape())
  }
  
  private func copyShape(expr: Expr) throws -> Expr {
    return .object(Shape(copy: try self.shape(from: expr)))
  }
  
  private func pointList(_ args: Arguments, skipLast: Bool) -> Expr {
    var first = true
    var res: Expr = .null
    var skip = skipLast
    for arg in args.reversed() {
      if skip {
        skip = false
      } else if first {
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
  
  private func line(start: Expr, end: Expr) throws -> Expr {
    let shape = Shape()
    guard case .pair(.flonum(let sx), .flonum(let sy)) = start else {
      throw RuntimeError.eval(.invalidPoint, start)
    }
    guard case .pair(.flonum(let ex), .flonum(let ey)) = end else {
      throw RuntimeError.eval(.invalidPoint, end)
    }
    shape.append(.move(to: CGPoint(x: sx, y: sy)))
    shape.append(.line(to: CGPoint(x: ex, y: ey)))
    return .object(shape)
  }
  
  private func polygon(args: Arguments) throws -> Expr {
    let shape = Shape()
    var pointList = self.pointList(args, skipLast: false)
    while case .pair(let point, let rest) = pointList {
      guard case .pair(.flonum(let x), .flonum(let y)) = point else {
        throw RuntimeError.eval(.invalidPoint, point)
      }
      if shape.isEmpty {
        shape.append(.move(to: CGPoint(x: x, y: y)))
      } else {
        shape.append(.line(to: CGPoint(x: x, y: y)))
      }
      pointList = rest
    }
    return .object(shape)
  }
  
  private func closedPolygon(args: Arguments) throws -> Expr {
    let shape = Shape(closed: true)
    var pointList = self.pointList(args, skipLast: false)
    while case .pair(let point, let rest) = pointList {
      guard case .pair(.flonum(let x), .flonum(let y)) = point else {
        throw RuntimeError.eval(.invalidPoint, point)
      }
      if shape.isEmpty {
        shape.append(.move(to: CGPoint(x: x, y: y)))
      } else {
        shape.append(.line(to: CGPoint(x: x, y: y)))
      }
      pointList = rest
    }
    return .object(shape)
  }
  
  private func rectangle(point: Expr, size: Expr, xradius: Expr?, yradius: Expr?) throws -> Expr {
    guard case .pair(.flonum(let x), .flonum(let y)) = point else {
      throw RuntimeError.eval(.invalidPoint, point)
    }
    guard case .pair(.flonum(let w), .flonum(let h)) = size else {
      throw RuntimeError.eval(.invalidSize, size)
    }
    if let xrad = try xradius?.asDouble(coerce: true) {
      if let yrad = try yradius?.asDouble(coerce: true) {
        return .object(Shape(.roundedRect(CGRect(x: x, y: y, width: w, height: h),
                                          xradius: xrad,
                                          yradius: yrad)))
      }
      return .object(Shape(.roundedRect(CGRect(x: x, y: y, width: w, height: h),
                                        xradius: xrad,
                                        yradius: xrad)))
    }
    return .object(Shape(.rect(CGRect(x: x, y: y, width: w, height: h))))
  }
  
  private func circle(point: Expr, radius: Expr) throws -> Expr {
    guard case .pair(.flonum(let x), .flonum(let y)) = point else {
      throw RuntimeError.eval(.invalidPoint, point)
    }
    let rad = try radius.asDouble(coerce: true)
    return .object(Shape(.oval(CGRect(x: x - rad, y: y - rad,
                                      width: 2.0 * rad, height: 2.0 * rad))))
  }
  
  private func oval(point: Expr, size: Expr) throws -> Expr {
    guard case .pair(.flonum(let x), .flonum(let y)) = point else {
      throw RuntimeError.eval(.invalidPoint, point)
    }
    guard case .pair(.flonum(let w), .flonum(let h)) = size else {
      throw RuntimeError.eval(.invalidSize, size)
    }
    return .object(Shape(.oval(CGRect(x: x, y: y, width: w, height: h))))
  }
  
  private func arc(point: Expr,
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
      return .object(Shape(.arc(center: CGPoint(x: x, y: y),
                                radius: rad,
                                startAngle: start,
                                endAngle: end,
                                clockwise: clockwise?.isTrue ?? true)))
    } else {
      return .object(Shape(.oval(CGRect(x: x - rad,
                                        y: y - rad,
                                        width: 2.0 * rad,
                                        height: 2.0 * rad))))
    }
  }
  
  private func glyphs(text: Expr, point: Expr, size: Expr, font: Expr) throws -> Expr {
    guard case .pair(.flonum(let x), .flonum(let y)) = point else {
      throw RuntimeError.eval(.invalidPoint, point)
    }
    guard case .pair(.flonum(let w), .flonum(let h)) = size else {
      throw RuntimeError.eval(.invalidSize, size)
    }
    guard case .object(let obj) = font, let fontBox = obj as? NativeFont else {
      throw RuntimeError.type(font, expected: [NativeFont.type])
    }
    return .object(Shape(.glyphs(try text.asString(),
                                 in: CGRect(x: x, y: y, width: w, height: h),
                                 font: fontBox.value,
                                 flipped: true)))
  }
  
  private func moveShape(shape: Expr, dx: Expr, dy: Expr?) throws -> Expr {
    let dx = try dx.asDouble(coerce: true)
    let dy = try dy?.asDouble(coerce: true) ?? dx
    return .object(
             Shape(.transformed(try self.shape(from: shape),
                                Transformation(CGAffineTransform(translationX: dx, y: dy)))))
  }
  
  private func scaleShape(shape: Expr, sx: Expr, sy: Expr?) throws -> Expr {
    let sx = try sx.asDouble(coerce: true)
    let sy = try sy?.asDouble(coerce: true) ?? sx
    return .object(Shape(.transformed(try self.shape(from: shape),
                                      Transformation(CGAffineTransform(scaleX: sx, y: sy)))))
  }
  
  private func transformShape(shape: Expr, transformation: Expr) throws -> Expr {
    return .object(Shape(.transformed(try self.shape(from: shape),
                                      try self.tformation(from: transformation))))
  }
  
  private func flipShape(shape: Expr, box: Expr?, orientation: Expr?) throws -> Expr {
    let bounds: CGRect?
    if let box = box {
      guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                       .pair(.flonum(let w), .flonum(let h))) = box else {
        throw RuntimeError.eval(.invalidRect, box)
      }
      bounds = CGRect(x: x, y: y, width: w, height: h)
    } else {
      bounds = nil
    }
    let horizontal: Bool
    let vertical: Bool
    if let orientation = orientation {
      guard case .symbol(let sym) = orientation else {
        throw RuntimeError.eval(.invalidFlipOrientation, orientation)
      }
      switch sym {
        case self.orientationHorizontal:
          horizontal = true
          vertical = false
        case self.orientationVertical:
          horizontal = false
          vertical = true
        case self.orientationMirror:
          horizontal = true
          vertical = true
        default:
          throw RuntimeError.eval(.invalidFlipOrientation, orientation)
      }
    } else {
      horizontal = false
      vertical = true
    }
    return .object(Shape(.flipped(try self.shape(from: shape),
                                  bounds,
                                  vertical: vertical,
                                  horizontal: horizontal)))
  }
  
  private func interpolate(points: Expr, args: Arguments) throws -> Expr {
    guard let (closed, alpha, algo) = args.optional(.false,
                                                    .flonum(1.0 / 3.0),
                                                    .symbol(self.interpolateHermite)) else {
      throw RuntimeError.argumentCount(min: 1, max: 4, args: .pair(points, .makeList(args)))
    }
    guard case .symbol(let sym) = algo else {
      throw RuntimeError.eval(.unknownInterpolateAlgorithm, algo)
    }
    let method: InterpolationMethod
    switch sym {
      case self.interpolateHermite:
        method = .hermite(closed: closed.isTrue, alpha: try alpha.asDouble(coerce: true))
      case self.interpolateCatmullRom:
        method = .catmullRom(closed: closed.isTrue, alpha: try alpha.asDouble(coerce: true))
      default:
        throw RuntimeError.eval(.unknownInterpolateAlgorithm, algo)
    }
    let CGPoints = try self.CGPoints(from: points)
    return .object(Shape(.interpolated(CGPoints, method: method)))
  }
  
  private func CGPoints(from expr: Expr) throws -> [CGPoint] {
    var CGPoints: [CGPoint] = []
    var pointList = expr
    while case .pair(let point, let rest) = pointList {
      guard case .pair(.flonum(let x), .flonum(let y)) = point else {
        throw RuntimeError.eval(.invalidPoint, point)
      }
      CGPoints.append(CGPoint(x: x, y: y))
      pointList = rest
    }
    return CGPoints
  }
  
  private func moveTo(point: Expr, shape: Expr?) throws -> Expr {
    guard case .pair(.flonum(let x), .flonum(let y)) = point else {
      throw RuntimeError.eval(.invalidPoint, point)
    }
    (try self.shape(from: shape)).append(.move(to: CGPoint(x: x, y: y)))
    return .void
  }
  
  private func lineTo(args: Arguments) throws -> Expr {
    let (shape, skipLast) = try self.shape(from: args)
    var points = self.pointList(args, skipLast: skipLast)
    while case .pair(let point, let rest) = points {
      guard case .pair(.flonum(let x), .flonum(let y)) = point else {
        throw RuntimeError.eval(.invalidPoint, point)
      }
      shape.append(.line(to: CGPoint(x: x, y: y)))
      points = rest
    }
    return .void
  }
  
  private func curveTo(point: Expr, ctrl1: Expr, ctrl2: Expr, shape: Expr?) throws -> Expr {
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
      .curve(to: CGPoint(x: x, y: y),
             controlCurrent: CGPoint(x: c1x, y: c1y),
             controlTarget: CGPoint(x: c2x, y: c2y)))
    return .void
  }
  
  private func relativeMoveTo(point: Expr, shape: Expr?) throws -> Expr {
    guard case .pair(.flonum(let x), .flonum(let y)) = point else {
      throw RuntimeError.eval(.invalidPoint, point)
    }
    (try self.shape(from: shape)).append(.relativeMove(to: CGPoint(x: x, y: y)))
    return .void
  }
  
  private func relativeLineTo(args: Arguments) throws -> Expr {
    let (shape, skipLast) = try self.shape(from: args)
    var points = self.pointList(args, skipLast: skipLast)
    while case .pair(let point, let rest) = points {
      guard case .pair(.flonum(let x), .flonum(let y)) = point else {
        throw RuntimeError.eval(.invalidPoint, point)
      }
      shape.append(.relativeLine(to: CGPoint(x: x, y: y)))
      points = rest
    }
    return .void
  }
  
  private func relativeCurveTo(point: Expr, ctrl1: Expr, ctrl2: Expr, shape: Expr?) throws -> Expr {
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
      .relativeCurve(to: CGPoint(x: x, y: y),
                     controlCurrent: CGPoint(x: c1x, y: c1y),
                     controlTarget: CGPoint(x: c2x, y: c2y)))
    return .void
  }
  
  private func addShape(other: Expr, shape: Expr?) throws -> Expr {
    (try self.shape(from: shape)).append(.include((try self.shape(from: other))))
    return .void
  }
  
  private func shapeBounds(shape: Expr) throws -> Expr {
    let box = (try self.shape(from: shape)).bounds
    return .pair(.pair(.flonum(Double(box.origin.x)), .flonum(Double(box.origin.y))),
                 .pair(.flonum(Double(box.width)), .flonum(Double(box.height))))
  }
  
  private func shapeContains(shape: Expr, points: Expr, some: Expr?) throws -> Expr {
    let shape = try self.shape(from: shape)
    let all = some?.isFalse ?? true
    switch points {
      case .null:
        return .true
      case .pair(.flonum(let x), .flonum(let y)):
        return .makeBoolean(shape.contains(CGPoint(x: x, y: y)))
      case .pair(_, _):
        var list = points
        if all {
          while case .pair(let pnt, let rest) = list {
            guard case .pair(.flonum(let x), .flonum(_)) = pnt else {
              throw RuntimeError.eval(.invalidPoint, pnt)
            }
            guard shape.contains(CGPoint(x: x, y: 0)) else {
              return .false
            }
            list = rest
          }
          guard case .null = list else {
            throw RuntimeError.type(points, expected: [.properListType])
          }
          return .true
        } else {
          while case .pair(let pnt, let rest) = list {
            guard case .pair(.flonum(let x), .flonum(_)) = pnt else {
              throw RuntimeError.eval(.invalidPoint, pnt)
            }
            if shape.contains(CGPoint(x: x, y: 0)) {
              return .true
            }
            list = rest
          }
          guard case .null = list else {
            throw RuntimeError.type(points, expected: [.properListType])
          }
          return .false
        }
      default:
        throw RuntimeError.eval(.invalidPoint, points)
    }
  }
  
  
  // Transformations
  
  private func isTransformation(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is Transformation {
      return .true
    }
    return .false
  }
  
  private func transformation(args: Arguments) throws -> Expr {
    var transform = CGAffineTransform.identity
    for arg in args {
      transform = transform.concatenating(try self.affineTransform(arg))
    }
    return .object(Transformation(transform))
  }
  
  private func invert(expr: Expr) throws -> Expr {
    return .object(Transformation(try self.affineTransform(expr).inverted()))
  }
  
  private func translate(dx: Expr, dy: Expr, expr: Expr?) throws -> Expr {
    return .object(Transformation(try self.affineTransform(expr)
                                    .translatedBy(x: CGFloat(try dx.asDouble(coerce: true)),
                                                  y: CGFloat(try dy.asDouble(coerce: true)))))
  }
  
  private func scale(dx: Expr, dy: Expr, expr: Expr?) throws -> Expr {
    return .object(Transformation(try self.affineTransform(expr)
                                    .scaledBy(x: CGFloat(try dx.asDouble(coerce: true)),
                                              y: CGFloat(try dy.asDouble(coerce: true)))))
  }
  
  private func rotate(angle: Expr, expr: Expr?) throws -> Expr {
    return .object(Transformation(try self.affineTransform(expr)
                                        .rotated(by: CGFloat(try angle.asDouble(coerce: true)))))
  }
  
  private func tformation(from expr: Expr) throws -> Transformation {
    guard case .object(let obj) = expr, let transform = obj as? Transformation else {
      throw RuntimeError.type(expr, expected: [Transformation.type])
    }
    return transform
  }
  
  private func affineTransform(_ expr: Expr?) throws -> CGAffineTransform {
    guard let tf = expr else {
      return CGAffineTransform.identity
    }
    guard case .object(let obj) = tf, let transform = obj as? Transformation else {
      throw RuntimeError.type(tf, expected: [Transformation.type])
    }
    return transform.affineTransform
  }
  
  // Colors
  
  private func isColor(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is Color {
      return .true
    }
    return .false
  }
  
  private func color(red: Expr, args: Arguments) throws -> Expr {
    if args.count < 2 {
      if args.isEmpty {
        switch red {
          case .string(let mstr):
            let str: String = mstr.hasPrefix("#") ? mstr.substring(from: 1) : (mstr as String)
            if let num = Int64(str as String, radix: 16) {
              if str.count <= 3 {
                return .object(Color(red: Double((num & 0xf00) >> 8) / 15.0,
                                     green: Double((num & 0xf0) >> 4) / 15.0,
                                     blue: Double(num & 0xf) / 15.0,
                                     alpha: 1.0))
              } else {
                return .object(Color(red: Double((num & 0xff0000) >> 16) / 255.0,
                                   green: Double((num & 0xff00) >> 8) / 255.0,
                                   blue: Double(num & 0xff) / 255.0,
                                   alpha: 1.0))
              }
            } else {
              throw RuntimeError.custom("eval error", "not a valid hex number designating a " +
                                        "color: \(red)", [])
            }
          case .fixnum(let num):
            return .object(Color(red: Double((num & 0xff0000) >> 16) / 255.0,
                                 green: Double((num & 0xff00) >> 8) / 255.0,
                                 blue: Double((num & 0xff)) / 255.0,
                                 alpha: 1.0))
          default:
            break
        }
      }
      _ = try red.asSymbol().identifier
      var colorListName = "Apple"
      if args.count == 1 {
        colorListName = try args.first!.asString()
      }
      throw RuntimeError.custom("eval error", "color lists unsupported: \(colorListName)", [])
    } else if args.count == 2 {
      return .object(Color(red: try red.asDouble(coerce: true),
                           green: try args.first!.asDouble(coerce: true),
                           blue: try args[args.startIndex + 1].asDouble(coerce: true),
                           alpha: 1.0))
    } else if args.count == 3 {
      return .object(Color(red: try red.asDouble(coerce: true),
                           green: try args.first!.asDouble(coerce: true),
                           blue: try args[args.startIndex + 1].asDouble(coerce: true),
                           alpha: try args[args.startIndex + 2].asDouble(coerce: true)))
    } else {
      throw RuntimeError.argumentCount(of: "color",
                                       min: 3,
                                       max: 4,
                                       args: .pair(red, .makeList(args)))
    }
  }
  
  private func colorToHex(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let color = obj as? Color else {
      throw RuntimeError.type(expr, expected: [Color.type])
    }
    return .makeString(color.nsColor.hexString)
  }
  
  private func colorRed(_ expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let color = obj as? Color else {
      throw RuntimeError.type(expr, expected: [Color.type])
    }
    return .flonum(color.red)
  }
  
  private func colorGreen(_ expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let color = obj as? Color else {
      throw RuntimeError.type(expr, expected: [Color.type])
    }
    return .flonum(color.green)
  }
  
  private func colorBlue(_ expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let color = obj as? Color else {
      throw RuntimeError.type(expr, expected: [Color.type])
    }
    return .flonum(color.blue)
  }
  
  private func colorAlpha(_ expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let color = obj as? Color else {
      throw RuntimeError.type(expr, expected: [Color.type])
    }
    return .flonum(color.alpha)
  }
  
  private func color(from expr: Expr) throws -> Color {
    guard case .object(let obj) = expr, let color = obj as? Color else {
      throw RuntimeError.type(expr, expected: [Color.type])
    }
    return color
  }
  
  // Fonts/points/sizes/rects
  
  private func isPoint(expr: Expr) throws -> Expr {
    guard case .pair(.flonum(_), .flonum(_)) = expr else {
      return .false
    }
    return .true
  }
  
  private func point(xc: Expr, yc: Expr) throws -> Expr {
    let x = try xc.asDouble(coerce: true)
    let y = try yc.asDouble(coerce: true)
    return .pair(.flonum(x), .flonum(y))
  }
  
  private func pointX(expr: Expr) throws -> Expr {
    guard case .pair(.flonum(let x), .flonum(_)) = expr else {
      throw RuntimeError.eval(.invalidPoint, expr)
    }
    return .flonum(x)
  }
  
  private func pointY(expr: Expr) throws -> Expr {
    guard case .pair(.flonum(_), .flonum(let y)) = expr else {
      throw RuntimeError.eval(.invalidPoint, expr)
    }
    return .flonum(y)
  }
  
  private func movePoint(expr: Expr, dx: Expr, dy: Expr) throws -> Expr {
    guard case .pair(.flonum(let x), .flonum(let y)) = expr else {
      throw RuntimeError.eval(.invalidPoint, expr)
    }
    return try .pair(.flonum(x + dx.asDouble(coerce: true)),
                     .flonum(y + dy.asDouble(coerce: true)))
  }
  
  private func scalePoint(expr: Expr, sx: Expr, sy: Expr?) throws -> Expr {
    guard case .pair(.flonum(let x), .flonum(let y)) = expr else {
      throw RuntimeError.eval(.invalidPoint, expr)
    }
    let sx = try sx.asDouble(coerce: true)
    let sy = try sy?.asDouble(coerce: true) ?? sx
    let pnt = CGPoint(x: x, y: y).applying(CGAffineTransform(scaleX: sx, y: sy))
    return .pair(.flonum(pnt.x), .flonum(pnt.y))
  }
  
  private func transformPoint(expr: Expr, transform: Expr) throws -> Expr {
    guard case .pair(.flonum(let x), .flonum(let y)) = expr else {
      throw RuntimeError.eval(.invalidPoint, expr)
    }
    let res = CGPoint(x: x, y: y).applying(try self.affineTransform(transform))
    return .pair(.flonum(res.x), .flonum(res.y))
  }
  
  private func isSize(expr: Expr) throws -> Expr {
    guard case .pair(.flonum(_), .flonum(_)) = expr else {
      return .false
    }
    return .true
  }
  
  private func size(wc: Expr, hc: Expr) throws -> Expr {
    let w = try wc.asDouble(coerce: true)
    let h = try hc.asDouble(coerce: true)
    return .pair(.flonum(w), .flonum(h))
  }
  
  private func sizeWidth(expr: Expr) throws -> Expr {
    guard case .pair(.flonum(let w), .flonum(_)) = expr else {
      throw RuntimeError.eval(.invalidSize, expr)
    }
    return .flonum(w)
  }
  
  private func sizeHeight(expr: Expr) throws -> Expr {
    guard case .pair(.flonum(_), .flonum(let h)) = expr else {
      throw RuntimeError.eval(.invalidSize, expr)
    }
    return .flonum(h)
  }
  
  private func sizeRatio(expr: Expr) throws -> Expr {
    guard case .pair(.flonum(let w), .flonum(let h)) = expr else {
      throw RuntimeError.eval(.invalidSize, expr)
    }
    return .flonum(w / h)
  }

  private func increaseSize(expr: Expr, dw: Expr, dh: Expr) throws -> Expr {
    guard case .pair(.flonum(let w), .flonum(let h)) = expr else {
      throw RuntimeError.eval(.invalidSize, expr)
    }
    return try .pair(.flonum(w + dw.asDouble(coerce: true)),
                     .flonum(h + dh.asDouble(coerce: true)))
  }

  private func scaleSize(expr: Expr, f: Expr) throws -> Expr {
    guard case .pair(.flonum(let w), .flonum(let h)) = expr else {
      throw RuntimeError.eval(.invalidSize, expr)
    }
    let factor = try f.asDouble(coerce: true)
    return .pair(.flonum(w * factor), .flonum(h * factor))
  }
  
  private func isRect(expr: Expr) throws -> Expr {
    guard case .pair(.pair(.flonum(_), .flonum(_)), .pair(.flonum(_), .flonum(_))) = expr else {
      return .false
    }
    return .true
  }
  
  private func rect(fst: Expr, snd: Expr, thrd: Expr?, fth: Expr?) throws -> Expr {
    if let width = thrd {
      let x = try fst.asDouble(coerce: true)
      let y = try snd.asDouble(coerce: true)
      let w = try width.asDouble(coerce: true)
      let h = fth == nil ? w : try fth!.asDouble(coerce: true)
      return .pair(.pair(.flonum(x), .flonum(y)), .pair(.flonum(w), .flonum(h)))
    } else {
      guard case .pair(let xc, let yc) = fst else {
        throw RuntimeError.eval(.invalidPoint, fst)
      }
      guard case .pair(let wc, let hc) = snd else {
        throw RuntimeError.eval(.invalidSize, snd)
      }
      let x = try xc.asDouble(coerce: true)
      let y = try yc.asDouble(coerce: true)
      let w = try wc.asDouble(coerce: true)
      let h = try hc.asDouble(coerce: true)
      return .pair(.pair(.flonum(x), .flonum(y)), .pair(.flonum(w), .flonum(h)))
    }
  }
  
  private func rectPoint(expr: Expr) throws -> Expr {
    guard case .pair(let point, .pair(.flonum(_), .flonum(_))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    guard case .pair(.flonum(_), .flonum(_)) = point else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    return point
  }
  
  private func rectMaxPoint(expr: Expr) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                     .pair(.flonum(let w), .flonum(let h))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    return .pair(.flonum(x + w), .flonum(y + h))
  }
  
  private func rectX(expr: Expr) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(_)), .pair(.flonum(_), .flonum(_))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    return .flonum(x)
  }
  
  private func rectY(expr: Expr) throws -> Expr {
    guard case .pair(.pair(.flonum(_), .flonum(let y)), .pair(.flonum(_), .flonum(_))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    return .flonum(y)
  }
  
  private func rectMaxX(expr: Expr) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(_)), .pair(.flonum(let w), .flonum(_))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    return .flonum(x + w)
  }
  
  private func rectMaxY(expr: Expr) throws -> Expr {
    guard case .pair(.pair(.flonum(_), .flonum(let y)), .pair(.flonum(_), .flonum(let h))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    return .flonum(y + h)
  }
  
  private func rectMidPoint(expr: Expr) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                     .pair(.flonum(let w), .flonum(let h))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    return .pair(.flonum(x + w / 2.0), .flonum(y + h / 2.0))
  }
  
  private func rectMidX(expr: Expr) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(_)), .pair(.flonum(let w), .flonum(_))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    return .flonum(x + w / 2.0)
  }
  
  private func rectMidY(expr: Expr) throws -> Expr {
    guard case .pair(.pair(.flonum(_), .flonum(let y)), .pair(.flonum(_), .flonum(let h))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    return .flonum(y + h / 2.0)
  }
  
  private func rectSize(expr: Expr) throws -> Expr {
    guard case .pair(.pair(.flonum(_), .flonum(_)), let size) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    guard case .pair(.flonum(_), .flonum(_)) = size else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    return size
  }
  
  private func rectWidth(expr: Expr) throws -> Expr {
    guard case .pair(.pair(.flonum(_), .flonum(_)), .pair(.flonum(let w), .flonum(_))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    return .flonum(w)
  }
  
  private func rectHeight(expr: Expr) throws -> Expr {
    guard case .pair(.pair(.flonum(_), .flonum(_)), .pair(.flonum(_), .flonum(let h))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    return .flonum(h)
  }
  
  private func moveRect(expr: Expr, dx: Expr, dy: Expr?) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(let y)), let dim) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    let dx = try dx.asDouble(coerce: true)
    let dy = try dy?.asDouble(coerce: true) ?? dx
    return .pair(.pair(.flonum(x + dx), .flonum(y + dy)), dim)
  }
  
  private func scaleRect(expr: Expr, sx: Expr, sy: Expr?, preserveMid: Expr?) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                     .pair(.flonum(let w), .flonum(let h))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    let rect = CGRect(origin: CGPoint(x: x, y: y), size: CGSize(width: w, height: h))
    let sx = try sx.asDouble(coerce: true)
    let sy = try sy?.asDouble(coerce: true) ?? sx
    var res = rect.applying(CGAffineTransform(scaleX: sx, y: sy))
    if preserveMid?.isTrue ?? false {
      res = CGRect(origin: CGPoint(x: x + (w - res.width) / 2.0, y: y + (h - res.height) / 2.0),
                   size: res.size)
    }
    return .pair(.pair(.flonum(res.origin.x), .flonum(res.origin.y)),
                 .pair(.flonum(res.width), .flonum(res.height)))
  }
  
  private func insetRect(expr: Expr, dn: Expr, args: Arguments) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                     .pair(.flonum(let w), .flonum(let h))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    let d = try dn.asDouble(coerce: true)
    switch args.count {
      case 0:
        return .pair(.pair(.flonum(x + d), .flonum(y + d)),
                     .pair(.flonum(w - 2 * d), .flonum(h - 2 * d)))
      case 1:
        let dy = try args.first!.asDouble(coerce: true)
        return .pair(.pair(.flonum(x + d), .flonum(y + dy)),
                     .pair(.flonum(w - 2 * d), .flonum(h - 2 * dy)))
      case 3:
        var iter = args.makeIterator()
        let top = try iter.next()!.asDouble(coerce: true)
        let right = try iter.next()!.asDouble(coerce: true)
        let bottom = try iter.next()!.asDouble(coerce: true)
        return .pair(.pair(.flonum(x + d), .flonum(y + top)),
                     .pair(.flonum(w - d - right), .flonum(h - top - bottom)))
      default:
        throw RuntimeError.argumentCount(of: "inset-rect",
                                         min: 2,
                                         max: 5,
                                         expr: .pair(expr, .pair(dn, .makeList(args))))
    }
  }
  
  private func transformRect(expr: Expr, transform: Expr) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                     .pair(.flonum(let w), .flonum(let h))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    let res = CGRect(origin: CGPoint(x: x, y: y), size: CGSize(width: w, height: h)).applying(
                try self.affineTransform(transform))
    return .pair(.pair(.flonum(res.origin.x), .flonum(res.origin.y)),
                 .pair(.flonum(res.width), .flonum(res.height)))
  }
  
  private func intersectRect(expr: Expr, other: Expr) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                     .pair(.flonum(let w), .flonum(let h))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    guard case .pair(.pair(.flonum(let ox), .flonum(let oy)),
                     .pair(.flonum(let ow), .flonum(let oh))) = other else {
      throw RuntimeError.eval(.invalidRect, other)
    }
    let res = CGRect(x: x, y: y, width: w, height: h).intersection(
                                                        CGRect(x: ox, y: oy, width: ow, height: oh))
    if res.isInfinite || res.origin.x.isInfinite || res.origin.y.isInfinite {
      return .false
    } else {
      return .pair(.pair(.flonum(res.origin.x), .flonum(res.origin.y)),
                   .pair(.flonum(res.width), .flonum(res.height)))
    }
  }
  
  private func unionRect(expr: Expr, other: Expr) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                     .pair(.flonum(let w), .flonum(let h))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    guard case .pair(.pair(.flonum(let ox), .flonum(let oy)),
                     .pair(.flonum(let ow), .flonum(let oh))) = other else {
      throw RuntimeError.eval(.invalidRect, other)
    }
    let res = CGRect(x: x, y: y, width: w, height: h).union(
                                                        CGRect(x: ox, y: oy, width: ow, height: oh))
    return .pair(.pair(.flonum(res.origin.x), .flonum(res.origin.y)),
                 .pair(.flonum(res.width), .flonum(res.height)))
  }
  
  private func rectContains(expr: Expr, other: Expr) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                     .pair(.flonum(let w), .flonum(let h))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    let rect = CGRect(origin: CGPoint(x: x, y: y), size: CGSize(width: w, height: h))
    switch other {
      case .false, .true:
        return other
      case .pair(.pair(.flonum(let x), .flonum(let y)), .pair(.flonum(let w), .flonum(let h))):
        return .makeBoolean(rect.contains(CGRect(origin: CGPoint(x: x, y: y),
                                                 size: CGSize(width: w, height: h))))
      case .pair(.flonum(let x), .flonum(let y)):
        return .makeBoolean(rect.contains(CGPoint(x: x, y: y)))
      default:
        throw RuntimeError.eval(.invalidRect, other)
    }
  }
  
  private func isFont(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is NativeFont {
      return .true
    }
    return .false
  }
  
  private func font(font: Expr, s: Expr, args: Arguments) throws -> Expr {
    let name = try font.asString()
    let size = try s.asDouble(coerce: true)
    // A variant of `font` which loads the font based on a font name and a size
    if args.isEmpty {
      guard let nsfont = UIFont(name: name, size: CGFloat(size)) else {
        return .false
      }
      return .object(NativeFont(nsfont))
    // A variant of `font` which loads the font based on a font family, a size, weight, and traits
    } else {
      var fontDescr = UIFontDescriptor().withFamily(name).withSize(CGFloat(size))
      var weight: CGFloat? = nil
      for arg in args {
        if weight == nil {
          if case .flonum(let x) = arg {
            weight = CGFloat(x)
            fontDescr = fontDescr.addingAttributes([.traits: [
              UIFontDescriptor.TraitKey.weight: UIFont.Weight(rawValue: weight!)]
            ])
            continue
          } else {
            weight = 0.0
          }
        }
        fontDescr = FontTraitModifier(rawValue: try arg.asInt()).apply(to: fontDescr)
      }
      return .object(NativeFont(UIFont(descriptor: fontDescr, size: CGFloat(size))))
    }
  }
  
  private func fontName(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let fontBox = obj as? NativeFont else {
      throw RuntimeError.type(expr, expected: [NativeFont.type])
    }
    return .makeString(fontBox.value.fontName)
  }
  
  private func fontFamilyName(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let fontBox = obj as? NativeFont else {
      throw RuntimeError.type(expr, expected: [NativeFont.type])
    }
    return .makeString(fontBox.value.familyName)
  }
  
  private func fontWeight(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let fontBox = obj as? NativeFont else {
      throw RuntimeError.type(expr, expected: [NativeFont.type])
    }
    return .flonum(Double(fontBox.value.rawWeight))
  }
  
  private func fontTraits(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let fontBox = obj as? NativeFont else {
      throw RuntimeError.type(expr, expected: [NativeFont.type])
    }
    let traits = FontTraitModifier(from: fontBox.value.fontDescriptor)
    guard traits.rawValue < Int64.max else {
      return .false
    }
    return .fixnum(Int64(traits.rawValue))
  }
  
  private func fontHasTraits(expr: Expr, args: Arguments) throws -> Expr {
    guard case .object(let obj) = expr, let fontBox = obj as? NativeFont else {
      throw RuntimeError.type(expr, expected: [NativeFont.type])
    }
    var traits: Int = 0
    for arg in args {
      traits |= try arg.asInt()
    }
    let fontTraits = FontTraitModifier(from: fontBox.value.fontDescriptor)
    return .makeBoolean(fontTraits.contains(FontTraitModifier(rawValue: traits)))
  }
  
  private func fontSize(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let fontBox = obj as? NativeFont else {
      throw RuntimeError.type(expr, expected: [NativeFont.type])
    }
    return .flonum(Double(fontBox.value.pointSize))
  }
  
  private func availableFonts(args: Arguments) throws -> Expr {
    var fonts: Set<String> = []
    for familyName in UIFont.familyNames {
      fonts.formUnion(UIFont.fontNames(forFamilyName: familyName))
    }
    if !args.isEmpty {
      var traits: Int = 0
      for arg in args {
        traits |= try arg.asInt()
      }
      let modifiers = FontTraitModifier(rawValue: traits)
      fonts = fonts.filter { name in
        if let font = UIFont(name: name, size: 12.0) {
          return FontTraitModifier(from: font.fontDescriptor).contains(modifiers)
        }
        return false
      }
    }
    let fontList = fonts.sorted(by: >)
    var res: Expr = .null
    for font in fontList {
      res = .pair(.makeString(font), res)
    }
    return res
  }
  
  private func availableFontFamilies() throws -> Expr {
    let fontFamilies = UIFont.familyNames.reversed()
    var res: Expr = .null
    for fontFamily in fontFamilies {
      res = .pair(.makeString(fontFamily), res)
    }
    return res
  }
  
  private func textSize(text: Expr,
                        font: Expr?,
                        dimensions: Expr?) throws -> Expr {
    let str = try text.asString()
    let fnt: UIFont
    if let font = font, !font.isFalse {
      guard case .object(let obj) = font, let f = (obj as? NativeFont)?.value else {
        throw RuntimeError.type(font, expected: [NativeFont.type])
      }
      fnt = f
    } else {
      fnt = UIFont.systemFont(ofSize: UIFont.systemFontSize)
    }
    let size: CGSize
    switch dimensions {
      case .none:
        size = CGSize(width: CGFloat.infinity, height: CGFloat.infinity)
      case .some(.pair(.flonum(let w), .flonum(let h))):
        size = CGSize(width: w, height: h)
      case .some(let w):
        size = CGSize(width: CGFloat(try w.asDouble(coerce: true)), height: CGFloat.infinity)
    }
    let pstyle: NSParagraphStyle = .default
    let attributes = [.font: fnt, .paragraphStyle: pstyle] as [NSAttributedString.Key: Any]
    let rect = str.boundingRect(with: size,
                                options: [.usesLineFragmentOrigin, .usesFontLeading],
                                attributes: attributes,
                                context: nil)
    return .pair(.flonum(Double(rect.width)), .flonum(Double(rect.height)))
  }
  
  private func styledTextSize(text: Expr, dimensions: Expr?) throws -> Expr {
    guard case .object(let obj) = text, let str = (obj as? StyledText)?.value else {
      throw RuntimeError.type(text, expected: [StyledText.type])
    }
    let size: CGSize
    switch dimensions {
      case .none:
        size = CGSize(width: CGFloat.infinity, height: CGFloat.infinity)
      case .some(.pair(.flonum(let w), .flonum(let h))):
        size = CGSize(width: w, height: h)
      case .some(let w):
        size = CGSize(width: CGFloat(try w.asDouble(coerce: true)), height: CGFloat.infinity)
    }
    let rect = str.boundingRect(with: size,
                                options: [.usesLineFragmentOrigin, .usesFontLeading],
                                context: nil)
    return .pair(.flonum(Double(rect.width)), .flonum(Double(rect.height)))
  }
  
  private func htmlSize(text: Expr, dimensions: Expr?) throws -> Expr {
    let http = Data(try text.asString().utf8)
    let str =
      try NSAttributedString(data: http,
                             options: [.documentType: NSAttributedString.DocumentType.html,
                                       .characterEncoding: String.Encoding.utf8.rawValue],
                             documentAttributes: nil)
    let size: CGSize
    switch dimensions {
      case .none:
        size = CGSize(width: CGFloat.infinity, height: CGFloat.infinity)
      case .some(.pair(.flonum(let w), .flonum(let h))):
        size = CGSize(width: w, height: h)
      case .some(let w):
        size = CGSize(width: CGFloat(try w.asDouble(coerce: true)), height: CGFloat.infinity)
    }
    let rect = str.boundingRect(with: size,
                                options: [.usesLineFragmentOrigin, .usesFontLeading],
                                context: nil)
    return .pair(.flonum(Double(rect.width)), .flonum(Double(rect.height)))
  }
  
  private func sizeSpecifier(_ expr: Expr) throws -> CGSize {
    switch expr {
      case .pair(.flonum(let w), .flonum(let h)):
        return CGSize(width: w, height: h)
      case .pair(.pair(.flonum(_), .flonum(_)), .pair(.flonum(let w), .flonum(let h))):
        return CGSize(width: w, height: h)
      case .object(let obj):
        if let imageBox = obj as? NativeImage {
          return imageBox.imageSize
        } else {
          fallthrough
        }
      default:
        throw RuntimeError.eval(.invalidSize, expr)
    }
  }
  
  private func transpose(_ expr: Expr, sx: Double, sy: Double, height: Double) throws -> Expr {
    switch expr {
      case .true, .false, .null:
        return expr
      case .pair(.flonum(let x), .flonum(let y)):
        let pnt = CGPoint(x: x, y: y)
        var res = pnt.applying(CGAffineTransform(scaleX: sx, y: sy))
        if sy < 0.0 {
          res = res.applying(CGAffineTransform(translationX: 0, y: height))
        }
        return .pair(.flonum(res.x), .flonum(res.y))
      case .pair(.pair(.flonum(let x), .flonum(let y)), .pair(.flonum(let w), .flonum(let h))):
        let rect = CGRect(x: x, y: y, width: w, height: h)
        var res = rect.applying(CGAffineTransform(scaleX: sx, y: sy))
        if sy < 0.0 {
          res = res.applying(CGAffineTransform(translationX: 0, y: height))
        }
        return .pair(.pair(.flonum(res.origin.x), .flonum(res.origin.y)),
                     .pair(.flonum(res.width), .flonum(res.height)))
      default:
        var shape = Shape(.transformed(try self.shape(from: expr),
                                       Transformation(CGAffineTransform(scaleX: sx, y: sy))))
        if sy < 0.0 {
          shape = Shape(.transformed(shape,
                                     Transformation(CGAffineTransform(translationX: 0, y: height))))
        }
        return .object(shape)
    }
  }
  
  private func transpose(expr: Expr, current: Expr, new: Expr, flp: Expr?) throws -> Expr {
    let base: CGSize = try self.sizeSpecifier(current)
    let target: CGSize
    let flip: Bool
    switch new {
      case .false:
        target = base
        flip = false
      case .true:
        target = base
        flip = true
      default:
        target = try self.sizeSpecifier(new)
        flip = flp?.isTrue ?? false
    }
    let sx = target.width / base.width
    let sy = (flip ? -1.0 : 1.0) * (target.height / base.height)
    switch expr {
      case .true, .false, .null:
        return expr
      case .pair(.flonum(_), .flonum(_)):
        return try self.transpose(expr, sx: sx, sy: sy, height: target.height)
      case .pair(.pair(.flonum(_), .flonum(_)), .pair(.flonum(_), .flonum(_))):
        return try self.transpose(expr, sx: sx, sy: sy, height: target.height)
      case .pair(_, _):
        var list = expr
        var res = Expr.null
        while case .pair(let elem, let next) = list {
          res = .pair(try self.transpose(elem, sx: sx, sy: sy, height: target.height), res)
          list = next
        }
        return res
      case .vector(let coll):
        var res = Exprs()
        for elem in coll.exprs {
          res.append(try self.transpose(elem, sx: sx, sy: sy, height: target.height))
        }
        return .vector(Collection(kind: coll.kind, exprs: res))
      default:
        return try self.transpose(expr, sx: sx, sy: sy, height: target.height)
    }
  }
}

public final class NativeFont: AnyNativeObject<UIFont> {

  /// Type representing fonts
  public static let type = Type.objectType(Symbol(uninterned: "font"))

  public override var type: Type {
    return NativeFont.type
  }
  
  public override var string: String {
    return "#<font \(self.value.fontName) \(self.value.pointSize)>"
  }
  
  public override func unpack(in context: Context) -> Exprs {
    return [.makeString(self.value.fontName),
            .makeString(self.value.familyName),
            .makeNumber(self.value.pointSize)]
  }
}

public final class NativeImage: AnyMutableNativeObject<UIImage> {

  /// Type representing images
  public static let type = Type.objectType(Symbol(uninterned: "image"))

  public override var type: Type {
    return NativeImage.type
  }
  
  public override var string: String {
    if let width = Int64(exactly: floor(self.value.size.width)),
       let height = Int64(exactly: floor(self.value.size.height)) {
      return "#<image \(self.identityString): \(width)×\(height)>"
    } else {
      return "#<image \(self.identityString)>"
    }
  }
  
  public var width: CGFloat {
    return self.value.size.width
  }
  
  public var height: CGFloat {
    return self.value.size.height
  }
  
  public var imageSize: CGSize {
    return self.value.size
  }
  
  public var pixelSize: CGSize? {
    let image = self.value
    return CGSize(width: image.size.width * image.scale, height: image.size.height * image.scale)
  }
  
  public func cgImage() -> CGImage? {
    return self.value.cgImage
  }
  
  public override func unpack(in context: Context) -> Exprs {
    return [.makeString(self.identityString),
            .makeNumber(self.value.size.width),
            .makeNumber(self.value.size.height)]
  }
}

struct FontTraitModifier: OptionSet {
  let rawValue: Int
  
  static let normal = FontTraitModifier(rawValue: 1 << 0)
  static let italic = FontTraitModifier(rawValue: 1 << 1)
  static let boldface = FontTraitModifier(rawValue: 1 << 2)
  static let unitalic = FontTraitModifier(rawValue: 1 << 3)
  static let unboldface = FontTraitModifier(rawValue: 1 << 4)
  static let narrow = FontTraitModifier(rawValue: 1 << 5)
  static let condensed = FontTraitModifier(rawValue: 1 << 6)
  static let compressed = FontTraitModifier(rawValue: 1 << 7)
  static let expanded = FontTraitModifier(rawValue: 1 << 8)
  static let poster = FontTraitModifier(rawValue: 1 << 9)
  static let monospace = FontTraitModifier(rawValue: 1 << 10)
  static let smallCaps = FontTraitModifier(rawValue: 1 << 11)
  
  init(rawValue: Int) {
    self.rawValue = rawValue
  }
  
  init(from descr: UIFontDescriptor) {
    let symTraits = descr.symbolicTraits
    var modifier: FontTraitModifier = []
    if symTraits.contains(.traitItalic) {
      modifier.insert(.italic)
    }
    if symTraits.contains(.traitBold) {
      modifier.insert(.boldface)
    }
    if symTraits.contains(.traitCondensed) {
      modifier.insert(.condensed)
      modifier.insert(.compressed)
      modifier.insert(.narrow)
    }
    if symTraits.contains(.traitExpanded) {
      modifier.insert(.expanded)
    }
    if symTraits.contains(.traitMonoSpace) {
      modifier.insert(.monospace)
    }
    // if let s = descr.object(forKey: .featureSettings) as? [[UIFontDescriptor.FeatureKey: Int]] {
    // }
    self = modifier
  }
  
  func apply(to descr: UIFontDescriptor) -> UIFontDescriptor {
    var traits = descr.symbolicTraits
    if self.contains(.normal) {
      traits.remove(.traitBold)
      traits.remove(.traitItalic)
    }
    if self.contains(.italic) {
      traits.insert(.traitItalic)
    }
    if self.contains(.boldface) {
      traits.insert(.traitBold)
    }
    if self.contains(.unitalic) {
      traits.remove(.traitItalic)
    }
    if self.contains(.unboldface) {
      traits.remove(.traitBold)
    }
    if self.contains(.narrow) {
      traits.insert(.traitCondensed)
      traits.remove(.traitExpanded)
    }
    if self.contains(.condensed) {
      traits.insert(.traitCondensed)
      traits.remove(.traitExpanded)
    }
    if self.contains(.compressed) {
      traits.insert(.traitCondensed)
      traits.remove(.traitExpanded)
    }
    if self.contains(.expanded) {
      traits.insert(.traitExpanded)
      traits.remove(.traitCondensed)
    }
    if self.contains(.poster) {
    }
    if self.contains(.monospace) {
      traits.insert(.traitMonoSpace)
    }
    if self.contains(.smallCaps) {
      let smallLettersToSmallCapsAttribute: [UIFontDescriptor.FeatureKey: Int] = [
          .type: kLowerCaseType,
          .selector: kLowerCaseSmallCapsSelector
      ]
      let capitalLettersToSmallCapsAttribute: [UIFontDescriptor.FeatureKey: Int] = [
          .type: kUpperCaseType,
          .selector: kUpperCaseSmallCapsSelector
      ]
      return (descr.withSymbolicTraits(traits) ?? descr).addingAttributes([
        .featureSettings: [smallLettersToSmallCapsAttribute, capitalLettersToSmallCapsAttribute]
      ])
    } else {
      return descr.withSymbolicTraits(traits) ?? descr
    }
  }
}

extension UIImage {
  func getExifData() -> CFDictionary? {
    var exifData: CFDictionary? = nil
    if let data = self.jpegData(compressionQuality: 1.0) {
      data.withUnsafeBytes {
        let bytes = $0.baseAddress?.assumingMemoryBound(to: UInt8.self)
        if let cfData = CFDataCreate(kCFAllocatorDefault, bytes, data.count), 
           let source = CGImageSourceCreateWithData(cfData, nil) {
          exifData = CGImageSourceCopyPropertiesAtIndex(source, 0, nil)
        }
      }
    }
    return exifData
  }
}

extension UIFont {
  var rawWeight: CGFloat {
    guard let weightNumber = traits[.weight] as? NSNumber else {
      return 0.0
    }
    return CGFloat(weightNumber.doubleValue)
  }
  
  var weight: UIFont.Weight {
    guard let weightNumber = traits[.weight] as? NSNumber else {
      return .regular
    }
    return UIFont.Weight(rawValue: CGFloat(weightNumber.doubleValue))
  }

  private var traits: [UIFontDescriptor.TraitKey: Any] {
    return fontDescriptor.object(forKey: .traits) as? [UIFontDescriptor.TraitKey: Any] ?? [:]
  }
}
