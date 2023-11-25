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
import MarkdownKit

///
/// This class implements the library `(lispkit draw)`. 
///
public final class DrawingLibrary: NativeLibrary {
  
  /// Exported parameter objects
  public let drawingParam: Procedure
  public let shapeParam: Procedure
  
  /// Available color lists
  public var colorLists: [NSColorList]
  
  /// Symbols used in enumeration values
  
  // Bitmap file types
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
    self.colorLists = NSColorList.availableColorLists
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
    self.define(Procedure("inline-drawing", inlineDrawing))
    self.define(Procedure("save-drawing", saveDrawing))
    self.define(Procedure("save-drawings", saveDrawings))
    
    // Images/bitmaps
    self.define("image-type-tag", as: NativeImage.type.objectTypeTag())
    self.define(Procedure("image?", isImage))
    self.define(Procedure("load-image", loadImage))
    self.define(Procedure("load-image-asset", loadImageAsset))
    self.define(Procedure("bytevector->image", bytevectorToImage))
    self.define(Procedure("image-size", imageSize))
    self.define(Procedure("set-image-size!", setImageSize))
    self.define(Procedure("bitmap?", isBitmap))
    self.define(Procedure("bitmap-size", bitmapSize))
    self.define(Procedure("bitmap-pixels", bitmapPixels))
    self.define(Procedure("bitmap-exif-data", bitmapExifData))
    self.define(Procedure("set-bitmap-exif-data!", setBitmapExifData))
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
    self.define(Procedure("rectangle", rectangle))
    self.define(Procedure("circle", circle))
    self.define(Procedure("oval", oval))
    self.define(Procedure("arc", arc))
    self.define(Procedure("glyphs", glyphs))
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
    self.define(Procedure("available-color-lists", availableColorLists))
    self.define(Procedure("available-colors", availableColors))
    self.define(Procedure("load-color-list", loadColorList))
    
    // Fonts/points/sizes/rects
    self.define("font-type-tag", as: NativeFont.type.objectTypeTag())
    self.define(Procedure("font?", isFont))
    self.define(Procedure("font", font))
    self.define(Procedure("font-name", fontName))
    self.define(Procedure("font-family-name", fontFamilyName))
    self.define(Procedure("font-weight", fontWeight))
    self.define(Procedure("font-traits", fontTraits))
    self.define(Procedure("font-has-traits", fontHasTraits))
    self.define(Procedure("font-size", fontSize))
    self.define(Procedure("available-fonts", availableFonts))
    self.define(Procedure("available-font-families", availableFontFamilies))
    self.define(Procedure("point?", isPoint))
    self.define(Procedure("point", point))
    self.define(Procedure("point-x", pointX))
    self.define(Procedure("point-y", pointY))
    self.define(Procedure("move-point", movePoint))
    self.define(Procedure("size?", isSize))
    self.define(Procedure("size", size))
    self.define(Procedure("size-width", sizeWidth))
    self.define(Procedure("size-height", sizeHeight))
    self.define(Procedure("increase-size", increaseSize))
    self.define(Procedure("scale-size", scaleSize))
    self.define(Procedure("rect?", isRect))
    self.define(Procedure("rect", rect))
    self.define(Procedure("rect-point", rectPoint))
    self.define(Procedure("rect-size", rectSize))
    self.define(Procedure("rect-x", rectX))
    self.define(Procedure("rect-y", rectY))
    self.define(Procedure("rect-width", rectWidth))
    self.define(Procedure("rect-height", rectHeight))
    self.define(Procedure("move-rect", moveRect))
    
    // Utilities
    self.define(Procedure("text-size", textSize))
    self.define(Procedure("styled-text-size", styledTextSize))
    self.define(Procedure("html-size", htmlSize))
    
    // Define constants
    self.define("zero-point", via: "(define zero-point (point 0 0))")
    self.define("black", via: "(define black (color 0 0 0))")
    self.define("gray", via: "(define gray (color 0.5 0.5 0.5))")
    self.define("white", via: "(define white (color 1 1 1))")
    self.define("red", via: "(define red (color 1 0 0))")
    self.define("green", via: "(define green (color 0 1 0))")
    self.define("blue", via: "(define blue (color 0 0 1))")
    self.define("yellow", via: "(define yellow (color 1 1 0))")
    self.define("italic", via: "(define italic \(NSFontTraitMask.italicFontMask.rawValue))")
    self.define("boldface", via: "(define boldface \(NSFontTraitMask.boldFontMask.rawValue))")
    self.define("unitalic", via: "(define unitalic \(NSFontTraitMask.unitalicFontMask.rawValue))")
    self.define("unboldface", via: "(define unboldface \(NSFontTraitMask.unboldFontMask.rawValue))")
    self.define("narrow", via: "(define narrow \(NSFontTraitMask.narrowFontMask.rawValue))")
    self.define("expanded", via: "(define expanded \(NSFontTraitMask.expandedFontMask.rawValue))")
    self.define("condensed", via:
      "(define condensed \(NSFontTraitMask.condensedFontMask.rawValue))")
    self.define("small-caps", via:
      "(define small-caps \(NSFontTraitMask.smallCapsFontMask.rawValue))")
    self.define("poster", via: "(define poster \(NSFontTraitMask.posterFontMask.rawValue))")
    self.define("compressed", via:
      "(define compressed \(NSFontTraitMask.compressedFontMask.rawValue))")
    self.define("monospace", via:
      "(define monospace \(NSFontTraitMask.fixedPitchFontMask.rawValue))")
    self.define("ultralight", via: "(define ultralight 1)")
    self.define("thin", via: "(define thin 2)")
    self.define("light", via: "(define light 3)")
    self.define("book", via: "(define book 4)")
    self.define("normal", via: "(define normal 5)")
    self.define("medium", via: "(define medium 6)")
    self.define("demi", via: "(define demi 7)")
    self.define("semi", via: "(define semi 8)")
    self.define("bold", via: "(define bold 9)")
    self.define("extra", via: "(define extra 10)")
    self.define("heavy", via: "(define heavy 11)")
    self.define("super", via: "(define super 12)")
    self.define("ultra", via: "(define ultra 13)")
    self.define("extrablack", via: "(define extrablack 14)")
    
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
  
  public override func initializations() {
    // Load all color list assets
    if let filename = self.context.fileHandler.assetFilePath(forFile: "HtmlColors",
                                                             ofType: "plist",
                                                             inFolder: "ColorLists"),
       let colorList = NSColorList(name: "HTML", fromFile: filename) {
      self.colorLists.append(colorList)
    }
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
  
  private func image(from expr: Expr) throws -> NSImage {
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
                                relativeCenter: NSPoint(x: x, y: y)))
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
    try self.drawing(from: drawing).append(.strokeLine(NSPoint(x: sx, y: sy),
                                                       NSPoint(x: ex, y: ey)))
    return .void
  }
  
  private func drawRect(expr: Expr, drawing: Expr?) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                     .pair(.flonum(let w), .flonum(let h))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    try self.drawing(from: drawing).append(.strokeRect(NSRect(x: x, y: y, width: w, height: h)))
    return .void
  }
  
  private func fillRect(expr: Expr, drawing: Expr?) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                     .pair(.flonum(let w), .flonum(let h))) = expr else {
                      throw RuntimeError.eval(.invalidRect, expr)
    }
    try self.drawing(from: drawing).append(.fillRect(NSRect(x: x, y: y, width: w, height: h)))
    return .void
  }
  
  private func drawEllipse(expr: Expr, drawing: Expr?) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                     .pair(.flonum(let w), .flonum(let h))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    try self.drawing(from: drawing).append(.strokeEllipse(NSRect(x: x, y: y, width: w, height: h)))
    return .void
  }
  
  private func fillEllipse(expr: Expr, drawing: Expr?) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                     .pair(.flonum(let w), .flonum(let h))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    try self.drawing(from: drawing).append(.fillEllipse(NSRect(x: x, y: y, width: w, height: h)))
    return .void
  }
  
  private func asObjectLocation(_ location: Expr) throws -> ObjectLocation {
    switch location {
      case .pair(.flonum(let x), .flonum(let y)):
        return .position(NSPoint(x: x, y: y))
      case .pair(.pair(.flonum(let x), .flonum(let y)), .pair(.flonum(let w), .flonum(let h))):
        return .boundingBox(NSRect(x: x, y: y, width: w, height: h))
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
        loc = .position(NSPoint(x: x, y: y))
      case .pair(.pair(.flonum(let x), .flonum(let y)), .pair(.flonum(let w), .flonum(let h))):
        loc = .boundingBox(NSRect(x: x, y: y, width: w, height: h))
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
    let compositionOperation: NSCompositingOperation
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
        compositionOperation = .sourceOver
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
    try self.drawing(from: drawing.isFalse ? nil : drawing).append(.image(try self.image(from: bitmap),
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
  
  private func loadImage(filename: Expr) throws -> Expr {
    let path = self.context.fileHandler.path(try filename.asPath(),
                                             relativeTo: self.context.evaluator.currentDirectoryPath)
    guard let nsimage = NSImage(contentsOfFile: path) else {
      throw RuntimeError.eval(.cannotLoadImage, filename)
    }
    return .object(NativeImage(nsimage))
  }
  
  private func loadImageAsset(name: Expr, type: Expr, dir: Expr? = nil) throws -> Expr {
    if let path = self.context.fileHandler.assetFilePath(
                    forFile: try name.asString(),
                    ofType: try type.asString(),
                    inFolder: try dir?.asPath() ?? "Images",
                    relativeTo: self.context.evaluator.currentDirectoryPath) {
      guard let nsimage = NSImage(contentsOfFile: path) else {
        throw RuntimeError.eval(.cannotLoadImageAsset, name, type, dir ?? .makeString("Images"))
      }
      return .object(NativeImage(nsimage))
    } else {
      throw RuntimeError.eval(.cannotLoadImageAsset, name, type, dir ?? .makeString("Images"))
    }
  }
  
  private func bytevectorToImage(expr: Expr, args: Arguments) throws -> Expr {
    let subvec = try BytevectorLibrary.subVector("bytevector->image", expr, args)
    guard let nsimage = NSImage(data: Data(subvec)) else {
      throw RuntimeError.eval(.cannotCreateImage, expr)
    }
    return .object(NativeImage(nsimage))
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
    try self.image(from: image).size = NSSize(width: w, height: h)
    return .void
  }

  private func isBitmap(expr: Expr) -> Expr {
    if case .object(let obj) = expr, let image = (obj as? NativeImage)?.value {
      for repr in image.representations {
        if repr is NSBitmapImageRep {
          return .true
        }
      }
    }
    return .false
  }

  private func bitmapSize(expr: Expr) throws -> Expr {
    if case .object(let obj) = expr, let image = (obj as? NativeImage)?.value {
      for repr in image.representations {
        if let bm = repr as? NSBitmapImageRep, bm.size.width > 0.0 {
          return .pair(.flonum(Double(bm.size.width)), .flonum(Double(bm.size.height)))
        }
      }
    }
    return .false
  }

  private func bitmapPixels(expr: Expr) throws -> Expr {
    if case .object(let obj) = expr, let image = (obj as? NativeImage)?.value {
      for repr in image.representations {
        if let bm = repr as? NSBitmapImageRep, bm.size.width > 0.0 {
          return .pair(.fixnum(Int64(bm.pixelsWide)), .fixnum(Int64(bm.pixelsHigh)))
        }
      }
    }
    return .false
  }

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
        let res = NSArray()
        var lst = expr
        while case .pair(let head, let tail) = lst {
          if let v = self.exifValue(from: head) {
            res.adding(v)
          }
          lst = tail
        }
        return res
      default:
        return nil
    }
  }

  private func bitmapExifData(image: Expr) throws -> Expr {
    let image = try self.image(from: image)
    for repr in image.representations {
      if let bitmapRepr = repr as? NSBitmapImageRep,
         bitmapRepr.size.width > 0.0,
         let exifData = bitmapRepr.value(forProperty: .exifData),
         let exifDict = exifData as? NSDictionary {
        var res: Expr = .null
        for (key, value) in exifDict {
          if let k = key as? String,
             let v = self.expr(from: value) {
            res = .pair(.pair(.symbol(self.context.symbols.intern(k)), v), res)
          }
        }
        return res
      }
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
    // Create a bitmap suitable for storing the image in a PNG
    guard let bitmap = NSBitmapImageRep(bitmapDataPlanes: nil,
                                        pixelsWide: Int(w * scale),
                                        pixelsHigh: Int(h * scale),
                                        bitsPerSample: 8,
                                        samplesPerPixel: 4,
                                        hasAlpha: true,
                                        isPlanar: false,
                                        colorSpaceName: NSColorSpaceName.deviceRGB,
                                        bytesPerRow: 0,
                                        bitsPerPixel: 0) else {
      throw RuntimeError.eval(.cannotCreateBitmap,
                              .pair(drawing, .pair(size, .pair(dpi ?? .fixnum(72), .null))))
    }
    // Set the intended size of the image (vs. size of the bitmap above)
    bitmap.size = NSSize(width: w, height: h)
    // Create a graphics context for drawing into the bitmap
    guard let context = NSGraphicsContext(bitmapImageRep: bitmap) else {
      throw RuntimeError.eval(.cannotCreateBitmap,
                              .pair(drawing, .pair(size, .pair(dpi ?? .fixnum(72), .null))))
    }
    let previous = NSGraphicsContext.current
    // Create a flipped graphics context if required
    NSGraphicsContext.current = NSGraphicsContext(cgContext: context.cgContext, flipped: true)
    let transform = NSAffineTransform()
    transform.translateX(by: 0.0, yBy: CGFloat(h))
    transform.scaleX(by: 1.0, yBy: -1.0)
    transform.concat()
    defer {
      NSGraphicsContext.current = previous
    }
    // Draw into the bitmap
    try self.drawing(from: drawing).draw()
    // Create an image and add the bitmap as a representation
    let nsimage = NSImage(size: bitmap.size)
    nsimage.addRepresentation(bitmap)
    return .object(NativeImage(nsimage))
  }
  
  private func bitmapCrop(bitmap: Expr, rect: Expr) throws -> Expr {
    let nsImage = try self.image(from: bitmap)
    var pixels = nsImage.size
    for repr in nsImage.representations {
      if let bm = repr as? NSBitmapImageRep, bm.size.width > 0.0 {
        pixels.width = CGFloat(bm.pixelsWide)
        pixels.height = CGFloat(bm.pixelsHigh)
      }
    }
    guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                     .pair(.flonum(let w), .flonum(let h))) = rect else {
      throw RuntimeError.eval(.invalidRect,rect)
    }
    let bounds = NSRect(x: x, y: y, width: w, height: h)
                   .intersection(NSRect(x: 0, y: 0, width: pixels.width, height: pixels.height))
    guard bounds.width > 0, bounds.height > 0 else {
      return .false
    }
    let size = CGSize(width: nsImage.size.width * bounds.width / pixels.width,
                      height: nsImage.size.height * bounds.height / pixels.height)
    guard let cgImage = nsImage.cgImage(forProposedRect: nil, context: nil, hints: nil) else {
      return .false
    }
    let res = NSImage(size: size)
    res.addRepresentation(NSBitmapImageRep(ciImage: CIImage(cgImage: cgImage).cropped(to: bounds)))
    return .object(NativeImage(res))
  }
  
  private func bitmapBlur(bitmap: Expr, radius: Expr) throws -> Expr {
    let nsImage = try self.image(from: bitmap)
    guard let cgImage = nsImage.cgImage(forProposedRect: nil, context: nil, hints: nil) else {
      return .false
    }
    let image = CIImage(cgImage: cgImage)
    let radius = CGFloat(try radius.asDouble(coerce: true))
    let blurredImage = image.clampedToExtent()
                            .applyingFilter(
                                "CIGaussianBlur",
                                parameters: [ kCIInputRadiusKey: radius ])
                            .cropped(to: image.extent)
    let bitmap = NSBitmapImageRep(ciImage: blurredImage)
    let res = NSImage(size: nsImage.size)
    res.addRepresentation(bitmap)
    return .object(NativeImage(res))
  }
  
  private func saveBitmap(filename: Expr, bitmap: Expr, format: Expr) throws -> Expr {
    guard case .symbol(let sym) = format else {
      throw RuntimeError.eval(.invalidImageFileType, format)
    }
    let fileType: NSBitmapImageRep.FileType
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
    return self.saveInFile(try self.image(from: bitmap),
                           try filename.asPath(),
                           fileType) ? .true : .false
  }
  
  private func saveInFile(_ image: NSImage,
                          _ filename: String,
                          _ filetype: NSBitmapImageRep.FileType) -> Bool {
    let url = URL(fileURLWithPath:
      self.context.fileHandler.path(filename,
                                    relativeTo: self.context.evaluator.currentDirectoryPath))
    // Go through all representations and try to encode them as `filetype`; return once the first
    // succeeds
    for repr in image.representations {
      // Encode bitmap
      if let bitmapRepr = repr as? NSBitmapImageRep,
        let data = bitmapRepr.representation(using: filetype, properties: [:]) {
        // Write encoded data into a file
        do {
          try data.write(to: url, options: .atomic)
          return true
        } catch {
          return false
        }
      }
    }
    return false
  }
  
  private func bitmapToBytevector(bitmap: Expr, format: Expr) throws -> Expr {
    let image = try self.image(from: bitmap)
    guard case .symbol(let sym) = format else {
      throw RuntimeError.eval(.invalidImageFileType, format)
    }
    let fileType: NSBitmapImageRep.FileType
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
    for repr in image.representations {
      if let bitmapRepr = repr as? NSBitmapImageRep,
         let data = bitmapRepr.representation(using: fileType, properties: [:]) {
        let count = data.count
        var res = [UInt8](repeating: 0, count: count)
        data.copyBytes(to: &res, count: count)
        return .bytes(MutableBox(res))
      }
    }
    return .false
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
      return .object(Shape(.shape(try self.shape(from: prototype))))
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
    shape.append(.move(to: NSPoint(x: sx, y: sy)))
    shape.append(.line(to: NSPoint(x: ex, y: ey)))
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
        shape.append(.move(to: NSPoint(x: x, y: y)))
      } else {
        shape.append(.line(to: NSPoint(x: x, y: y)))
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
        return .object(Shape(.roundedRect(NSRect(x: x, y: y, width: w, height: h),
                                          xradius: xrad,
                                          yradius: yrad)))
      }
      return .object(Shape(.roundedRect(NSRect(x: x, y: y, width: w, height: h),
                                        xradius: xrad,
                                        yradius: xrad)))
    }
    return .object(Shape(.rect(NSRect(x: x, y: y, width: w, height: h))))
  }
  
  private func circle(point: Expr, radius: Expr) throws -> Expr {
    guard case .pair(.flonum(let x), .flonum(let y)) = point else {
      throw RuntimeError.eval(.invalidPoint, point)
    }
    let rad = try radius.asDouble(coerce: true)
    return .object(Shape(.oval(NSRect(x: x - rad, y: y - rad,
                                      width: 2.0 * rad, height: 2.0 * rad))))
  }
  
  private func oval(point: Expr, size: Expr) throws -> Expr {
    guard case .pair(.flonum(let x), .flonum(let y)) = point else {
      throw RuntimeError.eval(.invalidPoint, point)
    }
    guard case .pair(.flonum(let w), .flonum(let h)) = size else {
      throw RuntimeError.eval(.invalidSize, size)
    }
    return .object(Shape(.oval(NSRect(x: x, y: y, width: w, height: h))))
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
      return .object(Shape(.arc(center: NSPoint(x: x, y: y),
                                radius: rad,
                                startAngle: start,
                                endAngle: end,
                                clockwise: clockwise?.isTrue ?? true)))
    } else {
      return .object(Shape(.oval(NSRect(x: x - rad,
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
                                 in: NSRect(x: x, y: y, width: w, height: h),
                                 font: fontBox.value,
                                 flipped: true)))
  }
  
  private func transformShape(shape: Expr, transformation: Expr) throws -> Expr {
    return .object(Shape(.transformed(try self.shape(from: shape),
                                      try self.tformation(from: transformation))))
  }
  
  private func flipShape(shape: Expr, box: Expr?, orientation: Expr?) throws -> Expr {
    let bounds: NSRect?
    if let box = box {
      guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                       .pair(.flonum(let w), .flonum(let h))) = box else {
        throw RuntimeError.eval(.invalidRect, box)
      }
      bounds = NSRect(x: x, y: y, width: w, height: h)
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
    let nspoints = try self.nsPoints(from: points)
    return .object(Shape(.interpolated(nspoints, method: method)))
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
  
  private func moveTo(point: Expr, shape: Expr?) throws -> Expr {
    guard case .pair(.flonum(let x), .flonum(let y)) = point else {
      throw RuntimeError.eval(.invalidPoint, point)
    }
    (try self.shape(from: shape)).append(.move(to: NSPoint(x: x, y: y)))
    return .void
  }
  
  private func lineTo(args: Arguments) throws -> Expr {
    let (shape, skipLast) = try self.shape(from: args)
    var points = self.pointList(args, skipLast: skipLast)
    while case .pair(let point, let rest) = points {
      guard case .pair(.flonum(let x), .flonum(let y)) = point else {
        throw RuntimeError.eval(.invalidPoint, point)
      }
      shape.append(.line(to: NSPoint(x: x, y: y)))
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
      .curve(to: NSPoint(x: x, y: y),
             controlCurrent: NSPoint(x: c1x, y: c1y),
             controlTarget: NSPoint(x: c2x, y: c2y)))
    return .void
  }
  
  private func relativeMoveTo(point: Expr, shape: Expr?) throws -> Expr {
    guard case .pair(.flonum(let x), .flonum(let y)) = point else {
      throw RuntimeError.eval(.invalidPoint, point)
    }
    (try self.shape(from: shape)).append(.relativeMove(to: NSPoint(x: x, y: y)))
    return .void
  }
  
  private func relativeLineTo(args: Arguments) throws -> Expr {
    let (shape, skipLast) = try self.shape(from: args)
    var points = self.pointList(args, skipLast: skipLast)
    while case .pair(let point, let rest) = points {
      guard case .pair(.flonum(let x), .flonum(let y)) = point else {
        throw RuntimeError.eval(.invalidPoint, point)
      }
      shape.append(.relativeLine(to: NSPoint(x: x, y: y)))
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
      .relativeCurve(to: NSPoint(x: x, y: y),
                     controlCurrent: NSPoint(x: c1x, y: c1y),
                     controlTarget: NSPoint(x: c2x, y: c2y)))
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
  
  
  // Transformations
  
  private func isTransformation(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is Transformation {
      return .true
    }
    return .false
  }
  
  private func transformation(args: Arguments) throws -> Expr {
    var transform = AffineTransform()
    for arg in args {
      transform.append(try self.affineTransform(arg))
    }
    return .object(Transformation(transform))
  }
  
  private func invert(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let tf = obj as? Transformation else {
      throw RuntimeError.type(expr, expected: [Transformation.type])
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
  
  private func tformation(from expr: Expr) throws -> Transformation {
    guard case .object(let obj) = expr, let transform = obj as? Transformation else {
      throw RuntimeError.type(expr, expected: [Transformation.type])
    }
    return transform
  }
  
  private func affineTransform(_ expr: Expr?) throws -> AffineTransform {
    guard let tf = expr else {
      return AffineTransform()
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
      let colorName = try red.asSymbol().identifier
      var colorListName = "Apple"
      if args.count == 1 {
        colorListName = try args.first!.asString()
      }
      if let colorList = NSColorList(named: colorListName) {
        if let nsColor = colorList.color(withKey: colorName) {
          return .object(Color(red: Double(nsColor.redComponent),
                               green: Double(nsColor.greenComponent),
                               blue: Double(nsColor.blueComponent),
                               alpha: Double(nsColor.alphaComponent)))
        } else {
          throw RuntimeError.custom("eval error", "unknown color \(colorName) in color list " +
                                    colorListName, [])
        }
      } else {
        throw RuntimeError.custom("eval error", "unknown color list: \(colorListName)", [])
      }
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
  
  private func availableColorLists() -> Expr {
    var res = Expr.null
    for cl in self.colorLists {
      if let name = cl.name {
        res = .pair(.makeString(name), res)
      }
    }
    return res
  }
  
  private func availableColors(name: Expr) throws -> Expr {
    if let colorList = NSColorList(named: try name.asString()) {
      var keys = Expr.null
      for key in colorList.allKeys {
        keys = .pair(.symbol(self.context.symbols.intern(key)), keys)
      }
      return keys
    } else {
      throw RuntimeError.custom("eval error", "unknown color list: \(name)", [])
    }
  }
  
  private func loadColorList(_ name: Expr, _ path: Expr) throws -> Expr {
    let colorListName = try name.asString()
    if let filename = self.context.fileHandler.assetFilePath(forFile: try path.asString(),
                                                             ofType: "plist",
                                                             inFolder: "ColorLists"),
       let colorList = NSColorList(name: colorListName, fromFile: filename) {
      for i in self.colorLists.indices {
        if let name = self.colorLists[i].name, name == colorListName {
          self.colorLists[i] = colorList
          return .true
        }
      }
      self.colorLists.append(colorList)
      return .true
    } else {
      return .false
    }
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
  
  private func moveRect(expr: Expr, dx: Expr, dy: Expr) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(let y)), let dim) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    return try .pair(.pair(.flonum(x + dx.asDouble(coerce: true)),
                           .flonum(y + dy.asDouble(coerce: true))), dim)
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
      guard let nsfont = NSFont(name: name, size: CGFloat(size)) else {
        return .false
      }
      return .object(NativeFont(nsfont))
    // A variant of `font` which loads the font based on a font family, a size, weight, and traits
    } else {
      var weight: Int? = nil
      var traits: Int = 0
      for arg in args {
        if weight == nil {
          weight = try arg.asInt(below: 16)
        } else {
          traits |= try arg.asInt()
        }
      }
      guard let nsfont = NSFontManager.shared.font(withFamily: name,
                                                   traits: NSFontTraitMask(rawValue: UInt(traits)),
                                                   weight: weight!,
                                                   size: CGFloat(size)) else {
        return .false
      }
      return .object(NativeFont(nsfont))
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
    guard let familyName = fontBox.value.familyName else {
      return .false
    }
    return .makeString(familyName)
  }
  
  private func fontWeight(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let fontBox = obj as? NativeFont else {
      throw RuntimeError.type(expr, expected: [NativeFont.type])
    }
    let weight = NSFontManager.shared.weight(of: fontBox.value)
    return .fixnum(Int64(weight))
  }
  
  private func fontTraits(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let fontBox = obj as? NativeFont else {
      throw RuntimeError.type(expr, expected: [NativeFont.type])
    }
    let traits = NSFontManager.shared.traits(of: fontBox.value).rawValue
    guard traits < Int64.max else {
      return .false
    }
    return .fixnum(Int64(traits))
  }
  
  private func fontHasTraits(expr: Expr, args: Arguments) throws -> Expr {
    guard case .object(let obj) = expr, let fontBox = obj as? NativeFont else {
      throw RuntimeError.type(expr, expected: [NativeFont.type])
    }
    var traits: Int = 0
    for arg in args {
      traits |= try arg.asInt()
    }
    return .makeBoolean(
      NSFontManager.shared.fontNamed(fontBox.value.fontName,
                                     hasTraits: NSFontTraitMask(rawValue: UInt(traits))))
  }
  
  private func fontSize(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let fontBox = obj as? NativeFont else {
      throw RuntimeError.type(expr, expected: [NativeFont.type])
    }
    return .flonum(Double(fontBox.value.pointSize))
  }
  
  private func availableFonts(args: Arguments) throws -> Expr {
    var fonts: Set<String>
    if args.isEmpty {
      fonts = Set(NSFontManager.shared.availableFonts)
    } else {
      var traits: Int = 0
      for arg in args {
        traits |= try arg.asInt()
      }
      guard let fnts = NSFontManager.shared.availableFontNames(with:
                         NSFontTraitMask(rawValue: UInt(traits))) else {
          return .false
      }
      fonts = Set(fnts)
    }
    let fontList = fonts.sorted(by: >)
    var res: Expr = .null
    for font in fontList {
      res = .pair(.makeString(font), res)
    }
    return res
  }
  
  private func availableFontFamilies() throws -> Expr {
    let fontFamilies = NSFontManager.shared.availableFontFamilies.reversed()
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
    let fnt: NSFont
    if let font = font, !font.isFalse {
      guard case .object(let obj) = font, let f = (obj as? NativeFont)?.value else {
        throw RuntimeError.type(font, expected: [NativeFont.type])
      }
      fnt = f
    } else {
      fnt = NSFont.systemFont(ofSize: NSFont.systemFontSize)
    }
    let size: NSSize
    switch dimensions {
      case .none:
        size = NSSize(width: CGFloat.infinity, height: CGFloat.infinity)
      case .some(.pair(.flonum(let w), .flonum(let h))):
        size = NSSize(width: w, height: h)
      case .some(let w):
        size = NSSize(width: CGFloat(try w.asDouble(coerce: true)), height: CGFloat.infinity)
    }
    let pstyle: NSParagraphStyle = .default
    let attributes = [.font: fnt, .paragraphStyle: pstyle] as [NSAttributedString.Key: Any]
    let rect = str.boundingRect(with: size,
                                options: [.usesLineFragmentOrigin, .usesFontLeading],
                                attributes: attributes)
    return .pair(.flonum(Double(rect.width)), .flonum(Double(rect.height)))
  }
  
  private func styledTextSize(text: Expr, dimensions: Expr?) throws -> Expr {
    guard case .object(let obj) = text, let str = (obj as? StyledText)?.value else {
      throw RuntimeError.type(text, expected: [StyledText.type])
    }
    let size: NSSize
    switch dimensions {
      case .none:
        size = NSSize(width: CGFloat.infinity, height: CGFloat.infinity)
      case .some(.pair(.flonum(let w), .flonum(let h))):
        size = NSSize(width: w, height: h)
      case .some(let w):
        size = NSSize(width: CGFloat(try w.asDouble(coerce: true)), height: CGFloat.infinity)
    }
    let rect = str.boundingRect(with: size, options: [.usesLineFragmentOrigin, .usesFontLeading])
    return .pair(.flonum(Double(rect.width)), .flonum(Double(rect.height)))
  }
  
  private func htmlSize(text: Expr, dimensions: Expr?) throws -> Expr {
    let http = Data(try text.asString().utf8)
    let str =
      try NSAttributedString(data: http,
                             options: [.documentType: NSAttributedString.DocumentType.html,
                                       .characterEncoding: String.Encoding.utf8.rawValue],
                             documentAttributes: nil)
    let size: NSSize
    switch dimensions {
      case .none:
        size = NSSize(width: CGFloat.infinity, height: CGFloat.infinity)
      case .some(.pair(.flonum(let w), .flonum(let h))):
        size = NSSize(width: w, height: h)
      case .some(let w):
        size = NSSize(width: CGFloat(try w.asDouble(coerce: true)), height: CGFloat.infinity)
    }
    let rect = str.boundingRect(with: size, options: [.usesLineFragmentOrigin, .usesFontLeading])
    return .pair(.flonum(Double(rect.width)), .flonum(Double(rect.height)))
  }
}

public final class NativeFont: AnyNativeObject<NSFont> {

  /// Type representing fonts
  public static let type = Type.objectType(Symbol(uninterned: "font"))

  public override var type: Type {
    return NativeFont.type
  }

  public override var string: String {
    return "#<font \(self.value.fontName) \(self.value.pointSize)>"
  }
  
  public override func unpack() -> Exprs {
    return [.makeString(self.value.fontName),
            .makeString(self.value.familyName ?? self.value.fontName),
            .makeNumber(self.value.pointSize)]
  }
}

public final class NativeImage: AnyNativeObject<NSImage> {

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
  
  public override func unpack() -> Exprs {
    return [.makeString(self.identityString),
            .makeNumber(self.value.size.width),
            .makeNumber(self.value.size.height)]
  }
}
