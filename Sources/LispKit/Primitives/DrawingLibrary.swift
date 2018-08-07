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

///
/// This class implements the library `(lispkit draw)`. 
///
public final class DrawingLibrary: NativeLibrary {
  
  /// Imported native library
  private var systemLibrary: SystemLibrary!
  
  /// Exported parameter objects
  public let drawingParam: Procedure
  public let shapeParam: Procedure
  
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
    try super.init(in: context)
  }
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "draw"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"],    "define-syntax", "syntax-rules")
    self.`import`(from: ["lispkit", "control"], "let")
    self.`import`(from: ["lispkit", "dynamic"], "parameterize")
    self.`import`(from: ["lispkit", "system"],  "current-directory")
  }
  
  /// Declarations of the library.
  public override func declarations() {
    // Parameter objects
    self.define("current-drawing", as: self.drawingParam)
    self.define("current-shape", as: self.shapeParam)
    // Drawings
    self.define(Procedure("drawing?", isDrawing))
    self.define(Procedure("make-drawing", makeDrawing))
    self.define(Procedure("copy-drawing", copyDrawing))
    self.define(Procedure("set-color", setColor))
    self.define(Procedure("set-fill-color", setFillColor))
    self.define(Procedure("set-shadow", setShadow))
    self.define(Procedure("remove-shadow", removeShadow))
    self.define(Procedure("enable-transformation", enableTransformation))
    self.define(Procedure("disable-transformation", disableTransformation))
    self.define(Procedure("draw", draw))
    self.define(Procedure("fill", fill))
    self.define(Procedure("draw-text", drawText))
    self.define(Procedure("draw-image", drawImage))
    self.define(Procedure("draw-drawing", drawDrawing))
    self.define(Procedure("inline-drawing", inlineDrawing))
    self.define(Procedure("save-drawing", saveDrawing))
    
    // Images/bitmaps
    self.define(Procedure("image?", isImage))
    self.define(Procedure("load-image", loadImage))
    self.define(Procedure("image-size", imageSize))
    self.define(Procedure("bitmap?", isBitmap))
    self.define(Procedure("make-bitmap", makeBitmap))
    self.define(Procedure("save-bitmap", saveBitmap))
    
    // Shapes
    self.define(Procedure("shape?", isShape))
    self.define(Procedure("make-shape", makeShape))
    self.define(Procedure("copy-shape", copyShape))
    self.define(Procedure("line", line))
    self.define(Procedure("polygon", polygon))
    self.define(Procedure("rectangular", rectangular))
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
    
    // Fonts/points/sizes/rects
    self.define(Procedure("font?", isFont))
    self.define(Procedure("font", font))
    self.define(Procedure("font-name", fontName))
    self.define(Procedure("font-size", fontSize))
    self.define(Procedure("point?", isPoint))
    self.define(Procedure("point", point))
    self.define(Procedure("point-x", pointX))
    self.define(Procedure("point-y", pointY))
    self.define(Procedure("size?", isSize))
    self.define(Procedure("size", size))
    self.define(Procedure("size-width", sizeWidth))
    self.define(Procedure("size-height", sizeHeight))
    self.define(Procedure("rect?", isRect))
    self.define(Procedure("rect", rect))
    self.define(Procedure("rect-point", rectPoint))
    self.define(Procedure("rect-size", rectSize))
    self.define(Procedure("rect-x", rectX))
    self.define(Procedure("rect-y", rectY))
    self.define(Procedure("rect-width", rectWidth))
    self.define(Procedure("rect-height", rectHeight))
    
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
    self.systemLibrary = self.nativeLibrary(SystemLibrary.self)
  }
  
  private func drawing(from expr: Expr?) throws -> Drawing {
    if let expr = expr {
      guard case .object(let obj) = expr, let drawing = obj as? Drawing else {
        throw RuntimeError.type(expr, expected: [.drawingType])
      }
      return drawing
    }
    guard let value = self.context.machine.getParam(self.drawingParam) else {
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
        throw RuntimeError.type(expr, expected: [.shapeType])
      }
      return shape
    }
    guard let value = self.context.machine.getParam(self.shapeParam) else {
      throw RuntimeError.eval(.invalidDefaultShape, .false)
    }
    guard case .object(let obj) = value, let shape = obj as? Shape else {
      throw RuntimeError.eval(.invalidDefaultShape, value)
    }
    return shape
  }
  
  private func image(from expr: Expr) throws -> NSImage {
    guard case .object(let obj) = expr,
          let imageBox = obj as? ImmutableBox<NSImage> else {
      throw RuntimeError.type(expr, expected: [.imageType])
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
  
  private func setColor(color: Expr, drawing: Expr?) throws -> Expr {
    try self.drawing(from: drawing).append(.setStrokeColor(try self.color(from: color)))
    return .void
  }
  
  private func setFillColor(color: Expr, drawing: Expr?) throws -> Expr {
    try self.drawing(from: drawing).append(.setFillColor(try self.color(from: color)))
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
    try self.drawing(from: drawing).append(.concatTransformation(try self.transformation(from: tf)))
    return .void
  }
  
  private func disableTransformation(tf: Expr, drawing: Expr?) throws -> Expr {
    try self.drawing(from: drawing).append(.undoTransformation(try self.transformation(from: tf)))
    return .void
  }
  
  private func draw(shape: Expr, width: Expr?, drawing: Expr?) throws -> Expr {
    let width = try width?.asDouble(coerce: true) ?? 1.0
    try self.drawing(from: drawing).append(.stroke(try self.shape(from: shape), width: width))
    return .void
  }
  
  private func fill(shape: Expr, drawing: Expr?) throws -> Expr {
    try self.drawing(from: drawing).append(.fill(try self.shape(from: shape)))
    return .void
  }
  
  private func fillGradient(shape: Expr, cols: Expr, gradient: Expr?, drawing: Expr?) throws -> Expr {
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
  
  private func drawText(text: Expr,
                        location: Expr,
                        font: Expr,
                        color: Expr?,
                        drawing: Expr?) throws -> Expr {
    guard case .object(let obj) = font, let fnt = (obj as? ImmutableBox<NSFont>)?.value else {
      throw RuntimeError.type(font, expected: [.fontType])
    }
    let color = color ?? .object(ImmutableBox(Color.black))
    guard case .object(let obj2) = color, let clr = (obj2 as? ImmutableBox<Color>)?.value else {
      throw RuntimeError.type(color, expected: [.colorType])
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
    try self.drawing(from: drawing).append(.text(try text.asString(),
                                                 font: fnt,
                                                 color: clr,
                                                 style: nil,
                                                 at: loc))
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
  
  private func saveDrawing(drawing: Expr,
                           path: Expr,
                           size: Expr,
                           title: Expr?,
                           author: Expr?) throws -> Expr {
    let url = URL(fileURLWithPath:
      self.context.fileHandler.path(try path.asPath(),
                                    relativeTo: self.systemLibrary.currentDirectoryPath))
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
  
  // Images/bitmaps
  
  private func isImage(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is ImmutableBox<NSImage> {
      return .true
    }
    return .false
  }
  
  private func loadImage(filename: Expr) throws -> Expr {
    let path = self.context.fileHandler.path(try filename.asPath(),
                                             relativeTo: self.systemLibrary.currentDirectoryPath)
    guard let nsimage = NSImage(contentsOfFile: path) else {
      throw RuntimeError.eval(.cannotLoadImage, filename)
    }
    return .object(ImmutableBox(nsimage))
  }
  
  private func imageSize(image: Expr) throws -> Expr {
    let size = try self.image(from: image).size
    if size.width == 0.0 && size.height == 0.0 {
      return .false
    } else {
      return .pair(.flonum(Double(size.width)), .flonum(Double(size.width)))
    }
  }
  
  private func isBitmap(expr: Expr) -> Expr {
    if case .object(let obj) = expr, let image = (obj as? ImmutableBox<NSImage>)?.value {
      for repr in image.representations {
        if repr is NSBitmapImageRep {
          return .true
        }
      }
    }
    return .false
  }
  
  private func makeBitmap(drawing: Expr, size: Expr, scle: Expr) throws -> Expr {
    guard case .pair(.flonum(let w), .flonum(let h)) = size, w > 0.0 && h > 0.0 else {
      throw RuntimeError.eval(.invalidSize, size)
    }
    let scale = try scle.asDouble(coerce: true)
    guard scale > 0.0 && scale <= 10.0 else {
      throw RuntimeError.range(parameter: 3,
                               of: "make-bitmap",
                               scle,
                               min: 0,
                               max: 10,
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
                              .pair(drawing, .pair(size, .pair(scle, .null))))
    }
    // Set the intended size of the image (vs. size of the bitmap above)
    bitmap.size = NSSize(width: w, height: h)
    // Create a graphics context for drawing into the bitmap
    guard let context = NSGraphicsContext(bitmapImageRep: bitmap) else {
      throw RuntimeError.eval(.cannotCreateBitmap,
                              .pair(drawing, .pair(size, .pair(scle, .null))))
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
    return .object(ImmutableBox(nsimage))
  }
  
  private func saveBitmap(bitmap: Expr, filename: Expr, format: Expr) throws -> Expr {
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
      self.context.fileHandler.path(filename, relativeTo: self.systemLibrary.currentDirectoryPath))
    // Go through all representations and try to encode them as PNG; return once the first
    // succeeds
    for repr in image.representations {
      // Encode bitmap in PNG format
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
  
  private func rectangular(point: Expr, size: Expr, xradius: Expr?, yradius: Expr?) throws -> Expr {
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
    guard case .object(let obj) = font, let fontBox = obj as? ImmutableBox<NSFont> else {
      throw RuntimeError.type(font, expected: [.fontType])
    }
    return .object(Shape(.glyphs(try text.asString(),
                                 in: NSRect(x: x, y: y, width: w, height: h),
                                 font: fontBox.value,
                                 flipped: true)))
  }
  
  private func transformShape(shape: Expr, transformation: Expr) throws -> Expr {
    return .object(Shape(.transformed(try self.shape(from: shape),
                                      try self.transformation(from: transformation))))
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
    var shape = try self.shape(from: nil)
    var skipLast = false
    if case .some(.object(let obj)) = args.last, let sh = obj as? Shape {
      shape = sh
      skipLast = true
    }
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
    var shape = try self.shape(from: nil)
    var skipLast = false
    if case .some(.object(let obj)) = args.last, let sh = obj as? Shape {
      shape = sh
      skipLast = true
    }
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
                                      alpha: try (alpha ?? .flonum(1.0)).asDouble(coerce: true))))
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
  
  private func color(from expr: Expr) throws -> Color {
    guard case .object(let obj) = expr, let colorRef = obj as? ImmutableBox<Color> else {
      throw RuntimeError.type(expr, expected: [.colorType])
    }
    return colorRef.value
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
  
  private func isFont(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is ImmutableBox<NSFont> {
      return .true
    }
    return .false
  }
  
  private func font(font: Expr, size: Expr) throws -> Expr {
    guard let nsfont = NSFont(name: try font.asString(),
                              size: CGFloat(try size.asDouble(coerce: true))) else {
      return .false
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
