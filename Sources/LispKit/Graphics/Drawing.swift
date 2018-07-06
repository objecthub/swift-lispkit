//
//  Drawing.swift
//  LispKit
//
//  Created by Matthias Zenger on 24/06/2018.
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
import CoreGraphics

///
/// Class `Drawing` represents a sequence of drawing instructions. The class offers the
/// following functionality:
///   - New instructions can be appended to a drawing
///   - The drawing can be drawn to the current graphics context
///   - The drawing can be written to a file. Natively supported are PDF, PNG, and JPEG.
///
public final class Drawing: Reference {
  
  /// The sequence of drawing instructions.
  public private(set) var instructions: [DrawingInstruction]
  
  /// Initializer providing an initial sequence of drawing instructions.
  public init(_ instructions: DrawingInstruction...) {
    self.instructions = instructions
  }
  
  /// Name of this reference type
  public override var typeDescription: String {
    return "drawing"
  }
  
  /// Appends a new drawing instruction.
  @discardableResult public func append(_ instruction: DrawingInstruction) -> Bool {
    switch instruction {
      case .inline(let drawing), .include(let drawing, _):
        if drawing.includes(self) {
          return false
        }
      default:
        break
    }
    self.instructions.append(instruction)
    return true
  }
  
  /// Draws the drawing to the current graphics context clipped to a given shape. The drawing is
  /// put into a new transparency layer. Upon exit, the previous graphics state is being
  /// restored.
  public func draw(clippedTo shape: Shape? = nil) {
    if let context = NSGraphicsContext.current {
      context.saveGraphicsState()
      context.cgContext.beginTransparencyLayer(auxiliaryInfo: nil)
      shape?.compile().addClip()
      defer {
        context.cgContext.endTransparencyLayer()
        context.restoreGraphicsState()
      }
      self.drawInline()
    }
  }
  
  /// Draw the drawing into the current graphics context without saving and restoring the
  /// graphics state.
  public func drawInline() {
    for instruction in self.instructions {
      instruction.draw()
    }
  }
  
  /// Returns true if the given drawing is included or inlined into this drawing.
  public func includes(_ drawing: Drawing) -> Bool {
    guard self !== drawing else {
      return true
    }
    for instruction in self.instructions {
      switch instruction {
        case .inline(let other), .include(let other, _):
          if other.includes(drawing) {
            return true
          }
        default:
          break
      }
    }
    return false
  }
  
  /// Saves the drawing into a PDF file at URL `url`. The canvas's size is provide via the
  /// `width` and `height` parameters. If `flipped` is set to false, it is assumed that the
  /// origin of the coordinate system is in the lower-left corner of the canvas with x values
  /// increasing to the right and y values increasing upwards. If `flipped` is set to true,
  /// the origin of the coordinate system is in the upper-left corner of the canvas with x values
  /// increasing to the right and y values increasing downwards.
  ///
  /// The optional parameters `title`, `author`, and `creator` are stored in the metadata of
  /// the generated PDF file.
  public func saveAsPDF(url: URL,
                        width: Int,
                        height: Int,
                        flipped: Bool = false,
                        title: String? = nil,
                        author: String? = nil,
                        creator: String? = nil) -> Bool {
    var mediaBox = NSRect(x: 0, y: 0, width: Double(width), height: Double(height))
    // Create a core graphics context suitable for drawing the image into a PDF file
    var pdfInfo = NSMutableDictionary()
    if let title = title {
      pdfInfo[kCGPDFContextTitle] = title
    }
    if let author = author {
      pdfInfo[kCGPDFContextAuthor] = author
    }
    if let creator = creator {
      pdfInfo[kCGPDFContextCreator] = creator
    }
    guard let cgc = CGContext(url as CFURL, mediaBox: &mediaBox, pdfInfo as CFDictionary) else {
      return false
    }
    // Create a graphics context for drawing into a PDF
    let context = NSGraphicsContext(cgContext: cgc, flipped: flipped)
    let previous = NSGraphicsContext.current
    NSGraphicsContext.current = context
    defer {
      NSGraphicsContext.current = previous
    }
    // Create a new PDF page
    cgc.beginPDFPage(nil)
    cgc.saveGState()
    // Flip graphics if required
    if flipped {
      cgc.translateBy(x: 0.0, y: CGFloat(height))
      cgc.scaleBy(x: 1.0, y: -1.0)
    }
    // Draw the image
    self.draw()
    cgc.restoreGState()
    // Close PDF page and document
    cgc.endPDFPage()
    cgc.closePDF()
    return true
  }
  
  /// Saves the drawing into a PNG file at URL `url`. The canvas's size is provide via the
  /// `width` and `height` parameters. The `scale` factor determines the actual size of the
  /// bitmap when multiplied with `width` and `height`. For instance, setting `scale` to 2.0
  /// will result in a PNF file using a "Retina"/2x resolution.
  ///
  /// If `flipped` is set to false, it is assumed that the origin of the coordinate system is
  /// in the lower-left corner of the canvas with x values increasing to the right and y values
  /// increasing upwards. If `flipped` is set to true, the origin of the coordinate system is
  /// in the upper-left corner of the canvas with x values increasing to the right and y
  /// values increasing downwards.
  public func saveAsPNG(url: URL,
                        width: Int,
                        height: Int,
                        scale: Double = 1.0,
                        flipped: Bool = false) -> Bool {
    return self.saveAsBitmap(url: url,
                             width: width,
                             height: height,
                             scale: scale,
                             format: NSBitmapImageRep.FileType.png,
                             flipped: flipped)
  }
  
  /// Saves the drawing into a JPEF file at URL `url`. The canvas's size is provide via the
  /// `width` and `height` parameters. The `scale` factor determines the actual size of the
  /// bitmap when multiplied with `width` and `height`. For instance, setting `scale` to 2.0
  /// will result in a JPEG file using a "Retina"/2x resolution.
  ///
  /// If `flipped` is set to false, it is assumed that the origin of the coordinate system is
  /// in the lower-left corner of the canvas with x values increasing to the right and y values
  /// increasing upwards. If `flipped` is set to true, the origin of the coordinate system is
  /// in the upper-left corner of the canvas with x values increasing to the right and y
  /// values increasing downwards.
  public func saveAsJPG(url: URL,
                        width: Int,
                        height: Int,
                        scale: Double = 1.0,
                        flipped: Bool = false) -> Bool {
    return self.saveAsBitmap(url: url,
                             width: width,
                             height: height,
                             scale: scale,
                             format: NSBitmapImageRep.FileType.jpeg,
                             flipped: flipped)
  }
  
  /// Saves the drawing into a file at URL `url` of format `format`. The canvas's size is
  /// provide via the `width` and `height` parameters. The `scale` factor determines the
  /// actual size of the bitmap when multiplied with `width` and `height`. For instance,
  /// setting `scale` to 2.0 will result in file using a "Retina"/2x resolution.
  ///
  /// If `flipped` is set to false, it is assumed that the origin of the coordinate system is
  /// in the lower-left corner of the canvas with x values increasing to the right and y values
  /// increasing upwards. If `flipped` is set to true, the origin of the coordinate system is
  /// in the upper-left corner of the canvas with x values increasing to the right and y
  /// values increasing downwards.
  public func saveAsBitmap(url: URL,
                           width: Int,
                           height: Int,
                           scale: Double,
                           format: NSBitmapImageRep.FileType,
                           flipped: Bool) -> Bool {
    // Create a bitmap suitable for storing the image in a PNG
    guard let bitmap = NSBitmapImageRep(bitmapDataPlanes: nil,
                                        pixelsWide: Int(Double(width) * scale),
                                        pixelsHigh: Int(Double(height) * scale),
                                        bitsPerSample: 8,
                                        samplesPerPixel: 4,
                                        hasAlpha: true,
                                        isPlanar: false,
                                        colorSpaceName: Color.colorSpaceName,
                                        bytesPerRow: 0,
                                        bitsPerPixel: 0) else {
      return false
    }
    // Set the intended size of the image (vs. size of the bitmap above)
    bitmap.size = NSSize(width: width, height: height)
    // Create a graphics context for drawing into the bitmap
    guard let context = NSGraphicsContext(bitmapImageRep: bitmap) else {
      return false
    }
    let previous = NSGraphicsContext.current
    // Create a flipped graphics context if required
    if flipped {
      NSGraphicsContext.current = NSGraphicsContext(cgContext: context.cgContext, flipped: true)
      let transform = NSAffineTransform()
      transform.translateX(by: 0.0, yBy: CGFloat(height))
      transform.scaleX(by: 1.0, yBy: -1.0)
      transform.concat()
    } else {
      NSGraphicsContext.current = context
    }
    defer {
      NSGraphicsContext.current = previous
    }
    // Draw the image
    self.draw()
    // Encode bitmap in PNG format
    guard let data = bitmap.representation(using: format, properties: [:]) else {
      return false
    }
    // Write encoded data into a file
    do {
      try data.write(to: url, options: .atomic)
      return true
    } catch {
      return false
    }
  }
  
  public func flush() {
    for instruction in self.instructions {
      instruction.markDirty()
    }
  }
}

///
/// Enumeration of all supported drawing instructions.
///
public enum DrawingInstruction {
  case setStrokeColor(Color)
  case setFillColor(Color)
  case setBlendMode(BlendMode)
  case setShadow(Color, dx: Double, dy: Double, blurRadius: Double)
  case removeShadow
  case setTransformation(Transformation)
  case concatTransformation(Transformation)
  case undoTransformation(Transformation)
  case stroke(Shape, width: Double)
  case strokeDashed(Shape, width: Double, lengths: [Double], phase: Double)
  case fill(Shape)
  case fillLinearGradient(Shape, [Color], angle: Double)
  case fillRadialGradient(Shape, [Color], relativeCenter: NSPoint)
  case text(String, font: NSFont?, color: Color?, style: NSParagraphStyle?, at: ObjectLocation)
  case attributedText(NSAttributedString, at: ObjectLocation)
  case image(NSImage, ObjectLocation, operation: NSCompositingOperation, opacity: Double)
  case inline(Drawing)
  case include(Drawing, clippedTo: Shape?)
  
  fileprivate func draw() {
    switch self {
      case .setStrokeColor(let color):
        color.nsColor.setStroke()
      case .setFillColor(let color):
        color.nsColor.setFill()
      case .setBlendMode(let blendMode):
        NSGraphicsContext.current?.cgContext.setBlendMode(blendMode)
      case .setShadow(let color, let dx, let dy, let blurRadius):
        let shadow = NSShadow()
        shadow.shadowOffset = NSSize(width: dx, height: dy)
        shadow.shadowBlurRadius = CGFloat(blurRadius)
        shadow.shadowColor = color.nsColor
        shadow.set()
      case .removeShadow:
        let shadow = NSShadow()
        shadow.shadowOffset = NSSize(width: 0.0, height: 0.0)
        shadow.shadowBlurRadius = 0.0
        shadow.shadowColor = nil
        shadow.set()
      case .setTransformation(let transformation):
        NSAffineTransform(transform: transformation.affineTransform).set()
      case .concatTransformation(let transformation):
        NSAffineTransform(transform: transformation.affineTransform).concat()
      case .undoTransformation(let transformation):
        let transform = NSAffineTransform(transform: transformation.affineTransform)
        transform.invert()
        transform.concat()
      case .stroke(let shape, let width):
        shape.stroke(lineWidth: width)
      case .strokeDashed(let shape, let width, let dashLengths, let dashPhase):
        shape.stroke(lineWidth: width, lineDashPhase: dashPhase, lineDashLengths: dashLengths)
      case .fill(let shape):
        shape.fill()
      case .fillLinearGradient(let shape, let colors, let angle):
        NSGradient(colors: Color.nsColorArray(colors))?.draw(in: shape.compile(), angle: CGFloat(angle * 180.0 / .pi))
      case .fillRadialGradient(let shape, let colors, let center):
        NSGradient(colors: Color.nsColorArray(colors))?.draw(in: shape.compile(), relativeCenterPosition: center)
      case .text(let str, let font, let color, let paragraphStyle, let location):
        let pstyle: NSParagraphStyle
        if let style = paragraphStyle {
          pstyle = style
        } else {
          let style = NSMutableParagraphStyle()
          style.alignment = .left
          pstyle = style
        }
        let attributes = [
          .font: font ?? NSFont.systemFont(ofSize: NSFont.systemFontSize),
          .foregroundColor: (color ?? Color.black).nsColor,
          .paragraphStyle: pstyle,
        ] as [NSAttributedStringKey: Any]
        let textRect: NSRect
        switch location {
          case .position(let point):
            textRect = NSRect(x: point.x, y: point.y,
                              width: CGFloat.infinity, height: CGFloat.infinity)
          case .boundingBox(let box):
            textRect = box
        }
        str.draw(in: textRect, withAttributes: attributes)
      case .attributedText(let attribStr, let location):
        switch location {
          case .position(let point):
            attribStr.draw(at: point)
          case .boundingBox(let box):
            attribStr.draw(in: box)
        }
      case .image(let image, let location, let oper, let opacity):
        switch location {
          case .position(let point):
           image.draw(in: NSRect(x: point.x,
                                 y: point.y,
                                 width: image.alignmentRect.width,
                                 height: image.alignmentRect.height),
                      from: NSZeroRect,
                      operation: oper,
                      fraction: CGFloat(opacity),
                      respectFlipped: true,
                      hints: [:])
          case .boundingBox(let box):
            image.draw(in: box,
                       from: NSZeroRect,
                       operation: oper,
                       fraction: CGFloat(opacity),
                       respectFlipped: true,
                       hints: [:])
        }
      case .include(let drawing, let clippingRegion):
        drawing.draw(clippedTo: clippingRegion)
      case .inline(let drawing):
        drawing.drawInline()
    }
  }
  
  func markDirty() {
    switch self {
      case .stroke(let shape, _):
        shape.markDirty()
      case .strokeDashed(let shape, _, _, _):
        shape.markDirty()
      case .fill(let shape):
        shape.markDirty()
      case .fillLinearGradient(let shape, _, _):
        shape.markDirty()
      case .fillRadialGradient(let shape, _, _):
        shape.markDirty()
      default:
        break
    }
  }
}

///
/// Enumeration of all supported blend modes.
///
public typealias BlendMode = CGBlendMode

///
/// Enumeration of all supported object locations.
///
public enum ObjectLocation {
  case position(NSPoint)
  case boundingBox(NSRect)
}
