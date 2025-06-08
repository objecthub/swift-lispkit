//
//  Drawing_iOS.swift
//  LispKit
//
//  Created by Matthias Zenger on 23/04/2021.
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

import CoreGraphics
import Foundation
import UIKit
import PDFKit
import MobileCoreServices
import UniformTypeIdentifiers

///
/// Class `Drawing` represents a sequence of drawing instructions. The class offers the
/// following functionality:
///   - New instructions can be appended to a drawing
///   - The drawing can be drawn to the current graphics context
///   - The drawing can be written to a file. Natively supported are PDF, PNG, and JPEG.
///
public final class Drawing: NativeObject, CustomStringConvertible, @unchecked Sendable {
  
  /// Type representing drawings.
  public static let type = Type.objectType(Symbol(uninterned: "drawing"))
  
  /// The sequence of drawing instructions.
  public private(set) var instructions: [DrawingInstruction]
  
  /// Initializer copying another drawing.
  public init(copy drawing: Drawing) {
    self.instructions = drawing.instructions
  }
  
  /// Initializer providing an initial sequence of drawing instructions.
  public init(_ instructions: DrawingInstruction...) {
    self.instructions = instructions
  }

  /// Return native object type.
  public override var type: Type {
    return Self.type
  }
  
  /// Return the number of drawing instructions
  public var numInstructions: Int {
    return self.instructions.count
  }
  
  /// Appends a new drawing instruction.
  @discardableResult public func append(_ instruction: DrawingInstruction) -> Bool {
    switch instruction {
      // Do not allow recursive dependencies
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
  
  /// Clears all drawing instructions
  public func clear() {
    self.instructions.removeAll()
  }
  
  /// Draws the drawing to the current graphics context clipped to a given shape. The drawing is
  /// put into a new transparency layer. Upon exit, the previous graphics state is being
  /// restored.
  public func draw(clippedTo shape: Shape? = nil, in context: CGContext? = nil) {
    if let context = context ?? UIGraphicsGetCurrentContext() {
      context.saveGState()
      context.beginTransparencyLayer(auxiliaryInfo: nil)
      shape?.compile().addClip()
      defer {
        context.endTransparencyLayer()
        context.restoreGState()
      }
      self.drawInline(in: context)
    }
  }
  
  /// Draw the drawing into the current graphics context without saving and restoring the
  /// graphics state.
  public func drawInline(in context: CGContext) {
    for instruction in self.instructions {
      instruction.draw(in: context)
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
    // First check if we can write to the URL
    var dir: ObjCBool = false
    let parent = url.deletingLastPathComponent().path
    guard FileManager.default.fileExists(atPath: parent, isDirectory: &dir) && dir.boolValue else {
      return false
    }
    guard FileManager.default.isWritableFile(atPath: parent) else {
      return false
    }
    // Define a media box
    var mediaBox = CGRect(x: 0, y: 0, width: Double(width), height: Double(height))
    // Create a core graphics context suitable for drawing the image into a PDF file
    let pdfInfo = NSMutableDictionary()
    if let title = title {
      pdfInfo[kCGPDFContextTitle] = title
    }
    if let author = author {
      pdfInfo[kCGPDFContextAuthor] = author
    }
    if let creator = creator {
      pdfInfo[kCGPDFContextCreator] = creator
    }
    guard let context = CGContext(url as CFURL, mediaBox: &mediaBox, pdfInfo as CFDictionary) else {
      return false
    }
    UIGraphicsPushContext(context)
    defer {
      UIGraphicsPopContext()
    }
    // Create a new PDF page
    context.beginPDFPage(nil)
    context.saveGState()
    // Flip graphics if required
    if flipped {
      context.translateBy(x: 0.0, y: CGFloat(height))
      context.scaleBy(x: 1.0, y: -1.0)
    }
    // Draw the image
    self.draw()
    context.restoreGState()
    // Close PDF page and document
    context.endPDFPage()
    context.closePDF()
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
                             format: .png,
                             qualityFactor: nil,
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
                        qualityFactor: Double = 0.8,
                        flipped: Bool = false) -> Bool {
    return self.saveAsBitmap(url: url,
                             width: width,
                             height: height,
                             scale: scale,
                             format: .jpeg,
                             qualityFactor: qualityFactor,
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
                           format: BitmapImageFileType,
                           qualityFactor: Double?,
                           flipped: Bool) -> Bool {
    // Create a bitmap suitable for storing the image in a PNG
    guard let context = CGContext(data: nil,
                                  width: Int(Double(width) * scale),
                                  height: Int(Double(height) * scale),
                                  bitsPerComponent: 8,
                                  bytesPerRow: 0,
                                  space: Color.colorSpaceName,
                                  bitmapInfo: CGBitmapInfo(rawValue:
                                                CGImageAlphaInfo.premultipliedFirst.rawValue)
                                              .union(.byteOrder32Little).rawValue) else {
      return false
    }
    // Create a flipped graphics context if required
    if flipped {
      context.translateBy(x: 0.0, y: CGFloat(height))
      context.scaleBy(x: 1.0, y: -1.0)
    }
    UIGraphicsPushContext(context)
    defer {
      UIGraphicsPopContext()
    }
    // Draw the image
    self.draw()
    // Encode bitmap
    guard let image = UIGraphicsGetImageFromCurrentImageContext(),
          let data = format.data(for: image, qualityFactor: qualityFactor) else {
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
  
  public var description: String {
    var res = "drawing with \(self.instructions.count) instructions"
    var i = 0
    for instruction in self.instructions {
      res += "\n  \(i): \(instruction)"
      i += 1
    }
    return res
  }
}

///
/// Enumeration of all supported drawing instructions.
///
public enum DrawingInstruction: CustomStringConvertible, @unchecked Sendable {
  case setStrokeColor(Color)
  case setFillColor(Color)
  case setStrokeWidth(Double)
  case setBlendMode(BlendMode)
  case setShadow(Color, dx: Double, dy: Double, blurRadius: Double)
  case removeShadow
  case setTransformation(Transformation)
  case concatTransformation(Transformation)
  case undoTransformation(Transformation)
  case strokeLine(CGPoint, CGPoint)
  case strokeRect(CGRect)
  case fillRect(CGRect)
  case strokeEllipse(CGRect)
  case fillEllipse(CGRect)
  case stroke(Shape, width: Double)
  case strokeDashed(Shape, width: Double, lengths: [Double], phase: Double)
  case fill(Shape)
  case fillLinearGradient(Shape, [Color], angle: Double)
  case fillRadialGradient(Shape, [Color], relativeCenter: CGPoint)
  case text(String, font: Font?, color: Color?, style: NSParagraphStyle?, at: ObjectLocation)
  case attributedText(NSAttributedString, at: ObjectLocation)
  case image(UIImage, ObjectLocation, operation: CGBlendMode, opacity: Double)
  case page(PDFPage, PDFDisplayBox, CGRect)
  case annotation(PDFAnnotation, PDFDisplayBox, CGRect)
  case inline(Drawing)
  case include(Drawing, clippedTo: Shape?)
  
  /// Draw the instruction if there is a valid current drawing context
  public func draw() {
    if let context = UIGraphicsGetCurrentContext() {
      self.draw(in: context)
    }
  }
  
  /// Draw the instruction into the given drawing context
  public func draw(in context: CGContext) {
    switch self {
      case .setStrokeColor(let color):
        context.setStrokeColor(color.nsColor.cgColor)
      case .setFillColor(let color):
        context.setFillColor(color.nsColor.cgColor)
      case .setStrokeWidth(let width):
        context.setLineWidth(CGFloat(width))
      case .setBlendMode(let blendMode):
        context.setBlendMode(blendMode)
      case .setShadow(let color, let dx, let dy, let blurRadius):
        context.setShadow(offset: CGSize(width: dx, height: dy),
                          blur: CGFloat(blurRadius),
                          color: color.nsColor.cgColor)
      case .removeShadow:
        context.setShadow(offset: CGSize(width: 0.0, height: 0.0), blur: 0.0, color: nil)
      case .setTransformation(let transformation):
        let transform = context.ctm
        context.concatenate(transform.inverted())
        context.concatenate(transformation.affineTransform)
      case .concatTransformation(let transformation):
        context.concatenate(transformation.affineTransform)
      case .undoTransformation(let transformation):
        context.concatenate(transformation.affineTransform.inverted())
      case .strokeLine(let start, let end):
        context.beginPath()
        context.move(to: start)
        context.addLine(to: end)
        context.strokePath()
      case .strokeRect(let rct):
        context.stroke(rct)
      case .fillRect(let rct):
        context.fill(rct)
      case .strokeEllipse(let rct):
        context.strokeEllipse(in: rct)
      case .fillEllipse(let rct):
        context.fillEllipse(in: rct)
      case .stroke(let shape, let width):
        shape.stroke(lineWidth: width)
      case .strokeDashed(let shape, let width, let dashLengths, let dashPhase):
        shape.stroke(lineWidth: width, lineDashPhase: dashPhase, lineDashLengths: dashLengths)
      case .fill(let shape):
        shape.fill()
      case .fillLinearGradient(let shape, let colors, let angle):
        if let gradient = CGGradient(colorsSpace: Color.colorSpaceName,
                                     colors: Color.cgColorArray(colors) as CFArray,
                                     locations: nil) {
          context.saveGState()
          context.addPath(shape.compile().cgPath)
          context.closePath()
          context.clip()
          context.drawLinearGradient(gradient,
                                     start: .zero,
                                     end: CGPoint(x: cos(angle), y: sin(angle)),
                                     options: [])
          context.restoreGState()
        }
      case .fillRadialGradient(let shape, let colors, let center):
        if let gradient = CGGradient(colorsSpace: Color.colorSpaceName,
                                     colors: Color.cgColorArray(colors) as CFArray,
                                     locations: nil) {
          context.saveGState()
          let path = shape.compile()
          context.addPath(path.cgPath)
          context.closePath()
          context.clip()
          let bounds = path.bounds
          var radius = abs(center.distance(to: CGPoint(x: bounds.minX, y: bounds.minY)))
          var r = abs(center.distance(to: CGPoint(x: bounds.minX, y: bounds.maxY)))
          if r > radius {
            radius = r
          }
          r = abs(center.distance(to: CGPoint(x: bounds.maxX, y: bounds.minY)))
          if r > radius {
            radius = r
          }
          r = abs(center.distance(to: CGPoint(x: bounds.maxX, y: bounds.maxY)))
          if r > radius {
            radius = r
          }
          context.drawRadialGradient(gradient,
                                     startCenter: center,
                                     startRadius: 0,
                                     endCenter: center,
                                     endRadius: radius,
                                     options: [])
          context.restoreGState()
        }
      case .text(let str, let font, let color, let paragraphStyle, let location):
        let pstyle: NSParagraphStyle
        if let style = paragraphStyle {
          pstyle = style
        } else {
          // let style = NSMutableParagraphStyle()
          // style.alignment = .left
          // pstyle = style
          pstyle = .default
        }
        let attributes = [
          .font: font ?? Font.systemFont(ofSize: Font.systemFontSize),
          .foregroundColor: (color ?? Color.black).nsColor,
          .paragraphStyle: pstyle,
          ] as [NSAttributedString.Key: Any]
        let textRect: CGRect
        switch location {
          case .position(let point):
            textRect = CGRect(x: point.x, y: point.y,
                              width: CGFloat.infinity, height: CGFloat.infinity)
          case .boundingBox(let box):
            textRect = box
        }
        if let current = UIGraphicsGetCurrentContext(), current == context {
          context.saveGState()
          str.draw(with: textRect,
                   options: [.usesLineFragmentOrigin, .usesFontLeading],
                   attributes: attributes,
                   context: nil)
          context.restoreGState()
        } else {
          UIGraphicsPushContext(context)
          context.saveGState()
          str.draw(with: textRect,
                   options: [.usesLineFragmentOrigin, .usesFontLeading],
                   attributes: attributes,
                   context: nil)
          context.restoreGState()
          UIGraphicsPopContext()
        }
      case .attributedText(let attribStr, let location):
        let textRect: CGRect
        switch location {
          case .position(let point):
            textRect = CGRect(x: point.x, y: point.y,
                              width: CGFloat.infinity, height: CGFloat.infinity)
          case .boundingBox(let box):
            textRect = box
        }
        if let current = UIGraphicsGetCurrentContext(), current == context {
          attribStr.draw(with: textRect,
                         options: [.usesLineFragmentOrigin, .usesFontLeading],
                         context: nil)
        } else {
          UIGraphicsPushContext(context)
          attribStr.draw(with: textRect,
                         options: [.usesLineFragmentOrigin, .usesFontLeading],
                         context: nil)
          UIGraphicsPopContext()
        }
      case .image(let image, let location, let oper, let opa):
        if let current = UIGraphicsGetCurrentContext(), current == context {
          switch location {
            case .position(let point):
              image.draw(at: CGPoint(x: point.x, y: point.y), blendMode: oper, alpha: CGFloat(opa))
            case .boundingBox(let box):
              image.draw(in: box, blendMode: oper, alpha: CGFloat(opa))
          }
        } else {
          UIGraphicsPushContext(context)
          switch location {
            case .position(let point):
              image.draw(at: CGPoint(x: point.x, y: point.y), blendMode: oper, alpha: CGFloat(opa))
            case .boundingBox(let box):
              image.draw(in: box, blendMode: oper, alpha: CGFloat(opa))
          }
          UIGraphicsPopContext()
        }
      case .page(let page, let box, let rect):
        context.saveGState()
        // PDF coordinate system is Y-flipped from Core Graphics
        context.translateBy(x: rect.origin.x, y: rect.origin.y + rect.height)
        // Apply the PDF's crop box transform
        let bounds = page.bounds(for: box)
        page.transform(context, for: box)
        // Scale PDF to view size
        context.scaleBy(x: rect.width / bounds.width, y: -rect.height / bounds.height)
        // Draw
        page.draw(with: box, to: context)
        context.restoreGState()
      case .annotation(let annotation, let box, let rect):
        if let page = annotation.page {
          context.saveGState()
          // PDF coordinate system is Y-flipped from Core Graphics
          context.translateBy(x: rect.origin.x, y: rect.origin.y + rect.height)
          // Apply the PDF's crop box transform
          let bounds = page.bounds(for: box)
          page.transform(context, for: box)
          // Scale PDF to view size
          context.scaleBy(x: rect.width / bounds.width, y: -rect.height / bounds.height)
          // Draw
          annotation.draw(with: box, in: context)
          context.restoreGState()
        }
      case .include(let drawing, let clippingRegion):
        drawing.draw(clippedTo: clippingRegion, in: context)
      case .inline(let drawing):
        drawing.drawInline(in: context)
    }
  }
  
  public var description: String {
    switch self {
      case .setStrokeColor(_):
        return "setStrokeColor"
      case .setFillColor(_):
        return "setFillColor"
      case .setStrokeWidth(_):
        return "setStrokeWidth"
      case .setBlendMode(_):
        return "setBlendMode"
      case .setShadow(_, dx: _, dy: _, blurRadius: _):
        return "setShadow"
      case .removeShadow:
        return "setShadow"
      case .setTransformation(_):
        return "setTransformation"
      case .concatTransformation(_):
        return "concatTransformation"
      case .undoTransformation(_):
        return "undoTransformation"
      case .strokeLine(_, _):
        return "strokeLine"
      case .strokeRect(_):
        return "strokeRect"
      case .fillRect(_):
        return "fillRect"
      case .strokeEllipse(_):
        return "strokeEllipse"
      case .fillEllipse(_):
        return "fillEllipse"
      case .stroke(_, width: _):
        return "stroke"
      case .strokeDashed(_, width: _, lengths: _, phase: _):
        return "strokeDashed"
      case .fill(_):
        return "fill"
      case .fillLinearGradient(_, _, angle: _):
        return "fillLinearGradient"
      case .fillRadialGradient(_, _, relativeCenter: _):
        return "fillRadialGradient"
      case .text(_, font: _, color: _, style: _, at: _):
        return "text"
      case .attributedText(_, at: _):
        return "attributedText"
      case .image(_, _, operation: _, opacity: _):
        return "image"
      case .page(_, _, _):
        return "page"
      case .annotation(_, _, _):
        return "annotation"
      case .inline(_):
        return "inline"
      case .include(_, clippedTo: _):
        return "include"
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
  case position(CGPoint)
  case boundingBox(CGRect)
}

public enum BitmapImageFileType {
  case tiff
  case bmp
  case gif
  case jpeg
  case png
  
  // qualityFactor = 0: maximum compression
  // qualityFactor = 1: no compression
  public func data(for image: UIImage, qualityFactor: Double?) -> Data? {
    switch self {
      case .tiff:
        guard let cgImage = image.cgImage else {
          return nil
        }
        let tiffOptions: Dictionary<String, Int> = [
          kCGImagePropertyTIFFCompression as String: 4
        ]
        let options: NSMutableDictionary = [
          kCGImagePropertyTIFFDictionary as String : tiffOptions,
          // kCGImagePropertyDepth as String : 1,
          kCGImagePropertyDPIWidth as String : Int(CGFloat(cgImage.width)*72.0/image.size.width),
          kCGImagePropertyDPIHeight as String : Int(CGFloat(cgImage.height)*72.0/image.size.height),
          kCGImagePropertyColorModel as String : kCGImagePropertyColorModelRGB as String,
          kCGImagePropertyOrientation as String : NSNumber(value: image.imageOrientation.rawValue)
        ]
        if let qualityFactor {
          options[kCGImageDestinationLossyCompressionQuality as String] = qualityFactor as NSNumber
        }
        let md = NSMutableData()
        guard let dest =
            CGImageDestinationCreateWithData(md, UTType.tiff.identifier as CFString, 1, nil) else {
          return nil
        }
        CGImageDestinationAddImage(dest, cgImage, options)
        CGImageDestinationFinalize(dest)
        return md as Data
      case .bmp:
        guard let cgImage = image.cgImage else {
          return nil
        }
        let options: NSMutableDictionary = [
          kCGImagePropertyDPIWidth as String : Int(CGFloat(cgImage.width)*72.0/image.size.width),
          kCGImagePropertyDPIHeight as String : Int(CGFloat(cgImage.height)*72.0/image.size.height),
          kCGImagePropertyColorModel as String : kCGImagePropertyColorModelRGB as String,
          kCGImagePropertyHasAlpha as String: NSNumber(value: true),
          kCGImagePropertyOrientation as String : NSNumber(value: image.imageOrientation.rawValue)
        ]
        if let qualityFactor {
          options[kCGImageDestinationLossyCompressionQuality as String] = qualityFactor as NSNumber
        }
        let md = NSMutableData()
        guard let dest =
            CGImageDestinationCreateWithData(md, UTType.bmp.identifier as CFString, 1, nil) else {
          return nil
        }
        CGImageDestinationAddImage(dest, cgImage, options)
        CGImageDestinationFinalize(dest)
        return md as Data
      case .gif:
        guard let cgImage = image.cgImage else {
          return nil
        }
        let options: NSMutableDictionary = [
          kCGImagePropertyDPIWidth as String : Int(CGFloat(cgImage.width)*72.0/image.size.width),
          kCGImagePropertyDPIHeight as String : Int(CGFloat(cgImage.height)*72.0/image.size.height),
          kCGImagePropertyColorModel as String : kCGImagePropertyColorModelRGB as String,
          kCGImagePropertyOrientation as String : NSNumber(value: image.imageOrientation.rawValue)
        ]
        if let qualityFactor {
          options[kCGImageDestinationLossyCompressionQuality as String] = qualityFactor as NSNumber
        }
        let md = NSMutableData()
        guard let dest =
            CGImageDestinationCreateWithData(md, UTType.gif.identifier as CFString, 1, nil) else {
          return nil
        }
        CGImageDestinationAddImage(dest, cgImage, options)
        CGImageDestinationFinalize(dest)
        return md as Data
      case .jpeg:
        return image.jpegData(compressionQuality: qualityFactor ?? 0.8)
      case .png:
        return image.pngData()
    }
  }
}

extension CGPoint {
  func distance(to point: CGPoint) -> CGFloat {
    return sqrt(pow((point.x - x), 2) + pow((point.y - y), 2))
  }
}
