//
//  ImageLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 08/01/2025.
//  Copyright © 2025 ObjectHub. All rights reserved.
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
import CoreImage
#if os(iOS) || os(watchOS) || os(tvOS)
import UIKit
#elseif os(macOS)
import AppKit
#endif

public final class ImageLibrary: NativeLibrary {
  
  /// Filter categories
  private let filterCategories: SymbolTable.Enumeration<String>
  
  /// Filter argument attributes
  private let argumentAttributes: SymbolTable.Enumeration<String>
  
  /// Filter argument types
  private let argumentTypes: SymbolTable.Enumeration<String>
  
  /// Canonical symbols
  private let rgbColorSpace: Symbol
  private let deviceRgbColorSpace: CGColorSpace
  
  /// Filter identifier/name mapping
  private let filterName: [Symbol : String]
  private let filterIdentifier: [String: Symbol]
  
  private let ciContext: CIContext
  
  /// Initialize symbols
  public required init(in context: Context) throws {
    self.deviceRgbColorSpace = CGColorSpaceCreateDeviceRGB()
    self.rgbColorSpace = context.symbols.intern("rgb-color-space")
    self.filterCategories = context.symbols.Enum("image filter category") {
      ("distortion-effect", kCICategoryDistortionEffect)
      ("geometry-adjustment", kCICategoryGeometryAdjustment)
      ("composite-operation", kCICategoryCompositeOperation)
      ("halftone-effect", kCICategoryHalftoneEffect)
      ("color-adjustment", kCICategoryColorAdjustment)
      ("color-effect", kCICategoryColorEffect)
      ("transition", kCICategoryTransition)
      ("tile-effect", kCICategoryTileEffect)
      ("generator", kCICategoryGenerator)
      ("reduction", kCICategoryReduction)
      ("gradient", kCICategoryGradient)
      ("stylize", kCICategoryStylize)
      ("sharpen", kCICategorySharpen)
      ("blur", kCICategoryBlur)
      ("video", kCICategoryVideo)
      ("still-image", kCICategoryStillImage)
      ("interlaced", kCICategoryInterlaced)
      ("non-square-pixels", kCICategoryNonSquarePixels)
      ("high-dynamic-range", kCICategoryHighDynamicRange)
      ("builtin", kCICategoryBuiltIn)
      ("filter-generator", kCICategoryFilterGenerator)
    }
    self.argumentAttributes = context.symbols.Enum("image filter argument key") {
      ("description", kCIAttributeDescription)
      ("reference-documentation", kCIAttributeReferenceDocumentation)
      ("class", kCIAttributeClass)
      ("type", kCIAttributeType)
      ("min", kCIAttributeMin)
      ("max", kCIAttributeMax)
      ("slider-min", kCIAttributeSliderMin)
      ("slider-max", kCIAttributeSliderMax)
      ("default", kCIAttributeDefault)
      ("identity", kCIAttributeIdentity)
      ("name", kCIAttributeName)
      ("display-name", kCIAttributeDisplayName)
    }
    self.argumentTypes = context.symbols.Enum("image filter argument type") {
      ("time", kCIAttributeTypeTime)
      ("scalar", kCIAttributeTypeScalar)
      ("distance", kCIAttributeTypeDistance)
      ("angle", kCIAttributeTypeAngle)
      ("boolean", kCIAttributeTypeBoolean)
      ("integer", kCIAttributeTypeInteger)
      ("count", kCIAttributeTypeCount)
      ("color", kCIAttributeTypeColor)
      ("opaque-color", kCIAttributeTypeOpaqueColor)
      ("gradient", kCIAttributeTypeGradient)
      ("point", kCIAttributeTypePosition)
      ("offset", kCIAttributeTypeOffset)
      ("coordinate-3d", kCIAttributeTypePosition3)
      ("rect", kCIAttributeTypeRectangle)
      ("abstract-image", kCIAttributeTypeImage)
      ("transformation", kCIAttributeTypeTransform)
      ("date", "CIAttributeTypeDate")
      ("styled-text", "CIAttributeTypeAttributedString")
      ("string", "CIAttributeTypeString")
      ("number", "CIAttributeTypeNumber")
      ("bytevector", "CIAttributeTypeData")
      ("image-coefficients", "CIAttributeTypeCIVector")
      ("array", "CIAttributeTypeArray")
      ("color-space", "CIAttributeTypeColorSpace")
    }
    var filterName: [Symbol : String] = [:]
    var filterIdentifier: [String : Symbol] = [:]
    func enter(name: String, identifier: String) {
      let sym = context.symbols.intern(identifier)
      filterName[sym] = name
      filterIdentifier[name] = sym
    }
    for name in CIFilter.filterNames(inCategories: nil) {
      switch name {
        case "CIConvolutionRGB7X7":
          enter(name: name, identifier: "convolution-rgb-7x7")
        case "CIConvolutionRGB5X5":
          enter(name: name, identifier: "convolution-rgb-5x5")
        case "CIConvolutionRGB3X3":
          enter(name: name, identifier: "convolution-rgb-3x3")
        case "CIConvolution7X7":
          enter(name: name, identifier: "convolution-7x7")
        case "CIConvolution5X5":
          enter(name: name, identifier: "convolution-5x5")
        case "CIConvolution3X3":
          enter(name: name, identifier: "convolution-3x3")
        case "CIConvertRGBtoLab":
          enter(name: name, identifier: "convert-rgb-to-lab")
        case "CICMYKHalftone":
          enter(name: name, identifier: "cmyk-halftone")
        default:
          if let identifier = Self.externalName(name, prefix: ("CI", "")) {
            enter(name: name, identifier: identifier)
          }
      }
    }
    self.filterName = filterName
    self.filterIdentifier = filterIdentifier
    self.ciContext = CIContext(options: [:])
    try super.init(in: context)
  }
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "image"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("available-image-filter-categories",
                          self.availableImageFilterCategories))
    self.define(Procedure("image-filter-category", self.imageFilterCategory))
    self.define(Procedure("available-image-filter-implementations",
                          self.availableImageFilterImplementations))
    self.define(Procedure("abstract-image?", self.isAbstractImage))
    self.define(Procedure("make-abstract-image", self.makeAbstractImage))
    self.define(Procedure("image->abstract-image", self.imageToAbstractImage))
    self.define(Procedure("color->abstract-image", self.colorToAbstractImage))
    self.define(Procedure("abstract-image->image", self.abstractImageToImage))
    self.define(Procedure("abstract-image-bounds", self.abstractImageBounds))
    self.define(Procedure("abstract-image-adjustment-filters", self.abstractImageAdjustmentFilters))
    self.define(Procedure("image-filter?", self.isImageFilter))
    self.define(Procedure("make-image-filter", self.makeImageFilter))
    self.define(Procedure("image-filter-name", self.imageFilterName))
    self.define(Procedure("image-filter-implementation", self.imageFilterImplementation))
    self.define(Procedure("image-filter-categories", self.imageFilterCategories))
    self.define(Procedure("image-filter-available", self.imageFilterAvailable))
    self.define(Procedure("image-filter-inputs", self.imageFilterInputs))
    self.define(Procedure("image-filter-outputs", self.imageFilterOutputs))
    self.define(Procedure("image-filter-output", self.imageFilterOutput))
    self.define(Procedure("image-filter-argument", self.imageFilterArgument))
    self.define(Procedure("image-filter-argument-ref", self.imageFilterArgumentRef))
    self.define(Procedure("image-filter-argument-set!", self.imageFilterArgumentSet))
    self.define(Procedure("image-coefficients", self.imageCoefficients))
    self.define(Procedure("image-coefficients->vector", self.imageCoefficientsToVector))
    self.define(Procedure("image-coefficients->point", self.imageCoefficientsToPoint))
    self.define(Procedure("image-coefficients->rect", self.imageCoefficientsToRect))
    self.define(Procedure("apply-image-filter", self.applyImageFilter))
    self.define(Procedure("map-image-point", self.mapImagePoint))
    self.define(Procedure("map-image-rect", self.mapImageRect))
  }
  
  public static func internalName(_ name: String, prefix: String?) -> String {
    guard let prefix else {
      let comp = name.split(separator: "-", omittingEmptySubsequences: true)
      guard !comp.isEmpty else {
        return ""
      }
      return comp[0] + comp[1...].map { str in str.capitalized }.joined()
    }
    return prefix +
           name.split(separator: "-", omittingEmptySubsequences: true).map { str in
             str.capitalized
           }.joined()
  }
  
  public static func externalName(_ name: String, prefix: (String, String)? = nil) -> String? {
    if prefix == nil || name.hasPrefix(prefix!.0) {
      var res = prefix == nil ? "" : prefix!.1
      var j = prefix == nil ? name.startIndex
                                   : name.index(name.startIndex, offsetBy: prefix!.0.count)
      guard prefix == nil || prefix!.0.isEmpty || name[j].isUppercase else {
        return nil
      }
      while j < name.endIndex {
        if prefix == nil {
          res += name[j].lowercased()
          j = name.index(after: j)
        } else {
          while j < name.endIndex && name[j].isUppercase {
            res += name[j].lowercased()
            j = name.index(after: j)
          }
        }
        if j < name.endIndex {
          var i = j
          while i < name.endIndex && !name[i].isUppercase && name[i] != "_" {
            i = name.index(after: i)
          }
          res += name[j..<i]
          j = i
          if j < name.endIndex {
            if name[j] == "_" {
              return res + name[j...].lowercased()
            }
            res += "-"
          }
        }
      }
      if !res.isEmpty {
        return res
      }
    }
    return nil
  }
  
  private func name(from: Expr, prefix: String? = nil) throws -> String {
    if case .symbol(let sym) = from {
      return Self.internalName(sym.identifier, prefix: prefix)
    } else {
      return try from.asString()
    }
  }
  
  private func abstractImage(from: Expr) throws -> AbstractImage {
    guard case .object(let obj) = from,
          let abstractImage = obj as? AbstractImage else {
      throw RuntimeError.type(from, expected: [AbstractImage.type])
    }
    return abstractImage
  }
  
  private func imageFilter(from: Expr) throws -> ImageFilter {
    guard case .object(let obj) = from,
          let imageFilter = obj as? ImageFilter else {
      throw RuntimeError.type(from, expected: [ImageFilter.type])
    }
    return imageFilter
  }
  
  private func imageCoefficients(from: Expr) throws -> ImageCoefficients {
    guard case .object(let obj) = from,
          let coefficients = obj as? ImageCoefficients else {
      throw RuntimeError.type(from, expected: [ImageCoefficients.type])
    }
    return coefficients
  }
  
  private func availableImageFilterCategories(raw: Expr?) throws -> Expr {
    if raw?.isTrue ?? false {
      var res = Expr.null
      for category in self.filterCategories.valueList() {
        res = .pair(.makeString(category), res)
      }
      return res
    } else {
      return self.filterCategories.symbolList()
    }
  }
  
  private func imageFilterCategory(expr: Expr, raw: Expr?) throws -> Expr {
    let raw = raw?.isTrue ?? false
    switch expr {
      case .string(let str):
        if raw {
          return self.filterCategories.isKey(str as String) ? expr : .false
        } else if self.filterCategories.isKey(str as String) {
          return self.filterCategories.expr(for: str as String)
        } else {
          return .false
        }
      case .symbol(_):
        if !raw {
          return self.filterCategories.isValue(expr) ? expr : .false
        } else if self.filterCategories.isValue(expr) {
          return .makeString(try self.filterCategories.value(for: expr))
        } else {
          return .false
        }
      default:
        return .false
    }
  }
  
  private func isImageFilterCategory(obj: Expr) throws -> Expr {
    if case .string(let str) = obj {
      return .makeBoolean(self.filterCategories.isKey(str as String))
    } else {
      return .makeBoolean(self.filterCategories.isValue(obj))
    }
  }
  
  private func availableImageFilterImplementations(categories: Expr?, raw: Expr?) throws -> Expr {
    let filterNames: [String]
    if var categories, categories.isTrue {
      var categoryNames: [String] = []
      while case .pair(let name, let rest) = categories {
        categoryNames.append(try self.filterCategories.value(for: name))
        categories = rest
      }
      filterNames = CIFilter.filterNames(inCategories: categoryNames)
    } else {
      filterNames = CIFilter.filterNames(inCategories: nil)
    }
    var res = Expr.null
    for filterName in filterNames.reversed() {
      if raw?.isFalse ?? true, let identifier = self.filterIdentifier[filterName] {
        res = .pair(.symbol(identifier), res)
      } else {
        res = .pair(.makeString(filterName), res)
      }
    }
    return res
  }
  
  private func isAbstractImage(expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is AbstractImage else {
      return .false
    }
    return .true
  }
  
  private func makeAbstractImage(expr: Expr?, flip: Expr?) throws -> Expr {
    if let expr {
      let res: AbstractImage?
      switch expr {
        case .false:
          res = AbstractImage(ciImage: .empty())
        case .string(_):
          res = AbstractImage(
                  url: URL(fileURLWithPath:
                            self.context.fileHandler.path(
                              try expr.asPath(),
                              relativeTo: self.context.evaluator.currentDirectoryPath)),
                  flipped: flip?.isTrue ?? false)
        case .bytes(let bvec):
          res = AbstractImage(data: Data(bvec.value), flipped: flip?.isTrue ?? false)
        case .object(let obj):
          if let imageBox = obj as? NativeImage {
            res = AbstractImage(image: imageBox, flipped: flip?.isTrue ?? false)
          } else if let aimg = obj as? AbstractImage {
            res = aimg
          } else {
            return .false
          }
        default:
          return .false
      }
      guard let res else {
        return .false
      }
      return .object(res)
    } else {
      return .object(AbstractImage(ciImage: .empty()))
    }
  }
  
  private func imageToAbstractImage(expr: Expr, flip: Expr?) throws -> Expr {
    guard case .object(let obj) = expr,
          let imageBox = obj as? NativeImage,
          let abstractImage = AbstractImage(image: imageBox, flipped: flip?.isTrue ?? false) else {
      throw RuntimeError.type(expr, expected: [NativeImage.type])
    }
    return .object(abstractImage)
  }
  
  private func colorToAbstractImage(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let nc = obj as? Color else {
      throw RuntimeError.type(expr, expected: [Color.type])
    }
    #if os(iOS) || os(watchOS) || os(tvOS)
    return .object(ImagePipe(ciImage: CIImage(color: CIColor(color: nc.nsColor))))
    #elseif os(macOS)
    return .object(AbstractImage(ciImage:
              CIImage(color: CIColor(color: nc.nsColor) ??
                             CIColor(red: nc.red, green: nc.green, blue: nc.blue, alpha: nc.alpha))))
    #endif
  }
  
  private func abstractImageToImage(expr: Expr, ppi: Expr?, flip: Expr?) throws -> Expr {
    let aimg = try self.abstractImage(from: expr)
    let scale = (try ppi?.asDouble(coerce: true) ?? 72.0)/72.0
    guard scale > 0.0 && scale <= 10.0 else {
      throw RuntimeError.range(parameter: 2,
                               of: "abstract-image->image",
                               ppi ?? .fixnum(72),
                               min: 0,
                               max: 720,
                               at: SourcePosition.unknown)
    }
    guard let cgImage = aimg.cgImage(using: self.ciContext, flipped: flip?.isTrue ?? false) else {
      return .false
    }
    #if os(iOS) || os(watchOS) || os(tvOS)
    let updateImage = UIImage(cgImage: cgImage, scale: scale, orientation: .up)
    #elseif os(macOS)
    let rep = NSBitmapImageRep(cgImage: cgImage)
    rep.size = CGSize(width: aimg.ciImage.extent.size.width / scale,
                      height: aimg.ciImage.extent.size.height / scale)
    rep.pixelsWide = Int(aimg.ciImage.extent.width)
    rep.pixelsHigh = Int(aimg.ciImage.extent.height)
    let updateImage = NSImage(size: rep.size)
    updateImage.addRepresentation(rep)
    #endif
    return .object(NativeImage(updateImage))
  }
  
  private func abstractImageBounds(expr: Expr) throws -> Expr {
    let aimg = try self.abstractImage(from: expr)
    let box = aimg.ciImage.extent
    if box.isInfinite {
      return .false
    } else {
      return .pair(.pair(.flonum(Double(box.origin.x)), .flonum(Double(box.origin.y))),
                   .pair(.flonum(Double(box.width)), .flonum(Double(box.height))))
    }
  }
  
  private func abstractImageAdjustmentFilters(img: Expr, expr: Expr?) throws -> Expr {
    let aimg = try self.abstractImage(from: img)
    var options: [CIImageAutoAdjustmentOption : Any] = [:]
    var list = expr ?? .null
    while case .pair(let head, let rest) = list {
      guard case .pair(let key, let value) = head else {
        throw RuntimeError.eval(.invalidAutoAdjustmentOption, head)
      }
      switch try key.asSymbol().identifier {
        case "crop":
          options[.crop] = value.isTrue
        case "enhance":
          options[.enhance] = value.isTrue
        case "rotate":
          options[.level] = value.isTrue
        case "red-eye":
          options[.redEye] = value.isTrue
        default:
          throw RuntimeError.eval(.invalidAutoAdjustmentOption, head)
      }
      list = rest
    }
    guard case .null = list else {
      throw RuntimeError.type(expr ?? .null, expected: [.properListType])
    }
    let filters = aimg.ciImage.autoAdjustmentFilters(options: options).reversed()
    var res = Expr.null
    for filter in filters {
      res = .pair(.object(ImageFilter(ciFilter: filter,
                                      identifier: self.filterIdentifier[filter.name])), res)
    }
    return res
  }
  
  private func isImageFilter(expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is AbstractImage else {
      return .false
    }
    return .true
  }
  
  private func parameter(value: Expr, for key: Expr) throws -> Any {
    switch value {
      case .null:
        return NSDictionary()
      case .true:
        return NSNumber(value: true)
      case .false:
        return NSNumber(value: false)
      case .string(let str):
        return NSString(string: str)
      case .fixnum(let num):
        return NSNumber(value: num)
      case .flonum(let num):
        return NSNumber(value: num)
      case .symbol(let sym):
        switch sym {
          case self.rgbColorSpace:
            return self.deviceRgbColorSpace
          default:
            throw RuntimeError.eval(.invalidFilterParameterValue, key, value)
        }
      case .pair(.flonum(let x), .flonum(let y)):
        return CIVector(cgPoint: CGPoint(x: x, y: y))
      case .pair(.pair(.flonum(let x), .flonum(let y)), .pair(.flonum(let w), .flonum(let h))):
        return CIVector(cgRect: CGRect(x: x, y: y, width: w, height: h))
      case .pair(_, _):
        let dict = NSMutableDictionary()
        var list = value
        while case .pair(.pair(.string(let str), let val), let rest) = list {
          dict[str as String] = try self.parameter(value: val, for: .string(str))
          list = rest
        }
        guard case .null = list else {
          throw RuntimeError.eval(.invalidFilterParameterValue, key, value)
        }
        return NSDictionary(dictionary: dict)
      case .vector(let coll):
        let array = NSMutableArray()
        for comp in coll.exprs {
          array.add(try self.parameter(value: comp, for: key))
        }
        return NSArray(array: array)
      case .object(let obj):
        if let nd = obj as? NativeDateTime, let date = nd.value.date {
          return date as NSDate
        } else if let nc = obj as? Color {
          #if os(iOS) || os(watchOS) || os(tvOS)
          return CIColor(color: nc.nsColor)
          #elseif os(macOS)
          return CIColor(color: nc.nsColor) ??
                 CIColor(red: nc.red, green: nc.green, blue: nc.blue, alpha: nc.alpha)
          #endif
        } else if let aimg = obj as? AbstractImage {
          return aimg.ciImage
        } else if let vector = obj as? ImageCoefficients {
          return vector.ciVector
        } else if let transform = obj as? Transformation {
          let t = transform.affineTransform
          #if os(iOS) || os(watchOS) || os(tvOS)
          return NSValue(cgAffineTransform: t)
          #elseif os(macOS)
          return t as NSAffineTransform
          #endif
        } else if let text = obj as? StyledText {
          return NSAttributedString(attributedString: text.value)
        } else {
          fallthrough
        }
      default:
        throw RuntimeError.eval(.invalidFilterParameterValue, key, value)
    }
  }
  
  private func parameterExpr(for obj: Any) -> Expr? {
    if let num = obj as? Int64 {
      return .makeNumber(num)
    } else if let num = obj as? Int {
      return .makeNumber(num)
    } else if let num = obj as? Double {
      return .makeNumber(num)
    } else if let bool = obj as? Bool {
      return .makeBoolean(bool)
    } else if let date = obj as? Date {
      return .object(NativeDateTime(DateTimeLibrary.calendar.dateComponents(in: TimeZone.current,
                                                                            from: date)))
    } else if let color = obj as? NativeColor {
      return .object(Color(color))
    } else if let color = obj as? CIColor {
      return .object(Color(red: color.red, green: color.green, blue: color.blue, alpha: color.alpha))
    } else if let aimg = obj as? CIImage {
      return .object(AbstractImage(ciImage: aimg))
    } else if let astr = obj as? NSAttributedString {
      return .object(StyledText(NSMutableAttributedString(attributedString: astr)))
    } else if let str = obj as? String {
      return .makeString(str)
    } else if let rect = obj as? CGRect {
      return .pair(.pair(.flonum(rect.origin.x), .flonum(rect.origin.y)),
                   .pair(.flonum(rect.width), .flonum(rect.height)))
    } else if let pnt = obj as? CGPoint {
      return .pair(.flonum(pnt.x), .flonum(pnt.y))
    } else if let vector = obj as? CIVector {
      return .object(ImageCoefficients(ciVector: vector))
    } else if let array = obj as? NSArray {
      var exprs: Exprs = []
      for comp in array {
        if let expr = self.parameterExpr(for: comp) {
          exprs.append(expr)
        }
      }
      return .vector(Collection(kind: .vector, exprs: exprs))
    } else if let dictionary = obj as? NSDictionary {
      var res: Exprs = []
      for (k, v) in dictionary {
        if let key = k as? String,
           let value = self.parameterExpr(for: v) {
          res.append(.pair(.makeString(key), value))
        }
      }
      return .makeList(res)
    } else if let o = obj as? AnyClass, o === self.deviceRgbColorSpace {
      return .symbol(self.rgbColorSpace)
    } else {
      #if os(iOS) || os(watchOS) || os(tvOS)
      if let val = obj as? NSValue, String(cString: val.objCType).hasPrefix("{CGAffineTransform") {
        return .object(Transformation(val.cgAffineTransformValue))
      }
      #elseif os(macOS)
      if let transform = obj as? NSAffineTransform {
        return .object(Transformation(transform as AffineTransform))
      }
      #endif
      return nil
    }
  }
  
  private func parameters(from: Expr?, input: CIImage? = nil) throws -> [String : Any] {
    var res: [String : Any] = [:]
    guard var list = from else {
      return res
    }
    while case .pair(let param, let rest) = list {
      guard case .pair(let key, let value) = param else {
        throw RuntimeError.type(param, expected: [.pairType])
      }
      let k = try self.name(from: key)
      res[k] = try self.parameter(value: value, for: key)
      list = rest
    }
    guard case .null = list else {
      throw RuntimeError.type(from!, expected: [.properListType])
    }
    if let input {
      res[kCIInputImageKey] = input
    }
    return res
  }
  
  private func coerceToImagePipe(from: Expr!) throws -> AbstractImage? {
    guard let from else {
      return nil
    }
    switch from {
      case .string(_):
        let path = self.context.fileHandler.path(
                     try from.asPath(),
                     relativeTo: self.context.evaluator.currentDirectoryPath)
        #if os(iOS) || os(watchOS) || os(tvOS)
        guard let cgImage = UIImage(contentsOfFile: path)?.cgImage else {
          throw RuntimeError.eval(.cannotLoadImage, from)
        }
        #elseif os(macOS)
        guard let nsimage = NSImage(contentsOfFile: path) else {
          throw RuntimeError.eval(.cannotLoadImage, from)
        }
        var rect = CGRect(origin: CGPoint(x: 0, y: 0), size: nsimage.size)
        guard let cgImage = nsimage.cgImage(forProposedRect: &rect, context: nil, hints: nil) else {
          throw RuntimeError.eval(.cannotLoadImage, from)
        }
        #endif
        return AbstractImage(ciImage: CIImage(cgImage: cgImage), flipped: false)
      case .object(let obj):
        if let imageBox = obj as? NativeImage {
          return AbstractImage(image: imageBox, flipped: false)
        } else if let abstractImage = obj as? AbstractImage {
          return abstractImage
        } else if let imageFilter = obj as? ImageFilter,
                  let image = imageFilter.ciFilter.outputImage {
          return AbstractImage(ciImage: image)
        } else if let nc = obj as? Color {
          #if os(iOS) || os(watchOS) || os(tvOS)
          return ImagePipe(ciImage: CIImage(color: CIColor(color: nc.nsColor)))
          #elseif os(macOS)
          return AbstractImage(ciImage:
                   CIImage(color: CIColor(color: nc.nsColor) ??
                                  CIColor(red: nc.red, green: nc.green, blue: nc.blue, alpha: nc.alpha)))
          #endif
        } else {
          fallthrough
        }
      default:
        throw RuntimeError.type(from, expected: [NativeImage.type, AbstractImage.type, ImageFilter.type])
    }
  }
  
  private func makeImageFilter(expr: Expr, fst: Expr?, snd: Expr?) throws -> Expr {
    var input: Expr? = nil
    var params: Expr? = nil
    if let fst {
      if let snd {
        input = fst
        params = snd
      } else {
        params = fst
      }
    }
    let name: String
    if case .symbol(let sym) = expr {
      guard let mappedName = self.filterName[sym] else {
        return .false
      }
      name = mappedName
    } else {
      name = try expr.asString()
    }
    let aimg = try self.coerceToImagePipe(from: input)
    guard let filter = CIFilter(
                         name: name,
                         parameters: try self.parameters(from: params, input: aimg?.ciImage)) else {
      return .false
    }
    return .object(ImageFilter(ciFilter: filter, identifier: self.filterIdentifier[name]))
  }
  
  private func imageFilterName(expr: Expr) throws -> Expr {
    guard let value = try self.imageFilter(from: expr)
                              .ciFilter.attributes[kCIAttributeFilterDisplayName] else {
      return .false
    }
    return self.parameterExpr(for: value) ?? .false
  }
  
  private func imageFilterImplementation(expr: Expr, raw: Expr?) throws -> Expr {
    let raw = raw?.isTrue ?? false
    switch expr {
      case .string(let str):
        if let ident = self.filterIdentifier[str as String] {
          return raw ? expr : .symbol(ident)
        } else {
          return .false
        }
      case .symbol(let ident):
        if let str = self.filterName[ident] {
          return raw ? .makeString(str) : expr
        } else {
          return .false
        }
      default:
        if !raw, let sym = try self.imageFilter(from: expr).identifier {
          return .symbol(sym)
        } else {
          return .makeString(try self.imageFilter(from: expr).ciFilter.name)
        }
    }
  }
  
  private func imageFilterCategories(expr: Expr, raw: Expr?) throws -> Expr {
    guard let v = try self.imageFilter(from: expr).ciFilter.attributes[kCIAttributeFilterCategories],
          let array = v as? NSArray else {
      return .null
    }
    var exprs: Exprs = []
    for comp in array {
      if let str = comp as? String {
        if !(raw?.isTrue ?? false), case .symbol(let sym) = self.filterCategories.expr(for: str) {
          exprs.append(.symbol(sym))
        } else {
          exprs.append(.makeString(str))
        }
      }
    }
    return .makeList(exprs)
  }
  
  private func imageFilterAvailable(expr: Expr) throws -> Expr {
    let mac = try self.imageFilter(from: expr).ciFilter.attributes[kCIAttributeFilterAvailable_Mac]
    let ios = try self.imageFilter(from: expr).ciFilter.attributes[kCIAttributeFilterAvailable_iOS]
    return .values(.pair((mac == nil ? nil : self.parameterExpr(for: mac!)) ?? .false,
                         .pair((ios == nil ? nil : self.parameterExpr(for: ios!)) ?? .false,
                               .null)))
  }
  
  private func imageFilterInputs(expr: Expr, raw: Expr?) throws -> Expr {
    let keys = try self.imageFilter(from: expr).ciFilter.inputKeys
    var res: Exprs = []
    for key in keys {
      if !(raw?.isTrue ?? false), let ident = Self.externalName(key) {
        res.append(.symbol(self.context.symbols.intern(ident)))
      } else {
        res.append(.makeString(key))
      }
    }
    return .makeList(res)
  }
  
  private func imageFilterOutputs(expr: Expr, raw: Expr?) throws -> Expr {
    let keys = try self.imageFilter(from: expr).ciFilter.outputKeys
    var res: Exprs = []
    for key in keys {
      if !(raw?.isTrue ?? false), let ident = Self.externalName(key) {
        res.append(.symbol(self.context.symbols.intern(ident)))
      } else {
        res.append(.makeString(key))
      }
    }
    return .makeList(res)
  }
  
  private func imageFilterOutput(expr: Expr) throws -> Expr {
    if let outputImage = try self.imageFilter(from: expr).ciFilter.outputImage {
      return .object(AbstractImage(ciImage: outputImage))
    } else {
      return .false
    }
  }
  
  private func imageFilterArgument(expr: Expr, key: Expr) throws -> Expr {
    guard let meta = try self.imageFilter(from: expr).ciFilter.attributes[self.name(from: key)],
          let dictionary = meta as? NSDictionary else {
      return .false
    }
    var res: Exprs = []
    var type: String? = nil
    var clazz: String? = nil
    var description: String? = nil
    for (k, v) in dictionary {
      if let key = k as? String,
         let value = self.parameterExpr(for: v) {
        if key == kCIAttributeType, case .string(let str) = value {
          type = str as String
        } else if let name = Self.externalName(key, prefix: ("CIAttribute", "")) {
          if key == kCIAttributeDescription, case .string(let str) = value {
            description = str as String
          }
          if name == "class", case .string(let str) = value {
            clazz = str as String
          }
          res.append(.pair(.symbol(self.context.symbols.intern(name)), value))
        } else {
          res.append(.pair(.makeString(key), value))
        }
      }
    }
    if let clazz, type == nil {
      switch clazz {
        case "NSAttributedString":
          type = "CIAttributeTypeAttributedString"
        case "NSDate", "Date":
          type = "CIAttributeTypeDate"
        case "NSString":
          type = "CIAttributeTypeString"
        case "NSNumber":
          type = "CIAttributeTypeNumber"
        case "NSData", "Data":
          type = "CIAttributeTypeData"
        case "CIVector":
          type = "CIAttributeTypeCIVector"
        case "NSArray":
          type = "CIAttributeTypeArray"
        case "NSObject":
          if description?.contains("CGColorSpace") ?? false {
            type = "CIAttributeTypeColorSpace"
          }
        default:
          break
      }
    }
    if let type, case .symbol(let sym) = self.argumentTypes.expr(for: type) {
      res.append(.pair(.symbol(self.context.symbols.intern("type")), .symbol(sym)))
    }
    return .makeList(res)
  }
  
  private func imageFilterArgumentRef(expr: Expr, key: Expr, default: Expr?) throws -> Expr {
    guard let value = try self.imageFilter(from: expr).ciFilter.value(
                        forKey: self.name(from: key)) else {
      return `default` ?? .false
    }
    return self.parameterExpr(for: value) ?? .null
  }
  
  private func imageFilterArgumentSet(expr: Expr, key: Expr, value: Expr) throws -> Expr {
    let k = try self.name(from: key)
    try self.imageFilter(from: expr).ciFilter.setValue(self.parameter(value: value, for: key),
                                                       forKey: k)
    return .void
  }
  
  private func imageCoefficients(args: Arguments) throws -> Expr {
    var array: [CGFloat] = []
    for arg in args {
      switch arg {
        case .null:
          break
        case .vector(let coll):
          for expr in coll.exprs {
            array.append(try expr.asDouble(coerce: true))
          }
        case .pair(_, _):
          var todo = Exprs()
          var list = arg
          while case .pair(let head, let tail) = list {
            switch head {
              case .null:
                list = tail
              case .pair(_, _):
                switch tail {
                  case .null:
                    break
                  case .pair(_, _):
                    todo.append(tail)
                  default:
                    todo.append(.pair(tail, .null))
                }
                list = head
              default:
                array.append(try head.asDouble(coerce: true))
                list = tail
            }
            while todo.count > 0 {
              if case .pair(_, _) = list {
                break
              } else if !list.isNull {
                array.append(try list.asDouble(coerce: true))
              }
              list = todo.removeLast()
            }
          }
          if !list.isNull {
            array.append(try list.asDouble(coerce: true))
          }
        default:
          array.append(try arg.asDouble(coerce: true))
      }
    }
    return .object(ImageCoefficients(ciVector: CIVector(values: array, count: array.count)))
  }
  
  private func imageCoefficientsToVector(expr: Expr) throws -> Expr {
    let ciVector = try self.imageCoefficients(from: expr).ciVector
    var exprs: Exprs = []
    for i in 0..<ciVector.count {
      exprs.append(.flonum(ciVector.value(at: i)))
    }
    return .vector(Collection(kind: .vector, exprs: exprs))
  }
  
  private func imageCoefficientsToPoint(expr: Expr) throws -> Expr {
    let ciVector = try self.imageCoefficients(from: expr).ciVector
    if ciVector.count >= 2 {
      let point = ciVector.cgPointValue
      return .pair(.flonum(point.x), .flonum(point.y))
    } else {
      return .false
    }
  }
  
  private func imageCoefficientsToRect(expr: Expr) throws -> Expr {
    let ciVector = try self.imageCoefficients(from: expr).ciVector
    if ciVector.count >= 4 {
      let rect = ciVector.cgRectValue
      return .pair(.pair(.flonum(rect.origin.x), .flonum(rect.origin.y)),
                   .pair(.flonum(rect.width), .flonum(rect.height)))
    } else {
      return .false
    }
  }
  
  private func applyImageFilter(expr: Expr, args: Arguments) throws -> Expr {
    var image = try self.abstractImage(from: expr).ciImage
    for a in args {
      var list = a
      if case .pair(.symbol(_), _) = a {
        list = .pair(a, .null)
      }
      while case .pair(let arg, let rest) = list {
        guard case .pair(let ident, let params) = arg else {
          throw RuntimeError.type(arg, expected: [.pairType])
        }
        let name: String
        if case .symbol(let sym) = ident {
          guard let mappedName = self.filterName[sym] else {
            return .false
          }
          name = mappedName
        } else {
          name = try ident.asString()
        }
        guard let filter = CIFilter(name: name,
                                    parameters: try self.parameters(from: params, input: image)) else {
          return .false
        }
        if let outputImage = filter.outputImage {
          image = outputImage
        } else {
          return .false
        }
        list = rest
      }
      guard case .null = list else {
        throw RuntimeError.type(a, expected: [.properListType])
      }
    }
    return .object(AbstractImage(ciImage: image))
  }
  
  private func mapImagePoint(expr: Expr, img: Expr) throws -> Expr {
    guard case .pair(.flonum(let x), .flonum(let y)) = expr else {
      throw RuntimeError.eval(.invalidPoint, expr)
    }
    guard case .object(let obj) = img,
          let imageBox = obj as? NativeImage else {
      throw RuntimeError.type(img, expected: [NativeImage.type])
    }
    let imageSize = imageBox.imageSize
    guard let pixelSize = imageBox.pixelSize,
          imageSize.width > 0 && imageSize.height > 0 &&
          pixelSize.width > 0 && pixelSize.height > 0 else {
      return .false
    }
    let pnt = CGPoint(x: x, y: y)
    let sx = pixelSize.width / imageSize.width
    let sy = pixelSize.height / imageSize.height
    let res = pnt.applying(CGAffineTransform(scaleX: sx, y: -sy))
                 .applying(CGAffineTransform(translationX: 0, y: pixelSize.height))
    return .pair(.flonum(res.x), .flonum(res.y))
  }
  
  private func mapImageRect(expr: Expr, img: Expr) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                     .pair(.flonum(let w), .flonum(let h))) = expr else {
      throw RuntimeError.eval(.invalidRect, expr)
    }
    guard case .object(let obj) = img,
          let imageBox = obj as? NativeImage else {
      throw RuntimeError.type(img, expected: [NativeImage.type])
    }
    let imageSize = imageBox.imageSize
    guard let pixelSize = imageBox.pixelSize,
          imageSize.width > 0 && imageSize.height > 0 &&
          pixelSize.width > 0 && pixelSize.height > 0 else {
      return .false
    }
    let rect = CGRect(origin: CGPoint(x: x, y: y), size: CGSize(width: w, height: h))
    let sx = pixelSize.width / imageSize.width
    let sy = pixelSize.height / imageSize.height
    let res = rect.applying(CGAffineTransform(scaleX: sx, y: -sy))
                  .applying(CGAffineTransform(translationX: 0, y: pixelSize.height))
    return .pair(.pair(.flonum(res.origin.x), .flonum(res.origin.y)),
                 .pair(.flonum(res.width), .flonum(res.height)))
  }
}

///
/// Image coefficients are implemented in terms of the `CIVector` class provided by
/// Core Image.
/// 
public struct ImageCoefficients: CustomExpr {
  public static let type = Type.objectType(Symbol(uninterned: "image-coefficients"))
  
  public let ciVector: CIVector
  
  public var type: Type {
    return Self.type
  }
  
  private func appendElems(to prefix: String) -> String {
    var res = prefix
    var sep = prefix.isEmpty ? "" : ": "
    for i in 0..<min(36, self.ciVector.count) {
      res += "\(sep) \(self.ciVector.value(at: i))"
      sep = ", "
    }
    return self.ciVector.count > 36 ? res + ", …" : res
  }
  
  public var tagString: String {
    return self.appendElems(to: "image-coefficients \(self.identityString) \(self.ciVector.count)")
  }
  
  public var identity: UInt {
    return UInt(bitPattern: ObjectIdentifier(self.ciVector))
  }
  
  public var identityString: String {
    return String(self.identity, radix: 16)
  }
  
  public var hash: Int {
    return self.ciVector.hash
  }
  
  public func equals(to expr: Expr) -> Bool {
    guard case .object(let obj) = expr,
          let other = obj as? ImageCoefficients else {
      return false
    }
    return self.ciVector === other.ciVector
  }
  
  public func unpack(in context: Context) -> Exprs {
    return [.vector(Collection(kind: .immutableVector, exprs: [
      .makeString(self.identityString),
      .makeNumber(self.ciVector.count),
      .makeString(self.appendElems(to: ""))
    ]))]
  }
}

///
/// Abstract images are implemented in terms of the `CIImage` class provided by
/// Core Image.
/// 
public struct AbstractImage: CustomExpr {
  public static let type = Type.objectType(Symbol(uninterned: "abstract-image"))
  
  public let ciImage: CIImage
  
  public init?(url: URL, flipped: Bool = false) {
    guard var ciImage = CIImage(contentsOf: url) else {
      return nil
    }
    if flipped {
      let height = ciImage.extent.height
      ciImage = ciImage.transformed(by: CGAffineTransformMakeScale(1, -1))
      ciImage = ciImage.transformed(by: CGAffineTransformMakeTranslation(0, height))
    }
    self.ciImage = ciImage
  }
  
  public init?(data: Data, flipped: Bool = false) {
    guard var ciImage = CIImage(data: data) else {
      return nil
    }
    if flipped {
      let height = ciImage.extent.height
      ciImage = ciImage.transformed(by: CGAffineTransformMakeScale(1, -1))
      ciImage = ciImage.transformed(by: CGAffineTransformMakeTranslation(0, height))
    }
    self.ciImage = ciImage
  }
  
  public init?(image: NativeImage, flipped: Bool = false) {
    #if os(iOS) || os(watchOS) || os(tvOS)
    guard let cgImage = image.value.cgImage else {
      return nil
    }
    #elseif os(macOS)
    var rect = CGRect(origin: CGPoint(x: 0, y: 0), size: image.value.size)
    guard let cgImage = image.value.cgImage(forProposedRect: &rect, context: nil, hints: nil) else {
      return nil
    }
    #endif
    var ciImage = CIImage(cgImage: cgImage)
    if flipped {
      let height = ciImage.extent.height
      ciImage = ciImage.transformed(by: CGAffineTransformMakeScale(1, -1))
      ciImage = ciImage.transformed(by: CGAffineTransformMakeTranslation(0, height))
    }
    self.ciImage = ciImage
  }
  
  public init(ciImage: CIImage, flipped: Bool = false) {
    var ciImage = ciImage
    if flipped {
      let height = ciImage.extent.height
      ciImage = ciImage.transformed(by: CGAffineTransformMakeScale(1, -1))
      ciImage = ciImage.transformed(by: CGAffineTransformMakeTranslation(0, height))
    }
    self.ciImage = ciImage
  }
  
  public func cgImage(using ciContext: CIContext, flipped: Bool = false) -> CGImage? {
    var ciImage = self.ciImage
    if flipped {
      let height = ciImage.extent.height
      ciImage = ciImage.transformed(by: CGAffineTransformMakeScale(1, -1))
      ciImage = ciImage.transformed(by: CGAffineTransformMakeTranslation(0, height))
    }
    return ciContext.createCGImage(ciImage, from: ciImage.extent)
  }
  
  public var type: Type {
    return Self.type
  }
  
  public var tagString: String {
    if let width = Int64(exactly: floor(self.ciImage.extent.width)),
       let height = Int64(exactly: floor(self.ciImage.extent.height)) {
      return "abstract-image \(self.identityString): \(width)×\(height)"
    } else {
      return "abstract-image \(self.identityString)"
    }
  }
  
  public var identity: UInt {
    return UInt(bitPattern: ObjectIdentifier(self.ciImage))
  }
  
  public var identityString: String {
    return String(self.identity, radix: 16)
  }
  
  public var hash: Int {
    return self.ciImage.hash
  }
  
  public func equals(to expr: Expr) -> Bool {
    guard case .object(let obj) = expr, let other = obj as? AbstractImage else {
      return false
    }
    return self.ciImage === other.ciImage
  }
  
  public func unpack(in context: Context) -> Exprs {
    return [.vector(Collection(kind: .immutableVector, exprs: [
      .makeString(self.identityString),
      .pair(.makeNumber(self.ciImage.extent.width), .makeNumber(self.ciImage.extent.height))
    ]))]
  }
}

  ///
  /// Image filters are implemented in terms of the `CIFilter` class provided by
  /// Core Image.
  /// 
  public struct ImageFilter: CustomExpr {
    public static let type = Type.objectType(Symbol(uninterned: "image-filter"))
    
    public let ciFilter: CIFilter
    public let identifier: Symbol?
    
    public var type: Type {
      return Self.type
    }
    
    public var tagString: String {
      return "image-filter \(self.identityString): \(identifier?.identifier ?? "\"\(self.ciFilter.name)\"")"
    }
    
    public var identity: UInt {
      return UInt(bitPattern: ObjectIdentifier(self.ciFilter))
    }
    
    public var identityString: String {
      return String(self.identity, radix: 16)
    }
    
    public var hash: Int {
      return self.ciFilter.hash
    }
    
    public func equals(to expr: Expr) -> Bool {
      guard case .object(let obj) = expr,
            let other = obj as? ImageFilter else {
        return false
      }
      return self.ciFilter === other.ciFilter
    }
    
    public func unpack(in context: Context) -> Exprs {
      return [.vector(Collection(kind: .immutableVector, exprs: [
        .makeString(self.identityString),
        .makeString(self.ciFilter.name)
      ]))]
    }
  }
