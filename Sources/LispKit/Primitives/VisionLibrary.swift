//
//  VisionLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 02/01/2025.
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
import Vision

public final class VisionLibrary: NativeLibrary {
  
  /// Initialize symbols.
  public required init(in context: Context) throws {
    try super.init(in: context)
  }
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "vision"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("recognize-text", self.recognizeText))
    self.define(Procedure("recognized-text-confidence", self.recognizedTextConfidence))
    self.define(Procedure("recognized-text-string", self.recognizedTextString))
    self.define(Procedure("recognized-text-corners", self.recognizedTextCorners))
    self.define(Procedure("recognized-text-bounds", self.recognizedTextBoundingBox))
    self.define(Procedure("detect-rectangles", self.detectRectangles))
    self.define(Procedure("supported-barcode-symbologies", self.supportedBarcodeSymbologies))
    self.define(Procedure("recognize-barcodes", self.recognizeBarcodes))
    self.define(Procedure("supported-classification-identifiers", self.supportedClassIdent))
    self.define(Procedure("classify-image", self.classifyImage))
  }
  
  /// Initializations of the library.
  public override func initializations() {
  }
  
  private func color(from: Expr) throws -> NativeColor? {
    if from.isFalse {
      return nil
    }
    guard case .object(let obj) = from, let color = obj as? Color else {
      throw RuntimeError.type(from, expected: [Color.type])
    }
    return color.nsColor
  }
  
  private func bitmap(from: Expr) throws -> (NativeImage, CGImage) {
    guard case .object(let obj) = from,
          let image = obj as? NativeImage else {
      throw RuntimeError.type(from, expected: [NativeImage.type])
    }
    guard let cgImage = image.cgImage() else {
      throw RuntimeError.eval(.imageIsNotABitmap, from)
    }
    return (image, cgImage)
  }
  
  private func imageRect(forNormalizedRect rect: CGRect, width: Double, height: Double) -> CGRect {
    return CGRect(x: rect.minX * width,
                  y: (1.0 - rect.maxY) * height,
                  width: rect.width * width,
                  height: rect.height * height)
  }
  
  private func imagePoint(forNormalizedPoint point: CGPoint, width: Double, height: Double) -> CGPoint {
    return CGPoint(x: point.x * width, y: (1.0 - point.y) * height)
  }
  
  private func recognizedText(from: Expr) throws -> RecognizedText {
    guard case .object(let obj) = from,
          let rt = obj as? RecognizedText else {
      throw RuntimeError.type(from, expected: [RecognizedText.type])
    }
    return rt
  }
  
  private func recognizeText(expr: Expr, args: Arguments) throws -> Expr {
    guard let (area, candidates, langs, words, minTextHeight) =
            args.optional(.false, .fixnum(1), .true, .false, .false) else {
      throw RuntimeError.argumentCount(of: "recognize-text",
                                       min: 1,
                                       max: 5,
                                       args: .pair(expr, .makeList(args)))
    }
    let (image, cgImage) = try self.bitmap(from: expr)
    let top = try candidates.asInt(above: 1, below: 11)
    let requestHandler = VNImageRequestHandler(cgImage: cgImage)
    let result = Future(external: false)
    let context = self.context
    let request = VNRecognizeTextRequest { (request: VNRequest, error: Error?) in
      do {
        if let error = error {
          _ = try result.setResult(in: context, to: .error(RuntimeError.os(error)), raise: true)
        } else if let observations = request.results {
          var res = Expr.null
          for observation in observations.reversed() {
            if let textObservation = observation as? VNRecognizedTextObservation {
              var obs = Expr.null
              for candidate in textObservation.topCandidates(top).reversed() {
                obs = .pair(.object(RecognizedText(recognized: candidate,
                                                   width: image.width,
                                                   height: image.height)), obs)
              }
              let bounds = self.imageRect(forNormalizedRect: textObservation.boundingBox,
                                          width: image.width,
                                          height: image.height)
              res = .pair(.pair(.pair(.pair(.flonum(bounds.origin.x), .flonum(bounds.origin.y)),
                                      .pair(.flonum(bounds.width), .flonum(bounds.height))), obs), res)
            }
          }
          _ = try result.setResult(in: context, to: res, raise: false)
        } else {
          _ = try result.setResult(in: context, to: .false, raise: false)
        }
      } catch let error {
        do {
          _ = try result.setResult(in: context,
                                   to: .error(RuntimeError.eval(.unableToReturnResultViaFuture,
                                                                .object(result),
                                                                .error(RuntimeError.os(error)))),
                                   raise: true)
        } catch {}
      }
    }
    if area.isTrue {
      guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                       .pair(.flonum(let w), .flonum(let h))) = area else {
        throw RuntimeError.eval(.invalidRect, area)
      }
      request.regionOfInterest = CGRect(x: x, y: y, width: w, height: h)
    }
    request.recognitionLevel = .accurate
    request.usesLanguageCorrection = true
    if langs.isFalse {
      request.automaticallyDetectsLanguage = false
    } else if case .true = langs {
      request.automaticallyDetectsLanguage = true
    } else {
      var languages: [String] = []
      var list = langs
      while case .pair(let lang, let rest) = list {
        let code = try lang.asString()
        guard Locale.LanguageCode(code).isISOLanguage else {
          return .false
        }
        languages.append(code)
        list = rest
      }
      request.recognitionLanguages = languages
      switch list {
        case .null, .true:
          request.automaticallyDetectsLanguage = true
        case .false:
          request.automaticallyDetectsLanguage = false
        default:
          throw RuntimeError.type(list, expected: [.booleanType])
      }
    }
    if words.isTrue {
      var customWords: [String] = []
      var list = words
      while case .pair(let word, let rest) = list {
        customWords.append(try word.asString())
        list = rest
      }
      guard case .null = list else {
        throw RuntimeError.type(words, expected: [.properListType])
      }
      request.customWords = customWords
    }
    if minTextHeight.isTrue {
      let height = Float(try minTextHeight.asDouble(coerce: true))
      if height > 0.0 && height <= 10000.0 {
        request.minimumTextHeight = height
      }
    }
    // try request.supportedRecognitionLanguages()
    try requestHandler.perform([request])
    return .object(result)
  }
  
  private func recognizedTextConfidence(expr: Expr) throws -> Expr {
    return .makeNumber(Double(try self.recognizedText(from: expr).recognized.confidence))
  }
  
  private func recognizedTextString(expr: Expr) throws -> Expr {
    return .makeString(try self.recognizedText(from: expr).recognized.string)
  }
  
  private func recognizedTextCorners(expr: Expr, fst: Expr?, snd: Expr?) throws -> Expr {
    let txt = try self.recognizedText(from: expr)
    let rt = txt.recognized
    let observed: VNRectangleObservation?
    guard !rt.string.isEmpty else {
      return .false
    }
    if let fst {
      let a = min(try fst.asInt(above: 0, below: Int.max), rt.string.count - 1)
      let s = rt.string.index(rt.string.startIndex, offsetBy: a)
      if let snd {
        let b = min(try snd.asInt(above: a + 1, below: Int.max), rt.string.count)
        observed = try rt.boundingBox(for: s..<rt.string.index(rt.string.startIndex, offsetBy: b))
      } else {
        observed = try rt.boundingBox(for: s..<rt.string.index(after: s))
      }
    } else {
      observed = try rt.boundingBox(for: rt.string.startIndex..<rt.string.endIndex)
    }
    guard let observed else {
      return .false
    }
    let bl = imagePoint(forNormalizedPoint: observed.bottomLeft, width: txt.width, height: txt.height)
    let br = imagePoint(forNormalizedPoint: observed.bottomRight, width: txt.width, height: txt.height)
    let tr = imagePoint(forNormalizedPoint: observed.topRight, width: txt.width, height: txt.height)
    let tl = imagePoint(forNormalizedPoint: observed.topLeft, width: txt.width, height: txt.height)
    return .pair(.pair(.flonum(tl.x), .flonum(tl.y)),
                 .pair(.pair(.flonum(tr.x), .flonum(tr.y)),
                       .pair(.pair(.flonum(br.x), .flonum(br.y)),
                             .pair(.pair(.flonum(bl.x), .flonum(bl.y)),
                                   .null))))
  }
  
  private func recognizedTextBoundingBox(expr: Expr, fst: Expr?, snd: Expr?) throws -> Expr {
    let txt = try self.recognizedText(from: expr)
    let rt = txt.recognized
    let observed: VNRectangleObservation?
    guard !rt.string.isEmpty else {
      return .false
    }
    if let fst {
      let a = min(try fst.asInt(above: 0, below: Int.max), rt.string.count - 1)
      let s = rt.string.index(rt.string.startIndex, offsetBy: a)
      if let snd {
        let b = min(try snd.asInt(above: a + 1, below: Int.max), rt.string.count)
        observed = try rt.boundingBox(for: s..<rt.string.index(rt.string.startIndex, offsetBy: b))
      } else {
        observed = try rt.boundingBox(for: s..<rt.string.index(after: s))
      }
    } else {
      observed = try rt.boundingBox(for: rt.string.startIndex..<rt.string.endIndex)
    }
    guard let observed else {
      return .false
    }
    let bl = imagePoint(forNormalizedPoint: observed.bottomLeft, width: txt.width, height: txt.height)
    let br = imagePoint(forNormalizedPoint: observed.bottomRight, width: txt.width, height: txt.height)
    let tr = imagePoint(forNormalizedPoint: observed.topRight, width: txt.width, height: txt.height)
    let tl = imagePoint(forNormalizedPoint: observed.topLeft, width: txt.width, height: txt.height)
    let x = min(bl.x, br.x, tr.x, tl.x)
    let y = min(bl.y, br.y, tr.y, tl.y)
    return .pair(.pair(.flonum(x), .flonum(y)),
                 .pair(.flonum(max(bl.x, br.x, tr.x, tl.x) - x),
                       .flonum(max(bl.y, br.y, tr.y, tl.y) - y)))
  }
  
  private func detectRectangles(expr: Expr, args: Arguments) throws -> Expr {
    guard let (area, num, aspectRatio, tolerance, size, confidence) =
            args.optional(.false, .false, .false, .false, .false, .false) else {
      throw RuntimeError.argumentCount(of: "detect-rectangles",
                                       min: 1,
                                       max: 6,
                                       args: .pair(expr, .makeList(args)))
    }
    let (image, cgImage) = try self.bitmap(from: expr)
    let requestHandler = VNImageRequestHandler(cgImage: cgImage)
    let result = Future(external: false)
    let context = self.context
    let request = VNDetectRectanglesRequest { (request: VNRequest, error: Error?) in
      do {
        if let error = error {
          _ = try result.setResult(in: context, to: .error(RuntimeError.os(error)), raise: true)
        } else if let observations = request.results {
          var res = Expr.null
          for observation in observations.reversed() {
            if let obs = observation as? VNRectangleObservation {
              let bounds = self.imageRect(forNormalizedRect: obs.boundingBox,
                                          width: image.width,
                                          height: image.height)
              let bl = self.imagePoint(forNormalizedPoint: obs.bottomLeft, width: image.width, height: image.height)
              let br = self.imagePoint(forNormalizedPoint: obs.bottomRight, width: image.width, height: image.height)
              let tr = self.imagePoint(forNormalizedPoint: obs.topRight, width: image.width, height: image.height)
              let tl = self.imagePoint(forNormalizedPoint: obs.topLeft, width: image.width, height: image.height)
              res = .pair(.pair(.pair(.pair(.flonum(bounds.origin.x), .flonum(bounds.origin.y)),
                                      .pair(.flonum(bounds.width), .flonum(bounds.height))),
                                .pair(.flonum(Double(obs.confidence)),
                                      .pair(.pair(.pair(.flonum(tl.x), .flonum(tl.y)),
                                                  .pair(.pair(.flonum(tr.x), .flonum(tr.y)),
                                                        .pair(.pair(.flonum(br.x), .flonum(br.y)),
                                                              .pair(.pair(.flonum(bl.x), .flonum(bl.y)),
                                                                    .null)))),
                                            .null))), res)
            }
          }
          _ = try result.setResult(in: context, to: res, raise: false)
        } else {
          _ = try result.setResult(in: context, to: .false, raise: false)
        }
      } catch let error {
        do {
          _ = try result.setResult(in: context,
                                   to: .error(RuntimeError.eval(.unableToReturnResultViaFuture,
                                                                .object(result),
                                                                .error(RuntimeError.os(error)))),
                                   raise: true)
        } catch {}
      }
    }
    if area.isTrue {
      guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                       .pair(.flonum(let w), .flonum(let h))) = area else {
        throw RuntimeError.eval(.invalidRect, area)
      }
      request.regionOfInterest = CGRect(x: x, y: y, width: w, height: h)
    }
    if aspectRatio.isTrue {
      guard case .pair(let minARatio, let maxARatio) = aspectRatio else {
        throw RuntimeError.type(aspectRatio, expected: [.pairType])
      }
      var minAR = try minARatio.asDouble(coerce: true)
      if minAR < 0.0 {
        minAR = 0.0
      } else if minAR > 1.0 {
        minAR = 1.0
      }
      var maxAR = try maxARatio.asDouble(coerce: true)
      if maxAR < minAR {
        maxAR = min(minAR + 0.05, 1.0)
      } else if maxAR > 1.0 {
        maxAR = 1.0
      }
      request.minimumAspectRatio = Float(minAR)
      request.maximumAspectRatio = Float(maxAR)
    } else {
      request.minimumAspectRatio = 0.3
      request.maximumAspectRatio = 1.0
    }
    if tolerance.isTrue {
      let t = try tolerance.asDouble(coerce: true)
      guard t >= 0.0 && t <= 45.0 else {
        throw RuntimeError.range(parameter: 4, of: "detect-rectangles", tolerance, min: 0, max: 45)
      }
      request.quadratureTolerance = VNDegrees(t)
    } else {
      request.quadratureTolerance = 20.0
    }
    if size.isTrue {
      let s = try size.asDouble(coerce: true)
      guard s >= 0.0 && s <= 1.0 else {
        throw RuntimeError.range(parameter: 5, of: "detect-rectangles", tolerance, min: 0, max: 1)
      }
      request.minimumSize = Float(s)
    } else {
      request.minimumSize = 0.2
    }
    if confidence.isTrue {
      let c = try confidence.asDouble(coerce: true)
      guard c >= 0.0 && c <= 1.0 else {
        throw RuntimeError.range(parameter: 6, of: "detect-rectangles", tolerance, min: 0, max: 1)
      }
      request.minimumConfidence = VNConfidence(c)
    } else {
      request.minimumConfidence = 0.0
    }
    if num.isTrue {
      request.maximumObservations = try num.asInt(above: 0, below: 100001)
    } else {
      request.maximumObservations = 0
    }
    try requestHandler.perform([request])
    return .object(result)
  }
  
  private let defaultSymbologies: [VNBarcodeSymbology] = [
    .aztec,
    .code128,
    .dataMatrix,
    .ean8,
    .ean13,
    .gs1DataBar,
    .qr,
    .pdf417
  ]
  
  private func expr(for symbology: VNBarcodeSymbology) -> Expr {
    var ident = symbology.rawValue
    switch ident {
      case VNBarcodeSymbology.aztec.rawValue:
        ident = "aztec"
      case VNBarcodeSymbology.codabar.rawValue:
        ident = "codabar"
      case VNBarcodeSymbology.code39.rawValue:
        ident = "code39"
      case VNBarcodeSymbology.code39Checksum.rawValue:
        ident = "code39-checksum"
      case VNBarcodeSymbology.code39FullASCII.rawValue:
        ident = "code39-full-ascii"
      case VNBarcodeSymbology.code39FullASCIIChecksum.rawValue:
        ident = "code39-full-ascii-checksum"
      case VNBarcodeSymbology.code93.rawValue:
        ident = "code93"
      case VNBarcodeSymbology.code93i.rawValue:
        ident = "code93i"
      case VNBarcodeSymbology.code128.rawValue:
        ident = "code128"
      case VNBarcodeSymbology.dataMatrix.rawValue:
        ident = "data-matrix"
      case VNBarcodeSymbology.ean8.rawValue:
        ident = "ean8"
      case VNBarcodeSymbology.ean13.rawValue:
        ident = "ean13"
      case VNBarcodeSymbology.gs1DataBar.rawValue:
        ident = "gs1-databar"
      case VNBarcodeSymbology.gs1DataBarExpanded.rawValue:
        ident = "gs1-databar-expanded"
      case VNBarcodeSymbology.gs1DataBarLimited.rawValue:
        ident = "gs1-databar-limited"
      case VNBarcodeSymbology.i2of5.rawValue:
        ident = "i2of5"
      case VNBarcodeSymbology.i2of5Checksum.rawValue:
        ident = "i2of5-checksum"
      case VNBarcodeSymbology.itf14.rawValue:
        ident = "itf14"
      case VNBarcodeSymbology.microPDF417.rawValue:
        ident = "micro-pdf417"
      case VNBarcodeSymbology.microQR.rawValue:
        ident = "micro-qr"
      case VNBarcodeSymbology.qr.rawValue:
        ident = "qr"
      case VNBarcodeSymbology.pdf417.rawValue:
        ident = "pdf417"
      case VNBarcodeSymbology.upce.rawValue:
        ident = "upce"
      case "VNBarcodeSymbologyMSIPlessey":
        ident = "msi-plessey"
      default:
        break
    }
    return .symbol(self.context.symbols.intern(ident))
  }
  
  private func symbology(from: Expr) throws -> VNBarcodeSymbology {
    let ident = try from.asSymbol().identifier
    switch ident {
      case "aztec":
        return .aztec
      case "codabar":
        return .codabar
      case "code39":
        return .code39
      case "code39-checksum":
        return .code39Checksum
      case "code39-full-ascii":
        return .code39FullASCII
      case "code39-full-ascii-checksum":
        return .code39FullASCIIChecksum
      case "code93":
        return .code93
      case "code93i":
        return .code93
      case "code128":
        return .code128
      case "data-matrix":
        return .dataMatrix
      case "ean8":
        return .ean8
      case "ean13":
        return .ean13
      case "gs1-databar":
        return .gs1DataBar
      case "gs1-databar-expanded":
        return .gs1DataBarExpanded
      case "gs1-databar-limited":
        return .gs1DataBarLimited
      case "i2of5":
        return .i2of5
      case "i2of5-checksum":
        return .i2of5Checksum
      case "itf14":
        return .itf14
      case "micro-pdf417":
        return .microPDF417
      case "micro-qr":
        return .microQR
      case "pdf417":
        return .pdf417
      case "qr":
        return .qr
      case "upce":
        return .upce
      case "msi-plessey":
        return VNBarcodeSymbology(rawValue: "VNBarcodeSymbologyMSIPlessey")
      default:
        return VNBarcodeSymbology(rawValue: ident)
    }
  }
  
  private func supportedBarcodeSymbologies() throws -> Expr {
    var res = Expr.null
    for symbology in try VNDetectBarcodesRequest().supportedSymbologies() {
      res = .pair(self.expr(for: symbology), res)
    }
    return res
  }
  
  private func recognizeBarcodes(expr: Expr, area: Expr?, symbologies: Expr?) throws -> Expr {
    let (image, cgImage) = try self.bitmap(from: expr)
    let requestHandler = VNImageRequestHandler(cgImage: cgImage)
    let result = Future(external: false)
    let context = self.context
    let request = VNDetectBarcodesRequest { (request: VNRequest, error: Error?) in
      do {
        if let error = error {
          _ = try result.setResult(in: context, to: .error(RuntimeError.os(error)), raise: true)
        } else if let observations = request.results {
          var res = Expr.null
          for observation in observations.reversed() {
            if let obs = observation as? VNBarcodeObservation {
              let bounds = self.imageRect(forNormalizedRect: obs.boundingBox,
                                          width: image.width,
                                          height: image.height)
              let bl = self.imagePoint(forNormalizedPoint: obs.bottomLeft, width: image.width, height: image.height)
              let br = self.imagePoint(forNormalizedPoint: obs.bottomRight, width: image.width, height: image.height)
              let tr = self.imagePoint(forNormalizedPoint: obs.topRight, width: image.width, height: image.height)
              let tl = self.imagePoint(forNormalizedPoint: obs.topLeft, width: image.width, height: image.height)
              let payloadString: Expr
              if let str = obs.payloadStringValue {
                payloadString = .makeString(str)
              } else {
                payloadString = .false
              }
              let symbology: Expr = self.expr(for: obs.symbology)
              res = .pair(.pair(.pair(.pair(.flonum(bounds.origin.x), .flonum(bounds.origin.y)),
                                      .pair(.flonum(bounds.width), .flonum(bounds.height))),
                                .pair(.flonum(Double(obs.confidence)),
                                      .pair(.pair(.pair(.flonum(tl.x), .flonum(tl.y)),
                                                  .pair(.pair(.flonum(tr.x), .flonum(tr.y)),
                                                        .pair(.pair(.flonum(br.x), .flonum(br.y)),
                                                              .pair(.pair(.flonum(bl.x), .flonum(bl.y)),
                                                                    .null)))),
                                            .pair(symbology,
                                                  .pair(payloadString, .null))))), res)
            }
          }
          _ = try result.setResult(in: context, to: res, raise: false)
        } else {
          _ = try result.setResult(in: context, to: .false, raise: false)
        }
      } catch let error {
        do {
          _ = try result.setResult(in: context,
                                   to: .error(RuntimeError.eval(.unableToReturnResultViaFuture,
                                                                .object(result),
                                                                .error(RuntimeError.os(error)))),
                                   raise: true)
        } catch {}
      }
    }
    if let area, area.isTrue {
      guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                       .pair(.flonum(let w), .flonum(let h))) = area else {
        throw RuntimeError.eval(.invalidRect, area)
      }
      request.regionOfInterest = CGRect(x: x, y: y, width: w, height: h)
    }
    var syms: [VNBarcodeSymbology] = []
    if var list = symbologies {
      while case .pair(let symbology, let rest) = list {
        syms.append(try self.symbology(from: symbology))
        list = rest
      }
      request.symbologies = syms
    } else {
      request.symbologies = self.defaultSymbologies
    }
    // Support from iOS 17 on
    // if let coalesce {
    //  request.coalesceCompositeSymbologies = coalesce.isTrue
    // }
    try requestHandler.perform([request])
    return .object(result)
  }
  
  private func supportedClassIdent() throws -> Expr {
    var res: Exprs = []
    for identifier in try VNClassifyImageRequest().supportedIdentifiers().sorted() {
      res.append(.makeString(identifier))
    }
    return .makeList(res)
  }
  
  private func classifyImage(expr: Expr, area: Expr?, confidence: Expr?) throws -> Expr {
    let (_, cgImage) = try self.bitmap(from: expr)
    let requestHandler = VNImageRequestHandler(cgImage: cgImage)
    let result = Future(external: false)
    let context = self.context
    let minimumConfidence = confidence == nil ? 0.0 : try confidence!.asDouble(coerce: true)
    let request = VNClassifyImageRequest { (request: VNRequest, error: Error?) in
      do {
        if let error = error {
          _ = try result.setResult(in: context, to: .error(RuntimeError.os(error)), raise: true)
        } else if let observations = request.results {
          var res: Exprs = []
          for observation in observations {
            if let obs = observation as? VNClassificationObservation,
               Double(obs.confidence) > minimumConfidence {
              res.append(.pair(.makeNumber(Double(obs.confidence)),
                               .pair(.makeString(obs.identifier), .null)))
            }
          }
          _ = try result.setResult(in: context, to: .makeList(res), raise: false)
        } else {
          _ = try result.setResult(in: context, to: .false, raise: false)
        }
      } catch let error {
        do {
          _ = try result.setResult(in: context,
                                   to: .error(RuntimeError.eval(.unableToReturnResultViaFuture,
                                                                .object(result),
                                                                .error(RuntimeError.os(error)))),
                                   raise: true)
        } catch {}
      }
    }
    if let area, area.isTrue {
      guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                       .pair(.flonum(let w), .flonum(let h))) = area else {
        throw RuntimeError.eval(.invalidRect, area)
      }
      request.regionOfInterest = CGRect(x: x, y: y, width: w, height: h)
    }
    try requestHandler.perform([request])
    return .object(result)
  }
}

public struct RecognizedText: CustomExpr {
  public static let type = Type.objectType(Symbol(uninterned: "recognized-text"))
  
  public let recognized: VNRecognizedText
  public let width: Double
  public let height: Double
  
  public var type: Type {
    return Self.type
  }
  
  public var tagString: String {
    return "recognized-text \(self.identityString): confidence=\(self.recognized.confidence) " +
           "text=\"\(self.recognized.string.truncated(limit: 32))\""
  }
  
  public var identity: UInt {
    return UInt(bitPattern: ObjectIdentifier(self.recognized))
  }
  
  public var identityString: String {
    return String(self.identity, radix: 16)
  }
  
  public var hash: Int {
    return self.recognized.hash
  }
  
  public func equals(to expr: Expr) -> Bool {
    guard case .object(let obj) = expr,
          let other = obj as? RecognizedText else {
      return false
    }
    return self.recognized === other.recognized
  }
  
  public func unpack(in context: Context) -> Exprs {
    return [.makeString(self.identityString),
            .makeNumber(Double(self.recognized.confidence)),
            .makeString(self.recognized.string.truncated(limit: 32))]
  }
}
