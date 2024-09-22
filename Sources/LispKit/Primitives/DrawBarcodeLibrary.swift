//
//  DrawBarcodeLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 20/09/2024.
//  Copyright © 2024 ObjectHub. All rights reserved.
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

public final class DrawBarcodeLibrary: NativeLibrary {
  
  // QR error correction levels
  private let low: Symbol
  private let medium: Symbol
  private let quartile: Symbol
  private let high: Symbol
  
    /// Initialize symbols.
  public required init(in context: Context) throws {
    self.low = context.symbols.intern("low")
    self.medium = context.symbols.intern("medium")
    self.quartile = context.symbols.intern("quartile")
    self.high = context.symbols.intern("high")
    try super.init(in: context)
  }
  
    /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "draw", "barcode"]
  }
  
    /// Dependencies of the library.
  public override func dependencies() {
  }
  
    /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("code128-image", self.code128Image))
    self.define(Procedure("qr-code-image", self.qrCodeImage))
    self.define(Procedure("aztec-code-image", self.aztecCodeImage))
    self.define(Procedure("pdf417-code-image", self.pdf417CodeImage))
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
  
  private func code128Image(str: Expr, ht: Expr, args: Arguments) throws -> Expr {
    guard let (pad, scale, color, background, dpi) = args.optional(.flonum(0.0),
                                                                   .flonum(1.0),
                                                                   .false,
                                                                   .false,
                                                                   .flonum(72.0)) else {
      throw RuntimeError.argumentCount(of: "code128-image",
                                       min: 2,
                                       max: 7,
                                       args: .pair(str, .pair(ht, .makeList(args))))
    }
    guard let data = try str.asString().data(using: .ascii, allowLossyConversion: true) else {
      return .false
    }
    let padding = pad.isTrue ? try pad.asDouble(coerce: true) : 0.0
    let height = try ht.asDouble(coerce: true)
    let scaleDpi = try dpi.asDouble(coerce: true) / 72.0
    guard scaleDpi > 0.0 && scaleDpi <= 10.0 else {
      throw RuntimeError.range(parameter: 7, of: "code128-image", dpi, min: 0, max: 720)
    }
    guard padding >= 0.0 && padding <= 20.0 else {
      throw RuntimeError.range(parameter: 3, of: "code128-image", pad, min: 0, max: 20)
    }
    guard height >= 1.0 && height <= 500.0 else {
      throw RuntimeError.range(parameter: 2, of: "code128-image", ht, min: 1, max: 500)
    }
    let bc = Code128Barcode(inputMessage: data,
                            inputQuietSpace: NSNumber(floatLiteral: padding),
                            inputBarcodeHeight: NSNumber(floatLiteral: height))
    guard let image = Barcode.generateImage(from: bc,
                                            scaleDpi: scaleDpi,
                                            scale: try scale.asDouble(coerce: true),
                                            color: try self.color(from: color),
                                            background: try self.color(from: background)) else {
      return .false
    }
    return .object(image)
  }
  
  private func qrCodeImage(str: Expr, args: Arguments) throws -> Expr {
    guard let (corr, scale, color, background, dpi) = args.optional(.symbol(self.medium),
                                                                    .flonum(2.0),
                                                                    .false,
                                                                    .false,
                                                                    .flonum(72.0)) else {
      throw RuntimeError.argumentCount(of: "qr-code-image",
                                       min: 1,
                                       max: 6,
                                       args: .pair(str, .makeList(args)))
    }
    guard let data = try str.asString().data(using: .isoLatin1, allowLossyConversion: true) else {
      return .false
    }
    let corrLevel: QRCode.QRCorrectionLevel
    if corr.isTrue {
      switch try corr.asSymbol() {
        case self.low:
          corrLevel = .l
        case self.medium:
          corrLevel = .m
        case self.quartile:
          corrLevel = .q
        case self.high:
          corrLevel = .h
        default:
          return .false
      }
    } else {
      corrLevel = .m
    }
    let scaleDpi = try dpi.asDouble(coerce: true) / 72.0
    guard scaleDpi > 0.0 && scaleDpi <= 10.0 else {
      throw RuntimeError.range(parameter: 6, of: "qr-code-image", dpi, min: 0, max: 720)
    }
    let bc = QRCode(inputMessage: data, inputCorrectionLevel: corrLevel)
    guard let image = Barcode.generateImage(from: bc,
                                            scaleDpi: scaleDpi,
                                            scale: try scale.asDouble(coerce: true),
                                            color: try self.color(from: color),
                                            background: try self.color(from: background)) else {
      return .false
    }
    return .object(image)
  }
  
  private func aztecCodeImage(str: Expr, args: Arguments) throws -> Expr {
    guard let (corr, compact, scale, color, background, dpi) = args.optional(.flonum(23.0),
                                                                             .null,
                                                                             .flonum(1.0),
                                                                             .false,
                                                                             .false,
                                                                             .flonum(72.0)) else {
      throw RuntimeError.argumentCount(of: "aztec-code-image",
                                       min: 1,
                                       max: 7,
                                       args: .pair(str, .makeList(args)))
    }
    guard let data = try str.asString().data(using: .isoLatin1, allowLossyConversion: true) else {
      return .false
    }
    let corrLevel = corr.isTrue ? try corr.asDouble(coerce: true) : 23.0
    let compactRep = compact.isNull ? nil : compact.isTrue
    let scaleDpi = try dpi.asDouble(coerce: true) / 72.0
    guard scaleDpi > 0.0 && scaleDpi <= 10.0 else {
      throw RuntimeError.range(parameter: 7, of: "aztec-code-image", dpi, min: 0, max: 720)
    }
    guard corrLevel >= 5.0 && corrLevel <= 95.0 else {
      throw RuntimeError.range(parameter: 2, of: "aztec-code-image", corr, min: 0, max: 20)
    }
    let bc = AztecBarcode(inputMessage: data,
                          inputCorrectionLevel: NSNumber(floatLiteral: corrLevel),
                          inputLayers: nil,
                          inputCompactStyle: compactRep)
    guard let image = Barcode.generateImage(from: bc,
                                            scaleDpi: scaleDpi,
                                            scale: try scale.asDouble(coerce: true),
                                            color: try self.color(from: color),
                                            background: try self.color(from: background)) else {
      return .false
    }
    return .object(image)
  }
  
  private func pdf417CodeImage(str: Expr, args: Arguments) throws -> Expr {
    guard let (config, scale, color, background, dpi) = args.optional(.null,
                                                                      .flonum(1.0),
                                                                      .false,
                                                                      .false,
                                                                      .flonum(72.0)) else {
      throw RuntimeError.argumentCount(of: "pdf417-code-image",
                                       min: 1,
                                       max: 6,
                                       args: .pair(str, .makeList(args)))
    }
    guard let data = try str.asString().data(using: .isoLatin1, allowLossyConversion: true) else {
      return .false
    }
    let scaleDpi = try dpi.asDouble(coerce: true) / 72.0
    guard scaleDpi > 0.0 && scaleDpi <= 10.0 else {
      throw RuntimeError.range(parameter: 7, of: "pdf417-code-image", dpi, min: 0, max: 720)
    }
    var bc = PDF417Barcode(inputMessage: data)
    var list = config
    while case .pair(.pair(let ident, let value), let rest) = list {
      let key: String
      switch ident {
        case .symbol(let sym):
          key = sym.identifier
        case .string(let str):
          key = str as String
        default:
          throw RuntimeError.type(ident, expected: [.strType, .symbolType])
      }
      switch key {
        case "min-width":
          bc.inputMinWidth = NSNumber(floatLiteral: try value.asDouble(coerce: true))
        case "max-width":
          bc.inputMaxWidth = NSNumber(floatLiteral: try value.asDouble(coerce: true))
        case "min-height":
          bc.inputMinHeight = NSNumber(floatLiteral: try value.asDouble(coerce: true))
        case "max-height":
          bc.inputMaxHeight = NSNumber(floatLiteral: try value.asDouble(coerce: true))
        case "columns":
          bc.inputDataColumns = NSNumber(integerLiteral: try value.asInt(above: 0, below: 31))
        case "rows":
          bc.inputRows = NSNumber(integerLiteral: try value.asInt(above: 0, below: 91))
        case "preferred-aspect-ratio":
          bc.inputPreferredAspectRatio = NSNumber(floatLiteral: try value.asDouble(coerce: true))
        case "compaction-mode":
          bc.inputCompactionMode = NSNumber(integerLiteral: try value.asInt(above: 0, below: 4))
        case "compact-style":
          bc.inputCompactStyle = value.isTrue
        case "correction-level":
          bc.inputCorrectionLevel = NSNumber(integerLiteral: try value.asInt(above: 0, below: 9))
        case "always-specify-compaction":
          bc.inputAlwaysSpecifyCompaction = value.isTrue
        default:
          throw RuntimeError.eval(.unsupportedBarcodeSetting, ident)
      }
      list = rest
    }
    guard list.isNull else {
      throw RuntimeError.type(list, expected: [.nullType])
    }
    guard let image = Barcode.generateImage(from: bc,
                                            scaleDpi: scaleDpi,
                                            scale: try scale.asDouble(coerce: true),
                                            color: try self.color(from: color),
                                            background: try self.color(from: background)) else {
      return .false
    }
    return .object(image)
  }
}
