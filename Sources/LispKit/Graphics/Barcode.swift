//
//  Barcode.swift
//  LispKit
//
//  Created by Matthias Zenger on 20/09/2024.
//  Copyright © 2024 ObjectHub. All rights reserved.
//

import Foundation
#if os(iOS) || os(watchOS) || os(tvOS)
import UIKit
#elseif os(macOS)
import AppKit
#endif

protocol Barcodable {
  var name: String { get }
  var properties: [String : Any] { get }
}

struct Barcode {
  
  static func generateImage(from barcode: Barcodable,
                            scaleDpi: CGFloat = 1.0,
                            scale: CGFloat,
                            color: NativeColor?,
                            background: NativeColor?) -> NativeImage? {
    return Self.generateImage(from: barcode,
                              scaleDpi: scaleDpi,
                              scaleX: scale,
                              scaleY: scale,
                              color: color,
                              background: background)
  }
  
  static func generateImage(from barcode: Barcodable,
                            scaleDpi: CGFloat = 1.0,
                            scaleX: CGFloat,
                            scaleY: CGFloat,
                            color: NativeColor?,
                            background: NativeColor?) -> NativeImage? {
    if let filter = CIFilter(name: barcode.name) {
      filter.setValuesForKeys(barcode.properties)
      if var image = filter.outputImage {
        if let color {
          if let background {
            image = self.color(image: image, color: color, background: background) ?? image
          } else {
            image = self.tinted(image: self.transparent(image: image), color: color) ?? image
          }
        } else if let background {
          image = self.tinted(image: self.alphaBlack(image: image), color: background) ?? image
        }
        return self.transform(image: image, with: CGAffineTransform(scaleX: scaleDpi * scaleX, y: scaleDpi * scaleY), scale: scaleDpi)
      }
    }
    return nil
  }
  
  #if os(iOS) || os(watchOS) || os(tvOS)
  private static func transform(image: CIImage,
                                with transform: CGAffineTransform,
                                scale: CGFloat) -> NativeImage? {
    let ciImage = image.transformed(by: transform)
    let context = CIContext(options: nil)
    if let cgImage = context.createCGImage(ciImage, from: ciImage.extent) {
      return NativeImage(UIImage(cgImage: cgImage, scale: scale, orientation: .up))
    } else {
      return NativeImage(UIImage(ciImage: ciImage))
    }
  }
  #elseif os(macOS)
  private static func transform(image: CIImage,
                                with transform: CGAffineTransform,
                                scale: CGFloat) -> NativeImage? {
    let ciImage = image.transformed(by: transform)
    let rep = NSBitmapImageRep(ciImage: ciImage)
    rep.size = NSSize(width: rep.size.width / scale, height: rep.size.height / scale)
    let image = NSImage(size: rep.size)
    image.addRepresentation(rep)
    return NativeImage(image)
  }
  #endif
  
  private static func color(image: CIImage,
                            color: NativeColor,
                            background: NativeColor) -> CIImage? {
    if let colorFilter = CIFilter(name: "CIFalseColor") {
      colorFilter.setDefaults()
      colorFilter.setValue(image, forKey: "inputImage")
      colorFilter.setValue(CIColor(cgColor: color.cgColor), forKey: "inputColor0")
      colorFilter.setValue(CIColor(cgColor: background.cgColor), forKey: "inputColor1")
      return colorFilter.outputImage
    }
    return nil
  }
  
  private static func invert(image: CIImage) -> CIImage? {
    if let invertedColorFilter = CIFilter(name: "CIColorInvert") {
      invertedColorFilter.setValue(image, forKey: "inputImage")
      return invertedColorFilter.outputImage
    }
    return nil
  }
  
  private static func alphaBlack(image: CIImage) -> CIImage? {
    if let blackTransparentFilter = CIFilter(name: "CIMaskToAlpha") {
      blackTransparentFilter.setValue(image, forKey: "inputImage")
      return blackTransparentFilter.outputImage
    }
    return nil
  }
  
  private static func transparent(image: CIImage) -> CIImage? {
    if let invertedImage = self.invert(image: image) {
      return self.alphaBlack(image: invertedImage)
    }
    return nil
  }
  
  private static func tinted(image transparentImage: CIImage?, color: NativeColor) -> CIImage? {
    if let filter = CIFilter(name: "CIMultiplyCompositing"),
       let colorFilter = CIFilter(name: "CIConstantColorGenerator") {
      colorFilter.setValue(CIColor(color: color), forKey: kCIInputColorKey)
      if let colorImage = colorFilter.outputImage {
        filter.setValue(colorImage, forKey: kCIInputImageKey)
        filter.setValue(transparentImage, forKey: kCIInputBackgroundImageKey)
        return filter.outputImage
      }
    }
    return nil
  }
}

/// Generates an Aztec code (two-dimensional barcode) from input data.
///
/// Generates an output image representing the input data according to the ISO/IEC 24778:2008
/// standard. The width and height of each module (square dot) of the code in the output image
/// is one pixel. To create an Aztec code from a string or URL, convert it to an NSData object
/// using the NSISOLatin1StringEncoding string encoding. The output image also includes two
/// pixels of padding on each side (for example, a 15 x 15 code creates a 19 x 19 image).
struct AztecBarcode: Barcodable {
  let name = "CIAztecCodeGenerator"
  
  /// Force a compact style Aztec code to true or false. Set to nil for automatic.
  /// A Boolean value that determines whether to use the compact or full-size Aztec
  /// barcode format. The compact format can store up to 44 bytes of message data
  /// (including data added for correction) in up to 4 layers, producing a barcode
  /// image sized between 15 x 15 and 27 x 27 pixels. The full-size format can store
  /// up to 1914 bytes of message data (including correction) in up to 32 layers,
  /// producing a barcode image sized no larger than 151 x 151 pixels.
  let inputCompactStyle: Bool?
  
  /// Aztec error correction value between 5 and 95
  /// The percentage of redundancy to add to the message data in the barcode encoding.
  /// A higher correction level allows a barcode to be correctly read even when partially damaged.
  let inputCorrectionLevel: NSNumber
  
  /// Aztec layers value between 1 and 32. Set to nil for automatic.
  /// The number of concentric squares (with a width of two pixels each) encoding the
  /// barcode data. When this parameter is set to zero, Core Image automatically determines
  /// the appropriate number of layers to encode the message at the specified correction level.
  let inputLayers: NSNumber?
  
  /// The message to encode in the Aztec Barcode
  /// The data to be encoded as an Aztec code. An NSData object whose display name is Message.
  let inputMessage: Data
  
  init(inputMessage: Data,
       inputCorrectionLevel: NSNumber = 5.0,
       inputLayers: NSNumber? = nil,
       inputCompactStyle: Bool? = nil) {
    self.inputCompactStyle = inputCompactStyle
    self.inputCorrectionLevel = inputCorrectionLevel
    self.inputLayers = inputLayers
    self.inputMessage = inputMessage
  }
  
  var properties: [String: Any] {
    var response: [String: Any] = [:]
    if let inputCompactStyle = inputCompactStyle {
      response["inputCompactStyle"] = inputCompactStyle
    }
    response["inputCorrectionLevel"] = inputCorrectionLevel
    if let inputLayers = inputLayers {
      response["inputLayers"] = inputLayers
    }
    response["inputMessage"] = NSData(data: inputMessage)
    return response
  }
}

/// Generates a Quick Response code (two-dimensional barcode) from input data.
///
/// Generates an output image representing the input data according to the ISO/IEC 18004:2006
/// standard. The width and height of each module (square dot) of the code in the output image
/// is one point. To create a QR code from a string or URL, convert it to an NSData object
/// using the NSISOLatin1StringEncoding string encoding.
///
/// The inputCorrectionLevel parameter controls the amount of additional data encoded in the
/// output image to provide error correction. Higher levels of error correction result in
/// larger output images but allow larger areas of the code to be damaged or obscured without.
/// There are four possible correction modes (with corresponding error resilience levels):
///   L: 7%, M: 15%, Q: 25%, H: 30%
struct QRCode: Barcodable {
  enum QRCorrectionLevel: String {
    case l
    case m
    case q
    case h
  }
  
  let name = "CIQRCodeGenerator"
  
  /// The message to encode in the QR Code
  /// The data to be encoded as a QR code. An NSData object whose display name is Message.
  let inputMessage: Data
  
  /// QR Code correction level L, M, Q, or H.
  /// A single letter specifying the error correction format. An NSString object whose
  /// display name is CorrectionLevel.
  let inputCorrectionLevel: QRCorrectionLevel
  
  var properties: [String: Any] {
    [
      "inputCorrectionLevel": inputCorrectionLevel.rawValue.uppercased(),
      "inputMessage": inputMessage as NSData
    ]
  }
}

/// Generates a PDF417 code (two-dimensional barcode) from input data.
///
/// Generates an output image representing the input data according to the ISO 15438 standard.
/// PDF417 codes are commonly used in postage, package tracking, personal identification
/// documents, and coffeeshop membership cards. The width and height of each module (square dot)
/// of the code in the output image is one point. To create a PDF417 code from a string or URL,
/// convert it to an NSData object using the NSISOLatin1StringEncoding string encoding.
struct PDF417Barcode: Barcodable {
  let name = "CIPDF417BarcodeGenerator"
  
  /// The message to encode in the PDF417 Barcode
  /// The data to be encoded as a barcode. An NSData object whose display name is Message.
  let inputMessage: Data
  
  /// The minimum width of the generated barcode in pixels. (Number. Min: 56.0 Max: 583.0)
  /// The minimum width of the barcode’s data area, in pixels. An NSNumber object whose
  /// display name is MinWidth.
  var inputMinWidth: NSNumber?
  
  /// The maximum width of the generated barcode in pixels. (Number. Min: 56.0 Max: 583.0)
  /// The maximum width of the barcode’s data area, in pixels. An NSNumber object whose
  /// display name is MaxWidth.
  var inputMaxWidth: NSNumber?
  
  /// The minimum height of the generated barcode in pixels. (Number. Min: 13.0 Max: 283.0)
  /// The minimum height of the barcode’s data area, in pixels. An NSNumber object whose
  /// display name is MinHeight.
  var inputMinHeight: NSNumber?
  
  /// The maximum height of the generated barcode in pixels. (Number. Min: 13.0 Max: 283.0)
  /// The maximum height of the barcode’s data area, in pixels. An NSNumber object whose
  /// display name is MaxHeight.
  var inputMaxHeight: NSNumber?
  
  /// The number of data columns in the generated barcode (Number. Min: 1.0 Max: 30.0)
  /// The number of data columns in the generated code. If zero, the generator uses a number
  /// of columns based on the width, height, and aspect ratio. An NSNumber object whose
  /// display name is DataColumns.
  var inputDataColumns: NSNumber?
  
  /// The number of rows in the generated barcode (Number. Min: 3.0 Max: 90.0)
  /// The number of data rows in the generated code. If zero, the generator uses a number
  /// of rows based on the width, height, and aspect ratio. An NSNumber object whose display
  /// name is Rows.
  var inputRows: NSNumber?
  
  /// The preferred aspect ratio of the generated barcode (Number. Min: 0.0)
  /// The preferred ratio of width over height for the generated barcode. The generator
  /// approximates this with an actual aspect ratio based on the data and other parameters
  /// you specify. An NSNumber object whose display name is PreferredAspectRatio.
  var inputPreferredAspectRatio: NSNumber?
  
  /// The compaction mode of the generated barcode. (Number. Min: 0.0 Max: 3.0)
  /// An option that determines which method the generator uses to compress data.
  ///   Automatic. The generator automatically chooses a compression method.
  ///              This option is the default.
  ///   Numeric.   Valid only when the message is an ASCII-encoded string of digits,
  ///              achieving optimal compression for that type of data.
  ///   Text.      Valid only when the message is all ASCII-encoded alphanumeric and
  ///              punctuation characters, achieving optimal compression for that type of data.
  ///   Byte.      Valid for any data, but least compact.
  /// An NSNumber object whose display name is CompactionMode.
  var inputCompactionMode: NSNumber?
  
  /// Force a compact style Aztec code to @YES or @NO. Set to nil for automatic.
  /// (Number. Min: 0.0 Max: 1.0)
  /// A Boolean value that determines whether to omit redundant elements to make the
  /// generated barcode more compact. An NSNumber object whose display name is CompactStyle.
  var inputCompactStyle: Bool?
  
  /// The correction level ratio of the generated barcode (Number. Min: 0.0 Max: 8.0)
  /// An integer between 0 and 8, inclusive, that determines the amount of redundancy to
  /// include in the barcode’s data to prevent errors when the barcode is read. If
  /// unspecified, the generator chooses a correction level based on the size of the
  /// message data. An NSNumber object whose display name is CorrectionLevel.
  var inputCorrectionLevel: NSNumber?
  
  /// Force compaction style to @YES or @NO. Set to nil for automatic. (Number. Min: 0.0 Max: 1.0)
  /// A Boolean value that determines whether to include information about the compaction
  /// mode in the barcode even when such information is redundant. (If a PDF417 barcode does
  /// not contain compaction mode information, a reader assumes text-based compaction. Some
  /// barcodes include this information even when using text-based compaction.)
  /// An NSNumber object whose display name is AlwaysSpecifyCompaction.
  var inputAlwaysSpecifyCompaction: Bool?
  
  var properties: [String: Any] {
    var res: [String : Any] = [
      "inputMessage": inputMessage as NSData,
    ]
    if let inputMinWidth {
      res["inputMinWidth"] = inputMinWidth
    }
    if let inputMaxWidth {
      res["inputMaxWidth"] = inputMaxWidth
    }
    if let inputMinHeight {
      res["inputMinHeight"] = inputMinHeight
    }
    if let inputMaxHeight {
      res["inputMaxHeight"] = inputMaxHeight
    }
    if let inputPreferredAspectRatio {
      res["inputPreferredAspectRatio"] = inputPreferredAspectRatio
    }
    if let inputCompactionMode {
      res["inputCompactionMode"] = inputCompactionMode
    }
    if let inputCompactStyle {
      res["inputCompactStyle"] = inputCompactStyle
    }
    if let inputAlwaysSpecifyCompaction {
      res["inputAlwaysSpecifyCompaction"] = inputAlwaysSpecifyCompaction
    }
    if let inputCorrectionLevel {
      res["inputCorrectionLevel"] = inputCorrectionLevel
    }
    if let inputDataColumns {
      res["inputDataColumns"] = inputDataColumns
    }
    if let inputRows {
      res["inputRows"] = inputRows
    }
    return res
  }
}

/// Generates a Code 128 one-dimensional barcode from input data.
///
/// Generates an output image representing the input data according to the ISO/IEC 15417:2007
/// standard. The width of each module (vertical line) of the barcode in the output image is
/// one pixel. The height of the barcode is 32 pixels. To create a barcode from a string or URL,
/// convert it to an NSData object using the NSASCIIStringEncoding string encoding.
struct Code128Barcode: Barcodable {
  let name = "CICode128BarcodeGenerator"
  
  /// The message to encode in the Code 128 Barcode
  /// The data to be encoded as a Code 128 barcode. Must not contain non-ASCII characters.
  /// An NSData object whose display name is Message.
  let inputMessage: Data
  
  /// The number of empty white pixels that should surround the barcode. (Scalar. Min: 0.0 Max: 20.0)
  /// The number of pixels of added white space on each side of the barcode. An NSNumber object
  /// whose attribute type is CIAttributeTypeScalar and whose display name is QuietSpace.
  let inputQuietSpace: NSNumber
  
  /// The height of the generated barcode in pixels. (Scalar. Min: 1.0 Max: 500.0)
  let inputBarcodeHeight: NSNumber
  
  var properties: [String: Any] {
    [
      "inputBarcodeHeight": inputBarcodeHeight,
      "inputQuietSpace": inputQuietSpace,
      "inputMessage": inputMessage as NSData
    ]
  }
}
