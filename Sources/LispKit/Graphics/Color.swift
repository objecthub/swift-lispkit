//
//  Color.swift
//  LispKit
//
//  Created by Matthias Zenger on 06/07/2018.
//  Copyright © 2018-2022 ObjectHub. All rights reserved.
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
#if os(iOS) || os(watchOS) || os(tvOS)
import UIKit
#elseif os(macOS)
import AppKit
#endif

///
/// Representation of a RGB color.
///
public final class Color: NativeObject {

  /// Type representing colors.
  public static let type = Type.objectType(Symbol(uninterned: "color"))

  /// The color space for colors represented by this struct.
  #if os(iOS) || os(watchOS) || os(tvOS)
  public static let colorSpaceName: CGColorSpace = CGColorSpaceCreateDeviceRGB()
  #elseif os(macOS)
  public static let colorSpaceName: NSColorSpaceName = NSColorSpaceName.deviceRGB
  #endif
  
  // Predefined colors
  public static let black = Color(red: 0.0, green: 0.0, blue: 0.0)
  public static let white = Color(red: 1.0, green: 1.0, blue: 1.0)
  public static let red = Color(red: 1.0, green: 0.0, blue: 0.0)
  public static let green = Color(red: 0.0, green: 1.0, blue: 0.0)
  public static let blue = Color(red: 0.0, green: 0.0, blue: 1.0)
  
  // Color components
  public let red: Double
  public let green: Double
  public let blue: Double
  public let alpha: Double
  
  /// Initializer
  public init(red: Double, green: Double, blue: Double, alpha: Double = 1.0) {
    self.red = red
    self.green = green
    self.blue = blue
    self.alpha = alpha
  }
  
  public init?(_ cg: CGColor) {
    guard let model = cg.colorSpace?.model else {
      return nil
    }
    let components = cg.components
    switch model {
      case .rgb:
        self.red = Double(components?[0] ?? 0.0)
        self.green = Double(components?[1] ?? 0.0)
        self.blue = Double(components?[2] ?? 0.0)
        self.alpha = Double(components?[3] ?? 0.0)
      case .monochrome:
        let comp = Double(components?[0] ?? 0.0)
        self.red = comp
        self.green = comp
        self.blue = comp
        self.alpha = Double(components?[1] ?? 0.0)
      default:
        return nil
    }
  }
  
  public init(_ nc: NativeColor) {
    #if os(iOS) || os(watchOS) || os(tvOS)
    var red: CGFloat = 0.0
    var green: CGFloat = 0.0
    var blue: CGFloat = 0.0
    var alpha: CGFloat = 0.0
    nc.getRed(&red, green: &green, blue: &blue, alpha: &alpha)
    self.red = red
    self.green = green
    self.blue = blue
    self.alpha = alpha
    #elseif os(macOS)
    self.red = nc.redComponent
    self.green = nc.greenComponent
    self.blue = nc.blueComponent
    self.alpha = nc.alphaComponent
    #endif
  }
  
  /// Return native object type.
  public override var type: Type {
    return Self.type
  }

  /// Return string representation of native object.
  public override var string: String {
    if self.alpha < 1.0 {
      return "#<color \(self.red) \(self.green) \(self.blue) \(self.alpha)>"
    } else {
      return "#<color \(self.red) \(self.green) \(self.blue)>"
    }
  }
  
  /// Unpack this native object.
  public override func unpack() -> Exprs {
    return [.flonum(self.red),
            .flonum(self.green),
            .flonum(self.blue),
            .flonum(self.alpha)]
  }
  
  /// The corresponding `NSColor` object
  public var nsColor: NativeColor {
    #if os(iOS) || os(watchOS) || os(tvOS)
    return UIColor(red: CGFloat(self.red),
                   green: CGFloat(self.green),
                   blue: CGFloat(self.blue),
                   alpha: CGFloat(self.alpha))
    #elseif os(macOS)
    return NSColor(calibratedRed: CGFloat(self.red),
                   green: CGFloat(self.green),
                   blue: CGFloat(self.blue),
                   alpha: CGFloat(self.alpha))
    #endif
  }
  
  /// Returns an array of `NSColor` objects for a given array of `Color` objects
  public static func nsColorArray(_ colors: [Color]) -> [NativeColor] {
    var res: [NativeColor] = []
    for color in colors {
      res.append(color.nsColor)
    }
    return res
  }
  
  /// Returns an array of `CGColor` objects for a given array of `Color` objects
  #if os(iOS) || os(watchOS) || os(tvOS)
  public static func cgColorArray(_ colors: [Color]) -> [CGColor] {
    var res: [CGColor] = []
    for color in colors {
      res.append(color.nsColor.cgColor)
    }
    return res
  }
  #endif
  
  public override var hash: Int {
    var hasher = Hasher()
    hasher.combine(self.red)
    hasher.combine(self.green)
    hasher.combine(self.blue)
    hasher.combine(self.alpha)
    return hasher.finalize()
  }

  public override func equals(_ obj: NativeObject) -> Bool {
    guard let other = obj as? Color else {
      return false
    }
    return self.red == other.red &&
           self.green == other.green &&
           self.blue == other.blue &&
           self.alpha == other.alpha
  }
}

#if os(iOS) || os(watchOS) || os(tvOS)
public typealias NativeColor = UIColor
#elseif os(macOS)
public typealias NativeColor = NSColor
#endif
