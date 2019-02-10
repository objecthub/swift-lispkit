//
//  Color.swift
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
/// Representation of a RGB color.
///
public struct Color {

  /// The color space for colors represented by this struct
  public static var colorSpaceName: NSColorSpaceName {
    return NSColorSpaceName.deviceRGB
  }

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

  /// The corresponding `NSColor` object
  public var nsColor: NSColor {
    return NSColor(calibratedRed: CGFloat(self.red),
                   green: CGFloat(self.green),
                   blue: CGFloat(self.blue),
                   alpha: CGFloat(self.alpha))
  }

  /// Returns an array of `NSColor` objects for a given array of `Color` structs
  public static func nsColorArray(_ colors: [Color]) -> [NSColor] {
    var res: [NSColor] = []
    for color in colors {
      res.append(color.nsColor)
    }
    return res
  }
}
