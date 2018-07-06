//
//  Transformation.swift
//  LispKit
//
//  Created by Matthias Zenger on 30/06/2018.
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
/// A `Transformation` object represents an affine graphics coordinate transformation.
/// Supported are combinations of coordinate translations, scaling, and rotations.
/// Internally, a transformation is represented by a transformation matrix. There is also
/// functionality available to invert this matrix (= undo the transformation).
///
public final class Transformation: Reference {
  
  /// The identify transformation (= no mapping).
  public static let identity = Transformation()
  
  /// The internal representation using an `AffineTransform` object.
  public private(set) var affineTransform: AffineTransform
  
  /// Initializer
  public init(_ affineTransform: AffineTransform) {
    self.affineTransform = affineTransform
  }
  
  /// Initializer
  public init(_ transform: Transformation? = nil) {
    if let transform = transform {
      self.affineTransform = transform.affineTransform
    } else {
      self.affineTransform = AffineTransform()
    }
  }
  
  /// Name of this reference type
  public override var typeDescription: String {
    return "transformation"
  }
  
  /// Shift coordinates by x/y.
  public func translate(x: Double, y: Double) {
    self.affineTransform.translate(x: CGFloat(x), y: CGFloat(y))
  }
  
  /// Scale coordinates by factors x/y
  public func scale(x: Double, y: Double) {
    self.affineTransform.scale(x: CGFloat(x), y: CGFloat(y))
  }
  
  /// Rotate coordinates by a given angle (around the origin)
  public func rotate(angle: Double) {
    self.affineTransform.rotate(byRadians: CGFloat(angle))
  }
  
  /// Append a transformation.
  public func append(_ tf: Transformation) {
    self.affineTransform.append(tf.affineTransform)
  }
  
  /// Prepend a transformation.
  public func prepend(_ tf: Transformation) {
    self.affineTransform.prepend(tf.affineTransform)
  }
  
  /// Inver the transformation matrix
  public func invert() {
    self.affineTransform.invert()
  }
}
