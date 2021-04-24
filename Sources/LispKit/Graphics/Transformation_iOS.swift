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
import CoreGraphics
import UIKit

///
/// A `Transformation` object represents an affine graphics coordinate transformation.
/// Supported are combinations of coordinate translations, scaling, and rotations.
/// Internally, a transformation is represented by a transformation matrix. There is also
/// functionality available to invert this matrix (= undo the transformation).
///
public final class Transformation: NativeObject {

  /// Type representing transformations.
  public static let type = Type.objectType(Symbol(uninterned: "transformation"))
  
  /// The identify transformation (= no mapping).
  public static let identity = Transformation()
  
  /// The internal representation using an `CGAffineTransform` struct.
  public private(set) var affineTransform: CGAffineTransform
  
  /// Initializer
  public init(_ affineTransform: CGAffineTransform) {
    self.affineTransform = affineTransform
  }
  
  /// Initializer
  public init(_ transform: Transformation? = nil) {
    if let transform = transform {
      self.affineTransform = transform.affineTransform
    } else {
      self.affineTransform = CGAffineTransform()
    }
  }

  /// Return native object type.
  public override var type: Type {
    return Self.type
  }
  
  /// Shift coordinates by x/y.
  public func translate(x: Double, y: Double) {
    self.affineTransform = self.affineTransform.translatedBy(x: CGFloat(x), y: CGFloat(y))
  }
  
  /// Scale coordinates by factors x/y
  public func scale(x: Double, y: Double) {
    self.affineTransform = self.affineTransform.scaledBy(x: CGFloat(x), y: CGFloat(y))
  }
  
  /// Rotate coordinates by a given angle (around the origin)
  public func rotate(angle: Double) {
    self.affineTransform = self.affineTransform.rotated(by: CGFloat(angle))
  }
  
  /// Append a transformation.
  public func append(_ tf: Transformation) {
    self.affineTransform = self.affineTransform.concatenating(tf.affineTransform)
  }
  
  /// Prepend a transformation.
  public func prepend(_ tf: Transformation) {
    self.affineTransform = tf.affineTransform.concatenating(self.affineTransform)
  }
  
  /// Inver the transformation matrix
  public func invert() {
    self.affineTransform = self.affineTransform.inverted()
  }
}
