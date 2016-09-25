//
//  TextOutputTarget.swift
//  LispKit
//
//  Created by Matthias Zenger on 20/06/2016.
//  Copyright © 2016 ObjectHub. All rights reserved.
//

import Foundation


public protocol TextOutputTarget {
  func flush(_ completely: Bool) -> Bool
  func writeString(_ str: String) -> Bool
}

public struct UTF8EncodedTarget: TextOutputTarget {
  private let output: BinaryOutput
  
  public init(output: BinaryOutput) {
    self.output = output
  }

  @discardableResult public func flush(_ completely: Bool = false) -> Bool {
    return self.output.flush(completely)
  }
  
  public func writeString(_ str: String) -> Bool {
    for byte in str.utf8 {
      guard self.output.write(byte) else {
        return false
      }
    }
    return true
  }
}
