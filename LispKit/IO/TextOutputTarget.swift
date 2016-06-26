//
//  TextOutputTarget.swift
//  LispKit
//
//  Created by Matthias Zenger on 20/06/2016.
//  Copyright © 2016 ObjectHub. All rights reserved.
//

import Foundation


public protocol TextOutputTarget {
  func flush(completely: Bool) -> Bool
  func writeString(str: String) -> Bool
}

public struct UTF8EncodedTarget: TextOutputTarget {
  private let output: BinaryOutput
  
  public init(output: BinaryOutput) {
    self.output = output
  }

  public func flush(completely: Bool = false) -> Bool {
    return self.output.flush(completely)
  }
  
  public func writeString(str: String) -> Bool {
    for byte in str.utf8 {
      guard self.output.write(byte) else {
        return false
      }
    }
    return true
  }
}
