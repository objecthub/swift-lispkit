//
//  TextOutputTarget.swift
//  LispKit
//
//  Created by Matthias Zenger on 20/06/2016.
//  Copyright © 2016-2017 ObjectHub. All rights reserved.
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
