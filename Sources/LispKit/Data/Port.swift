//
//  Port.swift
//  LispKit
//
//  Created by Matthias Zenger on 04/06/2016.
//  Copyright © 2016 ObjectHub. All rights reserved.
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

open class Port: Reference, CustomStringConvertible {
  
  public enum Kind {
    case textInputPort(TextInput)
    case textOutputPort(TextOutput)
    case binaryInputPort(BinaryInput)
    case binaryOutputPort(BinaryOutput)
  }
  
  public let kind: Kind
  open var isOpen: Bool = true
  
  public init(input: TextInput) {
    self.kind = .textInputPort(input)
  }
  
  public init(output: TextOutput) {
    self.kind = .textOutputPort(output)
  }
  
  public init(input: BinaryInput) {
    self.kind = .binaryInputPort(input)
  }
  
  public init(output: BinaryOutput) {
    self.kind = .binaryOutputPort(output)
  }
  
  private init(_ kind: Kind) {
    self.kind = kind
  }
  
  open var isBinaryPort: Bool {
    switch self.kind {
      case .binaryInputPort(_), .binaryOutputPort(_):
        return true
      default:
        return false
    }
  }
  
  open var isTextualPort: Bool {
    switch self.kind {
      case .textInputPort(_), .textOutputPort(_):
        return true
      default:
        return false
    }
  }
  
  open var isInputPort: Bool {
    switch self.kind {
      case .textInputPort(_), .binaryInputPort(_):
        return true
      default:
        return false
    }
  }
  
  open var isOutputPort: Bool {
    switch self.kind {
      case .textOutputPort(_), .binaryOutputPort(_):
        return true
      default:
        return false
    }
  }
  
  open var outputString: String? {
    guard case .textOutputPort(let output) = self.kind else {
      return nil
    }
    return output.currentBuffer
  }
  
  open var outputBinary: [UInt8]? {
    guard case .binaryOutputPort(let output) = self.kind else {
      return nil
    }
    return output.currentBuffer
  }
  
  open var url: URL? {
    switch self.kind {
      case .textInputPort(let input):
        return input.url
      case .textOutputPort(let output):
        return output.url
      case .binaryInputPort(let input):
        return input.url
      case .binaryOutputPort(let output):
        return output.url
    }
  }
  
  override open var typeDescription: String {
    return (self.isTextualPort ? "text-" : "binary-") +
           (self.isInputPort ? "input-port" : "output-port")
  }
  
  open var identDescription: String {
    if let url = self.url {
      return url.path
    } else {
      return self.isBinaryPort ? "bytevector" : "string"
    }
  }
  
  open var description: String {
    return "\(self.typeDescription):\(self.identDescription)"
  }
  
  open func close() {
    switch self.kind {
      case .textInputPort(let input):
        input.close()
      case .textOutputPort(let output):
        output.close()
      case .binaryInputPort(let input):
        input.close()
      case .binaryOutputPort(let output):
        output.close()
    }
    self.isOpen = false
  }
}
