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

public class Port: Reference, CustomStringConvertible {
  
  public enum Kind {
    case TextInputPort(TextInput)
    case TextOutputPort(TextOutput)
    case BinaryInputPort(BinaryInput)
    case BinaryOutputPort(BinaryOutput)
  }
  
  public let kind: Kind
  public var isOpen: Bool = true
  
  public init(input: TextInput) {
    self.kind = .TextInputPort(input)
  }
  
  public init(output: TextOutput) {
    self.kind = .TextOutputPort(output)
  }
  
  public init(input: BinaryInput) {
    self.kind = .BinaryInputPort(input)
  }
  
  public init(output: BinaryOutput) {
    self.kind = .BinaryOutputPort(output)
  }
  
  private init(_ kind: Kind) {
    self.kind = kind
  }
  
  public var isBinaryPort: Bool {
    switch self.kind {
      case .BinaryInputPort(_), .BinaryOutputPort(_):
        return true
      default:
        return false
    }
  }
  
  public var isTextualPort: Bool {
    switch self.kind {
      case .TextInputPort(_), .TextOutputPort(_):
        return true
      default:
        return false
    }
  }
  
  public var isInputPort: Bool {
    switch self.kind {
      case .TextInputPort(_), .BinaryInputPort(_):
        return true
      default:
        return false
    }
  }
  
  public var isOutputPort: Bool {
    switch self.kind {
      case .TextOutputPort(_), .BinaryOutputPort(_):
        return true
      default:
        return false
    }
  }
  
  public var outputString: String? {
    guard case .TextOutputPort(let output) = self.kind else {
      return nil
    }
    return output.currentBuffer
  }
  
  public var outputBinary: [UInt8]? {
    guard case .BinaryOutputPort(let output) = self.kind else {
      return nil
    }
    return output.currentBuffer
  }
  
  public var url: NSURL? {
    switch self.kind {
      case .TextInputPort(let input):
        return input.url
      case .TextOutputPort(let output):
        return output.url
      case .BinaryInputPort(let input):
        return input.url
      case .BinaryOutputPort(let output):
        return output.url
    }
  }
  
  public var typeDescription: String {
    return (self.isTextualPort ? "text-" : "binary-") +
           (self.isInputPort ? "input-port" : "output-port")
  }
  
  public var identDescription: String {
    if let url = self.url {
      return url.path ?? url.absoluteString
    } else {
      return self.isBinaryPort ? "bytevector" : "string"
    }
  }
  
  public var description: String {
    return "\(self.typeDescription):\(self.identDescription)"
  }
  
  public func close() {
    switch self.kind {
      case .TextInputPort(let input):
        input.close()
      case .TextOutputPort(let output):
        output.close()
      case .BinaryInputPort(let input):
        input.close()
      case .BinaryOutputPort(let output):
        output.close()
    }
  }
}

