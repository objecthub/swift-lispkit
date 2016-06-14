//
//  Port.swift
//  LispKit
//
//  Created by Matthias Zenger on 04/06/2016.
//  Copyright © 2016 ObjectHub. All rights reserved.
//

import Foundation

public class Port: Reference, CustomStringConvertible {
  
  private enum Kind {
    case ConsoleInputPort
    case ConsoleOutputPort
    case TextInputPort(TextInput)
    case TextOutputPort(TextOutput)
    case BinaryInputPort(BinaryInput)
    case BinaryOutputPort(BinaryOutput)
  }
  
  private let kind: Kind
  var isOpen: Bool = true
  
  public static let consoleInput: Port = Port(.ConsoleInputPort)
  public static let consoleOutput: Port = Port(.ConsoleOutputPort)
  
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
      case .TextInputPort(_), .TextOutputPort(_), .ConsoleInputPort, .ConsoleOutputPort:
        return true
      default:
        return false
    }
  }
  
  public var isInputPort: Bool {
    switch self.kind {
      case .ConsoleInputPort, .TextInputPort, .BinaryInputPort:
        return true
      default:
        return false
    }
  }
  
  public var isOutputPort: Bool {
    switch self.kind {
      case .ConsoleOutputPort, .TextOutputPort(_), .BinaryOutputPort(_):
        return true
      default:
        return false
    }
  }
  
  public var isConsolePort: Bool {
    switch self.kind {
      case .ConsoleInputPort, .ConsoleOutputPort:
        return true
      default:
        return false
    }
  }
  
  public var url: NSURL? {
    switch self.kind {
      case .ConsoleInputPort, .ConsoleOutputPort:
        return nil
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
    if self.isConsolePort {
      return "console"
    } else if let url = self.url {
      return url.path ?? url.absoluteString
    } else {
      return self.isBinaryPort ? "bytevector" : "string"
    }
  }
  
  public var description: String {
    return self.typeDescription + ":" + self.identDescription
  }
}

