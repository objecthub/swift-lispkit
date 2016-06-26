//
//  Console.swift
//  LispKit
//
//  Created by Matthias Zenger on 05/05/2016.
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

///
/// `Console` defines the protocol for interacting with the console window. It provides
/// methods to set the console window status, to write strings as well as to read strings.
///
public protocol Console: TextInputSource, TextOutputTarget {
  
  /// Prints the given string into the console window.
  func print(str: String)
  
  /// Reads a string from the console window.
  func read() -> String?
}

///
/// Implement `TextInputSource` and `TextOutputTarget` functionality via the `Console`
/// methods.
///
extension Console {
  
  /// The console always blocks before providing a new string.
  public var nextReadMightBlock: Bool {
    return true
  }
  
  /// Read the next string from the console. This operation always blocks.
  public func readString() -> String? {
    return self.read()
  }
  
  /// `flush` always succeeds.
  public func flush(completely: Bool = false) -> Bool {
    return true
  }
  
  /// Print the given string to the console.
  public func writeString(str: String) -> Bool {
    self.print(str)
    return true
  }
}

///
/// Struct `CommandLineConsole` implements `Console` with Swift's built in `print` and
/// `readLine` functions.
///
public struct CommandLineConsole: Console {
  
  public init() {}
  
  /// Prints the given string into the console window.
  public func print(str: String) {
    Swift.print(str, separator: "", terminator: "")
  }
  
  /// Reads a string from the console window.
  public func read() -> String? {
    return Swift.readLine(stripNewline: false)
  }
}

