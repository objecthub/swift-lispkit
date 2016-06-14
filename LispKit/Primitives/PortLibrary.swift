//
//  PortLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 09/06/2016.
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


public final class PortLibrary: Library {
  
  public override func export() {
    define(Procedure("port?", isPort))
    define(Procedure("input-port?", isInputPort))
    define(Procedure("output-port?", isOutputPort))
    define(Procedure("textual-port?", isTextualPort))
    define(Procedure("binary-port?", isBinaryPort))
    define(Procedure("input-port-open?", isInputPortOpen))
    define(Procedure("output-port-open?", isOutputPortOpen))
    define(Procedure("open-input-file", openInputFile))
    define(Procedure("open-binary-input-file", openBinaryInputFile))
    define(Procedure("open-output-file", openOutputFile))
    define(Procedure("open-binary-output-file", openBinaryOutputFile))
    define(Procedure("open-input-string", openInputString))
    define(Procedure("open-output-string", openOutputString))
    define(Procedure("open-input-bytevector", openInputBytevector))
    define(Procedure("open-output-bytevector", openOutputBytevector))
    define(Procedure("get-output-string", getOutputString))
    define(Procedure("get-output-bytevector", getOutputBytevector))
    define(Procedure("close-port", closePort))
    define(Procedure("close-input-port", closeInputPort))
    define(Procedure("close-output-port", closeOutputPort))
    define(Procedure("current-input-port", currentInputPort))
    define(Procedure("current-output-port", currentOutputPort))
    define(Procedure("current-error-port", currentErrorPort))
    define(Procedure("call-with-port", callWithPort))
    define(Procedure("call-with-input-file", callWithInputFile))
    define(Procedure("call-with-output-file", callWithOutputFile))
    define(Procedure("with-input-from-file", withInputFromFile))
    define(Procedure("with-output-to-file", withOutputFromFile))
    define(Procedure("eof-object?", isEofObject))
    define(Procedure("eof-object", eofObject))
    define(Procedure("read", read))
    define(Procedure("read-char", readChar))
    define(Procedure("peek-char", peekChar))
    define(Procedure("char-ready?", isCharReady))
    define(Procedure("read-line", readLine))
    define(Procedure("read-string", readString))
    define(Procedure("read-u8", readU8))
    define(Procedure("peek-u8", peekU8))
    define(Procedure("u8-ready?", u8Ready))
    define(Procedure("read-bytevector", readBytevector))
    define(Procedure("read-bytevector!", readBytevectorSet))
    define(Procedure("write", write))
    define(Procedure("write-simple", writeSimple))
    define(Procedure("display", display))
    define(Procedure("newline", newline))
    define(Procedure("write-char", writeChar))
    define(Procedure("write-string", writeString))
    define(Procedure("write-u8", writeU8))
    define(Procedure("write-bytevector", writeBytevector))
    define(Procedure("flush-output-port", flushOutputPort))
  }
  
  func isPort(expr: Expr) -> Expr {
    return .Undef
  }
  
  func isInputPort(expr: Expr) -> Expr {
    return .Undef
  }
  
  func isOutputPort(expr: Expr) -> Expr {
    return .Undef
  }
  
  func isTextualPort(expr: Expr) -> Expr {
    return .Undef
  }
  
  func isBinaryPort(expr: Expr) -> Expr {
    return .Undef
  }
  
  func isInputPortOpen(expr: Expr) -> Expr {
    return .Undef
  }
  
  func isOutputPortOpen(expr: Expr) -> Expr {
    return .Undef
  }
  
  func openInputFile(expr: Expr) -> Expr {
    return .Undef
  }
  
  func openBinaryInputFile(expr: Expr) -> Expr {
    return .Undef
  }
  
  func openOutputFile(expr: Expr) -> Expr {
    return .Undef
  }
  
  func openBinaryOutputFile(expr: Expr) -> Expr {
    return .Undef
  }
  
  func openInputString(expr: Expr) -> Expr {
    return .Undef
  }
  
  func openOutputString(expr: Expr) -> Expr {
    return .Undef
  }
  
  func openInputBytevector(expr: Expr) -> Expr {
    return .Undef
  }
  
  func openOutputBytevector(expr: Expr) -> Expr {
    return .Undef
  }
  
  func getOutputString(expr: Expr) -> Expr {
    return .Undef
  }
  
  func getOutputBytevector(expr: Expr) -> Expr {
    return .Undef
  }
  
  func closePort(expr: Expr) -> Expr {
    return .Undef
  }
  
  func closeInputPort(expr: Expr) -> Expr {
    return .Undef
  }
  
  func closeOutputPort(expr: Expr) -> Expr {
    return .Undef
  }
  
  func currentInputPort(expr: Expr) -> Expr {
    return .Undef
  }
  
  func currentOutputPort(expr: Expr) -> Expr {
    return .Undef
  }
  
  func currentErrorPort(expr: Expr) -> Expr {
    return .Undef
  }
  
  func callWithPort(expr: Expr) -> Expr {
    return .Undef
  }
  
  func callWithInputFile(expr: Expr) -> Expr {
    return .Undef
  }
  
  func callWithOutputFile(expr: Expr) -> Expr {
    return .Undef
  }
  
  func withInputFromFile(expr: Expr) -> Expr {
    return .Undef
  }
  
  func withOutputFromFile(expr: Expr) -> Expr {
    return .Undef
  }
  
  func isEofObject(expr: Expr) -> Expr {
    return .Undef
  }
  
  func eofObject(expr: Expr) -> Expr {
    return .Undef
  }
  
  func read(expr: Expr) -> Expr {
    return .Undef
  }
  
  func readChar(expr: Expr) -> Expr {
    return .Undef
  }
  
  func peekChar(expr: Expr) -> Expr {
    return .Undef
  }
  
  func isCharReady(expr: Expr) -> Expr {
    return .Undef
  }
  
  func readLine(expr: Expr) -> Expr {
    return .Undef
  }
  
  func readString(expr: Expr) -> Expr {
    return .Undef
  }
  
  func readU8(expr: Expr) -> Expr {
    return .Undef
  }
  
  func peekU8(expr: Expr) -> Expr {
    return .Undef
  }
  
  func u8Ready(expr: Expr) -> Expr {
    return .Undef
  }
  
  func readBytevector(expr: Expr) -> Expr {
    return .Undef
  }
  
  func readBytevectorSet(expr: Expr) -> Expr {
    return .Undef
  }
  
  func write(expr: Expr) -> Expr {
    return .Undef
  }
  
  func writeSimple(expr: Expr) -> Expr {
    return .Undef
  }
  
  func writeChar(expr: Expr) -> Expr {
    return .Undef
  }
  
  func writeString(expr: Expr) -> Expr {
    return .Undef
  }
  
  func writeU8(expr: Expr) -> Expr {
    return .Undef
  }
  
  func writeBytevector(expr: Expr) -> Expr {
    return .Undef
  }
  
  func flushOutputPort(expr: Expr) -> Expr {
    return .Undef
  }
  
  
  func display(expr: Expr) -> Expr {
    switch expr {
      case .Str(let str):
        context.console.print(str.value)
      default:
        context.console.print(expr.description)
    }
    return .Void
  }
  
  func newline() -> Expr {
    context.console.print("\n")
    return .Void
  }
}
