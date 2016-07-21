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
    define(Procedure("write-shared", writeShared))
    define(Procedure("write-simple", writeSimple))
    define(Procedure("display", display))
    define(Procedure("newline", newline))
    define(Procedure("write-char", writeChar))
    define(Procedure("write-string", writeString))
    define(Procedure("write-u8", writeU8))
    define(Procedure("write-bytevector", writeBytevector))
    define(Procedure("flush-output-port", flushOutputPort))
    define("call-with-port",
           compile: "(lambda (port proc) (let ((res (proc port))) (close-port port) res))")
    define("call-with-input-file",
           compile: "(lambda (filename proc)" +
                    "   (let* ((port (open-input-file filename))" +
                    "          (res (proc port)))" +
                    "     (close-input-port port)" +
                    "     res))")
    define("call-with-output-file",
           compile: "(lambda (filename proc)" +
                    "   (let* ((port (open-output-file filename))" +
                    "          (res (proc port)))" +
                    "     (close-output-port port)" +
                    "     res))")
  }
  
  
  func textInputFrom(expr: Expr?) throws -> TextInput {
    let port = try expr?.asPort() ?? self.context.inputPort
    guard case .TextInputPort(let input) = port.kind else {
      throw EvalError.TypeError(.Prt(port), [.TextInputPortType])
    }
    return input
  }
  
  func binaryInputFrom(expr: Expr?) throws -> BinaryInput {
    let port = try expr?.asPort() ?? self.context.inputPort
    guard case .BinaryInputPort(let input) = port.kind else {
      throw EvalError.TypeError(.Prt(port), [.BinaryInputPortType])
    }
    return input
  }
  
  func textOutputFrom(expr: Expr?) throws -> TextOutput {
    let port = try expr?.asPort() ?? self.context.outputPort
    guard case .TextOutputPort(let output) = port.kind else {
      throw EvalError.TypeError(.Prt(port), [.TextInputPortType])
    }
    return output
  }
  
  func binaryOutputFrom(expr: Expr?) throws -> BinaryOutput {
    let port = try expr?.asPort() ?? self.context.outputPort
    guard case .BinaryOutputPort(let output) = port.kind else {
      throw EvalError.TypeError(.Prt(port), [.BinaryInputPortType])
    }
    return output
  }
  
  func isPort(expr: Expr) -> Expr {
    if case .Prt(_) = expr {
      return .True
    }
    return .False
  }
  
  func isInputPort(expr: Expr) -> Expr {
    guard case .Prt(let port) = expr else {
      return .False
    }
    return .Boolean(port.isInputPort)
  }
  
  func isOutputPort(expr: Expr) -> Expr {
    guard case .Prt(let port) = expr else {
      return .False
    }
    return .Boolean(port.isOutputPort)
  }
  
  func isTextualPort(expr: Expr) -> Expr {
    guard case .Prt(let port) = expr else {
      return .False
    }
    return .Boolean(port.isTextualPort)
  }
  
  func isBinaryPort(expr: Expr) throws -> Expr {
    guard case .Prt(let port) = expr else {
      return .False
    }
    return .Boolean(port.isBinaryPort)
  }
  
  func isInputPortOpen(expr: Expr) throws -> Expr {
    let port = try expr.asPort()
    return .Boolean(port.isInputPort && port.isOpen)
  }
  
  func isOutputPortOpen(expr: Expr) throws -> Expr {
    let port = try expr.asPort()
    return .Boolean(port.isOutputPort && port.isOpen)
  }
  
  func openInputFile(expr: Expr) throws -> Expr {
    let filename = try expr.asStr()
    guard let input = BinaryInput(path: filename) else {
      throw EvalError.CannotOpenFile(filename)
    }
    return .Prt(Port(input: TextInput(input: input)))
  }
  
  func openBinaryInputFile(expr: Expr) throws -> Expr {
    let filename = try expr.asStr()
    guard let input = BinaryInput(path: filename) else {
      throw EvalError.CannotOpenFile(filename)
    }
    return .Prt(Port(input: input))
  }
  
  func openOutputFile(expr: Expr) throws -> Expr {
    let filename = try expr.asStr()
    guard let output = BinaryOutput(path: filename) else {
      throw EvalError.CannotOpenFile(filename)
    }
    return .Prt(Port(output: TextOutput(output: output)))
  }
  
  func openBinaryOutputFile(expr: Expr) throws -> Expr {
    let filename = try expr.asStr()
    guard let output = BinaryOutput(path: filename) else {
      throw EvalError.CannotOpenFile(filename)
    }
    return .Prt(Port(output: output))
  }
  
  func openInputString(expr: Expr) throws -> Expr {
    return .Prt(Port(input: TextInput(string: try expr.asStr())))
  }
  
  func openOutputString() -> Expr {
    return .Prt(Port(output: TextOutput()))
  }
  
  func openInputBytevector(expr: Expr) throws -> Expr {
    return .Prt(Port(input: BinaryInput(data: try expr.asByteVector().value)))
  }
  
  func openOutputBytevector() -> Expr {
    return .Prt(Port(output: BinaryOutput()))
  }
  
  func getOutputString(expr: Expr) throws -> Expr {
    guard let buffer = try expr.asPort().outputString else {
      throw EvalError.TypeError(expr, [.TextOutputPortType])
    }
    return .StringFor(buffer)
  }
  
  func getOutputBytevector(expr: Expr) throws -> Expr {
    guard let buffer = try expr.asPort().outputBinary else {
      throw EvalError.TypeError(expr, [.BinaryOutputPortType])
    }
    return .Bytes(MutableBox(buffer))
  }
  
  func closePort(expr: Expr) throws -> Expr {
    try expr.asPort().close()
    return .Void
  }
  
  func closeInputPort(expr: Expr) throws -> Expr {
    let port = try expr.asPort()
    if port.isInputPort {
      port.close()
    }
    return .Void
  }
  
  func closeOutputPort(expr: Expr) throws -> Expr {
    let port = try expr.asPort()
    if port.isOutputPort {
      port.close()
    }
    return .Void
  }
  
  func currentInputPort(expr: Expr) -> Expr {
    return .Prt(self.context.inputPort)
  }
  
  func currentOutputPort(expr: Expr) -> Expr {
    return .Prt(self.context.outputPort)
  }
  
  func currentErrorPort(expr: Expr) -> Expr {
    return .Prt(self.context.errorPort)
  }
  
  /// TODO: Figure out how to implement this function (needs to be in Scheme directly)
  func withInputFromFile(expr: Expr) -> Expr {
    return .Undef
  }
  
  /// TODO: Figure out how to implement this function (needs to be in Scheme directly)
  func withOutputFromFile(expr: Expr) -> Expr {
    return .Undef
  }
  
  func isEofObject(expr: Expr) -> Expr {
    return .Boolean(expr == .Eof)
  }
  
  func eofObject() -> Expr {
    return .Eof
  }
  
  func read(expr: Expr?) throws -> Expr {
    let input = try self.textInputFrom(expr)
    let parser = Parser(symbols: self.context.symbols, input: input)
    return try parser.parse(false)
  }
  
  func readChar(expr: Expr?) throws -> Expr {
    let input = try self.textInputFrom(expr)
    guard let ch = input.read() else {
      return .Eof
    }
    return .Char(ch)
  }
  
  func peekChar(expr: Expr?) throws -> Expr {
    let input = try self.textInputFrom(expr)
    guard let ch = input.peek() else {
      return .Eof
    }
    return .Char(ch)
  }
  
  func isCharReady(port: Expr?) throws -> Expr {
    let input = try self.textInputFrom(port)
    return .Boolean(!input.readMightBlock)
  }
  
  func readLine(expr: Expr?) throws -> Expr {
    let input = try self.textInputFrom(expr)
    guard let str = input.readLine() else {
      return .Eof
    }
    return .StringFor(str)
  }
  
  func readString(nchars: Expr, expr: Expr?) throws -> Expr {
    let input = try self.textInputFrom(expr)
    guard let str = input.readString(try nchars.asInt()) else {
      return .Eof
    }
    return .StringFor(str)
  }
  
  func readU8(expr: Expr?) throws -> Expr {
    let input = try self.binaryInputFrom(expr)
    guard let byte = input.read() else {
      return .Eof
    }
    return .Fixnum(Int64(byte))
  }
  
  func peekU8(expr: Expr?) throws -> Expr {
    let input = try self.binaryInputFrom(expr)
    guard let byte = input.peek() else {
      return .Eof
    }
    return .Fixnum(Int64(byte))
  }
  
  func u8Ready(port: Expr?) throws -> Expr {
    let input = try self.binaryInputFrom(port)
    return .Boolean(!input.readMightBlock)
  }
  
  func readBytevector(nbytes: Expr, expr: Expr?) throws -> Expr {
    let input = try self.binaryInputFrom(expr)
    guard let bytes = input.readMany(try nbytes.asInt()) else {
      return .Eof
    }
    return .Bytes(MutableBox(bytes))
  }
  
  func readBytevectorSet(bvec: Expr, args: Arguments) throws -> Expr {
    let bvector = try bvec.asByteVector()
    guard let (pexpr, s, e) = args.optional(.Prt(self.context.inputPort),
                                            .Number(0),
                                            .Number(bvector.value.count)) else {
      throw EvalError.ArgumentCountError(formals: 4, args: .Pair(bvec, .List(args)))
    }
    let (start, end) = (try s.asInt(), try e.asInt())
    guard start >= 0 && start < bvector.value.count else {
      throw EvalError.ParameterOutOfBounds(
        "read-bytevector!", 3, Int64(start), Int64(0), Int64(bvector.value.count - 1))
    }
    guard end >= start && end <= bvector.value.count else {
      throw EvalError.ParameterOutOfBounds(
        "read-bytevector!", 4, Int64(end), Int64(start), Int64(bvector.value.count))
    }
    let input = try self.binaryInputFrom(pexpr)
    guard let n = input.readInto(&bvector.value, start: start, end: end) else {
      return .Eof
    }
    return .Fixnum(Int64(n))
  }
  
  func write(expr: Expr, port: Expr?) throws -> Expr {
    let output = try self.textOutputFrom(port)
    guard output.writeString(expr.description) else {
      throw EvalError.CannotWriteToPort(port ?? .Prt(self.context.outputPort))
    }
    return .Void
  }
  
  /// TODO: Implement this function correctly; i.e. force usage of datum labels for shared
  /// structures.
  func writeShared(expr: Expr, port: Expr?) throws -> Expr {
    let output = try self.textOutputFrom(port)
    guard output.writeString(expr.description) else {
      throw EvalError.CannotWriteToPort(port ?? .Prt(self.context.outputPort))
    }
    return .Void
  }
  
  /// TODO: Implement this function correctly; i.e. never use datum labels for shared
  /// structures.
  func writeSimple(expr: Expr, port: Expr?) throws -> Expr {
    let output = try self.textOutputFrom(port)
    guard output.writeString(expr.description) else {
      throw EvalError.CannotWriteToPort(port ?? .Prt(self.context.outputPort))
    }
    return .Void
  }
  
  func display(expr: Expr, port: Expr? = nil) throws -> Expr {
    let output = try self.textOutputFrom(port)
    guard output.writeString(expr.unescapedDescription) else {
      throw EvalError.CannotWriteToPort(port ?? .Prt(self.context.outputPort))
    }
    return .Void
  }
  
  func newline(port: Expr?) throws -> Expr {
    guard try self.textOutputFrom(port).writeString("\n") else {
      throw EvalError.CannotWriteToPort(port ?? .Prt(self.context.outputPort))
    }
    return .Void
  }
  
  func writeChar(expr: Expr, port: Expr?) throws -> Expr {
    guard try self.textOutputFrom(port).write(expr.asChar()) else {
      throw EvalError.CannotWriteToPort(port ?? .Prt(self.context.outputPort))
    }
    return .Void
  }
  
  func writeString(expr: Expr, args: Arguments) throws -> Expr {
    if args.count < 2 {
      guard try self.textOutputFrom(args.first).writeString(expr.asStr()) else {
        throw EvalError.CannotWriteToPort(args.first ?? .Prt(self.context.outputPort))
      }
    } else {
      let chars = try expr.asStr().utf16
      guard let (port, s, e) = args.optional(.Prt(self.context.outputPort),
                                             .Number(0),
                                             .Number(chars.count)) else {
        throw EvalError.ArgumentCountError(formals: 4, args: .Pair(expr, .List(args)))
      }
      let (start, end) = (try s.asInt(), try e.asInt())
      guard start >= 0 && start < chars.count else {
        throw EvalError.ParameterOutOfBounds(
          "write-string", 3, Int64(start), Int64(0), Int64(chars.count - 1))
      }
      guard end >= start && end <= chars.count else {
        throw EvalError.ParameterOutOfBounds(
          "write-string", 4, Int64(end), Int64(start), Int64(chars.count))
      }
      var uniChars: [UniChar] = []
      for ch in chars[chars.startIndex.advancedBy(start)..<chars.startIndex.advancedBy(end)] {
        uniChars.append(ch)
      }
      let str = String(utf16CodeUnits: uniChars, count: end - start)
      guard try self.textOutputFrom(port).writeString(str) else {
        throw EvalError.CannotWriteToPort(port)
      }
    }
    return .Void
  }
  
  func writeU8(expr: Expr, port: Expr?) throws -> Expr {
    guard try self.binaryOutputFrom(port).write(expr.asByte()) else {
      throw EvalError.CannotWriteToPort(port ?? .Prt(self.context.outputPort))
    }
    return .Void
  }
  
  func writeBytevector(expr: Expr, args: Arguments) throws -> Expr {
    let bvector = try expr.asByteVector().value
    if args.count < 2 {
      guard try self.binaryOutputFrom(args.first)
                    .writeFrom(bvector, start: 0, end: bvector.count) else {
        throw EvalError.CannotWriteToPort(args.first ?? .Prt(self.context.outputPort))
      }
    } else {
      guard let (port, s, e) = args.optional(.Prt(self.context.outputPort),
                                             .Number(0),
                                             .Number(bvector.count)) else {
                                              throw EvalError.ArgumentCountError(formals: 4, args: .Pair(expr, .List(args)))
      }
      let (start, end) = (try s.asInt(), try e.asInt())
      guard start >= 0 && start < bvector.count else {
        throw EvalError.ParameterOutOfBounds(
          "write-bytevector", 3, Int64(start), Int64(0), Int64(bvector.count - 1))
      }
      guard end >= start && end <= bvector.count else {
        throw EvalError.ParameterOutOfBounds(
          "write-bytevector", 4, Int64(end), Int64(start), Int64(bvector.count))
      }
      guard try self.binaryOutputFrom(port).writeFrom(bvector, start: start, end: end) else {
        throw EvalError.CannotWriteToPort(port)
      }
    }
    return .Void
  }
  
  func flushOutputPort(port: Expr?) throws -> Expr {
    let port = try port?.asPort() ?? self.context.outputPort
    switch port.kind {
      case .BinaryOutputPort(let output):
        output.flush(true)
      case .TextOutputPort(let output):
        output.flush(true)
      default:
        throw EvalError.TypeError(.Prt(port), [.OutputPortType])
    }
    return .Void
  }
}
