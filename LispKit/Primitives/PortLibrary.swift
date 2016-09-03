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

///
/// Port library: based on R7RS spec.
/// 
public final class PortLibrary: NativeLibrary {
  
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
    define("with-input-from-file", compile:
      "(lambda (file thunk)" +
      "  (let ((old (current-input-port))" +
      "        (new (open-input-file file)))" +
      "    (dynamic-wind (lambda () (current-input-port new))" +
      "                  (lambda () (let ((res (thunk))) (close-input-port new) res))" +
      "                  (lambda () (current-input-port old)))))")
    define("with-output-to-file", compile:
      "(lambda (file thunk)" +
      "  (let ((old (current-output-port))" +
      "        (new (open-output-file file)))" +
      "    (dynamic-wind (lambda () (current-output-port new))" +
      "                  (lambda () (let ((res (thunk))) (close-output-port new) res))" +
      "                  (lambda () (current-output-port old)))))")
    define("call-with-port", compile:
      "(lambda (port proc) (let ((res (proc port))) (close-port port) res))")
    define("call-with-input-file", compile:
      "(lambda (filename proc)" +
      "   (let* ((port (open-input-file filename))" +
      "          (res (proc port)))" +
      "     (close-input-port port)" +
      "     res))")
    define("call-with-output-file", compile:
      "(lambda (filename proc)" +
      "   (let* ((port (open-output-file filename))" +
      "          (res (proc port)))" +
      "     (close-output-port port)" +
      "     res))")
  }
  
  
  func textInputFrom(_ expr: Expr?) throws -> TextInput {
    let port = try expr?.asPort() ?? self.context.inputPort
    guard case .textInputPort(let input) = port.kind else {
      throw EvalError.typeError(.port(port), [.textInputPortType])
    }
    return input
  }
  
  func binaryInputFrom(_ expr: Expr?) throws -> BinaryInput {
    let port = try expr?.asPort() ?? self.context.inputPort
    guard case .binaryInputPort(let input) = port.kind else {
      throw EvalError.typeError(.port(port), [.binaryInputPortType])
    }
    return input
  }
  
  func textOutputFrom(_ expr: Expr?) throws -> TextOutput {
    let port = try expr?.asPort() ?? self.context.outputPort
    guard case .textOutputPort(let output) = port.kind else {
      throw EvalError.typeError(.port(port), [.textInputPortType])
    }
    return output
  }
  
  func binaryOutputFrom(_ expr: Expr?) throws -> BinaryOutput {
    let port = try expr?.asPort() ?? self.context.outputPort
    guard case .binaryOutputPort(let output) = port.kind else {
      throw EvalError.typeError(.port(port), [.binaryInputPortType])
    }
    return output
  }
  
  func isPort(_ expr: Expr) -> Expr {
    if case .port(_) = expr {
      return .true
    }
    return .false
  }
  
  func isInputPort(_ expr: Expr) -> Expr {
    guard case .port(let port) = expr else {
      return .false
    }
    return .Boolean(port.isInputPort)
  }
  
  func isOutputPort(_ expr: Expr) -> Expr {
    guard case .port(let port) = expr else {
      return .false
    }
    return .Boolean(port.isOutputPort)
  }
  
  func isTextualPort(_ expr: Expr) -> Expr {
    guard case .port(let port) = expr else {
      return .false
    }
    return .Boolean(port.isTextualPort)
  }
  
  func isBinaryPort(_ expr: Expr) throws -> Expr {
    guard case .port(let port) = expr else {
      return .false
    }
    return .Boolean(port.isBinaryPort)
  }
  
  func isInputPortOpen(_ expr: Expr) throws -> Expr {
    let port = try expr.asPort()
    return .Boolean(port.isInputPort && port.isOpen)
  }
  
  func isOutputPortOpen(_ expr: Expr) throws -> Expr {
    let port = try expr.asPort()
    return .Boolean(port.isOutputPort && port.isOpen)
  }
  
  func openInputFile(_ expr: Expr) throws -> Expr {
    let filename = try expr.asStr()
    guard let input = BinaryInput(path: filename) else {
      throw EvalError.cannotOpenFile(filename)
    }
    return .port(Port(input: TextInput(input: input)))
  }
  
  func openBinaryInputFile(_ expr: Expr) throws -> Expr {
    let filename = try expr.asStr()
    guard let input = BinaryInput(path: filename) else {
      throw EvalError.cannotOpenFile(filename)
    }
    return .port(Port(input: input))
  }
  
  func openOutputFile(_ expr: Expr) throws -> Expr {
    let filename = try expr.asStr()
    guard let output = BinaryOutput(path: filename) else {
      throw EvalError.cannotOpenFile(filename)
    }
    return .port(Port(output: TextOutput(output: output)))
  }
  
  func openBinaryOutputFile(_ expr: Expr) throws -> Expr {
    let filename = try expr.asStr()
    guard let output = BinaryOutput(path: filename) else {
      throw EvalError.cannotOpenFile(filename)
    }
    return .port(Port(output: output))
  }
  
  func openInputString(_ expr: Expr) throws -> Expr {
    return .port(Port(input: TextInput(string: try expr.asStr())))
  }
  
  func openOutputString() -> Expr {
    return .port(Port(output: TextOutput()))
  }
  
  func openInputBytevector(_ expr: Expr) throws -> Expr {
    return .port(Port(input: BinaryInput(data: try expr.asByteVector().value)))
  }
  
  func openOutputBytevector() -> Expr {
    return .port(Port(output: BinaryOutput()))
  }
  
  func getOutputString(_ expr: Expr) throws -> Expr {
    guard let buffer = try expr.asPort().outputString else {
      throw EvalError.typeError(expr, [.textOutputPortType])
    }
    return .StringFor(buffer)
  }
  
  func getOutputBytevector(_ expr: Expr) throws -> Expr {
    guard let buffer = try expr.asPort().outputBinary else {
      throw EvalError.typeError(expr, [.binaryOutputPortType])
    }
    return .bytes(MutableBox(buffer))
  }
  
  func closePort(_ expr: Expr) throws -> Expr {
    try expr.asPort().close()
    return .void
  }
  
  func closeInputPort(_ expr: Expr) throws -> Expr {
    let port = try expr.asPort()
    if port.isInputPort {
      port.close()
    }
    return .void
  }
  
  func closeOutputPort(_ expr: Expr) throws -> Expr {
    let port = try expr.asPort()
    if port.isOutputPort {
      port.close()
    }
    return .void
  }
  
  func currentInputPort(_ expr: Expr) -> Expr {
    return .port(self.context.inputPort)
  }
  
  func currentOutputPort(_ expr: Expr) -> Expr {
    return .port(self.context.outputPort)
  }
  
  func currentErrorPort(_ expr: Expr) -> Expr {
    return .port(self.context.errorPort)
  }
  
  func isEofObject(_ expr: Expr) -> Expr {
    return .Boolean(expr == .eof)
  }
  
  func eofObject() -> Expr {
    return .eof
  }
  
  func read(_ expr: Expr?) throws -> Expr {
    let input = try self.textInputFrom(expr)
    let parser = Parser(symbols: self.context.symbols, input: input)
    return try parser.parse(false)
  }
  
  func readChar(_ expr: Expr?) throws -> Expr {
    let input = try self.textInputFrom(expr)
    guard let ch = input.read() else {
      return .eof
    }
    return .char(ch)
  }
  
  func peekChar(_ expr: Expr?) throws -> Expr {
    let input = try self.textInputFrom(expr)
    guard let ch = input.peek() else {
      return .eof
    }
    return .char(ch)
  }
  
  func isCharReady(_ port: Expr?) throws -> Expr {
    let input = try self.textInputFrom(port)
    return .Boolean(!input.readMightBlock)
  }
  
  func readLine(_ expr: Expr?) throws -> Expr {
    let input = try self.textInputFrom(expr)
    guard let str = input.readLine() else {
      return .eof
    }
    return .StringFor(str)
  }
  
  func readString(_ nchars: Expr, expr: Expr?) throws -> Expr {
    let input = try self.textInputFrom(expr)
    guard let str = input.readString(try nchars.asInt()) else {
      return .eof
    }
    return .StringFor(str)
  }
  
  func readU8(_ expr: Expr?) throws -> Expr {
    let input = try self.binaryInputFrom(expr)
    guard let byte = input.read() else {
      return .eof
    }
    return .fixnum(Int64(byte))
  }
  
  func peekU8(_ expr: Expr?) throws -> Expr {
    let input = try self.binaryInputFrom(expr)
    guard let byte = input.peek() else {
      return .eof
    }
    return .fixnum(Int64(byte))
  }
  
  func u8Ready(_ port: Expr?) throws -> Expr {
    let input = try self.binaryInputFrom(port)
    return .Boolean(!input.readMightBlock)
  }
  
  func readBytevector(_ nbytes: Expr, expr: Expr?) throws -> Expr {
    let input = try self.binaryInputFrom(expr)
    guard let bytes = input.readMany(try nbytes.asInt()) else {
      return .eof
    }
    return .bytes(MutableBox(bytes))
  }
  
  func readBytevectorSet(_ bvec: Expr, args: Arguments) throws -> Expr {
    let bvector = try bvec.asByteVector()
    guard let (pexpr, s, e) = args.optional(.port(self.context.inputPort),
                                            .Number(bvector.value.count),
                                            .Number(0)) else {
      throw EvalError.argumentCountError(formals: 4, args: .pair(bvec, .List(args)))
    }
    let (start, end) = (try s.asInt(), try e.asInt())
    guard start >= 0 && start < bvector.value.count else {
      throw EvalError.parameterOutOfBounds(
        "read-bytevector!", 3, Int64(start), Int64(0), Int64(bvector.value.count - 1))
    }
    guard end >= start && end <= bvector.value.count else {
      throw EvalError.parameterOutOfBounds(
        "read-bytevector!", 4, Int64(end), Int64(start), Int64(bvector.value.count))
    }
    let input = try self.binaryInputFrom(pexpr)
    guard let n = input.readInto(&bvector.value, start: start, end: end) else {
      return .eof
    }
    return .fixnum(Int64(n))
  }
  
  func write(_ expr: Expr, port: Expr?) throws -> Expr {
    let output = try self.textOutputFrom(port)
    guard output.writeString(expr.description) else {
      throw EvalError.cannotWriteToPort(port ?? .port(self.context.outputPort))
    }
    return .void
  }
  
  /// TODO: Implement this function correctly; i.e. force usage of datum labels for shared
  /// structures.
  func writeShared(_ expr: Expr, port: Expr?) throws -> Expr {
    let output = try self.textOutputFrom(port)
    guard output.writeString(expr.description) else {
      throw EvalError.cannotWriteToPort(port ?? .port(self.context.outputPort))
    }
    return .void
  }
  
  /// TODO: Implement this function correctly; i.e. never use datum labels for shared
  /// structures.
  func writeSimple(_ expr: Expr, port: Expr?) throws -> Expr {
    let output = try self.textOutputFrom(port)
    guard output.writeString(expr.description) else {
      throw EvalError.cannotWriteToPort(port ?? .port(self.context.outputPort))
    }
    return .void
  }
  
  func display(_ expr: Expr, port: Expr? = nil) throws -> Expr {
    let output = try self.textOutputFrom(port)
    guard output.writeString(expr.unescapedDescription) else {
      throw EvalError.cannotWriteToPort(port ?? .port(self.context.outputPort))
    }
    return .void
  }
  
  func newline(_ port: Expr?) throws -> Expr {
    guard try self.textOutputFrom(port).writeString("\n") else {
      throw EvalError.cannotWriteToPort(port ?? .port(self.context.outputPort))
    }
    return .void
  }
  
  func writeChar(_ expr: Expr, port: Expr?) throws -> Expr {
    guard try self.textOutputFrom(port).write(expr.asChar()) else {
      throw EvalError.cannotWriteToPort(port ?? .port(self.context.outputPort))
    }
    return .void
  }
  
  func writeString(_ expr: Expr, args: Arguments) throws -> Expr {
    if args.count < 2 {
      guard try self.textOutputFrom(args.first).writeString(expr.asStr()) else {
        throw EvalError.cannotWriteToPort(args.first ?? .port(self.context.outputPort))
      }
    } else {
      let chars = try expr.asStr().utf16
      guard let (port, s, e) = args.optional(.port(self.context.outputPort),
                                             .Number(chars.count),
                                             .Number(0)) else {
        throw EvalError.argumentCountError(formals: 4, args: .pair(expr, .List(args)))
      }
      let (start, end) = (try s.asInt(), try e.asInt())
      guard start >= 0 && start < chars.count else {
        throw EvalError.parameterOutOfBounds(
          "write-string", 3, Int64(start), Int64(0), Int64(chars.count - 1))
      }
      guard end >= start && end <= chars.count else {
        throw EvalError.parameterOutOfBounds(
          "write-string", 4, Int64(end), Int64(start), Int64(chars.count))
      }
      var uniChars: [UniChar] = []
      for ch in chars[chars.index(chars.startIndex, offsetBy: start)..<chars.index(chars.startIndex, offsetBy: end)] {
        uniChars.append(ch)
      }
      let str = String(utf16CodeUnits: uniChars, count: end - start)
      guard try self.textOutputFrom(port).writeString(str) else {
        throw EvalError.cannotWriteToPort(port)
      }
    }
    return .void
  }
  
  func writeU8(_ expr: Expr, port: Expr?) throws -> Expr {
    guard try self.binaryOutputFrom(port).write(expr.asByte()) else {
      throw EvalError.cannotWriteToPort(port ?? .port(self.context.outputPort))
    }
    return .void
  }
  
  func writeBytevector(_ expr: Expr, args: Arguments) throws -> Expr {
    let bvector = try expr.asByteVector().value
    if args.count < 2 {
      guard try self.binaryOutputFrom(args.first)
                    .writeFrom(bvector, start: 0, end: bvector.count) else {
        throw EvalError.cannotWriteToPort(args.first ?? .port(self.context.outputPort))
      }
    } else {
      guard let (port, s, e) = args.optional(.port(self.context.outputPort),
                                             .Number(bvector.count),
                                             .Number(0)) else {
                                              throw EvalError.argumentCountError(formals: 4, args: .pair(expr, .List(args)))
      }
      let (start, end) = (try s.asInt(), try e.asInt())
      guard start >= 0 && start < bvector.count else {
        throw EvalError.parameterOutOfBounds(
          "write-bytevector", 3, Int64(start), Int64(0), Int64(bvector.count - 1))
      }
      guard end >= start && end <= bvector.count else {
        throw EvalError.parameterOutOfBounds(
          "write-bytevector", 4, Int64(end), Int64(start), Int64(bvector.count))
      }
      guard try self.binaryOutputFrom(port).writeFrom(bvector, start: start, end: end) else {
        throw EvalError.cannotWriteToPort(port)
      }
    }
    return .void
  }
  
  func flushOutputPort(_ port: Expr?) throws -> Expr {
    let port = try port?.asPort() ?? self.context.outputPort
    switch port.kind {
      case .binaryOutputPort(let output):
        output.flush(true)
      case .textOutputPort(let output):
        output.flush(true)
      default:
        throw EvalError.typeError(.port(port), [.outputPortType])
    }
    return .void
  }
}
