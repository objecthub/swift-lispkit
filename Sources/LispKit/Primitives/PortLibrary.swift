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
  
  /// Imported native library
  private var systemLibrary: SystemLibrary!
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "port"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"],    "define", "lambda", "quote")
    self.`import`(from: ["lispkit", "control"], "let", "let*")
    self.`import`(from: ["lispkit", "system"],  "current-directory")
    self.`import`(from: ["lispkit", "dynamic"], "dynamic-wind")
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("port?", isPort))
    self.define(Procedure("input-port?", isInputPort))
    self.define(Procedure("output-port?", isOutputPort))
    self.define(Procedure("textual-port?", isTextualPort))
    self.define(Procedure("binary-port?", isBinaryPort))
    self.define(Procedure("input-port-open?", isInputPortOpen))
    self.define(Procedure("output-port-open?", isOutputPortOpen))
    self.define(Procedure("open-input-file", openInputFile))
    self.define(Procedure("open-binary-input-file", openBinaryInputFile))
    self.define(Procedure("open-output-file", openOutputFile))
    self.define(Procedure("open-binary-output-file", openBinaryOutputFile))
    self.define(Procedure("open-input-string", openInputString))
    self.define(Procedure("open-output-string", openOutputString))
    self.define(Procedure("open-input-bytevector", openInputBytevector))
    self.define(Procedure("open-output-bytevector", openOutputBytevector))
    self.define(Procedure("open-input-url", openInputUrl))
    self.define(Procedure("open-binary-input-url", openBinaryInputUrl))
    self.define(Procedure("get-output-string", getOutputString))
    self.define(Procedure("get-output-bytevector", getOutputBytevector))
    self.define(Procedure("close-port", closePort))
    self.define(Procedure("close-input-port", closeInputPort))
    self.define(Procedure("close-output-port", closeOutputPort))
    self.define(Procedure("current-input-port", currentInputPort))
    self.define(Procedure("current-output-port", currentOutputPort))
    self.define(Procedure("current-error-port", currentErrorPort))
    self.define(Procedure("eof-object?", isEofObject))
    self.define(Procedure("eof-object", eofObject))
    self.define(Procedure("read", read))
    self.define(Procedure("read-char", readChar))
    self.define(Procedure("peek-char", peekChar))
    self.define(Procedure("char-ready?", isCharReady))
    self.define(Procedure("read-token", readToken))
    self.define(Procedure("read-line", readLine))
    self.define(Procedure("read-string", readString))
    self.define(Procedure("read-u8", readU8))
    self.define(Procedure("peek-u8", peekU8))
    self.define(Procedure("u8-ready?", u8Ready))
    self.define(Procedure("read-bytevector", readBytevector))
    self.define(Procedure("read-bytevector!", readBytevectorSet))
    self.define(Procedure("write", write))
    self.define(Procedure("write-shared", writeShared))
    self.define(Procedure("write-simple", writeSimple))
    self.define(Procedure("display", display))
    self.define(Procedure("newline", newline))
    self.define(Procedure("write-char", writeChar))
    self.define(Procedure("write-string", writeString))
    self.define(Procedure("write-u8", writeU8))
    self.define(Procedure("write-bytevector", writeBytevector))
    self.define(Procedure("flush-output-port", flushOutputPort))
    self.define("with-input-from-port", via:
      "(define (with-input-from-port new thunk)",
      "  (let ((old (current-input-port)))",
      "    (dynamic-wind (lambda () (current-input-port new))",
      "                  (lambda () (let ((res (thunk))) (close-input-port new) res))",
      "                  (lambda () (current-input-port old)))))")
    self.define("with-output-to-port", via:
      "(define (with-output-to-port new thunk)",
      "  (let ((old (current-output-port)))",
      "    (dynamic-wind (lambda () (current-output-port new))",
      "                  (lambda () (let ((res (thunk))) (close-output-port new) res))",
      "                  (lambda () (current-output-port old)))))")
    self.define("with-input-from-file", via:
      "(define (with-input-from-file file thunk)",
      "  (let ((old (current-input-port))",
      "        (new (open-input-file file)))",
      "    (dynamic-wind (lambda () (current-input-port new))",
      "                  (lambda () (let ((res (thunk))) (close-input-port new) res))",
      "                  (lambda () (current-input-port old)))))")
    self.define("with-output-to-file", via:
      "(define (with-output-to-file file thunk)",
      "  (let ((old (current-output-port))",
      "        (new (open-output-file file)))",
      "    (dynamic-wind (lambda () (current-output-port new))",
      "                  (lambda () (let ((res (thunk))) (close-output-port new) res))",
      "                  (lambda () (current-output-port old)))))")
    self.define("with-input-from-string", via:
      "(define (with-input-from-string str thunk)",
      "  (let ((old (current-input-port))",
      "        (new (open-input-string str)))",
      "    (dynamic-wind (lambda () (current-input-port new))",
      "                  (lambda () (let ((res (thunk))) (close-input-port new) res))",
      "                  (lambda () (current-input-port old)))))")
    self.define("with-output-to-string", via:
      "(define (with-output-to-string thunk)",
      "  (let ((old (current-output-port))",
      "        (new (open-output-string)))",
      "    (dynamic-wind (lambda () (current-output-port new))",
      "                  (lambda () (let ((res (thunk))) (close-output-port new) res))",
      "                  (lambda () (current-output-port old)))))")
    self.define("with-input-from-url", via:
      "(define (with-input-from-url url thunk)",
      "  (let ((old (current-input-port))",
      "        (new (open-input-url url)))",
      "    (dynamic-wind (lambda () (current-input-port new))",
      "                  (lambda () (let ((res (thunk))) (close-input-port new) res))",
      "                  (lambda () (current-input-port old)))))")
    self.define("call-with-port", via:
      "(define (call-with-port port proc) (let ((res (proc port))) (close-port port) res))")
    self.define("call-with-input-file", via:
      "(define (call-with-input-file filename proc)",
      "   (let* ((port (open-input-file filename))",
      "          (res (proc port)))",
      "     (close-input-port port)",
      "     res))")
    self.define("call-with-output-file", via:
      "(define (call-with-output-file filename proc)",
      "   (let* ((port (open-output-file filename))",
      "          (res (proc port)))",
      "     (close-output-port port)",
      "     res))")
    self.define("call-with-input-url", via:
      "(define (call-with-input-url url proc)",
      "   (let* ((port (open-input-url url))",
      "          (res (proc port)))",
      "     (close-input-port port)",
      "     res))")
  }
  
  public override func initializations() {
    self.systemLibrary = self.nativeLibrary(SystemLibrary.self)
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
    return .makeBoolean(port.isInputPort)
  }
  
  func isOutputPort(_ expr: Expr) -> Expr {
    guard case .port(let port) = expr else {
      return .false
    }
    return .makeBoolean(port.isOutputPort)
  }
  
  func isTextualPort(_ expr: Expr) -> Expr {
    guard case .port(let port) = expr else {
      return .false
    }
    return .makeBoolean(port.isTextualPort)
  }
  
  func isBinaryPort(_ expr: Expr) throws -> Expr {
    guard case .port(let port) = expr else {
      return .false
    }
    return .makeBoolean(port.isBinaryPort)
  }
  
  func isInputPortOpen(_ expr: Expr) throws -> Expr {
    let port = try expr.asPort()
    return .makeBoolean(port.isInputPort && port.isOpen)
  }
  
  func isOutputPortOpen(_ expr: Expr) throws -> Expr {
    let port = try expr.asPort()
    return .makeBoolean(port.isOutputPort && port.isOpen)
  }
  
  func openInputFile(_ expr: Expr) throws -> Expr {
    let filename =
      self.context.fileHandler.path(try expr.asPath(),
                                    relativeTo: self.systemLibrary.currentDirectoryPath)
    guard let input = BinaryInput(path: filename) else {
      throw EvalError.cannotOpenFile(filename)
    }
    return .port(Port(input: TextInput(input: input)))
  }
  
  func openBinaryInputFile(_ expr: Expr) throws -> Expr {
    let filename =
      self.context.fileHandler.path(try expr.asPath(),
                                    relativeTo: self.systemLibrary.currentDirectoryPath)
    guard let input = BinaryInput(path: filename) else {
      throw EvalError.cannotOpenFile(filename)
    }
    return .port(Port(input: input))
  }
  
  func openOutputFile(_ expr: Expr) throws -> Expr {
    let filename =
      self.context.fileHandler.path(try expr.asPath(),
                                    relativeTo: self.systemLibrary.currentDirectoryPath)
    guard let output = BinaryOutput(path: filename) else {
      throw EvalError.cannotOpenFile(filename)
    }
    return .port(Port(output: TextOutput(output: output)))
  }
  
  func openBinaryOutputFile(_ expr: Expr) throws -> Expr {
    let filename =
      self.context.fileHandler.path(try expr.asPath(),
                                    relativeTo: self.systemLibrary.currentDirectoryPath)
    guard let output = BinaryOutput(path: filename) else {
      throw EvalError.cannotOpenFile(filename)
    }
    return .port(Port(output: output))
  }
  
  func openInputString(_ expr: Expr) throws -> Expr {
    return .port(Port(input: TextInput(string: try expr.asString())))
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
  
  func openInputUrl(_ expr: Expr) throws -> Expr {
    let url = try expr.asURL()
    guard let input = BinaryInput(url: url) else {
      throw EvalError.cannotOpenUrl(url.description)
    }
    return .port(Port(input: TextInput(input: input)))
  }
  
  func openBinaryInputUrl(_ expr: Expr) throws -> Expr {
    let url = try expr.asURL()
    guard let input = BinaryInput(url: url) else {
      throw EvalError.cannotOpenUrl(url.description)
    }
    return .port(Port(input: input))
  }
  
  func getOutputString(_ expr: Expr) throws -> Expr {
    guard let buffer = try expr.asPort().outputString else {
      throw EvalError.typeError(expr, [.textOutputPortType])
    }
    return .makeString(buffer)
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
  
  func currentInputPort(_ expr: Expr?) throws -> Expr {
    switch expr {
      case .none:
        return .port(self.context.inputPort)
      case .some(.port(let port)):
        guard port.isInputPort else {
          throw EvalError.typeError(expr!, [.inputPortType])
        }
        self.context.inputPort = port
        return .void
      default:
        throw EvalError.typeError(expr!, [.inputPortType])
    }
  }
  
  func currentOutputPort(_ expr: Expr?) throws -> Expr {
    switch expr {
      case .none:
        return .port(self.context.outputPort)
      case .some(.port(let port)):
        guard port.isOutputPort else {
          throw EvalError.typeError(expr!, [.outputPortType])
        }
        self.context.outputPort = port
        return .void
      default:
        throw EvalError.typeError(expr!, [.outputPortType])
    }
  }
  
  func currentErrorPort(_ expr: Expr?) throws -> Expr {
    switch expr {
      case .none:
        return .port(self.context.errorPort)
      case .some(.port(let port)):
        guard port.isOutputPort else {
          throw EvalError.typeError(expr!, [.outputPortType])
        }
        self.context.errorPort = port
        return .void
      default:
        throw EvalError.typeError(expr!, [.outputPortType])
    }
  }
  
  func isEofObject(_ expr: Expr) -> Expr {
    return .makeBoolean(expr == .eof)
  }
  
  func eofObject() -> Expr {
    return .eof
  }
  
  func read(_ expr: Expr?) throws -> Expr {
    let input = try self.textInputFrom(expr)
    let parser = Parser(symbols: self.context.symbols, input: input)
    do {
      return try parser.parse(false)
    } catch SyntaxError.empty {
      return .eof
    } catch let error {
      throw error
    }
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
    return .makeBoolean(!input.readMightBlock)
  }
  
  func readToken(_ expr: Expr?, _ delim: Expr?) throws -> Expr {
    let input = try self.textInputFrom(expr)
    let delimiters = delim == nil ? CharacterSet.whitespacesAndNewlines
                                  : CharacterSet(charactersIn: try delim!.asString())
    guard let str = input.readToken(delimiters: delimiters) else {
      return .eof
    }
    return .makeString(str)
  }
  
  func readLine(_ expr: Expr?) throws -> Expr {
    let input = try self.textInputFrom(expr)
    guard let str = input.readLine() else {
      return .eof
    }
    return .makeString(str)
  }
  
  func readString(_ nchars: Expr, expr: Expr?) throws -> Expr {
    let input = try self.textInputFrom(expr)
    guard let str = input.readString(try nchars.asInt()) else {
      return .eof
    }
    return .makeString(str)
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
    return .makeBoolean(!input.readMightBlock)
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
                                            .makeNumber(bvector.value.count),
                                            .makeNumber(0)) else {
      throw EvalError.argumentCountError(formals: 4, args: .pair(bvec, .makeList(args)))
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
    guard try self.textOutputFrom(port).write(expr.asUniChar()) else {
      throw EvalError.cannotWriteToPort(port ?? .port(self.context.outputPort))
    }
    return .void
  }
  
  func writeString(_ expr: Expr, args: Arguments) throws -> Expr {
    if args.count < 2 {
      guard try self.textOutputFrom(args.first).writeString(expr.asString()) else {
        throw EvalError.cannotWriteToPort(args.first ?? .port(self.context.outputPort))
      }
    } else {
      let chars = try expr.asString().utf16
      guard let (port, s, e) = args.optional(.port(self.context.outputPort),
                                             .makeNumber(chars.count),
                                             .makeNumber(0)) else {
        throw EvalError.argumentCountError(formals: 4, args: .pair(expr, .makeList(args)))
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
    guard try self.binaryOutputFrom(port).write(expr.asUInt8()) else {
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
                                             .makeNumber(bvector.count),
                                             .makeNumber(0)) else {
                                              throw EvalError.argumentCountError(formals: 4, args: .pair(expr, .makeList(args)))
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
