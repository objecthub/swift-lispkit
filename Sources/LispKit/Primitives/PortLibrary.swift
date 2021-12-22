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
  
  /// Exported parameter objects
  public let outputPortParam: Procedure
  public let inputPortParam: Procedure
  public let errorPortParam: Procedure
  
  /// Initialize port library, in particular its parameter objects.
  public required init(in context: Context) throws {
    self.outputPortParam = Procedure(.null, .port(context.outputPort))
    self.inputPortParam = Procedure(.null, .port(context.inputPort))
    self.errorPortParam = Procedure(.null, .port(context.outputPort))
    try super.init(in: context)
  }
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "port"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"],    "define", "lambda", "quote")
    self.`import`(from: ["lispkit", "control"], "let", "let*", "if")
    self.`import`(from: ["lispkit", "list"],    "car", "cons")
    self.`import`(from: ["lispkit", "dynamic"], "parameterize")
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define("current-output-port", as: self.outputPortParam)
    self.define("current-input-port", as: self.inputPortParam)
    self.define("current-error-port", as: self.errorPortParam)
    self.define("default-output-port", as: .port(context.outputPort))
    self.define("default-input-port", as: .port(context.inputPort))
    self.define(Procedure("port?", isPort))
    self.define(Procedure("input-port?", isInputPort))
    self.define(Procedure("output-port?", isOutputPort))
    self.define(Procedure("textual-port?", isTextualPort))
    self.define(Procedure("binary-port?", isBinaryPort))
    self.define(Procedure("input-port-open?", isInputPortOpen))
    self.define(Procedure("output-port-open?", isOutputPortOpen))
    self.define(Procedure("open-input-file", openInputFile))
    self.define(Procedure("open-binary-input-file", openBinaryInputFile))
    self.define(Procedure("open-input-asset", openInputAsset))
    self.define(Procedure("open-binary-input-asset", openBinaryInputAsset))
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
      "(define (with-input-from-port new thunk) (parameterize ((current-input-port new)) (thunk)))")
    self.define("with-output-to-port", via:
      "(define (with-output-to-port new thunk) (parameterize ((current-output-port new)) (thunk)))")
    self.define("with-input-from-file", via:
      "(define (with-input-from-file file thunk)",
      "  (let ((new (open-input-file file)))",
      "    (parameterize ((current-input-port new))",
      "      (let ((res (thunk))) (close-input-port new) res))))")
    self.define("with-output-to-file", via:
      "(define (with-output-to-file file thunk)",
      "  (let ((new (open-output-file file)))",
      "    (parameterize ((current-output-port new))",
      "      (let ((res (thunk))) (close-output-port new) res))))")
    self.define("with-input-from-string", via:
      "(define (with-input-from-string str thunk)",
      "  (let ((new (open-input-string str)))",
      "    (parameterize ((current-input-port new))",
      "      (let ((res (thunk))) (close-input-port new) res))))")
    self.define("with-output-to-string", via:
      "(define (with-output-to-string thunk)",
      "  (let ((new (open-output-string)))",
      "    (parameterize ((current-output-port new))",
      "      (let ((res (thunk))) (close-output-port new) res))))")
    self.define("call-with-output-string", via:
      "(define (call-with-output-string procedure)",
      "  (let ((port (open-output-string)))",
      "    (procedure port)",
      "    (get-output-string port)))")
    self.define("call-with-output-bytevector", via:
      "(define (call-with-output-bytevector procedure)",
      "  (let ((port (open-output-bytevector)))",
      "    (procedure port)",
      "    (get-output-bytevector port)))")
    self.define("with-input-from-url", via:
      "(define (with-input-from-url url thunk)",
      "  (let ((new (open-input-url url)))",
      "    (parameterize ((current-input-port new))",
      "      (let ((res (thunk))) (close-input-port new) res))))")
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
    self.define("try-call-with-input-url", via:
      "(define (try-call-with-input-url url proc thunk)",
      "  (let ((port (open-input-url url 60.0 #f)))",
      "    (if port",
      "        (car (cons (proc port) (close-input-port port)))",
      "        (thunk))))")
  }
  
  public var outputPort: Port? {
    guard case .some(.port(let port)) = self.context.evaluator.getParam(self.outputPortParam) else {
      return nil
    }
    return port
  }
  
  public var inputPort: Port? {
    guard case .some(.port(let port)) = self.context.evaluator.getParam(self.inputPortParam) else {
      return nil
    }
    return port
  }
  
  public var errorPort: Port? {
    guard case .some(.port(let port)) = self.context.evaluator.getParam(self.inputPortParam) else {
      return nil
    }
    return port
  }
  
  public func defaultPort(_ param: Procedure) throws -> Port {
    guard let value = self.context.evaluator.getParam(param) else {
      throw RuntimeError.eval(.invalidDefaultPort, .false)
    }
    guard case .port(let port) = value else {
      throw RuntimeError.eval(.invalidDefaultPort, value)
    }
    return port
  }
  
  func textInputFrom(_ expr: Expr?, open: Bool = false) throws -> TextInput {
    let port = try expr?.asPort() ?? self.defaultPort(self.inputPortParam)
    guard !open || port.isOpen else {
      throw RuntimeError.eval(.portClosed, .port(port))
    }
    guard case .textInputPort(let input) = port.kind else {
      throw RuntimeError.type(.port(port), expected: [.textInputPortType])
    }
    return input
  }
  
  func binaryInputFrom(_ expr: Expr?, open: Bool = false) throws -> BinaryInput {
    let port = try expr?.asPort() ?? self.defaultPort(self.inputPortParam)
    guard !open || port.isOpen else {
      throw RuntimeError.eval(.portClosed, .port(port))
    }
    guard case .binaryInputPort(let input) = port.kind else {
      throw RuntimeError.type(.port(port), expected: [.binaryInputPortType])
    }
    return input
  }
  
  func textOutputFrom(_ expr: Expr?, open: Bool = false) throws -> TextOutput {
    let port = try expr?.asPort() ?? self.defaultPort(self.outputPortParam)
    guard !open || port.isOpen else {
      throw RuntimeError.eval(.portClosed, .port(port))
    }
    guard case .textOutputPort(let output) = port.kind else {
      throw RuntimeError.type(.port(port), expected: [.textInputPortType])
    }
    return output
  }
  
  func binaryOutputFrom(_ expr: Expr?, open: Bool = false) throws -> BinaryOutput {
    let port = try expr?.asPort() ?? self.defaultPort(self.outputPortParam)
    guard !open || port.isOpen else {
      throw RuntimeError.eval(.portClosed, .port(port))
    }
    guard case .binaryOutputPort(let output) = port.kind else {
      throw RuntimeError.type(.port(port), expected: [.binaryInputPortType])
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
  
  func openInputFile(_ expr: Expr, _ fail: Expr?) throws -> Expr {
    let filename =
      self.context.fileHandler.path(try expr.asPath(),
                                    relativeTo: self.context.evaluator.currentDirectoryPath)
    guard let input = BinaryInput(path: filename,
                                  abortionCallback: self.context.evaluator.isAbortionRequested) else {
      if let fail = fail {
        return fail
      } else {
        throw RuntimeError.eval(.cannotOpenFile, .makeString(filename))
      }
    }
    return .port(Port(input: TextInput(input: input)))
  }
  
  func openBinaryInputFile(_ expr: Expr, _ fail: Expr?) throws -> Expr {
    let filename =
      self.context.fileHandler.path(try expr.asPath(),
                                    relativeTo: self.context.evaluator.currentDirectoryPath)
    guard let input = BinaryInput(path: filename,
                                  abortionCallback: self.context.evaluator.isAbortionRequested) else {
      if let fail = fail {
        return fail
      } else {
        throw RuntimeError.eval(.cannotOpenFile, .makeString(filename))
      }
    }
    return .port(Port(input: input))
  }
  
  func openInputAsset(_ expr: Expr, _ type: Expr, _ dir: Expr?) throws -> Expr {
    if let filename = self.context.fileHandler.assetFilePath(
                        forFile: try expr.asString(),
                        ofType: try type.asString(),
                        inFolder: try dir?.asPath(),
                        relativeTo: self.context.evaluator.currentDirectoryPath) {
      guard let input =
                  BinaryInput(path: filename,
                              abortionCallback: self.context.evaluator.isAbortionRequested) else {
        throw RuntimeError.eval(.cannotOpenAsset, expr, type)
      }
      return .port(Port(input: TextInput(input: input)))
    } else {
      throw RuntimeError.eval(.cannotOpenAsset, expr, type)
    }
  }
  
  func openBinaryInputAsset(_ expr: Expr, _ type: Expr, _ dir: Expr?) throws -> Expr {
    if let filename = self.context.fileHandler.assetFilePath(
                        forFile: try expr.asString(),
                        ofType: try type.asString(),
                        inFolder: try dir?.asPath(),
                        relativeTo: self.context.evaluator.currentDirectoryPath) {
      guard let input =
                  BinaryInput(path: filename,
                              abortionCallback: self.context.evaluator.isAbortionRequested) else {
        throw RuntimeError.eval(.cannotOpenAsset, expr, type)
      }
      return .port(Port(input: input))
    } else {
      throw RuntimeError.eval(.cannotOpenAsset, expr, type)
    }
  }
  
  func openOutputFile(_ expr: Expr, _ fail: Expr?) throws -> Expr {
    let filename =
      self.context.fileHandler.path(try expr.asPath(),
                                    relativeTo: self.context.evaluator.currentDirectoryPath)
    guard let output = BinaryOutput(path: filename) else {
      if let fail = fail {
        return fail
      } else {
        throw RuntimeError.eval(.cannotOpenFile, .makeString(filename))
      }
    }
    return .port(Port(output: TextOutput(output: output)))
  }
  
  func openBinaryOutputFile(_ expr: Expr, _ fail: Expr?) throws -> Expr {
    let filename =
      self.context.fileHandler.path(try expr.asPath(),
                                    relativeTo: self.context.evaluator.currentDirectoryPath)
    guard let output = BinaryOutput(path: filename) else {
      if let fail = fail {
        return fail
      } else {
        throw RuntimeError.eval(.cannotOpenFile, .makeString(filename))
      }
    }
    return .port(Port(output: output))
  }
  
  func openInputString(_ expr: Expr) throws -> Expr {
    return .port(Port(input: TextInput(string: try expr.asString(),
                                       abortionCallback: self.context.evaluator.isAbortionRequested)))
  }
  
  func openOutputString() -> Expr {
    return .port(Port(output: TextOutput()))
  }
  
  func openInputBytevector(_ expr: Expr) throws -> Expr {
    let input = BinaryInput(data: try expr.asByteVector().value,
                            abortionCallback: self.context.evaluator.isAbortionRequested)
    return .port(Port(input: input))
  }
  
  func openOutputBytevector() -> Expr {
    return .port(Port(output: BinaryOutput()))
  }
  
  func openInputUrl(_ expr: Expr, _ timeout: Expr?, _ fail: Expr?) throws -> Expr {
    let url = try expr.asURL()
    guard let input = BinaryInput(url: url,
                                  timeout: try timeout?.asDouble(coerce: true) ?? 60.0,
                                  abortionCallback: self.context.evaluator.isAbortionRequested) else {
      if let fail = fail {
        return fail
      } else {
        throw RuntimeError.eval(.cannotOpenUrl, .makeString(url.description))
      }
    }
    return .port(Port(input: TextInput(input: input)))
  }
  
  func openBinaryInputUrl(_ expr: Expr, _ timeout: Expr?, _ fail: Expr?) throws -> Expr {
    let url = try expr.asURL()
    guard let input = BinaryInput(url: url,
                                  timeout: try timeout?.asDouble(coerce: true) ?? 60.0,
                                  abortionCallback: self.context.evaluator.isAbortionRequested) else {
      if let fail = fail {
        return fail
      } else {
        throw RuntimeError.eval(.cannotOpenUrl, .makeString(url.description))
      }
    }
    return .port(Port(input: input))
  }
  
  func getOutputString(_ expr: Expr) throws -> Expr {
    guard let buffer = try expr.asPort().outputString else {
      throw RuntimeError.type(expr, expected: [.textOutputPortType])
    }
    return .makeString(buffer)
  }
  
  func getOutputBytevector(_ expr: Expr) throws -> Expr {
    guard let buffer = try expr.asPort().outputBinary else {
      throw RuntimeError.type(expr, expected: [.binaryOutputPortType])
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
  
  func isEofObject(_ expr: Expr) -> Expr {
    return .makeBoolean(expr == .eof)
  }
  
  func eofObject() -> Expr {
    return .eof
  }
  
  func read(_ expr: Expr?) throws -> Expr {
    let input = try self.textInputFrom(expr, open: true)
    let parser = Parser(symbols: self.context.symbols, input: input)
    do {
      let res = try parser.parse(false).datum
      if case .eof = res {
        return res
      }
      input.unread() // put the look-ahead character back into the input stream
      return res
    } catch let error as RuntimeError {
      guard case .syntax(.empty) = error.descriptor else {
        throw error
      }
      return .eof
    }
  }
  
  func readChar(_ expr: Expr?) throws -> Expr {
    let input = try self.textInputFrom(expr, open: true)
    guard let ch = input.read() else {
      return .eof
    }
    return .char(ch)
  }
  
  func peekChar(_ expr: Expr?) throws -> Expr {
    let input = try self.textInputFrom(expr, open: true)
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
    let input = try self.textInputFrom(expr, open: true)
    let delimiters = delim == nil ? CharacterSet.whitespacesAndNewlines
                                  : CharacterSet(charactersIn: try delim!.asString())
    guard let str = input.readToken(delimiters: delimiters) else {
      return .eof
    }
    return .makeString(str)
  }
  
  func readLine(_ expr: Expr?) throws -> Expr {
    let input = try self.textInputFrom(expr, open: true)
    guard let str = input.readLine() else {
      return .eof
    }
    return .makeString(str)
  }
  
  func readString(_ nchars: Expr, expr: Expr?) throws -> Expr {
    let input = try self.textInputFrom(expr, open: true)
    guard let str = input.readString(try nchars.asInt()) else {
      return .eof
    }
    return .makeString(str)
  }
  
  func readU8(_ expr: Expr?) throws -> Expr {
    let input = try self.binaryInputFrom(expr, open: true)
    guard let byte = input.read() else {
      return .eof
    }
    return .fixnum(Int64(byte))
  }
  
  func peekU8(_ expr: Expr?) throws -> Expr {
    let input = try self.binaryInputFrom(expr, open: true)
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
    let input = try self.binaryInputFrom(expr, open: true)
    guard let bytes = input.readMany(try nbytes.asInt()) else {
      return .eof
    }
    return .bytes(MutableBox(bytes))
  }
  
  func readBytevectorSet(_ bvec: Expr, args: Arguments) throws -> Expr {
    let bvector = try bvec.asByteVector()
    guard let (pexpr, s, e) = args.optional(.port(try self.defaultPort(self.inputPortParam)),
                                            .makeNumber(bvector.value.count),
                                            .makeNumber(0)) else {
      throw RuntimeError.argumentCount(num: 4, args: .pair(bvec, .makeList(args)))
    }
    let (start, end) = (try s.asInt(), try e.asInt())
    guard start >= 0 && start < bvector.value.count else {
      throw RuntimeError.range(parameter: 3,
                               of: "read-bytevector!",
                               s,
                               min: 0,
                               max: Int64(bvector.value.count - 1))
    }
    guard end >= start && end <= bvector.value.count else {
      throw RuntimeError.range(parameter: 4,
                               of: "read-bytevector!",
                               e,
                               min: Int64(start),
                               max: Int64(bvector.value.count))
    }
    let input = try self.binaryInputFrom(pexpr)
    guard let n = input.readInto(&bvector.value, start: start, end: end) else {
      return .eof
    }
    return .fixnum(Int64(n))
  }
  
  func write(_ expr: Expr, port: Expr?) throws -> Expr {
    let output = try self.textOutputFrom(port, open: true)
    guard output.writeString(expr.description) else {
      let outPort = try port ?? .port(self.defaultPort(self.outputPortParam))
      throw RuntimeError.eval(.cannotWriteToPort, outPort)
    }
    return .void
  }
  
  /// TODO: Implement this function correctly; i.e. force usage of datum labels for shared
  /// structures.
  func writeShared(_ expr: Expr, port: Expr?) throws -> Expr {
    let output = try self.textOutputFrom(port, open: true)
    guard output.writeString(expr.description) else {
      let outPort = try port ?? .port(self.defaultPort(self.outputPortParam))
      throw RuntimeError.eval(.cannotWriteToPort, outPort)
    }
    return .void
  }
  
  /// TODO: Implement this function correctly; i.e. never use datum labels for shared
  /// structures.
  func writeSimple(_ expr: Expr, port: Expr?) throws -> Expr {
    let output = try self.textOutputFrom(port, open: true)
    guard output.writeString(expr.description) else {
      let outPort = try port ?? .port(self.defaultPort(self.outputPortParam))
      throw RuntimeError.eval(.cannotWriteToPort, outPort)
    }
    return .void
  }
  
  func display(_ expr: Expr, port: Expr? = nil) throws -> Expr {
    let output = try self.textOutputFrom(port, open: true)
    guard output.writeString(expr.unescapedDescription) else {
      let outPort = try port ?? .port(self.defaultPort(self.outputPortParam))
      throw RuntimeError.eval(.cannotWriteToPort, outPort)
    }
    return .void
  }
  
  func newline(_ port: Expr?) throws -> Expr {
    guard try self.textOutputFrom(port, open: true).writeString("\n") else {
      let outPort = try port ?? .port(self.defaultPort(self.outputPortParam))
      throw RuntimeError.eval(.cannotWriteToPort, outPort)
    }
    return .void
  }
  
  func writeChar(_ expr: Expr, port: Expr?) throws -> Expr {
    guard try self.textOutputFrom(port, open: true).write(expr.asUniChar()) else {
      let outPort = try port ?? .port(self.defaultPort(self.outputPortParam))
      throw RuntimeError.eval(.cannotWriteToPort, outPort)
    }
    return .void
  }
  
  func writeString(_ expr: Expr, args: Arguments) throws -> Expr {
    if args.count < 2 {
      guard try self.textOutputFrom(args.first, open: true).writeString(expr.asString()) else {
        let outPort = try args.first ?? .port(self.defaultPort(self.outputPortParam))
        throw RuntimeError.eval(.cannotWriteToPort, outPort)
      }
    } else {
      let chars = try expr.asString().utf16
      guard let (port, s, e) = args.optional(.port(try self.defaultPort(self.outputPortParam)),
                                             .makeNumber(chars.count),
                                             .makeNumber(0)) else {
        throw RuntimeError.argumentCount(of: "write-string",
                                         min: 4,
                                         max: 4,
                                         args: .pair(expr, .makeList(args)))
      }
      let (start, end) = (try s.asInt(), try e.asInt())
      guard start >= 0 && start < chars.count else {
        throw RuntimeError.range(parameter: 3,
                                 of: "write-string",
                                 s,
                                 min: 0,
                                 max: Int64(chars.count - 1))
      }
      guard end >= start && end <= chars.count else {
        throw RuntimeError.range(parameter: 4,
                                 of: "write-string",
                                 e,
                                 min: Int64(start),
                                 max: Int64(chars.count))
      }
      var uniChars: [UniChar] = []
      for ch in chars[chars.index(chars.startIndex, offsetBy: start)..<chars.index(chars.startIndex, offsetBy: end)] {
        uniChars.append(ch)
      }
      let str = String(utf16CodeUnits: uniChars, count: end - start)
      guard try self.textOutputFrom(port).writeString(str) else {
        throw RuntimeError.eval(.cannotWriteToPort, port)
      }
    }
    return .void
  }
  
  func writeU8(_ expr: Expr, port: Expr?) throws -> Expr {
    guard try self.binaryOutputFrom(port, open: true).write(expr.asUInt8()) else {
      let outPort = try port ?? .port(self.defaultPort(self.outputPortParam))
      throw RuntimeError.eval(.cannotWriteToPort, outPort)
    }
    return .void
  }
  
  func writeBytevector(_ expr: Expr, args: Arguments) throws -> Expr {
    let bvector = try expr.asByteVector().value
    if args.count < 2 {
      guard try self.binaryOutputFrom(args.first, open: true)
                    .writeFrom(bvector, start: 0, end: bvector.count) else {
        let outPort = try args.first ?? .port(self.defaultPort(self.outputPortParam))
        throw RuntimeError.eval(.cannotWriteToPort, outPort)
      }
    } else {
      guard let (port, s, e) = args.optional(.port(try self.defaultPort(self.outputPortParam)),
                                             .makeNumber(bvector.count),
                                             .makeNumber(0)) else {
        throw RuntimeError.argumentCount(of: "write-bytevector",
                                         min: 4,
                                         max: 4,
                                         args: .pair(expr, .makeList(args)))
      }
      let (start, end) = (try s.asInt(), try e.asInt())
      guard start >= 0 && start < bvector.count else {
        throw RuntimeError.range(parameter: 3,
                                 of: "write-bytevector",
                                 s,
                                 min: 0,
                                 max: Int64(bvector.count - 1))
      }
      guard end >= start && end <= bvector.count else {
        throw RuntimeError.range(parameter: 4,
                                 of: "write-bytevector",
                                 e,
                                 min: Int64(start),
                                 max: Int64(bvector.count))
      }
      guard try self.binaryOutputFrom(port).writeFrom(bvector, start: start, end: end) else {
        throw RuntimeError.eval(.cannotWriteToPort, port)
      }
    }
    return .void
  }
  
  func flushOutputPort(_ port: Expr?) throws -> Expr {
    let port = try port?.asPort() ?? self.defaultPort(self.outputPortParam)
    switch port.kind {
      case .binaryOutputPort(let output):
        output.flush(true)
      case .textOutputPort(let output):
        output.flush(true)
      default:
        throw RuntimeError.type(.port(port), expected: [.outputPortType])
    }
    return .void
  }
}
