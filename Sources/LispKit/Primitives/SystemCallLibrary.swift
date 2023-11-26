//
//  SystemCallLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 07/06/2020.
//  Copyright © 2020-2023 ObjectHub. All rights reserved.
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
/// System Call library
///
public final class SystemCallLibrary: NativeLibrary {

  /// Imported native library
  private unowned var portLibrary: PortLibrary!

  /// Initialize port library, in particular its parameter objects.
  public required init(in context: Context) throws {
    try super.init(in: context)
  }

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "system", "call"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "port"], "current-output-port")
  }

  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("system-call", self.systemCall))
  }

  public override func initializations() {
    self.portLibrary = self.nativeLibrary(PortLibrary.self)
  }

  private func systemCall(expr: Expr,
                          arguments: Expr,
                          args: Arguments) throws -> Expr {
    guard let (e, op, ipt) = args.optional(.false,
                                           .port(portLibrary.outputPort ?? self.context.outputPort),
                                           .false) else {
      throw RuntimeError.argumentCount(of: "system-call",
                                       min: 2,
                                       max: 5,
                                       args: .pair(expr, .pair(arguments, .makeList(args))))
    }
    return try self.systemCall(binary: expr,
                               arguments: arguments,
                               environment: e,
                               outport: op,
                               input: ipt)
  }

  private func systemCall(binary expr: Expr,
                          arguments: Expr,
                          environment e: Expr,
                          outport op: Expr,
                          input ipt: Expr) throws -> Expr {
    let proc = Process()
    proc.currentDirectoryURL = URL(fileURLWithPath: self.context.evaluator.currentDirectoryPath,
                                   isDirectory: true)
    proc.executableURL =
      URL(fileURLWithPath: try expr.asPath(),
          relativeTo: URL(fileURLWithPath: self.context.evaluator.currentDirectoryPath,
                          isDirectory: true))
    var args: [String] = []
    var lst = arguments
    while case .pair(let car, let cdr) = lst {
      args.append(car.unescapedDescription)
      lst = cdr
    }
    proc.arguments = args
    if e.isTrue {
      var env: [String : String] = [:]
      lst = e
      while case .pair(let car, let cdr) = lst {
        guard case .pair(let name, let value) = car else {
          throw RuntimeError.type(car, expected: [.pairType])
        }
        env[try name.asString()] = value.unescapedDescription
        lst = cdr
      }
      proc.environment = env
    }
    let condition = NSCondition()
    proc.terminationHandler = { _ in
      condition.lock()
      condition.signal()
      condition.unlock()
    }
    let outputPipe = Pipe()
    let textOutput = op.isTrue ? try portLibrary.textOutputFrom(op) : nil
    if op.isTrue {
      outputPipe.fileHandleForReading.readabilityHandler = { handle in
        condition.lock()
        let data = handle.availableData
        if data.count > 0 {
          _ = textOutput?.writeString(String(data: data, encoding: .utf8) ?? "")
        }
        condition.unlock()
      }
      proc.standardOutput = outputPipe
      proc.standardError = outputPipe
    } else {
      proc.standardOutput = nil
      proc.standardError = nil
    }
    let inputPipe = Pipe()
    if ipt.isTrue {
      if let data = try ipt.asString().data(using: .utf8) {
        inputPipe.fileHandleForWriting.write(data)
      }
    }
    inputPipe.fileHandleForWriting.closeFile()
    proc.standardInput = inputPipe
    condition.lock()
    defer {
      if proc.isRunning {
        proc.terminate()
      }
      outputPipe.fileHandleForReading.readabilityHandler = nil
      inputPipe.fileHandleForWriting.writeabilityHandler = nil
      condition.unlock()
    }
    try proc.run()
    while proc.isRunning && !self.context.evaluator.isAbortionRequested() {
      condition.wait(until: Date(timeInterval: 0.7, since: Date()))
    }
    inputPipe.fileHandleForWriting.writeabilityHandler = nil
    outputPipe.fileHandleForReading.readabilityHandler = nil
    if op.isTrue {
      let restdata = outputPipe.fileHandleForReading.availableData
      if restdata.count > 0 {
        _ = textOutput?.writeString(String(data: restdata, encoding: .utf8) ?? "")
      }
    }
    return .fixnum(Int64(proc.isRunning ? -1 : proc.terminationStatus))
  }
}
