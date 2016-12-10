//
//  SystemLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 10/12/2016.
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

import Cocoa


public final class SystemLibrary: NativeLibrary {
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "system"]
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("load", load))
    self.define(Procedure("file-exists?", fileExists))
    self.define(Procedure("delete-file", deleteFile))
    self.define(Procedure("directory-list", directoryList))
    self.define(Procedure("get-environment-variable", getEnvironmentVariable))
    self.define(Procedure("get-environment-variables", getEnvironmentVariables))
    self.define(Procedure("gc", gc))
    self.define(Procedure("exit", exit))
    self.define(Procedure("compile", compile))
    self.define(Procedure("disassemble", disassemble))
    self.define(Procedure("available-symbols", availableSymbols))
    self.define(Procedure("loaded-libraries", loadedLibraries))
    self.define(Procedure("environment-info", environmentInfo))
    self.define("time", as: SpecialForm(compileTime))
    self.define(Procedure("current-second", currentSecond))
    self.define(Procedure("current-jiffy", currentJiffy))
    self.define(Procedure("jiffies-per-second", jiffiesPerSecond))
  }
  
  
  private func load(expr: Expr, e: Expr?) throws -> Expr {
    let path = try expr.asString()
    return self.context.machine.eval(
      file: self.context.fileHandler.filePath(forFile: path) ??
            self.context.fileHandler.libraryFilePath(forFile: path) ??
            path,
      in: .global(try e?.asEnvironment() ?? self.context.environment))
  }
  
  private func fileExists(expr: Expr) throws -> Expr {
    let path = try expr.asString()
    return .makeBoolean(self.context.fileHandler.fileExists(atPath: path))
  }
  
  private func deleteFile(expr: Expr) throws -> Expr {
    let path = try expr.asString()
    try self.context.fileHandler.deleteFile(atPath: path)
    return .void
  }
  
  private func directoryList(expr: Expr) throws -> Expr {
    let path = try expr.asString()
    let contents = try self.context.fileHandler.contentsOfDirectory(atPath: path)
    var res = Expr.null
    for item in contents {
      res = .pair(.makeString(item), res)
    }
    return res
  }

  private func getEnvironmentVariable(expr: Expr) throws -> Expr {
    let name = try expr.asString()
    guard let value = ProcessInfo.processInfo.environment[name] else {
      return .false
    }
    return .makeString(value)
  }
  
  private func getEnvironmentVariables() -> Expr {
    var alist = Expr.null
    for (name, value) in ProcessInfo.processInfo.environment {
      alist = .pair(.pair(.makeString(name), .makeString(value)), alist)
    }
    return alist
  }
  
  private func gc() -> Expr {
    context.console.print("BEFORE: " + context.objects.description + "\n")
    let res = Expr.fixnum(Int64(self.context.objects.collectGarbage()))
    context.console.print("AFTER: " + context.objects.description + "\n")
    return res
  }
  
  private func exit() -> Expr {
    NSApplication.shared().terminate(self)
    return .undef
  }
  
  private func compileTime(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let exec, .null)) = expr else {
      throw EvalError.argumentCountError(formals: 1, args: expr)
    }
    compiler.emit(.pushCurrentTime)
    try compiler.compile(exec, in: env, inTailPos: false)
    compiler.emit(.swap)
    compiler.emit(.pushCurrentTime)
    compiler.emit(.swap)
    compiler.emit(.flMinus)
    try compiler.pushValue(.makeString("elapsed time = "))
    compiler.emit(.display)
    compiler.emit(.display)
    compiler.emit(.newline)
    return false
  }
  
  private func compile(exprs: Arguments) throws -> Expr {
    var seq = Expr.null
    for expr in exprs.reversed() {
      seq = .pair(expr, seq)
    }
    let code = try Compiler.compile(expr: seq,
                                    in: self.context.global,
                                    optimize: true)
    context.console.print(code.description)
    return .void
  }
  
  private func disassemble(expr: Expr) throws -> Expr {
    guard case .procedure(let proc) = expr else {
      throw EvalError.typeError(expr, [.procedureType])
    }
    switch proc.kind {
    case .closure(_, let captured, let code):
      context.console.print(code.description)
      if captured.count > 0 {
        context.console.print("CAPTURED:\n")
        for i in captured.indices {
          context.console.print("  \(i): \(captured[i])\n")
        }
      }
    case .continuation(let vmState):
      context.console.print(vmState.description + "\n")
      context.console.print(vmState.registers.code.description)
      if vmState.registers.captured.count > 0 {
        context.console.print("CAPTURED:\n")
        for i in vmState.registers.captured.indices {
          context.console.print("  \(i): \(vmState.registers.captured[i])\n")
        }
      }
    default:
      context.console.print("cannot disassemble \(expr)\n")
    }
    return .void
  }
  
  private func availableSymbols() -> Expr {
    var res = Expr.null
    for sym in self.context.symbols {
      res = .pair(.symbol(sym), res)
    }
    return res
  }
  
  private func loadedLibraries() -> Expr {
    var res = Expr.null
    for library in self.context.libraries.loaded {
      res = .pair(library.name, res)
    }
    return res
  }
  
  private func environmentInfo() -> Expr {
    context.console.print("MANAGED OBJECT POOL\n")
    context.console.print("  tracked objects    : \(context.objects.numTrackedObjects)\n")
    context.console.print("  tracked capacity   : \(context.objects.trackedObjectCapacity)\n")
    context.console.print("  managed objects    : \(context.objects.numManagedObjects)\n")
    context.console.print("  managed capacity   : \(context.objects.managedObjectCapacity)\n")
    context.console.print("GARBAGE COLLECTOR\n")
    context.console.print("  gc cycles          : \(context.objects.cycles)\n")
    context.console.print("  last tag           : \(context.objects.tag)\n")
    context.console.print("GLOBAL LOCATIONS\n")
    context.console.print("  allocated locations: \(context.locations.count)\n")
    return .void
  }
  
  private func currentSecond() -> Expr {
    var time = timeval(tv_sec: 0, tv_usec: 0)
    gettimeofday(&time, nil)
    return .flonum(Double(time.tv_sec) + (Double(time.tv_usec) / 1000000.0))
  }
  
  private func currentJiffy() -> Expr {
    var time = timeval(tv_sec: 0, tv_usec: 0)
    gettimeofday(&time, nil)
    return .fixnum(Int64(time.tv_sec) * 1000 + Int64(time.tv_usec / 1000))
  }
  
  private func jiffiesPerSecond() -> Expr {
    return .fixnum(1000)
  }
}
