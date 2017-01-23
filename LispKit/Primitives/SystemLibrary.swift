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
  
  /// Container for the current directory path parameter.
  private var currentDirectoryProc: Procedure!
  private var compileAndEvalFirstProc: Procedure!
  
  public var currentDirectoryPath: String {
    get {
      do {
        return try self.context.machine.getParam(self.currentDirectoryProc)!.asString()
      } catch {
        preconditionFailure("current directory path not a string")
      }
    }
    set {
      _ = self.context.machine.setParam(self.currentDirectoryProc, to: .makeString(newValue))
    }
  }
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "system"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "dynamic"], "call-with-current-continuation")
    self.`import`(from: ["lispkit", "base"], "define", "set!", "lambda")
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.currentDirectoryProc =
      Procedure(.procedure(Procedure("_validCurrentPath", self.validCurrentPath)),
                .makeString(self.context.fileHandler.currentDirectoryPath))
    self.compileAndEvalFirstProc =
      Procedure("_compileAndEvalFirst", self.compileAndEvalFirst)
    self.define("current-directory", as: .procedure(self.currentDirectoryProc))
    self.define(Procedure("file-path", filePath))
    self.define(Procedure("parent-file-path", parentFilePath))
    self.define(Procedure("file-path-root?", filePathRoot))
    self.define(Procedure("load", load))
    self.define(Procedure("file-exists?", fileExists))
    self.define(Procedure("directory-exists?", directoryExists))
    self.define(Procedure("file-or-directory-exists?", fileOrDirectoryExists))
    self.define(Procedure("delete-file", deleteFile))
    self.define(Procedure("delete-directory", deleteDirectory))
    self.define(Procedure("delete-file-or-directory", deleteFileOrDirectory))
    self.define(Procedure("copy-file", copyFile))
    self.define(Procedure("copy-directory", copyDirectory))
    self.define(Procedure("copy-file-or-directory", copyFileOrDirectory))
    self.define(Procedure("move-file", copyFile))
    self.define(Procedure("move-directory", copyDirectory))
    self.define(Procedure("move-file-or-directory", copyFileOrDirectory))
    self.define(Procedure("file-size", fileSize))
    self.define(Procedure("directory-list", directoryList))
    self.define(Procedure("make-directory", makeDirectory))
    self.define(Procedure("get-environment-variable", getEnvironmentVariable))
    self.define(Procedure("get-environment-variables", getEnvironmentVariables))
    self.define(Procedure("gc", gc))
    self.define(Procedure("emergency-exit", emergencyExit))
    self.define(Procedure("compile", compile))
    self.define(Procedure("disassemble", disassemble))
    self.define(Procedure("available-symbols", availableSymbols))
    self.define(Procedure("loaded-libraries", loadedLibraries))
    self.define(Procedure("environment-info", environmentInfo))
    self.define("time", as: SpecialForm(compileTime))
    self.define(Procedure("current-second", currentSecond))
    self.define(Procedure("current-jiffy", currentJiffy))
    self.define(Procedure("jiffies-per-second", jiffiesPerSecond))
    self.define(Procedure("_trigger-exit", triggerExit))
    self.define(Procedure("_report-error", reportError))
    self.define("exit", via: "(define exit 0)")
    self.execute("(call-with-current-continuation " +
                 "  (lambda (cont) (set! exit (lambda args (_trigger-exit cont args)))))")
    self.define("error", via: "(define error 0)")
    self.execute("(call-with-current-continuation " +
                 "  (lambda (cont) (set! error (lambda args (_report-error cont args)))))")
  }
  
  private func filePath(expr: Expr, base: Expr?) throws -> Expr {
    var root = self.currentDirectoryPath
    if let base = try base?.asPath() {
      root = self.context.fileHandler.path(base, relativeTo: self.currentDirectoryPath)
    }
    return .makeString(self.context.fileHandler.path(try expr.asString(), relativeTo: root))
  }
  
  private func parentFilePath(expr: Expr) throws -> Expr {
    return .makeString(
      self.context.fileHandler.directory(try expr.asString(),
                                         relativeTo: self.currentDirectoryPath))
  }
  
  private func filePathRoot(expr: Expr) throws -> Expr {
    return .makeBoolean(
      self.context.fileHandler.path(try expr.asString(),
                                    relativeTo: self.currentDirectoryPath) == "/")
  }
  
  private func load(args: Arguments) throws -> (Procedure, [Expr]) {
    guard args.count == 1 || args.count == 2  else {
      throw EvalError.argumentCountError(formals: 2, args: .makeList(args))
    }
    // Extract arguments
    let path = try args.first!.asPath()
    let filename =
      self.context.fileHandler.filePath(forFile: path,
                                        relativeTo: self.currentDirectoryPath) ??
      self.context.fileHandler.libraryFilePath(forFile: path,
                                               relativeTo: self.currentDirectoryPath) ??
      self.context.fileHandler.path(path, relativeTo: self.currentDirectoryPath)
    var environment = self.context.environment
    if args.count == 2 {
      environment = try args[args.startIndex + 1].asEnvironment()
    }
    // Load file
    let str = try String(contentsOfFile: filename, encoding: String.Encoding.utf8)
    // Parse file and store parsed expressions in a list
    let parser = Parser(symbols: self.context.symbols, src: str)
    var exprs = Exprs()
    while !parser.finished {
      exprs.append(try parser.parse())
    }
    // Hand over work to `compileAndEvalFirst`
    return (self.compileAndEvalFirstProc, [.makeList(exprs), .env(environment!)])
  }
  
  private func compileAndEvalFirst(args: Arguments) throws -> (Procedure, [Expr]) {
    guard args.count == 2 else {
      throw EvalError.argumentCountError(formals: 2, args: .makeList(args))
    }
    let env = args[args.startIndex + 1]
    switch args.first! {
      case .null:
        return (BaseLibrary.voidProc, [])
      case .pair(let expr, let rest):
        let source = Expr.pair(
          expr,
          .pair(.makeList(.procedure(self.compileAndEvalFirstProc),
                          .makeList(.symbol(Symbol(self.context.symbols.quote,
                                                   .global(self.context.environment))),
                                    rest),
                          env),
                .null))
        let code = try Compiler.compile(expr: source, in: .global(try env.asEnvironment()))
        return (Procedure(code), [])
      default:
        throw EvalError.typeError(args.first!, [.properListType])
    }
  }
  
  private func validCurrentPath(param: Expr, expr: Expr, setter: Expr) throws -> Expr {
    self.currentDirectoryPath =
      self.context.fileHandler.path(try expr.asPath(),
                                    relativeTo: self.currentDirectoryPath)
    return .makeString(self.currentDirectoryPath)
  }
  
  private func fileExists(expr: Expr) throws -> Expr {
    return .makeBoolean(
      self.context.fileHandler.isFile(atPath: try expr.asPath(),
                                      relativeTo: self.currentDirectoryPath))
  }
  
  private func directoryExists(expr: Expr) throws -> Expr {
    return .makeBoolean(
      self.context.fileHandler.isDirectory(atPath: try expr.asPath(),
                                           relativeTo: self.currentDirectoryPath))
  }
  
  private func fileOrDirectoryExists(expr: Expr) throws -> Expr {
    return .makeBoolean(
      self.context.fileHandler.itemExists(atPath: try expr.asPath(),
                                          relativeTo: self.currentDirectoryPath))
  }
  
  private func deleteFile(expr: Expr) throws -> Expr {
    let path = try expr.asPath()
    if self.context.fileHandler.isFile(atPath: path,
                                       relativeTo: self.currentDirectoryPath) {
      try self.context.fileHandler.deleteItem(atPath: path,
                                              relativeTo: self.currentDirectoryPath)
      return .void
    } else {
      throw EvalError.unknownFile(path)
    }
  }
  
  private func deleteDirectory(expr: Expr) throws -> Expr {
    let path = try expr.asPath()
    if self.context.fileHandler.isDirectory(atPath: path,
                                            relativeTo: self.currentDirectoryPath) {
      try self.context.fileHandler.deleteItem(atPath: path,
                                              relativeTo: self.currentDirectoryPath)
      return .void
    } else {
      throw EvalError.unknownDirectory(path)
    }
  }
  
  private func deleteFileOrDirectory(expr: Expr) throws -> Expr {
    try self.context.fileHandler.deleteItem(atPath: try expr.asPath(),
                                            relativeTo: self.currentDirectoryPath)
    return .void
  }
  
  private func copyFile(fromPath: Expr, toPath: Expr) throws -> Expr {
    let path = try fromPath.asPath()
    if self.context.fileHandler.isFile(atPath: path,
                                       relativeTo: self.currentDirectoryPath) {
      try self.context.fileHandler.copyItem(atPath: path,
                                            toPath: try toPath.asPath(),
                                            relativeTo: self.currentDirectoryPath)
      return .void
    } else {
      throw EvalError.unknownFile(path)
    }
  }
  
  private func copyDirectory(fromPath: Expr, toPath: Expr) throws -> Expr {
    let path = try fromPath.asPath()
    if self.context.fileHandler.isDirectory(atPath: path,
                                            relativeTo: self.currentDirectoryPath) {
      try self.context.fileHandler.copyItem(atPath: path,
                                            toPath: try toPath.asPath(),
                                            relativeTo: self.currentDirectoryPath)
      return .void
    } else {
      throw EvalError.unknownDirectory(path)
    }
  }
  
  
  private func copyFileOrDirectory(fromPath: Expr, toPath: Expr) throws -> Expr {
    try self.context.fileHandler.copyItem(atPath: try fromPath.asPath(),
                                          toPath: try toPath.asPath(),
                                          relativeTo: self.currentDirectoryPath)
    return .void
  }
  
  private func moveFile(fromPath: Expr, toPath: Expr) throws -> Expr {
    let path = try fromPath.asPath()
    if self.context.fileHandler.isFile(atPath: path,
                                       relativeTo: self.currentDirectoryPath) {
      try self.context.fileHandler.moveItem(atPath: path,
                                            toPath: try toPath.asPath(),
                                            relativeTo: self.currentDirectoryPath)
      return .void
    } else {
      throw EvalError.unknownFile(path)
    }
  }
  
  private func moveDirectory(fromPath: Expr, toPath: Expr) throws -> Expr {
    let path = try fromPath.asPath()
    if self.context.fileHandler.isDirectory(atPath: path,
                                            relativeTo: self.currentDirectoryPath) {
      try self.context.fileHandler.moveItem(atPath: path,
                                            toPath: try toPath.asPath(),
                                            relativeTo: self.currentDirectoryPath)
      return .void
    } else {
      throw EvalError.unknownDirectory(path)
    }
  }
  
  
  private func moveFileOrDirectory(fromPath: Expr, toPath: Expr) throws -> Expr {
    try self.context.fileHandler.moveItem(atPath: try fromPath.asPath(),
                                          toPath: try toPath.asPath(),
                                          relativeTo: self.currentDirectoryPath)
    return .void
  }
  
  private func fileSize(expr: Expr) throws -> Expr {
    guard let size = self.context.fileHandler.fileSize(atPath: try expr.asPath(),
                                                       relativeTo: self.currentDirectoryPath) else {
      throw EvalError.unknownFile(try expr.asPath())
    }
    return .fixnum(size)
  }
  
  private func directoryList(expr: Expr) throws -> Expr {
    let contents = try self.context.fileHandler.contentsOfDirectory(
      atPath: try expr.asPath(), relativeTo: self.currentDirectoryPath)
    var res = Expr.null
    for item in contents {
      res = .pair(.makeString(item), res)
    }
    return res
  }
  
  private func makeDirectory(expr: Expr) throws -> Expr {
    try self.context.fileHandler.makeDirectory(atPath: try expr.asPath(),
                                               relativeTo: self.currentDirectoryPath)
    return .void
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
  
  private func triggerExit(args: Arguments) throws -> (Procedure, [Expr]) {
    guard args.count == 2 else {
      throw EvalError.argumentCountError(formals: 2, args: .makeList(args))
    }
    let exit = try args.first!.asProcedure()
    let obj: Expr
    switch args[args.startIndex + 1] {
      case .null:
        obj = .true
      case .pair(let expr, .null):
        obj = expr
      default:
        throw EvalError.argumentCountError(formals: 1, args: args[args.startIndex + 1])
    }
    self.context.machine.exitTriggered = true
    return (exit, [obj])
  }
  
  private func reportError(args: Arguments) throws -> (Procedure, [Expr]) {
    guard args.count == 2 else {
      throw EvalError.argumentCountError(formals: 2, args: .makeList(args))
    }
    let exit = try args.first!.asProcedure()
    let obj: Expr
    switch args[args.startIndex + 1] {
      case .pair(let message, let rest):
        obj = .error(AnyError(CustomError(kind: "custom error",
                                          message: message.unescapedDescription,
                                          irritants: rest.toExprs().0)))
      default:
        throw EvalError.leastArgumentCountError(formals: 1, args: args[args.startIndex + 1])
    }
    return (exit, [obj])
  }
  
  private func emergencyExit(expr: Expr?) -> Expr {
    if self.context.delegate != nil {
      self.context.delegate!.emergencyExit(obj: expr)
    } else {
      NSApplication.shared().terminate(self)
    }
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
    context.console.print("  tracked objects    : \(self.context.objects.numTrackedObjects)\n")
    context.console.print("  tracked capacity   : \(self.context.objects.trackedObjectCapacity)\n")
    context.console.print("  managed objects    : \(self.context.objects.numManagedObjects)\n")
    context.console.print("  managed capacity   : \(self.context.objects.managedObjectCapacity)\n")
    context.console.print("GARBAGE COLLECTOR\n")
    context.console.print("  gc cycles          : \(self.context.objects.cycles)\n")
    context.console.print("  last tag           : \(self.context.objects.tag)\n")
    context.console.print("GLOBAL LOCATIONS\n")
    context.console.print("  allocated locations: \(self.context.heap.locations.count)\n")
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
