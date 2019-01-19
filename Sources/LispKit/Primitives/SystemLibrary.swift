//
//  SystemLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 10/12/2016.
//  Copyright © 2016, 2017 ObjectHub. All rights reserved.
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
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.currentDirectoryProc =
      Procedure(.procedure(Procedure("_validCurrentPath", self.validCurrentPath)),
                .makeString(self.context.initialHomePath ??
                            self.context.fileHandler.currentDirectoryPath))
    self.compileAndEvalFirstProc =
      Procedure("_compileAndEvalFirst", self.compileAndEvalFirst)
    self.define("current-directory", as: self.currentDirectoryProc)
    self.define(Procedure("file-path", self.filePath))
    self.define(Procedure("parent-file-path", self.parentFilePath))
    self.define(Procedure("file-path-root?", self.filePathRoot))
    self.define(Procedure("load", self.load))
    self.define(Procedure("file-exists?", self.fileExists))
    self.define(Procedure("directory-exists?", self.directoryExists))
    self.define(Procedure("file-or-directory-exists?", self.fileOrDirectoryExists))
    self.define(Procedure("delete-file", self.deleteFile))
    self.define(Procedure("delete-directory", self.deleteDirectory))
    self.define(Procedure("delete-file-or-directory", self.deleteFileOrDirectory))
    self.define(Procedure("copy-file", self.copyFile))
    self.define(Procedure("copy-directory", self.copyDirectory))
    self.define(Procedure("copy-file-or-directory", self.copyFileOrDirectory))
    self.define(Procedure("move-file", self.copyFile))
    self.define(Procedure("move-directory", self.copyDirectory))
    self.define(Procedure("move-file-or-directory", self.copyFileOrDirectory))
    self.define(Procedure("file-size", self.fileSize))
    self.define(Procedure("directory-list", self.directoryList))
    self.define(Procedure("make-directory", self.makeDirectory))
    self.define(Procedure("open-file", self.openFile))
    self.define(Procedure("get-environment-variable", self.getEnvironmentVariable))
    self.define(Procedure("get-environment-variables", self.getEnvironmentVariables))
    self.define(Procedure("command-line", self.commandLine))
    self.define(Procedure("gc", self.gc))
    self.define(Procedure("compile", self.compile))
    self.define(Procedure("disassemble", self.disassemble))
    self.define(Procedure("trace-calls", self.traceCalls))
    self.define(Procedure("available-symbols", self.availableSymbols))
    self.define(Procedure("loaded-libraries", self.loadedLibraries))
    self.define(Procedure("loaded-sources", self.loadedSources))
    self.define(Procedure("environment-info", self.environmentInfo))
    self.define(SpecialForm("time", self.compileTime))
    self.define(Procedure("seconds-from-gmt", self.secondsFromGmt))
    self.define(Procedure("current-second", self.currentSecond))
    self.define(Procedure("current-jiffy", self.currentJiffy))
    self.define(Procedure("jiffies-per-second", self.jiffiesPerSecond))
    self.define(Procedure("time-zone", self.timeZone))
    self.define(Procedure("second->date-time", self.secondToDateTime))
    self.define(Procedure("date-time->second", self.dateTimeToSecond))
    self.define(Procedure("date-time->string", self.dateTimeToString))
    self.define(Procedure("string->date-time", self.stringToDateTime))
    self.define(Procedure("features", self.features))
    self.define(Procedure("implementation-name", self.implementationName))
    self.define(Procedure("implementation-version", self.implementationVersion))
    self.define(Procedure("cpu-architecture", self.cpuArchitecture))
    self.define(Procedure("machine-name", self.machineName))
    self.define(Procedure("machine-model", self.machineModel))
    self.define(Procedure("os-type", self.osType))
    self.define(Procedure("os-version", self.osVersion))
    self.define(Procedure("os-name", self.osName))
    self.define(Procedure("os-release", self.osRelease))
    self.define(Procedure("current-user-name", self.currentUserName))
    self.define(Procedure("user-data", self.userData))
    self.define(Procedure("open-url", self.openUrl))
    self.define(Procedure("http-get", httpGet))
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
  
  private func load(args: Arguments) throws -> (Procedure, Exprs) {
    guard args.count == 1 || args.count == 2  else {
      throw RuntimeError.argumentCount(of: "load", min: 1, max: 2, args: .makeList(args))
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
    // Load file and parse expressions
    let exprs = try self.context.machine.parse(file: filename)
    let sourceDir = self.context.fileHandler.directory(filename)
    // Hand over work to `compileAndEvalFirst`
    return (self.compileAndEvalFirstProc, [exprs, .makeString(sourceDir), .env(environment!)])
  }
  
  private func compileAndEvalFirst(args: Arguments) throws -> (Procedure, Exprs) {
    guard args.count == 3 else {
      throw RuntimeError.argumentCount(min: 3, max: 3, args: .makeList(args))
    }
    let sourceDir = args[args.startIndex + 1]
    let env = args[args.startIndex + 2]
    switch args.first! {
      case .null:
        return (CoreLibrary.voidProc, [])
      case .pair(let expr, let rest):
        let source = Expr.pair(
          expr,
          .pair(.makeList(.procedure(self.compileAndEvalFirstProc),
                          .makeList(.symbol(Symbol(self.context.symbols.quote,
                                                   .global(self.context.environment))),
                                    rest),
                          sourceDir,
                          env),
                .null))
        let code = try Compiler.compile(expr: source,
                                        in: .global(try env.asEnvironment()),
                                        optimize: true,
                                        inDirectory: try sourceDir.asString())
        return (Procedure(code), [])
      default:
        throw RuntimeError.type(args.first!, expected: [.properListType])
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
      throw RuntimeError.eval(.unknownFile, expr)
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
      throw RuntimeError.eval(.unknownDirectory, expr)
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
      throw RuntimeError.eval(.unknownFile, fromPath)
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
      throw RuntimeError.eval(.unknownDirectory, fromPath)
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
      throw RuntimeError.eval(.unknownFile, fromPath)
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
      throw RuntimeError.eval(.unknownDirectory, fromPath)
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
      throw RuntimeError.eval(.unknownFile, expr)
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
  
  private func openFile(expr: Expr, withApp: Expr?, deactivate: Expr?) throws -> Expr {
    let path = self.context.fileHandler.path(try expr.asPath(),
                                             relativeTo: self.currentDirectoryPath)
    if let app = withApp {
      if let deactivate = deactivate {
        return .makeBoolean(NSWorkspace.shared.openFile(path,
                                                        withApplication: try app.asString(),
                                                        andDeactivate: deactivate.isTrue))
      } else {
        return .makeBoolean(NSWorkspace.shared.openFile(path,
                                                        withApplication: try app.asString()))
      }
    } else {
      return .makeBoolean(NSWorkspace.shared.openFile(path))
    }
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
  
  private func commandLine() -> Expr {
    let args = self.context.commandLineArguments.reversed()
    var res = Expr.null
    for arg in args {
      res = .pair(.makeString(arg), res)
    }
    return res
  }
  
  private func gc() -> Expr {
    self.context.delegate.print("BEFORE: " + context.objects.description + "\n")
    let res = Expr.fixnum(Int64(self.context.objects.collectGarbage()))
    self.context.delegate.print("AFTER: " + context.objects.description + "\n")
    return res
  }
  
  private func compileTime(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let exec, .null)) = expr else {
      throw RuntimeError.argumentCount(of: "time", num: 1, expr: expr)
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
    self.context.delegate.print(code.description)
    return .void
  }
  
  private func disassemble(expr: Expr) throws -> Expr {
    guard case .procedure(let proc) = expr else {
      throw RuntimeError.type(expr, expected: [.procedureType])
    }
    switch proc.kind {
      case .closure(_, let captured, let code):
        self.context.delegate.print(code.description)
        if captured.count > 0 {
          self.context.delegate.print("CAPTURED:\n")
          for i in captured.indices {
            self.context.delegate.print("  \(i): \(captured[i])\n")
          }
        }
      case .rawContinuation(let vmState):
        self.context.delegate.print(vmState.description + "\n")
        self.context.delegate.print(vmState.registers.code.description)
        if vmState.registers.captured.count > 0 {
          self.context.delegate.print("CAPTURED:\n")
          for i in vmState.registers.captured.indices {
            self.context.delegate.print("  \(i): \(vmState.registers.captured[i])\n")
          }
        }
      default:
        self.context.delegate.print("cannot disassemble \(expr)\n")
    }
    return .void
  }
  
  private func traceCalls(_ expr: Expr?) throws -> Expr {
    if let expr = expr {
      switch (expr) {
        case .false:
          self.context.machine.traceCalls = false
        default:
          self.context.machine.traceCalls = true
      }
    }
    return .makeBoolean(self.context.machine.traceCalls)
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
  
  private func loadedSources() -> Expr {
    var res = Expr.null
    for url in self.context.sourceManager.sourceUrls {
      res = .pair(.makeString(url.path), res)
    }
    return res
  }
  
  private func environmentInfo() -> Expr {
    let console = self.context.delegate
    console.print("OBJECT SIZES\n")
    console.print("  atom size          : \(MemoryLayout<Expr>.size) bytes\n")
    console.print("  atom stride size   : \(MemoryLayout<Expr>.stride) bytes\n")
    console.print("  instr size         : \(MemoryLayout<Instruction>.size) bytes\n")
    console.print("  instr stride size  : \(MemoryLayout<Instruction>.stride) bytes\n")
    console.print("MANAGED OBJECT POOL\n")
    console.print("  tracked objects    : \(self.context.objects.numTrackedObjects)\n")
    console.print("  tracked capacity   : \(self.context.objects.trackedObjectCapacity)\n")
    console.print("  managed objects    : \(self.context.objects.numManagedObjects)\n")
    console.print("  managed capacity   : \(self.context.objects.managedObjectCapacity)\n")
    console.print("MANAGED OBJECT DISTRIBUTION\n")
    for (typeName, count) in self.context.objects.managedObjectDistribution {
      console.print("  \(typeName): \(count)\n")
    }
    console.print("GARBAGE COLLECTOR\n")
    console.print("  gc cycles          : \(self.context.objects.cycles)\n")
    console.print("  last tag           : \(self.context.objects.tag)\n")
    console.print("GLOBAL LOCATIONS\n")
    console.print("  allocated locations: \(self.context.heap.locations.count)\n")
    return .void
  }
  
  private func secondsFromGmt() -> Expr {
    return .fixnum(Int64(NSTimeZone.local.secondsFromGMT()))
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
  
  private func timeZone(_ expr: Expr?) -> Expr {
    guard let timeZone = self.getTimeZone(expr) else {
      return .false
    }
    var abbrev = Expr.false
    if let a = timeZone.abbreviation() {
      abbrev = .makeString(a)
    }
    return .makeList(.makeString(timeZone.identifier),
                     abbrev,
                     .fixnum(Int64(timeZone.secondsFromGMT())))
  }
  
  private func secondToDateTime(_ seconds: Expr, _ timeZone: Expr?) throws -> Expr {
    guard let tzone = self.getTimeZone(timeZone) else {
      throw RuntimeError.eval(.invalidTimeZone, timeZone ?? .false)
    }
    return self.getDateComponents(Date(timeIntervalSince1970: try seconds.asDouble()), tzone)
  }
  
  private func dateTimeToSecond(_ dateTime: Expr, _ timeZone: Expr?) throws -> Expr {
    guard let tzone = self.getTimeZone(timeZone) else {
      throw RuntimeError.eval(.invalidTimeZone, timeZone ?? .false)
    }
    guard let (date, _) = self.getDate(dateTime, tzone) else {
      throw RuntimeError.eval(.invalidDateTime, dateTime)
    }
    return .makeNumber(date.timeIntervalSince1970)
  }
  
  private func dateTimeToString(_ dateTime: Expr, _ dateFormat: Expr?) throws -> Expr {
    guard let (date, tzone) = self.getDate(dateTime, TimeZone.current) else {
      throw RuntimeError.eval(.invalidDateTime, dateTime)
    }
    let formatter = DateFormatter()
    formatter.timeZone = tzone
    if let format = dateFormat {
      switch format {
        case .symbol(let sym):
          formatter.locale = Locale(identifier: sym.identifier)
          formatter.dateStyle = .short
          formatter.timeStyle = .medium
        default:
          formatter.dateFormat = try format.asString()
      }
    } else {
      formatter.locale = Locale.current
      formatter.dateStyle = .short
      formatter.timeStyle = .medium
    }
    return .makeString(formatter.string(from: date))
  }
  
  private func stringToDateTime(_ str: Expr,
                                _ timeZone: Expr?,
                                _ dateFormat: Expr?) throws -> Expr {
    guard let tzone = self.getTimeZone(timeZone) else {
      throw RuntimeError.eval(.invalidTimeZone, timeZone ?? .false)
    }
    let formatter = DateFormatter()
    formatter.timeZone = tzone
    if let format = dateFormat {
      switch format {
        case .symbol(let sym):
          formatter.locale = Locale(identifier: sym.identifier)
          formatter.dateStyle = .short
          formatter.timeStyle = .medium
        default:
          formatter.dateFormat = try format.asString()
      }
    } else {
      formatter.locale = Locale.current
      formatter.dateStyle = .short
      formatter.timeStyle = .medium
    }
    guard let date = formatter.date(from: try str.asString()) else {
      return .false
    }
    return self.getDateComponents(date, tzone)
  }
  
  private func getDateComponents(_ date: Date, _ tz: TimeZone) -> Expr {
    let dc = Calendar.current.dateComponents(in: tz, from: date)
    let dstOffset = tz.daylightSavingTimeOffset(for: date)
    return .makeList(.makeString(tz.identifier),
                     .fixnum(Int64(dc.year!)),
                     .fixnum(Int64(dc.month!)),
                     .fixnum(Int64(dc.day!)),
                     .fixnum(Int64(dc.hour!)),
                     .fixnum(Int64(dc.minute!)),
                     .fixnum(Int64(dc.second!)),
                     .fixnum(Int64(dc.nanosecond!)),
                     .fixnum(Int64(dc.weekday!)),
                     .fixnum(Int64(dc.weekOfYear!)),
                     .makeNumber(dstOffset))
  }
  
  private func getDate(_ dateTime: Expr, _ tzone: TimeZone) -> (Date, TimeZone)? {
    var dt = dateTime
    var tz = tzone
    switch dateTime {
      case .pair(.string(let str), let rest):
        guard let dttz = self.getTimeZone(.string(str)) else {
          return nil
        }
        tz = dttz
        dt = rest
      case .pair(.pair(let car, let cdr), let rest):
        guard let dttz = self.getTimeZone(.pair(car, cdr)) else {
          return nil
        }
        tz = dttz
        dt = rest
      case .pair(.false, let rest):
        dt = rest
      default:
        break
    }
    guard case .pair(.fixnum(let y), .pair(.fixnum(let m), .pair(.fixnum(let d), let time))) = dt,
          y >= 0 && y < Int.max, m >= 1 && m <= 12, d >= 1 && d <= 31 else {
      return nil
    }
    var hour: Int = 0
    var minute: Int = 0
    var second: Int = 0
    var nanosecond: Int = 0
    if case .pair(.fixnum(let hr), let rest) = time, hr >= 0 && hr <= 24 {
      hour = Int(hr)
      if case .pair(.fixnum(let min), let rest) = rest, min >= 0 && min <= 60 {
        minute = Int(min)
        if case .pair(.fixnum(let sec), let rest) = rest, sec >= 0 && sec <= 60 {
          second = Int(sec)
          if case .pair(.fixnum(let nano), _) = rest, nano >= 0 && nano <= Int.max {
            nanosecond = Int(nano)
          } else if !rest.isNull {
            return nil
          }
        } else if !rest.isNull {
          return nil
        }
      } else if !rest.isNull {
        return nil
      }
    } else if !time.isNull {
      return nil
    }
    let dc = DateComponents(calendar: Calendar.current,
                            timeZone: tz,
                            year: Int(y),
                            month: Int(m),
                            day: Int(d),
                            hour: hour,
                            minute: minute,
                            second: second,
                            nanosecond: nanosecond)
    guard dc.isValidDate, let date = dc.date else {
      return nil
    }
    return (date, tz)
  }
  
  private func getTimeZone(_ expr: Expr?) -> TimeZone? {
    guard let timezone = expr else {
      return TimeZone.current
    }
    switch timezone {
      case .pair(let tzid, let rest):
        switch tzid {
          case .fixnum(_), .string(_):
            if let res = self.getTimeZone(tzid) {
              return res
            } else if case .pair(_, _) = rest {
              return self.getTimeZone(rest)
            } else {
              return nil
            }
          default:
            return nil
        }
      case .fixnum(let delta):
        if delta > Int64(Int.min) && delta < Int64(Int.max) {
          return TimeZone(secondsFromGMT: Int(delta))
        } else {
          return nil
        }
      case .string(let str):
        return TimeZone(identifier: str as String) ?? TimeZone(abbreviation: str as String)
      default:
        return nil
    }
  }
  
  private func features() -> Expr {
    var res: Expr = .null
    for feature in self.context.features {
      res = .pair(.symbol(self.context.symbols.intern(feature)), res)
    }
    return res
  }
  
  private func implementationName() -> Expr {
    if let name = self.context.implementationName {
      return .makeString(name)
    } else {
      return .false
    }
  }
  
  private func implementationVersion() -> Expr {
    if let version = self.context.implementationVersion {
      return .makeString(version)
    } else {
      return .false
    }
  }
  
  private func cpuArchitecture() -> Expr {
    return .makeString(Sysctl.machine)
  }
  
  private func machineName() -> Expr {
    return .makeString(Sysctl.hostName)
  }
  
  private func machineModel() -> Expr {
    return .makeString(Sysctl.model)
  }
  
  private func osType() -> Expr {
    return .makeString(Sysctl.osType)
  }
  
  private func osVersion() -> Expr {
    return .makeString(Sysctl.osVersion)
  }
  
  private func osName() -> Expr {
    #if os(macOS)
      return .makeString("macOS")
    #elseif os(iOS)
      return .makeString("iOS")
    #elseif os(Linux)
      return .makeString("Linux")
    #endif
  }
  
  private func osRelease() -> Expr {
    return .makeString("\(ProcessInfo.processInfo.operatingSystemVersion.majorVersion)." +
                       "\(ProcessInfo.processInfo.operatingSystemVersion.minorVersion)")
  }
  
  private func currentUserName() -> Expr {
    return .makeString(NSUserName())
  }
  
  private func userData(_ expr: Expr) throws -> Expr {
    let username = try expr.asString()
    guard let pw = getpwnam(username) else {
      return .false
    }
    let uid = pw.pointee.pw_uid
    let gid = pw.pointee.pw_gid
    let name = String(cString: pw.pointee.pw_name)
    let dir = NSHomeDirectoryForUser(username) ?? String(cString: pw.pointee.pw_dir)
    let gecos = String(cString: pw.pointee.pw_gecos)
    let shell = String(cString: pw.pointee.pw_shell)
    return .pair(.makeNumber(Int64(uid)),
                 .pair(.makeNumber(Int64(gid)),
                       .pair(.makeString(name),
                             .pair(.makeString(gecos),
                                   .pair(.makeString(dir),
                                         .pair(.makeString(shell), .null))))))
  }
  
  private func openUrl(_ expr: Expr) throws -> Expr {
    return .makeBoolean(NSWorkspace.shared.open(try expr.asURL()))
  }
  
  private func httpGet(_ expr: Expr, _ tout: Expr?) throws -> Expr {
    let url = try expr.asURL()
    let timeout = try tout?.asDouble(coerce: true) ?? HTTPInputStream.defaultTimeout
    guard let stream = HTTPInputStream(url: url) else {
      throw RuntimeError.eval(.cannotOpenUrl, .makeString(url.description))
    }
    stream.open(timeout: timeout)
    stream.waitForResponse()
    guard let input = BinaryInput(source: stream,
                                  url: url,
                                  abortionCallback: self.context.machine.isAbortionRequested) else {
      throw RuntimeError.eval(.cannotOpenUrl, .makeString(url.description))
    }
    var response = Expr.null
    guard let headerFields = stream.headerFields else {
      throw RuntimeError.eval(.cannotOpenUrl, .makeString(url.description))
    }
    for (key, value) in headerFields {
      response = .pair(.pair(.makeString(key), .makeString(value)), response)
    }
    stream.waitForData()
    let bytes: [UInt8]
    if let bs = input.readMany(Int.max) {
      bytes = bs
    } else {
      bytes = []
    }
    return .values(.pair(response, .pair(.bytes(MutableBox(bytes)), .null)))
  }
}

