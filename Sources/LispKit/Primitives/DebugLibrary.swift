//
//  DebugLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 29/08/2020.
//  Copyright © 2020 ObjectHub. All rights reserved.
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
/// Debug library: LispKit-specific library providing access to system and code debugging 
/// functionality
///
public final class DebugLibrary: NativeLibrary {
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "debug"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
  }

  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("gc", self.gc))
    self.define(SpecialForm("time", self.compileTime))
    self.define(SpecialForm("time-values", self.compileTimeValues))
    self.define(SpecialForm("quote-expanded", self.compileQuoteExpanded))
    self.define(SpecialForm("quote-expanded-1", self.compileQuoteExpandedOne))
    self.define(Procedure("macroexpand", self.macroExpand))
    self.define(Procedure("macroexpand-1", self.macroExpandOne))
    self.define(Procedure("compile", self.compile))
    self.define(Procedure("disassemble", self.disassemble))
    self.define(Procedure("trace-calls", self.traceCalls))
    self.define(Procedure("procedure-trace?", self.procedureTrace))
    self.define(Procedure("set-procedure-trace!", self.setProcedureTrace))
    self.define(Procedure("available-symbols", self.availableSymbols))
    self.define(Procedure("loaded-libraries", self.loadedLibraries))
    self.define(Procedure("loaded-sources", self.loadedSources))
    self.define(Procedure("environment-info", self.environmentInfo))
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
  
  private func compileTimeValues(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let exec, .null)) = expr else {
      throw RuntimeError.argumentCount(of: "time-values", num: 1, expr: expr)
    }
    compiler.emit(.pushCurrentTime)
    try compiler.compile(exec, in: env, inTailPos: false)
    compiler.emit(.swap)
    compiler.emit(.pushCurrentTime)
    compiler.emit(.swap)
    compiler.emit(.flMinus)
    compiler.emit(.swap)
    compiler.emit(.flatpack(2))
    return false
  }
  
  private func compileQuoteExpanded(compiler: Compiler,
                             expr: Expr,
                             env: Env,
                             tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let invocation, .null)) = expr else {
      throw RuntimeError.argumentCount(of: "quote-expanded", num: 1, expr: expr)
    }
    var expr = invocation
    while let expanded = try compiler.macroExpand(expr, in: env) {
      expr = expanded
    }
    compiler.pushConstant(expr)
    return false
  }
  
  private func compileQuoteExpandedOne(compiler: Compiler,
                                expr: Expr,
                                env: Env,
                                tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let invocation, .null)) = expr else {
      throw RuntimeError.argumentCount(of: "quote-expanded-1", num: 1, expr: expr)
    }
    if let res = try compiler.macroExpand(invocation, in: env) {
      compiler.pushConstant(res)
    } else {
      compiler.pushConstant(invocation)
    }
    return false
  }
  
  private func macroExpand(expr: Expr, e: Expr?) throws -> Expr {
    let env = Env.global(try e?.asEnvironment() ?? self.context.environment)
    var expr = expr
    while let expanded = try Compiler.macroExpand(expr: expr, in: env) {
      expr = expanded
    }
    return expr.datum
  }
  
  private func macroExpandOne(expr: Expr, e: Expr?) throws -> Expr {
    let env = Env.global(try e?.asEnvironment() ?? self.context.environment)
    return try Compiler.macroExpand(expr: expr, in: env)?.datum ?? expr
  }
  
  private func compile(exprs: Arguments) throws -> Expr {
    var seq = Expr.null
    var environment: Environment? = nil
    for expr in exprs.reversed() {
      if seq.isNull, environment == nil, case .env(let env) = expr {
        environment = env
      } else {
        seq = .pair(expr, seq)
      }
    }
    let code = try Compiler.compile(expr: seq,
                                    in: .global(environment ?? self.context.environment),
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
        case .fixnum(let level):
          if level == 0 {
            self.context.machine.traceCalls = .off
          } else if level == 1 {
            self.context.machine.traceCalls = .byProc
          } else {
            self.context.machine.traceCalls = .on
          }
        case .false:
          self.context.machine.traceCalls = .off
        default:
          self.context.machine.traceCalls = .on
      }
    }
    switch context.machine.traceCalls {
      case .off:
        return .fixnum(0)
      case .byProc:
        return .fixnum(1)
      case .on:
        return .fixnum(2)
    }
  }

  private func procedureTrace(_ expr: Expr) throws -> Expr {
    return .makeBoolean(try expr.asProcedure().traced)
  }

  private func setProcedureTrace(_ expr: Expr, _ value: Expr) throws -> Expr {
    try expr.asProcedure().traced = value.isTrue
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

  private func loadedSources() -> Expr {
    var res = Expr.null
    for url in self.context.sources.sourceUrls {
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
}
