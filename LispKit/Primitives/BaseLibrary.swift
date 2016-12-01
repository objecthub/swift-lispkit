//
//  BaseLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 22/01/2016.
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


public final class BaseLibrary: NativeLibrary {
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "base"]
  }
  
  internal static let idProc = Procedure("identity", BaseLibrary.identity)
  
  /// Declarations of the library.
  public override func declarations() {
    // Basic primitives
    self.define(BaseLibrary.idProc)
    self.define(Procedure("procedure?", isProcedure))
    self.define(Procedure("eval", eval, compileEval))
    self.define(Procedure("apply", apply, compileApply))
    self.define(Procedure("equal?", isEqual))
    self.define(Procedure("eqv?", isEqv))
    self.define(Procedure("eq?", isEq))
    self.define("quote", as: SpecialForm(compileQuote))
    self.define("quasiquote", as: SpecialForm(compileQuasiquote))
    self.define("lambda", as: SpecialForm(compileLambda))
    self.define("case-lambda", as: SpecialForm(compileCaseLambda))
    
    // Definition primitives
    self.define("define", as: SpecialForm(compileDefine))
    self.define("define-syntax", as: SpecialForm(compileDefineSyntax))
    self.define("define-library", as: SpecialForm(compileDefineLibrary))
    self.define("syntax-rules", as: SpecialForm(compileSyntaxRules))
    self.define("set!", as: SpecialForm(compileSet))
    self.define(Procedure("load", load))
    
    // Delayed execution
    self.define(Procedure("promise?", isPromise))
    self.define(Procedure("force", compileForce, in: self.context))
    self.define(Procedure("make-promise", makePromise))
    self.define(Procedure("eager", makePromise))
    self.define("delay", as: SpecialForm(compileDelay))
    self.define("delay-force", as: SpecialForm(compileDelayForce))
    self.define("lazy", as: SpecialForm(compileDelayForce))
    
    // Symbol primitives
    self.define(Procedure("symbol?", isSymbol))
    self.define(Procedure("gensym", gensym))
    self.define(Procedure("string->symbol", stringToSymbol))
    self.define(Procedure("symbol->string", symbolToString))
    
    // Boolean primitives
    self.define(Procedure("boolean?", isBoolean))
    self.define("and", as: SpecialForm(compileAnd))
    self.define("or", as: SpecialForm(compileOr))
    self.define(Procedure("not", not))
    
    // Environments
    self.define(Procedure("environment?", isEnvironment))
    self.define(Procedure("environment", environment))
    self.define(Procedure("interaction-environment", interactionEnvironment))
    
    // System primitives
    self.define(Procedure("get-environment-variable", getEnvironmentVariable))
    self.define(Procedure("get-environment-variables", getEnvironmentVariables))
    self.define(Procedure("void", voidConst))
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
  
  
  //-------- MARK: - Basic primitives
  
  static func identity(expr: Expr) -> Expr {
    return expr
  }
  
  func eval(args: Arguments) throws -> Code {
    guard args.count > 0 else {
      throw EvalError.leastArgumentCountError(formals: 1, args: .makeList(args))
    }
    guard args.count < 3 else {
      throw EvalError.argumentCountError(formals: 2, args: .makeList(args))
    }
    var env = self.context.global
    if args.count == 2 {
      env = .global(try args[args.startIndex + 1].asEnvironment())
    }
    return try Compiler.compile(expr: .pair(args.first!, .null), in: env, optimize: true)
  }
  
  func compileEval(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let expr, let rest)) = expr else {
      throw EvalError.argumentCountError(formals: 1, args: .null)
    }
    compiler.emit(.makeFrame)
    switch rest {
      case .null:
        try compiler.compile(expr, in: env, inTailPos: false)
        try compiler.pushValue(.env(env.environment ?? self.context.environment))
      case .pair(let e, .null):
        try compiler.compile(expr, in: env, inTailPos: false)
        try compiler.compile(e, in: env, inTailPos: false)
      default:
        throw EvalError.argumentCountError(formals: 1, args: .pair(expr, rest))
    }
    compiler.emit(.compile)
    return compiler.call(0, inTailPos: tail)
  }
  
  func apply(args: Arguments) throws -> (Procedure, [Expr]) {
    guard args.count > 1 else {
      throw EvalError.leastArgumentCountError(formals: 2, args: .makeList(args))
    }
    guard case .procedure(let proc) = args.first! else {
      throw EvalError.typeError(args.first!, [.procedureType])
    }
    var exprs = Exprs()
    for arg in args[args.startIndex+1..<args.endIndex-1] {
      exprs.append(arg)
    }
    var next = args.last!
    while case .pair(let arg, let rest) = next {
      exprs.append(arg)
      next = rest
    }
    guard next.isNull else {
      throw EvalError.typeError(args.last!, [.properListType])
    }
    return (proc, exprs)
  }
  
  func compileApply(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let fun, let arglist)) = expr else {
      throw EvalError.leastArgumentCountError(formals: 2, args: expr)
    }
    compiler.emit(.makeFrame)
    try compiler.compile(fun, in: env, inTailPos: false)
    var next = arglist
    var n = 0
    while case .pair(let arg, let rest) = next {
      n += 1
      try compiler.compile(arg, in: env, inTailPos: false)
      if rest.isNull {
        compiler.emit(.apply(n))
        return false
      }
      next = rest
    }
    throw EvalError.leastArgumentCountError(formals: 2, args: expr)
  }
  
  func isEqual(this: Expr, that: Expr) -> Expr {
    return .makeBoolean(equalExpr(that, this))
  }
  
  func isEqv(this: Expr, that: Expr) -> Expr {
    return .makeBoolean(eqvExpr(that, this))
  }
  
  func isEq(this: Expr, that: Expr) -> Expr {
    return .makeBoolean(eqExpr(that, this))
  }
  
  func compileQuote(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let arg, .null)) = expr else {
      throw EvalError.argumentCountError(formals: 1, args: expr)
    }
    try compiler.pushValue(arg)
    return false
  }
  
  func compileQuasiquote(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let arg, .null)) = expr else {
      throw EvalError.argumentCountError(formals: 1, args: expr)
    }
    switch arg {
      case .pair(_, _):
        return try compiler.compile(reduceQQ(arg, in: env), in: env, inTailPos: tail)
      case .vector(let vector):
        var nvec = 0
        var nelem = 0
        for expr in vector.exprs {
          if case .pair(let car, .pair(let elem, let cddr)) = expr {
            switch car {
              case .symbol(context.symbols.unquote):
                guard cddr == .null else {
                  throw EvalError.invalidContextInQuasiquote(context.symbols.unquote, expr)
                }
                try compiler.compile(elem, in: env, inTailPos: false)
                nelem += 1
                continue
              case .symbol(context.symbols.unquoteSplicing):
                guard cddr == .null else {
                  throw EvalError.invalidContextInQuasiquote(context.symbols.unquoteSplicing, expr)
                }
                if nelem > 0 {
                  compiler.emit(.vector(nelem))
                  nelem = 0
                  nvec += 1
                }
                try compiler.compile(elem, in: env, inTailPos: false)
                compiler.emit(.dup)
                compiler.emit(.isVector)
                let vectorJumpIp = compiler.emitPlaceholder()
                compiler.emit(.listToVector)
                compiler.patch(.branchIf(compiler.offsetToNext(vectorJumpIp)), at: vectorJumpIp)
                nvec += 1
                continue
              default:
                break
            }
          }
          try compiler.pushValue(expr)
          nelem += 1
        }
        if nvec == 0 {
          compiler.emit(.vector(nelem))
        } else if nelem == 0 {
          compiler.emit(.vectorAppend(nvec))
        } else {
          compiler.emit(.vector(nelem))
          compiler.emit(.vectorAppend(nvec + 1))
        }
      default:
        try compiler.pushValue(arg)
    }
    return false
  }
  
  private func reduceQQ(_ expr: Expr, in env: Env) throws -> Expr {
    guard case .pair(let car, let cdr) = expr else {
      return Expr.makeList(.symbol(Symbol(context.symbols.quote, env.global)), expr)
    }
    switch car {
      case .symbol(context.symbols.unquote):
        guard case .pair(let cadr, .null) = cdr else {
          throw EvalError.invalidContextInQuasiquote(context.symbols.unquote, car)
        }
        return cadr
      case .symbol(context.symbols.quasiquote):
        guard case .pair(let cadr, .null) = cdr else {
          throw EvalError.invalidContextInQuasiquote(context.symbols.quasiquote, car)
        }
        return try reduceQQ(reduceQQ(cadr, in: env), in: env)
      case .symbol(context.symbols.unquoteSplicing):
        throw EvalError.invalidContextInQuasiquote(context.symbols.unquoteSplicing, car)
      case .pair(.symbol(context.symbols.unquoteSplicing), let cdar):
        guard case .pair(let cadar, .null) = cdar else {
          throw EvalError.invalidContextInQuasiquote(context.symbols.unquoteSplicing, car)
        }
        return Expr.makeList(.symbol(Symbol(context.symbols.append, env.global)),
                             cadar,
                             try reduceQQ(cdr, in: env))
      default:
        return Expr.makeList(.symbol(Symbol(context.symbols.cons, env.global)),
                             try reduceQQ(car, in: env),
                             try reduceQQ(cdr, in: env))
    }
  }
  
  func compileLambda(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let arglist, let body)) = expr else {
      throw EvalError.leastArgumentCountError(formals: 1, args: expr)
    }
    try compiler.compileLambda(nil, arglist, body, env)
    return false
  }
  
  func compileCaseLambda(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, let cases) = expr else {
      preconditionFailure("broken case-lambda invocation")
    }
    try compiler.compileCaseLambda(nil, cases, env)
    return false
  }
  
  func isProcedure(expr: Expr) -> Expr {
    if case .procedure(_) = expr {
      return .true
    }
    return .false
  }
    
  //-------- MARK: - Definition primitives
  
  func compileDefine(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    // Extract signature and definition
    guard case .pair(_, .pair(let sig, let def)) = expr else {
      throw EvalError.leastArgumentCountError(formals: 2, args: expr)
    }
    // Check that define is not executed in a local environment
    if case .local(let group) = env {
      throw EvalError.defineInLocalEnv(signature: sig, definition: def, group: group)
    }
    // Compile definition and store result in global environment
    switch sig {
      case .symbol(let sym):
        guard case .pair(let value, .null) = def else {
          throw EvalError.malformedDefinition(Expr.makeList(def))
        }
        let index = compiler.registerConstant(sig)
        try compiler.compile(value, in: env, inTailPos: false)
        let loc = env.environment!.forceDefinedLocationRef(for: sym).location!
        compiler.patchMakeClosure(index)
        compiler.emit(.defineGlobal(loc))
        compiler.emit(.pushConstant(index))
        return false
      case .pair(.symbol(let sym), let arglist):
        let index = compiler.registerConstant(.symbol(sym))
        try compiler.compileLambda(index, arglist, def, env)
        let loc = env.environment!.forceDefinedLocationRef(for: sym).location!
        compiler.emit(.defineGlobal(loc))
        compiler.emit(.pushConstant(index))
        return false
      default:
        throw EvalError.malformedDefinition(.pair(sig, Expr.makeList(def)))
    }
  }
  
  func compileDefineSyntax(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    // Extract keyword and transformer definition
    guard case .pair(_, .pair(let kword, .pair(let transformer, .null))) = expr else {
      throw EvalError.argumentCountError(formals: 2, args: expr)
    }
    // Check that the keyword is a symbol
    guard case .symbol(let sym) = kword else {
      throw EvalError.typeError(kword, [.symbolType])
    }
    // Check that define is not executed in a local environment
    if case .local(let group) = env {
      throw EvalError.defineSyntaxInLocalEnv(keyword: sym, definition: transformer, group: group)
    }
    // Compile transformer and store it as global keyword
    let index = compiler.registerConstant(kword)
    try compiler.compile(transformer, in: env, inTailPos: false)
    let loc = env.environment!.forceDefinedLocationRef(for: sym).location!
    compiler.emit(.makeSyntax)
    compiler.emit(.defineGlobal(loc))
    compiler.emit(.pushConstant(index))
    return false
  }
  
  func compileDefineLibrary(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let name, let decls)) = expr else {
      throw EvalError.leastArgumentCountError(formals: 1, args: expr)
    }
    try compiler.context.libraries.load(name: name, declarations: decls)
    compiler.emit(.pushVoid)
    return false
  }
  
  func compileSyntaxRules(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let lit, let patTrans)) = expr else {
      throw EvalError.leastArgumentCountError(formals: 1, args: expr)
    }
    var patterns = Exprs()
    var templates = Exprs()
    var transRules = patTrans
    while case .pair(let rule, let rest) = transRules {
      guard case .pair(let car, .pair(let cadr, .null)) = rule else {
        throw EvalError.malformedSyntaxRule(rule)
      }
      guard case .pair(_, let pat) = car else {
        throw EvalError.malformedSyntaxRulePattern(error: nil, pattern: car)
      }
      if let errExpr = checkPattern(car) {
        throw EvalError.malformedSyntaxRulePattern(error: errExpr, pattern: car)
      }
      patterns.append(pat)
      templates.append(cadr)
      transRules = rest
    }
    guard transRules == .null else {
      throw EvalError.malformedSyntaxRulePattern(error: nil, pattern: transRules)
    }
    var literalSet = Set<Symbol>()
    var expr = lit
    while case .pair(.symbol(let sym), let cdr) = expr {
      literalSet.insert(sym)
      expr = cdr
    }
    guard expr == .null else {
      throw EvalError.malformedSyntaxRuleLiterals(lit)
    }
    let rules = SyntaxRules(context: self.context,
                            literals: literalSet,
                            patterns: patterns,
                            templates: templates,
                            in: compiler.rulesEnv)
    compiler.emit(.pushConstant(compiler.registerConstant(.procedure(Procedure(rules)))))
    return false
  }
  
  func checkPattern(_ expr: Expr) -> Expr? {
    switch expr {
      case .eof, .null, .true, .false, .symbol(_), .string(_), .char(_),
           .fixnum(_), .bignum(_), .rational(_), .bigrat(_), .flonum(_), .complex(_):
        return nil
      case .pair(let car, let cdr):
        if let errExpr = checkPattern(car) {
          return errExpr
        } else if let errExpr = checkPattern(cdr) {
          return errExpr
        } else {
          return nil
        }
      case .vector(let vector):
        for pat in vector.exprs {
          if let errExpr = checkPattern(pat) {
            return errExpr
          }
        }
        return nil
      default:
        return expr
    }
  }
  
  func compileSet(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let symbol, .pair(let value, .null))) = expr else {
      throw EvalError.argumentCountError(formals: 2, args: expr)
    }
    let sym = try symbol.asSymbol()
    try compiler.compile(value, in: env, inTailPos: false)
    compiler.setValueOf(sym, in: env)
    compiler.emit(.pushVoid)
    return false
  }
  
  func load(expr: Expr, e: Expr?) throws -> Expr {
    let path = lispKitFilePath(for: try expr.asString())
    return self.context.machine.eval(
      file: path,
      in: .global(try e?.asEnvironment() ?? self.context.environment))
  }
  
  //-------- MARK: - Delayed execution
  
  func isPromise(expr: Expr) -> Expr {
    if case .promise(_) = expr {
      return .true
    }
    return .false
  }
  
  func makePromise(expr: Expr) -> Expr {
    return .promise(Promise(expr))
  }
  
  func compileDelayForce(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let delayed, .null)) = expr else {
      throw EvalError.argumentCountError(formals: 1, args: expr)
    }
    try compiler.compileLambda(nil, .null, .pair(delayed, .null), env)
    compiler.emit(.makePromise)
    return false
  }
  
  func compileDelay(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let delayed, .null)) = expr else {
      throw EvalError.argumentCountError(formals: 1, args: expr)
    }
    let body = Expr.pair(.makeList(.symbol(compiler.context.symbols.makePromise), delayed), .null)
    try compiler.compileLambda(nil, .null, body, env)
    compiler.emit(.makePromise)
    return false
  }
  
  func compileForce(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let promise, .null)) = expr else {
      throw EvalError.argumentCountError(formals: 1, args: expr)
    }
    try compiler.compile(promise, in: env, inTailPos: false)
    compiler.emit(.force)
    compiler.emit(.storeInPromise)
    return false
  }
  
  //-------- MARK: - Symbol primitives
  
  func isSymbol(expr: Expr) -> Expr {
    if case .symbol(_) = expr {
      return .true
    }
    return .false
  }
  
  func gensym(expr: Expr?) throws -> Expr {
    return .symbol(context.symbols.gensym(try expr?.asString() ?? "g"))
  }
  
  func stringToSymbol(expr: Expr) throws -> Expr {
    return .symbol(context.symbols.intern(try expr.asString()))
  }
  
  func symbolToString(expr: Expr) throws -> Expr {
    return .makeString(try expr.asSymbol().description)
  }
  
  //-------- MARK: - Boolean primitives
  
  func isBoolean(expr: Expr) -> Expr {
    switch expr {
      case .true, .false:
        return .true
      default:
        return .false
    }
  }
  
  func not(expr: Expr) -> Expr {
    if case .false = expr {
      return .true
    }
    return .false
  }
  
  func compileAnd(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, let exprs) = expr else {
      preconditionFailure()
    }
    var trueJumps = [Int]()
    var args = exprs
    while case .pair(let arg, let rest) = args {
      if rest == .null {
        try compiler.compile(arg, in: env, inTailPos: tail)
        for ip in trueJumps {
          compiler.patch(.and(compiler.offsetToNext(ip)), at: ip)
        }
        return false
      } else {
        try compiler.compile(arg, in: env, inTailPos: false)
        trueJumps.append(compiler.emitPlaceholder())
      }
      args = rest
    }
    guard args == .null else {
      throw EvalError.malformedArgumentList(exprs)
    }
    compiler.emit(.pushTrue)
    return false
  }
  
  func compileOr(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, let exprs) = expr else {
      preconditionFailure()
    }
    var falseJumps = [Int]()
    var args = exprs
    while case .pair(let arg, let rest) = args {
      if rest == .null {
        try compiler.compile(arg, in: env, inTailPos: tail)
        for ip in falseJumps {
          compiler.patch(.or(compiler.offsetToNext(ip)), at: ip)
        }
        return false
      } else {
        try compiler.compile(arg, in: env, inTailPos: false)
        falseJumps.append(compiler.emitPlaceholder())
      }
      args = rest
    }
    guard args == .null else {
      throw EvalError.malformedArgumentList(exprs)
    }
    compiler.emit(.pushFalse)
    return false
  }
  
  
  //-------- MARK: - Environments
  
  func isEnvironment(expr: Expr) -> Expr {
    switch expr {
      case .env(_):
        return .true
      default:
        return .false
    }
  }
  
  func environment(exprs: Arguments) throws -> Expr {
    var importSets = [ImportSet]()
    for expr in exprs {
      guard let importSet = ImportSet(expr, in: self.context) else {
        throw EvalError.malformedImportSet(expr)
      }
      importSets.append(importSet)
    }
    return Expr.env(try Environment(in: self.context, importing: importSets))
  }
  
  func interactionEnvironment() -> Expr {
    return Expr.env(self.context.environment)
  }
  
  
  //-------- MARK: - System primitives
  
  func getEnvironmentVariable(expr: Expr) throws -> Expr {
    let name = try expr.asString()
    guard let value = ProcessInfo.processInfo.environment[name] else {
      return .false
    }
    return .makeString(value)
  }
  
  func getEnvironmentVariables() -> Expr {
    var alist = Expr.null
    for (name, value) in ProcessInfo.processInfo.environment {
      alist = .pair(.pair(.makeString(name), .makeString(value)), alist)
    }
    return alist
  }
  
  func voidConst() -> Expr {
    return .void
  }
  
  func gc() -> Expr {
    context.console.print("BEFORE: " + context.objects.description + "\n")
    let res = Expr.fixnum(Int64(self.context.objects.collectGarbage()))
    context.console.print("AFTER: " + context.objects.description + "\n")
    return res
  }
  
  func exit() -> Expr {
    NSApplication.shared().terminate(self)
    return .undef
  }
  
  func compileTime(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
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
  
  func compile(exprs: Arguments) throws -> Expr {
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
  
  func disassemble(expr: Expr) throws -> Expr {
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
  
  func availableSymbols() -> Expr {
    var res = Expr.null
    for sym in self.context.symbols {
      res = .pair(.symbol(sym), res)
    }
    return res
  }
  
  func loadedLibraries() -> Expr {
    var res = Expr.null
    for library in self.context.libraries.loaded {
      res = .pair(library.name, res)
    }
    return res
  }
  
  func environmentInfo() -> Expr {
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
  
  func currentSecond() -> Expr {
    var time = timeval(tv_sec: 0, tv_usec: 0)
    gettimeofday(&time, nil)
    return .flonum(Double(time.tv_sec) + (Double(time.tv_usec) / 1000000.0))
  }
  
  func currentJiffy() -> Expr {
    var time = timeval(tv_sec: 0, tv_usec: 0)
    gettimeofday(&time, nil)
    return .fixnum(Int64(time.tv_sec) * 1000 + Int64(time.tv_usec / 1000))
  }
  
  func jiffiesPerSecond() -> Expr {
    return .fixnum(1000)
  }
}
