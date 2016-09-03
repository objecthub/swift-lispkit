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
  
  internal static let idProc = Procedure("identity", BaseLibrary.identity)
  
  public override func export() {
    // Basic primitives
    define(BaseLibrary.idProc)
    define(Procedure("procedure?", isProcedure))
    define(Procedure("eval", eval, compileEval))
    define(Procedure("apply", apply, compileApply))
    define(Procedure("equal?", isEqual))
    define(Procedure("eqv?", isEqv))
    define(Procedure("eq?", isEq))
    define("quote", SpecialForm(compileQuote))
    define("quasiquote", SpecialForm(compileQuasiquote))
    define("lambda", SpecialForm(compileLambda))
    define("case-lambda", SpecialForm(compileCaseLambda))
    
    // Definition primitives
    define("define", SpecialForm(compileDefine))
    define("define-syntax", SpecialForm(compileDefineSyntax))
    define("syntax-rules", SpecialForm(compileSyntaxRules))
    define("set!", SpecialForm(compileSet))
    define(Procedure("load", load))
    
    // Delayed execution
    define(Procedure("promise?", isPromise))
    define(Procedure("force", compileForce, in: self.context))
    define(Procedure("make-promise", makePromise))
    define(Procedure("eager", makePromise))
    define("delay", SpecialForm(compileDelay))
    define("delay-force", SpecialForm(compileDelayForce))
    define("lazy", SpecialForm(compileDelayForce))
    
    // Symbol primitives
    define(Procedure("symbol?", isSymbol))
    define(Procedure("gensym", gensym))
    define(Procedure("string->symbol", stringToSymbol))
    define(Procedure("symbol->string", symbolToString))
    define(Procedure("symtable", symtable))
    
    // Boolean primitives
    define(Procedure("boolean?", isBoolean))
    define("and", SpecialForm(compileAnd))
    define("or", SpecialForm(compileOr))
    define(Procedure("not", not))
    
    // System primitives
    define(Procedure("void", voidConst))
    define(Procedure("gc", gc))
    define(Procedure("exit", exit))
    define(Procedure("compile", compile))
    define(Procedure("disassemble", disassemble))
    define(Procedure("inspect", inspect))
    define("time", SpecialForm(compileTime))
  }
  
  
  //-------- MARK: - Basic primitives
  
  static func identity(_ expr: Expr) -> Expr {
    return expr
  }
  
  func eval(_ args: Arguments) throws -> Code {
    guard args.count > 0 else {
      throw EvalError.leastArgumentCountError(formals: 1, args: .List(args))
    }
    guard args.count < 3 else {
      throw EvalError.argumentCountError(formals: 2, args: .List(args))
    }
    return try Compiler.compile(self.context,
                                expr: .pair(args.first!, .null),
                                in: .interaction,
                                optimize: true)
  }
  
  func compileEval(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let arg, .null)) = expr else {
      throw EvalError.argumentCountError(formals: 1, args: expr)
    }
    compiler.emit(.makeFrame)
    try compiler.compile(arg, in: env, inTailPos: false)
    compiler.emit(.compile)
    return compiler.call(0, inTailPos: tail)
  }
  
  func apply(_ args: Arguments) throws -> (Procedure, [Expr]) {
    guard args.count > 1 else {
      throw EvalError.leastArgumentCountError(formals: 2, args: .List(args))
    }
    guard case .proc(let proc) = args.first! else {
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
  
  func compileApply(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
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
  
  func isEqual(_ this: Expr, that: Expr) -> Expr {
    return .Boolean(equalExpr(that, this))
  }
  
  func isEqv(_ this: Expr, that: Expr) -> Expr {
    return .Boolean(eqvExpr(that, this))
  }
  
  func isEq(_ this: Expr, that: Expr) -> Expr {
    return .Boolean(eqExpr(that, this))
  }
  
  func compileQuote(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let arg, .null)) = expr else {
      throw EvalError.argumentCountError(formals: 1, args: expr)
    }
    try compiler.pushValue(arg)
    return false
  }
  
  func compileQuasiquote(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let arg, .null)) = expr else {
      throw EvalError.argumentCountError(formals: 1, args: expr)
    }
    switch arg {
      case .pair(_, _):
        return try compiler.compile(reduceQQ(arg), in: env, inTailPos: tail)
      case .vector(let vector):
        var nvec = 0
        var nelem = 0
        for expr in vector.exprs {
          if case .pair(let car, .pair(let elem, let cddr)) = expr {
            switch car {
              case .sym(context.symbols.UNQUOTE):
                guard cddr == .null else {
                  throw EvalError.invalidContextInQuasiquote(context.symbols.UNQUOTE, expr)
                }
                try compiler.compile(elem, in: env, inTailPos: false)
                nelem += 1
                continue
              case .sym(context.symbols.UNQUOTESPLICING):
                guard cddr == .null else {
                  throw EvalError.invalidContextInQuasiquote(context.symbols.UNQUOTESPLICING, expr)
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
  
  fileprivate func reduceQQ(_ expr: Expr) throws -> Expr {
    guard case .pair(let car, let cdr) = expr else {
      return Expr.List(.sym(Symbol(context.symbols.QUOTE, .system)), expr)
    }
    switch car {
      case .sym(context.symbols.UNQUOTE):
        guard case .pair(let cadr, .null) = cdr else {
          throw EvalError.invalidContextInQuasiquote(context.symbols.UNQUOTE, car)
        }
        return cadr
      case .sym(context.symbols.QUASIQUOTE):
        guard case .pair(let cadr, .null) = cdr else {
          throw EvalError.invalidContextInQuasiquote(context.symbols.QUASIQUOTE, car)
        }
        return try reduceQQ(reduceQQ(cadr))
      case .sym(context.symbols.UNQUOTESPLICING):
        throw EvalError.invalidContextInQuasiquote(context.symbols.UNQUOTESPLICING, car)
      case .pair(.sym(context.symbols.UNQUOTESPLICING), let cdar):
        guard case .pair(let cadar, .null) = cdar else {
          throw EvalError.invalidContextInQuasiquote(context.symbols.UNQUOTESPLICING, car)
        }
        return Expr.List(.sym(Symbol(context.symbols.APPEND, .system)),
                         cadar,
                         try reduceQQ(cdr))
      default:
        return Expr.List(.sym(Symbol(context.symbols.CONS, .system)),
                         try reduceQQ(car),
                         try reduceQQ(cdr))
    }
  }
  
  func compileLambda(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let arglist, let body)) = expr else {
      throw EvalError.leastArgumentCountError(formals: 1, args: expr)
    }
    try compiler.compileLambda(nil, arglist, body, env)
    return false
  }
  
  func compileCaseLambda(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, let cases) = expr else {
      preconditionFailure("broken case-lambda invocation")
    }
    try compiler.compileCaseLambda(nil, cases, env)
    return false
  }
  
  func isProcedure(_ expr: Expr) -> Expr {
    if case .proc(_) = expr {
      return .true
    }
    return .false
  }
    
  //-------- MARK: - Definition primitives
  
  func compileDefine(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
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
      case .sym(_):
        guard case .pair(let value, .null) = def else {
          throw EvalError.malformedDefinition(Expr.List(def))
        }
        let index = compiler.registerConstant(sig)
        try compiler.compile(value, in: env, inTailPos: false)
        compiler.patchMakeClosure(index)
        compiler.emit(.defineGlobal(index))
        compiler.emit(.pushConstant(index))
        return false
      case .pair(let symSig, let arglist):
        let index = compiler.registerConstant(symSig)
        try compiler.compileLambda(index, arglist, def, env)
        compiler.emit(.defineGlobal(index))
        compiler.emit(.pushConstant(index))
        return false
      default:
        throw EvalError.malformedDefinition(.pair(sig, Expr.List(def)))
    }
  }
  
  func compileDefineSyntax(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    // Extract keyword and transformer definition
    guard case .pair(_, .pair(let kword, .pair(let transformer, .null))) = expr else {
      throw EvalError.argumentCountError(formals: 2, args: expr)
    }
    // Check that the keyword is a symbol
    guard case .sym(let sym) = kword else {
      throw EvalError.typeError(kword, [.symbolType])
    }
    // Check that define is not executed in a local environment
    if case .local(let group) = env {
      throw EvalError.defineSyntaxInLocalEnv(keyword: sym, definition: transformer, group: group)
    }
    // Compile transformer and store it as global keyword
    let index = compiler.registerConstant(kword)
    try compiler.compile(transformer, in: env, inTailPos: false)
    compiler.emit(.makeSyntax)
    compiler.emit(.defineGlobal(index))
    compiler.emit(.pushConstant(index))
    return false
  }
  
  func compileSyntaxRules(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
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
    while case .pair(.sym(let sym), let cdr) = expr {
      literalSet.insert(sym)
      expr = cdr
    }
    guard expr == .null else {
      throw EvalError.malformedSyntaxRuleLiterals(lit)
    }
    let rules = SyntaxRules(self.context,
                            literals: literalSet,
                            patterns: patterns,
                            templates: templates,
                            in: compiler.rulesEnv)
    compiler.emit(.pushConstant(compiler.registerConstant(.proc(Procedure(rules)))))
    return false
  }
  
  func checkPattern(_ expr: Expr) -> Expr? {
    switch expr {
      case .eof, .null, .true, .false, .sym(_), .str(_), .char(_),
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
  
  func compileSet(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let symbol, .pair(let value, .null))) = expr else {
      throw EvalError.argumentCountError(formals: 2, args: expr)
    }
    let sym = try symbol.asSymbol()
    try compiler.compile(value, in: env, inTailPos: false)
    compiler.setValueOf(sym, in: env)
    compiler.emit(.pushVoid)
    return false
  }
  
  func load(_ expr: Expr) throws -> Expr {
    let filename = try expr.asStr()
    return self.context.machine.evalFile(
      Bundle.main.path(
        forResource: filename, ofType: "scm", inDirectory: "LispKit/Library") ?? filename)
  }
  
  
  //-------- MARK: - Delayed execution
  
  func isPromise(_ expr: Expr) -> Expr {
    if case .promise(_) = expr {
      return .true
    }
    return .false
  }
  
  func makePromise(_ expr: Expr) -> Expr {
    return .promise(Future(expr))
  }
  
  func compileDelayForce(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let delayed, .null)) = expr else {
      throw EvalError.argumentCountError(formals: 1, args: expr)
    }
    try compiler.compileLambda(nil, .null, .pair(delayed, .null), env)
    compiler.emit(.makePromise)
    return false
  }
  
  func compileDelay(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let delayed, .null)) = expr else {
      throw EvalError.argumentCountError(formals: 1, args: expr)
    }
    try compiler.compileLambda(
      nil, .null, .pair(.List(.sym(compiler.context.symbols.MAKEPROMISE), delayed), .null), env)
    compiler.emit(.makePromise)
    return false
  }
  
  func compileForce(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let promise, .null)) = expr else {
      throw EvalError.argumentCountError(formals: 1, args: expr)
    }
    try compiler.compile(promise, in: env, inTailPos: false)
    compiler.emit(.force)
    compiler.emit(.storeInPromise)
    return false
  }
  
  //-------- MARK: - Symbol primitives
  
  func isSymbol(_ expr: Expr) -> Expr {
    if case .sym(_) = expr {
      return .true
    }
    return .false
  }
  
  func gensym(_ expr: Expr?) throws -> Expr {
    return .sym(context.symbols.gensym(try expr?.asStr() ?? "g"))
  }
  
  func stringToSymbol(_ expr: Expr) throws -> Expr {
    return .sym(context.symbols.intern(try expr.asStr()))
  }
  
  func symbolToString(_ expr: Expr) throws -> Expr {
    return .str(NSMutableString(string: try expr.asSymbol().description))
  }
  
  func symtable() -> Expr {
    var res = Expr.null
    for sym in self.context.symbols {
      res = .pair(.sym(sym), res)
    }
    return res
  }
  
  
  //-------- MARK: - Boolean primitives
  
  func isBoolean(_ expr: Expr) -> Expr {
    switch expr {
      case .true, .false:
        return .true
      default:
        return .false
    }
  }
  
  func not(_ expr: Expr) -> Expr {
    if case .false = expr {
      return .true
    }
    return .false
  }
  
  func compileAnd(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
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
  
  func compileOr(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
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
  
  
  //-------- MARK: - System primitives
  
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
  
  func compileTime(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let exec, .null)) = expr else {
      throw EvalError.argumentCountError(formals: 1, args: expr)
    }
    compiler.emit(.pushCurrentTime)
    try compiler.compile(exec, in: env, inTailPos: false)
    compiler.emit(.swap)
    compiler.emit(.pushCurrentTime)
    compiler.emit(.swap)
    compiler.emit(.flMinus)
    try compiler.pushValue(.str(NSMutableString(string: "elapsed time = ")))
    compiler.emit(.display)
    compiler.emit(.display)
    compiler.emit(.newline)
    return false
  }
  
  func compile(_ exprs: Arguments) throws -> Expr {
    var seq = Expr.null
    for expr in exprs.reversed() {
      seq = .pair(expr, seq)
    }
    let code = try Compiler.compile(self.context, expr: seq, optimize: true)
    context.console.print(code.description)
    return .void
  }
  
  func disassemble(_ expr: Expr) throws -> Expr {
    guard case .proc(let proc) = expr else {
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
  
  func inspect(_ expr: Expr) -> Expr {
    return expr
  }
}
