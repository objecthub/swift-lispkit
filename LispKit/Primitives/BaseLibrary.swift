//
//  CoreLibrary.swift
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


public final class BaseLibrary: Library {
  
  public override func export() {
    // Basic primitives
    define("eval", Procedure(eval, compileEval))
    define("apply", Procedure(apply, compileApply))
    define("equal?", Procedure(isEqual))
    define("eqv?", Procedure(isEqv))
    define("eq?", Procedure(isEq))
    define("quote", SpecialForm(compileQuote))
    define("quasiquote", SpecialForm(compileQuasiquote))
    define("lambda", SpecialForm(compileLambda))
    define("procedure?", Procedure(isProcedure))
    
    // Definition primitives
    define("define", SpecialForm(compileDefine))
    define("define-syntax", SpecialForm(compileDefineSyntax))
    define("syntax-rules", SpecialForm(compileSyntaxRules))
    define("set!", SpecialForm(compileSet))
    define("load", Procedure(load))
    
    // Delayed execution
    define("promise?", Procedure(isPromise))
    define("delay", SpecialForm(compileDelay))
    define("force", Procedure(force, compileForce))
    
    // Symbol primitives
    define("symbol?", Procedure(isSymbol))
    define("gensym", Procedure(gensym))
    define("string->symbol", Procedure(stringToSymbol))
    define("symbol->string", Procedure(symbolToString))
    define("symtable", Procedure(symtable))
    
    // Boolean primitives
    define("boolean?", Procedure(isBoolean))
    define("and", SpecialForm(compileAnd))
    define("or", SpecialForm(compileOr))
    define("not", Procedure(not))
    
    // System primitives
    define("gc", Procedure(gc))
    define("exit", Procedure(exit))
    define("time", SpecialForm(compileTime))
    define("compile", Procedure(compile))
    define("disassemble", Procedure(disassemble))
  }
  
  
  //-------- MARK: - Basic primitives
  
  func eval(expr: Expr) throws -> Expr {
    return try context.machine.eval(expr)
  }
  
  func compileEval(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .Pair(_, .Pair(let arg, .Null)) = expr else {
      throw EvalError.ArgumentCountError(formals: 1, args: expr)
    }
    compiler.emit(.MakeFrame)
    try compiler.compile(arg, in: env, inTailPos: false)
    compiler.emit(.Compile)
    return compiler.call(0, tail)
  }
  
  func apply(fun: Expr, _ args: Expr, _ arglist: Arguments) throws -> Expr {
    guard arglist.count > 0 else {
      return try self.context.machine.apply(fun, to: args)
    }
    var exprs = Exprs()
    var next = args
    for arg in arglist {
      exprs.append(next)
      next = arg
    }
    return try context.machine.apply(fun, to: .List(exprs, append: next))
  }
  
  func compileApply(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .Pair(_, .Pair(let fun, let arglist)) = expr else {
      throw EvalError.LeastArgumentCountError(formals: 2, args: expr)
    }
    compiler.emit(.MakeFrame)
    try compiler.compile(fun, in: env, inTailPos: false)
    var next = arglist
    var n = 0
    while case .Pair(let arg, let rest) = next {
      n += 1
      try compiler.compile(arg, in: env, inTailPos: false)
      if rest.isNull {
        compiler.emit(.Apply(n))
        return false
      }
      next = rest
    }
    throw EvalError.LeastArgumentCountError(formals: 2, args: expr)
  }
  
  func isEqual(this: Expr, that: Expr) -> Expr {
    return .Boolean(equalExpr(this, that))
  }
  
  func isEqv(this: Expr, that: Expr) -> Expr {
    return .Boolean(eqvExpr(this, that))
  }
  
  func isEq(this: Expr, that: Expr) -> Expr {
    return .Boolean(eqExpr(this, that))
  }
  
  func compileQuote(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .Pair(_, .Pair(let arg, .Null)) = expr else {
      throw EvalError.ArgumentCountError(formals: 1, args: expr)
    }
    try compiler.pushValue(arg)
    return false
  }
  
  func compileQuasiquote(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .Pair(_, .Pair(let arg, .Null)) = expr else {
      throw EvalError.ArgumentCountError(formals: 1, args: expr)
    }
    switch arg {
      case .Pair(_, _):
        return try compiler.compile(reduceQQ(arg), in: env, inTailPos: tail)
      case .Vec(let vector):
        var nvec = 0
        var nelem = 0
        for expr in vector.exprs {
          if case .Pair(let car, .Pair(let elem, let cddr)) = expr {
            switch car {
              case .Sym(context.symbols.UNQUOTE):
                guard cddr == .Null else {
                  throw EvalError.InvalidContextInQuasiquote(context.symbols.UNQUOTE, expr)
                }
                try compiler.compile(elem, in: env, inTailPos: false)
                nelem += 1
                continue
              case .Sym(context.symbols.UNQUOTESPLICING):
                guard cddr == .Null else {
                  throw EvalError.InvalidContextInQuasiquote(context.symbols.UNQUOTESPLICING, expr)
                }
                if nelem > 0 {
                  compiler.emit(.Vector(nelem))
                  nelem = 0
                  nvec += 1
                }
                try compiler.compile(elem, in: env, inTailPos: false)
                compiler.emit(.Dup)
                compiler.emit(.IsVector)
                let vectorJumpIp = compiler.emitPlaceholder()
                compiler.emit(.ListToVector)
                compiler.patch(.BranchIf(compiler.offsetToNext(vectorJumpIp)), at: vectorJumpIp)
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
          compiler.emit(.Vector(nelem))
        } else if nelem == 0 {
          compiler.emit(.VectorAppend(nvec))
        } else {
          compiler.emit(.Vector(nelem))
          compiler.emit(.VectorAppend(nvec + 1))
        }
      default:
        try compiler.pushValue(arg)
    }
    return false
  }
  
  private func reduceQQ(expr: Expr) throws -> Expr {
    guard case .Pair(let car, let cdr) = expr else {
      return Expr.List(.Sym(Symbol(context.symbols.QUOTE, .System)), expr)
    }
    switch car {
      case .Sym(context.symbols.UNQUOTE):
        guard case .Pair(let cadr, .Null) = cdr else {
          throw EvalError.InvalidContextInQuasiquote(context.symbols.UNQUOTE, car)
        }
        return cadr
      case .Sym(context.symbols.QUASIQUOTE):
        guard case .Pair(let cadr, .Null) = cdr else {
          throw EvalError.InvalidContextInQuasiquote(context.symbols.QUASIQUOTE, car)
        }
        return try reduceQQ(reduceQQ(cadr))
      case .Sym(context.symbols.UNQUOTESPLICING):
        throw EvalError.InvalidContextInQuasiquote(context.symbols.UNQUOTESPLICING, car)
      case .Pair(.Sym(context.symbols.UNQUOTESPLICING), let cdar):
        guard case .Pair(let cadar, .Null) = cdar else {
          throw EvalError.InvalidContextInQuasiquote(context.symbols.UNQUOTESPLICING, car)
        }
        return Expr.List(.Sym(Symbol(context.symbols.APPEND, .System)),
                         cadar,
                         try reduceQQ(cdr))
      default:
        return Expr.List(.Sym(Symbol(context.symbols.CONS, .System)),
                         try reduceQQ(car),
                         try reduceQQ(cdr))
    }
  }
  
  func compileLambda(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .Pair(_, .Pair(let arglist, let body)) = expr else {
      throw EvalError.LeastArgumentCountError(formals: 1, args: expr)
    }
    compiler.emit(try BaseLibrary.compileProc(compiler, arglist, body, env, false))
    return false
  }
  
  static func compileProc(compiler: Compiler,
                          _ arglist: Expr,
                          _ body: Expr,
                          _ env: Env,
                          _ promise: Bool) throws -> Instruction {
    // Create closure compiler as child of the current compiler
    let closureCompiler = Compiler(compiler.context, env)
    // Compile arguments
    try closureCompiler.compileArgList(arglist)
    // Compile body
    try closureCompiler.compileBody(body)
    // Link compiled closure in the current compiler
    let codeIndex = compiler.code.count
    let code = closureCompiler.bundle()
    compiler.code.append(code)
    // Generate code for pushing captured bindings onto the stack
    for def in closureCompiler.captures.definitions {
      let capture = closureCompiler.captures.captures[def!]!
      if capture.origin.owner === compiler {
        compiler.emit(.PushLocal(def!.index))
      } else {
        compiler.emit(.PushCaptured(compiler.captures.capture(def!, from: capture.origin)))
      }
    }
    // Return captured binding count and index of compiled closure
    if promise {
      return .MakePromise(closureCompiler.captures.count, codeIndex)
    } else {
      return .MakeClosure(closureCompiler.captures.count, codeIndex)
    }
  }
  
  func isProcedure(expr: Expr) -> Expr {
    if case .Proc(_) = expr {
      return .True
    }
    return .False
  }

  
  //-------- MARK: - Definition primitives
  
  func compileDefine(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .Pair(_, .Pair(let sig, let def)) = expr else {
      throw EvalError.LeastArgumentCountError(formals: 2, args: expr)
    }
    switch sig {
      case .Sym(_):
        guard case .Pair(let value, .Null) = def else {
          throw EvalError.MalformedDefinition(Expr.List(def))
        }
        let index = compiler.registerConstant(sig)
        try compiler.compile(value, in: env, inTailPos: false)
        compiler.emit(.DefineGlobal(index))
        compiler.emit(.PushConstant(index))
        return false
      case .Pair(.Sym(let sym), let arglist):
        let index = compiler.registerConstant(.Sym(sym))
        compiler.emit(try BaseLibrary.compileProc(compiler, arglist, def, env, false))
        compiler.emit(.DefineGlobal(index))
        compiler.emit(.PushConstant(index))
        return false
      default:
        throw EvalError.MalformedDefinition(.Pair(sig, Expr.List(def)))
    }
  }
  
  func compileDefineSyntax(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .Pair(_, .Pair(let kword, .Pair(let transformer, .Null))) = expr else {
      throw EvalError.ArgumentCountError(formals: 2, args: expr)
    }
    guard case .Sym(_) = kword else {
      throw EvalError.TypeError(kword, [.SymbolType])
    }
    let index = compiler.registerConstant(kword)
    try compiler.compile(transformer, in: env, inTailPos: false)
    compiler.emit(.MakeSyntax)
    compiler.emit(.DefineGlobal(index))
    compiler.emit(.PushConstant(index))
    return false
  }
  
  func compileSyntaxRules(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .Pair(_, .Pair(let lit, let patTrans)) = expr else {
      throw EvalError.LeastArgumentCountError(formals: 1, args: expr)
    }
    var patterns = Exprs()
    var templates = Exprs()
    var transRules = patTrans
    while case .Pair(let rule, let rest) = transRules {
      guard case .Pair(let car, .Pair(let cadr, .Null)) = rule else {
        throw EvalError.MalformedSyntaxRule(rule)
      }
      guard case .Pair(_, let pat) = car else {
        throw EvalError.MalformedSyntaxRulePattern(error: nil, pattern: car)
      }
      if let errExpr = checkPattern(car) {
        throw EvalError.MalformedSyntaxRulePattern(error: errExpr, pattern: car)
      }
      patterns.append(pat)
      templates.append(cadr)
      transRules = rest
    }
    guard transRules == .Null else {
      throw EvalError.MalformedSyntaxRulePattern(error: nil, pattern: transRules)
    }
    var literalSet = Set<Symbol>()
    var expr = lit
    while case .Pair(.Sym(let sym), let cdr) = expr {
      literalSet.insert(sym)
      expr = cdr
    }
    guard expr == .Null else {
      throw EvalError.MalformedSyntaxRuleLiterals(lit)
    }
    let rules = SyntaxRules(self.context,
                            literals: literalSet,
                            patterns: patterns,
                            templates: templates,
                            in: compiler.rulesEnv)
    compiler.emit(.PushConstant(compiler.registerConstant(.Proc(Procedure(rules)))))
    return false
  }
  
  func checkPattern(expr: Expr) -> Expr? {
    switch expr {
      case .Eof, .Null, .True, .False, .Sym(_), .Str(_), .Char(_),
           .Fixnum(_), .Bignum(_), .Rat(_), .Bigrat(_), .Flonum(_), .Complexnum(_):
        return nil
      case .Pair(let car, let cdr):
        if let errExpr = checkPattern(car) {
          return errExpr
        } else if let errExpr = checkPattern(cdr) {
          return errExpr
        } else {
          return nil
        }
      case .Vec(let vector):
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
    guard case .Pair(_, .Pair(let symbol, .Pair(let value, .Null))) = expr else {
      throw EvalError.ArgumentCountError(formals: 2, args: expr)
    }
    let sym = try symbol.asSymbol()
    try compiler.compile(value, in: env, inTailPos: false)
    compiler.setValueOf(sym, in: env)
    return false
  }
  
  func load(expr: Expr) throws -> Expr {
    let filename = try expr.asStr()
    return self.context.machine.evalFile(
      NSBundle.mainBundle().pathForResource(
        filename, ofType: "scm", inDirectory: "LispKit/Library") ?? filename)
  }
  
  
  //-------- MARK: - Delayed execution
  
  func isPromise(expr: Expr) -> Expr {
    if case .Promise(_) = expr {
      return .True
    }
    return .False
  }
    
  func compileDelay(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .Pair(_, .Pair(let delayed, .Null)) = expr else {
      throw EvalError.ArgumentCountError(formals: 1, args: expr)
    }
    compiler.emit(try BaseLibrary.compileProc(compiler, .Null, .Pair(delayed, .Null), env, true))
    return false
  }
  
  func force(expr: Expr) throws -> Expr {
    guard case .Promise(let future) = expr else {
      return expr
    }
    switch future.state {
      case .Unevaluated(let proc):
        do {
          let res = try context.machine.apply(.Proc(proc), to: .Null)
          future.state = .Value(res)
          return res
        } catch let error {
          future.state = .Thrown(error)
          throw error
        }
      case .Value(let value):
        return value
      case .Thrown(let error):
        throw error
    }
  }
  
  func compileForce(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .Pair(_, .Pair(let promise, .Null)) = expr else {
      throw EvalError.ArgumentCountError(formals: 1, args: expr)
    }
    try compiler.compile(promise, in: env, inTailPos: false)
    compiler.emit(.Force)
    compiler.emit(.StoreInPromise)
    return false
  }
  
  //-------- MARK: - Symbol primitives
  
  func isSymbol(expr: Expr) -> Expr {
    if case .Sym(_) = expr {
      return .True
    }
    return .False
  }
  
  func gensym(expr: Expr?) throws -> Expr {
    return .Sym(context.symbols.gensym(try expr?.asStr() ?? "g"))
  }
  
  func stringToSymbol(expr: Expr) throws -> Expr {
    return .Sym(context.symbols.intern(try expr.asStr()))
  }
  
  func symbolToString(expr: Expr) throws -> Expr {
    return .Str(Box(try expr.asSymbol().description))
  }
  
  func symtable() -> Expr {
    var res = Expr.Null
    for sym in self.context.symbols {
      res = .Pair(.Sym(sym), res)
    }
    return res
  }
  
  
  //-------- MARK: - Boolean primitives
  
  func isBoolean(expr: Expr) -> Expr {
    switch expr {
      case .True, .False:
        return .True
      default:
        return .False
    }
  }
  
  func not(expr: Expr) -> Expr {
    if case .False = expr {
      return .True
    }
    return .False
  }
  
  func compileAnd(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .Pair(_, let exprs) = expr else {
      preconditionFailure()
    }
    var trueJumps = [Int]()
    var args = exprs
    while case .Pair(let arg, let rest) = args {
      if rest == .Null {
        try compiler.compile(arg, in: env, inTailPos: tail)
        for ip in trueJumps {
          compiler.patch(.And(compiler.offsetToNext(ip)), at: ip)
        }
        return false
      } else {
        try compiler.compile(arg, in: env, inTailPos: false)
        trueJumps.append(compiler.emitPlaceholder())
      }
      args = rest
    }
    guard args == .Null else {
      throw EvalError.MalformedArgumentList(exprs)
    }
    compiler.emit(.PushTrue)
    return false
  }
  
  func compileOr(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .Pair(_, let exprs) = expr else {
      preconditionFailure()
    }
    var falseJumps = [Int]()
    var args = exprs
    while case .Pair(let arg, let rest) = args {
      if rest == .Null {
        try compiler.compile(arg, in: env, inTailPos: tail)
        for ip in falseJumps {
          compiler.patch(.Or(compiler.offsetToNext(ip)), at: ip)
        }
        return false
      } else {
        try compiler.compile(arg, in: env, inTailPos: false)
        falseJumps.append(compiler.emitPlaceholder())
      }
      args = rest
    }
    guard args == .Null else {
      throw EvalError.MalformedArgumentList(exprs)
    }
    compiler.emit(.PushFalse)
    return false
  }
  
  
  //-------- MARK: - System primitives
  
  func gc() -> Expr {
    context.console.print("BEFORE: " + context.objects.description + "\n")
    let res = Expr.Fixnum(Int64(self.context.objects.collectGarbage()))
    context.console.print("AFTER: " + context.objects.description + "\n")
    return res
  }
  
  func exit() -> Expr {
    NSApplication.sharedApplication().terminate(self)
    return .Undef
  }
  
  func compileTime(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .Pair(_, .Pair(let exec, .Null)) = expr else {
      throw EvalError.ArgumentCountError(formals: 1, args: expr)
    }
    compiler.emit(.PushCurrentTime)
    try compiler.compile(exec, in: env, inTailPos: false)
    compiler.emit(.Swap)
    compiler.emit(.PushCurrentTime)
    compiler.emit(.Swap)
    compiler.emit(.FlMinus)
    try compiler.pushValue(.Str(Box("elapsed time = ")))
    compiler.emit(.Display)
    compiler.emit(.Display)
    compiler.emit(.Newline)
    return false
  }
  
  func compile(exprs: Arguments) throws -> Expr {
    let compiler = Compiler(self.context, .Interaction)
    var seq = Expr.Null
    for expr in exprs.reverse() {
      seq = .Pair(expr, seq)
    }
    try compiler.compileBody(seq)
    let code = compiler.bundle()
    context.console.print(code.description)
    return .Void
  }
  
  func disassemble(expr: Expr) throws -> Expr {
    guard case .Proc(let proc) = expr else {
      throw EvalError.TypeError(expr, [.ProcedureType])
    }
    switch proc.kind {
      case .Closure(let captured, let code):
        context.console.print(code.description)
        if captured.count > 0 {
          context.console.print("CAPTURED:\n")
          for i in captured.indices {
            context.console.print("  \(i): \(captured[i])\n")
          }
        }
      default:
        context.console.print("cannot disassemble \(expr)")
    }
    return .Void
  }
}
