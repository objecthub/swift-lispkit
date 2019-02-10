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

import Foundation

public final class CoreLibrary: NativeLibrary {

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "core"]
  }

  internal static let idProc = Procedure("identity", CoreLibrary.identity)
  internal static let voidProc = Procedure("void", CoreLibrary.voidConst)

  /// Declarations of the library.
  public override func declarations() {

    // Basic primitives
    self.define(CoreLibrary.idProc)
    self.define(Procedure("procedure?", isProcedure))
    self.define(Procedure("eval", eval, compileEval))
    self.define(Procedure("apply", apply, compileApply))
    self.define(Procedure("equal?", isEqual, compileEqual))
    self.define(Procedure("eqv?", isEqv, compileEqv))
    self.define(Procedure("eq?", isEq, compileEq))
    self.define(SpecialForm("quote", compileQuote))
    self.define(SpecialForm("quasiquote", compileQuasiquote))
    self.define(SpecialForm("lambda", compileLambda))
    self.define(SpecialForm("λ", compileLambda))
    self.define(SpecialForm("case-lambda", compileCaseLambda))
    self.define(SpecialForm("case-λ", compileCaseLambda))

    // Definition primitives
    self.define(SpecialForm("define", compileDefine))
    self.define(SpecialForm("define-values", compileDefineValues))
    self.define(SpecialForm("define-syntax", compileDefineSyntax))
    self.define(SpecialForm("define-library", compileDefineLibrary))
    self.define(SpecialForm("syntax-rules", compileSyntaxRules))
    self.define(SpecialForm("set!", compileSet))

    // Delayed execution
    self.define(Procedure("promise?", isPromise))
    self.define(Procedure("make-promise", makePromise))
    self.define(Procedure("eager", makePromise))
    self.define(SpecialForm("delay", compileDelay))
    self.define(SpecialForm("delay-force", compileDelayForce))
    self.define(SpecialForm("lazy", compileDelayForce))
    self.define(Procedure("stream?", isStream))
    self.define(Procedure("make-stream", makeStream))
    self.define(Procedure("stream-eager", makeStream))
    self.define(SpecialForm("stream-delay", compileStreamDelay))
    self.define(SpecialForm("stream-delay-force", compileStreamDelayForce))
    self.define(SpecialForm("stream-lazy", compileStreamDelayForce))
    self.define(Procedure("force", compileForce, in: self.context))

    // Symbol primitives
    self.define(Procedure("symbol?", isSymbol))
    self.define(Procedure("gensym", gensym))
    self.define(Procedure("symbol=?", stringEquals))
    self.define(Procedure("string->symbol", stringToSymbol))
    self.define(Procedure("symbol->string", symbolToString))

    // Boolean primitives
    self.define(Procedure("boolean?", isBoolean))
    self.define(Procedure("boolean=?", isBooleanEq))
    self.define(SpecialForm("and", compileAnd))
    self.define(SpecialForm("or", compileOr))
    self.define(Procedure("not", not, compileNot))

    // Conditional & inclusion compilation
    self.define(SpecialForm("cond-expand", compileCondExpand))
    self.define(SpecialForm("include", include))
    self.define(SpecialForm("include-ci", includeCi))

    // Multiple values
    self.define(Procedure("values", values, compileValues))
    self.define(Procedure("apply-with-values", applyWithValues))
    self.define("call-with-values", via:
      "(define (call-with-values producer consumer) (apply-with-values consumer (producer)))")

    // Environments
    self.define(Procedure("environment?", isEnvironment))
    self.define(Procedure("environment", environment))
    self.define(Procedure("environment-bound-names", environmentBoundNames))
    self.define(Procedure("environment-bindings", environmentBindings))
    self.define(Procedure("environment-bound?", environmentBound))
    self.define(Procedure("environment-lookup", environmentLookup))
    self.define(Procedure("environment-assignable?", environmentAssignable))
    self.define(Procedure("environment-assign!", environmentAssign))
    self.define(Procedure("scheme-report-environment", schemeReportEnvironment))
    self.define(Procedure("null-environment", nullEnvironment))
    self.define(Procedure("interaction-environment", interactionEnvironment))

    // Syntax errors
    self.define(SpecialForm("syntax-error", compileSyntaxError))

    // Helpers
    self.define(Procedure("void", CoreLibrary.voidConst))
    self.define(Procedure("void?", CoreLibrary.isVoid))
    self.define(CoreLibrary.idProc)
  }


  //-------- MARK: - Basic primitives

  static func identity(expr: Expr) -> Expr {
    return expr
  }

  private func eval(args: Arguments) throws -> Code {
    guard args.count > 0 else {
      throw RuntimeError.argumentCount(num: 1, args: .makeList(args))
    }
    guard args.count < 3 else {
      throw RuntimeError.argumentCount(num: 2, args: .makeList(args))
    }
    var env = self.context.global
    if args.count == 2 {
      env = .global(try args[args.startIndex + 1].asEnvironment())
    }
    return try Compiler.compile(expr: .pair(args.first!, .null),
                                in: env,
                                optimize: true)
  }

  private func compileEval(compiler: Compiler, e: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let expr, let rest)) = e else {
      throw RuntimeError.argumentCount(min: 1, max: 2, expr: e)
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
        throw RuntimeError.argumentCount(min: 1, max: 2, args: .pair(expr, rest))
    }
    compiler.emit(.compile)
    return compiler.call(0, inTailPos: tail)
  }

  private func apply(args: Arguments) throws -> (Procedure, Exprs) {
    guard args.count > 1 else {
      throw RuntimeError.argumentCount(min: 2, args: .makeList(args))
    }
    guard case .procedure(let proc) = args.first! else {
      throw RuntimeError.type(args.first!, expected: [.procedureType])
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
      throw RuntimeError.type(args.last!, expected: [.properListType])
    }
    return (proc, exprs)
  }

  private func compileApply(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let fun, let arglist)) = expr else {
      throw RuntimeError.argumentCount(min: 2, expr: expr)
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
    throw RuntimeError.argumentCount(min: 2, expr: expr)
  }

  private func isEqual(this: Expr, that: Expr) -> Expr {
    return .makeBoolean(equalExpr(that, this))
  }

  private func compileEqual(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let this, .pair(let that, .null))) = expr else {
      throw RuntimeError.argumentCount(num: 2, expr: expr)
    }
    _ = try compiler.compile(this, in: env, inTailPos: false)
    _ = try compiler.compile(that, in: env, inTailPos: false)
    compiler.emit(.equal)
    return false
  }

  private func isEqv(this: Expr, that: Expr) -> Expr {
    return .makeBoolean(eqvExpr(that, this))
  }

  private func compileEqv(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let this, .pair(let that, .null))) = expr else {
      throw RuntimeError.argumentCount(num: 2, expr: expr)
    }
    _ = try compiler.compile(this, in: env, inTailPos: false)
    _ = try compiler.compile(that, in: env, inTailPos: false)
    compiler.emit(.eqv)
    return false
  }

  private func isEq(this: Expr, that: Expr) -> Expr {
    return .makeBoolean(eqExpr(that, this))
  }

  private func compileEq(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let this, .pair(let that, .null))) = expr else {
      throw RuntimeError.argumentCount(num: 2, expr: expr)
    }
    _ = try compiler.compile(this, in: env, inTailPos: false)
    _ = try compiler.compile(that, in: env, inTailPos: false)
    compiler.emit(.eq)
    return false
  }

  private func compileQuote(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let arg, .null)) = expr else {
      throw RuntimeError.argumentCount(num: 1, expr: expr)
    }
    try compiler.pushValue(arg)
    return false
  }

  private func compileQuasiquote(compiler: Compiler,
                                 expr: Expr,
                                 env: Env,
                                 tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let arg, .null)) = expr else {
      throw RuntimeError.argumentCount(num: 1, expr: expr)
    }
    switch arg {
      case .pair(_, _):
        return try compiler.compile(reduceQQ(arg, in: env),
                                    in: env,
                                    inTailPos: tail)
      case .vector(let vector):
        var nvec = 0
        var nelem = 0
        for expr in vector.exprs {
          if case .pair(let car, .pair(let elem, let cddr)) = expr {
            switch car {
              case .symbol(let s) where s.interned == self.context.symbols.unquote:
                guard cddr == .null else {
                  throw RuntimeError.eval(.invalidContextInQuasiquote,
                                          .symbol(self.context.symbols.unquote),
                                          expr)
                }
                try compiler.compile(elem, in: env, inTailPos: false)
                nelem += 1
                continue
              case .symbol(let s) where s.interned == self.context.symbols.unquoteSplicing:
                guard cddr == .null else {
                  throw RuntimeError.eval(.invalidContextInQuasiquote,
                                          .symbol(self.context.symbols.unquoteSplicing),
                                          expr)
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
      return Expr.makeList(.symbol(Symbol(self.context.symbols.quote, env.global)), expr)
    }
    switch car {
      case .symbol(let s) where s.interned == self.context.symbols.unquote:
        guard case .pair(let cadr, .null) = cdr else {
          throw RuntimeError.eval(.invalidContextInQuasiquote,
                                  .symbol(self.context.symbols.unquote),
                                  car)
        }
        return cadr
      case .symbol(let s) where s.interned == self.context.symbols.quasiquote:
        guard case .pair(let cadr, .null) = cdr else {
          throw RuntimeError.eval(.invalidContextInQuasiquote,
                                  .symbol(self.context.symbols.quasiquote),
                                  car)
        }
        return try reduceQQ(reduceQQ(cadr, in: env), in: env)
      case .symbol(let s) where s.interned == self.context.symbols.unquoteSplicing:
        throw RuntimeError.eval(.invalidContextInQuasiquote,
                                .symbol(self.context.symbols.unquoteSplicing),
                                car)
      case .pair(.symbol(let s), let cdar) where s.interned == self.context.symbols.unquoteSplicing:
        guard case .pair(let cadar, .null) = cdar else {
          throw RuntimeError.eval(.invalidContextInQuasiquote,
                                  .symbol(self.context.symbols.unquoteSplicing),
                                  car)
        }
        return Expr.makeList(.symbol(Symbol(self.context.symbols.append, env.global)),
                             cadar,
                             try reduceQQ(cdr, in: env))
      default:
        return Expr.makeList(.symbol(Symbol(self.context.symbols.cons, env.global)),
                             try reduceQQ(car, in: env),
                             try reduceQQ(cdr, in: env))
    }
  }

  private func compileLambda(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let arglist, let body)) = expr else {
      throw RuntimeError.argumentCount(num: 1, expr: expr)
    }
    try compiler.compileLambda(nil, arglist, body, env)
    return false
  }

  private func compileCaseLambda(compiler: Compiler,
                                 expr: Expr,
                                 env: Env,
                                 tail: Bool) throws -> Bool {
    guard case .pair(_, let cases) = expr else {
      preconditionFailure("broken case-lambda invocation")
    }
    try compiler.compileCaseLambda(nil, cases, env)
    return false
  }

  private func isProcedure(expr: Expr) -> Expr {
    if case .procedure(_) = expr {
      return .true
    }
    return .false
  }

  //-------- MARK: - Definition primitives

  private func compileDefine(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    // Extract signature and definition
    guard case .pair(_, .pair(let sig, let def)) = expr else {
      throw RuntimeError.argumentCount(num: 2, expr: expr)
    }
    // Check that define is not executed in a local environment
    if case .local(_) = env {
      throw RuntimeError.eval(.defineInLocalEnv, sig, def)
    }
    // Compile definition and store result in global environment
    switch sig {
      case .symbol(let sym):
        guard case .pair(let value, .null) = def else {
          throw RuntimeError.eval(.malformedDefinition, Expr.makeList(def))
        }
        let index = compiler.registerConstant(sig)
        try compiler.compile(value, in: env, inTailPos: false)
        let environment = env.environment!
        if let libName = environment.libraryName, environment.isImported(sym) {
          throw RuntimeError.eval(.redefinitionOfImport, sig, libName)
        }
        let loc = environment.forceDefinedLocationRef(for: sym).location!
        compiler.patchMakeClosure(index)
        compiler.emit(.defineGlobal(loc))
        compiler.emit(.pushConstant(index))
        return false
      case .pair(.symbol(let sym), let arglist):
        let index = compiler.registerConstant(.symbol(sym))
        try compiler.compileLambda(index, arglist, def, env)
        let environment = env.environment!
        if let libName = environment.libraryName, environment.isImported(sym) {
          throw RuntimeError.eval(.redefinitionOfImport, .symbol(sym), libName)
        }
        let loc = environment.forceDefinedLocationRef(for: sym).location!
        compiler.emit(.defineGlobal(loc))
        compiler.emit(.pushConstant(index))
        return false
      default:
        throw RuntimeError.eval(.malformedDefinition, .pair(sig, Expr.makeList(def)))
    }
  }

  private func compileDefineValues(compiler: Compiler,
                                   expr: Expr,
                                   env: Env,
                                   tail: Bool) throws -> Bool {
    // Extract signature and definition
    guard case .pair(_, .pair(let sig, .pair(let value, .null))) = expr else {
      throw RuntimeError.argumentCount(num: 2, expr: expr)
    }
    // Check that define is not executed in a local environment
    if case .local(_) = env {
      throw RuntimeError.eval(.defineInLocalEnv, sig, value)
    }
    // Compile definition and store result in global environment
    switch sig {
      case .null:
        try compiler.compile(value, in: env, inTailPos: false)
        compiler.emit(.unpack(0, false))
        compiler.emit(.pushVoid)
        return false
      case .symbol(let sym):
        let index = compiler.registerConstant(sig)
        try compiler.compile(value, in: env, inTailPos: false)
        let environment = env.environment!
        if let libName = environment.libraryName, environment.isImported(sym) {
          throw RuntimeError.eval(.redefinitionOfImport, sig, libName)
        }
        compiler.emit(.unpack(0, true))
        let loc = environment.forceDefinedLocationRef(for: sym).location!
        compiler.emit(.defineGlobal(loc))
        compiler.emit(.pushConstant(index))
        return false
      case .pair(_, _):
        try compiler.compile(value, in: env, inTailPos: false)
        let environment = env.environment!
        var vars = sig
        var syms = [Symbol]()
        while case .pair(.symbol(let sym), let rest) = vars {
          if syms.contains(sym) {
            throw RuntimeError.eval(.duplicateBinding, .symbol(sym), sig)
          } else if let libName = environment.libraryName, environment.isImported(sym) {
            throw RuntimeError.eval(.redefinitionOfImport, .symbol(sym), libName)
          }
          syms.append(sym)
          vars = rest
        }
        switch vars {
          case .null:
            compiler.emit(.unpack(syms.count, false))
          case .symbol(let sym):
            compiler.emit(.unpack(syms.count, true))
            syms.append(sym)
          default:
            throw RuntimeError.eval(.malformedDefinition, .pair(sig, Expr.makeList(value)))
        }
        var res: Expr = .null
        for sym in syms.reversed() {
          let loc = environment.forceDefinedLocationRef(for: sym).location!
          compiler.emit(.defineGlobal(loc))
          res = .pair(.symbol(sym), res)
        }
        if syms.count == 0 {
          res = .void
        } else if syms.count == 1 {
          res = .symbol(syms.first!)
        } else {
          res = .values(res)
        }
        let index = compiler.registerConstant(res)
        compiler.emit(.pushConstant(index))
        return false
      default:
        throw RuntimeError.eval(.malformedDefinition, .pair(sig, Expr.makeList(value)))
    }
  }

  private func compileDefineSyntax(compiler: Compiler,
                                   expr: Expr,
                                   env: Env,
                                   tail: Bool) throws -> Bool {
    // Extract keyword and transformer definition
    guard case .pair(_, .pair(let kword, .pair(let transformer, .null))) = expr else {
      throw RuntimeError.argumentCount(num: 2, expr: expr)
    }
    // Check that the keyword is a symbol
    guard case .symbol(let sym) = kword else {
      throw RuntimeError.type(kword, expected: [.symbolType])
    }
    // Check that define is not executed in a local environment
    if case .local(_) = env {
      throw RuntimeError.eval(.defineSyntaxInLocalEnv, .symbol(sym), transformer)
    }
    // Compile transformer and store it as global keyword
    let index = compiler.registerConstant(kword)
    let oldSyntaxSym = compiler.syntaxSym
    compiler.syntaxSym = sym
    try compiler.compile(transformer, in: env, inTailPos: false)
    compiler.syntaxSym = oldSyntaxSym
    let environment = env.environment!
    if let libName = environment.libraryName, environment.isImported(sym) {
      throw RuntimeError.eval(.redefinitionOfImport, .symbol(sym), libName)
    }
    let loc = environment.forceDefinedLocationRef(for: sym).location!
    compiler.emit(.makeSyntax(index))
    compiler.emit(.defineGlobal(loc))
    compiler.emit(.pushConstant(index))
    return false
  }

  private func compileDefineLibrary(compiler: Compiler,
                                    expr: Expr,
                                    env: Env,
                                    tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let name, let decls)) = expr else {
      throw RuntimeError.argumentCount(num: 1, expr: expr)
    }
    try compiler.context.libraries.load(name: name,
                                        declarations: decls,
                                        origin: compiler.sourceDirectory)
    compiler.emit(.pushVoid)
    return false
  }

  private func compileSyntaxRules(compiler: Compiler,
                                  expr: Expr,
                                  env: Env,
                                  tail: Bool) throws -> Bool {
    var ellipsis: Symbol? = nil
    var lit: Expr = .null
    var transRules: Expr = .null
    switch expr {
      case .pair(_, .pair(.symbol(let sym), .pair(let literals, let patTrans))):
        ellipsis = sym.interned
        lit = literals
        transRules = patTrans
      case .pair(_, .pair(let literals, let patTrans)):
        lit = literals
        transRules = patTrans
      default:
        throw RuntimeError.argumentCount(num: 1, expr: expr)
    }
    var patterns = Exprs()
    var templates = Exprs()
    while case .pair(let rule, let rest) = transRules {
      guard case .pair(let car, .pair(let cadr, .null)) = rule else {
        throw RuntimeError.eval(.malformedSyntaxRule, rule)
      }
      guard case .pair(_, let pat) = car else {
        throw RuntimeError.eval(.malformedSyntaxRulePattern, car)
      }
      if let errExpr = check(pattern: car, ellipsis: ellipsis ?? self.context.symbols.ellipsis) {
        throw RuntimeError.eval(.malformedPatternInSyntaxRule, errExpr, car)
      }
      patterns.append(pat)
      templates.append(cadr)
      transRules = rest
    }
    guard transRules == .null else {
      throw RuntimeError.eval(.malformedSyntaxRulePattern, transRules)
    }
    var literalSet = Set<Symbol>()
    var expr = lit
    while case .pair(.symbol(let sym), let cdr) = expr {
      literalSet.insert(sym.interned)
      expr = cdr
    }
    guard expr == .null else {
      throw RuntimeError.eval(.malformedSyntaxRuleLiterals, lit)
    }
    let rules = SyntaxRules(context: self.context,
                            name: compiler.syntaxSym,
                            literals: literalSet,
                            ellipsis: ellipsis,
                            patterns: patterns,
                            templates: templates,
                            in: compiler.rulesEnv)
    compiler.emit(.pushConstant(compiler.registerConstant(.procedure(Procedure(rules)))))
    return false
  }

  private func check(pattern: Expr, ellipsis: Symbol) -> Expr? {
    var expr = pattern
    var ellipsisFound = false
    var first = true
    while case .pair(let car, let cdr) = expr {
      if case .symbol(ellipsis) = car {
        if ellipsisFound || first {
          return pattern
        }
        ellipsisFound = true
      } else if let errExpr = self.check(pattern: car, ellipsis: ellipsis) {
        return errExpr
      }
      first = false
      expr = cdr
    }
    switch expr {
      case .eof, .null, .true, .false, .symbol(_), .string(_), .char(_),
           .fixnum(_), .bignum(_), .rational(_, _), .flonum(_), .complex(_):
        return nil
      case .vector(let vector):
        ellipsisFound = false
        first = true
        for pat in vector.exprs {
          if case .symbol(ellipsis) = pat {
            if ellipsisFound || first {
              return expr
            }
            ellipsisFound = true
          } else if let errExpr = check(pattern: pat, ellipsis: ellipsis) {
            return errExpr
          }
          first = false
        }
        return nil
      default:
        return expr
    }
  }

  private func compileSet(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let symbol, .pair(let value, .null))) = expr else {
      throw RuntimeError.argumentCount(num: 2, expr: expr)
    }
    let sym = try symbol.asSymbol()
    try compiler.compile(value, in: env, inTailPos: false)
    compiler.setValueOf(sym, in: env)
    compiler.emit(.pushVoid)
    return false
  }

  //-------- MARK: - Delayed execution

  private func isPromise(expr: Expr) -> Expr {
    if case .promise(let future) = expr,
       case .promise = future.kind {
      return .true
    }
    return .false
  }

  private func makePromise(expr: Expr) -> Expr {
    return .promise(Promise(kind: .promise, value: expr))
  }

  private func compileDelayForce(compiler: Compiler,
                                 expr: Expr,
                                 env: Env,
                                 tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let delayed, .null)) = expr else {
      throw RuntimeError.argumentCount(num: 1, expr: expr)
    }
    try compiler.compileLambda(nil,
                               .null,
                               .pair(delayed, .null),
                               env)
    compiler.emit(.makePromise)
    return false
  }

  private func compileDelay(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let delayed, .null)) = expr else {
      throw RuntimeError.argumentCount(num: 1, expr: expr)
    }
    let body = Expr.pair(.makeList(.symbol(compiler.context.symbols.makePromise), delayed), .null)
    try compiler.compileLambda(nil, .null, body, env)
    compiler.emit(.makePromise)
    return false
  }

  private func compileForce(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let promise, .null)) = expr else {
      throw RuntimeError.argumentCount(num: 1, expr: expr)
    }
    try compiler.compile(promise, in: env, inTailPos: false)
    compiler.emit(.force)
    compiler.emit(.storeInPromise)
    return false
  }

  private func isStream(expr: Expr) -> Expr {
    if case .promise(let future) = expr,
       case .stream = future.kind {
      return .true
    }
    return .false
  }

  private func makeStream(expr: Expr) -> Expr {
    return .promise(Promise(kind: .stream, value: expr))
  }

  private func compileStreamDelayForce(compiler: Compiler,
                                       expr: Expr,
                                       env: Env,
                                       tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let delayed, .null)) = expr else {
      throw RuntimeError.argumentCount(num: 1, expr: expr)
    }
    try compiler.compileLambda(nil,
                               .null,
                               .pair(delayed, .null),
                               env)
    compiler.emit(.makeStream)
    return false
  }

  private func compileStreamDelay(compiler: Compiler,
                                  expr: Expr,
                                  env: Env,
                                  tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let delayed, .null)) = expr else {
      throw RuntimeError.argumentCount(num: 1, expr: expr)
    }
    let body = Expr.pair(.makeList(.symbol(compiler.context.symbols.makeStream), delayed), .null)
    try compiler.compileLambda(nil, .null, body, env)
    compiler.emit(.makeStream)
    return false
  }


  //-------- MARK: - Symbol primitives

  private func isSymbol(expr: Expr) -> Expr {
    if case .symbol(_) = expr {
      return .true
    }
    return .false
  }

  private func gensym(expr: Expr?) throws -> Expr {
    return .symbol(self.context.symbols.gensym(try expr?.asString() ?? "g"))
  }

  private func stringEquals(expr: Expr, args: Arguments) throws -> Expr {
    let sym = try expr.asSymbol()
    for arg in args {
      guard try sym == arg.asSymbol() else {
        return .false
      }
    }
    return .true
  }

  private func stringToSymbol(expr: Expr) throws -> Expr {
    return .symbol(self.context.symbols.intern(try expr.asString()))
  }

  private func symbolToString(expr: Expr) throws -> Expr {
    return .makeString(try expr.asSymbol().rawIdentifier)
  }

  //-------- MARK: - Boolean primitives

  private func isBoolean(expr: Expr) -> Expr {
    switch expr {
      case .true, .false:
        return .true
      default:
        return .false
    }
  }

  private func isBooleanEq(_ expr1: Expr, _ expr2: Expr, args: Arguments) throws -> Expr {
    var fst: Bool
    switch expr1 {
      case .true:
        fst = true
      case .false:
        fst = false
      default:
        throw RuntimeError.type(expr1, expected: [.booleanType])
    }
    switch expr2 {
      case .true:
        guard fst else {
          return .false
        }
      case .false:
        guard !fst else {
          return .false
        }
      default:
        throw RuntimeError.type(expr1, expected: [.booleanType])
    }
    for arg in args {
      switch arg {
        case .true:
          guard fst else {
            return .false
          }
        case .false:
          guard !fst else {
            return .false
          }
        default:
          throw RuntimeError.type(expr1, expected: [.booleanType])
      }
    }
    return .true
  }

  private func not(expr: Expr) -> Expr {
    if case .false = expr {
      return .true
    }
    return .false
  }

  private func compileNot(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let arg, .null)) = expr else {
      throw RuntimeError.argumentCount(num: 1, expr: expr)
    }
    _ = try compiler.compile(arg, in: env, inTailPos: false)
    compiler.emit(.not)
    return false
  }

  private func compileAnd(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
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
      throw RuntimeError.eval(.malformedArgumentList, exprs)
    }
    compiler.emit(.pushTrue)
    return false
  }

  private func compileOr(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
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
      throw RuntimeError.eval(.malformedArgumentList, exprs)
    }
    compiler.emit(.pushFalse)
    return false
  }

  private func compileCondExpand(compiler: Compiler,
                                 expr: Expr,
                                 env: Env,
                                 tail: Bool) throws -> Bool {
    // Extract expansion case list
    guard  case .pair(_, let caseList) = expr else {
      preconditionFailure()
    }
    var cases = caseList
    // Iterate through all expansion cases
    while case .pair(let cas, let rest) = cases {
      switch cas {
        case .pair(.symbol(compiler.context.symbols.else), let exprs):
          guard rest == .null else {
            throw RuntimeError.eval(.malformedCondExpandClause, cases)
          }
          return try compiler.compileSeq(exprs,
                                         in: env,
                                         inTailPos: tail,
                                         localDefine: false)
        case .pair(let reqs, let exprs):
          guard let featureReq = FeatureRequirement(reqs, in: compiler.context) else {
            throw RuntimeError.eval(.malformedCondExpandClause, cas)
          }
          if try featureReq.valid(in: compiler.context) {
            return try compiler.compileSeq(exprs,
                                           in: env,
                                           inTailPos: tail,
                                           localDefine: false)
          }
        default:
          throw RuntimeError.eval(.malformedCondExpandClause, cas)
      }
      cases = rest
    }
    return try compiler.compileSeq(.null,
                                   in: env,
                                   inTailPos: tail,
                                   localDefine: false)
  }

  private func include(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard  case .pair(_, let filenameList) = expr else {
      preconditionFailure()
    }
    return try self.include(compiler: compiler,
                            filenameList: filenameList,
                            foldCase: false,
                            env: env,
                            tail: tail)
  }

  private func includeCi(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard  case .pair(_, let filenameList) = expr else {
      preconditionFailure()
    }
    return try self.include(compiler: compiler,
                            filenameList: filenameList,
                            foldCase: true,
                            env: env,
                            tail: tail)
  }

  private func include(compiler: Compiler,
                       filenameList: Expr,
                       foldCase: Bool,
                       env: Env,
                       tail: Bool) throws -> Bool {
    var filenames = filenameList
    while case .pair(let filename, let rest) = filenames {
      let str = try filename.asPath()
      let resolvedName =
        self.context.fileHandler.filePath(forFile: str, relativeTo: compiler.sourceDirectory) ??
          self.context.fileHandler.path(str, relativeTo: compiler.sourceDirectory)
      let exprs = try self.context.machine.parse(file: resolvedName, foldCase: foldCase)
      try compiler.compileSeq(exprs,
                              in: env,
                              inTailPos: false,
                              localDefine: false,
                              inDirectory: self.context.fileHandler.directory(resolvedName))
      filenames = rest
    }
    guard filenames.isNull else {
      throw RuntimeError.eval(.malformedArgumentList, filenameList)
    }
    return try compiler.compileSeq(.null,
                                   in: env,
                                   inTailPos: tail,
                                   localDefine: false)
  }

  //-------- MARK: - Multiple values

  private func values(args: Arguments) -> Expr {
    switch args.count {
      case 0:
        return .void // .values(.null)
      case 1:
        return args.first!
      default:
        var res = Expr.null
        var idx = args.endIndex
        while idx > args.startIndex {
          idx = args.index(before: idx)
          res = .pair(args[idx], res)
        }
        return .values(res)
    }
  }

  private func compileValues(_ compiler: Compiler,
                             expr: Expr,
                             env: Env,
                             tail: Bool) throws -> Bool {
    guard case .pair(_, let cdr) = expr else {
      preconditionFailure()
    }
    switch try compiler.compileExprs(cdr, in: env) {
      case 0:
        compiler.emit(.pushVoid)
      case 1:
        break
      case let n:
        compiler.emit(.pack(n))
    }
    return false
  }

  private func applyWithValues(args: Arguments) throws -> (Procedure, Exprs) {
    guard args.count == 2 else {
      throw RuntimeError.argumentCount(num: 2, args: .makeList(args))
    }
    guard case .procedure(let consumer) = args.first! else {
      throw RuntimeError.type(args.first!, expected: [.procedureType])
    }
    let x = args[args.startIndex + 1]
    if case .void = x {
      return (consumer, [])
    }
    guard case .values(let expr) = x else {
      return (consumer, [x])
    }
    var exprs = Exprs()
    var next = expr
    while case .pair(let arg, let rest) = next {
      exprs.append(arg)
      next = rest
    }
    return (consumer, exprs)
  }

  //-------- MARK: - Environments

  private func isEnvironment(expr: Expr) -> Expr {
    switch expr {
      case .env(_):
        return .true
      default:
        return .false
    }
  }

  private func environment(exprs: Arguments) throws -> Expr {
    var importSets = [ImportSet]()
    for expr in exprs {
      guard let importSet = ImportSet(expr, in: self.context) else {
        throw RuntimeError.eval(.malformedImportSet, expr)
      }
      importSets.append(importSet)
    }
    return Expr.env(try Environment(in: self.context, importing: importSets))
  }

  private func environmentBoundNames(expr: Expr) throws -> Expr {
    guard case .env(let environment) = expr else {
      throw RuntimeError.type(expr, expected: [.envType])
    }
    var res = Expr.null
    for sym in environment.boundSymbols {
      res = .pair(.symbol(sym), res)
    }
    return res
  }

  private func environmentBindings(expr: Expr) throws -> Expr {
    guard case .env(let environment) = expr else {
      throw RuntimeError.type(expr, expected: [.envType])
    }
    var res = Expr.null
    for sym in environment.boundSymbols {
      if let value = environment[sym] {
        res = .pair(.pair(.symbol(sym), value), res)
      }
    }
    return res
  }

  private func environmentBound(expr: Expr, symbol: Expr) throws -> Expr {
    guard case .env(let environment) = expr else {
      throw RuntimeError.type(expr, expected: [.envType])
    }
    return .makeBoolean(!environment.isUndefined(try symbol.asSymbol()))
  }

  private func environmentLookup(expr: Expr, symbol: Expr) throws -> Expr {
    guard case .env(let environment) = expr else {
      throw RuntimeError.type(expr, expected: [.envType])
    }
    guard let value = environment[try symbol.asSymbol()] else {
      throw RuntimeError.eval(.notBoundInEnvironment, symbol)
    }
    return value
  }

  private func environmentAssignable(expr: Expr, symbol: Expr) throws -> Expr {
    guard case .env(let environment) = expr else {
      throw RuntimeError.type(expr, expected: [.envType])
    }
    let sym = try symbol.asSymbol()
    return .makeBoolean(!environment.isUndefined(sym) && !environment.isImmutable(sym))
  }

  private func environmentAssign(expr: Expr, symbol: Expr, value: Expr) throws -> Expr {
    guard case .env(let environment) = expr else {
      throw RuntimeError.type(expr, expected: [.envType])
    }
    let sym = try symbol.asSymbol()
    guard !environment.isUndefined(sym) && !environment.isImmutable(sym) else {
      throw RuntimeError.eval(.bindingImmutable, symbol)
    }
    _ = environment.set(sym, to: value)
    return .void
  }

  private func schemeReportEnvironment(expr: Expr) throws -> Expr {
    var importSets = [ImportSet]()
    guard let importSet = ImportSet(.pair(.symbol(self.context.symbols.scheme),
                                          .pair(.symbol(self.context.symbols.r5rs), .null)),
                                    in: self.context) else {
      throw RuntimeError.eval(.malformedImportSet, expr)
    }
    importSets.append(importSet)
    return Expr.env(try Environment(in: self.context, importing: importSets))
  }

  private func nullEnvironment(expr: Expr) throws -> Expr {
    var importSets = [ImportSet]()
    guard let importSet = ImportSet(.pair(.symbol(self.context.symbols.scheme),
                                          .pair(.symbol(self.context.symbols.r5rsSyntax), .null)),
                                    in: self.context) else {
      throw RuntimeError.eval(.malformedImportSet, expr)
    }
    importSets.append(importSet)
    return Expr.env(try Environment(in: self.context, importing: importSets))
  }

  private func interactionEnvironment() -> Expr {
    return .env(self.context.environment)
  }

  //-------- MARK: - Syntax errors

  private func compileSyntaxError(compiler: Compiler,
                                  expr: Expr,
                                  env: Env,
                                  tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let message, let irritants)) = expr else {
      throw RuntimeError.argumentCount(num: 1, expr: expr)
    }
    throw RuntimeError.custom("syntax error",
                              message.unescapedDescription,
                              Array(irritants.toExprs().0))
  }


  //-------- MARK: - System primitives

  static func voidConst() -> Expr {
    return .void
  }

  static func isVoid(expr: Expr) -> Expr {
    switch expr {
      case .void:
        return .true
      default:
        return .false
    }
  }
}
