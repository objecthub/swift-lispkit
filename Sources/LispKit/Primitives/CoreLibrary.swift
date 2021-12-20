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
  
  internal static let idProc = Procedure("identity", CoreLibrary.identity)
  internal static let voidProc = Procedure("void", CoreLibrary.voidConst)
  
  /// The compileAndEval procedure
  private(set) public var loader: Procedure!
  
  /// The define procedure
  private(set) public var defineSpecial: SpecialForm!
  
  /// The defineValues procedure
  private(set) public var defineValuesSpecial: SpecialForm!
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "core"]
  }
  
  /// Declarations of the library.
  public override func declarations() {
    // Basic primitives
    self.define(CoreLibrary.idProc)
    self.define(Procedure("eval", eval, compileEval))
    self.define(Procedure("apply", apply, compileApply))
    self.define(Procedure("equal?", isEqual, compileEqual))
    self.define(Procedure("eqv?", isEqv, compileEqv))
    self.define(Procedure("eq?", isEq, compileEq))
    self.define(SpecialForm("quote", compileQuote))
    self.define(SpecialForm("quasiquote", compileQuasiquote))
    self.define(SpecialForm("lambda", compileLambda))
    self.define(SpecialForm("λ", compileLambda))
    self.define(SpecialForm("lambda/tag", compileLambdaTag))
    self.define(SpecialForm("case-lambda", compileCaseLambda))
    self.define(SpecialForm("case-λ", compileCaseLambda))
    self.define(SpecialForm("case-lambda/tag", compileCaseLambdaTag))
    self.define(SpecialForm("thunk", compileThunk))
    self.define(SpecialForm("thunk*", compileThunkStar))
    
    // Loading primitives
    self.loader = Procedure("<loader>", compileAndEvalFirst)
    self.define(Procedure("load", load))
    
    // Definition primitives
    self.defineSpecial = SpecialForm("define", compileDefine)
    self.defineValuesSpecial = SpecialForm("define-values", compileDefineValues)
    self.define(self.defineSpecial)
    self.define(self.defineValuesSpecial)
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
    
    // Procedure primitives
    self.define(Procedure("thunk?", isThunk))
    self.define(Procedure("procedure?", isProcedure))
    self.define(Procedure("procedure/tag?", isProcedureTag))
    self.define(Procedure("procedure-of-arity?", isProcedureOfArity))
    self.define(Procedure("procedure-name", procedureName))
    self.define(Procedure("procedure-tag", procedureTag))
    self.define(Procedure("procedure-arity", procedureArity))
    self.define(Procedure("procedure-arity-range", procedureArityRange))
    self.define(Procedure("procedure-arity-includes?", isProcedureArityIncludes))
    self.define(Procedure("arity-at-least?", isArityAtLeast))
    self.define(Procedure("arity-at-least-value", arityAtLeastValue))
    
    // Symbol primitives
    self.define(Procedure("symbol?", isSymbol))
    self.define(Procedure("symbol-interned?", isSymbolInterned))
    self.define(Procedure("gensym", gensym))
    self.define(Procedure("symbol=?", stringEquals))
    self.define(Procedure("string->symbol", stringToSymbol))
    self.define(Procedure("string->uninterned-symbol", stringToUninternedSymbol))
    self.define(Procedure("symbol->string", symbolToString))
    
    // Boolean primitives
    self.define(Procedure("boolean?", isBoolean))
    self.define(Procedure("boolean=?", isBooleanEq))
    self.define(SpecialForm("and", compileAnd))
    self.define(SpecialForm("or", compileOr))
    self.define(Procedure("not", not, compileNot))
    self.define(Procedure("opt", opt))
    
    // Conditional & inclusion compilation
    self.define(SpecialForm("cond-expand", compileCondExpand))
    self.define(SpecialForm("include", include))
    self.define(SpecialForm("include-ci", includeCi))
    
    // Multiple values
    self.define(Procedure("values", values, compileValues))
    self.define(Procedure("apply-with-values", applyWithValues))
    self.define("call-with-values", via:
      "(define (call-with-values producer consumer) (apply-with-values consumer (producer)))")
    
    // Optional arguments
    self.define(SpecialForm("opt-lambda", compileOptLambda))
    self.define(SpecialForm("opt*-lambda", compileOptStarLambda))
    self.define("define-optionals", via:
      "(define-syntax define-optionals\n" +
      "  (syntax-rules ()\n" +
      "    ((_ (name . formals) body1 ... body2)\n" +
      "      (define name (opt-lambda formals body1 ... body2)))))")
    self.define("define-optionals*", via:
      "(define-syntax define-optionals*\n" +
      "  (syntax-rules ()\n" +
      "    ((_ (name . formals) body1 ... body2)\n" +
      "      (define name (opt*-lambda formals body1 ... body2)))))")
    
    // Environments
    self.define(Procedure("environment?", isEnvironment))
    self.define(Procedure("interaction-environment?", isInteractionEnvironment))
    self.define(Procedure("custom-environment?", isCustomEnvironment))
    self.define(SpecialForm("the-environment", compileTheEnvironment))
    self.define(Procedure("environment", environment))
    self.define(Procedure("environment-bound-names", environmentBoundNames))
    self.define(Procedure("environment-bindings", environmentBindings))
    self.define(Procedure("environment-bound?", environmentBound))
    self.define(Procedure("environment-lookup", environmentLookup))
    self.define(Procedure("environment-assignable?", environmentAssignable))
    self.define(Procedure("environment-assign!", environmentAssign))
    self.define(Procedure("environment-definable?", environmentDefinable))
    self.define(Procedure("environment-define", environmentDefine))
    self.define(Procedure("environment-define-syntax", environmentDefineSyntax))
    self.define(Procedure("environment-import", environmentImport))
    self.define(Procedure("environment-documentation", environmentDocumentation))
    self.define(Procedure("environment-assign-documentation!", environmentAssignDocumentation))
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
  
  public override func release() {
    super.release()
    self.loader = nil
    self.defineSpecial = nil
    self.defineValuesSpecial = nil
  }
  
  //-------- MARK: - Basic primitives
  
  static func identity(expr: Expr) -> Expr {
    return expr
  }
  
  private func eval(args: Arguments) throws -> Code {
    guard args.count > 0 else {
      throw RuntimeError.argumentCount(of: "eval", num: 1, args: .makeList(args))
    }
    guard args.count < 3 else {
      throw RuntimeError.argumentCount(of: "eval", num: 2, args: .makeList(args))
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
      throw RuntimeError.argumentCount(of: "eval", min: 1, max: 2, expr: e)
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
        throw RuntimeError.argumentCount(of: "eval", min: 1, max: 2, args: .pair(expr, rest))
    }
    compiler.emit(.compile)
    return compiler.call(0, inTailPos: tail)
  }
  
  private func apply(args: Arguments) throws -> (Procedure, Exprs) {
    guard args.count > 1 else {
      throw RuntimeError.argumentCount(of: "apply", min: 2, args: .makeList(args))
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
      throw RuntimeError.argumentCount(of: "apply", min: 2, expr: expr)
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
    throw RuntimeError.argumentCount(of: "apply", min: 2, expr: expr)
  }
  
  private func isEqual(this: Expr, that: Expr) -> Expr {
    return .makeBoolean(equalExpr(that, this))
  }
  
  private func compileEqual(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let this, .pair(let that, .null))) = expr else {
      throw RuntimeError.argumentCount(of: "equal?", num: 2, expr: expr)
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
      throw RuntimeError.argumentCount(of: "eqv?", num: 2, expr: expr)
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
      throw RuntimeError.argumentCount(of: "eq?", num: 2, expr: expr)
    }
    _ = try compiler.compile(this, in: env, inTailPos: false)
    _ = try compiler.compile(that, in: env, inTailPos: false)
    compiler.emit(.eq)
    return false
  }
  
  private func compileQuote(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let arg, .null)) = expr else {
      throw RuntimeError.argumentCount(of: "quote", num: 1, expr: expr)
    }
    try compiler.pushValue(arg)
    return false
  }
  
  private func compileQuasiquote(compiler: Compiler,
                                 expr: Expr,
                                 env: Env,
                                 tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let arg, .null)) = expr else {
      throw RuntimeError.argumentCount(of: "quasiquote", num: 1, expr: expr)
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
              case .symbol(let s) where s.root == self.context.symbols.unquote:
                guard cddr == .null else {
                  throw RuntimeError.eval(.invalidContextInQuasiquote,
                                          .symbol(self.context.symbols.unquote),
                                          expr)
                }
                try compiler.compile(elem, in: env, inTailPos: false)
                nelem += 1
                continue
              case .symbol(let s) where s.root == self.context.symbols.unquoteSplicing:
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
      case .symbol(let s) where s.root == self.context.symbols.unquote:
        guard case .pair(let cadr, .null) = cdr else {
          throw RuntimeError.eval(.invalidContextInQuasiquote,
                                  .symbol(self.context.symbols.unquote),
                                  car)
        }
        return cadr
      case .symbol(let s) where s.root == self.context.symbols.quasiquote:
        guard case .pair(let cadr, .null) = cdr else {
          throw RuntimeError.eval(.invalidContextInQuasiquote,
                                  .symbol(self.context.symbols.quasiquote),
                                  car)
        }
        return try reduceQQ(reduceQQ(cadr, in: env), in: env)
      case .symbol(let s) where s.root == self.context.symbols.unquoteSplicing:
        throw RuntimeError.eval(.invalidContextInQuasiquote,
                                .symbol(self.context.symbols.unquoteSplicing),
                                car)
      case .pair(.symbol(let s), let cdar) where s.root == self.context.symbols.unquoteSplicing:
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
      throw RuntimeError.argumentCount(of: "lambda", min: 1, expr: expr)
    }
    try compiler.compileLambda(nil, arglist, body, env)
    return false
  }
  
  private func compileOptLambda(compiler: Compiler,
                                expr: Expr,
                                env: Env,
                                tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let arglist, let body)) = expr else {
      throw RuntimeError.argumentCount(of: "opt-lambda", min: 1, expr: expr)
    }
    try compiler.compileLambda(nil, arglist, body, env, optionals: true, atomic: true)
    return false
  }
  
  private func compileOptStarLambda(compiler: Compiler,
                                    expr: Expr,
                                    env: Env,
                                    tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let arglist, let body)) = expr else {
      throw RuntimeError.argumentCount(of: "opt-lambda", min: 1, expr: expr)
    }
    try compiler.compileLambda(nil, arglist, body, env, optionals: true, atomic: false)
    return false
  }
  
  private func compileLambdaTag(compiler: Compiler,
                                expr: Expr,
                                env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let tag, .pair(let arglist, let body))) = expr else {
      throw RuntimeError.argumentCount(of: "lambda/tag", min: 2, expr: expr)
    }
    try compiler.compile(tag, in: env, inTailPos: false)
    try compiler.compileLambda(nil, arglist, body, env, tagged: true)
    return false
  }
  
  private func compileThunk(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, let body) = expr else {
      throw RuntimeError.argumentCount(of: "thunk", min: 0, expr: expr)
    }
    try compiler.compileLambda(nil, .null, body, env)
    return false
  }
  
  private func compileThunkStar(compiler: Compiler,
                                expr: Expr,
                                env: Env,
                                tail: Bool) throws -> Bool {
    guard case .pair(_, let body) = expr else {
      throw RuntimeError.argumentCount(of: "thunk*", min: 0, expr: expr)
    }
    try compiler.compileLambda(nil, .symbol(Symbol(uninterned: "_")), body, env)
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
  
  private func compileCaseLambdaTag(compiler: Compiler,
                                    expr: Expr,
                                    env: Env,
                                    tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let tag, let cases)) = expr else {
      preconditionFailure("broken case-lambda invocation")
    }
    try compiler.compile(tag, in: env, inTailPos: false)
    try compiler.compileCaseLambda(nil, cases, env, tagged: true)
    return false
  }
  
  //-------- MARK: - Loading procedures
  
  private func load(args: Arguments) throws -> (Procedure, Exprs) {
    guard args.count == 1 || args.count == 2  else {
      throw RuntimeError.argumentCount(of: "load", min: 1, max: 2, args: .makeList(args))
    }
    // Extract arguments
    let path = try args.first!.asPath()
    let filename =
      self.context.fileHandler.filePath(
        forFile: path, relativeTo: self.context.machine.currentDirectoryPath) ??
      self.context.fileHandler.libraryFilePath(
        forFile: path, relativeTo: self.context.machine.currentDirectoryPath) ??
      self.context.fileHandler.path(path, relativeTo: self.context.machine.currentDirectoryPath)
    var environment = self.context.environment
    if args.count == 2 {
      environment = try args[args.startIndex + 1].asEnvironment()
    }
    // Load file and parse expressions
    let exprs = try self.context.machine.parse(file: filename)
    let sourceDir = self.context.fileHandler.directory(filename)
    // Hand over work to `compileAndEvalFirst`
    return (self.loader, [exprs, .makeString(sourceDir), .env(environment!)])
  }

  private func compileAndEvalFirst(args: Arguments) throws -> (Procedure, Exprs) {
    guard args.count == 3 else {
      throw RuntimeError.argumentCount(min: 3, max: 3, args: .makeList(args))
    }
    let sourceDirExpr = args[args.startIndex + 1]
    let sourceDir: String?
    switch sourceDirExpr {
      case .string(let str):
        sourceDir = str as String
      default:
        sourceDir = nil
    }
    let env = args[args.startIndex + 2]
    switch args.first! {
      case .null:
        return (CoreLibrary.voidProc, [])
      case .pair(let expr, .null):
        let code = try Compiler.compile(expr: .pair(expr, .null),
                                        in: .global(try env.asEnvironment()),
                                        optimize: true,
                                        inDirectory: sourceDir)
        return (Procedure("<loader>", code), [])
      case .pair(let expr, let rest):
        let source = Expr.pair(expr,
                               .pair(.makeList(.procedure(self.loader), rest, sourceDirExpr, env),
                                     .null))
        let code = try Compiler.compile(expr: source,
                                        in: .global(try env.asEnvironment()),
                                        optimize: true,
                                        inDirectory: sourceDir)
        return (Procedure("<loader>", code), [])
      default:
        throw RuntimeError.type(args.first!, expected: [.properListType])
    }
  }
  
  //-------- MARK: - Definition primitives
  
  private func compileDefine(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    // Extract signature and definition
    guard case .pair(_, .pair(let sig, let def)) = expr else {
      throw RuntimeError.argumentCount(of: "define", num: 2, expr: expr)
    }
    // Check that define is not executed in a local environment
    if case .local(_) = env {
      throw RuntimeError.eval(.defineInLocalEnv, sig, def)
    }
    // Compile definition and store result in global environment
    switch sig {
      case .symbol(let sym):
        var doc: String? = nil
        let index = compiler.registerConstant(sig)
        switch def {
          case .pair(let value, .null):
            try compiler.compile(value, in: env, inTailPos: false)
          case .pair(let value, .pair(.string(let str), .null)):
            doc = str as String
            try compiler.compile(value, in: env, inTailPos: false)
          default:
            throw RuntimeError.eval(.malformedDefinition, Expr.makeList(def))
        }
        let environment = env.environment!
        if let libName = environment.libraryName, environment.isImported(sym) {
          throw RuntimeError.eval(.redefinitionOfImport, sig, libName)
        }
        let loc = environment.forceDefinedLocationRef(for: sym).location!
        compiler.patchMakeClosure(index)
        compiler.emit(.defineGlobal(loc))
        compiler.emit(.pushConstant(index))
        if let documentation = doc {
          self.context.heap.documentation[loc] = documentation
        }
        return false
      case .pair(.symbol(let sym), let arglist):
        var doc: String? = nil
        let index = compiler.registerConstant(.symbol(sym))
        switch def {
          case .pair(.string(let str), .pair(let car, let cdr)):
            doc = str as String
            try compiler.compileLambda(index, arglist, .pair(car, cdr), env)
          default:
            try compiler.compileLambda(index, arglist, def, env)
        }
        let environment = env.environment!
        if let libName = environment.libraryName, environment.isImported(sym) {
          throw RuntimeError.eval(.redefinitionOfImport, .symbol(sym), libName)
        }
        let loc = environment.forceDefinedLocationRef(for: sym).location!
        compiler.emit(.defineGlobal(loc))
        compiler.emit(.pushConstant(index))
        if let documentation = doc {
          self.context.heap.documentation[loc] = documentation
        }
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
      throw RuntimeError.argumentCount(of: "define", num: 2, expr: expr)
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
        var docs = [String?]()
        while case .pair(.symbol(let sym), let rest) = vars {
          if syms.contains(sym) {
            throw RuntimeError.eval(.duplicateBinding, .symbol(sym), sig)
          } else if let libName = environment.libraryName, environment.isImported(sym) {
            throw RuntimeError.eval(.redefinitionOfImport, .symbol(sym), libName)
          }
          syms.append(sym)
          if case .pair(.string(let str), let rest2) = rest {
            docs.append(str as String)
            vars = rest2
          } else {
            docs.append(nil)
            vars = rest
          }
        }
        switch vars {
          case .null:
            compiler.emit(.unpack(syms.count, false))
          case .symbol(let sym):
            compiler.emit(.unpack(syms.count, true))
            syms.append(sym)
            docs.append(nil)
          default:
            throw RuntimeError.eval(.malformedDefinition, .pair(sig, Expr.makeList(value)))
        }
        var res: Expr = .null
        for (i, sym) in syms.enumerated().reversed() {
          let loc = environment.forceDefinedLocationRef(for: sym).location!
          compiler.emit(.defineGlobal(loc))
          if let documentation = docs[i] {
            self.context.heap.documentation[loc] = documentation
          }
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
    var doc: String? = nil
    var kword: Expr
    var transformer: Expr
    switch expr {
      case .pair(_, .pair(let kw, .pair(let tf, .null))):
        kword = kw
        transformer = tf
      case .pair(_, .pair(let kw, .pair(.string(let str), .pair(let tf, .null)))):
        doc = str as String
        kword = kw
        transformer = tf
      default:
        throw RuntimeError.argumentCount(of: "define-syntax", min: 2, max: 3, expr: expr)
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
    if let documentation = doc {
      self.context.heap.documentation[loc] = documentation
    }
    return false
  }
  
  private func compileDefineLibrary(compiler: Compiler,
                                    expr: Expr,
                                    env: Env,
                                    tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let name, let decls)) = expr else {
      throw RuntimeError.argumentCount(of: "define-library", num: 1, expr: expr)
    }
    try compiler.context.libraries.register(name: name,
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
        ellipsis = sym.root
        lit = literals
        transRules = patTrans
      case .pair(_, .pair(let literals, let patTrans)):
        lit = literals
        transRules = patTrans
      default:
        throw RuntimeError.argumentCount(of: "syntax-rules", num: 1, expr: expr)
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
      literalSet.insert(sym.root)
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
      throw RuntimeError.argumentCount(of: "set!", num: 2, expr: expr)
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
      throw RuntimeError.argumentCount(of: "delay-force", num: 1, expr: expr)
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
      throw RuntimeError.argumentCount(of: "delay", num: 1, expr: expr)
    }
    let body = Expr.pair(.makeList(.symbol(compiler.context.symbols.makePromise), delayed), .null)
    try compiler.compileLambda(nil, .null, body, env)
    compiler.emit(.makePromise)
    return false
  }
  
  private func compileForce(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let promise, .null)) = expr else {
      throw RuntimeError.argumentCount(of: "force", num: 1, expr: expr)
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
      throw RuntimeError.argumentCount(of: "stream-delay-force", num: 1, expr: expr)
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
      throw RuntimeError.argumentCount(of: "stream-delay", num: 1, expr: expr)
    }
    let body = Expr.pair(.makeList(.symbol(compiler.context.symbols.makeStream), delayed), .null)
    try compiler.compileLambda(nil, .null, body, env)
    compiler.emit(.makeStream)
    return false
  }
  
  //-------- MARK: - Procedure primitives
  
  private func isThunk(expr: Expr) -> Expr {
    guard case .procedure(let proc) = expr else {
      return .false
    }
    return .makeBoolean(proc.arityAccepted(0))
  }
  
  private func isProcedure(expr: Expr) -> Expr {
    if case .procedure(_) = expr {
      return .true
    }
    return .false
  }
  
  private func isProcedureTag(expr: Expr) throws -> Expr {
    guard case .procedure(let proc) = expr,
          case .closure(_, let tag, _, _) = proc.kind,
          tag != .undef else {
      return .false
    }
    return .true
  }
  
  private func isProcedureOfArity(expr: Expr, arity: Expr) throws -> Expr {
    guard case .procedure(let proc) = expr else {
      return .false
    }
    return .makeBoolean(proc.arityAccepted(try arity.asInt()))
  }
  
  private func procedureName(expr: Expr) throws -> Expr {
    guard let name = (try expr.asProcedure()).originalName else {
      return .false
    }
    return .makeString(name)
  }
  
  private func procedureTag(expr: Expr) throws -> Expr {
    guard case .procedure(let proc) = expr,
          case .closure(_, let tag, _, _) = proc.kind,
          tag != .undef else {
      throw RuntimeError.eval(.procedureWithoutTag)
    }
    return tag
  }
  
  private func procedureArity(expr: Expr) throws -> Expr {
    let arities = (try expr.asProcedure()).arity
    var res = Expr.null
    for arity in arities {
      switch arity {
        case .exact(let n):
          res = .pair(.fixnum(Int64(n)), res)
        case .atLeast(let n):
          res = .pair(.fixnum(Int64(-n-1)), res)
      }
    }
    switch res {
      case .null:
        return .false
      case .pair(let a, .null):
        return a
      default:
        return res
    }
  }
  
  private func procedureArityRange(expr: Expr) throws -> Expr {
    let arities = (try expr.asProcedure()).arity
    var min = Int.max
    var max: Int? = 0
    for arity in arities {
      switch arity {
        case .exact(let n):
          if n < min {
            min = n
          }
          if let m = max, n > m {
            max = n
          }
        case .atLeast(let n):
          if n < min {
            min = n
          }
          max = nil
      }
    }
    return .pair(.makeNumber(min), max == nil ? .false : .makeNumber(max!))
  }
  
  private func isProcedureArityIncludes(expr: Expr, arity: Expr) throws -> Expr {
    return .makeBoolean((try expr.asProcedure()).arityAccepted(try arity.asInt()))
  }
  
  private func isArityAtLeast(expr: Expr) throws -> Expr {
    guard case .fixnum(let n) = expr else {
      return .false
    }
    return .makeBoolean(n < 0)
  }
  
  private func arityAtLeastValue(expr: Expr) throws -> Expr {
    guard case .fixnum(let n) = expr, n < 0 else {
      return .false
    }
    return .fixnum(Int64(-n-1))
  }
  
  //-------- MARK: - Symbol primitives
  
  private func isSymbol(expr: Expr) -> Expr {
    if case .symbol(_) = expr {
      return .true
    }
    return .false
  }

  private func isSymbolInterned(expr: Expr) -> Expr {
    if case .symbol(let sym) = expr {
      return .makeBoolean(sym.isInterned)
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

  private func stringToUninternedSymbol(expr: Expr) throws -> Expr {
    return .symbol(Symbol(uninterned: try expr.asString()))
  }
  
  private func symbolToString(expr: Expr) throws -> Expr {
    return .makeString(try expr.asSymbol().identifier)
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
      throw RuntimeError.argumentCount(of: "not", num: 1, expr: expr)
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

  private func opt(_ args: Arguments) throws -> (Procedure, Exprs) {
    guard args.count >= 2 && args.count <= 3 else {
      throw RuntimeError.argumentCount(of: "opt", min: 2, max: 3, args: .makeList(args))
    }
    guard case .procedure(let proc) = args.first! else {
      throw RuntimeError.type(args.first!, expected: [.procedureType])
    }
    let arg = args[args.startIndex + 1]
    if arg.isFalse {
      guard args.count == 3 else {
        return (CoreLibrary.idProc, [.true])
      }
      return (CoreLibrary.idProc, [args[args.startIndex + 2]])
    } else {
      return (proc, [arg])
    }
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
    return args.values
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
  
  private func isInteractionEnvironment(expr: Expr) -> Expr {
    switch expr {
      case .env(let env):
        return .makeBoolean(env.kind == .repl)
      default:
        return .false
    }
  }
  
  private func isCustomEnvironment(expr: Expr) -> Expr {
    switch expr {
      case .env(let env):
        return .makeBoolean(env.kind == .custom)
      default:
        return .false
    }
  }
  
  private func compileTheEnvironment(compiler: Compiler,
                                     expr: Expr,
                                     env: Env,
                                     tail: Bool) throws -> Bool {
    guard case .pair(_, .null) = expr else {
      throw RuntimeError.argumentCount(of: "the-environment", num: 0, expr: expr)
    }
    if let environment = env.environment {
      try compiler.pushValue(.env(environment))
    } else {
      try compiler.pushValue(.false)
    }
    return false
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
        switch value {
          case .undef, .uninit(_):
            res = .pair(.pair(.symbol(sym), .null), res)
          default:
            res = .pair(.pair(.symbol(sym), .pair(value, .null)), res)
        }
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
  
  private func environmentDocumentation(expr: Expr, symbol: Expr) throws -> Expr {
    guard case .env(let environment) = expr else {
      throw RuntimeError.type(expr, expected: [.envType])
    }
    guard let str = environment.documentation(try symbol.asSymbol()) else {
      return .false
    }
    return .makeString(str)
  }
  
  private func environmentAssignDocumentation(expr: Expr, symbol: Expr, doc: Expr) throws -> Expr {
    guard case .env(let environment) = expr else {
      throw RuntimeError.type(expr, expected: [.envType])
    }
    return .makeBoolean(environment.assignDoc(of: try symbol.asSymbol(), to: try doc.asString()))
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
    return .makeBoolean(environment.set(sym, to: value))
  }
  
  private func environmentDefinable(expr: Expr, symbol: Expr) throws -> Expr {
    guard case .env(let environment) = expr else {
      throw RuntimeError.type(expr, expected: [.envType])
    }
    let sym = try symbol.asSymbol()
    if case .library(_) = environment.kind {
      return .false
    } else if environment.kind != .custom &&
              environment.kind != .repl &&
              !environment.isUndefined(sym) {
      return .false
    }
    return .true
  }
  
  private func environmentDefine(expr: Expr, symbol: Expr, value: Expr) throws -> Expr {
    guard case .env(let environment) = expr else {
      throw RuntimeError.type(expr, expected: [.envType])
    }
    let sym = try symbol.asSymbol()
    if case .library(let lib) = environment.kind {
      throw RuntimeError.eval(.cannotModifyLibraryEnv, lib)
    } else if environment.kind != .custom &&
              environment.kind != .repl &&
              !environment.isUndefined(sym) {
      throw RuntimeError.eval(.erroneousRedefinition, symbol, value)
    }
    return .makeBoolean(environment.define(sym, as: value))
  }
  
  private func environmentDefineSyntax(expr: Expr, symbol: Expr, transformer: Expr) throws -> Expr {
    guard case .env(let environment) = expr else {
      throw RuntimeError.type(expr, expected: [.envType])
    }
    let sym = try symbol.asSymbol()
    if case .library(let lib) = environment.kind {
      throw RuntimeError.eval(.cannotModifyLibraryEnv, lib)
    } else if environment.kind != .custom &&
              environment.kind != .repl &&
              !environment.isUndefined(sym) {
      throw RuntimeError.eval(.erroneousRedefinition, symbol, transformer)
    }
    guard case .procedure(let proc) = transformer else {
      throw RuntimeError.eval(.malformedTransformer, transformer)
    }
    return .makeBoolean(environment.define(sym, as: .special(SpecialForm(sym.identifier, proc))))
  }
  
  private func environmentImport(expr: Expr, args: Arguments) throws -> Expr {
    guard case .env(let environment) = expr else {
      throw RuntimeError.type(expr, expected: [.envType])
    }
    if case .library(let lib) = environment.kind {
      throw RuntimeError.eval(.cannotModifyLibraryEnv, lib)
    }
    for arg in args {
      guard let importSet = ImportSet(arg, in: self.context) else {
        throw RuntimeError.eval(.malformedImportSet, arg)
      }
      try environment.import(importSet)
    }
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
