//
//  ControlFlowLibrary.swift
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


public final class ControlFlowLibrary: NativeLibrary {

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "control"]
  }

  /// Declarations of the library.
  public override func declarations() {
    self.define(SpecialForm("begin", self.compileBegin))
    self.define(SpecialForm("let", self.compileLet))
    self.define(SpecialForm("let*", self.compileLetStar))
    self.define(SpecialForm("letrec", self.compileLetRec))
    self.define(SpecialForm("letrec*", self.compileLetRecStar))
    self.define(SpecialForm("let-values", self.compileLetValues))
    self.define(SpecialForm("let*-values", self.compileLetStarValues))
    self.define(SpecialForm("let-optionals", self.compileLetOptionals))
    self.define(SpecialForm("let*-optionals", self.compileLetStarOptionals))
    self.define(SpecialForm("let-keywords", self.compileLetKeywords))
    self.define(SpecialForm("let*-keywords", self.compileLetStarKeywords))
    self.define(SpecialForm("let-syntax", self.compileLetSyntax))
    self.define(SpecialForm("letrec-syntax", self.compileLetRecSyntax))
    self.define(SpecialForm("do", self.compileDo))
    self.define(SpecialForm("if", self.compileIf))
    self.define(SpecialForm("when", self.compileWhen))
    self.define(SpecialForm("unless", self.compileUnless))
    self.define(SpecialForm("cond", self.compileCond))
    self.define(SpecialForm("case", self.compileCase))
  }

  private func splitBindings(_ bindingList: Expr) throws -> (Expr, Expr) {
    var symbols = Exprs()
    var exprs = Exprs()
    var bindings = bindingList
    while case .pair(let binding, let rest) = bindings {
      guard case .pair(.symbol(let sym), .pair(let expr, .null)) = binding else {
        throw RuntimeError.eval(.malformedBinding, binding, bindingList)
      }
      symbols.append(.symbol(sym))
      exprs.append(expr)
      bindings = rest
    }
    guard bindings.isNull else {
      throw RuntimeError.eval(.malformedBindings, bindingList)
    }
    return (Expr.makeList(symbols), Expr.makeList(exprs))
  }

  private func compileBegin(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, let exprs) = expr else {
      preconditionFailure("malformed begin")
    }
    return try compiler.compileSeq(exprs,
                                   in: env,
                                   inTailPos: tail,
                                   localDefine: false)
  }

  private func compileLet(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let first, let body)) = expr else {
      throw RuntimeError.argumentCount(of: "let", min: 1, expr: expr)
    }
    let initialLocals = compiler.numLocals
    var res = false
    switch first {
      case .null:
        return try compiler.compileSeq(body, in: env, inTailPos: tail)
      case .pair(_, _):
        let group = try compiler.compileBindings(first,
                                                 in: env,
                                                 atomic: true,
                                                 predef: false)
        res = try compiler.compileSeq(body,
                                      in: Env(group),
                                      inTailPos: tail)
        group.finalize()
      case .symbol(let sym):
        guard case .pair(let bindings, let rest) = body else {
          throw RuntimeError.argumentCount(of: "let", min: 2, expr: expr)
        }
        let (params, exprs) = try splitBindings(bindings)
        let group = BindingGroup(owner: compiler, parent: env)
        let index = group.allocBindingFor(sym).index
        compiler.emit(.pushUndef)
        compiler.emit(.makeLocalVariable(index))
        let nameIdx = compiler.registerConstant(first)
        try compiler.compileLambda(nameIdx,
                                   params,
                                   rest,
                                   Env(group))
        compiler.emit(.setLocalValue(index))
        res = try compiler.compile(.pair(first, exprs),
                                    in: Env(group),
                                    inTailPos: tail)
      default:
        throw RuntimeError.type(first, expected: [.listType, .symbolType])
    }
    if !res && compiler.numLocals > initialLocals {
      compiler.emit(.reset(initialLocals, compiler.numLocals - initialLocals))
    }
    compiler.numLocals = initialLocals
    return res
  }

  private func compileLetStar(_ compiler: Compiler,
                              expr: Expr,
                              env: Env,
                              tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let first, let body)) = expr else {
      throw RuntimeError.argumentCount(of: "let*", min: 1, expr: expr)
    }
    let initialLocals = compiler.numLocals
    switch first {
      case .null:
        return try compiler.compileSeq(body, in: env, inTailPos: tail)
      case .pair(_, _):
        let group = try compiler.compileBindings(first,
                                                 in: env,
                                                 atomic: false,
                                                 predef: false)
        let res = try compiler.compileSeq(body,
                                          in: Env(group),
                                          inTailPos: tail)
        return compiler.finalizeBindings(group, exit: res, initialLocals: initialLocals)
      default:
        throw RuntimeError.type(first, expected: [.listType])
    }
  }

  private func compileLetRec(_ compiler: Compiler,
                             expr: Expr,
                             env: Env,
                             tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let first, let body)) = expr else {
      throw RuntimeError.argumentCount(of: "letrec", min: 1, expr: expr)
    }
    let initialLocals = compiler.numLocals
    switch first {
    case .null:
      return try compiler.compileSeq(body,
                                     in: env,
                                     inTailPos: tail)
    case .pair(_, _):
      let group = try compiler.compileBindings(first,
                                               in: env,
                                               atomic: true,
                                               predef: true,
                                               postset: true)
      let res = try compiler.compileSeq(body,
                                        in: Env(group),
                                        inTailPos: tail)
      return compiler.finalizeBindings(group, exit: res, initialLocals: initialLocals)
    default:
      throw RuntimeError.type(first, expected: [.listType])
    }
  }

  private func compileLetRecStar(_ compiler: Compiler,
                                 expr: Expr,
                                 env: Env,
                                 tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let first, let body)) = expr else {
      throw RuntimeError.argumentCount(of: "letrec*", min: 1, expr: expr)
    }
    let initialLocals = compiler.numLocals
    switch first {
      case .null:
        return try compiler.compileSeq(body,
                                       in: env,
                                       inTailPos: tail)
      case .pair(_, _):
        let group = try compiler.compileBindings(first,
                                                 in: env,
                                                 atomic: true,
                                                 predef: true)
        let res = try compiler.compileSeq(body,
                                          in: Env(group),
                                          inTailPos: tail)
        return compiler.finalizeBindings(group, exit: res, initialLocals: initialLocals)
      default:
        throw RuntimeError.type(first, expected: [.listType])
    }
  }

  private func compileLetValues(_ compiler: Compiler,
                                expr: Expr,
                                env: Env,
                                tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let first, let body)) = expr else {
      throw RuntimeError.argumentCount(of: "let-values", min: 1, expr: expr)
    }
    let initialLocals = compiler.numLocals
    switch first {
      case .null:
        return try compiler.compileSeq(body,
                                       in: env,
                                       inTailPos: tail)
      case .pair(_, _):
        let group = try compiler.compileMultiBindings(first,
                                                      in: env,
                                                      atomic: true)
        let res = try compiler.compileSeq(body,
                                          in: Env(group),
                                          inTailPos: tail)
        return compiler.finalizeBindings(group, exit: res, initialLocals: initialLocals)
      default:
        throw RuntimeError.type(first, expected: [.listType])
    }
  }

  private func compileLetStarValues(_ compiler: Compiler,
                                    expr: Expr,
                                    env: Env,
                                    tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let first, let body)) = expr else {
      throw RuntimeError.argumentCount(of: "let*-values", min: 1, expr: expr)
    }
    let initialLocals = compiler.numLocals
    switch first {
      case .null:
        return try compiler.compileSeq(body,
                                       in: env,
                                       inTailPos: tail)
      case .pair(_, _):
        let group = try compiler.compileMultiBindings(first,
                                                      in: env,
                                                      atomic: false)
        let res = try compiler.compileSeq(body,
                                          in: Env(group),
                                          inTailPos: tail)
        return compiler.finalizeBindings(group, exit: res, initialLocals: initialLocals)
      default:
        throw RuntimeError.type(first, expected: [.listType])
    }
  }

  private func compileLetOptionals(_ compiler: Compiler,
                                   expr: Expr,
                                   env: Env,
                                   tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let optlist, .pair(let first, let body))) = expr else {
      throw RuntimeError.argumentCount(of: "let-optionals", min: 2, expr: expr)
    }
    let initialLocals = compiler.numLocals
    switch first {
      case .null:
        return try compiler.compileSeq(.pair(optlist, body),
                                       in: env,
                                       inTailPos: tail)
      case .pair(_, _):
        try compiler.compile(optlist, in: env, inTailPos: false)
        let group = try self.compileOptionalBindings(compiler, first, in: env, atomic: true)
        compiler.emit(.pop)
        let res = try compiler.compileSeq(body,
                                          in: Env(group),
                                          inTailPos: tail)
        return compiler.finalizeBindings(group, exit: res, initialLocals: initialLocals)
      default:
        throw RuntimeError.type(first, expected: [.listType])
    }
  }

  private func compileLetStarOptionals(_ compiler: Compiler,
                                       expr: Expr,
                                       env: Env,
                                       tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let optlist, .pair(let first, let body))) = expr else {
      throw RuntimeError.argumentCount(of: "let*-optionals", min: 2, expr: expr)
    }
    let initialLocals = compiler.numLocals
    switch first {
      case .null:
        return try compiler.compileSeq(.pair(optlist, body),
                                       in: env,
                                       inTailPos: tail)
      case .pair(_, _):
        try compiler.compile(optlist, in: env, inTailPos: false)
        let group = try self.compileOptionalBindings(compiler, first, in: env, atomic: false)
        compiler.emit(.pop)
        let res = try compiler.compileSeq(body,
                                          in: Env(group),
                                          inTailPos: tail)
        return compiler.finalizeBindings(group, exit: res, initialLocals: initialLocals)
      default:
        throw RuntimeError.type(first, expected: [.listType])
    }
  }

  private func compileOptionalBindings(_ compiler: Compiler,
                                      _ bindingList: Expr,
                                      in lenv: Env,
                                      atomic: Bool) throws -> BindingGroup {
    let group = BindingGroup(owner: compiler, parent: lenv)
    let env = atomic ? lenv : .local(group)
    var bindings = bindingList
    var prevIndex = -1
    while case .pair(let binding, let rest) = bindings {
      guard case .pair(.symbol(let sym), .pair(let expr, .null)) = binding else {
        throw RuntimeError.eval(.malformedBinding, binding, bindingList)
      }
      compiler.emit(.dup)
      compiler.emit(.isNull)
      let branchIfIp = compiler.emitPlaceholder()
      compiler.emit(.decons)
      let branchIp = compiler.emitPlaceholder()
      compiler.patch(.branchIf(compiler.offsetToNext(branchIfIp)), at: branchIfIp)
      try compiler.compile(expr, in: env, inTailPos: false)
      compiler.patch(.branch(compiler.offsetToNext(branchIp)), at: branchIp)
      let binding = group.allocBindingFor(sym)
      guard binding.index > prevIndex else {
        throw RuntimeError.eval(.duplicateBinding, .symbol(sym), bindingList)
      }
      if binding.isValue {
        compiler.emit(.setLocal(binding.index))
      } else {
        compiler.emit(.makeLocalVariable(binding.index))
      }
      prevIndex = binding.index
      bindings = rest
    }
    guard bindings.isNull else {
      throw RuntimeError.eval(.malformedBindings, bindingList)
    }
    return group
  }

  private func compileLetKeywords(_ compiler: Compiler,
                                  expr: Expr,
                                  env: Env,
                                  tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let optlist, .pair(let first, let body))) = expr else {
      throw RuntimeError.argumentCount(of: "let-keywords", min: 2, expr: expr)
    }
    let initialLocals = compiler.numLocals
    switch first {
      case .null:
        return try compiler.compileSeq(.pair(optlist, body),
                                       in: env,
                                       inTailPos: tail)
      case .pair(_, _):
        try compiler.compile(optlist, in: env, inTailPos: false)
        let group = try self.compileKeywordBindings(compiler, first, in: env, atomic: true)
        compiler.emit(.pop)
        let res = try compiler.compileSeq(body,
                                          in: Env(group),
                                          inTailPos: tail)
        return compiler.finalizeBindings(group, exit: res, initialLocals: initialLocals)
      default:
        throw RuntimeError.type(first, expected: [.listType])
    }
  }

  private func compileLetStarKeywords(_ compiler: Compiler,
                                      expr: Expr,
                                      env: Env,
                                      tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let optlist, .pair(let first, let body))) = expr else {
      throw RuntimeError.argumentCount(of: "let*-keywords", min: 2, expr: expr)
    }
    let initialLocals = compiler.numLocals
    switch first {
      case .null:
        return try compiler.compileSeq(.pair(optlist, body),
                                       in: env,
                                       inTailPos: tail)
      case .pair(_, _):
        try compiler.compile(optlist, in: env, inTailPos: false)
        let group = try self.compileKeywordBindings(compiler, first, in: env, atomic: false)
        compiler.emit(.pop)
        let res = try compiler.compileSeq(body,
                                          in: Env(group),
                                          inTailPos: tail)
        return compiler.finalizeBindings(group, exit: res, initialLocals: initialLocals)
      default:
        throw RuntimeError.type(first, expected: [.listType])
    }
  }

  private func compileKeywordBindings(_ compiler: Compiler,
                                      _ bindingList: Expr,
                                      in lenv: Env,
                                      atomic: Bool) throws -> BindingGroup {
    let group = BindingGroup(owner: compiler, parent: lenv)
    let env = atomic ? lenv : .local(group)
    var bindings = bindingList
    var prevIndex = -1
    let initialIp = compiler.emitPlaceholder()
    let backfillIp = compiler.offsetToNext(0)
    // Backfill keyword bindings with defaults
    while case .pair(let binding, let rest) = bindings {
      guard case .pair(.symbol(let sym), .pair(let expr, .null)) = binding else {
        throw RuntimeError.eval(.malformedBinding, binding, bindingList)
      }
      compiler.emit(.pushUndef)
      let pushValueIp = compiler.emitPlaceholder()
      compiler.emit(.eq)
      let alreadySetIp = compiler.emitPlaceholder()
      try compiler.compile(expr, in: env, inTailPos: false)
      let binding = group.allocBindingFor(sym)
      guard binding.index > prevIndex else {
        throw RuntimeError.eval(.duplicateBinding, .symbol(sym), bindingList)
      }
      compiler.emit(binding.isValue ? .setLocal(binding.index) : .setLocalValue(binding.index))
      compiler.patch(binding.isValue ? .pushLocal(binding.index) : .pushLocalValue(binding.index),
                     at: pushValueIp)
      compiler.patch(.branchIfNot(compiler.offsetToNext(alreadySetIp)), at: alreadySetIp)
      prevIndex = binding.index
      bindings = rest
    }
    guard bindings.isNull else {
      throw RuntimeError.eval(.malformedBindings, bindingList)
    }
    let finalIp = compiler.emitPlaceholder()
    compiler.patch(.branch(compiler.offsetToNext(initialIp)), at: initialIp)
    // Allocate space for all the bindings
    bindings = bindingList
    while case .pair(.pair(.symbol(let sym), _), let rest) = bindings {
      let binding = group.allocBindingFor(sym)
      compiler.emit(.pushUndef)
      compiler.emit(binding.isValue ? .setLocal(binding.index) : .makeLocalVariable(binding.index))
      bindings = rest
    }
    // Process keyword list
    bindings = bindingList
    let loopIp = compiler.emit(.dup)
    compiler.emit(.isNull)
    let listEmptyIp = compiler.emitPlaceholder()
    compiler.emit(.deconsKeyword)
    while case .pair(.pair(.symbol(let sym), _), let rest) = bindings {
      let binding = group.allocBindingFor(sym)
      compiler.emit(.dup)
      compiler.pushConstant(.symbol(compiler.context.symbols.intern(sym.identifier + ":")))
      compiler.emit(.eq)
      let keywordCompIp = compiler.emitPlaceholder()
      compiler.emit(.pop)
      compiler.emit(binding.isValue ? .setLocal(binding.index) : .makeLocalVariable(binding.index))
      compiler.emit(.branch(-compiler.offsetToNext(loopIp)))
      compiler.patch(.branchIfNot(compiler.offsetToNext(keywordCompIp)), at: keywordCompIp)
      bindings = rest
    }
    compiler.emit(.raiseError(EvalError.unknownKeyword.rawValue, 2))
    compiler.patch(.branchIf(compiler.offsetToNext(listEmptyIp)), at: listEmptyIp)
    // Jumop to the default backfill
    compiler.emit(.branch(-compiler.offsetToNext(backfillIp)))
    // Patch instructions jumping to the end
    compiler.patch(.branch(compiler.offsetToNext(finalIp)), at: finalIp)
    return group
  }

  private func compileLetSyntax(_ compiler: Compiler,
                                expr: Expr,
                                env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let first, let body)) = expr else {
      throw RuntimeError.argumentCount(of: "let-syntax", min: 1, expr: expr)
    }
    switch first {
      case .null:
        return try compiler.compileSeq(body,
                                       in: env,
                                       inTailPos: tail)
      case .pair(_, _):
        let group = try compiler.compileMacros(first,
                                               in: env,
                                               recursive: false)
        return try compiler.compileSeq(body,
                                       in: Env(group),
                                       inTailPos: tail)
      default:
        throw RuntimeError.type(first, expected: [.listType])
    }
  }

  private func compileLetRecSyntax(_ compiler: Compiler,
                                   expr: Expr,
                                   env: Env,
                                   tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let first, let body)) = expr else {
      throw RuntimeError.argumentCount(of: "letrec-syntax", min: 1, expr: expr)
    }
    switch first {
      case .null:
        return try compiler.compileSeq(body,
                                      in: env,
                                      inTailPos: tail)
      case .pair(_, _):
        let group = try compiler.compileMacros(first,
                                               in: env,
                                               recursive: true)
        return try compiler.compileSeq(body,
                                       in: Env(group),
                                       inTailPos: tail)
      default:
        throw RuntimeError.type(first, expected: [.listType])
    }
  }

  private func compileDo(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    // Decompose expression into bindings, exit, and body
    guard case .pair(_, .pair(let bindingList, .pair(let exit, let body))) = expr else {
      throw RuntimeError.argumentCount(of: "do", min: 2, expr: expr)
    }
    // Extract test and terminal expressions
    guard case .pair(let test, let terminal) = exit else {
      throw RuntimeError.eval(.malformedTest, exit)
    }
    let initialLocals = compiler.numLocals
    // Setup bindings
    let group = BindingGroup(owner: compiler, parent: env)
    var bindings = bindingList
    var prevIndex = -1
    var doBindings = [Int]()
    var stepExprs = Exprs()
    // Compile initial bindings
    while case .pair(let binding, let rest) = bindings {
      guard case .pair(.symbol(let sym), .pair(let start, let optStep)) = binding else {
        throw RuntimeError.eval(.malformedBinding, binding, bindingList)
      }
      try compiler.compile(start, in: env, inTailPos: false)
      let index = group.allocBindingFor(sym).index
      guard index > prevIndex else {
        throw RuntimeError.eval(.duplicateBinding, .symbol(sym), bindingList)
      }
      compiler.emit(.makeLocalVariable(index))
      switch optStep {
        case .pair(let step, .null):
          doBindings.append(index)
          stepExprs.append(step)
        case .null:
          break;
        default:
          throw RuntimeError.eval(.malformedBinding, binding, bindingList)
      }
      prevIndex = index
      bindings = rest
    }
    guard bindings.isNull else {
      throw RuntimeError.eval(.malformedBindings, bindingList)
    }
    // Compile test expression
    let testIp = compiler.offsetToNext(0)
    try compiler.compile(test, in: Env(group), inTailPos: false)
    let exitJumpIp = compiler.emitPlaceholder()
    // Compile body
    try compiler.compileSeq(body, in: Env(group), inTailPos: false)
    compiler.emit(.pop)
    // Compile step expressions and update bindings
    for step in stepExprs {
      try compiler.compile(step, in: Env(group), inTailPos: false)
    }
    for index in doBindings.reversed() {
      compiler.emit(.setLocalValue(index))
    }
    // Loop
    compiler.emit(.branch(-compiler.offsetToNext(testIp)))
    // Exit if the test expression evaluates to true
    compiler.patch(.branchIf(compiler.offsetToNext(exitJumpIp)), at: exitJumpIp)
    // Compile terminal expressions
    let res = try compiler.compileSeq(terminal,
                                      in: Env(group),
                                      inTailPos: tail)
    // Remove bindings from stack
    if !res && compiler.numLocals > initialLocals {
      compiler.emit(.reset(initialLocals, compiler.numLocals - initialLocals))
    }
    compiler.numLocals = initialLocals
    return res
  }

  private func compileIf(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let cond, .pair(let thenp, let alternative))) = expr else {
      throw RuntimeError.argumentCount(of: "if", min: 2, expr: expr)
    }
    var elsep = Expr.void
    if case .pair(let ep, .null) = alternative {
      elsep = ep
    }
    try compiler.compile(cond, in: env, inTailPos: false)
    let elseJumpIp = compiler.emitPlaceholder()
    // Compile if in tail position
    if try compiler.compile(elsep, in: env, inTailPos: tail) {
      compiler.patch(.branchIf(compiler.offsetToNext(elseJumpIp)), at: elseJumpIp)
      return try compiler.compile(thenp, in: env, inTailPos: true)
    }
    // Compile if in non-tail position
    let exitJumpIp = compiler.emitPlaceholder()
    compiler.patch(.branchIf(compiler.offsetToNext(elseJumpIp)), at: elseJumpIp)
    if try compiler.compile(thenp, in: env, inTailPos: tail) {
      compiler.patch(.return, at: exitJumpIp)
      return true
    }
    compiler.patch(.branch(compiler.offsetToNext(exitJumpIp)), at: exitJumpIp)
    return false
  }

  private func compileWhen(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let cond, let exprs)) = expr else {
      throw RuntimeError.argumentCount(of: "when", min: 1, expr: expr)
    }
    try compiler.compile(cond, in: env, inTailPos: false)
    let elseJumpIp = compiler.emitPlaceholder()
    compiler.emit(.pushVoid)
    let exitJumpIp = compiler.emitPlaceholder()
    compiler.patch(.branchIf(compiler.offsetToNext(elseJumpIp)), at: elseJumpIp)
    if try compiler.compileSeq(exprs, in: env, inTailPos: tail) {
      compiler.patch(.return, at: exitJumpIp)
      return true
    }
    compiler.patch(.branch(compiler.offsetToNext(exitJumpIp)), at: exitJumpIp)
    return false
  }

  private func compileUnless(_ compiler: Compiler,
                             expr: Expr,
                             env: Env,
                             tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let cond, let exprs)) = expr else {
      throw RuntimeError.argumentCount(of: "unless", min: 1, expr: expr)
    }
    try compiler.compile(cond, in: env, inTailPos: false)
    let elseJumpIp = compiler.emitPlaceholder()
    compiler.emit(.pushVoid)
    let exitJumpIp = compiler.emitPlaceholder()
    compiler.patch(.branchIfNot(compiler.offsetToNext(elseJumpIp)), at: elseJumpIp)
    if try compiler.compileSeq(exprs, in: env, inTailPos: tail) {
      compiler.patch(.return, at: exitJumpIp)
      return true
    }
    compiler.patch(.branch(compiler.offsetToNext(exitJumpIp)), at: exitJumpIp)
    return false
  }

  private func compileCond(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    // Extract case list
    guard  case .pair(_, let caseList) = expr else {
      preconditionFailure()
    }
    // Keep track of jumps for successful cases
    var exitJumps = [Int]()
    var exitOrJumps = [Int]()
    var cases = caseList
    // Track if there was an else case and whether there was a tail call in the else case
    var elseCaseTailCall: Bool? = nil
    // Iterate through all cases
    while case .pair(let cas, let rest) = cases {
      switch cas {
        case .pair(.symbol(let s), let exprs) where s.root == compiler.context.symbols.else:
          guard rest == .null else {
            throw RuntimeError.eval(.malformedCondClause, cases)
          }
          elseCaseTailCall = try compiler.compileSeq(exprs,
                                                     in: env,
                                                     inTailPos: tail)
        case .pair(let test, .null):
          try compiler.compile(test, in: env, inTailPos: false)
          exitOrJumps.append(compiler.emitPlaceholder())
        case .pair(let test, .pair(.symbol(let s), .pair(let proc, .null)))
            where s.root == compiler.context.symbols.doubleArrow:
          // Compile condition
          try compiler.compile(test, in: env, inTailPos: false)
          // Jump if it's false or inject stack frame
          let escapeIp = compiler.emitPlaceholder()
          // Inject stack frame
          let pushFrameIp = compiler.emit(.injectFrame)
          // Compile procedure
          try compiler.compile(proc, in: env, inTailPos: false)
          // Swap procedure with argument (= condition)
          compiler.emit(.swap)
          // Call procedure
          if compiler.call(1, inTailPos: tail) {
            // Remove InjectFrame if this was a tail call
            compiler.patch(.noOp, at: pushFrameIp)
          } else {
            exitJumps.append(compiler.emitPlaceholder())
          }
          compiler.patch(.keepOrBranchIfNot(compiler.offsetToNext(escapeIp)), at: escapeIp)
        case .pair(let test, let exprs):
          try compiler.compile(test, in: env, inTailPos: false)
          let escapeIp = compiler.emitPlaceholder()
          if !(try compiler.compileSeq(exprs,
                                       in: env,
                                       inTailPos: tail)) {
            exitJumps.append(compiler.emitPlaceholder())
          }
          compiler.patch(.branchIfNot(compiler.offsetToNext(escapeIp)), at: escapeIp)
        default:
          throw RuntimeError.eval(.malformedCondClause, cas)
      }
      cases = rest
    }
    // Was there an else case?
    if let wasTailCall = elseCaseTailCall {
      // Did the else case and all other cases have tail calls?
      if wasTailCall && exitJumps.count == 0 && exitOrJumps.count == 0 {
        return true
      }
    } else {
      // There was no else case: return false
      compiler.emit(.pushFalse)
    }
    // Resolve jumps to current instruction
    for ip in exitJumps {
      compiler.patch(.branch(compiler.offsetToNext(ip)), at: ip)
    }
    for ip in exitOrJumps {
      compiler.patch(.or(compiler.offsetToNext(ip)), at: ip)
    }
    return false
  }

  private func compileCase(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let key, let caseList)) = expr else {
      throw RuntimeError.argumentCount(of: "case", min: 3, expr: expr)
    }
    // Keep track of jumps for successful cases
    var exitJumps = [Int]()
    var cases = caseList
    // Track if there was an else case and whether there was a tail call in the else case
    var elseCaseTailCall: Bool? = nil
    // Compile key
    try compiler.compile(key, in: env, inTailPos: false)
    // Compile cases
    while case .pair(let cas, let rest) = cases {
      switch cas {
        case .pair(.symbol(let s), .pair(.symbol(let t), .pair(let proc, .null)))
               where s.root == compiler.context.symbols.else &&
                     t.root == compiler.context.symbols.doubleArrow:
          guard rest == .null else {
            throw RuntimeError.eval(.malformedCaseClause, cases)
          }
          // Inject stack frame
          let pushFrameIp = compiler.emit(.injectFrame)
          // Compile procedure
          try compiler.compile(proc, in: env, inTailPos: false)
          // Swap procedure with argument (= condition)
          compiler.emit(.swap)
          // Call procedure
          if compiler.call(1, inTailPos: tail) {
            // Remove InjectFrame if this was a tail call
            compiler.patch(.noOp, at: pushFrameIp)
            elseCaseTailCall = true
          } else {
            elseCaseTailCall = false
          }
        case .pair(.symbol(let s), let exprs) where s.root == compiler.context.symbols.else:
          guard rest == .null else {
            throw RuntimeError.eval(.malformedCaseClause, cases)
          }
          compiler.emit(.pop)
          elseCaseTailCall = try compiler.compileSeq(exprs,
                                                     in: env,
                                                     inTailPos: tail)
        case .pair(var keys, .pair(.symbol(let s), .pair(let proc, .null)))
               where s.root == compiler.context.symbols.doubleArrow:
          // Check keys
          var positiveJumps = [Int]()
          while case .pair(let value, let next) = keys {
            compiler.emit(.dup)
            try compiler.pushValue(value)
            compiler.emit(.eqv)
            positiveJumps.append(compiler.emitPlaceholder())
            keys = next
          }
          guard keys.isNull else {
            throw RuntimeError.eval(.malformedCaseClause, cas)
          }
          let jumpToNextCase = compiler.emitPlaceholder()
          for ip in positiveJumps {
            compiler.patch(.branchIf(compiler.offsetToNext(ip)), at: ip)
          }
          // Inject stack frame
          let pushFrameIp = compiler.emit(.injectFrame)
          // Compile procedure
          try compiler.compile(proc, in: env, inTailPos: false)
          // Swap procedure with argument (= condition)
          compiler.emit(.swap)
          // Call procedure
          if compiler.call(1, inTailPos: tail) {
            // Remove InjectFrame if this was a tail call
            compiler.patch(.noOp, at: pushFrameIp)
          } else {
            exitJumps.append(compiler.emitPlaceholder())
          }
          compiler.patch(.branch(compiler.offsetToNext(jumpToNextCase)), at: jumpToNextCase)
        case .pair(var keys, let exprs):
          var positiveJumps = [Int]()
          while case .pair(let value, let next) = keys {
            compiler.emit(.dup)
            try compiler.pushValue(value)
            compiler.emit(.eqv)
            positiveJumps.append(compiler.emitPlaceholder())
            keys = next
          }
          guard keys.isNull else {
            throw RuntimeError.eval(.malformedCaseClause, cas)
          }
          let jumpToNextCase = compiler.emitPlaceholder()
          for ip in positiveJumps {
            compiler.patch(.branchIf(compiler.offsetToNext(ip)), at: ip)
          }
          compiler.emit(.pop)
          if !(try compiler.compileSeq(exprs,
                                       in: env,
                                       inTailPos: tail)) {
            exitJumps.append(compiler.emitPlaceholder())
          }
          compiler.patch(.branch(compiler.offsetToNext(jumpToNextCase)), at: jumpToNextCase)
        default:
          throw RuntimeError.eval(.malformedCaseClause, cas)
      }
      cases = rest
    }
    // Was there an else case?
    if let wasTailCall = elseCaseTailCall {
      // Did the else case and all other cases have tail calls?
      if wasTailCall && exitJumps.count == 0 {
        return true
      }
    } else {
      // There was no else case: drop key and return false
      compiler.emit(.pop)
      compiler.emit(.pushFalse)
    }
    // Resolve jumps to current instruction
    for ip in exitJumps {
      compiler.patch(.branch(compiler.offsetToNext(ip)), at: ip)
    }
    return false
  }
}
