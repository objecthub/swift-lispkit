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
    self.define("begin", as: SpecialForm(self.compileBegin))
    self.define("let", as: SpecialForm(self.compileLet))
    self.define("let*", as: SpecialForm(self.compileLetStar))
    self.define("letrec", as: SpecialForm(self.compileLetRec))
    self.define("letrec*", as: SpecialForm(self.compileLetRecStar))
    self.define("let-values", as: SpecialForm(self.compileLetValues))
    self.define("let*-values", as: SpecialForm(self.compileLetStarValues))
    self.define("let-optionals", as: SpecialForm(self.compileLetOptionals))
    self.define("let*-optionals", as: SpecialForm(self.compileLetStarOptionals))
    self.define("let-syntax", as: SpecialForm(self.compileLetSyntax))
    self.define("letrec-syntax", as: SpecialForm(self.compileLetRecSyntax))
    self.define("do", as: SpecialForm(self.compileDo))
    self.define("if", as: SpecialForm(self.compileIf))
    self.define("when", as: SpecialForm(self.compileWhen))
    self.define("unless", as: SpecialForm(self.compileUnless))
    self.define("cond", as: SpecialForm(self.compileCond))
    self.define("case", as: SpecialForm(self.compileCase))
  }
  
  private func splitBindings(_ bindingList: Expr) throws -> (Expr, Expr) {
    var symbols = Exprs()
    var exprs = Exprs()
    var bindings = bindingList
    while case .pair(let binding, let rest) = bindings {
      guard case .pair(.symbol(let sym), .pair(let expr, .null)) = binding else {
        throw EvalError.malformedBindings(binding, bindingList)
      }
      symbols.append(.symbol(sym))
      exprs.append(expr)
      bindings = rest
    }
    guard bindings.isNull else {
      throw EvalError.malformedBindings(nil, bindingList)
    }
    return (Expr.makeList(symbols), Expr.makeList(exprs))
  }

  private func compileBegin(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, let exprs) = expr else {
      preconditionFailure("malformed begin")
    }
    return try compiler.compileSeq(exprs, in: env, inTailPos: tail, localDefine: false)
  }

  private func compileLet(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let first, let body)) = expr else {
      throw EvalError.leastArgumentCountError(formals: 1, args: expr)
    }
    let initialLocals = compiler.numLocals
    var res = false
    switch first {
      case .null:
        return try compiler.compileSeq(body, in: env, inTailPos: tail)
      case .pair(_, _):
        let group = try compiler.compileBindings(first, in: env, atomic: true, predef: false)
        res = try compiler.compileSeq(body, in: Env(group), inTailPos: tail)
        group.finalize()
      case .symbol(let sym):
        guard case .pair(let bindings, let rest) = body else {
          throw EvalError.leastArgumentCountError(formals: 2, args: expr)
        }
        let (params, exprs) = try splitBindings(bindings)
        let group = BindingGroup(owner: compiler, parent: env)
        let index = group.allocBindingFor(sym).index
        compiler.emit(.pushUndef)
        compiler.emit(.makeLocalVariable(index))
        let nameIdx = compiler.registerConstant(first)
        try compiler.compileLambda(nameIdx, params, rest, Env(group))
        compiler.emit(.setLocalValue(index))
        res = try compiler.compile(.pair(first, exprs), in: Env(group), inTailPos: tail)
      default:
        throw EvalError.typeError(first, [.listType, .symbolType])
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
      throw EvalError.leastArgumentCountError(formals: 1, args: expr)
    }
    let initialLocals = compiler.numLocals
    switch first {
      case .null:
        return try compiler.compileSeq(body, in: env, inTailPos: tail)
      case .pair(_, _):
        let group = try compiler.compileBindings(first, in: env, atomic: false, predef: false)
        let res = try compiler.compileSeq(body, in: Env(group), inTailPos: tail)
        return compiler.finalizeBindings(group, exit: res, initialLocals: initialLocals)
      default:
        throw EvalError.typeError(first, [.listType])
    }
  }

  private func compileLetRec(_ compiler: Compiler,
                             expr: Expr,
                             env: Env,
                             tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let first, let body)) = expr else {
      throw EvalError.leastArgumentCountError(formals: 1, args: expr)
    }
    let initialLocals = compiler.numLocals
    switch first {
    case .null:
      return try compiler.compileSeq(body, in: env, inTailPos: tail)
    case .pair(_, _):
      let group = try compiler.compileBindings(first,
                                               in: env,
                                               atomic: true,
                                               predef: true,
                                               postset: true)
      let res = try compiler.compileSeq(body, in: Env(group), inTailPos: tail)
      return compiler.finalizeBindings(group, exit: res, initialLocals: initialLocals)
    default:
      throw EvalError.typeError(first, [.listType])
    }
  }

  private func compileLetRecStar(_ compiler: Compiler,
                                 expr: Expr,
                                 env: Env,
                                 tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let first, let body)) = expr else {
      throw EvalError.leastArgumentCountError(formals: 1, args: expr)
    }
    let initialLocals = compiler.numLocals
    switch first {
      case .null:
        return try compiler.compileSeq(body, in: env, inTailPos: tail)
      case .pair(_, _):
        let group = try compiler.compileBindings(first, in: env, atomic: true, predef: true)
        let res = try compiler.compileSeq(body, in: Env(group), inTailPos: tail)
        return compiler.finalizeBindings(group, exit: res, initialLocals: initialLocals)
      default:
        throw EvalError.typeError(first, [.listType])
    }
  }

  private func compileLetValues(_ compiler: Compiler,
                                expr: Expr,
                                env: Env,
                                tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let first, let body)) = expr else {
      throw EvalError.leastArgumentCountError(formals: 1, args: expr)
    }
    let initialLocals = compiler.numLocals
    switch first {
      case .null:
        return try compiler.compileSeq(body, in: env, inTailPos: tail)
      case .pair(_, _):
        let group = try compiler.compileMultiBindings(first, in: env, atomic: true)
        let res = try compiler.compileSeq(body, in: Env(group), inTailPos: tail)
        return compiler.finalizeBindings(group, exit: res, initialLocals: initialLocals)
      default:
        throw EvalError.typeError(first, [.listType])
    }
  }
  
  private func compileLetStarValues(_ compiler: Compiler,
                                    expr: Expr,
                                    env: Env,
                                    tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let first, let body)) = expr else {
      throw EvalError.leastArgumentCountError(formals: 1, args: expr)
    }
    let initialLocals = compiler.numLocals
    switch first {
      case .null:
        return try compiler.compileSeq(body, in: env, inTailPos: tail)
      case .pair(_, _):
        let group = try compiler.compileMultiBindings(first, in: env, atomic: false)
        let res = try compiler.compileSeq(body, in: Env(group), inTailPos: tail)
        return compiler.finalizeBindings(group, exit: res, initialLocals: initialLocals)
      default:
        throw EvalError.typeError(first, [.listType])
    }
  }
  
  private func compileLetOptionals(_ compiler: Compiler,
                                   expr: Expr,
                                   env: Env,
                                   tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let optlist, .pair(let first, let body))) = expr else {
      throw EvalError.leastArgumentCountError(formals: 2, args: expr)
    }
    let initialLocals = compiler.numLocals
    switch first {
      case .null:
        return try compiler.compileSeq(.pair(optlist, body), in: env, inTailPos: tail)
      case .pair(_, _):
        try compiler.compile(optlist, in: env, inTailPos: false)
        let group = try self.compileOptionalBindings(compiler, first, in: env, atomic: true)
        compiler.emit(.pop)
        let res = try compiler.compileSeq(body, in: Env(group), inTailPos: tail)
        return compiler.finalizeBindings(group, exit: res, initialLocals: initialLocals)
      default:
        throw EvalError.typeError(first, [.listType])
    }
  }
  
  private func compileLetStarOptionals(_ compiler: Compiler,
                                       expr: Expr,
                                       env: Env,
                                       tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let optlist, .pair(let first, let body))) = expr else {
      throw EvalError.leastArgumentCountError(formals: 2, args: expr)
    }
    let initialLocals = compiler.numLocals
    switch first {
      case .null:
        return try compiler.compileSeq(.pair(optlist, body), in: env, inTailPos: tail)
      case .pair(_, _):
        try compiler.compile(optlist, in: env, inTailPos: false)
        let group = try self.compileOptionalBindings(compiler, first, in: env, atomic: false)
        compiler.emit(.pop)
        let res = try compiler.compileSeq(body, in: Env(group), inTailPos: tail)
        return compiler.finalizeBindings(group, exit: res, initialLocals: initialLocals)
      default:
        throw EvalError.typeError(first, [.listType])
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
        throw EvalError.malformedBindings(binding, bindingList)
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
        throw EvalError.duplicateBinding(sym, bindingList)
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
      throw EvalError.malformedBindings(nil, bindingList)
    }
    return group
  }
  
  private func compileLetSyntax(_ compiler: Compiler,
                                expr: Expr,
                                env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let first, let body)) = expr else {
      throw EvalError.leastArgumentCountError(formals: 1, args: expr)
    }
    switch first {
      case .null:
        return try compiler.compileSeq(body, in: env, inTailPos: tail)
      case .pair(_, _):
        let group = try compiler.compileMacros(first, in: env, recursive: false)
        return try compiler.compileSeq(body, in: Env(group), inTailPos: tail)
      default:
        throw EvalError.typeError(first, [.listType])
    }
  }

  private func compileLetRecSyntax(_ compiler: Compiler,
                                   expr: Expr,
                                   env: Env,
                                   tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let first, let body)) = expr else {
      throw EvalError.leastArgumentCountError(formals: 1, args: expr)
    }
    switch first {
      case .null:
        return try compiler.compileSeq(body, in: env, inTailPos: tail)
      case .pair(_, _):
        let group = try compiler.compileMacros(first, in: env, recursive: true)
        return try compiler.compileSeq(body, in: Env(group), inTailPos: tail)
      default:
        throw EvalError.typeError(first, [.listType])
    }
  }

  private func compileDo(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    // Decompose expression into bindings, exit, and body
    guard case .pair(_, .pair(let bindingList, .pair(let exit, let body))) = expr else {
      throw EvalError.leastArgumentCountError(formals: 2, args: expr)
    }
    // Extract test and terminal expressions
    guard case .pair(let test, let terminal) = exit else {
      throw EvalError.malformedTest(exit)
    }
    let initialLocals = compiler.numLocals
    // Setup bindings
    let group = BindingGroup(owner: compiler, parent: env)
    var bindings = bindingList
    var prevIndex = -1
    var doBindings = [Int]()
    var stepExprs = [Expr]()
    // Compile initial bindings
    while case .pair(let binding, let rest) = bindings {
      guard case .pair(.symbol(let sym), .pair(let start, let optStep)) = binding else {
        throw EvalError.malformedBindings(binding, bindingList)
      }
      try compiler.compile(start, in: env, inTailPos: false)
      let index = group.allocBindingFor(sym).index
      guard index > prevIndex else {
        throw EvalError.duplicateBinding(sym, bindingList)
      }
      compiler.emit(.makeLocalVariable(index))
      switch optStep {
        case .pair(let step, .null):
          doBindings.append(index)
          stepExprs.append(step)
        case .null:
          break;
        default:
          throw EvalError.malformedBindings(binding, bindingList)
      }
      prevIndex = index
      bindings = rest
    }
    guard bindings.isNull else {
      throw EvalError.malformedBindings(nil, bindingList)
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
    let res = try compiler.compileSeq(terminal, in: Env(group), inTailPos: tail)
    // Remove bindings from stack
    if !res && compiler.numLocals > initialLocals {
      compiler.emit(.reset(initialLocals, compiler.numLocals - initialLocals))
    }
    compiler.numLocals = initialLocals
    return res
  }

  private func compileIf(_ compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
    guard case .pair(_, .pair(let cond, .pair(let thenp, let alternative))) = expr else {
      throw EvalError.leastArgumentCountError(formals: 2, args: expr)
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
      throw EvalError.leastArgumentCountError(formals: 1, args: expr)
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
      throw EvalError.leastArgumentCountError(formals: 1, args: expr)
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
        case .pair(.symbol(let sym), let exprs) where sym === compiler.context.symbols.else:
          guard rest == .null else {
            throw EvalError.malformedCondClause(cases)
          }
          elseCaseTailCall = try compiler.compileSeq(exprs, in: env, inTailPos: tail)
        case .pair(let test, .null):
          try compiler.compile(test, in: env, inTailPos: false)
          exitOrJumps.append(compiler.emitPlaceholder())
        case .pair(let test, .pair(.symbol(let sym), .pair(let proc, .null)))
            where sym === compiler.context.symbols.doubleArrow:
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
          if !(try compiler.compileSeq(exprs, in: env, inTailPos: tail)) {
            exitJumps.append(compiler.emitPlaceholder())
          }
          compiler.patch(.branchIfNot(compiler.offsetToNext(escapeIp)), at: escapeIp)
        default:
          throw EvalError.malformedCondClause(cas)
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
      throw EvalError.leastArgumentCountError(formals: 3, args: expr)
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
        case .pair(.symbol(compiler.context.symbols.else), let exprs):
          guard rest == .null else {
            throw EvalError.malformedCaseClause(cases)
          }
          compiler.emit(.pop)
          elseCaseTailCall = try compiler.compileSeq(exprs, in: env, inTailPos: tail)
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
            throw EvalError.malformedCaseClause(cas)
          }
          let jumpToNextCase = compiler.emitPlaceholder()
          for ip in positiveJumps {
            compiler.patch(.branchIf(compiler.offsetToNext(ip)), at: ip)
          }
          compiler.emit(.pop)
          if !(try compiler.compileSeq(exprs, in: env, inTailPos: tail)) {
            exitJumps.append(compiler.emitPlaceholder())
          }
          compiler.patch(.branch(compiler.offsetToNext(jumpToNextCase)), at: jumpToNextCase)
        default:
          throw EvalError.malformedCaseClause(cas)
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
