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


public final class ControlFlowLibrary: Library {
  
  public override func export() {
    define("begin", SpecialForm(compileBegin))
    define("let", SpecialForm(compileLet))
    define("let*", SpecialForm(compileLetStar))
    define("letrec", SpecialForm(compileLetRec))
    define("let-syntax", SpecialForm(compileLetSyntax))
    define("letrec-syntax", SpecialForm(compileLetRecSyntax))
    define("do", SpecialForm(compileDo))
    define("if", SpecialForm(compileIf))
    define("cond", SpecialForm(compileCond))
    define("case", SpecialForm(compileCase))
  }
}

func compileBegin(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
  guard case .Pair(_, let exprs) = expr else {
    preconditionFailure()
  }
  return try compiler.compileSeq(exprs, in: env, inTailPos: tail)
}

private func compileBindings(compiler: Compiler,
                             bindingList: Expr,
                             in lenv: Env,
                             atomic: Bool,
                             predef: Bool) throws -> BindingGroup {
  let group = BindingGroup(owner: compiler, parent: lenv, nextIndex: compiler.nextLocalIndex)
  let env = atomic && !predef ? lenv : .Local(group)
  var bindings = bindingList
  if predef {
    while case .Pair(.Pair(.Sym(let sym), _), let rest) = bindings {
      compiler.emit(.PushUndef)
      compiler.emit(.SetLocalVariable(group.allocBindingFor(sym).index))
      bindings = rest
    }
    bindings = bindingList
  }
  var prevIndex = -1
  while case .Pair(let binding, let rest) = bindings {
    guard case .Pair(.Sym(let sym), .Pair(let expr, .Null)) = binding else {
      throw EvalError.MalformedBindings(binding, bindingList)
    }
    try compiler.compile(expr, in: env, inTailPos: false)
    let index = group.allocBindingFor(sym).index
    guard index > prevIndex else {
      throw EvalError.DuplicateBinding(sym, bindingList)
    }
    compiler.emit(predef ? .SetLocalValue(index) : .SetLocalVariable(index))
    prevIndex = index
    bindings = rest
  }
  guard bindings.isNull else {
    throw EvalError.MalformedBindings(nil, bindingList)
  }
  return group
}

private func compileMacros(compiler: Compiler,
                           bindingList: Expr,
                           in lenv: Env,
                           recursive: Bool) throws -> BindingGroup {
  var numMacros = 0
  let group = BindingGroup(owner: compiler, parent: lenv, nextIndex: {
    numMacros += 1
    return numMacros - 1
  })
  let env = recursive ? Env(group) : lenv
  var bindings = bindingList
  while case .Pair(let binding, let rest) = bindings {
    guard case .Pair(.Sym(let sym), .Pair(let transformer, .Null)) = binding else {
      throw EvalError.MalformedBindings(binding, bindingList)
    }
    let procExpr = try compiler.context.machine.eval(transformer,
                                                     in: env.syntacticalEnv,
                                                     usingRulesEnv: env)
    guard case .Proc(let proc) = procExpr else {
      throw EvalError.MalformedTransformer(transformer) //FIXME: Find better error message
    }
    guard group.bindingFor(sym) == nil else {
      throw EvalError.DuplicateBinding(sym, bindingList)
    }
    group.defineMacro(sym, proc: proc)
    bindings = rest
  }
  guard bindings.isNull else {
    throw EvalError.MalformedBindings(nil, bindingList)
  }
  return group
}

func splitBindings(bindingList: Expr) throws -> (Expr, Expr) {
  var symbols = Exprs()
  var exprs = Exprs()
  var bindings = bindingList
  while case .Pair(let binding, let rest) = bindings {
    guard case .Pair(.Sym(let sym), .Pair(let expr, .Null)) = binding else {
      throw EvalError.MalformedBindings(binding, bindingList)
    }
    symbols.append(.Sym(sym))
    exprs.append(expr)
    bindings = rest
  }
  guard bindings.isNull else {
    throw EvalError.MalformedBindings(nil, bindingList)
  }
  return (Expr.List(symbols), Expr.List(exprs))
}

func compileLet(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
  guard case .Pair(_, .Pair(let first, let body)) = expr else {
    throw EvalError.LeastArgumentCountError(formals: 1, args: expr)
  }
  let initialLocals = compiler.numLocals
  var res = false
  switch first {
    case .Null:
      return try compiler.compileSeq(body, in: env, inTailPos: tail)
    case .Pair(_, _):
      let group = try compileBindings(compiler, bindingList: first, in: env, atomic: true, predef: false)
      res = try compiler.compileSeq(body, in: Env(group), inTailPos: tail)
    case .Sym(let sym):
      guard case .Pair(let bindings, let rest) = body else {
        throw EvalError.LeastArgumentCountError(formals: 2, args: expr)
      }
      let (params, exprs) = try splitBindings(bindings)
      let group = BindingGroup(owner: compiler, parent: env, nextIndex: compiler.nextLocalIndex)
      let index = group.allocBindingFor(sym).index
      compiler.emit(.PushUndef)
      compiler.emit(.SetLocalVariable(index))
      compiler.emit(try BaseLibrary.compileProc(compiler, params, rest, Env(group), false))
      compiler.emit(.SetLocalValue(index))
      res = try compiler.compile(.Pair(first, exprs), in: Env(group), inTailPos: tail)
    default:
      throw EvalError.TypeError(first, [.ListType, .SymbolType])
  }
  if !res && compiler.numLocals > initialLocals {
    compiler.emit(.ResetLocals(initialLocals, compiler.numLocals - initialLocals))
  }
  compiler.numLocals = initialLocals
  return res
}

func compileLetStar(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
  guard case .Pair(_, .Pair(let first, let body)) = expr else {
    throw EvalError.LeastArgumentCountError(formals: 1, args: expr)
  }
  let initialLocals = compiler.numLocals
  switch first {
    case .Null:
      return try compiler.compileSeq(body, in: env, inTailPos: tail)
    case .Pair(_, _):
      let group = try compileBindings(compiler, bindingList: first, in: env, atomic: false, predef: false)
      let res = try compiler.compileSeq(body, in: Env(group), inTailPos: tail)
      if !res && compiler.numLocals > initialLocals {
        compiler.emit(.ResetLocals(initialLocals, compiler.numLocals - initialLocals))
      }
      compiler.numLocals = initialLocals
      return res
    default:
      throw EvalError.TypeError(first, [.ListType])
  }
}

func compileLetRec(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
  guard case .Pair(_, .Pair(let first, let body)) = expr else {
    throw EvalError.LeastArgumentCountError(formals: 1, args: expr)
  }
  let initialLocals = compiler.numLocals
  switch first {
    case .Null:
      return try compiler.compileSeq(body, in: env, inTailPos: tail)
    case .Pair(_, _):
      let group = try compileBindings(compiler, bindingList: first, in: env, atomic: true, predef: true)
      let res = try compiler.compileSeq(body, in: Env(group), inTailPos: tail)
      if !res && compiler.numLocals > initialLocals {
        compiler.emit(.ResetLocals(initialLocals, compiler.numLocals - initialLocals))
      }
      compiler.numLocals = initialLocals
      return res
    default:
      throw EvalError.TypeError(first, [.ListType])
  }
}

func compileLetSyntax(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
  guard case .Pair(_, .Pair(let first, let body)) = expr else {
    throw EvalError.LeastArgumentCountError(formals: 1, args: expr)
  }
  switch first {
    case .Null:
      return try compiler.compileSeq(body, in: env, inTailPos: tail)
    case .Pair(_, _):
      let group = try compileMacros(compiler, bindingList: first, in: env, recursive: false)
      return try compiler.compileSeq(body, in: Env(group), inTailPos: tail)
    default:
      throw EvalError.TypeError(first, [.ListType])
  }
}

func compileLetRecSyntax(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
  guard case .Pair(_, .Pair(let first, let body)) = expr else {
    throw EvalError.LeastArgumentCountError(formals: 1, args: expr)
  }
  switch first {
    case .Null:
      return try compiler.compileSeq(body, in: env, inTailPos: tail)
    case .Pair(_, _):
      let group = try compileMacros(compiler, bindingList: first, in: env, recursive: true)
      return try compiler.compileSeq(body, in: Env(group), inTailPos: tail)
    default:
      throw EvalError.TypeError(first, [.ListType])
  }
}

func compileDo(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
  // Decompose expression into bindings, exit, and body
  guard case .Pair(_, .Pair(let bindingList, .Pair(let exit, let body))) = expr else {
    throw EvalError.LeastArgumentCountError(formals: 2, args: expr)
  }
  // Extract test and terminal expressions
  guard case .Pair(let test, let terminal) = exit else {
    throw EvalError.MalformedTest(exit)
  }
  let initialLocals = compiler.numLocals
  // Setup bindings
  let group = BindingGroup(owner: compiler, parent: env, nextIndex: compiler.nextLocalIndex)
  var bindings = bindingList
  var prevIndex = -1
  var doBindings = [Int]()
  var stepExprs = [Expr]()
  // Compile initial bindings
  while case .Pair(let binding, let rest) = bindings {
    guard case .Pair(.Sym(let sym), .Pair(let start, let optStep)) = binding else {
      throw EvalError.MalformedBindings(binding, bindingList)
    }
    try compiler.compile(start, in: env, inTailPos: false)
    let index = group.allocBindingFor(sym).index
    guard index > prevIndex else {
      throw EvalError.DuplicateBinding(sym, bindingList)
    }
    compiler.emit(.SetLocalVariable(index))
    switch optStep {
      case .Pair(let step, .Null):
        doBindings.append(index)
        stepExprs.append(step)
      case .Null:
        break;
      default:
        throw EvalError.MalformedBindings(binding, bindingList)
    }
    prevIndex = index
    bindings = rest
  }
  guard bindings.isNull else {
    throw EvalError.MalformedBindings(nil, bindingList)
  }
  // Compile test expression
  let testIp = compiler.offsetToNext(0)
  try compiler.compile(test, in: Env(group), inTailPos: false)
  let exitJumpIp = compiler.emitPlaceholder()
  // Compile body
  try compiler.compileSeq(body, in: Env(group), inTailPos: false)
  compiler.emit(.Pop)
  // Compile step expressions and update bindings
  for step in stepExprs {
    try compiler.compile(step, in: Env(group), inTailPos: false)
  }
  for index in doBindings.reverse() {
    compiler.emit(.SetLocalValue(index))
  }
  // Loop
  compiler.emit(.Branch(-compiler.offsetToNext(testIp)))
  // Exit if the test expression evaluates to true
  compiler.patch(.BranchIf(compiler.offsetToNext(exitJumpIp)), at: exitJumpIp)
  // Compile terminal expressions
  let res = try compiler.compileSeq(terminal, in: Env(group), inTailPos: tail)
  // Remove bindings from stack
  if !res && compiler.numLocals > initialLocals {
    compiler.emit(.ResetLocals(initialLocals, compiler.numLocals - initialLocals))
  }
  compiler.numLocals = initialLocals
  return res
}

func compileIf(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
  guard case .Pair(_, .Pair(let cond, .Pair(let thenp, let alternative))) = expr else {
    throw EvalError.LeastArgumentCountError(formals: 2, args: expr)
  }
  var elsep = Expr.Void
  if case .Pair(let ep, .Null) = alternative {
    elsep = ep
  }
  try compiler.compile(cond, in: env, inTailPos: false)
  let elseJumpIp = compiler.emitPlaceholder()
  // Compile if in tail position
  if try compiler.compile(elsep, in: env, inTailPos: tail) {
    compiler.patch(.BranchIf(compiler.offsetToNext(elseJumpIp)), at: elseJumpIp)
    return try compiler.compile(thenp, in: env, inTailPos: true)
  }
  // Compile if in non-tail position
  let exitJumpIp = compiler.emitPlaceholder()
  compiler.patch(.BranchIf(compiler.offsetToNext(elseJumpIp)), at: elseJumpIp)
  if try compiler.compile(thenp, in: env, inTailPos: tail) {
    compiler.patch(.Return, at: exitJumpIp)
    return true
  }
  compiler.patch(.Branch(compiler.offsetToNext(exitJumpIp)), at: exitJumpIp)
  return false
}

func compileCond(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
  // Extract case list
  guard  case .Pair(_, let caseList) = expr else {
    preconditionFailure()
  }
  // Keep track of jumps for successful cases
  var exitJumps = [Int]()
  var exitOrJumps = [Int]()
  var cases = caseList
  // Track if there was an else case and whether there was a tail call in the else case
  var elseCaseTailCall: Bool? = nil
  // Iterate through all cases
  while case .Pair(let cas, let rest) = cases {
    switch cas {
      case .Pair(.Sym(let sym), let exprs) where sym === compiler.context.symbols.ELSE:
        guard rest == .Null else {
          throw EvalError.MalformedCondClause(cases)
        }
        elseCaseTailCall = try compiler.compileSeq(exprs, in: env, inTailPos: tail)
      case .Pair(let test, .Null):
        try compiler.compile(test, in: env, inTailPos: false)
        exitOrJumps.append(compiler.emitPlaceholder())
      case .Pair(let test, .Pair(.Sym(let sym), .Pair(let res, .Null)))
          where sym === compiler.context.symbols.DOUBLEARROW:
        try compiler.compile(test, in: env, inTailPos: false)
        let escapeIp = compiler.emitPlaceholder()
        if !(try compiler.compile(res, in: env, inTailPos: tail)) {
          exitJumps.append(compiler.emitPlaceholder())
        }
        compiler.patch(.BranchIfNot(compiler.offsetToNext(escapeIp)), at: escapeIp)
      case .Pair(let test, let exprs):
        try compiler.compile(test, in: env, inTailPos: false)
        let escapeIp = compiler.emitPlaceholder()
        if !(try compiler.compileSeq(exprs, in: env, inTailPos: tail)) {
          exitJumps.append(compiler.emitPlaceholder())
        }
        compiler.patch(.BranchIfNot(compiler.offsetToNext(escapeIp)), at: escapeIp)
      default:
        throw EvalError.MalformedCondClause(cas)
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
    compiler.emit(.PushFalse)
  }
  // Resolve jumps to current instruction
  for ip in exitJumps {
    compiler.patch(.Branch(compiler.offsetToNext(ip)), at: ip)
  }
  for ip in exitOrJumps {
    compiler.patch(.Or(compiler.offsetToNext(ip)), at: ip)
  }
  return false
}

func compileCase(compiler: Compiler, expr: Expr, env: Env, tail: Bool) throws -> Bool {
  guard case .Pair(_, .Pair(let key, let caseList)) = expr else {
    throw EvalError.LeastArgumentCountError(formals: 3, args: expr)
  }
  // Keep track of jumps for successful cases
  var exitJumps = [Int]()
  var cases = caseList
  // Track if there was an else case and whether there was a tail call in the else case
  var elseCaseTailCall: Bool? = nil
  // Compile key
  try compiler.compile(key, in: env, inTailPos: false)
  // Compile cases
  while case .Pair(let cas, let rest) = cases {
    switch cas {
      case .Pair(.Sym(compiler.context.symbols.ELSE), let exprs):
        guard rest == .Null else {
          throw EvalError.MalformedCaseClause(cases)
        }
        compiler.emit(.Pop)
        elseCaseTailCall = try compiler.compileSeq(exprs, in: env, inTailPos: tail)
      case .Pair(var keys, let exprs):
        var positiveJumps = [Int]()
        while case .Pair(let value, let next) = keys {
          compiler.emit(.Dup)
          try compiler.pushValue(value)
          compiler.emit(.Eqv)
          positiveJumps.append(compiler.emitPlaceholder())
          keys = next
        }
        guard keys.isNull else {
          throw EvalError.MalformedCaseClause(cas)
        }
        let jumpToNextCase = compiler.emitPlaceholder()
        for ip in positiveJumps {
          compiler.patch(.BranchIf(compiler.offsetToNext(ip)), at: ip)
        }
        compiler.emit(.Pop)
        if !(try compiler.compileSeq(exprs, in: env, inTailPos: tail)) {
          exitJumps.append(compiler.emitPlaceholder())
        }
        compiler.patch(.Branch(compiler.offsetToNext(jumpToNextCase)), at: jumpToNextCase)
      default:
        throw EvalError.MalformedCaseClause(cas)
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
    compiler.emit(.Pop)
    compiler.emit(.PushFalse)
  }
  // Resolve jumps to current instruction
  for ip in exitJumps {
    compiler.patch(.Branch(compiler.offsetToNext(ip)), at: ip)
  }
  return false
}


