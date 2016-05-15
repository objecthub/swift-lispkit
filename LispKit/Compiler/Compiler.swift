//
//  Code.swift
//  LispKit
//
//  Created by Matthias Zenger on 03/02/2016.
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

import NumberKit

///
/// Instances of class `Compiler` are used to either compile functions or expressions.
/// 
public final class Compiler {
  public let context: Context
  private var env: Env
  internal let rulesEnv: Env
  internal let checkpointer: Checkpointer
  private var argCp: UInt?
  internal var captures: CaptureGroup!
  internal var numLocals: Int = 0
  private var maxLocals: Int = 0
  private var arguments: BindingGroup?
  private var constants: [Expr] = []
  internal var fragments: [Code] = []
  private var instructions: [Instruction] = []
  
  
  public static func compile(context: Context, _ expr: Expr, _ opt: Bool = false) throws -> Code {
    let checkpointer = Checkpointer()
    var compiler = Compiler(context, .Interaction, .Interaction, checkpointer)
    try compiler.compileBody(expr)
    if opt {
      log(checkpointer.description)
      checkpointer.reset()
      compiler = Compiler(context, .Interaction, .Interaction, checkpointer)
      try compiler.compileBody(expr)
      log(checkpointer.description)
    }
    return compiler.bundle()
  }
  
  public init(_ context: Context, _ env: Env, _ rulesEnv: Env? = nil, _ cp: Checkpointer? = nil) {
    self.context = context
    self.env = env
    self.rulesEnv = rulesEnv ?? env
    self.checkpointer = cp ?? env.bindingGroup?.owner.checkpointer ?? Checkpointer()
    self.argCp = nil
    self.captures = CaptureGroup(owner: self, parent: env.bindingGroup?.owner.captures)
    self.arguments = nil
  }
  
  public var parent: Compiler? {
    return self.captures.parent?.owner
  }
  
  public func compileArgList(arglist: Expr) throws {
    self.argCp = self.checkpointer.checkpoint()
    let arguments = BindingGroup(owner: self, parent: self.env, nextIndex: self.nextLocalIndex)
    var next = arglist
    loop: while case .Pair(let arg, let cdr) = next {
      switch arg {
        case .Sym(let sym):
          arguments.allocBindingFor(sym,
                                    isVar: !self.checkpointer.isValueBinding(sym, at: self.argCp!))
        default:
          break loop
      }
      next = cdr
    }
    switch next {
      case .Null:
        self.emit(.AssertArgCount(arguments.count))
      case .Sym(let sym):
        if arguments.count > 0 {
          self.emit(.AssertMinArgs(arguments.count))
        }
        self.emit(.CollectRest(arguments.count))
        arguments.allocBindingFor(sym)
      default:
        throw EvalError.MalformedArgumentList(arglist)
    }
    self.arguments = arguments
    self.env = .Local(arguments)
  }
  
  public func compileBody(expr: Expr) throws {
    if expr.isNull {
      self.emit(.PushNull)
      self.emit(.Return)
    } else {
      // Reserve instruction for reserving local variables
      let reserveLocalIp = self.emit(.NoOp)
      // Turn arguments into local variables
      if let arguments = self.arguments {
        for sym in arguments.symbols {
          if let sym = sym, binding = arguments.bindingFor(sym) where !binding.isValue {
            self.emit(.MakeArgVariable(binding.index))
          }
        }
      }
      // Compile body
      if !(try compileSeq(expr, in: self.env, inTailPos: true)) {
        self.emit(.Return)
      }
      // Insert instruction to reserve local variables
      if self.maxLocals > self.arguments?.count ?? 0 {
        self.patch(
          .ReserveLocals(self.maxLocals - (self.arguments?.count ?? 0)), at: reserveLocalIp)
      }
      // Checkpoint argument mutability
      if let arguments = self.arguments {
        for sym in arguments.symbols {
          if let sym = sym where arguments.bindingFor(sym)?.isImmutableVariable ?? false {
            self.checkpointer.associate(.ValueBinding(sym), with: self.argCp!)
          }
        }
      }
    }
  }
  
  public func nextLocalIndex() -> Int {
    self.numLocals += 1
    if self.numLocals > self.maxLocals {
      self.maxLocals = self.numLocals
    }
    return self.numLocals - 1
  }
  
  public func removeLastInstr() -> Int {
    self.instructions.removeLast()
    return self.instructions.count - 1
  }
  
  public func emit(instr: Instruction) -> Int {
    self.instructions.append(instr)
    return self.instructions.count - 1
  }
  
  public func patch(instr: Instruction, at: Int) {
    self.instructions[at] = instr
  }
  
  public func emitPlaceholder() -> Int {
    return self.emit(.NoOp)
  }
  
  public func call(n: Int, _ tail: Bool) -> Bool {
    if tail {
      self.emit(.TailCall(n))
      return true
    } else {
      self.emit(.Call(n))
      return false
    }
  }
  
  public func offsetToNext(ip: Int) -> Int {
    return self.instructions.count - ip
  }
  
  /// Push the given expression onto the stack
  public func pushConstant(expr: Expr) {
    self.emit(.PushConstant(self.registerConstant(expr)))
  }
  
  public func registerConstant(expr: Expr) -> Int {
    for i in self.constants.indices {
      if self.constants[i] == expr {
        return i
      }
    }
    self.constants.append(expr.datum)
    return self.constants.count - 1
  }
  
  /// Push the value of the given symbol in the given environment onto the stack
  public func pushValueOf(sym: Symbol, in env: Env) throws {
    switch self.pushLocalValueOf(sym, in: env) {
      case .Success:
        break // Nothing to do
      case .GlobalLookupRequired(let lexicalSym, let global):
        if global.isSystem {
          if let value = self.context.systemScope[lexicalSym] {
            try self.pushValue(value)
          }
        }
        self.emit(.PushGlobal(self.registerConstant(.Sym(lexicalSym))))
      case .MacroExpansionRequired(_):
        throw EvalError.IllegalKeywordUsage(.Sym(sym))
    }
  }
  
  public enum LocalLookupResult {
    case Success
    case MacroExpansionRequired(Procedure)
    case GlobalLookupRequired(Symbol, Env)
  }
  
  public func pushLocalValueOf(sym: Symbol, in env: Env) -> LocalLookupResult {
    var env = env
    // Iterate through the local binding groups until `sym` is found
    while case .Local(let group) = env {
      if let binding = group.bindingFor(sym) {
        if case .Macro(let proc) = binding.kind {
          return .MacroExpansionRequired(proc)
        } else if group.owner === self {
          if binding.isValue {
            self.emit(.PushLocal(binding.index))
          } else {
            self.emit(.PushLocalValue(binding.index))
          }
        } else {
          let capturedIndex = self.captures.capture(binding, from: group)
          if binding.isValue {
            self.emit(.PushCaptured(capturedIndex))
          } else {
            self.emit(.PushCapturedValue(capturedIndex))
          }
        }
        return .Success
      }
      env = group.parent
    }
    // If `sym` wasn't found, look into the lexical environment
    if let (lexicalSym, lexicalEnv) = sym.lexical {
      return self.pushLocalValueOf(lexicalSym, in: lexicalEnv)
    }
    // Return global scope
    return .GlobalLookupRequired(sym, env)
  }
  
  public func pushValue(expr: Expr) throws {
    switch expr {
    case .Undef:
    break // TODO
    case .Void:
      self.emit(.PushVoid)
    case .Eof:
      self.emit(.PushEof)
    case .Null:
      self.emit(.PushNull)
    case .True:
      self.emit(.PushTrue)
    case .False:
      self.emit(.PushFalse)
    case .Fixnum(let num):
      self.emit(.PushFixnum(num))
    case .Bignum(let num):
      self.emit(.PushBignum(num))
    case .Rat(let num):
      self.emit(.PushRat(num))
    case .Bigrat(let num):
      self.emit(.PushBigrat(num))
    case .Flonum(let num):
      self.emit(.PushFlonum(num))
    case .Complexnum(let num):
      self.emit(.PushComplex(num))
    case .Char(let char):
      self.emit(.PushChar(char))
    case .Sym(_), .Str(_), .Vec(_), .Promise(_), .Proc(_), .Error(_), .Pair(_, _):
      self.pushConstant(expr)
    case .Special(_):
      throw EvalError.IllegalKeywordUsage(expr)
    case .Var(_):
      preconditionFailure("cannot compile variables")
    }
  }
  
  /// Set the value of the given symbol in the given environment
  public func setValueOf(sym: Symbol, in env: Env) {
    if let _ = self.setLocalValueOf(sym, in: env) {
      self.emit(.SetGlobal(self.registerConstant(.Sym(sym))))
    }
  }
  
  public func setLocalValueOf(sym: Symbol, in lenv: Env) -> Env? {
    var env = lenv
    // Iterate through the local binding groups until `sym` is found
    while case .Local(let group) = env {
      if let binding = group.bindingFor(sym) {
        if group.owner === self {
          self.emit(.SetLocalValue(binding.index))
        } else {
          self.emit(.SetCapturedValue(self.captures.capture(binding, from: group)))
        }
        binding.wasMutated()
        return nil
      }
      env = group.parent
    }
    // If `sym` wasn't found, look into the lexical environment
    if let (lexicalSym, lexicalEnv) = sym.lexical {
      return self.setLocalValueOf(lexicalSym, in: lexicalEnv)
    }
    return env
  }
  
  /// Compile expression `expr` in environment `env`. Parameter `tail` specifies if `expr`
  /// is located in a tail position. This allows compile to generate code with tail calls.
  public func compile(expr: Expr, in env: Env, inTailPos tail: Bool) throws -> Bool {
    let cp = self.checkpointer.checkpoint()
    switch expr {
      case .Sym(let sym):
        try self.pushValueOf(sym, in: env)
      case .Pair(.Sym(let sym), let cdr):
        let pushFrameIp = self.emit(.MakeFrame)
        // Try to push the function if locally defined
        switch self.pushLocalValueOf(sym, in: env) {
          case .Success:
            break // Nothing to do
          case .GlobalLookupRequired(let lexicalSym, let global):
            // Is there a special compiler plugin for this global binding
            if let value = self.checkpointer.fromGlobalEnv(cp) ??
                           global.scope(self.context)[lexicalSym] {
              self.checkpointer.associate(.FromGlobalEnv(value), with: cp)
              switch value {
                case .Proc(let proc):
                  if case .Primitive(_, .Some(let formCompiler)) = proc.kind {
                    self.removeLastInstr()
                    return try formCompiler(self, expr, env, tail)
                  }
                case .Special(let special):
                  self.removeLastInstr()
                  switch special.kind {
                    case .Primitive(let formCompiler):
                      return try formCompiler(self, expr, env, tail)
                    case .Macro(let transformer):
                      let expanded = try
                        self.checkpointer.expansion(cp) ??
                        self.context.machine.apply(.Proc(transformer), to: .Pair(cdr, .Null), in: env)
                      self.checkpointer.associate(.Expansion(expanded), with: cp)
                      log("expanded = \(expanded)")
                      return try self.compile(expanded, in: env, inTailPos: tail)
                  }
                default:
                  break // Compile as normal global function call
              }
            }
            // Push function from global binding
            self.emit(.PushGlobal(self.registerConstant(.Sym(lexicalSym))))
          case .MacroExpansionRequired(let transformer):
            let expanded = try
              self.checkpointer.expansion(cp) ??
              self.context.machine.apply(.Proc(transformer), to: .Pair(cdr, .Null), in: env)
            self.checkpointer.associate(.Expansion(expanded), with: cp)
            log("expanded = \(expanded)")
            return try self.compile(expanded, in: env, inTailPos: tail)
        }
        // Push arguments and call function
        if self.call(try self.compileExprs(cdr, in: env), tail) {
          // Remove MakeFrame if this was a tail call
          self.patch(.NoOp, at: pushFrameIp)
          return true
        }
      case .Pair(let car, let cdr):
        let pushFrameIp = self.emit(.MakeFrame)
        // Push function
        try self.compile(car, in: env, inTailPos: false)
        // Push arguments and call function
        if self.call(try self.compileExprs(cdr, in: env), tail) {
          // Remove MakeFrame if this was a tail call
          self.patch(.NoOp, at: pushFrameIp)
          return true
        }
      default:
        try self.pushValue(expr)
    }
    return false
  }
  
  /// Compile the sequence of expressions `expr` in environment `env`. Parameter `tail`
  /// specifies if `expr` is located in a tail position. This allows compile to generate
  /// code with tail calls.
  public func compileSeq(expr: Expr, in env: Env, inTailPos tail: Bool) throws -> Bool {
    var first = true
    var exit = false
    var next = expr
    while case .Pair(let car, let cdr) = next {
      if !first {
        self.emit(.Pop)
      }
      exit = try self.compile(car, in: env, inTailPos: tail && cdr.isNull)
      first = false
      next = cdr
    }
    guard next.isNull else {
      throw EvalError.TypeError(expr, [.ProperListType])
    }
    return exit
  }
  
  /// Compile the given list of expressions `expr` in environment `env` and push each result
  /// onto the stack. This method returns the number of expressions that were evaluated and
  /// whose result has been stored on the stack.
  public func compileExprs(expr: Expr, in env: Env) throws -> Int {
    var n = 0
    var next = expr
    while case .Pair(let car, let cdr) = next {
      try self.compile(car, in: env, inTailPos: false)
      n += 1
      next = cdr
    }
    guard next.isNull else {
      throw EvalError.TypeError(expr, [.ProperListType])
    }
    return n
  }
  
  /// Bundle the generated code into a `Code` object.
  public func bundle() -> Code {
    return Code(self.instructions, self.constants, self.fragments)
  }
}
