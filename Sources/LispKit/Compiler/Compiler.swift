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

import Foundation
import NumberKit

///
/// Class `Compiler` provides a framework for compiling LispKit expressions. Static method
/// `compile` invokes the compiler in a given environment and returns a `Code` object
/// which encapsulates the generated code.
/// 
public final class Compiler {
  
  /// Context in which this compiler is running
  public unowned let context: Context
  
  /// Environment in which expressions should be compiled
  internal var env: Env
  
  /// Meta-environment in which macro expressions are evaluated
  internal let rulesEnv: Env
  
  /// Checkpointer for attaching information to help optimize the code in the second
  /// compilation phase.
  internal let checkpointer: Checkpointer
  
  /// Capture list
  private var captures: CaptureGroup!
  
  /// Current number of local values/variables
  internal var numLocals: Int = 0
  
  /// Maximum number of local values/variables
  private var maxLocals: Int = 0
  
  /// List of arguments
  private var arguments: BindingGroup?
  
  /// Constant pool
  private var constants: Exprs = []
  
  /// List of code fragments
  private var fragments: Fragments = []
  
  /// Instruction sequence
  private var instructions: Instructions = []
  
  /// Directory of the current source file
  internal var sourceDirectory: String
  
  /// Parent compiler (needed since `Compiler` objects are nested, e.g. if nested
  /// functions get compiled)
  public var parent: Compiler? {
    return self.captures.parent?.owner
  }
  
  /// Initializes a compiler object from the given context, environments, and checkpointer.
  private init(in env: Env,
               and rulesEnv: Env,
               usingCheckpointer cp: Checkpointer) {
    self.context = env.environment!.context
    self.env = env
    self.rulesEnv = rulesEnv
    self.checkpointer = cp
    self.arguments = nil
    self.sourceDirectory = env.environment!.context.fileHandler.currentDirectoryPath
    self.captures = CaptureGroup(owner: self, parent: env.bindingGroup?.owner.captures)
  }
  
  /// Compiles the given expression `expr` in the environment `env` and the rules environment
  /// `rulesEnv`. If `optimize` is set to true, the compiler will be invoked twice. The
  /// information collected in the first phase will be used to optimize the code in the second
  /// phase.
  public static func compile(expr: Expr,
                             in env: Env,
                             and rulesEnv: Env? = nil,
                             optimize: Bool = false,
                             inDirectory: String? = nil) throws -> Code {
    let checkpointer = Checkpointer()
    var compiler = Compiler(in: env,
                            and: rulesEnv ?? env,
                            usingCheckpointer: checkpointer)
    if let dir = inDirectory {
      compiler.sourceDirectory = dir
    }
    try compiler.compileBody(expr)
    if optimize {
      log(checkpointer.description)
      checkpointer.reset()
      compiler = Compiler(in: env,
                          and: rulesEnv ?? env,
                          usingCheckpointer: checkpointer)
      if let dir = inDirectory {
        compiler.sourceDirectory = dir
      }
      try compiler.compileBody(expr)
      log(checkpointer.description)
    }
    return compiler.bundle()
  }
  
  /// Compiles the given list of arguments and returns the corresponding binding group as well
  /// as the remaining argument list
  private func collectArguments(_ arglist: Expr) -> (BindingGroup, Expr) {
    let arguments = BindingGroup(owner: self, parent: self.env)
    var next = arglist
    loop: while case .pair(let arg, let cdr) = next {
      switch arg {
        case .symbol(let sym):
          arguments.allocBindingFor(sym)
        default:
          break loop
      }
      next = cdr
    }
    return (arguments, next)
  }
  
  /// Compiles the given body of a function (or expression, if this compiler is not used to
  /// compile a function).
  private func compileBody(_ expr: Expr, localDefine: Bool = false) throws {
    if expr.isNull {
      self.emit(.pushVoid)
      self.emit(.return)
    } else {
      // Reserve instruction for reserving local variables
      let reserveLocalIp = self.emitPlaceholder()
      // Turn arguments into local variables
      if let arguments = self.arguments {
        for sym in arguments.symbols {
          if let sym = sym, let binding = arguments.bindingFor(sym) , !binding.isValue {
            self.emit(.makeVariableArgument(binding.index))
          }
        }
      }
      // Compile body
      if !(try compileSeq(expr, in: self.env, inTailPos: true, localDefine: localDefine)) {
        self.emit(.return)
      }
      // Insert instruction to reserve local variables
      if self.maxLocals > self.arguments?.count ?? 0 {
        self.patch(.alloc(self.maxLocals - (self.arguments?.count ?? 0)), at: reserveLocalIp)
      }
    }
    // Checkpoint argument mutability
    self.arguments?.finalize()
  }
  
  /// Allocates a new local value/variable.
  public func nextLocalIndex() -> Int {
    self.numLocals += 1
    if self.numLocals > self.maxLocals {
      self.maxLocals = self.numLocals
    }
    return self.numLocals - 1
  }
  
  /// Removes the last instruction from the instruction sequence.
  @discardableResult public func removeLastInstr() -> Int {
    self.instructions.removeLast()
    return self.instructions.count - 1
  }
  
  /// Appends the given instruction to the instruction sequence.
  @discardableResult public func emit(_ instr: Instruction) -> Int {
    self.instructions.append(instr)
    return self.instructions.count - 1
  }
  
  /// Replaces the instruction at the position `at` with the instruction `instr`.
  public func patch(_ instr: Instruction, at: Int) {
    self.instructions[at] = instr
  }
  
  /// Appends a placeholder instruction to the instruction sequence.
  @discardableResult public func emitPlaceholder() -> Int {
    return self.emit(.noOp)
  }
  
  /// Injects the name of a closure in form of an index into the constant pool into the
  /// `.makeClosure` operation.
  public func patchMakeClosure(_ nameIdx: Int) {
    if case .some(.makeClosure(-1, let n, let index)) = self.instructions.last {
      self.instructions[self.instructions.count - 1] = .makeClosure(nameIdx, n, index)
    }
  }
  
  /// Injects the name of a closure into the `.makeClosure` operation.
  public func patchMakeClosure(_ sym: Symbol) {
    if case .some(.makeClosure(-1, let n, let index)) = self.instructions.last {
      let nameIdx = self.registerConstant(.symbol(sym))
      self.instructions[self.instructions.count - 1] = .makeClosure(nameIdx, n, index)
    }
  }
  
  /// Calls a procedure on the stack with `n` arguments. Uses a tail call if `tail` is set
  /// to true.
  public func call(_ n: Int, inTailPos tail: Bool) -> Bool {
    if tail {
      self.emit(.tailCall(n))
      return true
    } else {
      self.emit(.call(n))
      return false
    }
  }
  
  /// Computes the offset between the next instruction and the given instruction pointer `ip`.
  public func offsetToNext(_ ip: Int) -> Int {
    return self.instructions.count - ip
  }
  
  /// Pushes the given expression onto the stack.
  public func pushConstant(_ expr: Expr) {
    self.emit(.pushConstant(self.registerConstant(expr)))
  }
  
  /// Attaches the given expression to the constant pool and returns the index into the constant
  /// pool. `registerConstant` makes sure that expressions are not added twice.
  public func registerConstant(_ expr: Expr) -> Int {
    for i in self.constants.indices {
      if eqExpr(expr, self.constants[i]) {
        return i
      }
    }
    self.constants.append(expr.datum)
    return self.constants.count - 1
  }
  
  /// Push the value of the given symbol in the given environment onto the stack.
  public func pushValueOf(_ sym: Symbol, in env: Env) throws {
    switch self.pushLocalValueOf(sym, in: env) {
      case .success:
        break // Nothing to do
      case .globalLookupRequired(let lexicalSym, let environment):
        let locRef = self.forceDefinedLocationRef(for: lexicalSym, in: environment)
        if case .immutableImport(let loc) = locRef {
          let value = self.context.heap.locations[loc]
          if value.isUndef {
            self.emit(.pushGlobal(locRef.location!))
          } else {
            try self.pushValue(value)
          }
        } else {
          self.emit(.pushGlobal(locRef.location!))
        }
      case .macroExpansionRequired(_):
        throw RuntimeError.eval(.illegalKeywordUsage, .symbol(sym))
    }
  }
  
  /// Result type of `pushLocalValueOf` method.
  public enum LocalLookupResult {
    
    /// `Success` indicates that a local value/variable was successfully pushed onto the stack
    case success
    
    /// `MacroExpansionRequired(proc)` indicates that the binding refers to a macro and the
    /// compiler needs to expand the expression with the macro expander procedure `proc`.
    case macroExpansionRequired(Procedure)
    
    /// `GlobalLookupRequired(gsym, genv)` indicates that a suitable binding wasn't found in the
    /// local environment and thus a lookup in the global environment `genv` needs to be made.
    /// Note that `gsym` and `sym` do not necessarily need to be the same due to the way how
    /// hygienic macro expansion is implemented.
    case globalLookupRequired(Symbol, Environment)
  }
  
  /// Pushes the value/variable bound to symbol `sym` in the local environment `env`. If this
  /// wasn't possible, the method returns an instruction on how to proceed.
  private func pushLocalValueOf(_ sym: Symbol, in env: Env) -> LocalLookupResult {
    var env = env
    // Iterate through the local binding groups until `sym` is found
    while case .local(let group) = env {
      if let binding = group.bindingFor(sym) {
        if case .macro(let proc) = binding.kind {
          return .macroExpansionRequired(proc)
        } else if group.owner === self {
          if binding.isValue {
            self.emit(.pushLocal(binding.index))
          } else {
            self.emit(.pushLocalValue(binding.index))
          }
        } else {
          let capturedIndex = self.captures.capture(binding, from: group)
          if binding.isValue {
            self.emit(.pushCaptured(capturedIndex))
          } else {
            self.emit(.pushCapturedValue(capturedIndex))
          }
        }
        return .success
      }
      env = group.parent
    }
    // If `sym` wasn't found, look into the lexical environment
    if let (lexicalSym, lexicalEnv) = sym.lexical {
      // If the lexical environment is a global environment, return that a global lookup is needed
      if case .global(_) = lexicalEnv {
        return .globalLookupRequired(sym, env.environment!)
      }
      // Find the new lexical symbol in the new lexcial environment
      let res = self.pushLocalValueOf(lexicalSym, in: lexicalEnv)
      // If this didn't succeed, return that a global lookup is needed
      guard case .globalLookupRequired(_, _) = res else {
        return res
      }
    }
    // Return global scope
    return .globalLookupRequired(sym, env.environment!)
  }
  
  /// Checks if the symbol `sym` is bound in the local environment `env`.
  private func lookupLocalValueOf(_ sym: Symbol, in env: Env) -> LocalLookupResult {
    var env = env
    // Iterate through the local binding groups until `sym` is found
    while case .local(let group) = env {
      if let binding = group.bindingFor(sym) {
        if case .macro(let proc) = binding.kind {
          return .macroExpansionRequired(proc)
        }
        return .success
      }
      env = group.parent
    }
    // If `sym` wasn't found, look into the lexical environment
    if let (lexicalSym, lexicalEnv) = sym.lexical {
      // If the lexical environment is a global environment, return that a global lookup is needed
      if case .global(_) = lexicalEnv {
        return .globalLookupRequired(sym, env.environment!)
      }
      // Find the new lexical symbol in the new lexcial environment
      let res = self.lookupLocalValueOf(lexicalSym, in: lexicalEnv)
      // If this didn't succeed, return that a global lookup is needed
      guard case .globalLookupRequired(_, _) = res else {
        return res
      }
    }
    // Return global scope
    return .globalLookupRequired(sym, env.environment!)
  }
  
  /// Generates instructions to push the given expression onto the stack.
  public func pushValue(_ value: Expr) throws {
    let expr = value.datum
    switch expr {
      case .undef, .uninit(_):
        self.emit(.pushUndef)
      case .void:
        self.emit(.pushVoid)
      case .eof:
        self.emit(.pushEof)
      case .null:
        self.emit(.pushNull)
      case .true:
        self.emit(.pushTrue)
      case .false:
        self.emit(.pushFalse)
      case .fixnum(let num):
        self.emit(.pushFixnum(num))
      case .bignum(let num):
        self.emit(.pushBignum(num))
      case .rational(.fixnum(let n), .fixnum(let d)):
        self.emit(.pushRat(Rational(n, d)))
      case .rational(.bignum(let n), .bignum(let d)):
        self.emit(.pushBigrat(Rational(n, d)))
      case .rational(_, _):
        preconditionFailure("incorrectly encoded rational number")
      case .flonum(let num):
        self.emit(.pushFlonum(num))
      case .complex(let num):
        self.emit(.pushComplex(num.value))
      case .char(let char):
        self.emit(.pushChar(char))
      case .symbol(_), .string(_), .bytes(_), .pair(_, _), .box(_), .mpair(_),
           .vector(_), .record(_), .table(_), .promise(_), .procedure(_), .env(_),
           .port(_), .object(_), .tagged(_, _), .error(_):
        self.pushConstant(expr)
      case .special(_):
        throw RuntimeError.eval(.illegalKeywordUsage, expr)
      case .values(_):
        preconditionFailure("cannot push multiple values onto stack")
      case .syntax(_, _):
        preconditionFailure("cannot push syntax onto stack")
    }
  }
  
  /// Bind symbol `sym` to the value on top of the stack in environment `env`.
  public func setValueOf(_ sym: Symbol, in env: Env) {
    switch self.setLocalValueOf(sym, in: env) {
      case .success:
        break; // Nothing to do
      case .globalLookupRequired(let lexicalSym, let environment):
        let loc = self.forceDefinedLocationRef(for: lexicalSym, in: environment).location!
        self.emit(.setGlobal(loc))
      case .macroExpansionRequired(_):
        preconditionFailure("setting bindings should never trigger macro expansion")
    }
  }
  
  /// Bind symbol `sym` to the value on top of the stack assuming `lenv` is a local
  /// environment (i.e. the bindings are located on the stack). If this
  /// wasn't possible, the method returns an instruction on how to proceed.
  private func setLocalValueOf(_ sym: Symbol, in lenv: Env) -> LocalLookupResult {
    var env = lenv
    // Iterate through the local binding groups until `sym` is found
    while case .local(let group) = env {
      if let binding = group.bindingFor(sym) {
        if group.owner === self {
          self.emit(.setLocalValue(binding.index))
        } else {
          self.emit(.setCapturedValue(self.captures.capture(binding, from: group)))
        }
        binding.wasMutated()
        return .success
      }
      env = group.parent
    }
    // If `sym` wasn't found, look into the lexical environment
    if let (lexicalSym, lexicalEnv) = sym.lexical {
      // If the lexical environment is a global environment, return that a global lookup is needed
      if case .global(_) = lexicalEnv {
        return .globalLookupRequired(sym, env.environment!)
      }
      // Find the new lexical symbol in the new lexcial environment
      let res = self.setLocalValueOf(lexicalSym, in: lexicalEnv)
      // If this didn't succeed, return that a global lookup is needed
      guard case .globalLookupRequired(_, _) = res else {
        return res
      }
    }
    return .globalLookupRequired(sym, env.environment!)
  }
  
  private func locationRef(for sym: Symbol,
                           in environment: Environment) -> Environment.LocationRef {
    var environment = environment
    var res = environment.locationRef(for: sym)
    var sym = sym
    while case .undefined = res, let (lexicalSym, lexicalEnv) = sym.lexical {
      sym = lexicalSym
      environment = lexicalEnv.environment!
      res = environment.locationRef(for: sym)
    }
    return res
  }
  
  private func forceDefinedLocationRef(for sym: Symbol,
                                       in environment: Environment) -> Environment.LocationRef {
    var environment = environment
    var res = environment.locationRef(for: sym)
    var sym = sym
    while case .undefined = res, let (lexicalSym, lexicalEnv) = sym.lexical {
      sym = lexicalSym
      environment = lexicalEnv.environment!
      res = environment.locationRef(for: sym)
    }
    if case .undefined = res {
      return environment.forceDefinedLocationRef(for: sym)
    }
    return res
  }
  
  private func value(of sym: Symbol, in environment: Environment) -> Expr? {
    var environment = environment
    var res = environment[sym]
    var sym = sym
    while res == nil, let (lexicalSym, lexicalEnv) = sym.lexical {
      sym = lexicalSym
      environment = lexicalEnv.environment!
      res = environment[sym]
    }
    return res
  }
  
  /// Expand expression `expr` in environment `env`.
  private func expand(_ expr: Expr, in env: Env) throws -> Expr? {
    switch expr {
      case .pair(.symbol(let sym), let cdr):
        let cp = self.checkpointer.checkpoint()
        switch self.lookupLocalValueOf(sym, in: env) {
          case .success:
            return nil
          case .globalLookupRequired(let lexicalSym, let environment):
            if let value = self.checkpointer.fromGlobalEnv(cp) ??
                           value(of: lexicalSym, in: environment) {
              self.checkpointer.associate(.fromGlobalEnv(value), with: cp)
              switch value {
                case .special(let special):
                  switch special.kind {
                    case .primitive(_):
                      return nil
                    case .macro(let transformer):
                      let expanded = try
                        self.checkpointer.expansion(cp) ??
                        self.context.machine.apply(.procedure(transformer), to: .pair(cdr, .null))
                      self.checkpointer.associate(.expansion(expanded), with: cp)
                      log("expanded = \(expanded)")
                      return expanded
                  }
                default:
                  return nil
              }
            } else {
              return nil
            }
          case .macroExpansionRequired(let transformer):
            let expanded =
              try self.context.machine.apply(.procedure(transformer), to: .pair(cdr, .null))
            log("expanded = \(expanded)")
            return expanded
        }
      default:
        return nil
    }
  }
  
  /// Expand expression `expr` in environment `env`.
  private func expand(_ expr: Expr, in env: Env, into: inout Exprs, depth: Int = 20) throws {
    guard depth > 0 else {
      into.append(expr)
      return
    }
    if case .pair(.symbol(let fun), let embedded) = expr,
       fun.interned == self.context.symbols.begin,
       env.isImmutable(fun) {
      var lst = embedded
      while case .pair(let e, let next) = lst {
        try self.expand(e, in: env, into: &into, depth: depth)
        lst = next
      }
    } else if let expanded = try self.expand(expr, in: env) {
      try self.expand(expanded, in: env, into: &into, depth: depth - 1)
    } else {
      into.append(expr)
    }
  }
  
  /// Compile expression `expr` in environment `env`. Parameter `tail` specifies if `expr`
  /// is located in a tail position. This allows compile to generate code with tail calls.
  @discardableResult public func compile(_ expr: Expr,
                                         in env: Env,
                                         inTailPos tail: Bool) throws -> Bool {
    let cp = self.checkpointer.checkpoint()
    switch expr {
      case .symbol(let sym):
        try self.pushValueOf(sym, in: env)
      case .pair(.symbol(let sym), let cdr):
        let pushFrameIp = self.emit(.makeFrame)
        // Try to push the function if locally defined
        switch self.pushLocalValueOf(sym, in: env) {
          case .success:
            break // Nothing to do
          case .globalLookupRequired(let lexicalSym, let environment):
            // Is there a special compiler plugin for this global binding, or is this a
            // keyword/special form)?
            if let value = self.checkpointer.fromGlobalEnv(cp) ??
                           self.value(of: lexicalSym, in: environment) {
              self.checkpointer.associate(.fromGlobalEnv(value), with: cp)
              switch value {
                case .procedure(let proc):
                  if case .primitive(_, _, .some(let formCompiler)) = proc.kind,
                     self.checkpointer.systemDefined(cp) || environment.isImmutable(lexicalSym) {
                    self.checkpointer.associate(.systemDefined, with: cp)
                    self.removeLastInstr()
                    return try formCompiler(self, expr, env, tail)
                  }
                case .special(let special):
                  self.removeLastInstr()
                  switch special.kind {
                    case .primitive(let formCompiler):
                      return try formCompiler(self, expr, env, tail)
                    case .macro(let transformer):
                      let expanded = try
                        self.checkpointer.expansion(cp) ??
                        self.context.machine.apply(.procedure(transformer), to: .pair(cdr, .null))
                      self.checkpointer.associate(.expansion(expanded), with: cp)
                      log("expanded = \(expanded)")
                      return try self.compile(expanded, in: env, inTailPos: tail)
                  }
                default:
                  break // Compile as normal global function call
              }
            }
            // Push function from global binding
            let locRef = self.forceDefinedLocationRef(for: lexicalSym, in: environment)
            if case .immutableImport(let loc) = locRef {
              let value = self.context.heap.locations[loc]
              if value.isUndef {
                self.emit(.pushGlobal(loc))
              } else {
                try self.pushValue(value)
              }
            } else {
              self.emit(.pushGlobal(locRef.location!))
            }
          case .macroExpansionRequired(let transformer):
            let expanded =
              try self.context.machine.apply(.procedure(transformer), to: .pair(cdr, .null))
            log("expanded = \(expanded)")
            return try self.compile(expanded, in: env, inTailPos: tail)
        }
        // Push arguments and call function
        if self.call(try self.compileExprs(cdr, in: env), inTailPos: tail) {
          // Remove MakeFrame if this was a tail call
          self.patch(.noOp, at: pushFrameIp)
          return true
        }
      case .pair(let car, let cdr):
        let pushFrameIp = self.emit(.makeFrame)
        // Push function
        try self.compile(car, in: env, inTailPos: false)
        // Push arguments and call function
        if self.call(try self.compileExprs(cdr, in: env), inTailPos: tail) {
          // Remove MakeFrame if this was a tail call
          self.patch(.noOp, at: pushFrameIp)
          return true
        }
      default:
        try self.pushValue(expr)
    }
    return false
  }
  
  /// Compile the given list of expressions `expr` in environment `env` and push each result
  /// onto the stack. This method returns the number of expressions that were evaluated and
  /// whose result has been stored on the stack.
  public func compileExprs(_ expr: Expr, in env: Env) throws -> Int {
    var n = 0
    var next = expr
    while case .pair(let car, let cdr) = next {
      try self.compile(car, in: env, inTailPos: false)
      n += 1
      next = cdr
    }
    guard next.isNull else {
      throw RuntimeError.type(expr, expected: [.properListType])
    }
    return n
  }
  
  /// Compile the sequence of expressions `expr` in environment `env`. Parameter `tail`
  /// specifies if `expr` is located in a tail position. This allows the compiler to generate
  /// code with tail calls.
  @discardableResult public func compileSeq(_ expr: Expr,
                                            in env: Env,
                                            inTailPos tail: Bool,
                                            localDefine: Bool = true,
                                            inDirectory: String? = nil) throws -> Bool {
    // Return void for empty sequences
    guard !expr.isNull else {
      self.emit(.pushVoid)
      return false
    }
    // Override the source directory if the code comes from a different file
    var oldDir = self.sourceDirectory
    if let dir = inDirectory {
      self.sourceDirectory = dir
    }
    defer {
      self.sourceDirectory = oldDir
    }
    // Partially expand expressions in the sequence
    var next = expr
    var exprs = Exprs()
    while case .pair(let car, let cdr) = next {
      if localDefine {
        try self.expand(car, in: env, into: &exprs)
      } else {
        exprs.append(car)
      }
      next = cdr
    }
    // Throw error if the sequence is not a proper list
    guard next.isNull else {
      throw RuntimeError.type(expr, expected: [.properListType])
    }
    // Identify internal definitions
    var i = 0
    var bindings = Exprs()
    if localDefine {
      loop: while i < exprs.count {
        guard case .pair(.symbol(let fun), let binding) = exprs[i],
              fun.interned == self.context.symbols.define,
              env.isImmutable(fun) else {
          break loop
        }
        // Distinguish value definitions from function definitions
        switch binding {
          case .pair(.symbol(let sym), .pair(let def, .null)):
            bindings.append(.pair(.symbol(sym), .pair(def, .null)))
          case .pair(.pair(.symbol(let sym), let args), let def):
            bindings.append(
              .pair(.symbol(sym),
                    .pair(.pair(.symbol(Symbol(self.context.symbols.lambda, env.global)),
                                .pair(args, def)),
                          .null)))
          default:
            break loop
        }
        i += 1
      }
    }
    // Compile the sequence
    var exit = false
    if i == 0 {
      // Compilation with no internal definitions
      while i < exprs.count {
        if i > 0 {
          self.emit(.pop)
        }
        exit = try self.compile(exprs[i], in: env, inTailPos: tail && (i == exprs.count - 1))
        i += 1
      }
      return exit
    } else {
      // Compilation with internal definitions
      let initialLocals = self.numLocals
      let group = try self.compileBindings(.makeList(bindings), in: env, atomic: true, predef: true)
      let lenv = Env(group)
      var first = true
      while i < exprs.count {
        if !first {
          self.emit(.pop)
        }
        exit = try self.compile(exprs[i], in: lenv, inTailPos: tail && (i == exprs.count - 1))
        first = false
        i += 1
      }
      // Push void in case there is no non-define expression left
      if first {
        self.emit(.pushVoid)
      }
      return self.finalizeBindings(group, exit: exit, initialLocals: initialLocals)
    }
  }
  
  /// Compiles the given binding list of the form
  /// ```((ident init) ...)```
  /// and returns a `BindingGroup` with information about the established local bindings.
  public func compileBindings(_ bindingList: Expr,
                              in lenv: Env,
                              atomic: Bool,
                              predef: Bool,
                              postset: Bool = false) throws -> BindingGroup {
    let group = BindingGroup(owner: self, parent: lenv)
    let env = atomic && !predef ? lenv : .local(group)
    var bindings = bindingList
    if predef || postset {
      while case .pair(.pair(.symbol(let sym), _), let rest) = bindings {
        let binding = group.allocBindingFor(sym)
        // This is a hack for now; we need to make sure forward references work, e.g. in
        // lambda expressions. A way to do this is to allocate variables for all bindings that
        // are predefined.
        binding.wasMutated()
        self.emit(.pushUndef)
        self.emit(binding.isValue ? .setLocal(binding.index) : .makeLocalVariable(binding.index))
        bindings = rest
      }
      bindings = bindingList
    }
    var definitions: [Definition] = []
    var prevIndex = -1
    while case .pair(let binding, let rest) = bindings {
      guard case .pair(.symbol(let sym), .pair(let expr, .null)) = binding else {
        throw RuntimeError.eval(.malformedBinding, binding, bindingList)
      }
      try self.compile(expr, in: env, inTailPos: false)
      self.patchMakeClosure(sym)
      let binding = group.allocBindingFor(sym)
      guard binding.index > prevIndex else {
        throw RuntimeError.eval(.duplicateBinding, .symbol(sym), bindingList)
      }
      if postset {
        definitions.append(binding)
      } else if binding.isValue {
        self.emit(.setLocal(binding.index))
      } else if predef {
        self.emit(.setLocalValue(binding.index))
      } else {
        self.emit(.makeLocalVariable(binding.index))
      }
      prevIndex = binding.index
      bindings = rest
    }
    guard bindings.isNull else {
      throw RuntimeError.eval(.malformedBindings, bindingList)
    }
    for binding in definitions.reversed() {
      if binding.isValue {
        self.emit(.setLocal(binding.index))
      } else {
        self.emit(.setLocalValue(binding.index))
      }
    }
    return group
  }
  
  /// Compiles the given binding list of the form
  /// ```(((ident ...) init) ...)```
  /// and returns a `BindingGroup` with information about the established local bindings.
  public func compileMultiBindings(_ bindingList: Expr,
                                   in lenv: Env,
                                   atomic: Bool) throws -> BindingGroup {
    let group = BindingGroup(owner: self, parent: lenv)
    let env = atomic ? lenv : .local(group)
    var bindings = bindingList
    var prevIndex = -1
    while case .pair(let binding, let rest) = bindings {
      guard case .pair(let variables, .pair(let expr, .null)) = binding else {
        throw RuntimeError.eval(.malformedBinding, binding, bindingList)
      }
      try self.compile(expr, in: env, inTailPos: false)
      var vars = variables
      var syms = [Symbol]()
      while case .pair(.symbol(let sym), let rest) = vars {
        syms.append(sym)
        vars = rest
      }
      switch vars {
        case .null:
          self.emit(.unpack(syms.count, false))
        case .symbol(let sym):
          self.emit(.unpack(syms.count, true))
          let binding = group.allocBindingFor(sym)
          if binding.isValue {
            self.emit(.setLocal(binding.index))
          } else {
            self.emit(.makeLocalVariable(binding.index))
          }
          prevIndex = binding.index
        default:
          throw RuntimeError.eval(.malformedBinding, binding, bindingList)
      }
      for sym in syms.reversed() {
        let binding = group.allocBindingFor(sym)
        guard binding.index > prevIndex else {
          throw RuntimeError.eval(.duplicateBinding, .symbol(sym), bindingList)
        }
        if binding.isValue {
          self.emit(.setLocal(binding.index))
        } else {
          self.emit(.makeLocalVariable(binding.index))
        }
        prevIndex = binding.index
      }
      bindings = rest
    }
    guard bindings.isNull else {
      throw RuntimeError.eval(.malformedBindings, bindingList)
    }
    return group
  }
  
  /// This function should be used for finalizing the compilation of blocks with local
  /// bindings. It finalizes the binding group and resets the local bindings so that the
  /// garbage collector can deallocate the objects that are not used anymore.
  public func finalizeBindings(_ group: BindingGroup, exit: Bool, initialLocals: Int) -> Bool {
    group.finalize()
    if !exit && self.numLocals > initialLocals {
      self.emit(.reset(initialLocals, self.numLocals - initialLocals))
    }
    self.numLocals = initialLocals
    return exit
  }
  
  /// Binds a list of keywords to macro transformers in the given local environment `lenv`.
  /// If `recursive` is set to true, the macro transformers are evaluated in an environment that
  /// includes their own definition.
  public func compileMacros(_ bindingList: Expr,
                            in lenv: Env,
                            recursive: Bool) throws -> BindingGroup {
    var numMacros = 0
    let group = BindingGroup(owner: self, parent: lenv, nextIndex: {
      numMacros += 1
      return numMacros - 1
    })
    let env = recursive ? Env(group) : lenv
    var bindings = bindingList
    while case .pair(let binding, let rest) = bindings {
      guard case .pair(.symbol(let sym), .pair(let transformer, .null)) = binding else {
        throw RuntimeError.eval(.malformedBinding, binding, bindingList)
      }
      let procExpr = try self.context.machine.compileAndEval(expr: transformer,
                                                             in: env.syntacticalEnv,
                                                             usingRulesEnv: env)
      guard case .procedure(let proc) = procExpr else {
        throw RuntimeError.eval(.malformedTransformer, transformer) //FIXME: Find better error message
      }
      guard group.bindingFor(sym) == nil else {
        throw RuntimeError.eval(.duplicateBinding, .symbol(sym), bindingList)
      }
      group.defineMacro(sym, proc: proc)
      bindings = rest
    }
    guard bindings.isNull else {
      throw RuntimeError.eval(.malformedBindings, bindingList)
    }
    return group
  }
  
  /// Compiles a closure consisting of a list of formal arguments `arglist`, a list of
  /// expressions `body`, and a local environment `env`. It puts the closure on top of the
  /// stack.
  public func compileLambda(_ nameIdx: Int?,
                            _ arglist: Expr,
                            _ body: Expr,
                            _ env: Env,
                            continuation: Bool = false) throws {
    // Create closure compiler as child of the current compiler
    let closureCompiler = Compiler(in: env,
                                   and: env,
                                   usingCheckpointer: self.checkpointer)
    // Compile arguments
    let (arguments, next) = closureCompiler.collectArguments(arglist)
    switch next {
      case .null:
        closureCompiler.emit(.assertArgCount(arguments.count))
      case .symbol(let sym):
        if arguments.count > 0 {
          closureCompiler.emit(.assertMinArgCount(arguments.count))
        }
        closureCompiler.emit(.collectRest(arguments.count))
        arguments.allocBindingFor(sym)
      default:
        throw RuntimeError.eval(.malformedArgumentList, arglist)
    }
    closureCompiler.arguments = arguments
    closureCompiler.env = .local(arguments)
    // Compile body
    try closureCompiler.compileBody(body, localDefine: true)
    // Link compiled closure in the current compiler
    let codeIndex = self.fragments.count
    let code = closureCompiler.bundle()
    self.fragments.append(code)
    // Generate code for pushing captured bindings onto the stack
    for def in closureCompiler.captures.definitions {
      if let def = def, let capture = closureCompiler.captures.captureFor(def) {
        if capture.origin.owner === self {
          self.emit(.pushLocal(def.index))
        } else {
          self.emit(.pushCaptured(self.captures.capture(def, from: capture.origin)))
        }
      }
    }
    // Return captured binding count and index of compiled closure
    self.emit(.makeClosure(nameIdx ?? (continuation ? -2 : -1),
                           closureCompiler.captures.count,
                           codeIndex))
  }
  
  /// Compiles a closure consisting of a list of formal arguments `arglist`, a list of
  /// expressions `body`, and a local environment `env`. It puts the closure on top of the
  /// stack.
  public func compileCaseLambda(_ nameIdx: Int?, _ cases: Expr, _ env: Env) throws {
    // Create closure compiler as child of the current compiler
    let closureCompiler = Compiler(in: env,
                                   and: env,
                                   usingCheckpointer: self.checkpointer)
    // Iterate through all cases
    var current = cases
    loop: while case .pair(.pair(let args, let body), let nextCase) = current {
      // Reset compiler
      closureCompiler.env = env
      closureCompiler.numLocals = 0
      closureCompiler.maxLocals = 0
      closureCompiler.arguments = nil
      // Compile arguments
      let (arguments, next) = closureCompiler.collectArguments(args)
      var exactIp = -1
      var minIp = -1
      let numArgs = arguments.count
      switch next {
        case .null:
          exactIp = closureCompiler.emitPlaceholder()
        case .symbol(let sym):
          if arguments.count > 0 {
            minIp = closureCompiler.emitPlaceholder()
          }
          closureCompiler.emit(.collectRest(arguments.count))
          arguments.allocBindingFor(sym)
        default:
          throw RuntimeError.eval(.malformedArgumentList, args)
      }
      closureCompiler.arguments = arguments
      closureCompiler.env = .local(arguments)
      // Compile body
      try closureCompiler.compileBody(body)
      // Fix jumps
      if exactIp >= 0 {
        closureCompiler.patch(
          .branchIfArgMismatch(numArgs, closureCompiler.offsetToNext(exactIp)), at: exactIp)
      } else if minIp >= 0 {
        closureCompiler.patch(
          .branchIfMinArgMismatch(numArgs, closureCompiler.offsetToNext(minIp)), at: minIp)
      } else {
        break loop
      }
      // Move to next case
      current = nextCase
    }
    // Compile final "else" case
    switch current {
      case .pair(.pair(_, _), _):
        break // early exit
      case .null:
        closureCompiler.emit(.noMatchingArgCount)
      default:
        throw RuntimeError.eval(.malformedCaseLambda, current)
    }
    // Link compiled closure in the current compiler
    let codeIndex = self.fragments.count
    let code = closureCompiler.bundle()
    self.fragments.append(code)
    // Generate code for pushing captured bindings onto the stack
    for def in closureCompiler.captures.definitions {
      if let def = def, let capture = closureCompiler.captures.captureFor(def) {
        if capture.origin.owner === self {
          self.emit(.pushLocal(def.index))
        } else {
          self.emit(.pushCaptured(self.captures.capture(def, from: capture.origin)))
        }
      }
    }
    // Return captured binding count and index of compiled closure
    self.emit(.makeClosure(nameIdx ?? -1,
                           closureCompiler.captures.count,
                           codeIndex))
  }
  
  /// Bundles the code generated by this compiler into a `Code` object.
  public func bundle() -> Code {
    // Performce peephole optimization
    self.optimize()
    // Create code object
    return Code(self.instructions, self.constants, self.fragments)
  }
  
  /// This is just a placeholder for now. Will add a peephole optimizer eventually. For now,
  /// only NOOPs at the beginning of a code block are removed.
  private func optimize() {
    var ip = 0
    switch self.instructions[ip] {
      case .assertArgCount(_), .assertMinArgCount(_):
        ip += 1
      default:
        break
    }
    self.eliminateNoOpsAt(ip)
    if case .makeVariableArgument(_) = self.instructions[ip] {
      ip += 1
    }
    self.eliminateNoOpsAt(ip)
  }
  
  /// Remove sequences of NoOp at instruction position `ip`.
  private func eliminateNoOpsAt(_ ip: Int) {
    while ip < self.instructions.count {
      guard case .noOp = self.instructions[ip] else {
        return
      }
      self.instructions.remove(at: ip)
    }
  }
}
