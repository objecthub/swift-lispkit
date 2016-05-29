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
/// Class `Compiler` provides a framework for compiling LispKit expressions. Static method
/// `compile` invokes the compiler in a given environment and returns a `Code` object
/// which encapsulates the generated code.
/// 
public final class Compiler {
  
  /// Context of the compiler
  public let context: Context
  
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
  private var constants: [Expr] = []
  
  /// List of code fragments
  private var fragments: [Code] = []
  
  /// Instruction sequence
  private var instructions: [Instruction] = []
  
  /// Returns the parent compiler (since `Compiler` objects are nested, e.g. if nested
  /// functions get compiled)
  public var parent: Compiler? {
    return self.captures.parent?.owner
  }
  
  /// Initializes a compiler object from the given context, environments, and checkpointer.
  private init(_ context: Context, _ env: Env, _ rulesEnv: Env, _ cp: Checkpointer? = nil) {
    self.context = context
    self.env = env
    self.rulesEnv = rulesEnv
    self.checkpointer = cp ?? env.bindingGroup?.owner.checkpointer ?? Checkpointer()
    self.captures = CaptureGroup(owner: self, parent: env.bindingGroup?.owner.captures)
    self.arguments = nil
  }

  /// Compiles the given expression `expr` in the environment `env` and the rules environment
  /// `rulesEnv`. If `optimize` is set to true, the compiler will be invoked twice. The
  /// information collected in the first phase will be used to optimize the code in the second
  /// phase.
  public static func compile(context: Context,
                             expr: Expr,
                             in env: Env = .Interaction,
                             and rulesEnv: Env? = nil,
                             optimize: Bool = false) throws -> Code {
    let checkpointer = Checkpointer()
    var compiler = Compiler(context, env, rulesEnv ?? env, checkpointer)
    try compiler.compileBody(expr)
    if optimize {
      log(checkpointer.description)
      checkpointer.reset()
      compiler = Compiler(context, env, rulesEnv ?? env, checkpointer)
      try compiler.compileBody(expr)
      log(checkpointer.description)
    }
    return compiler.bundle()
  }
  
  /// Compiles the given list of arguments (if this `Compiler` object is used for compiling
  /// a function).
  private func compileArgList(arglist: Expr) throws {
    let arguments = BindingGroup(owner: self, parent: self.env)
    var next = arglist
    loop: while case .Pair(let arg, let cdr) = next {
      switch arg {
        case .Sym(let sym):
          arguments.allocBindingFor(sym)
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
          self.emit(.AssertMinArgCount(arguments.count))
        }
        self.emit(.CollectRest(arguments.count))
        arguments.allocBindingFor(sym)
      default:
        throw EvalError.MalformedArgumentList(arglist)
    }
    self.arguments = arguments
    self.env = .Local(arguments)
  }
  
  /// Compiles the given body of a function (or expression, if this compiler is not used to
  /// compile a function).
  private func compileBody(expr: Expr) throws {
    if expr.isNull {
      self.emit(.PushVoid)
      self.emit(.Return)
    } else {
      // Reserve instruction for reserving local variables
      let reserveLocalIp = self.emitPlaceholder()
      // Turn arguments into local variables
      if let arguments = self.arguments {
        for sym in arguments.symbols {
          if let sym = sym, binding = arguments.bindingFor(sym) where !binding.isValue {
            self.emit(.MakeVariableArgument(binding.index))
          }
        }
      }
      // Compile body
      if !(try compileSeq(expr, in: self.env, inTailPos: true, localDefine: false)) {
        self.emit(.Return)
      }
      // Insert instruction to reserve local variables
      if self.maxLocals > self.arguments?.count ?? 0 {
        self.patch(.Alloc(self.maxLocals - (self.arguments?.count ?? 0)), at: reserveLocalIp)
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
  public func removeLastInstr() -> Int {
    self.instructions.removeLast()
    return self.instructions.count - 1
  }
  
  /// Appends the given instruction to the instruction sequence.
  public func emit(instr: Instruction) -> Int {
    self.instructions.append(instr)
    return self.instructions.count - 1
  }
  
  /// Replaces the instruction at the position `at` with the instruction `instr`.
  public func patch(instr: Instruction, at: Int) {
    self.instructions[at] = instr
  }
  
  /// Appends a placeholder instruction to the instruction sequence.
  public func emitPlaceholder() -> Int {
    return self.emit(.NoOp)
  }
  
  /// Calls a procedure on the stack with `n` arguments. Uses a tail call if `tail` is set
  /// to true.
  public func call(n: Int, _ tail: Bool) -> Bool {
    if tail {
      self.emit(.TailCall(n))
      return true
    } else {
      self.emit(.Call(n))
      return false
    }
  }
  
  /// Computes the offset between the next instruction and the given instruction pointer `ip`.
  public func offsetToNext(ip: Int) -> Int {
    return self.instructions.count - ip
  }
  
  /// Pushes the given expression onto the stack.
  public func pushConstant(expr: Expr) {
    self.emit(.PushConstant(self.registerConstant(expr)))
  }
  
  /// Attaches the given expression to the constant pool and returns the index into the constant
  /// pool. `registerConstant` makes sure that expressions are not added twice.
  public func registerConstant(expr: Expr) -> Int {
    for i in self.constants.indices {
      if self.constants[i] == expr {
        return i
      }
    }
    self.constants.append(expr.datum)
    return self.constants.count - 1
  }
  
  /// Push the value of the given symbol in the given environment onto the stack.
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
  
  /// Result type of `pushLocalValueOf` method.
  public enum LocalLookupResult {
    
    /// `Success` indicates that a local value/variable was successfully pushed onto the stack
    case Success
    
    /// `MacroExpansionRequired(proc)` indicates that the binding refers to a macro and the
    /// compiler needs to expand the expression with the macro expander procedure `proc`.
    case MacroExpansionRequired(Procedure)
    
    /// `GlobalLookupRequired(gsym, genv)` indicates that a suitable binding wasn't found in the
    /// local environment and thus a lookup in the global environment `genv` needs to be made via
    /// symbol `gsym`. Note that `gsym` and `sym` do not necessarily need to be the same due to
    /// the way how hygienic macro expansion is implemented.
    case GlobalLookupRequired(Symbol, Env)
  }
  
  /// Pushes the value/variable bound to symbol `sym` in the local environment `env`. If this
  /// wasn't possible, the method returns an instruction on how to proceed.
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
  
  /// Pushes the value/variable bound to symbol `sym` in the local environment `env`. If this
  /// wasn't possible, the method returns an instruction on how to proceed.
  public func lookupLocalValueOf(sym: Symbol, in env: Env) -> LocalLookupResult {
    var env = env
    // Iterate through the local binding groups until `sym` is found
    while case .Local(let group) = env {
      if let binding = group.bindingFor(sym) {
        if case .Macro(let proc) = binding.kind {
          return .MacroExpansionRequired(proc)
        }
        return .Success
      }
      env = group.parent
    }
    // If `sym` wasn't found, look into the lexical environment
    if let (lexicalSym, lexicalEnv) = sym.lexical {
      return self.lookupLocalValueOf(lexicalSym, in: lexicalEnv)
    }
    // Return global scope
    return .GlobalLookupRequired(sym, env)
  }
  
  /// Generates instructions to push the given expression onto the stack.
  public func pushValue(expr: Expr) throws {
    switch expr {
      case .Undef:
        self.emit(.PushUndef)
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
        preconditionFailure("cannot push variables as values")
    }
  }
  
  /// Bind symbol `sym` to the value on top of the stack in environment `env`.
  public func setValueOf(sym: Symbol, in env: Env) {
    switch self.setLocalValueOf(sym, in: env) {
      case .Success:
        break; // Nothing to do
      case .GlobalLookupRequired(let lexicalSym, _):
        self.emit(.SetGlobal(self.registerConstant(.Sym(lexicalSym))))
      case .MacroExpansionRequired(_):
        preconditionFailure("setting bindings should never trigger macro expansion")
    }
  }
  
  /// Bind symbol `sym` to the value on top of the stack assuming `lenv` is a local
  /// environment (i.e. the bindings are located on the stack). If this
  /// wasn't possible, the method returns an instruction on how to proceed.
  public func setLocalValueOf(sym: Symbol, in lenv: Env) -> LocalLookupResult {
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
        return .Success
      }
      env = group.parent
    }
    // If `sym` wasn't found, look into the lexical environment
    if let (lexicalSym, lexicalEnv) = sym.lexical {
      return self.setLocalValueOf(lexicalSym, in: lexicalEnv)
    }
    return .GlobalLookupRequired(sym, lenv)
  }
  
  /// Compile expression `expr` in environment `env`. Parameter `tail` specifies if `expr`
  /// is located in a tail position. This allows compile to generate code with tail calls.
  public func expand(expr: Expr, in env: Env) throws -> Expr {
    switch expr {
      case .Pair(.Sym(let sym), let cdr):
        let cp = self.checkpointer.checkpoint()
        switch self.lookupLocalValueOf(sym, in: env) {
          case .Success:
            return expr
          case .GlobalLookupRequired(let lexicalSym, let global):
            if let value = self.checkpointer.fromGlobalEnv(cp) ??
                           global.scope(self.context)[lexicalSym] {
              self.checkpointer.associate(.FromGlobalEnv(value), with: cp)
              switch value {
                case .Special(let special):
                  switch special.kind {
                    case .Primitive(_):
                      return expr
                    case .Macro(let transformer):
                      let expanded = try
                        self.checkpointer.expansion(cp) ??
                        self.context.machine.apply(.Proc(transformer), to: .Pair(cdr, .Null), in: env)
                      self.checkpointer.associate(.Expansion(expanded), with: cp)
                      log("expanded = \(expanded)")
                      return expanded
                  }
                default:
                  return expr
              }
            } else {
              return expr
            }
          case .MacroExpansionRequired(let transformer):
            let expanded =
              try self.context.machine.apply(.Proc(transformer), to: .Pair(cdr, .Null), in: env)
            log("expanded = \(expanded)")
            return expanded
        }
      default:
        return expr
    }
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
            let expanded =
              try self.context.machine.apply(.Proc(transformer), to: .Pair(cdr, .Null), in: env)
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
  
  /// Compile the sequence of expressions `expr` in environment `env`. Parameter `tail`
  /// specifies if `expr` is located in a tail position. This allows the compiler to generate
  /// code with tail calls.
  public func compileSeq(expr: Expr,
                         in env: Env, inTailPos tail: Bool,
                         localDefine: Bool = true) throws -> Bool {
    // Return void for empty sequences
    guard !expr.isNull else {
      self.emit(.PushVoid)
      return false
    }
    // Partially expand expressions in the sequence
    var next = expr
    var exprs = Exprs()
    while case .Pair(let car, let cdr) = next {
      exprs.append(localDefine ? try self.expand(car, in: env) : car)
      next = cdr
    }
    // Throw error if the sequence is not a proper list
    guard next.isNull else {
      throw EvalError.TypeError(expr, [.ProperListType])
    }
    // Identify internal definitions
    var i = 0
    var bindings = Exprs()
    if localDefine {
      loop: while i < exprs.count {
        guard case .Pair(.Sym(let fun), let binding) = exprs[i]
              where fun.interned == self.context.symbols.DEFINE &&
                    env.systemDefined(fun, in: self.context) else {
          break loop
        }
        // Distinguish value definitions from function definitions
        switch binding {
          case .Pair(.Sym(let sym), .Pair(let def, .Null)):
            bindings.append(.Pair(.Sym(sym), .Pair(def, .Null)))
          case .Pair(.Pair(.Sym(let sym), let args), .Pair(let def, .Null)):
            bindings.append(
              .Pair(.Sym(sym), .Pair(.Pair(.Sym(Symbol(self.context.symbols.LAMBDA, .System)),
                                           .Pair(args, .Pair(def, .Null))),
                                     .Null)))
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
          self.emit(.Pop)
        }
        exit = try self.compile(exprs[i], in: env, inTailPos: tail && (i == exprs.count - 1))
        i += 1
      }
      return exit
    } else {
      // Compilation with internal definitions
      let initialLocals = self.numLocals
      let group = try self.compileBindings(.List(bindings), in: env, atomic: true, predef: true)
      let lenv = Env(group)
      var first = true
      while i < exprs.count {
        if !first {
          self.emit(.Pop)
        }
        exit = try self.compile(exprs[i], in: lenv, inTailPos: tail && (i == exprs.count - 1))
        first = false
        i += 1
      }
      // Push void in case there is no non-define expression left
      if first {
        self.emit(.PushVoid)
      }
      return self.finalizeBindings(group, exit: exit, initialLocals: initialLocals)
    }
  }
  
  /// Compiles the given binding list of the form
  /// ```((ident init) ...)```
  /// and returns a `BindingGroup` with information about the established local bindings.
  public func compileBindings(bindingList: Expr,
                              in lenv: Env,
                              atomic: Bool,
                              predef: Bool) throws -> BindingGroup {
    let group = BindingGroup(owner: self, parent: lenv)
    let env = atomic && !predef ? lenv : .Local(group)
    var bindings = bindingList
    if predef {
      while case .Pair(.Pair(.Sym(let sym), _), let rest) = bindings {
        let binding = group.allocBindingFor(sym)
        // This is a hack for now; we need to make sure forward references work, e.g. in
        // lambda expressions. A way to do this is to allocate variables for all bindings that
        // are predefined.
        binding.wasMutated()
        self.emit(.PushUndef)
        self.emit(binding.isValue ? .SetLocal(binding.index) : .MakeLocalVariable(binding.index))
        bindings = rest
      }
      bindings = bindingList
    }
    var prevIndex = -1
    while case .Pair(let binding, let rest) = bindings {
      guard case .Pair(.Sym(let sym), .Pair(let expr, .Null)) = binding else {
        throw EvalError.MalformedBindings(binding, bindingList)
      }
      try self.compile(expr, in: env, inTailPos: false)
      let binding = group.allocBindingFor(sym)
      guard binding.index > prevIndex else {
        throw EvalError.DuplicateBinding(sym, bindingList)
      }
      if binding.isValue {
        self.emit(.SetLocal(binding.index))
      } else if predef {
        self.emit(.SetLocalValue(binding.index))
      } else {
        self.emit(.MakeLocalVariable(binding.index))
      }
      prevIndex = binding.index
      bindings = rest
    }
    guard bindings.isNull else {
      throw EvalError.MalformedBindings(nil, bindingList)
    }
    return group
  }
  
  /// This function should be used for finalizing the compilation of blocks with local
  /// bindings. It finalizes the binding group and resets the local bindings so that the
  /// garbage collector can deallocate the objects that are not used anymore.
  public func finalizeBindings(group: BindingGroup, exit: Bool, initialLocals: Int) -> Bool {
    group.finalize()
    if !exit && self.numLocals > initialLocals {
      self.emit(.Reset(initialLocals, self.numLocals - initialLocals))
    }
    self.numLocals = initialLocals
    return exit
  }
  
  /// Binds a list of keywords to macro transformers in the given local environment `lenv`.
  /// If `recursive` is set to true, the macro transformers are evaluated in an environment that
  /// includes their own definition.
  public func compileMacros(bindingList: Expr,
                            in lenv: Env,
                            recursive: Bool) throws -> BindingGroup {
    var numMacros = 0
    let group = BindingGroup(owner: self, parent: lenv, nextIndex: {
      numMacros += 1
      return numMacros - 1
    })
    let env = recursive ? Env(group) : lenv
    var bindings = bindingList
    while case .Pair(let binding, let rest) = bindings {
      guard case .Pair(.Sym(let sym), .Pair(let transformer, .Null)) = binding else {
        throw EvalError.MalformedBindings(binding, bindingList)
      }
      let procExpr = try self.context.machine.eval(transformer,
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
  
  /// Compiles a closure consisting of a list of formal arguments `arglist`, a list of
  /// expressions `body`, and a local environment `env`. It puts the closure on top of the
  /// stack.
  public func compileProc(arglist: Expr, _ body: Expr, _ env: Env) throws {
    // Create closure compiler as child of the current compiler
    let closureCompiler = Compiler(self.context, env, env, self.checkpointer)
    // Compile arguments
    try closureCompiler.compileArgList(arglist)
    // Compile body
    try closureCompiler.compileBody(body)
    // Link compiled closure in the current compiler
    let codeIndex = self.fragments.count
    let code = closureCompiler.bundle()
    self.fragments.append(code)
    // Generate code for pushing captured bindings onto the stack
    for def in closureCompiler.captures.definitions {
      if let def = def, capture = closureCompiler.captures.captureFor(def) {
        if capture.origin.owner === self {
          self.emit(.PushLocal(def.index))
        } else {
          self.emit(.PushCaptured(self.captures.capture(def, from: capture.origin)))
        }
      }
    }
    // Return captured binding count and index of compiled closure
    self.emit(.MakeClosure(closureCompiler.captures.count, codeIndex))
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
      case .AssertArgCount(_), .AssertMinArgCount(_):
        ip += 1
      default:
        break
    }
    self.eliminateNoOpsAt(ip)
    if case .MakeVariableArgument(_) = self.instructions[ip] {
      ip += 1
    }
    self.eliminateNoOpsAt(ip)
  }
  
  /// Remove sequences of NoOp at instruction position `ip`.
  private func eliminateNoOpsAt(ip: Int) {
    while ip < self.instructions.count {
      guard case .NoOp = self.instructions[ip] else {
        return
      }
      self.instructions.removeAtIndex(ip)
    }
  }
}
