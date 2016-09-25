//
//  Library.swift
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

///
/// `Library` defines a framework for defining builtin functionality for LispKit in a modular
/// fashion. Concrete implementations subclass `Library` and override the `export` method with
/// declarations of constants, procedures, and special forms.
/// 
open class NativeLibrary {
  
  /// The context into which this library gets exported
  let context: Context
  
  /// Initializer
  public required init(_ context: Context) {
    self.context = context
    self.export()
  }
  
  /// The `export` method needs to be overridden in subclasses of `Library`. These
  /// overriding implementations declare all bindings that are exported by the library.
  open func export() {
    // This method needs to be overridden for concrete primitive libraries
  }
  
  open static func importProc(from context: Context,
                                name: String,
                                lib: String = #file) -> Procedure {
    let sym = context.symbols.intern(name)
    guard case .some(.procedure(let proc)) = context.systemScope[sym] else {
      preconditionFailure("cannot import \(name) in library \(lib)")
    }
    return proc
  }
  
  open func define(_ proc: Procedure) {
    self.context.systemScope[self.context.symbols.intern(proc.name)] = .procedure(proc)
  }
    
  open func define(_ name: String, _ special: SpecialForm) {
    self.context.systemScope[self.context.symbols.intern(name)] = .special(special)
  }
  
  open func define(_ name: String, compile sourcecode: String) {
    let expr = self.context.machine.evalStr(sourcecode, in: .system)
    if case .error(let error) = expr {
      preconditionFailure("compilation failure: " + error.description)
    }
    self.context.systemScope[self.context.symbols.intern(name)] = expr
  }
  
  open func define(_ name: String, syntax sourcecode: String) {
    switch self.context.machine.evalStr(sourcecode, in: .system) {
      case .error(let error):
        preconditionFailure("compilation failure: " + error.description)
      case .procedure(let proc):
        self.context.systemScope[self.context.symbols.intern(name)] = .special(SpecialForm(proc))
      case let expr:
        preconditionFailure("broken syntax transformer: " + expr.description)
    }
  }
  
  open func procedure(_ sourcecode: String) -> Procedure {
    let expr = self.context.machine.evalStr(sourcecode, in: .system)
    guard case .procedure(let proc) = expr else {
      preconditionFailure("predefined procedure not a procedure")
    }
    return proc
  }
  
  open func compile(_ sourcecode: String) {
    if case .error(let error) = self.context.machine.evalStr(sourcecode, in: .system) {
      preconditionFailure("compilation failure: " + error.description)
    }
  }
  
  open func invoke(_ instr: Instruction,
                     with expr: Expr,
                     in env: Env,
                     for compiler: Compiler) throws -> Bool {
    guard case .pair(_, .pair(let arg, .null)) = expr else {
      throw EvalError.argumentCountError(formals: 1, args: expr)
    }
    try compiler.compile(arg, in: env, inTailPos: false)
    compiler.emit(instr)
    return false
  }
  
  // Sublibrary declaration
  
  func include(_ lib: NativeLibrary.Type) {
    let _ = lib.init(self.context)
  }
}
