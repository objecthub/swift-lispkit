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
public class NativeLibrary {
  
  /// The context into which this library gets exported
  let context: Context
  
  /// Initializer
  public required init(_ context: Context) {
    self.context = context
    self.export()
  }
  
  /// The `export` method needs to be overridden in subclasses of `Library`. These
  /// overriding implementations declare all bindings that are exported by the library.
  public func export() {
    // This method needs to be overridden for concrete primitive libraries
  }
  
  public static func importProc(from context: Context,
                                name: String,
                                lib: String = #file) -> Procedure {
    let sym = context.symbols.intern(name)
    guard case .Some(.Proc(let proc)) = context.systemScope[sym] else {
      preconditionFailure("cannot import \(name) in library \(lib)")
    }
    return proc
  }
  
  public func define(proc: Procedure) {
    self.context.systemScope[self.context.symbols.intern(proc.name)] = .Proc(proc)
  }
    
  public func define(name: String, _ special: SpecialForm) {
    self.context.systemScope[self.context.symbols.intern(name)] = .Special(special)
  }
  
  public func define(name: String, compile sourcecode: String) {
    let expr = self.context.machine.evalStr(sourcecode, in: .System)
    if case .Error(let error) = expr {
      preconditionFailure("compilation failure: " + error.description)
    }
    self.context.systemScope[self.context.symbols.intern(name)] = expr
  }
  
  public func define(name: String, syntax sourcecode: String) {
    switch self.context.machine.evalStr(sourcecode, in: .System) {
      case .Error(let error):
        preconditionFailure("compilation failure: " + error.description)
      case .Proc(let proc):
        self.context.systemScope[self.context.symbols.intern(name)] = .Special(SpecialForm(proc))
      case let expr:
        preconditionFailure("broken syntax transformer: " + expr.description)
    }
  }
  
  public func procedure(sourcecode: String) -> Procedure {
    let expr = self.context.machine.evalStr(sourcecode, in: .System)
    guard case .Proc(let proc) = expr else {
      preconditionFailure("predefined procedure not a procedure")
    }
    return proc
  }
  
  public func compile(sourcecode: String) {
    if case .Error(let error) = self.context.machine.evalStr(sourcecode, in: .System) {
      preconditionFailure("compilation failure: " + error.description)
    }
  }
  
  public func invoke(instr: Instruction,
                     with expr: Expr,
                     in env: Env,
                     for compiler: Compiler) throws -> Bool {
    guard case .Pair(_, .Pair(let arg, .Null)) = expr else {
      throw EvalError.ArgumentCountError(formals: 1, args: expr)
    }
    try compiler.compile(arg, in: env, inTailPos: false)
    compiler.emit(instr)
    return false
  }
  
  // Sublibrary declaration
  
  func include(lib: NativeLibrary.Type) {
    let _ = lib.init(self.context)
  }
}
