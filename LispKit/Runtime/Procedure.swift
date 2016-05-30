//
//  Procedure.swift
//  LispKit
//
//  Created by Matthias Zenger on 21/01/2016.
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
/// `Executable` objects are used to box closures, providing identity based
/// equality and hash values.
///
public final class Procedure: Reference, CustomStringConvertible {
  
  /// There are three kinds of procedures:
  ///    1. Primitives: These are built-in procedures
  ///    2. Closures: These are user-defined procedures, e.g. via `lambda`
  ///    3. Transformers: These are user-defined macro transformers defined via `syntax-rules`
  public enum Kind {
    case Primitive(Implementation, FormCompiler?)
    case Closure([Expr], Code)
    case Transformer(SyntaxRules)
  }
  
  public enum Implementation {
    case Impl0(() throws -> Expr)
    case Impl1((Expr) throws -> Expr)
    case Impl2((Expr, Expr) throws -> Expr)
    case Impl3((Expr, Expr, Expr) throws -> Expr)
    case Impl0O((Expr?) throws -> Expr)
    case Impl1O((Expr, Expr?) throws -> Expr)
    case Impl2O((Expr, Expr, Expr?) throws -> Expr)
    case Impl0R((Arguments) throws -> Expr)
    case Impl1R((Expr, Arguments) throws -> Expr)
    case Impl2R((Expr, Expr, Arguments) throws -> Expr)
  }
  
  /// Procedure kind
  public let kind: Kind
  
  /// Initializer for primitive procedures
  public init(_ proc: () throws -> Expr, _ compiler: FormCompiler? = nil) {
    self.kind = .Primitive(.Impl0(proc), compiler)
  }
  
  /// Initializer for primitive procedures
  public init(_ proc: (Expr) throws -> Expr, _ compiler: FormCompiler? = nil) {
    self.kind = .Primitive(.Impl1(proc), compiler)
  }
  
  /// Initializer for primitive procedures
  public init(_ proc: (Expr, Expr) throws -> Expr, _ compiler: FormCompiler? = nil) {
    self.kind = .Primitive(.Impl2(proc), compiler)
  }
  
  /// Initializer for primitive procedures
  public init(_ proc: (Expr, Expr, Expr) throws -> Expr, _ compiler: FormCompiler? = nil) {
    self.kind = .Primitive(.Impl3(proc), compiler)
  }
  
  /// Initializer for primitive procedures
  public init(_ proc: (Expr?) throws -> Expr, _ compiler: FormCompiler? = nil) {
    self.kind = .Primitive(.Impl0O(proc), compiler)
  }

  /// Initializer for primitive procedures
  public init(_ proc: (Expr, Expr?) throws -> Expr, _ compiler: FormCompiler? = nil) {
    self.kind = .Primitive(.Impl1O(proc), compiler)
  }
  
  /// Initializer for primitive procedures
  public init(_ proc: (Expr, Expr, Expr?) throws -> Expr, _ compiler: FormCompiler? = nil) {
    self.kind = .Primitive(.Impl2O(proc), compiler)
  }
  
  /// Initializer for primitive procedures
  public init(_ proc: (Arguments) throws -> Expr, _ compiler: FormCompiler? = nil) {
    self.kind = .Primitive(.Impl0R(proc), compiler)
  }
  
  /// Initializer for primitive procedures
  public init(_ proc: (Expr, Arguments) throws -> Expr, _ compiler: FormCompiler? = nil) {
    self.kind = .Primitive(.Impl1R(proc), compiler)
  }
  
  /// Initializer for primitive procedures
  public init(_ proc: (Expr, Expr, Arguments) throws -> Expr, _ compiler: FormCompiler? = nil) {
    self.kind = .Primitive(.Impl2R(proc), compiler)
  }
  
  /// Initializer for compiled closures
  public init(_ captured: [Expr], _ code: Code) {
    self.kind = .Closure(captured, code)
  }
  
  /// Initializer for compiled closures
  public init(_ code: Code) {
    self.kind = .Closure([], code)
  }
  
  /// Initializer for transformers
  public init(_ rules: SyntaxRules) {
    self.kind = .Transformer(rules)
  }
    
  public func mark(tag: UInt8) {
    switch self.kind {
      case .Closure(let captures, let code):
        for capture in captures {
          capture.mark(tag)
        }
        code.mark(tag)
      default:
        break
    }
  }
  
  /// A textual description
  public var description: String {
    return "proc#" + String(self.identity, radix: 16)
  }
}

public typealias Arguments = ArraySlice<Expr>

///
/// A `FormCompiler` is a function that compiles an expression for a given compiler in a
/// given environment and tail position returning a boolean which indicates whether the
/// expression has resulted in a tail call.
///
public typealias FormCompiler = (Compiler, Expr, Env, Bool) throws -> Bool
