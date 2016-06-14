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
    case Primitive(String, Implementation, FormCompiler?)
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
    case Impl3R((Expr, Expr, Expr, Arguments) throws -> Expr)
  }
  
  /// Procedure kind
  public let kind: Kind
  
  /// Initializer for primitive procedures
  public init(_ name: String, _ proc: () throws -> Expr, _ compiler: FormCompiler? = nil) {
    self.kind = .Primitive(name, .Impl0(proc), compiler)
  }
  
  /// Initializer for primitive procedures
  public init(_ name: String, _ proc: (Expr) throws -> Expr, _ compiler: FormCompiler? = nil) {
    self.kind = .Primitive(name, .Impl1(proc), compiler)
  }
  
  /// Initializer for primitive procedures
  public init(_ name: String,
              _ proc: (Expr, Expr) throws -> Expr,
              _ compiler: FormCompiler? = nil) {
    self.kind = .Primitive(name, .Impl2(proc), compiler)
  }
  
  /// Initializer for primitive procedures
  public init(_ name: String,
              _ proc: (Expr, Expr, Expr) throws -> Expr,
              _ compiler: FormCompiler? = nil) {
    self.kind = .Primitive(name, .Impl3(proc), compiler)
  }
  
  /// Initializer for primitive procedures
  public init(_ name: String, _ proc: (Expr?) throws -> Expr, _ compiler: FormCompiler? = nil) {
    self.kind = .Primitive(name, .Impl0O(proc), compiler)
  }

  /// Initializer for primitive procedures
  public init(_ name: String,
              _ proc: (Expr, Expr?) throws -> Expr, _ compiler: FormCompiler? = nil) {
    self.kind = .Primitive(name, .Impl1O(proc), compiler)
  }
  
  /// Initializer for primitive procedures
  public init(_ name: String,
              _ proc: (Expr, Expr, Expr?) throws -> Expr, _ compiler: FormCompiler? = nil) {
    self.kind = .Primitive(name, .Impl2O(proc), compiler)
  }
  
  /// Initializer for primitive procedures
  public init(_ name: String,
              _ proc: (Arguments) throws -> Expr, _ compiler: FormCompiler? = nil) {
    self.kind = .Primitive(name, .Impl0R(proc), compiler)
  }
  
  /// Initializer for primitive procedures
  public init(_ name: String,
              _ proc: (Expr, Arguments) throws -> Expr,
              _ compiler: FormCompiler? = nil) {
    self.kind = .Primitive(name, .Impl1R(proc), compiler)
  }
  
  /// Initializer for primitive procedures
  public init(_ name: String,
              _ proc: (Expr, Expr, Arguments) throws -> Expr,
              _ compiler: FormCompiler? = nil) {
    self.kind = .Primitive(name, .Impl2R(proc), compiler)
  }
  
  /// Initializer for primitive procedures
  public init(_ name: String,
              _ proc: (Expr, Expr, Expr, Arguments) throws -> Expr,
              _ compiler: FormCompiler? = nil) {
    self.kind = .Primitive(name, .Impl3R(proc), compiler)
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
  
  /// Returns the name of this procedure. This method either returns the name of a primitive
  /// procedure or the identity as a hex string.
  public var name: String {
    guard case .Primitive(let str, _, _) = self.kind else {
      return String(self.identity, radix: 16)
    }
    return str
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
    return "proc#" + self.name
  }
}

public typealias Arguments = ArraySlice<Expr>

public extension ArraySlice {
  public func optional(fst: Element, _ snd: Element) -> (Element, Element)? {
    switch self.count {
      case 0:
        return (fst, snd)
      case 1:
        return (self[self.startIndex], snd)
      case 2:
        return (self[self.startIndex], self[self.startIndex + 1])
      default:
        return nil
    }
  }
  
  public func optional(fst: Element, _ snd: Element, trd: Element) -> (Element, Element, Element)? {
    switch self.count {
      case 0:
        return (fst, snd, trd)
      case 1:
        return (self[self.startIndex], snd, trd)
      case 2:
        return (self[self.startIndex], self[self.startIndex + 1], trd)
      case 3:
        return (self[self.startIndex], self[self.startIndex + 1], self[self.startIndex + 2])
      default:
        return nil
    }
  }
}

///
/// A `FormCompiler` is a function that compiles an expression for a given compiler in a
/// given environment and tail position returning a boolean which indicates whether the
/// expression has resulted in a tail call.
///
public typealias FormCompiler = (Compiler, Expr, Env, Bool) throws -> Bool
