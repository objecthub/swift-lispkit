//
//  TypeLibrary.swift
//  LispKit
//
//  The design of the procedural interface for custom defined types was inspired by SRFI 137,
//  which was developed by John Cowan and Marc Nieper-Wißkirchen. Ultimately, the implementation
//  ended up being quite different, using an internal mechanism to tag expressions. This made
//  it possible to support value types and to avoid heavy-weight records.
//
//  In addition to the procedural interface, there is a declarative interface for fixed
//  simple types as well as an extensible type mechanism. Here is an example implementing 2D
//  points and colored points using the declarative interface via `define-type`:
//
//  ```
//  (define-type (point object)
//    point?
//    ((make-point x y) (cons x y))
//    ((point-x (p)) (car p))
//    ((point-y (p)) (cdr p))
//    ((point-move (p) dx dy) (make-point (+ (car p) dx) (+ (cdr p) dy))))
//  (define-type (color-point point)
//    color-point?
//    ((make-color-point x y c) (values x y c))
//    ((color-point-color (p c)) c))
//  (define cp (make-color-point 10 20 'red))
//  (point? cp)             => #t
//  (point-x cp)            => 10
//  (point-y cp)            => 20
//  (color-point-color cp)  => red
//  ```
//
//  Created by Matthias Zenger on 07/10/2017.
//  Copyright © 2017 ObjectHub. All rights reserved.
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

///
/// Type library
///
public final class TypeLibrary: NativeLibrary {
  
  // Type identifiers
  
  private let void: Symbol
  private let endOfFile: Symbol
  private let null: Symbol
  private let boolean: Symbol
  private let symbol: Symbol
  private let fixnum: Symbol
  private let bignum: Symbol
  private let rational: Symbol
  private let flonum: Symbol
  private let complex: Symbol
  private let char: Symbol
  private let string: Symbol
  private let bytevector: Symbol
  private let pair: Symbol
  private let mpair: Symbol
  private let array: Symbol
  private let vector: Symbol
  private let gvector: Symbol
  private let values: Symbol
  private let procedure: Symbol
  private let parameter: Symbol
  private let promise: Symbol
  private let syntax: Symbol
  private let environment: Symbol
  private let hashtable: Symbol
  private let port: Symbol
  private let recordType: Symbol
  private let error: Symbol
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "type"]
  }
  
  /// Initialize type library.
  public required init(in context: Context) throws {
    self.void = context.symbols.intern("void")
    self.endOfFile = context.symbols.intern("end-of-file")
    self.null = context.symbols.intern("null")
    self.boolean = context.symbols.intern("boolean")
    self.symbol = context.symbols.intern("symbol")
    self.fixnum = context.symbols.intern("fixnum")
    self.bignum = context.symbols.intern("bignum")
    self.rational = context.symbols.intern("rational")
    self.flonum = context.symbols.intern("flonum")
    self.complex = context.symbols.intern("complex")
    self.char = context.symbols.intern("char")
    self.string = context.symbols.intern("string")
    self.bytevector = context.symbols.intern("bytevector")
    self.pair = context.symbols.intern("pair")
    self.mpair = context.symbols.intern("mpair")
    self.array = context.symbols.intern("array")
    self.vector = context.symbols.intern("vector")
    self.gvector = context.symbols.intern("gvector")
    self.values = context.symbols.intern("values")
    self.procedure = context.symbols.intern("procedure")
    self.parameter = context.symbols.intern("parameter")
    self.promise = context.symbols.intern("promise")
    self.syntax = context.symbols.intern("syntax")
    self.environment = context.symbols.intern("environment")
    self.hashtable = context.symbols.intern("hashtable")
    self.port = context.symbols.intern("port")
    self.recordType = context.symbols.intern("record-type")
    self.error = context.symbols.intern("error")
    try super.init(in: context)
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"],    "define", "define-values", "define-syntax", "set!",
                                                "syntax-rules", "lambda", "values", "quote", "void",
                                                "identity")
    self.`import`(from: ["lispkit", "list"],    "null?", "cons", "car", "cdr", "cadr", "cddr")
    self.`import`(from: ["lispkit", "control"], "if", "let", "let*", "begin")
    self.`import`(from: ["lispkit", "dynamic"], "error", "assert")
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("type-of", self.typeOf))
    self.define(Procedure("_make-type-id", self.makeTypeId))
    self.define(Procedure("_tag", self.tag))
    self.define(Procedure("_untag", self.untag))
    self.define(Procedure("_instance?", self.isInstance))
    self.define("_typeproc", via: """
      (define (_typeproc type)
        (values (car type)                                                          ; type id
                (lambda (payload) (_tag type payload))                              ; constructor
                (lambda (expr) (_instance? expr type))                              ; predicate
                (lambda (expr) (if (_instance? expr type)                           ; accessor
                                   (_untag expr)
                                   (error "not an instance of type $1: $0" expr (car type))))
                (lambda (id) (_typeproc (_make-type-id id type)))))                 ; make subtype
      """)
    self.define("make-type", via:
      "(define (make-type id) (_typeproc (_make-type-id id)))")
    self.define("_extensible-type", via:
      "(define _extensible-type (_make-type-id \"extensible-type\"))")
    self.define("_make-type-repr", via:
      "(define (_make-type-repr make ctr tag) (_tag _extensible-type (cons tag (cons make ctr))))")
    self.define("extensible-type?", via:
      "(define (extensible-type? expr) (_instance? expr _extensible-type))")
    self.define("_type-repr-ref", via: """
      (define (_type-repr-ref expr)
        (if (_instance? expr _extensible-type)
            (_untag expr)
            (error "not an extensible type: $0" expr)))
      """)
    self.define("extensible-type-tag", via:
      "(define (extensible-type-tag expr) (car (_type-repr-ref expr)))")
    self.define("object", via:
      "(define object (_make-type-repr make-type identity 'object))")
    self.define(Procedure("_first-car", self.firstCar))
    self.define(Procedure("_apply-to-constructor", self.applyToConstructor))
    self.define("_define-operation", via: """
      (define-syntax _define-operation
        (syntax-rules ()
          ((_ ref (func (s) . ys) stmt ...)
            (define (func obj . ys) (let ((s (ref obj))) stmt ...)))
          ((_ ref (func y . ys) stmt ...)
            (define (func y . ys) stmt ...))))
      """)
    self.define("_define-func", via: """
      (define-syntax _define-func
        (syntax-rules ()
          ((_ ref (func (s0 ... sn) . ys) stmt ...)
            (define (func obj . ys) (let* ((s (ref obj))
                                           (s0 (_first-car s (set! s (cdr s))))
                                           ...
                                           (sn (car s)))
                                       stmt ...)))
          ((_ ref (func y . ys) stmt ...)
            (define (func y . ys) stmt ...))))
      """)
    self.define("define-type", via: """
      (define-syntax define-type
        (syntax-rules ()
          ((_ (type) pred ((make x ...) expr ...) ((func . ys) stmt ...) ...)
            (define-type (type object) pred ((make x ...) expr ...) ref ((func . ys) stmt ...) ...))
          ((_ (type super) pred ((make x ...) expr ...) ((func . ys) stmt ...) ...)
            (define-type (type super) pred ((make x ...) expr ...) ref ((func . ys) stmt ...) ...))
          ((_ (type) pred ((make x ...) expr ...) ref ((func . ys) stmt ...) ...)
            (define-type (type object) pred ((make x ...) expr ...) ref ((func . ys) stmt ...) ...))
          ((_ (type super) pred ((make x ...) expr ...) ref ((func . ys) stmt ...) ...)
            (begin
              (define-values (tpe new pred ref make-subtype)
                ((cadr (_type-repr-ref super)) (quote type)))
              (define (constr x ... ext)
                (_apply-to-constructor (cddr (_type-repr-ref super)) (begin expr ...) ext))
              (define type (_make-type-repr make-subtype constr tpe))
              (define (make x ...) (new (constr x ... (quote ()))))
              (_define-func ref (func . ys) stmt ...) ... (void)))
          ((_ type pred ((make . xs) expr ...) ((func . ys) stmt ...) ...)
            (define-type type pred ((make . xs) expr ...) ref ((func . ys) stmt ...) ...))
          ((_ type pred ((make . xs) expr ...) ref ((func . ys) stmt ...) ...)
            (begin
              (define-values (type new pred ref make-subtype) (make-type (quote type)))
              (define (make . xs) (new (begin expr ...)))
              (_define-operation ref (func . ys) stmt ...) ... (void)))))
      """)
  }
  
  private func typeOf(expr: Expr) -> Expr {
    switch expr {
      case .undef:
        return .false
      case .uninit(_):
        return .false
      case .void:
        return .symbol(self.void)
      case .eof:
        return .symbol(self.endOfFile)
      case .null:
        return .symbol(self.null)
      case .true:
        return .symbol(self.boolean)
      case .false:
        return .symbol(self.boolean)
      case .symbol(_):
        return .symbol(self.symbol)
      case .fixnum(_):
        return .symbol(self.fixnum)
      case .bignum(_):
        return .symbol(self.bignum)
      case .rational(_, _):
        return .symbol(self.rational)
      case .flonum(_):
        return .symbol(self.flonum)
      case .complex(_):
        return .symbol(self.complex)
      case .char(_):
        return .symbol(self.char)
      case .string(_):
        return .symbol(self.string)
      case .bytes(_):
        return .symbol(self.bytevector)
      case .pair(_, _):
        return .symbol(self.pair)
      case .box(_):
        return .symbol(self.pair)
      case .mpair(_):
        return .symbol(self.mpair)
      case .array(_):
        return .symbol(self.array)
      case .vector(let coll):
        switch coll.kind {
          case .vector:
            return .symbol(self.vector)
          case .growableVector:
            return .symbol(self.gvector)
          default:
            return .false
        }
      case .record(let coll):
        switch coll.kind {
          case .recordType:
            return .symbol(self.recordType)
          case .record(let icoll):
            if case .recordType = icoll.kind {
              return icoll.exprs[0]
            } else {
              return .false
            }
          default:
            return .false
        }
      case .table(_):
        return .symbol(self.hashtable)
      case .promise(_):
        return .symbol(self.promise)
      case .values(_):
        return .symbol(self.values)
      case .procedure(let proc):
        switch proc.kind {
          case .parameter(_):
            return .symbol(self.parameter)
          default:
            return .symbol(self.procedure)
        }
      case .special(_):
        return .symbol(self.syntax)
      case .env(_):
        return .symbol(self.environment)
      case .port(_):
        return .symbol(self.port)
      case .object(let obj):
        if case .objectType(let sym) = obj.type {
          return .symbol(sym)
        } else {
          return .false
        }
      case .tagged(.pair(.symbol(let sym), _), _):
        return .symbol(sym)
      case .tagged(let tag, _):
        if case .object(let objTag) = tag {
          if let enumType = objTag as? EnumType {
            return .symbol(enumType.id)
          }
        }
        return .false
      case .error(_):
        return .symbol(self.error)
      case .syntax(_, _):
        return .false
    }
  }
  
  func makeTypeId(_ expr: Expr, _ supertype: Expr?) throws -> Expr {
    let stpe = supertype ?? .null
    if case .symbol(let sym) = expr {
      return .pair(.symbol(Symbol(uninterned: sym.identifier)), stpe)
    } else {
      return .pair(.symbol(Symbol(uninterned: try expr.asString())), stpe)
    }
  }
  
  func tag(_ tag: Expr, _ expr: Expr) -> Expr {
    return .tagged(tag, expr)
  }
  
  func untag(_ expr: Expr) throws -> Expr {
    guard case .tagged(_, let untagged) = expr else {
      throw RuntimeError.type(expr, expected: [.taggedType])
    }
    return untagged
  }
  
  func isInstance(_ expr: Expr, _ supertype: Expr) throws -> Expr {
    guard case .tagged(let type, _) = expr,
          case .pair(let stpe, _) = supertype else {
      return .false
    }
    var current = type
    while case .pair(let tpe, let next) = current {
      if eqvExpr(tpe, stpe) {
        return .true
      } 
      current = next
    }
    return .false
  }

  private func firstCar(_ pair: Expr, _ expr: Expr) throws -> Expr {
    guard case .pair(let res, _) = pair else {
      throw RuntimeError.type(pair, expected: [.pairType])
    }
    return res
  }

  private func applyToConstructor(args: Arguments) throws -> (Procedure, Exprs) {
    guard args.count == 3 else {
      throw RuntimeError.argumentCount(num: 3, args: .makeList(args))
    }
    guard case .procedure(let consumer) = args.first! else {
      throw RuntimeError.type(args.first!, expected: [.procedureType])
    }
    let x = args[args.startIndex + 1]
    let y = args[args.startIndex + 2]
    if case .void = x {
      return (consumer, [])
    }
    guard case .values(let expr) = x else {
      return (consumer, [.pair(x, y)])
    }
    var exprs = Exprs()
    var next = expr
    while case .pair(let arg, let rest) = next {
      exprs.append(arg)
      next = rest
    }
    if exprs.count > 0 {
      exprs[exprs.count - 1] = .pair(exprs[exprs.count - 1], y)
    }
    return (consumer, exprs)
  }
}
