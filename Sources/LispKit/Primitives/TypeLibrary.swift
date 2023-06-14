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
//  (define-type (point obj)
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
//  Copyright © 2017-2023 ObjectHub. All rights reserved.
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
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "type"]
  }
  
  /// Initialize type library.
  public required init(in context: Context) throws {
    try super.init(in: context)
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"],    "define", "define-values", "define-syntax", "set!",
                                                "syntax-rules", "lambda", "values", "quote", "void",
                                                "identity")
    self.`import`(from: ["lispkit", "list"],    "null?", "cons", "car", "cdr", "cadr", "cddr")
    self.`import`(from: ["lispkit", "control"], "if", "let", "let*", "let-values", "begin")
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
    self.define("_obj-pair", via: """
      (define _obj-pair (let-values (((otag onew opred oref osub) (make-type 'obj)))
                             (cons (_make-type-repr osub identity otag) otag)))
      """)
    self.define("_obj", via: "(define _obj (car _obj-pair))")
    self.define("obj", via: "(define obj (car _obj-pair))")
    self.define("obj-type-tag", via: "(define obj-type-tag (cdr _obj-pair))")
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
            (define-type (type _obj) pred ((make x ...) expr ...) ref ((func . ys) stmt ...) ...))
          ((_ (type super) pred ((make x ...) expr ...) ((func . ys) stmt ...) ...)
            (define-type (type super) pred ((make x ...) expr ...) ref ((func . ys) stmt ...) ...))
          ((_ (type) pred ((make x ...) expr ...) ref ((func . ys) stmt ...) ...)
            (define-type (type _obj) pred ((make x ...) expr ...) ref ((func . ys) stmt ...) ...))
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
  
  public func typeOf(expr: Expr) -> Expr {
    if let syms = expr.typeTag(in: self.context) {
      var res = Expr.null
      for sym in syms.reversed() {
        res = .pair(.symbol(sym), res)
      }
      return res
    } else {
      return .null
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
