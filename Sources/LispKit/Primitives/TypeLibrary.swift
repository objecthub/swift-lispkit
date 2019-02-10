//
//  TypeLibrary.swift
//  LispKit
//
//  The design for custom defined types was inspired by SRFI 137, which was developed by
//  John Cowan and Marc Nieper-Wißkirchen. Ultimately, the implementation ended up being
//  quite different, using an internal mechanism to tag expressions. This made it possible
//  to support value types and to avoid heavy-weight records.
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

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "type"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"], "define", "lambda", "values", "quote")
    self.`import`(from: ["lispkit", "list"], "null?", "car")
    self.`import`(from: ["lispkit", "control"], "if")
    self.`import`(from: ["lispkit", "dynamic"], "error")
    self.`import`(from: ["lispkit", "box"], "mcons", "mcar")
  }

  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("_tag", self.tag))
    self.define(Procedure("_untag", self.untag))
    self.define(Procedure("_instance?", self.isInstance))
    self.define("_typeproc", via: """
      (define (_typeproc type)
        (values (lambda (payload) (_tag type payload))                              ; constructor
                (lambda (expr) (_instance? expr type))                              ; predicate
                (lambda (expr) (if (_instance? expr type)                           ; accessor
                                   (_untag expr)
                                   (error "not an instance of type $1: $0" expr (mcar type))))
                (lambda id (_typeproc (mcons (if (null? id) #f (car id)) type)))))  ; make subtype
      """)
      self.define("make-type", via:
        "(define (make-type . id) (_typeproc (mcons (if (null? id) #f (car id)) '())))")
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
    guard case .tagged(let type, _) = expr else {
      return .false
    }
    var current = type
    while case .mpair(let tuple) = current {
      if eqvExpr(current, supertype) {
        return .true
      }
      current = tuple.snd
    }
    return .false
  }
}

