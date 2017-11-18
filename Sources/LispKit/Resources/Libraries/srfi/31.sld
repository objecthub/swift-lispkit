;;; SRFI 31
;;; A special form rec for recursive evaluation.
;;;
;;; This SRFI implements a special form called `rec`. This form is a generalization and
;;; combination of the forms `rec` and `named-lambda` of [Clinger1985]. It allows the
;;; simple and non-imperative construction of self-referential expressions. As an important
;;; special case, it extends the A. Church form lambda such that it allows the direct
;;; definition of recursive procedures without using further special forms like `let` or
;;; `letrec`, without using advanced constructions like the H. B. Curry combinator and,
;;; unlike `define`, without introducing variable bindings into the external environment.
;;; 
;;; Author of spec: Mirko Luedde (mirko.luedde@sap.com)
;;; 
;;; Copyright © 2017 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
;;; except in compliance with the License. You may obtain a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed under the
;;; License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
;;; either express or implied. See the License for the specific language governing permissions
;;; and limitations under the License.

(define-library (srfi 31)
  (export rec)
  (import (lispkit base))
  
  (begin
    (define-syntax rec
      (syntax-rules ()
        ((rec (name . args) body ...)
          (letrec ((name (lambda args body ...))) name))
        ((rec name expr)
          (letrec ((name expr)) name)))))
)
