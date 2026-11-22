;;; SRFI 273
;;; Extensions to Data (Type-)Checking
;;;
;;; The original SRFI 253 established a basis for type-checked (or otherwise checked) data
;;; handling. But it lacked some quality-of-life features. This SRFI extends SRFI 253 to match
;;; existing implementation practice and common sense. Provided extensions are: check
;;; aliasing with `define-check`; pre- and post-declaration of type / check with
;;; `declare-checked`; return value checks in `lambda-checked`, `case-lambda-checked`, and
;;; `define-checked`; and some optimizable, supported, and explicitly unsupported patterns
;;; suggested to implementors.
;;;
;;; The return value checks and the arrow clauses of `check-case` are provided by library
;;; `(srfi 253)`. This library exports `define-check`, `declare-checked`,
;;; `define-values-checked`, and the auxiliary syntax `check-impl?`.
;;;
;;; Author of spec: Artyom Bologov
;;;
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
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
;;;
;;; Implementation notes:
;;;   LispKit does not infer or enforce types, so `declare-checked` is a declaration without
;;;   effect on the checked procedure or variable: it only verifies that the declaration is
;;;   well-formed and that its predicates evaluate to procedures. `define-check` verifies that
;;;   the predicate is a procedure. `define-values-checked` checks the values of all variables
;;;   before defining them; assignments to the variables are not checked.

(define-library (srfi 273)

  (export define-check
          declare-checked
          define-values-checked
          check-impl?)

  (import (lispkit base)
          (only (srfi 253) check-impl?))

  (begin

    ;; Returns `pred` after checking that it is a procedure; `who` and `name` are used in the
    ;; error message
    (define (checked-predicate who name pred-expr pred)
      (if (procedure? pred)
          pred
          (error (string-append (symbol->string who) ": "
                                (object->string pred-expr)
                                (if name
                                    (string-append " for " (object->string name))
                                    "")
                                " is not a predicate:")
                 pred)))

    ;; === define-check ===

    (define-syntax define-check
      (syntax-rules ()
        ((_ name predicate)
          (define name (checked-predicate 'define-check #f 'predicate predicate)))))

    ;; === define-values-checked ===

    ;; Checks the values `vals` of variables `vars` against the predicates `preds` (whose
    ;; expressions are `pred-exprs`) and returns them as multiple values
    (define (checked-variable-values vars pred-exprs preds vals)
      (if (not (= (length vals) (length vars)))
          (error (string-append "define-values-checked: expected "
                                (number->string (length vars))
                                " values for "
                                (object->string vars)
                                ", but received "
                                (number->string (length vals))
                                ":")
                 vals))
      (do ((vars vars (cdr vars))
           (exprs pred-exprs (cdr exprs))
           (preds preds (cdr preds))
           (rest vals (cdr rest)))
          ((null? vars) (apply values vals))
        (if (not ((car preds) (car rest)))
            (error (string-append "define-values-checked: variable "
                                  (object->string (car vars))
                                  " does not satisfy "
                                  (object->string (car exprs))
                                  ":")
                   (car rest)))))

    (define-syntax define-values-checked
      (syntax-rules ()
        ((_ (var ...) (check ...) form)
          (define-values (var ...)
            (call-with-values
              (lambda () form)
              (lambda vals
                (checked-variable-values '(var ...) '(check ...) (list check ...) vals)))))))

    ;; === declare-checked ===

    (define-syntax declare-checked
      (syntax-rules (=>)
        ((_ (name . args) => (ret ...))
          (%declare-checked name args (ret ...)))
        ((_ (name . args))
          (%declare-checked name args ()))
        ((_ name predicate)
          (begin
            (checked-predicate 'declare-checked 'name 'predicate predicate)
            (if #f #f)))))

    ;; Verifies the predicates of the parameters `args` and of the return values `rets`
    (define-syntax %declare-checked
      (syntax-rules ()
        ((_ name () (ret ...))
          (begin
            (checked-predicate 'declare-checked 'name 'ret ret) ...
            (if #f #f)))
        ((_ name ((arg pred) . args) rets)
          (begin
            (checked-predicate 'declare-checked 'arg 'pred pred)
            (%declare-checked name args rets)))
        ((_ name (arg . args) rets)
          (%declare-checked name args rets))
        ((_ name rest rets)
          (%declare-checked name () rets))))
  )
)
