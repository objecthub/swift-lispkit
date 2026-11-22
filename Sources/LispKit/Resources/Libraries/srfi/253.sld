;;; SRFI 253
;;; Data (Type-)Checking
;;;
;;; Data validation and type checking (supposedly) make for more correct code. And faster
;;; code too, sometimes. And, in rare cases, code that's easier to follow than un-checked
;;; code. Unfortunately, Scheme does not have many (type-)checking primitives out of the box.
;;; This SRFI provides some, with the aim of allowing more performant and correct code with
;;; minimum effort on the user side. Both (manual) argument checking/validation (`check-arg`)
;;; and return value(s) (`values-checked`) checking/coercion are provided. Syntax sugar like
;;; `define-checked` and `define-record-type-checked` is added on top.
;;;
;;; This library also supports the extensions of SRFI 273 to the forms of SRFI 253: return
;;; value checks via `=> (predicate ...)` in `lambda-checked`, `case-lambda-checked` and
;;; `define-checked`, arrow clauses `(predicate => procedure)` in `check-case`, and the
;;; auxiliary syntax `check-impl?`. Library `(srfi 273)` provides the other extensions of
;;; SRFI 273.
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
;;;   This is a new implementation, not a port of the sample implementation of the SRFI.
;;;   All checks are performed, and failing checks signal errors. `check-arg` is a procedure.
;;;   The other forms are macros that report the name of the procedure, the argument, and the
;;;   predicate expression of a failing check. `lambda-checked`, `case-lambda-checked` and
;;;   `define-checked` support rest arguments and internal definitions in procedure bodies.
;;;   The constructor of `define-record-type-checked` checks its arguments against the
;;;   predicates of the fields with the same names. Assignments to variables defined by
;;;   `define-checked` are not checked. Since LispKit has no implementation-specific types,
;;;   `(check-impl? datum)` is a predicate that accepts all values.

(define-library (srfi 253)

  (export check-arg
          values-checked
          check-case
          lambda-checked
          case-lambda-checked
          define-checked
          define-record-type-checked
          check-impl?)

  (import (lispkit base))

  (begin

    ;; === Errors ===

    (define (who-prefix who)
      (if who
          (string-append (if (symbol? who) (symbol->string who) (object->string who)) ": ")
          ""))

    ;; Signals that argument `name` of procedure `who` with value `val` does not satisfy
    ;; the predicate given by expression `pred-expr`
    (define (argument-check-error who name pred-expr val)
      (error (string-append (who-prefix who)
                            "argument "
                            (object->string name)
                            " does not satisfy "
                            (object->string pred-expr)
                            ":")
             val))

    ;; Signals that value number `i` (counting from 0, or #f for a single value) returned by
    ;; `who` does not satisfy `pred-expr`
    (define (value-check-error who i pred-expr val)
      (error (string-append (who-prefix who)
                            (if i (string-append "value " (number->string (+ i 1)) " ") "value ")
                            "does not satisfy "
                            (object->string pred-expr)
                            ":")
             val))

    ;; === check-impl? ===

    ;; Implementation-specific checks (SRFI 273): LispKit doesn't have any, so the datum is
    ;; ignored and all values are accepted
    (define-syntax check-impl?
      (syntax-rules ()
        ((_ datum) accept-all)))

    (define (accept-all x) #t)

    ;; === check-arg ===

    (define (check-arg pred arg . caller)
      (if (pred arg)
          arg
          (error (string-append (who-prefix (and (pair? caller) (car caller)))
                                "argument does not satisfy predicate:")
                 arg
                 pred)))

    ;; Returns `val` if it satisfies `pred`, otherwise signals an error
    (define (checked-argument who name pred-expr pred val)
      (if (pred val) val (argument-check-error who name pred-expr val)))

    (define (checked-value who i pred-expr pred val)
      (if (pred val) val (value-check-error who i pred-expr val)))

    ;; Checks the list of values `vals` against the predicates `preds` (whose expressions are
    ;; `pred-exprs`) and returns them as multiple values
    (define (checked-values who pred-exprs preds vals)
      (if (not (= (length vals) (length preds)))
          (error (string-append (who-prefix who) "expected "
                                (number->string (length preds))
                                " values, but received "
                                (number->string (length vals))
                                ":")
                 vals))
      (do ((i 0 (+ i 1))
           (exprs pred-exprs (cdr exprs))
           (preds preds (cdr preds))
           (rest vals (cdr rest)))
          ((null? preds) (apply values vals))
        (if (not ((car preds) (car rest)))
            (value-check-error who (and (> (length vals) 1) i) (car exprs) (car rest)))))

    ;; === values-checked ===

    (define-syntax values-checked
      (syntax-rules ()
        ((_ (pred) expr)
          (checked-value 'values-checked #f 'pred pred expr))
        ((_ (pred ...) expr ...)
          (%values-checked 0 () (pred ...) (expr ...)))))

    (define-syntax %values-checked
      (syntax-rules ()
        ((_ i (checked ...) () ())
          (values checked ...))
        ((_ i (checked ...) (pred . preds) (expr . exprs))
          (%values-checked (+ i 1)
                           (checked ... (checked-value 'values-checked i 'pred pred expr))
                           preds
                           exprs))))

    ;; Returns the values of `body ...` after checking them against `pred ...`
    (define-syntax returns-checked
      (syntax-rules ()
        ((_ who (pred) body ...)
          (call-with-values
            (lambda () body ...)
            (case-lambda
              ((x) (checked-value who #f 'pred pred x))
              (xs (checked-values who '(pred) (list pred) xs)))))
        ((_ who (pred ...) body ...)
          (call-with-values
            (lambda () body ...)
            (lambda xs (checked-values who '(pred ...) (list pred ...) xs))))))

    ;; === check-case ===

    (define-syntax check-case
      (syntax-rules ()
        ((_ expr clause ...)
          (let ((v expr))
            (%check-case v () clause ...)))))

    (define-syntax %check-case
      (syntax-rules (else =>)
        ((_ v (pred-expr ...))
          (error "check-case: value does not satisfy any of the predicates:"
                 v
                 '(pred-expr ...)))
        ((_ v (pred-expr ...) (else body ...))
          (let () body ...))
        ((_ v (pred-expr ...) (pred => proc) clause ...)
          (let ((res (pred v)))
            (if res
                (proc res)
                (%check-case v (pred-expr ... pred) clause ...))))
        ((_ v (pred-expr ...) (pred) clause ...)
          (let ((res (pred v)))
            (if res
                res
                (%check-case v (pred-expr ... pred) clause ...))))
        ((_ v (pred-expr ...) (pred body ...) clause ...)
          (if (pred v)
              (let () body ...)
              (%check-case v (pred-expr ... pred) clause ...)))))

    ;; === lambda-checked ===

    ;; Translates the formal parameters `args` of a checked lambda expression into a
    ;; lambda expression which checks the arguments on entry
    (define-syntax %lambda-checked
      (syntax-rules ()
        ;; All arguments processed
        ((_ who body (param ...) (check ...))
          (lambda (param ...) check ... (let () . body)))
        ;; Checked argument
        ((_ who body (param ...) (check ...) (name pred) . args)
          (%lambda-checked who
                           body
                           (param ... name)
                           (check ... (checked-argument who 'name 'pred pred name))
                           . args))
        ;; Unchecked argument
        ((_ who body (param ...) (check ...) name . args)
          (%lambda-checked who body (param ... name) (check ...) . args))
        ;; Rest argument
        ((_ who body (param ...) (check ...) . rest)
          (lambda (param ... . rest) check ... (let () . body)))))

    (define-syntax lambda-checked
      (syntax-rules (=>)
        ((_ args => (ret ...) body ...)
          (%lambda-checked 'lambda-checked
                           ((returns-checked 'lambda-checked (ret ...) body ...))
                           ()
                           ()
                           . args))
        ((_ args body ...)
          (%lambda-checked 'lambda-checked (body ...) () () . args))))

    ;; === case-lambda-checked ===

    (define-syntax case-lambda-checked
      (syntax-rules ()
        ((_ clause ...)
          (%case-lambda-checked (clause ...) ()))))

    (define-syntax %case-lambda-checked
      (syntax-rules (=>)
        ;; All clauses processed
        ((_ () (done ...))
          (case-lambda done ...))
        ;; Clause with return value checks
        ((_ ((args => (ret ...) body ...) clause ...) (done ...))
          (%case-lambda-clause (clause ...)
                               (done ...)
                               ((returns-checked 'case-lambda-checked (ret ...) body ...))
                               ()
                               ()
                               . args))
        ;; Clause without return value checks
        ((_ ((args body ...) clause ...) (done ...))
          (%case-lambda-clause (clause ...) (done ...) (body ...) () () . args))))

    (define-syntax %case-lambda-clause
      (syntax-rules ()
        ;; All arguments processed
        ((_ clauses (done ...) body (param ...) (check ...))
          (%case-lambda-checked clauses
                                (done ... ((param ...) check ... (let () . body)))))
        ;; Checked argument
        ((_ clauses done body (param ...) (check ...) (name pred) . args)
          (%case-lambda-clause clauses
                               done
                               body
                               (param ... name)
                               (check ... (checked-argument 'case-lambda-checked
                                                            'name 'pred pred name))
                               . args))
        ;; Unchecked argument
        ((_ clauses done body (param ...) (check ...) name . args)
          (%case-lambda-clause clauses done body (param ... name) (check ...) . args))
        ;; Rest argument
        ((_ clauses (done ...) body (param ...) (check ...) . rest)
          (%case-lambda-checked clauses
                                (done ... ((param ... . rest) check ... (let () . body)))))))

    ;; === define-checked ===

    (define-syntax define-checked
      (syntax-rules (=>)
        ((_ (name . args) => (ret ...) body ...)
          (define name
            (%lambda-checked 'name ((returns-checked 'name (ret ...) body ...)) () () . args)))
        ((_ (name . args) body ...)
          (define name (%lambda-checked 'name (body ...) () () . args)))
        ((_ name pred value)
          (define name (checked-value 'name #f 'pred pred value)))))

    ;; === define-record-type-checked ===

    ;; Checks the arguments `vals` of constructor `who` whose names are `names` against the
    ;; predicates of the corresponding fields in the association list `field-checks`, which
    ;; maps field names to pairs consisting of a predicate expression and a predicate
    (define (check-constructor-args who names vals field-checks)
      (do ((names names (cdr names))
           (vals vals (cdr vals)))
          ((null? names))
        (let ((check (assq (car names) field-checks)))
          (if (and check (not ((cddr check) (car vals))))
              (argument-check-error who (car names) (cadr check) (car vals))))))

    (define-syntax define-record-type-checked
      (syntax-rules ()
        ((_ type-name (constructor arg ...) predicate field ...)
          (%define-record-type-checked type-name
                                       (constructor arg ...)
                                       predicate
                                       ()
                                       ()
                                       ()
                                       field ...))))

    (define-syntax %define-record-type-checked
      (syntax-rules ()
        ;; All fields processed
        ((_ type-name (constructor arg ...) predicate (fspec ...) (check ...) (modifier-def ...))
          (begin
            (define-record-type type-name
              (internal-constructor arg ...)
              predicate
              fspec ...)
            (define (constructor arg ...)
              (check-constructor-args 'constructor
                                      '(arg ...)
                                      (list arg ...)
                                      (list check ...))
              (internal-constructor arg ...))
            modifier-def ...))
        ;; Field with modifier
        ((_ type-name ctor predicate (fspec ...) (check ...) (modifier-def ...)
            (field pred accessor modifier) field-spec ...)
          (%define-record-type-checked
            type-name
            ctor
            predicate
            (fspec ... (field accessor internal-modifier))
            (check ... (cons 'field (cons 'pred pred)))
            (modifier-def ...
              (define (modifier record value)
                (internal-modifier record (checked-argument 'modifier 'field 'pred pred value))))
            field-spec ...))
        ;; Field without modifier
        ((_ type-name ctor predicate (fspec ...) (check ...) (modifier-def ...)
            (field pred accessor) field-spec ...)
          (%define-record-type-checked
            type-name
            ctor
            predicate
            (fspec ... (field accessor))
            (check ... (cons 'field (cons 'pred pred)))
            (modifier-def ...)
            field-spec ...))))
  )
)
