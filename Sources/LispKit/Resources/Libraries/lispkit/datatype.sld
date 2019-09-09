;;; LISPKIT DATATYPE
;;;
;;; This library implements algebraic datatypes for LispKit. It provides the following
;;; functionality:
;;;    - `define-datatype` creates a new algebraic datatype consisting of a type test
;;;      predicate and a number of variants. Each variant implicitly defines a constructor and
;;;      a pattern.
;;;    - `define-pattern` introduces a new pattern and constructor for an existing datatype
;;;      variant.
;;;    - `match` for matching a value of an algebraic datatype against patterns, binding
;;;      pattern variables and executing the code of the first case whose pattern matches
;;;      the value.
;;;
;;; Here is an example of a datatype defining a tree for storing and finding elements:
;;;
;;;   (define-datatype tree tree?
;;;     (empty)
;;;     (node left element right) where (and (tree? left) (tree? right)))
;;;
;;; The datatype `tree` defines a predicate `tree?` for checking whether a value is of type
;;; `tree`. In addition, it defines two variants with corresponding constructors `empty` and
;;; `node` for creating values of type `tree`. Variant `node` defines an invariant that
;;; prevents nodes from being constructed unless `left` and `right` are also trees.
;;;
;;; The following line defines a new tree:
;;;
;;;   (define t1 (node (empty) 4 (node (empty) 7 (empty))))
;;;
;;; `t1` gets displayed in the following manner:
;;;
;;;   #tree:(node #tree:(empty) 4 #tree:(node #tree:(empty) 7 #tree:(empty)))
;;;
;;; Using `match`, values like `t1` can be deconstructed using pattern matching. The following
;;; function `elements` shows how to collect all elements from a tree in a list:
;;;
;;;   (define (elements tree)
;;;     (match tree
;;;       ((empty)       ())
;;;       ((node l e r)  (append (elements l) (list e) (elements r)))))
;;;
;;; `match` is a special form which takes a value of an algebraic datatype and matches it
;;; against a list of cases. Each case defines a pattern and a sequence of statements which
;;; get executed if the pattern matches the value.
;;;
;;; Cases can also optionally define a guard which is a boolean expression that gets executed
;;; if the pattern of the case matches a value. Only if the guard evaluates to true, the
;;; statements of the case get executed. Otherwise, pattern matching continues. The following
;;; function `insert` demonstrates this functionality:
;;;
;;;   (define (insert tree x)
;;;     (match tree
;;;       ((empty)
;;;         (node (empty) x (empty)))
;;;       ((node l e r) where (< x e)
;;;         (node (insert l x) e r))
;;;       ((node l e r)
;;;         (node l e (insert r x)))))
;;;
;;; A new tree `t2`, with two new elements inserted, can be created like this:
;;;
;;;   (define t2 (insert (insert t1 2) 9))
;;;
;;; If a pattern is used frequently containing a lot of boilerplate, it is possible to define
;;; a shortcut using the `define-pattern` syntax:
;;;
;;;   (define-pattern (single x)
;;;     (node (empty) x (empty)))
;;;
;;; With this declaration, it is possible to use `single` in patterns. The following example
;;; also shows that it is possible to use `else` for defining a fallback case, if no other pattern
;;; is matching.
;;;
;;;   (match t
;;;     ((empty) #f)
;;;     ((single x) x)
;;;     (else (error "two many elements")))
;;;
;;; `single` can also be used as a constructor for creating trees with a single element:
;;;
;;;   (single 6)
;;;
;;; An advanced feature of `match` is the usage of pattern alternatives in a single case of
;;; a `match` construct. This can be achieved using the `or` form on the top level of a
;;; pattern:
;;;
;;;   (define (has-many-elements tree)
;;;     (match tree
;;;       ((or (empty) (single _)) #f)
;;;       (else #t)))
;;;
;;; The underscore in the `(single _)` subpattern is a wildcard that matches every value and
;;; that does not bind a new variable.
;;;
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2017-2019 Matthias Zenger. All rights reserved.
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

(define-library (lispkit datatype)

  (export define-datatype
          define-pattern
          match)

  (import (lispkit base)
          (lispkit internal))

  (begin

    ;; Defines a new datatype with a given number of datatype variants. The definition
    ;; requires the symbol `type` denoting the new type, an optional type predicate `pred`
    ;; for testing whether a value is an instance of this type, and a list of constructors
    ;; of the form `(c p1 p2 ...)` where `c` is the constructor and `p1`, `p2`, ...
    ;; are parameter names of the constructor. A constructor can be extended with an
    ;; invariant for defining requirements the parameters need to meet. This is done via
    ;; clause `where i` preceeding the constructor declaration. `i` is a boolean expression
    ;; which gets checked when the constructor gets invoked.
    (define-syntax define-datatype
      (syntax-rules ()
        ((_ type (c p ...) case ...)
          (begin (define-values (make pred ref make-subtype) (make-type (quote type)))
                 (define-variant make pred ref ((c p ...) case ...))))
        ((_ type pred (c p ...) case ...)
          (begin (define-values (make pred ref make-subtype) (make-type (quote type)))
                 (define-variant make pred ref ((c p ...) case ...))))))

    ;; Defines a new datatype variant. Two cases are defined: one for variants that have an
    ;; invariant defined, and one for variants without invariant.
    (define-syntax define-variant
      (syntax-rules (where)
        ((_ make pred ref ())
          (void))
        ((_ make pred ref ((c p ...) where precon . rest))
          (begin
            (define (c . args)
              (if (datatype-deconstruction? args)
                  (if (and (pred (cadr args)) (eq? (quote c) (car (ref (cadr args)))))
                      (cdr (ref (cadr args)))
                      #f)
                  (apply (lambda (p ...)
                           (if precon
                               (make (list (quote c) p ...))
                               (error "cannot create datatype variant $0; invalid invariant $1"
                                      (list (quote c) (quote p) ...)
                                      (quote precon)))) args)))
            (define-variant make pred ref rest)))
        ((_ make pred ref ((c p ...) . rest))
          (begin
            (define (c . args)
              (if (datatype-deconstruction? args)
                  (if (and (pred (cadr args)) (eq? (quote c) (car (ref (cadr args)))))
                      (cdr (ref (cadr args)))
                      #f)
                  (apply (lambda (p ...) (make (list (quote c) p ...))) args)))
            (define-variant make pred ref rest)))))

    ;; Defines a new pattern which specializes an existing pattern. Such custom patterns
    ;; can be used in pattern matching expressions as well as constructors for defining
    ;; values.
    (define-syntax define-pattern
      (syntax-rules ()
        ((_ (v p ...) (c q ...))
           (define (v . args)
             (if (datatype-deconstruction? args)
                 (let ((params (deconstruct-datatype c (cadr args))))
                   (if params
                       (match-pattern (q ...) () params (begin (list p ...)))
                       #f))
                 (apply (lambda (p ...) (c (unwrap q) ...)) args))))))

    (define-syntax unwrap
      (syntax-rules (unquote _ as)
        ((unwrap _)
          #f)
        ((unwrap (unquote e))
          e)
        ((unwrap (p as v))
          (unwrap p))
        ((unwrap (c p ...))
          (c (unwrap p) ...))
        ((unwrap p)
          p)))

    ;; A special form for decomposing values of algebraic types via pattern matching.
    ;; A `match` construct takes a value `expr` to pattern match on, as well as a sequence
    ;; of cases. Each case consists of a list of pattern alternatives, an optional guard,
    ;; and a sequence of statements. `match` iterates through the cases and executes the
    ;; sequence of statements of the first case whose pattern is matching `expr` and whose
    ;; guard evaluates to true. The value returned by this sequence of statements is returned
    ;; by `match`.
    (define-syntax match
      (syntax-rules ()
        ((_ expr . cases)
          (let* ((x (list expr))
                 (res (match-cases cases x)))
            (if (pair? res)
                (car res)
                (error "no matching case: $0" x))))))

    (define-syntax match-cases
      (syntax-rules (else where)
        ((_ () x)
          #f)
        ((_ ((else code ...)) x)
          (list (begin code ...)))
        ((_ ((pat where pred code ...)) x)
          (match-case pat pred x (list (begin code ...))))
        ((_ ((pat code ...)) x)
          (match-case pat () x (list (begin code ...))))
        ((_ ((pat where pred code ...) . rest) x)
          (or (match-case pat pred x (list (begin code ...))) (match-cases rest x)))
        ((_ ((pat code ...) . rest) x)
          (or (match-case pat () x (list (begin code ...))) (match-cases rest x)))))

    (define-syntax match-case
      (syntax-rules (or)
        ((_ (or p ...) pred x code)
           (or (match-pattern (p) pred x code) ...))
        ((_ p pred x code)
           (match-pattern (p) pred x code))))

    (define-syntax match-pattern
      (syntax-rules (unquote as _)
        ((match-pattern () () x code)
          (if (null? x) code #f))
        ((match-pattern () pred x code)
          (if (and (null? x) pred) code #f))
        ((match-pattern (_ . rest) pred x code)
          (match-pattern rest pred (cdr x) code))
        ((match-pattern ((unquote e) p ...) pred x code)
          (if (equal? e (car x))
              (match-pattern (p ...) pred (cdr x) code)
              #f))
        ((match-pattern ((p as v) q ...) pred x code)
          (let ((v (car x)))
            (match-pattern (p q ...) pred x code)))
        ((match-pattern ((c p ...) . rest) pred x code)
          (let ((params (deconstruct-datatype c (car x))))
            (if params
                (match-pattern (p ...) () params (match-pattern rest pred (cdr x) code))
                #f)))
        ((match-pattern (v . rest) pred x code)
          (let ((v (car x)))
            (match-pattern rest pred (cdr x) code)))))
  )
)
