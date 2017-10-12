;;; LISPKIT DATATYPE
;;; 
;;; Implementation of algebraic datatypes. 
;;; 
;;; Author: Matthias Zenger
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

(define-library (lispkit datatype)
  
  (export define-datatype
          match)
  
  (import (scheme base))
  
  (begin
    
    ;; Defines a new datatype with a given number of datatype variants. The definition
    ;; requires the symbol `type` denoting the new type, an optional type predicate `pred`
    ;; for testing whether a value is an instance of this type, and a list of constructors
    ;; of the form `(c p1 p2 ...)` where `c` is the constructor and `p1`, `p2`, ...
    ;; are parameter names of the constructor.
    ;; 
    ;; Example of an algebraic datatype with two variants:
    ;;   (define-datatype binary-tree
    ;;     binary-tree?
    ;;     (leaf val)
    ;;     (node left right))
    (define-syntax define-datatype
      (syntax-rules ()
        ((_ type (c p ...) ...)
          (define-full-datatype type pred (c p ...) ...))
        ((_ type pred (c p ...) ...)
          (define-full-datatype type pred (c p ...) ...))))

    (define-syntax define-full-datatype
      (syntax-rules ()
        ((_ type pred (c p ...) ...)
          (begin
            (define-values (make pred ref make-subtype) (make-type (quote type)))
            (define (c . args)
              (if (and (pair? args)
                       (adt-selector? (car args))
                       (pair? (cdr args))
                       (null? (cddr args)))
                  (if (and (pred (cadr args)) (eq? (quote c) (car (ref (cadr args)))))
                      (cdr (ref (cadr args)))
                      #f)
                  (apply (lambda (p ...) (make (list (quote c) p ...))) args)))
            ...
            (void)))))
    
    ;; A special form for decomposing values of algebraic types via pattern matching.
    ;; A `match` construct takes a value `expr` to pattern match on, as well as a sequence
    ;; of cases. Each case consists of a pattern and a sequence of statements. `match`
    ;; iterates through the cases and executes the sequence of statements of the first
    ;; case whose pattern is matching `expr`. The value returned by this sequence of
    ;; statements is returned by `match`.
    (define-syntax match
      (syntax-rules ()
        ((_ expr . cases)
          (let* ((x (list expr))
                 (res (match-cases cases x)))
            (if (pair? res)
                (car res)
                (error "no matching case" x))))))
    
    (define-syntax match-cases
      (syntax-rules (else)
        ((_ () x)
          #f)
        ((_ ((else code)) x)
          (list (begin code)))
        ((_ ((pat code)) x)
          (match-case (pat) x (list (begin code))))
        ((_ ((pat code) . rest) x)
          (or (match-case (pat) x (list (begin code))) (match-cases rest x)))))
    
    (define-syntax match-case
      (syntax-rules (unquote _)
        ((match-case () x code)
          (if (null? x) code #f))
        ((match-case (_ . rest) x code)
          (match-case rest (cdr x) code))
        ((match-case ((unquote e) p ...) x code)
          (if (equal? e (car x))
              (match-case (p ...) (cdr x) code)
              #f))
        ((match-case ((c p ...) . rest) x code)
          (let ((params (c get-params (car x))))
            (if params
                (match-case (p ...) params (match-case rest (cdr x) code))
                #f)))
        ((match-case (v . rest) x code)
          (let ((v (car x)))
             (match-case rest (cdr x) code)))))
    
    ;; Internal type for creating a value of a new type that does not leak this library.
    (define-record-type adt-selector
      (make-adt-selector)
      adt-selector?)
    
    (define get-params (make-adt-selector))
  )
)
