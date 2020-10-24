;;; LISPKIT COMBINATOR
;;;
;;; This library defines abstractions for combinator-style programming. It provides access
;;; to functions for composing other functions.
;;;
;;; Special forms `cut` and `cute` were specified and implemented by Sebastian Egner in
;;; SRFI 26. Copyright © 2002 Sebastian Egner. All rights reserved.
;;;
;;; Author of library `(lispkit combinator)`: Matthias Zenger
;;; Copyright © 2020 Matthias Zenger. All rights reserved.
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

(define-library (lispkit combinator)
  
  (export identity
          const
          flip
          negate
          partial
          compose
          o
          conjoin
          disjoin
          list-of?
          each
          cut
          cute
          Y)
  
  (import (lispkit base))
  
  (begin
    
    ;; Returns a function accepting any number of arguments and returning the values `c` ...
    (define (const c . cs)
      (if (null? cs)
          (lambda _ c)
          (lambda _ (apply values c cs))))
    
    ;; Takes a function with two parameters and returns an equivalent function where the two
    ;; parameters are swapped.
    (define (flip f)
      (lambda (x y) (f y x)))
    
    ;; Returns a function which invokes `f` and returns the logical negation.
    (define (negate f)
      (lambda args (not (apply f args))))
    
    ;; Applies arguments `args` partially to `f` and returns a new function accepting the
    ;; remaining arguments. For a function `(f a1 a2 a3 ... an)`, `(partial f a1 a2)` will
    ;; return a function `(lambda (a3 ... an) (f a1 a2 a3 ... an))`.
    (define (partial f . args)
      (lambda args2
        (apply f (append args args2))))
    
    (define (_recc f . fs)
      (if (null? fs)
          f
          (lambda args (call-with-values (lambda () (apply (apply _recc fs) args)) f))))
    
    ;; Composes the given list of functions `fs` such that `((compose f1 f2 ... fn) x)` is
    ;; equivalent to `(f1 (f2 (... (fn x))))`. `compose` supports functions returning multiple
    ;; arguments.
    (define (compose . fs)
      (if (null? fs)
          values
          (apply _recc fs)))
    
    ;; Composes the given list of functions `fs` such that `((o f1 f2 ... fn) x)` is
    ;; equivalent to `(f1 (f2 (... (fn x))))`. `o` is a more efficient version of `compose`
    ;; which only works if the involved functions only return a single argument. `compose`
    ;; is more general and supports functions returning multiple arguments.
    (define (o . fs)
      (if (null? fs)
          identity
          (_reco fs)))
    
    (define (_reco fs)
      (if (null? (cdr fs))
          (car fs)
          (lambda (x) ((car fs) ((_reco (cdr fs)) x)))))
    
    ;; Returns a function invoking all functions `fs` and combining the results with `and`.
    ;; `((conjoin f1 f2 ...) x ...)` is equivalent to `(and (f1 x ...) (f2 x ...) ...)`.
    (define (conjoin . fs)
      (lambda (x)
        (do ((fs fs (cdr fs)))
            ((or (null? fs) (not ((car fs) x))) (null? fs)))))
    
    ;; Returns a function invoking all functions `fs` and combining the results with `or`.
    ;; `((disjoin f1 f2 ...) x ...)` is equivalent to `(or (f1 x ...) (f2 x ...) ...)`.
    (define (disjoin . fs)
      (lambda (x)
        (do ((fs fs (cdr fs)))
            ((or (null? fs) ((car fs) x)) (not (null? fs))))))
    
    ;; Given a predicate `f`, `(list-of? f)` returns a predicate which takes a list as its
    ;; argument and returns `#t` if for every element `x` of the list `(f x)` returns true.
    (define (list-of? f)
      (lambda (lst)
        (let loop ((lst lst))
          (cond ((null? lst)       #t)
                ((not (pair? lst)) #f)
                ((f (car lst))     (loop (cdr lst)))
                (else              #f)))))
    
    ;; `(each f ...)` returns a function which applies the functions `f` ... each individually
    ;; to its arguments in the given order, returning the result of the last function application.
    (define (each . fs)
      (cond ((null? fs)       void)
            ((null? (cdr fs)) (car fs))
            (else
              (lambda args
                (let loop ((fs fs))
                  (let ((h (car fs))
                        (t (cdr fs)))
                    (if (null? t)
                        (apply h args)
                        (begin (apply h args)
                               (loop t)))))))))
    
    (define-syntax %cut
      (syntax-rules (<> <...>)
        ((_ e? params args)
          (lambda params args))
        ((_ e? (params ...) (args ...) <> . rest)
          (%cut e? (params ... tmp) (args ... tmp) . rest))
        ((_ e? (params ...) (args ...) <...>)
          (%cut e? (params ... . tmp) (apply args ... tmp)))
        ((_ e? (params ...) (args ...) <...> . rest)
          (error "non-terminal <...> in cut"))
        ((_ #t (params ...) (args ...) x . rest)
          (let ((tmp x))
            (%cut #t (params ...) (args ... tmp) . rest)))
        ((_ #f (params ...) (args ...) x . rest)
          (%cut #f (params ...) (args ... x) . rest))))

    ;; Special form `cut` transforms a _cut-expr_ into a _lambda expression_ with as
    ;; many formal variables as there are slots `<>` in the list _slot-or-expr\*_.
    ;; The body of the resulting _lambda expression_ calls the first _slot-or-expr_
    ;; with arguments from _slot-or-expr\*_ in the order they appear. In case there is
    ;; a _rest-slot_ symbol, the resulting procedure is also of variable arity, and the
    ;; body calls the first _slot-or-expr_ with all arguments provided to the actual
    ;; call of the specialized procedure.
    ;;
    ;; ```
    ;; <cut-expr> = (cut <slot-or-expr> <slot-or-expr>*)
    ;;            | (cut <slot-or-expr> <slot-or-expr>* <...>) ; with "rest-slot"
    ;;            | (cute <slot-or-expr> <slot-or-expr>* ; evaluate non-slots at specialization time
    ;;            | (cute <slot-or-expr> <slot-or-expr>* <...>) ; with "rest-slot"
    ;; <slot-or-expr>	= <>   ; a "slot"
    ;;                | <expression> ; a "non-slot expression"
    ;; ```
    (define-syntax cut
      (syntax-rules ()
        ((_ args ...)
          (%cut #f () () args ...))))

    ;; Special form `cute` is similar to `cut`, except that it first binds new variables to
    ;; the result of evaluating the non-slot expressions (in an unspecific order) and then
    ;; substituting the variables for the non-slot expressions. In effect, `cut` evaluates
    ;; non-slot expressions at the time the resulting procedure is called, whereas `cute`
    ;; evaluates the non-slot expressions at the time the procedure is constructed.
    (define-syntax cute
      (syntax-rules ()
        ((_ args ...)
          (%cut #t () () args ...))))
    
    ;; _Y combinator_ for Scheme for building recursive functions.
    (define (Y f)
      ((lambda (g) (g g))
       (lambda (g) (f  (lambda a (apply (g g) a))))))
  )
)
