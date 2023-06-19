;;; SRFI 235
;;; Combinators
;;; 
;;; This SRFI contains various procedures that accept and return procedures,
;;; as well as a few others, drawn from an earlier version of Chicken.
;;; Common Lisp has a few of them too, and more come from the Standard Prelude
;;; from Programming Praxis. Using these procedures helps to keep code terse
;;; and reduce the need for ad hoc lambdas.
;;; 
;;; Many procedures such as map, filter, fold, and their equivalents for data
;;; structures other than lists accept an argument specifying the behavior to
;;; be performed on each element of the data structure. This can be done in
;;; one of two ways:
;;; 
;;;   - The code uses a lambda to describe the needed behavior
;;;   - The code is placed in an internal or external define, which requires
;;;     finding a good name and splits the action into different locations,
;;;     making the code harder to read.
;;; 
;;; Those composition procedures, called combinators, have been identified by
;;; the Scheme and Common Lisp communities as reducing such fragmentation while
;;; keeping the code dense.
;;; 
;;; Author of spec: John Cowan
;;;
;;; Copyright © 2023 Arvydas Silanskas. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; Adaptation to LispKit
;;;   Copyright © 2023 Matthias Zenger. All rights reserved.

(define-library (srfi 235)
  
  (export constantly
          complement
          swap
          flip
          on-left
          on-right
          conjoin
          disjoin
          each-of
          all-of
          any-of
          on
          left-section
          right-section
          apply-chain
          arguments-drop
          arguments-drop-right
          arguments-take
          arguments-take-right
          group-by
          begin-procedure
          if-procedure
          when-procedure
          unless-procedure
          value-procedure
          case-procedure
          and-procedure
          eager-and-procedure
          or-procedure
          eager-or-procedure
          funcall-procedure
          loop-procedure
          while-procedure
          until-procedure
          always
          never
          boolean)
  
  (import (lispkit base)
          (srfi 1))
  
  (begin
    (define (constantly . args)
      (lambda ignored-args
        (apply values args)))
    
    (define (complement proc)
      (lambda (obj)
        (not (proc obj))))
    
    (define (swap proc)
      (lambda (obj1 obj2 . rest)
        (apply proc obj2 obj1 rest)))
    
    (define (flip proc)
      (lambda args
        (apply proc (reverse args))))
    
    (define (on-left proc)
      (lambda (obj1 obj2)
        (proc obj1)))
    
    (define (on-right proc)
      (lambda (obj1 obj2)
        (proc obj2)))
    
    (define (conjoin . predicates)
      (case-lambda
        (() #t)
        (args (let loop-args ((args args))
                (if (null? args)
                    #t
                    (let ((arg (car args)))
                      (let loop-preds ((predicates predicates))
                        (cond
                          ((null? predicates) (loop-args (cdr args)))
                          ((not ((car predicates) arg)) #f)
                          (else (loop-preds (cdr predicates)))))))))))
    
    (define (disjoin . predicates)
      (case-lambda
        (() #t)
        (args (let loop-args ((args args))
                (if (null? args)
                    #t
                    (let ((arg (car args)))
                      (let loop-preds ((predicates predicates))
                        (cond
                          ((null? predicates) #f)
                          (((car predicates) arg) (loop-args (cdr args)))
                          (else (loop-preds (cdr predicates)))))))))))
    
    (define (each-of . procs)
      (lambda args
        (for-each
          (lambda (proc) (apply proc args))
          procs)))
    
    (define (all-of predicate)
      (lambda (lst)
        (let loop ((lst lst)
                   (last #t))
          (cond
            ((null? lst) last)
            ((predicate (car lst)) => (lambda (value)
                                        (loop (cdr lst) value)))
            (else #f)))))
    
    (define (any-of predicate)
      (lambda (lst)
        (if (null? lst)
            #f
            (let loop ((lst lst))
              (cond
                ((null? lst) #f)
                ((predicate (car lst)))
                (else (loop (cdr lst))))))))
    
    (define (on reducer mapper)
      (lambda objs
        (apply reducer (map mapper objs))))
    
    (define (left-section proc . args)
      (lambda objs
        (apply proc (append args objs))))
    
    (define (right-section proc . args)
      (let ((args-reverse (reverse args)))
        (lambda objs
          (apply proc (append objs args-reverse)))))
    
    (define (apply-chain . procs)
      (define procs/rev (reverse procs))
      (lambda args
        (let loop ((values-provider (lambda () (apply values args)))
                   (procs procs/rev))
          (if (null? procs)
              (values-provider)
              (loop (lambda ()
                      (call-with-values
                        values-provider
                        (car procs)))
                    (cdr procs))))))
    
    (define (arguments-drop/take proc drop/take n)
      (lambda args
        (apply proc (drop/take args n))))
    
    (define (arguments-drop proc n)
      (arguments-drop/take proc drop n))
    
    (define (arguments-drop-right proc n)
      (arguments-drop/take proc drop-right n))
    
    (define (arguments-take proc n)
      (arguments-drop/take proc take n))
    
    (define (arguments-take-right proc n)
      (arguments-drop/take proc take-right n))
    
    (define group-by
      (case-lambda
        ((key-proc)
          (group-by key-proc equal?))
        ((key-proc =)
          (lambda (lst)
            (let loop ((lst lst)
                       (res '()))
              (if (null? lst)
                  (map reverse res)
                  (loop (cdr lst) (let inner ((elem (car lst))
                                              (groups res))
                                    (if (null? groups)
                                        (list (list elem))
                                        (if (= (key-proc elem) (key-proc (caar groups)))
                                            (cons (cons elem (car groups)) (cdr groups))
                                            (cons (car groups) (inner elem (cdr groups)))))))))))))
    
    (define (begin-procedure . thunks)
      (let loop ((value (if #f #f))
                 (thunks thunks))
        (if (null? thunks)
            value
            (loop ((car thunks))
                  (cdr thunks)))))
    
    (define (if-procedure value then-thunk else-thunk)
      (if value
          (then-thunk)
          (else-thunk)))
    
    (define (when-procedure value . thunks)
      (when value
        (for-each
          (lambda (fn) (fn))
          thunks)))
    
    (define (unless-procedure value . thunks)
      (unless value
        (for-each
          (lambda (fn) (fn))
          thunks)))
    
    (define (value-procedure value then-proc else-thunk)
      (if value
          (then-proc value)
          (else-thunk)))
    
    (define case-procedure
      (case-lambda
        ((value thunk-alist)
          (case-procedure value thunk-alist (lambda args (if #f #f))))
        ((value thunk-alist else-thunk)
          (cond
            ((assv value thunk-alist) => (lambda (entry)
                                           ((cdr entry))))
            (else (else-thunk))))))
    
    (define and-procedure
      (case-lambda
        (() #t)
        (thunks (let loop ((thunks thunks))
                  (cond
                    ((null? (cdr thunks)) ((car thunks)))
                    ((not ((car thunks))) #f)
                    (else (loop (cdr thunks))))))))
    
    (define eager-and-procedure
      (case-lambda
        (() #t)
        (thunks (let loop ((thunks thunks)
                           (result #t))
                  (cond
                    ((null? (cdr thunks)) (let ((r ((car thunks))))
                                            (and result r)))
                    ((not ((car thunks))) (loop (cdr thunks) #f))
                    (else (loop (cdr thunks) result)))))))
    
    (define or-procedure
      (case-lambda
        (() #f)
        (thunks (let loop ((thunks thunks))
                  (cond
                    ((null? thunks) #f)
                    (((car thunks)) => values)
                    (else (loop (cdr thunks))))))))
    
    (define eager-or-procedure
      (case-lambda
        (() #f)
        (thunks (let loop ((thunks thunks)
                           (result #f))
                  (cond
                    ((null? thunks) result)
                    (((car thunks)) => (lambda (res)
                                         (loop (cdr thunks)
                                               (or result
                                                   res))))
                    (else (loop (cdr thunks) result)))))))
    
    (define (funcall-procedure thunk)
      (thunk))
    
    (define (loop-procedure thunk)
      (thunk)
      (loop-procedure thunk))
    
    (define (while-procedure thunk)
      (if (thunk)
          (while-procedure thunk)
          #f))
    
    (define (until-procedure thunk)
      (define v (thunk))
      (if v
          v
          (until-procedure thunk)))
    
    (define (always . args) #t)
    
    (define (never . args) #f)
    
    (define (boolean obj)
      (if obj #t #f))
    )
  )
