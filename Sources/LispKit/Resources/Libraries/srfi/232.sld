;;; SRFI 232
;;; Flexible curried procedures
;;;
;;; Scheme lacks a flexible way to create and apply curried procedures. This
;;; SRFI describes `curried`, a variant of lambda that creates true curried
;;; procedures which also behave just like ordinary Scheme procedures. They
;;; can be applied to their arguments one by one, all at once, or anywhere
;;; in between, without any novel syntax. `curried` also supports nullary and
;;; variadic procedures, and procedures created with it have predictable
;;; behavior when applied to surplus arguments.
;;; 
;;; Copyright © 2022 Wolfgang Corcoran-Mathe. All rights reserved.
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
;;;   Copyright © 2022 Matthias Zenger. All rights reserved.

(define-library (srfi 232)
  
  (export curried
          define-curried)
  
  (import (lispkit base))
  
  (begin
    
    (define (more-args f current)
      (lambda args (apply f (append current args))))
    
    (define-syntax one-or-more
      (syntax-rules ()
        ((one-or-more (arg0 arg1 ...) exp)
          (letrec
            ((f (case-lambda
                  (() f)
                  ((arg0 arg1 ...) exp)
                  ((arg0 arg1 ... . rest) (apply (f arg0 arg1 ...) rest))
                  (args (more-args f args)))))
            f))))
    
    (define-syntax rest-args
      (syntax-rules ()
        ((rest-args (arg0 arg1 ... . rest) exp)
          (letrec ((f (case-lambda
                        (() f)
                        ((arg0 arg1 ... . rest) exp)
                        (args (more-args f args)))))
            f))))
    
    (define-syntax curried-1
      (syntax-rules ()
        ((curried-1 () exp) exp)
        ((curried-1 (arg0 arg1 ...) exp)
          (one-or-more (arg0 arg1 ...) exp))
        ((curried-1 (arg0 arg1 ... . rest) exp)
          (rest-args (arg0 arg1 ... . rest) exp))
        ((curried-1 args exp)
          (lambda args exp))))
    
    (define-syntax curried
      (syntax-rules ()
        ((curried formals exp ...)
          (curried-1 formals (begin exp ...)))))
    
    (define-syntax define-curried
      (syntax-rules ()
        ((define-curried (var . formals) exp ...)
          (define var
            (curried formals exp ...)))))
  )
)
