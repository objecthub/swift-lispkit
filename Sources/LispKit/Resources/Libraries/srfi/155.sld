;;; SRFI 155
;;; Promises
;;;
;;; Scheme, like ML, is a programming language with strict evaluation while others,
;;; like Haskell, use lazy evaluation. Scheme, however, possesses the primitives delay
;;; and force that make it possible to express lazy algorithms.
;;; 
;;; Lazy evaluation does not go well in conjunction with imperative, non-functional,
;;; side-effecting code. It should, however, be applicable in a purely functional setting.
;;; This is the case for the delayed evaluation model as described in the R7RS as long as
;;; no dynamically bound variables, also known as parameter objects, are present. It is
;;; the purpose of this SRFI to rework the specification in the R7RS so that lazy
;;; evaluation works with purely functional code that makes use of dynamic environments
;;; or, more generally, the dynamic extent. This is done by remembering the dynamic extent
;;; in effect when the delay expression is evaluated.
;;; 
;;; Another perceived misfeature of the R7RS model of delayed evaluation is the apparent
;;; need of the `delay-force` special form to express iterative lazy algorithms. It is
;;; shown that the `delay-force` special form is unneeded and that the implementation can
;;; (and should) handle iterative lazy algorithms without space leaks.
;;; 
;;; Copyright © 2017 Marc Nieper-Wißkirchen. All rights reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software, and to permit
;;; persons to whom the Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies or
;;; substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;;; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2021 Matthias Zenger. All rights reserved.

(define-library (srfi 155)
  
  (export delay
          delay-force
          force
          make-promise
          promise?
          forcing-extent)
  
  (import (rename (lispkit base)
            (delay scheme-delay)
            (delay-force scheme-delay-force))
          (srfi 154))
  
  (begin
    
    (define current-forcing-extent (make-parameter #f))
    
    (define current-extents (make-parameter (vector #f #f)))
    
    (define (forcing-extent)
      (unless (current-forcing-extent)
        (error "forcing-extent: there is no promise being forced"))
      (current-forcing-extent))
    
    (define-syntax delay
      (syntax-rules (force)
        ((delay (force expression))
          (delay-force expression))
        ((delay expression)
          (let ((dynamic-extent
                  (if (and (vector-ref (current-extents) 1)
                           (eq? (current-dynamic-extent) (vector-ref (current-extents) 1)))
                      (vector-ref (current-extents) 0)
                      (current-dynamic-extent))))
            (scheme-delay
              (let ((forcing-extent (current-dynamic-extent)))
                (with-dynamic-extent dynamic-extent
                                     (lambda ()
                                       (let ((extents (vector dynamic-extent #f)))
                                         (parameterize ((current-extents extents)
                                                        (current-forcing-extent forcing-extent))
                                           (vector-set! extents 1 (current-dynamic-extent))
                                           expression))))))))))
    
    (define-syntax delay-force
      (syntax-rules ()
        ((delay-force expression)
          (let ((dynamic-extent (current-dynamic-extent)))
            (scheme-delay-force
              (with-dynamic-extent dynamic-extent (lambda () expression)))))))
  )
)
