;;; THIRD-PARTY ADAPTON MEMOIZATION
;;;
;;; This library implements memoization and is a part of the _miniAdapton_ system developed
;;; by Dakota Fisher, William Byrd, Matthew A. Hammer, and Matthew Might. The implementation
;;; is described in their paper "miniAdapton: A Minimal Implementation of Incremental
;;; Computation in Scheme".
;;;
;;; The MIT License (MIT)
;;; Copyright © 2016-2017 Dakota Fisher and William Byrd
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify,
;;; merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies
;;; or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; Adaptation to LispKit
;;;   Copyright © 2019 Matthias Zenger. All rights reserved.

(define-library (third-party adapton memoization)

  (export memoize
          lambda-memo
          define-memo)

  (import (lispkit base))

  (begin

    (define (make-kv-store) (list '()))

    (define (add-kv-store! s k v)
      (set-car! s `((,k . ,v) . ,(car s))))

    (define (lookup-kv-store s k)
      (assoc k (car s)))

    (define (memoize f)
      (let ((s (make-kv-store)))
        (lambda x
          (let ((k/v (lookup-kv-store s x)))
            (if k/v
                (cdr k/v)
                (let ((result (apply f x)))
                  (add-kv-store! s x result)
                  result))))))

    (define-syntax lambda-memo
      (syntax-rules ()
        ((_ (args ...) body ...)
          (let ((f* (memoize (lambda (args ...) body ...))))
            (lambda (args ...) (f* args ...))))))

    (define-syntax define-memo
      (syntax-rules ()
        ((_ (f args ...) body ...)
          (define f (lambda-memo (args ...) body ...)))))
  )
)
