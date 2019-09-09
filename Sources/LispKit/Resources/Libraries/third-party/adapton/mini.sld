;;; THIRD-PARTY ADAPTON MINI
;;;
;;; This library implements _miniAdapton_, a minimal implementation of _Adapton_ developed
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

(define-library (third-party adapton mini)

  (export adapton-force
          adapton-memoize
          adapton-memoize-l
          define-amemo
          lambda-amemo
          define-amemo-l
          lambda-amemo-l
          adapt
          define-avar
          avar-get
          avar-set!
          adapton-ref
          make-athunk
          adapton?
          adapton-ref-set!)

  (import (lispkit base)
          (third-party adapton micro)
          (third-party adapton memoization))

  (begin

    (define adapton-force
      (let ((currently-adapting #f))
        (lambda (a)
          (let ((prev-adapting
                 currently-adapting))
            (set! currently-adapting a)
            (let ((result (adapton-compute a)))
              (set! currently-adapting
                    prev-adapting)
              (when currently-adapting
                    (adapton-add-dcg-edge!
                     currently-adapting
                     a))
              result)))))

    (define-syntax adapt
      (syntax-rules ()
        ((_ expr)
          (make-athunk (lambda () expr)))))

    (define (adapton-memoize-l f)
      (memoize (lambda x (adapt (apply f x)))))

    (define (adapton-memoize f)
      (let ((f* (adapton-memoize-l f)))
        (lambda x (adapton-force (apply f* x)))))

    (define-syntax lambda-amemo-l
      (syntax-rules ()
        ((_ (args ...) body ...)
          (let ((f* (adapton-memoize-l (lambda (args ...) body ...))))
            (lambda (args ...) (f* args ...))))))

    (define-syntax lambda-amemo
      (syntax-rules ()
        ((_ (args ...) body ...)
          (let ((f* (adapton-memoize (lambda (args ...) body ...))))
            (lambda (args ...) (f* args ...))))))

    (define-syntax define-amemo-l
      (syntax-rules ()
        ((_ (f args ...) body ...)
          (define f (lambda-amemo-l (args ...) body ...)))))

    (define-syntax define-amemo
      (syntax-rules ()
        ((_ (f args ...) body ...)
          (define f (lambda-amemo (args ...) body ...)))))

    (define-syntax define-avar
      (syntax-rules ()
        ((_ name expr)
          (define name (adapton-ref (adapt expr))))))

    (define (avar-get v)
      (adapton-force (adapton-force v)))

    (define-syntax avar-set!
      (syntax-rules ()
        ((_ v expr)
          (adapton-ref-set! v (adapt expr)))))
  )
)
