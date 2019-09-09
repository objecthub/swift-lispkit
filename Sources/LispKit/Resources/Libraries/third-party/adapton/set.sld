;;; THIRD-PARTY ADAPTON SET
;;;
;;; This library implements sets and is a part of the _miniAdapton_ system developed by
;;; Dakota Fisher, William Byrd, Matthew A. Hammer, and Matthew Might. The implementation
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

(define-library (third-party adapton set)

  (export empty-set
          set-mem
          set-cons
          set-rem
          set-union
          set-intersect
          set-for-each
          set->list)

  (import (lispkit base)
          (only (srfi 1) fold))

  (begin

    (define empty-set '())

    (define (set-mem e s)
      (memv e s))

    (define (set-cons e s)
      (if (set-mem e s) s (cons e s)))

    (define (set-rem e s)
      (filter (lambda (x) (not (eqv? e x))) s))

    (define (set-union s1 s2)
      (fold set-cons s2 s1))

    (define (set-intersect s1 s2)
      (fold set-rem s2 s1))

    (define set-for-each for-each)

    (define set->list identity)
  )
)
