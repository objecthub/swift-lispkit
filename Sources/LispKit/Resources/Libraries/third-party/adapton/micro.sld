;;; THIRD-PARTY ADAPTON MICRO
;;;
;;; This library implements _microAdapton_ and is a part of the _miniAdapton_ system
;;; developed by Dakota Fisher, William Byrd, Matthew A. Hammer, and Matthew Might.
;;; The implementation is described in their paper "miniAdapton: A Minimal Implementation
;;; of Incremental Computation in Scheme".
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

(define-library (third-party adapton micro)

  (export adapton?
          adapton-compute
          adapton-ref
          adapton-ref-set!
          adapton-add-dcg-edge!
          adapton-del-dcg-edge!
          make-athunk)

  (import (lispkit base)
          (third-party adapton set))

  (begin
    (define-record-type adapton
      (adapton-cons thunk result sub super clean?)
      adapton?
      (thunk adapton-thunk)
      (result adapton-result adapton-result-set!)
      (sub adapton-sub adapton-sub-set!)
      (super adapton-super adapton-super-set!)
      (clean? adapton-clean? adapton-clean?-set!))

    (define (make-athunk thunk)
      (adapton-cons thunk 'empty empty-set empty-set #f))

    (define (adapton-add-dcg-edge! a-super a-sub)
      (adapton-sub-set! a-super (set-cons a-sub (adapton-sub a-super)))
      (adapton-super-set! a-sub (set-cons a-super (adapton-super a-sub))))

    (define (adapton-del-dcg-edge! a-super a-sub)
      (adapton-sub-set! a-super (set-rem a-sub (adapton-sub a-super)))
      (adapton-super-set! a-sub (set-rem a-super (adapton-super a-sub))))

    (define (adapton-compute a)
      (if (adapton-clean? a)
          (adapton-result a)
          (begin
            (set-for-each (lambda (x) (adapton-del-dcg-edge! a x)) (adapton-sub a))
            (adapton-clean?-set! a #t)
            (adapton-result-set! a ((adapton-thunk a)))
            (adapton-compute a))))

    (define (adapton-dirty! a)
      (when (adapton-clean? a)
            (adapton-clean?-set! a #f)
            (set-for-each adapton-dirty! (adapton-super a))))

    (define (adapton-ref val)
      (letrec ((a (adapton-cons
                   (lambda () (adapton-result a))
                   val
                   empty-set
                   empty-set
                   #t)))
        a))

    (define (adapton-ref-set! a val)
      (adapton-result-set! a val)
      (adapton-dirty! a))
  )
)
