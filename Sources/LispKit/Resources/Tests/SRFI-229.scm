;;; SRFI 229 REGRESSION TEST SUITE
;;;
;;; This is the test suite for SRFI 229.
;;;
;;; Copyright © 2021 Marc Nieper-Wißkirchen. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2021 Matthias Zenger. All rights reserved.

(import (lispkit base)
        (lispkit test)
        (srfi 229))

(test-begin "SRFI 229: Tagged procedures")

(parameterize ((current-test-comparator eqv?))
  (define f
    (lambda/tag 42
        (x)
      (* x x)))

  (define f*
    (lambda/tag 43
        (x)
      (* x x)))

  (define g
    (let ((y 10))
      (lambda/tag y ()
        (set! y (+ y 1))
        y)))
        
  (define h
    (let ((box (vector #f)))
      (case-lambda/tag box
        (() (vector-ref box 0))
        ((val) (vector-set! box 0 val)))))

  (test-equal #t (procedure/tag? f))
  (test-equal 9 (f 3))
  (test-equal 42 (procedure-tag f))
  (test-equal #f (eqv? f f*))
  (test-equal 10 (procedure-tag g))
  (test-equal 10 (let ((y 9)) (procedure-tag g)))
  (test-equal 11 (g))
  (test-equal 10 (procedure-tag g))
  (h 1)
  (test-equal 1 (vector-ref (procedure-tag h) 0))
  (test-equal 1 (h))
)

(test-end)
