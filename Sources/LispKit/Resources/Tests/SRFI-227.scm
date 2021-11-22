;;; SRFI 227 REGRESSION TEST SUITE
;;;
;;; This is the test suite for SRFI 227.
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
        (srfi 227))

(test-begin "SRFI 227: Optional arguments")

(define f
  (opt*-lambda (x (y 1) (z (* x x)))
    (list x y z)))

(test-equal '(1 2 3) (f 1 2 3))
(test-equal '(2 3 4) (f 2 3))
(test-equal '(2 1 4) (f 2))

(define g
  (let ((x 4))
    (opt-lambda (x (y 1) (z (* x x)))
      (list x y z))))

(test-equal '(1 2 3) (g 1 2 3))
(test-equal '(2 3 16) (g 2 3))
(test-equal '(2 1 16) (g 2))

(define h
  (opt-lambda args args))

(test-equal '(1 2) (h 1 2))

(test-equal '(1 (2))
    (let-optionals
        '(1 2)
        (x . y)
      (list x y)))

(test-equal '(1 2 3)
    (let-optionals
        '(1)
        (x (y 2) (z 3))
      (list x y z)))

(test-equal '(1 3 4)
    (let-optionals*
        '(1 3)
        (x (y 2) (z (+ x y)))
        (list x y z)))

(test-equal '(0 1)
                (let* ()
                  (define-optionals (f x (y 1))
                    (list x y))
                  (f 0)))

(test-equal '(3 9 ())
                (let* ()
                  (define-optionals* (f x (y (* x x)) . z)
                    (list x y z))
                  (f 3)))

(test-end)
