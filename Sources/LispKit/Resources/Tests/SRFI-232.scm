;;; SRFI 232 REGRESSION TEST SUITE
;;;
;;; This is the test suite for SRFI 232.
;;;
;;; Copyright © 2022 Wolfgang Corcoran-Mathe. All rights reserved.
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
;;;   Copyright © 2022 Matthias Zenger. All rights reserved.

(import (lispkit base)
        (lispkit test)
        (srfi 232))

(define-syntax test-equal
  (syntax-rules ()
    ((test-equal . rest)
      (test . rest))))

(define-syntax test-eqv
  (syntax-rules ()
    ((test-eqv . rest)
      (test-equal eqv? . rest))))

(test-begin "SRFI 232: Flexible curried procedures")

(test-group "Simple currying"
  (test-eqv 5 ((curried (x y) (+ x y)) 2 3))
  (test-eqv 5 (((curried (x y) (+ x y)) 2) 3))
  (test-eqv 5 ((curried (w x y z) (+ w x y z)) 1 1 1 2))
  (test-eqv 5 ((((curried (w x y z) (+ w x y z)) 1) 1) 1 2))
  (test-eqv 5 (((curried (w x y z) (+ w x y z)) 1) 1 1 2))
  (test-eqv 5 (((curried (w x y z) (+ w x y z)) 1 1) 1 2))
  (test-eqv 5 (((curried (w x y z) (+ w x y z)) 1 1 1) 2))
  (test-eqv 5 (((((curried (w x y z) (+ w x y z)) 1) 1) 1) 2))
)

(test-group "Variadic"
  (test-equal '(3 (3 4))
              ((curried (a b . rest) (list (+ a b) rest)) 1 2 3 4))
  (test-equal
   '(3 (3 4))
   (((curried (a b . rest) (list (+ a b) rest)) 1) 2 3 4))
  (test-equal '(3 ()) ((curried (a b . rest) (list (+ a b) rest)) 1 2))
)

(test-group "Nullary"
  (test-eqv 3 ((curried () (curried (x y) (+ x y))) 1 2))
  (test-eqv 3 (((curried () (curried (x y) (+ x y))) 1) 2))

  ;; "... while these behaviors are decidedly not wrong, they are
  ;;  perhaps mildly unsettling."
  (test-eqv 2
            ((curried (a)
               (curried ()
                 (curried ()
                   (curried (b) b)))) 1 2))
  (test-eqv 4 (((((((((curried (a b c) 4)))))))) 1 2 3))
)

(test-group "Extra arguments"
  (test-eqv 20 ((curried (x y) (curried (z) (* z (+ x y)))) 2 3 4))
  (test-eqv 20 (((curried (x y) (curried (z) (* z (+ x y)))) 2) 3 4))
)

(test-end)
