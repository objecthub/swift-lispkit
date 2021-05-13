;;; SRFI 219 REGRESSION TEST SUITE
;;;
;;; This is the test suite for SRFI 219.
;;;
;;; Copyright © 2021 Lassi Kortela. All rights reserved.
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
        (srfi 219))

(test-begin "SRFI 219: Define higher-order lambda")

(let ()
  (define ((greet/prefix prefix) suffix)
    (string-append prefix " " suffix))
  (let ((greet (greet/prefix "Hello")))
    (test-equal "Hello there!" (greet "there!"))))

(let ()
  (define ((append-to . a) . b)
    (apply append (append a b)))
  (test-equal '()
    ((append-to '()) '()))
  (test-equal '(1 2 3 4 5 6 7 8)
    ((append-to '(1 2) '(3 4)) '(5 6) '(7 8))))

(let ()
  (define (((jenga a b) c d))
    (list a b c d))
  (test-equal '(1 2 3 4)
    (((jenga 1 2) 3 4))))

(test-end)
