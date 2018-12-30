;;; SRFI 101 REGRESSION TEST SUITE
;;;
;;; This is the test suite for SRFI 101.
;;;
;;; Copyright © 2009 David Van Horn. All rights reserved.
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
;;;   Copyright © 2019 Matthias Zenger. All rights reserved.

(import (lispkit base)
        (lispkit test)
        (prefix (srfi 101) r:))

; quote pair? cons car cdr caar cadr cddr cdar null? list? list
; make-list length append reverse list-tail list-ref map for-each

(test-begin "SRFI 101: Random access lists")

(test-assert (let ((f (lambda () (r:quote x)))) (eq? (f) (f))))

(test (r:quote (1 2 3)) (r:list 1 2 3))

;; pair?
(test-assert (r:pair? (r:cons 'a 'b)))
(test-assert (r:pair? (r:list 'a 'b 'c)))
(test-not (r:pair? (r:quote ())))
(test-not (r:pair? (r:quote #(a b))))

;; cons
(test (r:cons 'a '()) (r:list 'a))
(test (r:cons (r:list 'a) (r:list 'b 'c 'd)) (r:list (r:list 'a) 'b 'c 'd))
(test (r:cons "a" (r:list 'b 'c)) (r:list "a" 'b 'c))
(test (r:cons 'a 3) (r:cons 'a 3))
(test (r:cons (r:list 'a 'b) 'c) (r:cons (r:list 'a 'b) 'c))

;; car
(test 'a (r:car (r:list 'a 'b 'c)))
(test (r:list 'a) (r:car (r:list (r:list 'a) 'b 'c 'd)))
(test 1 (r:car (r:cons 1 1)))
(test-error (r:car '()))

;; cdr
(test (r:list 'b 'c 'd) (r:cdr (r:list (r:list 'a) 'b 'c 'd)))
(test 2 (r:cdr (r:cons 1 2)))
(test-error (r:cdr '()))

;; null?
(test-assert (eq? r:null? null?))
(test-assert (r:null? (r:quote ())))
(test-not (r:null? (r:cons 1 2)))
(test-not (r:null? 4))

;; list?
(test-assert (r:list? (r:list 'a 'b 'c)))
(test-assert (r:list? (r:quote ())))
(test-not (r:list? (r:cons 'a 'b)))

;; list
(test (r:list 'a 7 'c) (r:list 'a (+ 3 4) 'c))
(test (r:quote ()) (r:list))

;; make-list
(test 5 (r:length (r:make-list 5)))
(test (r:list 0 0 0 0 0) (r:make-list 5 0))

;; length
(test 3 (r:length (r:list 'a 'b 'c)))
(test 3 (r:length (r:list 'a (r:list 'b) (r:list 'c))))
(test 0 (r:length (r:quote ())))

;; append
(test (r:list 'x 'y) (r:append (r:list 'x) (r:list 'y)))
(test (r:list 'a 'b 'c 'd) (r:append (r:list 'a) (r:list 'b 'c 'd)))
(test (r:list 'a (r:list 'b) (r:list 'c)) (r:append (r:list 'a (r:list 'b)) (r:list (r:list 'c))))
(test (r:cons 'a (r:cons 'b (r:cons 'c 'd))) (r:append (r:list 'a 'b) (r:cons 'c 'd)))
(test 'a (r:append (r:quote ()) 'a))

;; reverse
(test (r:list 'c 'b 'a) (r:reverse (r:list 'a 'b 'c)))
(test (r:list (r:list 'e (r:list 'f)) 'd (r:list 'b 'c) 'a)
      (r:reverse (r:list 'a (r:list 'b 'c) 'd (r:list 'e (r:list 'f)))))

;; list-tail
(test (r:list 'c 'd) (r:list-tail (r:list 'a 'b 'c 'd) 2))

;; list-ref
(test 'c (r:list-ref (r:list 'a 'b 'c 'd) 2))

;; list-set
(test (r:list 'a 'b 'x 'd) (r:list-set (r:list 'a 'b 'c 'd) 2 'x))

;; list-ref/update
(let-values (((a b) (r:list-ref/update (r:list 7 8 9 10) 2 -)))
  (test 9 a)
  (test (r:list 7 8 -9 10) (values b)))

;; map
(test (r:list 'b 'e 'h) (r:map r:cadr (r:list (r:list 'a 'b) (r:list 'd 'e) (r:list 'g 'h))))
(test (r:list 1 4 27 256 3125) (r:map (lambda (n) (expt n n)) (r:list 1 2 3 4 5)))
(test (r:list 5 7 9) (r:map + (r:list 1 2 3) (r:list 4 5 6)))

;; for-each
(test '#(0 1 4 9 16)
    (let ((v (make-vector 5)))
        (r:for-each (lambda (i) (vector-set! v i (* i i))) (r:list 0 1 2 3 4))
        v))

;; random-access-list->linear-access-list
;; linear-access-list->random-access-list
(test (r:quote ()) (r:random-access-list->linear-access-list (r:quote ())))
(test (r:quote ()) (r:linear-access-list->random-access-list '()))
(test (list 1 2 3) (r:random-access-list->linear-access-list (r:list 1 2 3)))
(test (r:list 1 2 3) (r:linear-access-list->random-access-list (list 1 2 3)))

(test-end)
