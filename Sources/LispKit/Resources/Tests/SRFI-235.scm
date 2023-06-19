;;; SRFI 235 REGRESSION TEST SUITE
;;;
;;; This is the test suite for SRFI 235.
;;;
;;; Copyright © 2023 Arvydas Silanskas. All rights reserved.
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
;;;   Copyright © 2023 Matthias Zenger. All rights reserved.

(import (lispkit base)
        (lispkit test)
        (srfi 235)
        (srfi 1))

(test-begin "Combinators")

(test-group
  "Combinators: constantly"
  (test '(1 2)
              (call-with-values (lambda () ((constantly 1 2) 'a 'b)) list))
  (test '(1)
              (call-with-values
                (lambda () ((constantly 1) 'a 'b))
                list))
  (test '()
              (call-with-values
                (lambda () ((constantly) 'a 'b))
                list)))

(test-group
  "Combinators: complement, swap, flip, on-left, on-right"
  (test #f ((complement symbol?) 'a))
  (test #t ((complement symbol?) 1))
  (test '(2 1 3 4) ((swap list) 1 2 3 4))
  (test '(4 3 2 1) ((flip list) 1 2 3 4))
  (test '(1) ((on-left list) 1 2))
  (test '(2) ((on-right list) 1 2)))

(test-group
  "Combinators: conjoin"
  (test-assert ((conjoin number? exact?)))
  (test-assert ((conjoin number? exact?) 1 2))
  (test-assert (not ((conjoin number? exact?) 1 2.)))
  (test-assert ((conjoin) 1 2)))

(test-group
  "Combinators: disjoin"
  (test-assert ((disjoin number? string?)))
  (test-assert ((disjoin number? string?) 1 "a"))
  (test-assert (not ((disjoin number? string?) 'a 'b)))
  (test-assert (not ((disjoin) 1 2))))

(test-group
  "Combinators: each-of"
  (let ((r1 #f)
        (r2 #f))
    ((each-of
       (lambda args (set! r1 args))
       (lambda args (set! r2 args)))
     1 2)
    (test r1 '(1 2))
    (test r2 '(1 2))))

(test-group
  "Combinators: all-of"
  (test-assert ((all-of string?) '()))
  (test-assert ((all-of string?) '("a" "b")))
  (test "b" ((all-of values) '("a" "b")))
  (test-assert (not ((all-of string?) '("a" b))))
  (test-assert
    (not ((all-of (lambda (x)
                    (when (equal? x 'c)
                      ;; should short circuit before this point
                      (test-assert #f))
                    (string? x)))
          '("a" b c)))))

(test-group
  "Combinators: any-of"
  (test-assert (not ((any-of string?) '())))
  (test-assert ((any-of string?) '("a" b)))
  (test "a" ((any-of values) '("a" "b")))
  (test-assert (not ((any-of string?) '(a b))))
  (test-assert ((any-of (lambda (x)
                          (when (equal? x 'b)
                            ;; should short circuit before this point
                            (test-assert #f))
                          (string? x)))
                '("a" b))))

(test-group
  "Combinators: on, left-section, right-section"
  (test '(2 3 4) ((on list (lambda (x) (+ 1 x))) 1 2 3))
  (test '(1 2 3 4) ((left-section list 1 2) 3 4))
  (test '(3 4 2 1) ((right-section list 1 2) 3 4)))

(test-group
  "Combinators: apply-chain"
  (define cadr* (apply-chain car cdr))
  (define factorial ;;test multivalue
    (apply-chain * (lambda (n) (apply values (cdr (iota (+ 1 n)))))))
  (test 2 (cadr* (list 1 2 3)))
  (test 120 (factorial 5)))

(test-group
  "Combinators: arguments-drop, arguments-drop-right, arguments-take, arguments-take-right"
  (test '(4) ((arguments-drop list 3) 1 2 3 4))
  (test '(1) ((arguments-drop-right list 3) 1 2 3 4))
  (test '(1 2 3) ((arguments-take list 3) 1 2 3 4))
  (test '(2 3 4) ((arguments-take-right list 3) 1 2 3 4)))

(test-group
  "Combinators: group-by"
  (test-equal
    '((1 3)
      (2 4))
    ((group-by odd?) '(1 2 3 4)))
  (test-equal
    '(("aa" "ab")
      ("ba" "bb"))
    ((group-by (lambda (str) (string-ref str 0))
               char=?)
     (list "aa" "ba" "bb" "ab"))))

(test-group
  "Combinators: begin-procedure, if-procedure, when-procedure, unless-procedure"
  (define lst1 '())
  (define lst2 '())
  (test 2 (begin-procedure (lambda () 1) (lambda () 2)))
  (test 1 (if-procedure #t (lambda () 1) (lambda () (test-assert #f))))
  (test 2 (if-procedure #f (lambda () (test-assert #f)) (lambda () 2)))
  (when-procedure #t
                  (lambda () (set! lst1 (cons 1 lst1)))
                  (lambda () (set! lst1 (cons 2 lst1))))
  (when-procedure #f
                  (lambda () (set! lst2 (cons 1 lst2)))
                  (lambda () (set! lst2 (cons 2 lst2))))
  (test '(2 1) lst1)
  (test '() lst2)
  (set! lst1 '())
  (set! lst2 '())
  (unless-procedure #t
                    (lambda () (set! lst1 (cons 1 lst1)))
                    (lambda () (set! lst1 (cons 2 lst1))))
  (unless-procedure #f
                    (lambda () (set! lst2 (cons 1 lst2)))
                    (lambda () (set! lst2 (cons 2 lst2))))
  (test '() lst1)
  (test '(2 1) lst2))

(test-group
  "Combinators: value-procedure"
  (test "1" (value-procedure 1 number->string (lambda () (test-assert #f))))
  (test 2 (value-procedure #f (lambda args (test-assert #f)) (lambda () 2))))

(test-group
  "Combinators: case-procedure"
  (test 2 (case-procedure 'b
                                `((a . ,(lambda () 1))
                                  (b . ,(lambda () 2)))))
  (test 3
              (case-procedure 'c
                              `((a . ,(lambda () 1))
                                (b . ,(lambda () 2)))
                              (lambda () 3))))

(test-group
  "Combinators: and-procedure"
  (test-assert (and-procedure))
  (test 2 (and-procedure (lambda () 1) (lambda () 2)))
  (test-assert (not (and-procedure (lambda () #f)
                                   (lambda () (test-assert #f))))))

(test-group
  "Combinators: eager-and-procedure"
  (test-assert (eager-and-procedure))
  (test 2 (eager-and-procedure (lambda () 1) (lambda () 2)))
  (let ((second-called? #f))
    (test-assert
       (not (eager-and-procedure (lambda () #f)
                                 (lambda ()
                                   (set! second-called? #t)
                                   #t))))
    (test-assert second-called?)))

(test-group
  "Combinators: or-procedure"
  (test-assert (not (or-procedure)))
  (test 2 (or-procedure (lambda () #f) (lambda () 2)))
  (test-assert (or-procedure (lambda () 1) (lambda () (test-assert #f)))))

(test-group
  "Combinators: eager-or-procedure"
  (test-assert (not (eager-or-procedure)))
  (test 2  (eager-or-procedure (lambda () #f) (lambda () 2)))
  (let ((second-called? #f))
    (test 1
                (eager-or-procedure (lambda () 1)
                                    (lambda () (set! second-called? #t) #f)))
    (test-assert second-called?)))

(test-group
  "Combinators: funcall-procedure, loop-procedure"
  (test 1 (funcall-procedure (lambda () 1)))
  (call/cc (lambda (k)
             (define v 0)
             (define (thunk)
               (when (> v 5)
                 (k #t))
               (set! v (+ 1 v)))
             (loop-procedure thunk)
             (test-assert #t))))

(test-group
  "Combinators: while-procedure, until-procedure"
  (define v 0)
  (define (thunk) (set! v (+ 1 v)) (< v 5))
  (define (thunk2) (set! v (+ 1 v)) (>= v 5))
  (while-procedure thunk)
  (test 5 v)
  (set! v 0)
  (until-procedure thunk2)
  (test 5 v))

(test-group
  "Combinators: always, never, boolean, values"
  (test-assert (always))
  (test-assert (always 'a))
  (test-assert (not (never)))
  (test-assert (not (never 'a)))
  (test #t (boolean 1))
  (test #f (boolean #f))
  (test 1 (values 1))
  (test 'a (values 'a)))

(test-end)
