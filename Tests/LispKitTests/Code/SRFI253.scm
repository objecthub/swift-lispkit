;;; SRFI253.scm
;;; Regression test data
;;;
;;; This test suite is based on the SRFI 253 test suite by Artyom Bologov. The tests for
;;; rest arguments, which the sample implementation of the SRFI does not support, are
;;; enabled. Additional tests at the end cover error reporting, return value checks,
;;; arrow clauses in `check-case`, internal definitions, and record constructors whose
;;; arguments are a subset of the fields in a different order.
;;;
;;; Copyright © 2024 Artyom Bologov. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
;;; to whom the Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies or
;;; substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
;;; PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
;;; FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
;;; OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2026 Matthias Zenger. All rights reserved.

(
  "SRFI 253: setup"
  #t
  (import (lispkit test))
  (import (srfi 253))
  (define-syntax check-arg-true
    (syntax-rules ()
      ((_ pred val)
       (begin
         (check-arg pred val)
         #t))))
  #t
)

(
  "SRFI 253: check-arg"
  0
  (test-begin "check-arg")
  ;; Sanity checks
  (test-assert (check-arg-true exact-integer? 3))
  (test-assert (check-arg-true integer? 3))
  (test-assert (check-arg-true boolean? #f))
  (test-assert (check-arg-true char? #\d))
  (test-assert (check-arg-true complex? 3+2i))
  (test-assert (check-arg-true inexact? 3.8))
  (test-assert (check-arg-true real? 3))
  (test-assert (check-arg-true real? 3/2))
  (test-assert (check-arg-true real? 3.8))
  (test-assert (check-arg-true list? '()))
  (test-assert (check-arg-true list? '(1 2 3)))
  (test-assert (check-arg-true null? '()))
  (test-assert (check-arg-true number? 3))
  (test-assert (check-arg-true number? 3+2i))
  (test-assert (check-arg-true number? 3.8))
  (test-assert (check-arg-true pair? '(1 2 3)))
  (test-assert (check-arg-true input-port? (current-input-port)))
  (test-assert (check-arg-true output-port? (current-output-port)))
  (test-assert (check-arg-true procedure? procedure?))
  (test-assert (check-arg-true rational? 3))
  (test-assert (check-arg-true rational? 3/2))
  (test-assert (check-arg-true string? ""))
  (test-assert (check-arg-true string? "hello"))
  (test-assert (check-arg-true symbol? 'hello))
  (test-assert (check-arg-true vector? #(1 2 3)))
  ;; Predicate checks
  (test-assert (check-arg-true (lambda (x) (positive? (string-length x))) "hello"))
  (test-assert (check-arg-true positive? 9))
  (test-assert (check-arg-true string-length "hello")) ;; If it works it works.
  (test-assert (check-arg-true (lambda (x) (and (integer? x) (positive? x))) 8))
  (test-assert (check-arg-true ((lambda (x y) (lambda (a) (and (x a) (y a)))) integer? positive?) 8))
  ;; Erroring checks
  (test-error (check-arg-true string? 3))
  (test-error (check-arg-true real? 3+2i))
  (test-error (check-arg-true symbol? "hello"))
  (test-error (check-arg-true procedure? 3))
  (test-error (check-arg-true (lambda (a) (> a 3)) 0))
  ;; Syntax checks
  (test-assert (begin (check-arg integer? 3 'testing-caller-arg) #t))
  (let ((failures (failed-tests))) (test-end) failures)
)

(
  "SRFI 253: values-checked"
  0
  (test-begin "values-checked")
  (test-equal 3 (values-checked (integer?) 3))
  (test-equal 3 (values-checked ((lambda (x) (= 3 x))) 3))
  (test-approx 3.0 (values-checked (real?) 3.0))
  ;; Implementation-specific, might be 3.0
  (test-equal 3 (values-checked (real?) 3))
  (test-assert (values-checked (integer? string?) 3 "hello"))
  (test-approx 3.0 (values-checked (inexact?) 3.0))
  (test-error (values-checked (integer?) "hello"))
  (test-error (values-checked (integer? string?) 3 3))
  (let ((failures (failed-tests))) (test-end) failures)
)

(
  "SRFI 253: check-case"
  0
  (test-begin "check-case")
  (test-assert (check-case "hello" (string? #t)))
  (test-assert (check-case 3 (integer? #t) (string? #f)))
  (test-assert (check-case 3.7 (inexact? #t)))
  (test-assert (check-case (current-output-port) (output-port? #t)))
  (test-assert (check-case #(1 2 3) (vector? #t)))
  (test-assert (check-case 3 (string? #f) (else #t)))
  (test-error (check-case 3 (string? #t)))
  (let ((failures (failed-tests))) (test-end) failures)
)

(
  "SRFI 253: lambda-checked"
  0
  (test-begin "lambda-checked")
  (test-assert (lambda-checked () #t))
  (test-assert (lambda-checked args #t))
  (test-assert (lambda-checked (a) #t))
  (test-assert (lambda-checked (a b) #t))
  (test-assert (lambda-checked ((a integer?)) #t))
  (test-assert (lambda-checked (a (b integer?)) #t))
  (test-assert (lambda-checked ((a string?) (b integer?)) #t))
  (test-assert ((lambda-checked () #t)))
  (test-assert ((lambda-checked args #t) 1 2 3))
  (test-assert ((lambda-checked (a) #t) 3))
  (test-assert ((lambda-checked (a) #t) "hello"))
  (test-assert ((lambda-checked ((a integer?)) #t) 3))
  (test-assert ((lambda-checked (a (b integer?)) #t) 3 3))
  (test-assert ((lambda-checked (a (b integer?)) #t) "hello" 3))
  (test-error ((lambda-checked ((a integer?)) #t) "hello"))
  (test-error ((lambda-checked (a (b integer?)) #t) "hello" "hi"))
  ;; SRFI 273 return value checks
  (test-assert ((lambda-checked ((b integer?)) => (integer?) 2) 1))
  (test-error ((lambda-checked ((b integer?)) => (integer?) #t) 1))
  (test-error ((lambda-checked () => (integer?) #t)))
  (test-error ((lambda-checked args => (integer?) #t) 1 2 3))
  ;; Rest args (not supported by the sample implementation)
  (test-assert (lambda-checked (a . c) #t))
  (test-assert (lambda-checked ((a integer?) . c) #t))
  (test-assert (lambda-checked (a b . c) #t))
  (test-assert (lambda-checked (a (b integer?) . c) #t))
  (test '(1 2 (3 4)) ((lambda-checked (a (b integer?) . c) (list a b c)) 1 2 3 4))
  (test-error ((lambda-checked (a (b integer?) . c) (list a b c)) 1 "2" 3 4))
  (let ((failures (failed-tests))) (test-end) failures)
)

(
  "SRFI 253: case-lambda-checked"
  0
  (test-begin "case-lambda-checked")
  (test-assert (case-lambda-checked
                (() #t)))
  (test-assert (case-lambda-checked
                (args #t)))
  (test-assert (case-lambda-checked
                ((a) #t)))
  (test-assert (case-lambda-checked
                ((a) #t)))
  (test-assert (case-lambda-checked
                (() #t) ((a) #t)))
  (test-assert (case-lambda-checked
                (() #t) ((a) #t) (args #t)))
  (test-assert (case-lambda-checked
                (((a integer?)) #t)))
  (test-assert (case-lambda-checked
                (((a integer?) b) #t)))
  (test-assert (case-lambda-checked
                ((a (b integer?)) #t)))
  (test-assert (case-lambda-checked
                (() #t)
                (((a integer?)) #t)
                ((a (b string?)) #t)
                (args #t)))
  (define checked-case-lambda
    (case-lambda-checked
     (() #t)
     (((a integer?)) #t)
     ((a (b string?)) #t)
     (((a string?) b . rest) #t)))
  (test-assert (checked-case-lambda))
  (test-assert (checked-case-lambda 3))
  (test-error (checked-case-lambda "hello"))
  (test-assert (checked-case-lambda 3 "hello"))
  (test-assert (checked-case-lambda "hi" "hello"))
  (test-error (checked-case-lambda 3 3 3))
  ;; SRFI 273 return value checks
  (define good-return-checked-case-lambda
    (case-lambda-checked
     (() => (integer?) 1)
     (((a integer?)) => (integer?) 1)
     (args => (integer?) 1)))
  (test-assert (good-return-checked-case-lambda 1))
  (test-assert (good-return-checked-case-lambda))
  (test-assert (good-return-checked-case-lambda 1 2 3))
  (define bad-return-checked-case-lambda
    (case-lambda-checked
     (() => (integer?) #t)
     (((a integer?)) => (integer?) #t)
     (args => (integer?) #t)))
  (test-error (bad-return-checked-case-lambda 1))
  (test-error (bad-return-checked-case-lambda))
  (test-error (bad-return-checked-case-lambda 1 2 3))
  (let ((failures (failed-tests))) (test-end) failures)
)

(
  "SRFI 253: define-checked"
  0
  (test-begin "define-checked")
  (define-checked (c) #t)
  (test-assert (c))
  (define-checked (c (a integer?)) #t)
  (test-assert (c 3))
  (test-error (c "hello"))
  (define-checked (c b) #t)
  (test-assert (c "anything"))
  (test-error (c 1 2 3))
  (define-checked (c (b string?)) #t)
  (test-assert (c "hello"))
  (test-error (c 3))
  ;; Rest args (not supported by the sample implementation)
  (define-checked (c b . d) #t)
  (test-error (c))
  (test-assert (c 1))
  (test-assert (c 1 2))
  (test-assert (c 1 2 3))
  (define-checked c string? "hello")
  (test-assert c)
  (set! c "whatever")
  (test-assert c)
  (let ((failures (failed-tests))) (test-end) failures)
)

(
  "SRFI 253: define-record-type-checked"
  0
  (test-begin "define-record-type-checked")
  (define-record-type-checked <test>
    (make-test a b)
    test?
    (a integer? test-a)
    (b string? test-b test-b-set!))
  (test-assert (make-test 1 "hello"))
  (test-error (make-test 1))
  (test-error (make-test 1 2))
  (test-error (make-test 1.2 "hello"))
  (define test-test (make-test 1 "hello"))
  (test-assert (begin
                 (test-b-set! test-test "foo")
                 #t))
  (test-error (test-b-set! test-test 1))
  (let ((failures (failed-tests))) (test-end) failures)
)

(
  "SRFI 253: LispKit specifics"
  0
  (test-begin "LispKit specifics")
  ;; check-arg returns its argument
  (test 3 (check-arg integer? 3))
  (test 3 (check-arg integer? 3 'caller))
  (test "caller: argument does not satisfy predicate:"
        (guard (e (#t (error-object-message e))) (check-arg integer? "x" 'caller)))
  ;; values-checked
  (test '(1 "a" #\b) (call-with-values (lambda () (values-checked (integer? string? char?) 1 "a" #\b))
                                       list))
  (test "values-checked: value 2 does not satisfy string?:"
        (guard (e (#t (error-object-message e))) (values-checked (integer? string?) 1 2)))
  (test '(1 2) (let ((n 0))
                 (values-checked (integer? integer?)
                                 (begin (set! n (+ n 1)) n)
                                 (begin (set! n (+ n 1)) n))
                 (list 1 n)))
  ;; check-case
  (test 'int (check-case 3 (string? 'str) (integer? 'int) (else 'other)))
  (test 'other (check-case #\a (string? 'str) (integer? 'int) (else 'other)))
  (test 2 (check-case '(1 2 3) (string? 'str) ((lambda (x) (memv 2 x)) => car)))
  (test '(2 3) (check-case '(1 2 3) ((lambda (x) (memv 2 x)))))
  (test 4 (let ((n 0)) (check-case (begin (set! n (+ n 1)) 3) (string? 'str) (integer? (+ n 3)))))
  (test-error (check-case 'x))
  (test "check-case: value does not satisfy any of the predicates:"
        (guard (e (#t (error-object-message e))) (check-case 3 (string? 'a) (char? 'b))))
  ;; lambda-checked with internal definitions and multiple return values
  (test 3 ((lambda-checked ((x integer?)) (define y (* x 2)) (+ y 1)) 1))
  (test-error ((lambda-checked ((x integer?)) (define y (* x 2)) (+ y 1)) "1"))
  (test '(1 "a") (call-with-values
                   (lambda () ((lambda-checked (a b) => (integer? string?) (values a b)) 1 "a"))
                   list))
  (test-error ((lambda-checked (a b) => (integer? string?) (values a b)) 1 2))
  (test-error ((lambda-checked (a b) => (integer? string?) a) 1 2))
  (test "lambda-checked: argument a does not satisfy integer?:"
        (guard (e (#t (error-object-message e))) ((lambda-checked ((a integer?)) a) "x")))
  ;; define-checked reports the name of the procedure
  (define-checked (add (x number?) (y number?)) => (number?) (+ x y))
  (test 3 (add 1 2))
  (test "add: argument y does not satisfy number?:"
        (guard (e (#t (error-object-message e))) (add 1 "2")))
  (define-checked (half (x integer?)) => (integer?) (/ x 2))
  (test 2 (half 4))
  (test "half: value does not satisfy integer?:"
        (guard (e (#t (error-object-message e))) (half 3)))
  (test-error (let () (define-checked v string? 3) v))
  ;; case-lambda-checked with rest arguments and internal definitions
  (define f (case-lambda-checked
             (((a integer?)) (define b (+ a 1)) b)
             (((a integer?) . rest) => (list?) (cons a rest))))
  (test 2 (f 1))
  (test '(1 2 3) (f 1 2 3))
  (test-error (f "1" 2 3))
  ;; Records: constructor arguments are matched with fields by name
  (define-record-type-checked <point>
    (make-point y x)
    point?
    (x real? point-x point-x-set!)
    (y real? point-y)
    (label string? point-label point-label-set!))
  (define p (make-point 2 1))
  (test '(1 2) (list (point-x p) (point-y p)))
  (test-assert (point? p))
  (test-error (make-point 'y 1))
  (test-error (make-point 2 'x))
  (test "make-point: argument x does not satisfy real?:"
        (guard (e (#t (error-object-message e))) (make-point 2 'x)))
  (point-label-set! p "origin")
  (test "origin" (point-label p))
  (test-error (point-label-set! p 'origin))
  (test-error (point-x-set! p "1"))
  (test-error (point-x 'not-a-point))
  (test-error (point-x-set! 'not-a-point 1))
  (let ((failures (failed-tests))) (test-end) failures)
)
