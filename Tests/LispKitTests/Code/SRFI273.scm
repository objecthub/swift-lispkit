;;; SRFI273.scm
;;; Regression test data
;;;
;;; This test suite is based on the SRFI 273 test suite by Artyom Bologov. Additional tests
;;; cover `check-impl?`, error reporting, and the validation of declarations.
;;;
;;; Copyright © 2026 Artyom Bologov. All rights reserved.
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
  "SRFI 273: setup"
  #t
  (import (lispkit test))
  (import (srfi 253))
  (import (srfi 273))
  (define-check email? string?)
  (define-check positive-integer?
    (lambda (x) (and (integer? x) (positive? x))))
  (define-syntax check-arg-true
    (syntax-rules ()
      ((_ pred val)
       (begin
         (check-arg pred val)
         #t))))
  #t
)

(
  "SRFI 273: define-check"
  0
  (test-begin "define-check")
  (test-equal 3 3)
  (test-assert (check-arg-true email? "srfi-273@srfi.schemers.org"))
  (test-assert (check-arg-true positive-integer? 3))
  (test-error (check-arg-true positive-integer? 0))
  (test-error (check-arg-true positive-integer? -8))
  ;; Domain error
  (test-error (let () (define-check invalid 3) invalid))
  (let ((failures (failed-tests))) (test-end) failures)
)

(
  "SRFI 273: define-values-checked"
  0
  (test-begin "define-values-checked")
  ;; Definitions are not allowed in expression contexts in LispKit, so the original tests
  ;; using `(begin (define-values-checked ...) #t)` were changed to use `(let () ...)`
  (test-assert (let ()
                 (define-values-checked (quot rem) (integer? integer?)
                   (truncate/ 1 2))
                 #t))
  (test-assert (let ()
                 (define-values-checked (a) (real?) 3)
                 #t))
  (test-error (let ()
                (define-values-checked (quot rem) (integer? string?)
                  (truncate/ 1 2))
                #t))
  (test-error (let () (define-values-checked (a) (string?) 3) a))
  ;; Ensure that symbols are not bound on type mismatch; the definition is evaluated via
  ;; `eval` so that its error does not abort this test
  (test-error (eval '(define-values-checked (x y) (integer? string?) (truncate/ 1 2))
                    (interaction-environment)))
  (test-error (eval 'x (interaction-environment)))
  (test-error (eval 'y (interaction-environment)))
  (let ((failures (failed-tests))) (test-end) failures)
)

(
  "SRFI 273: declare-checked"
  0
  (test-begin "declare-checked")
  ;; Post-declaration for standard symbols
  (test-assert (begin
                 (declare-checked (negative? (x real?)) => (boolean?))
                 #t))
  (test-assert (begin
                 (declare-checked (call/cc (k procedure?)))
                 #t))
  (let ((failures (failed-tests))) (test-end) failures)
)

(
  "SRFI 273: LispKit specifics"
  0
  (test-begin "LispKit specifics")
  ;; define-check
  (define-check non-empty-string? (lambda (s) (and (string? s) (> (string-length s) 0))))
  (define-checked (greet (name non-empty-string?)) (string-append "Hello, " name "!"))
  (test "Hello, Ann!" (greet "Ann"))
  (test-error (greet ""))
  (test "define-check: 3 is not a predicate:"
        (guard (e (#t (error-object-message e))) (let () (define-check invalid 3) invalid)))
  ;; define-values-checked
  (define-values-checked (q r) (integer? integer?) (floor/ 7 2))
  (test '(3 1) (list q r))
  (test '(1 2 3) (let () (define-values-checked (a b c) (integer? integer? integer?) (values 1 2 3))
                   (list a b c)))
  (test "define-values-checked: variable r2 does not satisfy string?:"
        (guard (e (#t (error-object-message e)))
          (let () (define-values-checked (q2 r2) (integer? string?) (floor/ 7 2)) q2)))
  (test-error (let () (define-values-checked (a b) (integer? integer?) 1) a))
  (test-error (let () (define-values-checked (a) (integer?) (values 1 2)) a))
  ;; declare-checked has no effect, but validates the declaration
  (test-assert (begin (declare-checked (numbered-string (str string?) (idx integer?)) => (string?))
                      #t))
  (define (numbered-string str idx) (string-append str (number->string idx)))
  (test "a1" (numbered-string "a" 1))
  (test-assert (begin (declare-checked (variadic a (b integer?) . rest) => (integer? string?)) #t))
  (test-assert (begin (declare-checked some-variable string?) #t))
  (test-error (declare-checked (foo (x 3))))
  (test-error (declare-checked (foo x) => ('integer)))
  (test-error (declare-checked some-variable "string?"))
  (test "declare-checked: 3 for x is not a predicate:"
        (guard (e (#t (error-object-message e))) (declare-checked (foo (x 3)))))
  ;; check-impl? accepts all values in LispKit
  (test 'anything ((lambda-checked ((x (check-impl? pointer))) x) 'anything))
  (test -1 (values-checked ((check-impl? uint)) -1))
  (test 'bar (check-arg (check-impl? foo) 'bar))
  (define-checked (ref (data (check-impl? pointer)) (idx integer?)) (list data idx))
  (test '(p 1) (ref 'p 1))
  (test-error (ref 'p 'x))
  (test-assert (begin (declare-checked (ref (data (check-impl? pointer)) (idx integer?))) #t))
  (let ((failures (failed-tests))) (test-end) failures)
)
