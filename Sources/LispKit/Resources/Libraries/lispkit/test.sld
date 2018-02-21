;;; LISPKIT TEST
;;;
;;; Simple framework for implementing and running simple test suites. This is derived from
;;; similar, but much more sophisticated facilities from Chicken and Chibi scheme.
;;;
;;; Some of this code was originally implemented by Alex Shinn for his matching library.
;;; Copyright © 2010-2014 Alex Shinn. All rights reserved.
;;; BSD-style license: http://synthcode.com/license.txt
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2018 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
;;; except in compliance with the License. You may obtain a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed under the
;;; License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
;;; either express or implied. See the License for the specific language governing permissions
;;; and limitations under the License.

(define-library (lispkit test)

  (export test-begin
          test-end
          test
          test-equal
          test-assert
          test-error
          test-group
          approx-equal?)

  (import (lispkit base))

  (begin

    (define tests-passed 0)
    (define tests-failed 0)
    (define tests-start-time 0)
    (define internal-fail-token (gensym))

    (define (test-begin)
      (set! tests-passed 0)
      (set! tests-failed 0)
      (set! tests-start-time (current-second)))

    (define (test-end)
      (let ((end (current-second))
            (total (+ tests-passed tests-failed)))
        (newline)
        (display "║ ")
        (display total)
        (display " tests completed in ")
        (display (format-float (inexact (/ (- end tests-start-time) 1000)) 3))
        (display " seconds")
        (newline)
        (display "║ ")
        (display tests-passed)
        (display " (")
        (display (format-percent tests-passed total))
        (display "%) tests passed")
        (newline)
        (display "║ ")
        (display tests-failed)
        (display " (")
      	(display (format-percent tests-failed total))
        (display "%) tests failed")
        (newline)))

    (define (test-failures) tests-failed)

    (define (format-result spec name expect result)
      (do ((ls spec (cdr ls)))
          ((null? ls) (newline))
        (cond ((eq? (car ls) 'expect)
                 (write expect))
              ((eq? (car ls) 'result)
                 (write result))
              ((eq? (car ls) 'name)
                 (if name (begin (display #\space) (display name))))
              (else
                 (display (car ls))))))

    (define (format-float n prec)
      (let* ((str (number->string n))
             (len (string-length str)))
        (let lp ((i (- len 1)))
          (cond ((negative? i)
                  (string-append str "." (make-string prec #\0)))
                ((eqv? #\. (string-ref str i))
                  (let ((diff (+ 1 (- prec (- len i)))))
                    (cond ((positive? diff)
                            (string-append str (make-string diff #\0)))
                          ((negative? diff)
                            (substring str 0 (+ i prec 1)))
                          (else
                            str))))
                (else
                  (lp (- i 1)))))))

    (define (format-percent num denom)
      (let ((x (if (zero? denom) num (inexact (/ num denom)))))
        (format-float (* 100 x) 2)))

    (define (run-test name thunk expect eq pass-msg fail-msg)
      (let ((result (thunk)))
        (cond ((eq expect result)
                 (set! tests-passed (+ tests-passed 1))
                 (format-result pass-msg name expect result))
              (else
                 (set! tests-failed (+ tests-failed 1))
                 (format-result fail-msg name expect result)))))

    (define (run-equal name thunk expect eq)
      (run-test name
                thunk
                expect
                eq
                '("[PASS]" name)
                (if (eq? expect #t)
                    '("[FAIL]" name ": received " result)
                    '("[FAIL]" name ": expected " expect " but received " result))))

    (define-syntax test
      (syntax-rules (quote)
        ((_ expect expr)
          (test (write-to-string 'expr) expect expr))
        ((_ name expect (expr ...))
          (test-equal name expect (expr ...) equal?))
        ((_ name (quote expect) expr)
          (test-equal name (quote expect) expr equal?))
        ((_ name (expect ...) expr)
          (syntax-error "the test expression should come last: (test <expected> (<expr> ...))"
                        '(test name (expect ...) expr)))
        ((_ name expect expr)
          (test-equal name expect expr equal?))
        ((_ a ...)
          (syntax-error "a test requires 2 or 3 arguments" '(test a ...)))))

    (define-syntax test-equal
      (syntax-rules ()
        ((_ name value expr eq)
          (run-equal name (lambda () expr) value eq))
        ((_ name value expr)
          (run-equal name (lambda () expr) value equal?))
        ((_ value expr)
          (test-equal (write-to-string 'expr) value expr))))

    (define-syntax test-assert
      (syntax-rules ()
        ((_ name expr)
          (run-equal name (lambda () (if expr #t #f)) #t eq?))
        ((_ expr)
          (test-assert (write-to-string 'expr) expr))))

    (define-syntax test-error
      (syntax-rules ()
        ((_ name expr)
          (run-equal name
                     (lambda () (with-exception-handler (lambda (e) internal-fail-token)
                                                        (lambda () expr)))
                     internal-fail-token
                     eq?))
        ((_ expr)
          (test-error (write-to-string 'expr) expr))))

    (define-syntax test-group
      (syntax-rules ()
        ((_ name body ...)
          (begin
            (newline)
            (display name)
            (display ":")
            (newline)
            body ...))))

    (define (approx-equal? a b epsilon)
      (cond ((> (abs a) (abs b))
              (approx-equal? b a epsilon))
            ((zero? a)
              (< (abs b) epsilon))
            (else
              (< (abs (/ (- a b) b)) epsilon))))

   (define (call-with-output-string proc)
     (let ((out (open-output-string)))
       (proc out)
       (get-output-string out)))

   (define (write-to-string x)
     (call-with-output-string
       (lambda (out)
         (let wr ((x x))
           (if (pair? x)
               (cond ((and (symbol? (car x))
                           (pair? (cdr x))
                           (null? (cddr x))
                           (assq (car x) '((quote . "'")
                                           (quasiquote . "`")
                                           (unquote . ",")
                                           (unquote-splicing . ",@"))))
                       => (lambda (s) (display (cdr s) out) (wr (cadr x))))
                     (else
                       (display "(" out)
                       (wr (car x))
                       (let lp ((ls (cdr x)))
                         (cond ((pair? ls)
                                (display " " out)
                                (wr (car ls))
                                (lp (cdr ls)))
                               ((not (null? ls))
                                (display " . " out)
                                (write ls out))))
                       (display ")" out)))
               (write x out))))))

   (define (display-to-string x)
     (if (string? x)
         x
         (call-with-output-string (lambda (out) (display x out)))))
  )
)

