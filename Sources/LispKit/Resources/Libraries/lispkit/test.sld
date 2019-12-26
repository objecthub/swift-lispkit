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

  (export epsilon
          approx-equal?
          test-begin
          test-end
          test-exit
          test-failures
          test
          test-equal
          test-approx
          test-assert
          test-not
          test-values
          test-error
          test-group
          test-group-failures
          current-test-comparator
          write-to-string)

  (import (lispkit base)
          (lispkit stack))

  ;; Convenience equality predicate for inexact values
  (begin

    (define epsilon (make-parameter 0.0000001))

    (define (approx-equal? a b . args)
      (let-optionals args ((eps (epsilon)))
        (cond ((> (abs a) (abs b))
                (approx-equal? b a eps))
              ((zero? a)
                (< (abs b) eps))
              (else
                (< (abs (/ (- a b) b)) eps)))))
  )

  ;; Manage test groups
  (begin

    (define open-test-groups (make-stack))

    (define-record-type test-group
      (make-test-group name passed failed start-time)
      test-group?
      (name test-group-name)
      (passed test-group-passed set-test-group-passed!)
      (failed test-group-failed set-test-group-failed!)
      (start-time test-group-start-time))

    (define (push-test-group name)
      (stack-push! open-test-groups (make-test-group name 0 0 (current-second))))

    (define (pop-test-group)
      (stack-pop! open-test-groups))

    (define (current-test-group)
      (if (stack-empty? open-test-groups)
          #f
          (stack-top open-test-groups)))

    (define (inc-passed)
      (let ((group (current-test-group)))
        (set-test-group-passed! group (fx1+ (test-group-passed group)))))

    (define (inc-failed)
      (let ((group (current-test-group)))
        (set-test-group-failed! group (fx1+ (test-group-failed group)))))

    (define (test-group-failures)
      (test-group-failed (current-test-group)))

    (define (test-failures)
      (fold-left (lambda (acc g) (fx+ acc (test-group-failed g)))
                 0
                 (stack->list open-test-groups)))

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

    (define (display-test-group group end-time)
      (let* ((passed (test-group-passed group))
             (failed (test-group-failed group))
             (total (+ passed failed)))
        (display top1)
        (newline)
        (cond ((test-group-name group)
                (display "│ ")
                (display (test-group-name group))
                (newline)))
        (display "│ ")
        (display total)
        (display " tests completed in ")
        (display (format-float (inexact (- end-time (test-group-start-time group))) 3))
        (display " seconds")
        (cond ((positive? failed)
                (newline)
                (display "│ ")
                (display (test-group-passed group))
                (display " (")
                (display (format-percent (test-group-passed group) total))
                (display "%) tests passed")
                (newline)
                (display "│ ")
                (display (test-group-failed group))
                (display " (")
                (display (format-percent (test-group-failed group) total))
                (display "%) tests failed")))
        (newline)
        (display bot2)
        (newline)))
  )

  ;; Execute tests
  (begin

    (define internal-fail-token (gensym))

    (define current-test-comparator (make-parameter equal?))

    (define (test-begin . args)
      (let-optionals args ((name #f))
        (cond (name
                (display top2)
                (newline)
                (display "│ ")
                (display name)
                (newline)
                (display bot1))
              (else
                (display line)))
        (newline)
        (push-test-group name)))

    (define (test-end . args)
      (let-optionals args ((name #f))
        (let* ((g (pop-test-group))
               (n (test-group-name g)))
          (display-test-group g (current-second))
          (cond ((and name (not (equal? name n)))
                  (error "ending test group $0, but expected end of $1" name n))
                ((current-test-group) => (lambda (o)
                  (set-test-group-passed! o (fx+ (test-group-passed o) (test-group-passed g)))
                  (set-test-group-failed! o (fx+ (test-group-failed o) (test-group-failed g)))))))))

    (define (test-exit . args)
      (cond ((current-test-group) => (lambda (g)
              (error "test group $0 not closed" (test-group-name g))))
            (else
              (void))))

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

    (define (run-test name thunk expect eq pass-msg fail-msg)
      (let ((result (guard (x (else x)) (thunk))))
        (cond ((eq expect result)
                 (inc-passed)
                 (format-result pass-msg name expect result))
              (else
                 (inc-failed)
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
          (test-equal name expect (expr ...) (current-test-comparator)))
        ((_ name (quote expect) expr)
          (test-equal name (quote expect) expr (current-test-comparator)))
        ((_ name (expect ...) expr)
          (syntax-error "the test expression should come last: (test <expected> (<expr> ...))"
                        '(test name (expect ...) expr)))
        ((_ name expect expr)
          (test-equal name expect expr (current-test-comparator)))
        ((_ a ...)
          (syntax-error "a test requires 2 or 3 arguments" '(test a ...)))))

    (define-syntax test-equal
      (syntax-rules ()
        ((_ name value expr eq)
          (run-equal name (lambda () expr) value eq))
        ((_ name value expr)
          (run-equal name (lambda () expr) value (current-test-comparator)))
        ((_ value expr)
          (test-equal (write-to-string 'expr) value expr))))

    (define-syntax test-approx
      (syntax-rules ()
        ((_ name value expr)
          (run-equal name (lambda () expr) value approx-equal?))
        ((_ value expr)
          (test-approx (write-to-string 'expr) value expr))))

    (define-syntax test-assert
      (syntax-rules ()
        ((_ name expr)
          (run-equal name (lambda () (if expr #t #f)) #t eq?))
        ((_ expr)
          (test-assert (write-to-string 'expr) expr))))

    (define-syntax test-not
      (syntax-rules ()
        ((_ expr) (test-assert (not expr)))
        ((_ name expr) (test-assert name (not expr)))))

    (define-syntax test-values
      (syntax-rules ()
        ((_ expect expr)
          (test-values #f expect expr))
        ((_ name expect expr)
          (test name (call-with-values (lambda () expect) (lambda results results))
                     (call-with-values (lambda () expr) (lambda results results))))))

    (define-syntax test-error
      (syntax-rules ()
        ((_ name expr)
          (run-equal name
                     (lambda () (guard (e (#t internal-fail-token)) expr))
                     internal-fail-token
                     eq?))
        ((_ expr)
          (test-error (write-to-string 'expr) expr))))

    (define-syntax test-group
      (syntax-rules ()
        ((_ name body ...)
          (begin
            (test-begin name)
            body ...
            (test-end)))))

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

  ;; Internal UI support
  (begin

    (define top2 "╒═══════════════════════════════════════════════════════════════════════════════")
    (define top1 "┌───────────────────────────────────────────────────────────────────────────────")
    (define line "════════════════════════════════════════════════════════════════════════════════")
    (define bot2 "╘═══════════════════════════════════════════════════════════════════════════════")
    (define bot1 "└───────────────────────────────────────────────────────────────────────────────")
  )
)
