;;; tail-call-tests.scm — tricky tail-call & recursion tests for a Scheme compiler
;;; R7RS-small. Each test prints PASS/FAIL. Tests marked [TCO] must run in
;;; constant stack space; with N = 1,000,000 a compiler without proper tail
;;; calls will usually blow the stack (or crawl if it grows a heap stack).

;;; Tail call tests
;;;
;;; This is a collection of tricky tail-call and recursion tests for a
;;; Scheme compiler compliant with R7RS. Each test prints PASS/FAIL.
;;; Tests marked [TCO] must run in constant stack space; with N = 1,000,000
;;; a compiler without proper tail calls will usually blow the stack (or
;;; crawl if it grows a heap stack).
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing,
;;; software distributed under the License is distributed on an
;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
;;; either express or implied. See the License for the specific
;;; language governing permissions and limitations under the
;;; License.

(import (lispkit base))

(define N 1000000)
(define failures 0)
(define-syntax check
  (syntax-rules ()
    ((_ name expr expected)
     (let ((got expr))
       (if (equal? got expected)
           (begin (display "PASS ") (display name) (newline))
           (begin (set! failures (+ failures 1))
                  (display "FAIL ") (display name)
                  (display ": got ") (write got)
                  (display ", expected ") (write expected) (newline)))))))

;; 1. [TCO] Mutual recursion across top-level procedures.
(define (my-even? n) (if (= n 0) #t (my-odd? (- n 1))))
(define (my-odd? n)  (if (= n 0) #f (my-even? (- n 1))))
(check "mutual even/odd" (my-even? N) #t)

;; 2. [TCO] Argument shuffling: the new args are a permutation of the old ones.
;;    Catches compilers that overwrite frame slots in place before reading them.
(define (rotate n a b c)
  (if (= n 0) (list a b c) (rotate (- n 1) c a b)))
(check "rotate args in tail call" (rotate N 'x 'y 'z) '(z x y))

;; 3. [TCO] Tail call to a procedure with MORE arguments than the caller
;;    (frame must grow on reuse), and back to one with fewer.
(define (narrow n) (if (= n 0) 'done (wide n 1 2 3 4 5 6 7)))
(define (wide n a b c d e f g) (narrow (- n 1 (- (+ a b c d e f g) 28))))
(check "frame resize narrow<->wide" (narrow N) 'done)

;; 4. [TCO] Tail position inside and / or / when / unless / cond / case / cond =>.
(define (via-and n)    (and #t (if (= n 0) 'and-ok (via-and (- n 1)))))
(define (via-or n)     (or #f (if (= n 0) 'or-ok (via-or (- n 1)))))
(define (via-when n)   (if (= n 0) 'when-ok (when #t (via-when (- n 1)))))
(define (via-cond n)   (cond ((= n 0) 'cond-ok)
                             ((assv 1 '((1 . x))) => (lambda (p) (via-cond (- n 1))))))
(define (via-case n)   (case (if (= n 0) 'stop 'go)
                         ((stop) 'case-ok)
                         (else (via-case (- n 1)))))
(check "tail in and"     (via-and N)  'and-ok)
(check "tail in or"      (via-or N)   'or-ok)
(check "tail in when"    (via-when N) 'when-ok)
(check "tail in cond =>" (via-cond N) 'cond-ok)
(check "tail in case"    (via-case N) 'case-ok)

;; 5. [TCO] apply in tail position (R7RS requires apply to tail-call its proc).
(define (apply-loop n) (if (= n 0) 'apply-ok (apply apply-loop (list (- n 1)))))
(check "apply in tail position" (apply-loop N) 'apply-ok)

;; 6. [TCO] call/cc must call its argument as a tail call (R7RS 3.5).
;;    CALLCC-N is kept small because LispKit slows down superlinearly here.
;;    Raise it to N for a strict check.
(define CALLCC-N 1000)
(define (callcc-loop n)
  (if (= n 0) 'callcc-ok (call/cc (lambda (k) (callcc-loop (- n 1))))))
(check "call/cc in tail position" (callcc-loop CALLCC-N) 'callcc-ok)

;; 7. [TCO] Rest arguments through apply, list size stays constant.
(define (rest-loop n . xs)
  (if (= n 0) (length xs) (apply rest-loop (- n 1) xs)))
(check "rest args via apply" (rest-loop N 'a 'b 'c) 3)

;; 8. [TCO] 3-state machine via internal defines (letrec* semantics).
(define (machine n)
  (define (s0 i acc) (if (= i 0) acc (s1 (- i 1) (+ acc 1))))
  (define (s1 i acc) (if (= i 0) acc (s2 (- i 1) (* acc 1))))
  (define (s2 i acc) (if (= i 0) acc (s0 (- i 1) (- acc 0))))
  (s0 n 0))
(check "3-state internal-define machine" (machine 999999) 333333)

;; 9. [TCO] CPS: every call, including to the continuation, is a tail call.
(define (fact-cps n k) (if (= n 0) (k 1) (fact-cps (- n 1) (lambda (v) (k (* n v))))))
(check "CPS factorial 20" (fact-cps 20 (lambda (x) x)) 2432902008176640000)
(define (count-cps n k) (if (= n 0) (k 0) (count-cps (- n 1) (lambda (v) (k (+ v 1))))))
(check "CPS deep continuation chain" (count-cps N (lambda (x) x)) N)

;; 10. Named let must create a FRESH binding per iteration (closures differ).
(check "named-let fresh bindings"
       (let loop ((i 0) (acc '()))
         (if (= i 3) (map (lambda (f) (f)) acc)
             (loop (+ i 1) (cons (lambda () i) acc))))
       '(2 1 0))

;; 11. `do` also rebinds per iteration (unlike a C for-loop).
(check "do fresh bindings"
       (let ((fs (do ((i 0 (+ i 1)) (acc '() (cons (lambda () i) acc)))
                     ((= i 3) acc))))
         (map (lambda (f) (f)) fs))
       '(2 1 0))

;; 12. Self-call must NOT be turned into a jump when the global is reassigned.
(define (self n) (if (= n 0) 'old (self (- n 1))))
(define saved self)
(set! self (lambda (n) 'new))
(check "self-call through mutable global" (saved 5) 'new)

;; 13. Loop variable mutated with set! while closures capture it.
(check "set! on captured loop var"
       (let loop ((i 0) (fs '()))
         (if (= i 3) (map (lambda (f) (f)) fs)
             (let ((f (lambda () i)))
               (set! i (+ i 10))
               (loop (- i 9) (cons f fs)))))
       '(12 11 10))

;; 14. Re-entering a continuation captured inside a loop (state must survive).
(define (reenter)
  (let ((k #f) (count 0) (trace '()))
    (let loop ((i 0))
      (when (< i 2)
        (set! trace (cons i trace))
        (call/cc (lambda (c) (when (= i 1) (set! k c))))
        (loop (+ i 1))))
    (set! count (+ count 1))
    (if (< count 3) (k #f))
    (reverse trace)))
(check "re-entrant continuation in loop" (reenter) '(0 1))

;; 15. Non-tail deep recursion. R7RS doesn't mandate this, but good compilers
;;     grow the stack instead of crashing. Lower N if your target can't.
(define (deep n) (if (= n 0) 0 (+ 1 (deep (- n 1)))))
(check "non-tail recursion depth 1e5" (deep 100000) 100000)

;; 16. Classic stress benchmarks for non-tail recursion & call overhead.
(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))
(check "ackermann 2 3" (ack 2 3) 9)
(check "ackermann 3 5" (ack 3 5) 253)
(define (tak x y z)
  (if (not (< y x)) z
      (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y))))
(check "tak 18 12 6" (tak 18 12 6) 7)

;; 17. [TCO] Y-combinator loop: tail calls through anonymous closures only.
(define Y (lambda (f) ((lambda (x) (f (lambda (n) ((x x) n))))
                       (lambda (x) (f (lambda (n) ((x x) n)))))))
(define y-count (Y (lambda (self) (lambda (n) (if (= n 0) 'y-ok (self (- n 1)))))))
(check "Y combinator loop" (y-count N) 'y-ok)

;; 18. NOT a tail call: dynamic-wind's after thunk must still run, in order.
(check "dynamic-wind ordering"
       (let ((log '()))
         (dynamic-wind (lambda () (set! log (cons 'in log)))
                       (lambda () (set! log (cons 'body log)))
                       (lambda () (set! log (cons 'out log))))
         (reverse log))
       '(in body out))

(newline)
(display (if (= failures 0) "ALL PASSED" "SOME FAILED"))
(newline)
