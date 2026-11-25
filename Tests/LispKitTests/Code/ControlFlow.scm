;;; ControlFlow.scm
;;; Regression test data
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2016 ObjectHub. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;      http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(
  "Let"
  6
  (let ((x 2) (y 3)) (* x y))
)

(
  "Nested let"
  35
  (let ((x 2) (y 3))
    (let ((x 7) (z (+ x y)))
      (* z x)))
)

(
  "Let and let*"
  70
  (let ((x 2) (y 3))
    (let* ((x 7) (z (+ x y)))
      (* z x)))
)

(
  "Let-values"
  3
  (let-values ((() (values)) ((a) (+ 1 2))) a)
)

(
  "Let and let*-values"
  (x y x y)
  (let ((a 'a) (b 'b) (x 'x) (y 'y))
    (let*-values (((a b) (values x y))
                  ((x y) (values a b)))
      (list a b x y)))
)

(
  "Letrec"
  #t
  (letrec ((even? (lambda (n) (if (zero? n) #t (odd? (- n 1)))))
           (odd? (lambda (n) (if (zero? n) #f (even? (- n 1))))))
    (even? 88))
)

(
  "Let loop"
  ((6 1 3) (-5 -2))
  (let loop ((numbers '(3 -2 1 6 -5))
             (nonneg '())
             (neg '()))
    (cond ((null? numbers) (list nonneg neg))
          ((>= (car numbers) 0)
           (loop (cdr numbers)
                 (cons (car numbers) nonneg)
                 neg))
          ((< (car numbers) 0)
           (loop (cdr numbers)
                 nonneg
                 (cons (car numbers) neg)))))
)

(
  "Let do"
  25
  (let ((x '(1 3 5 7 9)))
    (do ((x x (cdr x))
         (sum 0 (+ sum (car x))))
        ((null? x) sum)))
)

(
  "Do with one-armed if in argument"
  (1 5)
  (list 1 (do ((i 0 (+ i 1))) ((= i 3) 5) (if (= i 1) 7)))
)

(
  "Do with one-armed if and assignment in argument"
  2
  (+ 1 (let ((res 0))
         (do ((i 0 (+ i 1))) ((= i 3))
           (if (odd? i) (set! res (+ res i))))
         res))
)

(
  "Do with one-armed cond in argument"
  (1 5)
  (list 1 (do ((i 0 (+ i 1))) ((= i 3) 5) (cond ((= i 1) 7))))
)

(
  "Do with one-armed case, and, or, when, and unless in argument"
  (1 5)
  (list 1 (do ((i 0 (+ i 1))) ((= i 4) 5)
            (case i ((1) 7))
            (and (= i 1) 7)
            (or (= i 1) 7)
            (when (= i 1) 7)
            (unless (= i 1) 7)))
)

(
  "Do with two-armed if returning constants in argument"
  (1 5)
  (list 1 (do ((i 0 (+ i 1))) ((= i 3) 5) (if (= i 1) 7 8)))
)

(
  "Do with nested one-armed ifs in argument"
  (1 5 2)
  (list 1 (do ((i 0 (+ i 1))) ((= i 3) 5) (if (< i 2) (if (= i 1) i))) 2)
)

(
  "Do with one-armed if in nested arguments"
  #(1 (2 5) 3)
  (vector 1 (list 2 (do ((i 0 (+ i 1))) ((= i 3) 5) (if (= i 1) 'x))) 3)
)

(
  "For loop with where clause in argument"
  (1 (5 3 1))
  (begin
    (import (lispkit iterate))
    (let ((lst '(1 2 3 4 5)))
      (list 1 (let ((res '()))
                (for x in lst where (odd? x) (set! res (cons x res)))
                res))))
)

(
  "Named let compiled into a loop"
  (500000 124999750000)
  (begin
    (define (sum-below n)
      (let loop ((i 0) (acc 0))
        (if (< i n) (loop (+ i 1) (+ acc i)) (list i acc))))
    (sum-below 500000))
)

(
  "Named let loop at the top level"
  10
  (let loop ((i 0)) (if (< i 10) (loop (+ i 1)) i))
)

(
  "Named let loop with cond, case, when, and, or"
  (8 #f 9 #t done)
  (begin
    (define (f1 n)
      (let loop ((i 0))
        (cond ((> i n) i)
              ((odd? i) (loop (+ i 1)))
              (else (loop (+ i 2))))))
    (define (f2 n)
      (let loop ((i 0))
        (and (< i n) (loop (+ i 1)))))
    (define (f3 n)
      (let loop ((i 0))
        (case (remainder i 3)
          ((0) (if (> i n) i (loop (+ i 1))))
          (else (loop (+ i 1))))))
    (define (f4 n)
      (let loop ((i 0))
        (or (> i n) (loop (+ i 1)))))
    (define (f5 n)
      (let loop ((i 0))
        (when (< i n) (loop (+ i 1)))
        'done))
    (list (f1 6) (f2 300000) (f3 7) (f4 300000) (f5 10)))
)

(
  "Named let loop with internal definitions and nested let"
  (10 45)
  (let loop ((i 0) (acc 0))
    (define j (* i 1))
    (let ((k (+ acc j)))
      (if (< i 9) (loop (+ i 1) k) (list (+ i 1) k))))
)

(
  "Named let loop binds its variables anew in every iteration"
  ((2 1 0) (3 2 1))
  (list
    (let loop ((i 0) (fs '()))
      (if (< i 3)
          (loop (+ i 1) (cons (lambda () i) fs))
          (map (lambda (f) (f)) fs)))
    (let loop ((i 0) (fs '()))
      (if (< i 3)
          (let ((f (lambda () i)))
            (set! i (+ i 1))
            (loop i (cons f fs)))
          (map (lambda (f) (f)) fs))))
)

(
  "Named let loop variable shadowing the loop name"
  (3 ())
  (list (let loop ((loop 3)) loop) (let loop () '()))
)

(
  "Named let not in tail position"
  (6 120)
  (list (+ 1 (let loop ((i 0)) (if (< i 5) (loop (+ i 1)) i)))
        (let fact ((n 5)) (if (= n 0) 1 (* n (fact (- n 1))))))
)

(
  "Named let used as a value or called from a closure"
  (3 3 3)
  (list (let loop ((i 0)) (if (< i 3) (apply loop (list (+ i 1))) i))
        (let loop ((i 0)) (if (< i 3) ((lambda () (loop (+ i 1)))) i))
        (let outer ((i 0))
          (let inner ((j 0))
            (cond ((< j 2) (inner (+ j 1)))
                  ((< i 3) (outer (+ i 1)))
                  (else i)))))
)

(
  "Named let called with the wrong number of arguments"
  <error>
  (let loop ((i 0)) (if (< i 3) (loop) i))
)

(
  "Named let loop re-entered via a continuation"
  (3 3 3)
  (begin
    (define k #f)
    (define results '())
    (define (run)
      (let loop ((i 0) (acc 0))
        (if (= i 3)
            acc
            (begin
              (call/cc (lambda (c) (if (= i 1) (set! k c))))
              (loop (+ i 1) (+ acc i))))))
    (set! results (cons (run) results))
    (if (< (length results) 3) (k #f))
    results)
)

(
  "Mutated local variable shared by continuations"
  3
  (begin
    (define (count-up)
      (let ((x 0) (k #f))
        (call/cc (lambda (c) (set! k c)))
        (set! x (+ x 1))
        (if (< x 3) (k #f))
        x))
    (count-up))
)

(
  "Mutated local variables stored on the stack"
  (2 55 (1 . 2))
  (begin
    (define (f1) (define x 1) (set! x (+ x 1)) x)
    (define (f2 n)
      (let ((sum 0) (i 0))
        (do () ((> i n) sum)
          (set! sum (+ sum i))
          (set! i (+ i 1)))))
    (define (f3)
      (let ((a 1) (b 2))
        (set! a (cons a b))
        a))
    (list (f1) (f2 10) (f3)))
)

(
  "Do binds its variables anew in every iteration"
  ((2 1 0) (5 3 1))
  (list
    (do ((i 0 (+ i 1))
         (fs '() (cons (lambda () i) fs)))
        ((= i 3) (map (lambda (f) (f)) fs)))
    (do ((i 0 (+ i 1))
         (fs '()))
        ((>= i 6) (map (lambda (f) (f)) fs))
      (let ((f (lambda () i)))
        (set! i (+ i 1))
        (set! fs (cons f fs)))))
)

(
  "Named let loop does not retain unused locals of the enclosing procedure"
  100000
  (begin
    (define (naturals i) (cons i (delay (naturals (+ i 1)))))
    ; The loop must not keep the head of the stream alive via parameter `s` of `count-up`;
    ; otherwise, releasing the stream at the end overflows the native stack
    (define (count-up s n)
      (let loop ((s s) (k 0))
        (if (= k n) (car s) (loop (force (cdr s)) (+ k 1)))))
    (count-up (naturals 0) 100000))
)
