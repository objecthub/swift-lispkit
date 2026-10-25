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
