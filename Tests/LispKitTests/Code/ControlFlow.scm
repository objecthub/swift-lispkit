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
