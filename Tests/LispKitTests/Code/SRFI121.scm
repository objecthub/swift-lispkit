;;; SRFI121.scm
;;; Regression test data
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2017 ObjectHub. All rights reserved.
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
  "Generators constructors"
  (()
   (1 2 3)
   (8 9 10)
   (8 10 12)
   (3 4 5 6)
   (3 4 5 6 7)
   (3 5 7))
  (import (srfi 121))
  (list (generator->list (generator))
        (generator->list (generator 1 2 3))
        (generator->list (make-iota-generator 3 8))
        (generator->list (make-iota-generator 3 8 2))
        (generator->list (make-range-generator 3) 4)
        (generator->list (make-range-generator 3 8))
        (generator->list (make-range-generator 3 8 2)))
)

(
  "Advanced generators constructors"
  ((0 1 2)
   (1 2 3 4 5)
   (1 2 3 4 5)
   (5 4 3 2 1)
   (#\a #\b #\c #\d #\e)
   (10 20 30)
   (5 4 3 2 1)
   (0 2 4 6 8 10))
  (define g (make-coroutine-generator
              (lambda (yield) (let loop ((i 0))
                (when (< i 3) (yield i) (loop (+ i 1)))))))
  (define (for-each-digit proc n)
    (when (> n 0)
      (let-values (((div rem) (truncate/ n 10)))
        (proc rem)
        (for-each-digit proc div))))
  (list (generator->list g)
        (generator->list (list->generator '(1 2 3 4 5)))
        (generator->list (vector->generator '#(1 2 3 4 5)))
        (generator->list (reverse-vector->generator '#(1 2 3 4 5)))
        (generator->list (string->generator "abcde"))
        (generator->list (bytevector->generator (bytevector 10 20 30)))
        (generator->list (make-for-each-generator for-each-digit 12345))
        (generator->list (make-unfold-generator (lambda (s) (> s 5))
                                                (lambda (s) (* s 2))
                                                (lambda (s) (+ s 1))
                                                0)))
)

(
  "Generators operators"
  ((a b 0 1)
   (0 1 2 0 1)
   ()
   (15 22 31)
   (1 3 5 7 9)
   (2 4 6 8 10)
   (1 2 3)
   (4)
   (1 2)
   (1 2 0)
   (3 4))
  (define g (make-range-generator 1 5))
  (define g1 (generator 1 2 3))
  (define g2 (generator 4 5 6 7))
  (define (proc . args) (values (apply + args) (apply + args)))
  (list (generator->list (gcons* 'a 'b (make-range-generator 0 2)))
        (generator->list (gappend (make-range-generator 0 3)
                                  (make-range-generator 0 2)))
        (generator->list (gappend))
        (generator->list (gcombine proc 10 g1 g2))
        (generator->list (gfilter odd? (make-range-generator 1 11)))
        (generator->list (gremove odd? (make-range-generator 1 11)))
        (generator->list (gtake g 3))
        (generator->list g)
        (generator->list (gtake (make-range-generator 1 3) 3))
        (generator->list (gtake (make-range-generator 1 3) 3 0))
        (generator->list (gdrop (make-range-generator 1 5) 2)))
)

(
  "Advanced generators operators"
  ((1 2)
   (3 4)
   ()
   (0.0 1.0 0 2)
   (0.0 0 2)
   (a c e)
   (a d e)
   (1 2 3)
   (1))
  (define g (make-range-generator 1 5))
  (define g1 (make-range-generator 1 5))
  (define (small? x) (< x 3))
  (list (generator->list (gtake-while small? g1))
        (generator->list (gdrop-while small? g))
        (generator->list (gdrop-while (lambda args #t) (generator 1 2 3)))
        (generator->list (gdelete 1 (generator 0.0 1.0 0 1 2)))
        (generator->list (gdelete 1 (generator 0.0 1.0 0 1 2) =))
        (generator->list (gindex (list->generator '(a b c d e f))
                                 (list->generator '(0 2 4))))
        (generator->list (gselect (list->generator '(a b c d e f))
                         (list->generator '(#t #f #f #t #t #f))))
        (generator->list (gdelete-neighbor-dups (generator 1 1 2 3 3 3) =))
        (generator->list (gdelete-neighbor-dups (generator 1 2 3) (lambda args #t))))
)

(
  "Generators consumers"
  ((1 2 3)
   (5 4 3 2 1)
   #(1 2 3 4 5)
   #(1 2 3)
   "abc"
   (e d c b a . z))
  (list (generator->list (generator 1 2 3 4 5) 3)
        (generator->reverse-list (generator 1 2 3 4 5))
        (generator->vector (generator 1 2 3 4 5))
        (generator->vector (generator 1 2 3 4 5) 3)
        (generator->string (generator #\a #\b #\c))
        (with-input-from-port (open-input-string "a b c d e")
                              (lambda () (generator-fold cons 'z read))))
)

(
  "Advanced generators consumers"
  (6
   3
   2
   #t
   (4)
   #f
   (3 4)
   (#\a #\b #\c))
  (import (srfi 121))
  (import (only (srfi 1) unfold))
  (define n 0)
  (generator-for-each (lambda values (set! n (apply + values)))
    (generator 1) (generator 2) (generator 3))
  (define g (make-range-generator 2 5))
  (define g1 (make-range-generator 2 5))
  (list n
        (generator-find (lambda (x) (> x 2)) (make-range-generator 1 5))
        (generator-count odd? (make-range-generator 1 5))
        (generator-any odd? g)
        (generator->list g)
        (generator-every odd? g1)
        (generator->list g1)
        (generator-unfold (make-for-each-generator string-for-each "abc") unfold))
)
