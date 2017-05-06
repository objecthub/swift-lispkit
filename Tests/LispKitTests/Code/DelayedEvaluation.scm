;;; DelayedEvaluation.scm
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

;; The following tests are derived from SRFI-45
;; See http://srfi.schemers.org/srfi-45/srfi-45.html
;; 
;; Copyright (C) André van Tonder (2003). All Rights Reserved.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;; software and associated documentation files (the "Software"), to deal in the Software
;; without restriction, including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
;; to whom the Software is furnished to do so, subject to the following conditions:
;; The above copyright notice and this permission notice shall be included in all copies of
;; substantial portions of the Software.

(
  "Memoization 1"
  "hello"
  (let ((out (open-output-string)))
    (define s (delay (begin (display 'hello out) 1)))
    (force s)
    (force s)
    (get-output-string out))
)

(
  "Memoization 2"
  "bonjour"
  (let ((out (open-output-string)))
    (let ((s (delay (begin (display 'bonjour out) 2))))
      (+ (force s) (force s)))
    (get-output-string out))
)

(
  "Memoization 3"
  "hi"
  (let ((out (open-output-string)))
    (define r (delay (begin (display 'hi out) 1)))
    (define s (delay-force r))
    (define t (delay-force s))
    (force t)
    (force r)
    (get-output-string out))
)

(
  "Stream memoization"
  "hohohohoho"
  (let ((out (open-output-string)))
    (define (stream-drop s index)
      (delay-force (if (zero? index) s (stream-drop (cdr (force s)) (- index 1)))))
    (define (ones)
      (delay (begin (display 'ho out) (cons 1 (ones)))))
    (define s (ones))
    (car (force (stream-drop s 4)))
    (car (force (stream-drop s 4)))
    (get-output-string out))
)

(
  "Reentrancy 1"
  12
  (let ((res 0))
    (define count 0)
    (define p (delay (begin (set! count (+ count 1)) (if (> count x) count (force p)))))
    (define x 5)
    (set! res (+ res (force p)))
    (set! x 10)
    (set! res (+ res (force p)))
    res)
)

(
  "Reentrancy 2"
  second
  (begin
    (define f (let ((first? #t)) (delay (if first? (begin (set! first? #f) (force f)) 'second))))
    (force f))
)

(
  "Reentrancy 3"
  (5 0 10)
  (begin
    (define q (let ((count 5))
      (define (get-count) count)
      (define p
        (delay (if (<= count 0)
                   count
                   (begin (set! count (- count 1)) (force p) (set! count (+ count 2)) count))))
      (list get-count p)))
    (define get-count (car q))
    (define p (car (cdr q)))
    (list (get-count) (force p) (get-count)))
)

(
  "Memory leak 1"
  0
  (define (from n) (delay (cons n (from (+ n 1)))))
  (define-syntax match
    (syntax-rules ()
      ((match exp
         (()      exp1)
         ((h . t) exp2))
       (let ((lst exp))
         (cond ((null? lst) exp1)
               ((pair? lst) (let ((h (car lst))
                                  (t (cdr lst)))
                              exp2))
               (else 'match-error))))))
  (define (stream-filter p? s)
    (delay-force (match (force s)
                   (()      (delay '()))
                   ((h . t) (if (p? h)
                                (delay (cons h (stream-filter p? t)))
                                (stream-filter p? t))))))
  (define (stream-ref s index)
    (delay-force (match (force s)
                   (()      'error)
                   ((h . t) (if (zero? index) (delay h) (stream-ref t (- index 1)))))))
  (force (stream-ref (stream-filter zero? (from 0)) 0))
)

(
  "Memory leak 2"
  10000
  (define s (stream-ref (from 0) 10000))
  (force s)
)

(
  "Memory leak 3"
  6000
  (define (times3 n) (stream-ref (stream-filter (lambda (x) (zero? (modulo x n))) (from 0)) 3))
  (force (times3 2000))
)
