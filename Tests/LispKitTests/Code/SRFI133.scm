;;; SRFI133.scm
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
  "Vector basics"
  (#t
   #t
   3
   3
   3
   -32
   3
   0)
  (import (srfi 133))
  (define v (make-vector 3 3))
  (list (vector? #(1 2 3))
        (vector? (make-vector 10))
        (vector-ref v 0)
        (vector-ref v 1)
        (vector-ref v 2)
        (begin (vector-set! v 0 -32) (vector-ref v 0))
        (vector-length v)
        (vector-length '#()))
)

(
  "Vector constructors"
  (#(0 1 2 3 4)
   #(0 -1 -2 -3 -4 -5 -6 -7 -8 -9)
   #(0 1 2 3 4 5 6)
   #((0 . 4) (1 . 3) (2 . 2) (3 . 1) (4 . 0))
   #(a b c d e f g h i)
   #t
   #(g h i)
   #(d e f)
   #(1 2 3 4)
   #(x y)
   #(a b c d)
   #(a #(b) #(c))
   #(a b c d)
   #(a b h i))
  (define a2i '#(a b c d e f g h i))
  (list (vector 0 1 2 3 4)
        (vector-unfold (lambda (i x) (values x (- x 1))) 10 0)
        (vector-unfold values 7)
        (vector-unfold-right (lambda (i x) (values (cons i x) (+ x 1))) 5 0)
        (vector-copy a2i)
        (not (eqv? a2i (vector-copy a2i)))
        (vector-copy a2i 6)
        (vector-copy a2i 3 6)
        (vector-reverse-copy '#(5 4 3 2 1 0) 1 5)
        (vector-append '#(x) '#(y))
        (vector-append '#(a) '#(b c d))
        (vector-append '#(a #(b)) '#(#(c)))
        (vector-concatenate '(#(a b) #(c d)))
        (vector-append-subvectors '#(a b c d e) 0 2 '#(f g h i j) 2 4))
)

(
  "Vector predicates"
  (#f #f #f #t #t #f #f #t #t #f #t)
  (list (vector-empty? '#(a))
        (vector-empty? '#(()))
        (vector-empty? '#(#()))
        (vector-empty? '#())
        (vector= eq? '#(a b c d) '#(a b c d))
        (vector= eq? '#(a b c d) '#(a b d c))
        (vector= = '#(1 2 3 4 5) '#(1 2 3 4))
        (vector= eq?)
        (vector= eq? '#(a))
        (vector= eq? (vector (vector 'a)) (vector (vector 'a)))
        (vector= equal? (vector (vector 'a)) (vector (vector 'a))))
)

(
  "Vector iteration"
  (5
   (5 4 3 2 1 0)
   3
   (a b c d)
   #(1 4 9 16)
   #(5 8 9 8 5)
   #(0 1 4 9 16)
   #(0 2 12 36 80)
   (5 4 3 2 1 0)
   3
   2
   #(3 4 8 9 14 23 25 30 36))
  (define vos '#("abc" "abcde" "abcd"))
  (define vec '#(0 1 2 3 4 5))
  (define vec2 (vector 0 1 2 3 4))
  (define vec3 (vector 1 2 3 4 5))
  (define result '())
  (define (sqr x) (* x x))
  (list (vector-fold (lambda (len str) (max (string-length str) len)) 0 vos)
        (vector-fold (lambda (tail elt) (cons elt tail)) '() vec)
        (vector-fold (lambda (ctr n) (if (even? n) (+ ctr 1) ctr)) 0 vec)
        (vector-fold-right (lambda (tail elt) (cons elt tail)) '() '#(a b c d))
        (vector-map sqr '#(1 2 3 4))
        (vector-map * '#(1 2 3 4 5) '#(5 4 3 2 1))
        (begin (vector-map! sqr vec2) (vector-copy vec2))
        (begin (vector-map! * vec2 vec3) (vector-copy vec2))
        (begin (vector-for-each (lambda (x) (set! result (cons x result))) vec)
               (cons (car result) (cdr result)))
        (vector-count even? '#(3 1 4 1 5 9 2 5 6))
        (vector-count < '#(1 3 6 9) '#(2 4 6 8 10 12))
        (vector-cumulate + 0 '#(3 1 4 1 5 9 2 5 6)))
)

(
  "Vector searching"
  (2 1 #f 5 3 2 2 7 3 0 3 #f #t #t #f #f #f #t #f #t yes (#(1 2 3 x y z) . 3))
  (define (cmp a b) (cond ((< a b) -1)
                          ((= a b) 0)
                          (else    1)))
  (define v '#(0 2 4 6 8 10 12))
  (list (vector-index even? '#(3 1 4 1 5 9 6))
        (vector-index < '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))
        (vector-index = '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))
        (vector-index-right odd? '#(3 1 4 1 5 9 6))
        (vector-index-right < '#(3 1 4 1 5) '#(2 7 1 8 2))
        (vector-skip number? '#(1 2 a b 3 4 c d))
        (vector-skip = '#(1 2 3 4 5) '#(1 2 -3 4))
        (vector-skip-right number? '#(1 2 a b 3 4 c d))
        (vector-skip-right = '#(1 2 3 4 5) '#(1 2 -3 -4 5))
        (vector-binary-search v 0 cmp)
        (vector-binary-search v 6 cmp)
        (vector-binary-search v 1 cmp)
        (vector-any number? '#(1 2 x y z))
        (vector-any < '#(1 2 3 4 5) '#(2 1 3 4 5))
        (vector-any number? '#(a b c d e))
        (vector-any > '#(1 2 3 4 5) '#(1 2 3 4 5))
        (vector-every number? '#(1 2 x y z))
        (vector-every number? '#(1 2 3 4 5))
        (vector-every < '#(1 2 3) '#(2 3 3))
        (vector-every < '#(1 2 3) '#(2 3 4))
        (vector-any (lambda (x) (if (number? x) 'yes #f)) '#(1 2 x y z))
        (let-values (((new off) (vector-partition number? '#(1 x 2 y 3 z))))
        (cons (vector-copy new) (+ off 0))))
)

(
  "Vector mutation"
  (#(2 1 3)
   #(0 0 0)
   #(1 0 0)
   #(0 2 3)
   #(3 2 1)
   #(1 3 2)
   #(2 1 3)
   #(1 10 20 30 5)
   #(1 10 20 30 40)
   #(1 10 20 30 5)
   #(1 30 20 10 5)
   #(1 40 30 20 10)
   #(1 30 20 10 5)
   #(1 11 12 13 5)
   #(1 1 3 5 5)
   #(1 1 4 7 5)
   #(1 11 12 13 5)
   #(1 3 3 3 5)
   #(1 5 4 3 5))
  (define vs (vector 1 2 3))
  (define vf0 (vector 1 2 3))
  (define vf1 (vector 1 2 3))
  (define vf2 (vector 1 2 3))
  (define vr0 (vector 1 2 3))
  (define vr1 (vector 1 2 3))
  (define vr2 (vector 1 2 3))
  (define vc0 (vector 1 2 3 4 5))
  (define vc1 (vector 1 2 3 4 5))
  (define vc2 (vector 1 2 3 4 5))
  (define vrc0 (vector 1 2 3 4 5))
  (define vrc1 (vector 1 2 3 4 5))
  (define vrc2 (vector 1 2 3 4 5))
  (define vu0 (vector 1 2 3 4 5))
  (define vu1 (vector 1 2 3 4 5))
  (define vu2 (vector 1 2 3 4 5))
  (define vur0 (vector 1 2 3 4 5))
  (define vur1 (vector 1 2 3 4 5))
  (define vur2 (vector 1 2 3 4 5))
  (vector-swap! vs 0 1)
  (list (vector-copy vs)
        (begin (vector-fill! vf0 0) (vector-copy vf0))
        (begin (vector-fill! vf1 0 1) (vector-copy vf1))
        (begin (vector-fill! vf2 0 0 1) (vector-copy vf2))
        (begin (vector-reverse! vr0) (vector-copy vr0))
        (begin (vector-reverse! vr1 1) (vector-copy vr1))
        (begin (vector-reverse! vr2 0 2) (vector-copy vr2))
        (begin (vector-copy! vc0 1 '#(10 20 30)) (vector-copy vc0))
        (begin (vector-copy! vc1 1 '#(0 10 20 30 40) 1) (vector-copy vc1))
        (begin (vector-copy! vc2 1 '#(0 10 20 30 40) 1 4) (vector-copy vc2))
        (begin (vector-reverse-copy! vrc0 1 '#(10 20 30)) (vector-copy vrc0))
        (begin (vector-reverse-copy! vrc1 1 '#(0 10 20 30 40) 1) (vector-copy vrc1))
        (begin (vector-reverse-copy! vrc2 1 '#(0 10 20 30 40) 1 4) (vector-copy vrc2))
        (begin (vector-unfold! (lambda (i) (+ 10 i)) vu0 1 4) (vector-copy vu0))
        (begin (vector-unfold! (lambda (i x) (values (+ i x) (+ x 1))) vu1 1 4 0)
               (vector-copy vu1))
        (begin (vector-unfold! (lambda (i x y) (values (+ i x y) (+ x 1) (+ x 1))) vu2 1 4 0 0)
               (vector-copy vu2))
        (begin (vector-unfold-right! (lambda (i) (+ 10 i)) vur0 1 4) (vector-copy vur0))
        (begin (vector-unfold-right! (lambda (i x) (values (+ i x) (+ x 1))) vur1 1 4 0)
               (vector-copy vur1))
        (begin (vector-unfold-right! (lambda (i x y) (values (+ i x y) (+ x 1) (+ x 1)))
                                     vur2 1 4 0 0)
               (vector-copy vur2)))
)

(
  "Vector conversions"
  ((1 2 3)
   (2 3)
   (1 2)
   #(1 2 3)
   (3 2 1)
   (3 2)
   (2 1)
   #(3 2 1)
   "abc"
   "bc"
   "ab"
   #(#\a #\b #\c)
   #(#\b #\c)
   #(#\a #\b))
  (list (vector->list '#(1 2 3))
        (vector->list '#(1 2 3) 1)
        (vector->list '#(1 2 3) 0 2)
        (list->vector '(1 2 3))
        (reverse-vector->list '#(1 2 3))
        (reverse-vector->list '#(1 2 3) 1)
        (reverse-vector->list '#(1 2 3) 0 2)
        (reverse-list->vector '(1 2 3))
        (vector->string '#(#\a #\b #\c))
        (vector->string '#(#\a #\b #\c) 1)
        (vector->string '#(#\a #\b #\c) 0 2)
        (string->vector "abc")
        (string->vector "abc" 1)
        (string->vector "abc" 0 2))
)
