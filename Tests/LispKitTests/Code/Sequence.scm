;;; Sequence.scm
;;; Regression test data
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
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
  "Iteration over sequences"
  ((0 1 4 9 16)
   (0 1 2 3 4)
   ((1 a) (2 b) (3 c))
   (#\a #\b #\c)
   (1 2 3)
   (10 7 4 1)
   (1 2 3 4 5)
   (2 3 4)
   (5 3 1)
   (10 12 14))
  (import (lispkit sequence))
  (list (for/list ((i (in-range 5))) (* i i))
        (for/list ((i 5)) i)
        (for/list ((x '(1 2 3)) (y #(a b c d))) (list x y))
        (for/list ((c (in-string "abc"))) c)
        (for/list ((b (in-bytevector (bytevector 1 2 3)))) b)
        (for/list ((i (in-range 10 0 -3))) i)
        (for/list ((i (in-inclusive-range 1 5))) i)
        (for/list ((x (in-vector #(1 2 3 4 5) 1 4))) x)
        (for/list ((x (in-vector #(1 2 3 4 5) 4 -1 -2))) x)
        (for/list ((i 3) (j (in-naturals 10))) (+ i j)))
)

(
  "Nested iteration"
  (((1 a) (1 b) (2 a) (2 b))
   ((1 0) (2 0) (2 1))
   #t
   (2 3)
   ((1 a) (1 b) (3 a) (3 b)))
  (list (for*/list ((x '(1 2)) (y '(a b))) (list x y))
        (for*/list ((i (in-range 3)) (j (in-range i))) (list i j))
        (for*/and ((i '(1 2)) (j '(3 4))) (< i j))
        (for*/first ((i '(1 2 3)) (j '(1 2 3)) :when (= (+ i j) 5)) (list i j))
        (for/list ((i '(1 2 3)) :when (odd? i) (j '(a b))) (list i j)))
)

(
  "Accumulating iteration forms"
  (55
   120
   (3 2 1)
   (55 89)
   12
   #(0 2 4 6)
   #(0 2 4 6 0 0)
   #(0 2)
   #(0 1 2 3 x x)
   ((0 . 0) (1 . 1) (2 . 4)))
  (list (for/sum ((i (in-range 1 11))) i)
        (for/product ((i (in-range 1 6))) i)
        (for/fold ((acc '())) ((x '(1 2 3))) (cons x acc))
        (call-with-values
          (lambda () (for/fold ((a 0) (b 1)) ((i (in-range 10))) (values b (+ a b))))
          list)
        (for/fold ((acc 0) :result (* acc 2)) ((i (in-range 4))) (+ acc i))
        (for/vector ((i 4)) (* 2 i))
        (for/vector :length 6 ((i 4)) (* 2 i))
        (for/vector :length 2 ((i 4)) (* 2 i))
        (for/vector :length 6 :fill 'x ((i 4)) i)
        (sort (lambda (a b) (< (car a) (car b)))
              (hashtable->alist (for/hash ((i 3)) (values i (* i i))))))
)

(
  "Short-circuiting iteration forms"
  (#t #f 2 #f 2 3 #f)
  (list (for/and ((i '(1 2 3))) (< i 5))
        (for/and ((i '(1 2 3))) (< i 3))
        (for/or ((i '(1 2 3))) (and (> i 1) i))
        (for/or ((i '())) #t)
        (for/first ((i '(1 2 3)) :when (> i 1)) i)
        (for/last ((i '(1 2 3))) i)
        (for/first ((i '())) i))
)

(
  "Guards and breaks"
  ((0 2 4 6 8)
   (1 3 5 7 9)
   (0 1 2 3 4)
   (0 1 2 3 4)
   (1 2)
   (3 5 7)
   2)
  (list (for/list ((i (in-range 10)) :when (even? i)) i)
        (for/list ((i (in-range 10)) :unless (even? i)) i)
        (for/list ((i (in-naturals)) :break (> i 4)) i)
        (for/list ((i (in-naturals)) :final (> i 3)) i)
        (for/list ((i '(1 2 3 4))) :break (= i 3) i)
        (for/list ((i '(1 2 3))) (define y (* i 2)) (+ y 1))
        (for/and ((i '(1 2 3 4))) :break (= i 3) i))
)

(
  "Sequence constructors"
  ((a 1)
   ((a 0) (b 1) (c 2))
   (1 2 1 2 1)
   (1 2 3 4)
   ((1 a) (2 b) (3 c))
   ((1 2) (3 4) (5))
   (1 2 3)
   ("a" "b" "c")
   (1))
  (list (let ((h (make-equal-hashtable)))
          (hashtable-set! h 'a 1)
          (for/first (((k v) (in-hash h))) (list k v)))
        (for/list (((x i) (in-indexed '(a b c)))) (list x i))
        (for/list ((x (in-cycle '(1 2))) (i 5)) x)
        (for/list ((x (in-sequences '(1 2) #(3 4)))) x)
        (for/list (((x y) (in-parallel '(1 2 3) '(a b c)))) (list x y))
        (for/list ((l (in-slice 2 '(1 2 3 4 5)))) l)
        (for/list ((x (in-producer (let ((n 0)) (lambda () (set! n (+ n 1)) n)) 4))) x)
        (for/list ((l (in-lines (open-input-string "a\nb\nc")))) l)
        (for/list ((x (in-value 1))) x))
)

(
  "Sequence operations"
  ((10 20 30)
   (1 3 5 7 9)
   (1 0 2 0 3)
   10
   5
   b
   (3 4)
   (1 2 #\a)
   #t
   #f
   2
   #t
   #t)
  (list (sequence->list (sequence-map (lambda (x) (* x 10)) '(1 2 3)))
        (sequence->list (sequence-filter odd? (in-range 10)))
        (sequence->list (sequence-add-between '(1 2 3) 0))
        (sequence-fold + 0 (in-range 5))
        (sequence-length "hello")
        (sequence-ref '(a b c) 1)
        (sequence->list (sequence-tail '(1 2 3 4) 2))
        (sequence->list (sequence-append '(1) #(2) "a"))
        (sequence? '(1 2))
        (sequence? 'a)
        (sequence-count even? '(1 2 3 4))
        (sequence-andmap number? '(1 2 3))
        (sequence-ormap even? '(1 2 3)))
)

(
  "Converting sequences into streams"
  ((1 2 3)
   (a b c)
   (3 5 7)
   (5 6 7 8)
   ()
   #t
   #t
   #t
   (1 4 9)
   (0 1 2 3))
  (import (lispkit sequence) (lispkit stream))
  (list (stream->list (sequence->stream '(1 2 3)))
        (stream->list (sequence->stream #(a b c)))
        (stream->list (sequence->stream (in-range 3 8 2)))
        (stream->list (stream-take (sequence->stream (in-naturals 5)) 4))
        (stream->list (sequence->stream '()))
        (stream? (sequence->stream "ab"))
        (let ((s (list->stream '(1 2)))) (eq? s (sequence->stream s)))
        (sequence? (sequence->stream '(1)))
        (stream->list (stream-map (lambda (x) (* x x)) (sequence->stream '(1 2 3))))
        (sequence->list (in-stream (sequence->stream 4))))
)

(
  "Streams of sequences are lazy and can be traversed repeatedly"
  (0 1 (1 2 3) (1 2 3 4 5) 6)
  (import (lispkit sequence) (lispkit stream))
  (let* ((calls 0)
         (strm (sequence->stream
                 (in-producer (lambda () (set! calls (+ calls 1)) calls)
                              (lambda (x) (> x 5)))))
         (before calls)
         (head (stream-car strm))
         (after calls)
         (once (stream->list (stream-take strm 3)))
         (twice (stream->list strm)))
    (list before after once twice calls))
)

(
  "Invalid sequence for stream"
  <error>
  (sequence->stream 'a)
)

(
  "Evaluation order and large iterations"
  ((1 2 3) 4999950000 90000 (a b))
  (list (let ((log '()))
          (for ((i (in-range 3))) (set! log (cons (+ i 1) log)))
          (reverse log))
        (for/sum ((i (in-range 100000))) i)
        (for*/sum ((i (in-range 300)) (j (in-range 300))) 1)
        (for/list ((x (in-list '(a b c))) :break (eq? x 'c)) x))
)

(
  "Invalid sequence"
  <error>
  (for/list ((x 'a)) x)
)

(
  "Invalid for-clause"
  <error>
  (eval '(for/list ((x '(1 2)) :bogus) x) (environment '(lispkit base) '(lispkit sequence)))
)
