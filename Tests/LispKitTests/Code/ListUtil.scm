;;; ListUtil.scm
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
  "List utilities"
  ((1 2 3 4 5)
   (7)
   ()
   (a x b x c)
   (a)
   ()
   (a x c)
   (1 2 30)
   (1 b)
   (3 a)
   (1 2 3 4 5 6)
   20000)
  (import (lispkit list util))
  (list (flatten '(1 (2 (3 . 4)) () 5))
        (flatten 7)
        (flatten '())
        (add-between '(a b c) 'x)
        (add-between '(a) 'x)
        (add-between '() 'x)
        (list-set '(a b c) 1 'x)
        (list-update '(1 2 3) 2 (lambda (x) (* x 10)))
        (argmin car '((3 a) (1 b) (1 c) (2 d)))
        (argmax car '((3 a) (1 b) (3 c) (2 d)))
        (sort < (shuffle '(5 3 1 6 2 4)))
        (length (flatten (iota 20000))))
)

(
  "Grouping and duplicates"
  (((1 4 7) (2 5) (3 6))
   (((a 1) (a 3)) ((b 2) (b 5)) ((c 4)))
   ((1 3 5) (2 4))
   2
   #f
   "A")
  (list (group-by (lambda (x) (modulo x 3)) '(1 2 3 4 5 6 7))
        (group-by car '((a 1) (b 2) (a 3) (c 4) (b 5)))
        (group-by (lambda (x) (modulo x 2)) '(1 2 3 4 5) =)
        (check-duplicates '(1 2 3 2 1))
        (check-duplicates '(1 2 3))
        (check-duplicates '("a" "b" "A" "b") string-ci=?))
)

(
  "Combinatorics"
  (((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
   (())
   ((a b) (a c) (b c))
   (() (a) (b) (c) (a b) (a c) (b c) (a b c))
   ((1 a) (1 b) (2 a) (2 b))
   (())
   ((1) (2))
   720
   10)
  (list (permutations '(1 2 3))
        (permutations '())
        (combinations '(a b c) 2)
        (combinations '(a b c))
        (cartesian-product '(1 2) '(a b))
        (cartesian-product)
        (cartesian-product '(1 2))
        (length (permutations '(1 2 3 4 5 6)))
        (length (combinations '(a b c d e) 3)))
)

(
  "Element access"
  (a b c d e e a 2)
  (list (first '(a b c d e))
        (second '(a b c d e))
        (third '(a b c d e))
        (fourth '(a b c d e))
        (fifth '(a b c d e))
        (last '(a b c d e))
        (last '(a))
        (last '(1 2 . 3)))
)

(
  "Searching"
  (9 #f #f 2 #f 2 16 #f #f #t (4 9) 30 #f #t #t)
  (list (find (lambda (x) (> x 5)) '(3 1 9 2 8))
        (find even? '(1 3 5))
        (find even? '())
        (list-index even? '(1 3 4 5 6))
        (list-index (lambda (x) (> x 10)) '(1 2 3))
        (list-index = '(3 1 4) '(2 7 4))
        (any (lambda (x) (and (even? x) (* x x))) '(1 3 4 5))
        (any even? '(1 3 5))
        (any even? '())
        (any < '(5 1 9) '(1 2 3))
        (any (lambda (a b) (and (< a b) (list a b))) '(3 4 5) '(1 9))
        (every (lambda (x) (and (> x 0) (* x 10))) '(1 2 3))
        (every even? '(2 3 4))
        (every even? '())
        (every < '(1 2 3) '(2 3 4 0)))
)

(
  "Counting and folding"
  (8 2 0 (3 2 1) 2 (2 b (1 a z)) 0 10 0 2 9)
  (list (count odd? '(3 1 4 1 5 9 2 6 5 3 5))
        (count < '(1 5 3) '(2 4 6 0))
        (count even? '())
        (fold cons '() '(1 2 3))
        (fold - 0 '(1 2 3))
        (fold list 'z '(1 2 3) '(a b))
        (fold + 0 '())
        (reduce + 0 '(1 2 3 4))
        (reduce + 0 '())
        (reduce - 0 '(1 2 3 4))
        (reduce max 0 '(3 9 2)))
)

(
  "Removing duplicates and zipping"
  ((1 2 3 4) () ("a" "b") ((a 1) (b 2)) (1)
   ((1) (2) (3)) ((1 a) (2 b) (3 c)) ((1 a x) (2 b y)) ())
  (list (delete-duplicates '(1 2 1 3 2 4))
        (delete-duplicates '())
        (delete-duplicates '("a" "A" "b" "B" "a") string-ci=?)
        (delete-duplicates '((a 1) (b 2) (a 3)) (lambda (x y) (eq? (car x) (car y))))
        (delete-duplicates '(1 2 3 4) <)
        (zip '(1 2 3))
        (zip '(1 2 3) '(a b c))
        (zip '(1 2 3) '(a b) '(x y z))
        (zip '() '(1)))
)

(
  "SRFI 1 procedures on long lists"
  (199990000 10000 19999 #t #t 19999 20000 199990000)
  (let ((big (iota 20000)))
    (list (fold + 0 big)
          (count even? big)
          (last big)
          (any (lambda (x) (= x 19999)) big)
          (every (lambda (x) (>= x 0)) big)
          (list-index (lambda (x) (= x 19999)) big)
          (length (zip big big))
          (reduce + 0 big)))
)

(
  "Prefixes and suffixes"
  ((2 4) () () (2 4)
   (5 6) () ()
   (d e) () (a b c) (2 3 . d)
   (a b c) (a b c) () (1)
   ((a b) (c d e)) (() (a b)) ((a b) ()) (() ()))
  (list (take-while even? '(2 4 5 6))
        (take-while even? '())
        (take-while even? '(1 2))
        (take-while even? '(2 4))
        (drop-while even? '(2 4 5 6))
        (drop-while even? '())
        (drop-while even? '(2 4))
        (take-right '(a b c d e) 2)
        (take-right '(a b c) 0)
        (take-right '(a b c) 3)
        (take-right '(1 2 3 . d) 2)
        (drop-right '(a b c d e) 2)
        (drop-right '(a b c) 0)
        (drop-right '(a b c) 3)
        (drop-right '(1 2 3 . d) 2)
        (call-with-values (lambda () (split-at '(a b c d e) 2)) list)
        (call-with-values (lambda () (split-at '(a b) 0)) list)
        (call-with-values (lambda () (split-at '(a b) 2)) list)
        (call-with-values (lambda () (split-at '() 0)) list))
)

(
  "Prefixes and suffixes of long lists"
  (19990 10 5 19995 (10000 10000))
  (let ((big (iota 20000)))
    (list (length (take-while (lambda (x) (< x 19990)) big))
          (length (drop-while (lambda (x) (< x 19990)) big))
          (length (take-right big 5))
          (length (drop-right big 5))
          (call-with-values (lambda () (split-at big 10000))
                            (lambda (a b) (list (length a) (length b))))))
)

(
  "List utility errors"
  <error>
  (list-set '(1 2) 5 'x)
)

(
  "Empty argmin"
  <error>
  (argmin car '())
)

(
  "First of empty list"
  <error>
  (first '())
)

(
  "Third of short list"
  <error>
  (third '(1 2))
)

(
  "Fourth of short list"
  <error>
  (fourth '(1 2 3))
)

(
  "Fifth of short list"
  <error>
  (fifth '(1 2 3 4))
)

(
  "Last of empty list"
  <error>
  (last '())
)

(
  "Reduce of non-list"
  <error>
  (reduce + 0 5)
)

(
  "Take-right of short list"
  <error>
  (take-right '(1 2) 3)
)

(
  "Drop-right of short list"
  <error>
  (drop-right '(1 2) 3)
)

(
  "Split-at of short list"
  <error>
  (split-at '(1 2) 3)
)
