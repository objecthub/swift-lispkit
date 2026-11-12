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
  "List utility errors"
  <error>
  (list-set '(1 2) 5 'x)
)

(
  "Empty argmin"
  <error>
  (argmin car '())
)
