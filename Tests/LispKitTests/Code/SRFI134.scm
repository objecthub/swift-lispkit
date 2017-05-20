;;; SRFI134.scm
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
  "Ideque constructors"
  (()
   ()
   (1 2 3)
   (4 5 6 7)
   (10 9 8 7 6 5 4 3 2 1)
   (1 2 3 4 5 6 7 8 9 10)
   (0 2 4 6 8 10)
   ()
   ()
   ())
  (import (srfi 134))
  (list (ideque->list (ideque))
        (ideque->list (list->ideque '()))
        (ideque->list (ideque 1 2 3))
        (ideque->list (list->ideque '(4 5 6 7)))
        (ideque->list (ideque-unfold zero? values (lambda (n) (- n 1)) 10))
        (ideque->list (ideque-unfold-right zero? values (lambda (n) (- n 1)) 10))
        (ideque->list (ideque-tabulate 6 (lambda (n) (* n 2))))
        (ideque->list (ideque-unfold (lambda (n) #t) values (lambda (n) (+ n 1)) 0))
        (ideque->list (ideque-unfold-right (lambda (n) #t) values (lambda (n) (+ n 1)) 0))
        (ideque->list (ideque-tabulate 0 values)))
)

(
  "Ideque predicates"
  (#t #t #t #t #t #t #t #t #t #t #t #t #t #t)
  (list (ideque? (ideque))
        (not (ideque? 1))
        (ideque-empty? (ideque))
        (not (ideque-empty? (ideque 1)))
        (ideque= eq?)
        (ideque= eq? (ideque 1))
        (ideque= char-ci=? (ideque #\a #\b) (ideque #\A #\B))
        (ideque= char-ci=? (ideque) (ideque))
        (not (ideque= char-ci=? (ideque #\a #\b) (ideque #\A #\B #\c)))
        (not (ideque= char-ci=? (ideque #\a #\b) (ideque #\A)))
        (ideque= char-ci=? (ideque) (ideque) (ideque))
        (ideque= char-ci=? (ideque #\a #\b) (ideque #\A #\B) (ideque #\a #\B))
        (not (ideque= char-ci=? (ideque #\a #\b) (ideque #\A) (ideque #\a #\B)))
        (not (ideque= char-ci=? (ideque #\a #\b) (ideque #\A #\B) (ideque #\A #\B #\c))))
)

(
  "Ideque operations"
  (1 3 2 2 1 3 #t #t 0 0)
  (list (ideque-front (ideque 1 2 3))
        (ideque-back (ideque 1 2 3))
        (ideque-front (ideque-remove-front (ideque 1 2 3)))
        (ideque-back (ideque-remove-back (ideque 1 2 3)))
        (ideque-front (ideque-remove-back (ideque 1 2 3)))
        (ideque-back (ideque-remove-front (ideque 1 2 3)))
        (ideque-empty? (ideque-remove-front (ideque 1)))
        (ideque-empty? (ideque-remove-back (ideque 1)))
        (ideque-front (ideque-add-front (ideque 1 2 3) 0))
        (ideque-back (ideque-add-back (ideque 1 2 3) 0)))
)

(
  "Ideque accessors"
  (#t #t (3 2 1))
  (import (srfi 1))
  (define (check ideque-op list-op lis)
    (let ((dq (list->ideque lis)))
      (every (lambda (i) (equal? (list-op lis i) (ideque->list (ideque-op dq i)))) lis)))
  (list (check ideque-take take '(0 1 2 3 4 5 6))
        (check ideque-drop drop '(0 1 2 3 4 5))
        (map (lambda (n) (ideque-ref (ideque 3 2 1) n)) '(0 1 2)))
)

(
  "Ideque advanced operations"
  (7
   0
   ()
   ()
   (1 2 3 a b c d 5 6 7 8 9)
   ()
   (5 4 3 2 1)
   0
   3
   ((1 a) (2 b) (3 c))
   ((1 a x) (2 b y) (3 c z))
   ((1) (2) (3))
   ())
  (list (ideque-length (ideque 1 2 3 4 5 6 7))
        (ideque-length (ideque))
        (ideque->list (ideque-append))
        (ideque->list (ideque-append (ideque) (ideque)))
        (ideque->list (ideque-append (ideque 1 2 3) (ideque 'a 'b 'c 'd) (ideque) (ideque 5 6 7 8 9)))
        (ideque->list (ideque-reverse (ideque)))
        (ideque->list (ideque-reverse (ideque 1 2 3 4 5)))
        (ideque-count odd? (ideque))
        (ideque-count odd? (ideque 1 2 3 4 5))
        (ideque->list (ideque-zip (ideque 1 2 3) (ideque 'a 'b 'c 'd 'e)))
        (ideque->list (ideque-zip (ideque 1 2 3 4 5) (ideque 'a 'b 'c 'd 'e) (ideque 'x 'y 'z)))
        (ideque->list (ideque-zip (ideque 1 2 3)))
        (ideque->list (ideque-zip (ideque 1 2 3) (ideque))))
)

(
  "Ideque mapping"
  (#t
   (-1 -2 -3 -4 -5)
   (-1 -3 5 -8)
   (5 4 3 2 1)
   (1 2 3 4 5)
   (5 4 3 2 1 . z)
   (1 2 3 4 5 . z)
   (a a b b c c))
  (list (ideque-empty? (ideque-map list (ideque)))
        (ideque->list (ideque-map - (ideque 1 2 3 4 5)))
        (ideque->list (ideque-filter-map (lambda (x) (and (number? x) (- x))) (ideque 1 3 'a -5 8)))
        (let ((r '())) (ideque-for-each (lambda (n) (set! r (cons n r))) (ideque 1 2 3 4 5)) r)
        (let ((r '())) (ideque-for-each-right (lambda (n) (set! r (cons n r))) (ideque 1 2 3 4 5)) r)
        (ideque-fold cons 'z (ideque 1 2 3 4 5))
        (ideque-fold-right cons 'z (ideque 1 2 3 4 5))
        (ideque->list (ideque-append-map (lambda (x) (list x x)) (ideque 'a 'b 'c))))
)

(
  "Ideque filtering"
  ((1 3 5)
   (2 4)
   ((1 3 5) (2 4)))
  (import (srfi 8))
  (list (ideque->list (ideque-filter odd? (ideque 1 2 3 4 5)))
        (ideque->list (ideque-remove odd? (ideque 1 2 3 4 5)))
        (receive xs (ideque-partition odd? (ideque 1 2 3 4 5)) (map ideque->list xs)))
)

(
  "Ideque searching"
  (3
   boo
   #f
   4
   boo
   #f
   (1 3 2)
   (5 8 4 6 3 4 2)
   (3 4 2)
   (1 3 2 5 8 4 6)
   ()
   ()
   ()
   ()
   ((1 3 2) (5 8 4 6 3 4 2))
   ((5 8) (4 6 3 4 2 9))
   3
   5
   #f
   9
   #f
   1
   #f
   (1 2 3)
   ()
   (1 2 3)
   ())
  (import (srfi 121))
  (list (ideque-find number? (ideque 'a 3 'b 'c 4 'd) (lambda () 'boo))
        (ideque-find number? (ideque 'a 'b 'c 'd) (lambda () 'boo))
        (ideque-find number? (ideque 'a 'b 'c 'd))
        (ideque-find-right number? (ideque 'a 3 'b 'c 4 'd) (lambda () 'boo))
        (ideque-find-right number? (ideque 'a 'b 'c 'd) (lambda () 'boo))
        (ideque-find-right number? (ideque 'a 'b 'c 'd))
        (ideque->list (ideque-take-while (lambda (n) (< n 5)) (ideque 1 3 2 5 8 4 6 3 4 2)))
        (ideque->list (ideque-drop-while (lambda (n) (< n 5)) (ideque 1 3 2 5 8 4 6 3 4 2)))
        (ideque->list (ideque-take-while-right (lambda (n) (< n 5)) (ideque 1 3 2 5 8 4 6 3 4 2)))
        (ideque->list (ideque-drop-while-right (lambda (n) (< n 5)) (ideque 1 3 2 5 8 4 6 3 4 2)))
        (ideque->list (ideque-take-while (lambda (n) (< n 5)) (ideque 5 8 4 6 3 4 2 9)))
        (ideque->list (ideque-drop-while (lambda (n) (< n 5)) (ideque 1 4 3 2 3 4 2 1)))
        (ideque->list (ideque-take-while-right (lambda (n) (< n 5)) (ideque 5 8 4 6 3 4 2 9)))
        (ideque->list (ideque-drop-while-right (lambda (n) (< n 5)) (ideque 1 3 2 4 3 2 3 2)))
        (receive xs (ideque-span (lambda (n) (< n 5)) (ideque 1 3 2 5 8 4 6 3 4 2)) (map ideque->list xs))
        (receive xs (ideque-break (lambda (n) (< n 5)) (ideque 5 8 4 6 3 4 2 9)) (map ideque->list xs))
        (ideque-any (lambda (x) (and (number? x) x)) (ideque 'a 3 'b 'c 4 'd 'e))
        (ideque-any (lambda (x) (and (number? x) x)) (ideque 'a 'b 'c 'd 'e 5))
        (ideque-any (lambda (x) (and (number? x) x)) (ideque 'a 'b 'c 'd 'e))
        (ideque-every (lambda (x) (and (number? x) x)) (ideque 1 5 3 2 9))
        (ideque-every (lambda (x) (and (number? x) x)) (ideque 1 5 'a 2 9))
        (ideque-any (lambda (x) (and (odd? x) x)) (ideque 2 1 'a 'b 'c 'd))
        (ideque-every (lambda (x) (and (odd? x) x)) (ideque 1 2 'a 'b 'c 'd))
        (generator->list (ideque->generator (ideque 1 2 3)))
        (generator->list (ideque->generator (ideque)))
        (ideque->list (generator->ideque (generator 1 2 3)))
        (ideque->list (generator->ideque (generator))))
)
