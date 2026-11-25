;;; Benchmark: nqueens
;;;
;;; Counts the solutions of the 9-queens problem using lists and internal
;;; definitions.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may
;;; not use this file except in compliance with the License. You may obtain
;;; a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(load "harness.scm")

(define (queens board-size)
  (define (ok? row dist placed)
    (or (null? placed)
        (and (not (= (car placed) (+ row dist)))
             (not (= (car placed) (- row dist)))
             (not (= (car placed) row))
             (ok? row (+ dist 1) (cdr placed)))))
  (define (try x y z)
    (if (null? x)
        (if (null? y) 1 0)
        (+ (if (ok? (car x) 1 z) (try (append (cdr x) y) '() (cons (car x) z)) 0)
           (try (cdr x) (cons (car x) y) z))))
  (try (iota board-size 1) '() '()))
(run-bench "nqueens" (lambda () (queens 9)))
