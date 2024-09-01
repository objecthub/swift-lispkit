;;; Solve n-queens problem
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2017 Matthias Zenger. All rights reserved.
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

(import (lispkit base))

(define (queens n)
  (let try ((x 0) (y 0) (ps '()) (pss '()))
    (cond ((>= y n)                               ; all solutions found
            pss)
          ((>= x n)                               ; new solution found
            (cons (reverse ps) pss))
          ((safe? (cons x y) ps)                  ; is the current position safe?
            (try x (+ y 1) ps (try (+ x 1) 0 (cons (cons x y) ps) pss)))
          (else
            (try x (+ y 1) ps pss)))))

(define (safe? q ps)
  (cond ((null? ps)                                   #t)
        ((reach? (car q) (cdr q) (caar ps) (cdar ps)) #f)
        (else                                         (safe? q (cdr ps)))))

(define (reach? x1 y1 x2 y2)
  (or (= x1 x2) (= y1 y2) (= (abs (- x1 x2)) (abs (- y1 y2)))))

(define (pp-solutions pss n)
  (define (index p) (+ (* n (cdr p)) (car p)))
  (define (pp-board ps)
    (let ((board (make-vector (square n) ".")))
      (for-each (lambda (queen) (vector-set! board (index queen) "x")) ps)
      (let loop ((x 0) (y 0))
        (cond ((>= y n)  (newline))
              ((>= x n)  (newline)
                         (loop 0 (+ 1 y)))
              (else      (display (vector-ref board (index (cons x y))))
                         (loop (+ 1 x) y))))))
  (display (string-append (number->string (length pss)) " SOLUTIONS FOR N = " (number->string n)))
  (newline)
  (newline)
  (for-each pp-board pss))

(do ((i 4 (+ i 1)))
    ((= i 9))
  (pp-solutions (queens i) i))
