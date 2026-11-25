;;; Benchmark: closures
;;;
;;; Closure creation and invocation, a counter closure mutating a captured
;;; variable, and composed closures; 1M iterations.
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

(define (make-counter) (let ((n 0)) (lambda () (set! n (+ n 1)) n)))
(define (compose f g) (lambda (x) (f (g x))))
(define (go)
  (let ((c (make-counter)) (h (compose (lambda (x) (+ x 1)) (lambda (x) (* x 2)))))
    (let loop ((i 0) (s 0))
      (if (= i 1000000) (+ s (c)) (begin (c) (loop (+ i 1) (+ s (h i))))))))
(run-bench "closures" go)
