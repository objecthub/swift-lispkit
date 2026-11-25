;;; Benchmark: sieve
;;;
;;; Sieve of Eratosthenes up to 1M using a vector and nested `do` loops;
;;; exercises `vector-ref`/`vector-set!` and generic arithmetic.
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

(define (sieve n)
  (let ((v (make-vector (+ n 1) #t)))
    (vector-set! v 0 #f) (vector-set! v 1 #f)
    (do ((i 2 (+ i 1))) ((> (* i i) n))
      (if (vector-ref v i)
          (do ((j (* i i) (+ j i))) ((> j n)) (vector-set! v j #f))))
    (let loop ((i 0) (c 0)) (if (> i n) c (loop (+ i 1) (if (vector-ref v i) (+ c 1) c))))))
(run-bench "sieve" (lambda () (sieve 1000000)))
