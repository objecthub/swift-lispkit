;;; Benchmark: apply
;;;
;;; Variadic procedures called directly and via `apply`, 300k iterations; each
;;; call of `sum` also enters a named let.
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

(define (sum . xs)
  (let loop ((xs xs) (s 0)) (if (null? xs) s (loop (cdr xs) (+ s (car xs))))))
(define (go)
  (let loop ((i 0) (s 0))
    (if (= i 300000) s (loop (+ i 1) (+ s (apply sum (list i 1 2)) (sum i 3))))))
(run-bench "apply" go)
