;;; Benchmark: doloop
;;;
;;; `do` loop with `set!` on a local variable, 3M iterations; exercises boxed
;;; local variables and branch-based loops.
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

(define (count-set n)
  (let ((acc 0)) (do ((i 0 (+ i 1))) ((= i n) acc) (set! acc (+ acc i)))))
(run-bench "doloop" (lambda () (count-set 3000000)))
