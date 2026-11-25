;;; Benchmark: fib
;;;
;;; Doubly recursive fib(27) with generic arithmetic; stresses non-tail calls
;;; of a global procedure and native calls to `<`, `+`, `-`.
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

(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(run-bench "fib" (lambda () (fib 27)))
