;;; Benchmark: strings
;;;
;;; String conversion, concatenation, and access, 100k iterations.
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

(define (go)
  (let loop ((i 0) (acc 0))
    (if (= i 100000) acc
        (let* ((s (number->string i)) (t (string-append s "-" s)))
          (loop (+ i 1) (+ acc (string-length t) (char->integer (string-ref t 0))))))))
(run-bench "strings" go)
