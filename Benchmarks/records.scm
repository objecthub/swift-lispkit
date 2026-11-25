;;; Benchmark: records
;;;
;;; Allocates 500k records and uses their accessors and modifiers.
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

(define-record-type point (make-point x y) point? (x point-x set-point-x!) (y point-y))
(define (go)
  (let loop ((i 0) (s 0))
    (if (= i 500000) s
        (let ((p (make-point i (+ i 1))))
          (set-point-x! p (+ (point-x p) 1))
          (loop (+ i 1) (+ s (point-x p) (point-y p)))))))
(run-bench "records" go)
