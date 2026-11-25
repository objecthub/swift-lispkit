;;; Benchmark: hashtable
;;;
;;; Inserts 200k fixnum keys into an `equal?` hashtable and looks them up.
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
  (let ((ht (make-equal-hashtable)))
    (do ((i 0 (+ i 1))) ((= i 200000)) (hashtable-set! ht i (* i i)))
    (let loop ((i 0) (s 0)) (if (= i 200000) s (loop (+ i 1) (+ s (hashtable-ref ht i 0)))))))
(run-bench "hashtable" go)
