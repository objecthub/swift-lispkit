;;; Benchmark harness
;;;
;;; Defines `run-bench`, which times a thunk and prints a result line of the form
;;;   `<label>: <milliseconds> ms  result=<value>`. `run.py` parses these lines.
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


(define (run-bench label thunk)
  (let* ((t0 (current-second))
         (r (thunk))
         (t1 (current-second)))
    (display label) (display ": ")
    (display (exact (round (* 1000 (- t1 t0))))) (display " ms  result=")
    (write (if (and (pair? r) (> (length r) 3)) (list-head r 3) r)) (newline)))
