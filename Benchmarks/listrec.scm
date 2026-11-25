;;; Benchmark: listrec
;;;
;;; User-defined non-tail-recursive `map` over a 5k-element list, 300 times.
;;; The deep stack of shared list suffixes makes periodic garbage collection
;;; expensive.
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

(define (my-map f l) (if (null? l) '() (cons (f (car l)) (my-map f (cdr l)))))
(define (my-len l) (let loop ((l l) (n 0)) (if (pair? l) (loop (cdr l) (+ n 1)) n)))
(define (go)
  (let ((l (iota 5000)))
    (let loop ((k 0) (r 0))
      (if (= k 300) r (loop (+ k 1) (+ r (my-len (my-map (lambda (x) (+ x 1)) l))))))))
(run-bench "listrec" go)
