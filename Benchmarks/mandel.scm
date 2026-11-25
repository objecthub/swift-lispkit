;;; Benchmark: mandel
;;;
;;; Mandelbrot set on a 120x120 grid with at most 50 iterations per point;
;;; flonum arithmetic via generic operators.
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

(define (mandel-iter cr ci max)
  (let loop ((zr 0.0) (zi 0.0) (i 0))
    (if (or (= i max) (> (+ (* zr zr) (* zi zi)) 4.0)) i
        (loop (+ (- (* zr zr) (* zi zi)) cr) (+ (* 2.0 zr zi) ci) (+ i 1)))))
(define (mandel n)
  (let yl ((y 0) (s 0))
    (if (= y n) s
        (yl (+ y 1)
            (let xl ((x 0) (s s))
              (if (= x n) s
                  (xl (+ x 1)
                      (+ s (mandel-iter (- (* 3.0 (/ x n)) 2.0) (- (* 2.0 (/ y n)) 1.0) 50)))))))))
(run-bench "mandel" (lambda () (mandel 120)))
