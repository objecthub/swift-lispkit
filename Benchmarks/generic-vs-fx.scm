;;; Generic vs. specialized arithmetic
;;;
;;; Runs fib, tak, a named-let loop, a `do` loop, sieve, and mandelbrot twice:
;;;   once with generic arithmetic (`+`, `<`, ...) and once with the fixnum/flonum
;;;   specific operations (`fx+`, `fl*`, ...), which the compiler turns into VM
;;;   instructions. The difference is an upper bound for what compiling generic
;;;   arithmetic into instructions with fast paths can gain. Not part of the
;;;   default suite of `run.py`; run it with `run.py generic-vs-fx`.
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
(define (fibfx n) (if (fx< n 2) n (fx+ (fibfx (fx- n 1)) (fibfx (fx- n 2)))))
(run-bench "fib/generic" (lambda () (fib 27)))
(run-bench "fib/fx" (lambda () (fibfx 27)))

(define (tak x y z)
  (if (not (< y x)) z (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y))))
(define (takfx x y z)
  (if (not (fx< y x)) z
      (takfx (takfx (fx- x 1) y z) (takfx (fx- y 1) z x) (takfx (fx- z 1) x y))))
(run-bench "tak/generic" (lambda () (tak 22 16 8)))
(run-bench "tak/fx" (lambda () (takfx 22 16 8)))

(define (sum-to n) (let loop ((i 0) (acc 0)) (if (= i n) acc (loop (+ i 1) (+ acc i)))))
(define (sum-to-fx n)
  (let loop ((i 0) (acc 0)) (if (fx= i n) acc (loop (fx1+ i) (fx+ acc i)))))
(define (sum-do-fx n) (do ((i 0 (fx1+ i)) (acc 0 (fx+ acc i))) ((fx= i n) acc)))
(run-bench "named-let/generic" (lambda () (sum-to 1000000)))
(run-bench "named-let/fx" (lambda () (sum-to-fx 1000000)))
(run-bench "do-loop/fx" (lambda () (sum-do-fx 1000000)))

(define (sieve n)
  (let ((v (make-vector (+ n 1) #t)))
    (vector-set! v 0 #f) (vector-set! v 1 #f)
    (do ((i 2 (+ i 1))) ((> (* i i) n))
      (if (vector-ref v i) (do ((j (* i i) (+ j i))) ((> j n)) (vector-set! v j #f))))
    (do ((i 0 (+ i 1)) (c 0 (if (vector-ref v i) (+ c 1) c))) ((> i n) c))))
(define (sievefx n)
  (let ((v (make-vector (fx1+ n) #t)))
    (vector-set! v 0 #f) (vector-set! v 1 #f)
    (do ((i 2 (fx1+ i))) ((fx> (fx* i i) n))
      (if (vector-ref v i) (do ((j (fx* i i) (fx+ j i))) ((fx> j n)) (vector-set! v j #f))))
    (do ((i 0 (fx1+ i)) (c 0 (if (vector-ref v i) (fx1+ c) c))) ((fx> i n) c))))
(run-bench "sieve/generic" (lambda () (sieve 1000000)))
(run-bench "sieve/fx" (lambda () (sievefx 1000000)))

(define (mi cr ci max)
  (let loop ((zr 0.0) (zi 0.0) (i 0))
    (if (or (= i max) (> (+ (* zr zr) (* zi zi)) 4.0)) i
        (loop (+ (- (* zr zr) (* zi zi)) cr) (+ (* 2.0 zr zi) ci) (+ i 1)))))
(define (mifl cr ci max)
  (let loop ((zr 0.0) (zi 0.0) (i 0))
    (if (or (fx= i max) (fl> (fl+ (fl* zr zr) (fl* zi zi)) 4.0)) i
        (loop (fl+ (fl- (fl* zr zr) (fl* zi zi)) cr) (fl+ (fl* 2.0 (fl* zr zi)) ci) (fx1+ i)))))
(define (mloop f n)
  (do ((y 0 (+ y 1))
       (s 0 (do ((x 0 (+ x 1))
                 (s s (+ s (f (- (* 3.0 (/ x n)) 2.0) (- (* 2.0 (/ y n)) 1.0) 50))))
                ((= x n) s))))
      ((= y n) s)))
(run-bench "mandel/generic" (lambda () (mloop mi 100)))
(run-bench "mandel/fl" (lambda () (mloop mifl 100)))
