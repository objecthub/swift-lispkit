;;; Math.scm
;;; Regression test data
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2022 ObjectHub. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;      http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(
  "Fixnum basic"
  (0 2 4 9 -5 1 7 5040 1 0 4 3 3)
  (list (fx+)
        (fx+ 2)
        (fx+ 1 2 -3 4)
        (fx- -9)
        (fx- 10 8 7)
        (fx*)
        (fx* 7)
        (fx* 2 3 4 5 6 7)
        (fx/ 1)
        (fx/ 2)
        (fx/ 16 4)
        (fx/ 60 5 4)
        (fx/ 60 5 2 2))
)

(
  "Fixnum basic apply"
  (0 2 4 9 -5 1 7 5040 1 0 4 3 3)
  (list (apply fx+ '())
        (apply fx+ '(2))
        (apply fx+ '(1 2 -3 4))
        (apply fx- '(-9))
        (apply fx- '(10 8 7))
        (apply fx* '())
        (apply fx* '(7))
        (apply fx* '(2 3 4 5 6 7))
        (apply fx/ '(1))
        (apply fx/ '(2))
        (apply fx/ '(16 4))
        (apply fx/ '(60 5 4))
        (apply fx/ '(60 5 2 2)))
)

(
  "Fixnum comparisons"
  (#t #t #f #t #t #f #f #f #f #t #t #f #t #t #f #t)
  (list (fx= 5 (fx+ 1 4))
        (fx= 1 1 1 1)
        (fx= 1 1 2 1 1)
        (fx< 3 6)
        (fx< 1 2 3 4)
        (fx< 1 2 0 3 4)
        (fx> 3 6)
        (fx> 1 2 3 4)
        (fx> 1 2 0 3 4)
        (fx> 12 7 3 -1 -4)
        (fx<= 3 3 4 4 4)
        (fx<= 3 2 4 5)
        (fx<= 1 2)
        (fx>= 3 3)
        (fx>= 3 4)
        (fx>= 10 10 9 8 8 8 7))
)

(
  "Fixnum comparisons apply"
  (#t #t #f #t #t #f #f #f #f #t #t #f #t #t #f #t)
  (list (apply fx= (list 5 (fx+ 1 4)))
        (apply fx= '(1 1 1 1))
        (apply fx= '(1 1 2 1 1))
        (apply fx< '(3 6))
        (apply fx< '(1 2 3 4))
        (apply fx< '(1 2 0 3 4))
        (apply fx> '(3 6))
        (apply fx> '(1 2 3 4))
        (apply fx> '(1 2 0 3 4))
        (apply fx> '(12 7 3 -1 -4))
        (apply fx<= '(3 3 4 4 4))
        (apply fx<= '(3 2 4 5))
        (apply fx<= '(1 2))
        (apply fx>= '(3 3))
        (apply fx>= '(3 4))
        (apply fx>= '(10 10 9 8 8 8 7)))
)

(
  "Flonum basic"
  (0.0 2.0 4.0 9.0 -5.0 1.0 7.0 5040.0 1.0 0.5 4.0 3.0 3.0)
  (list (fl+)
        (fl+ 2.0)
        (fl+ 1.0 2.0 -3.0 4.0)
        (fl- -9.0)
        (fl- 10.0 8.0 7.0)
        (fl*)
        (fl* 7.0)
        (fl* 2.0 3.0 4.0 5.0 6.0 7.0)
        (fl/ 1.0)
        (fl/ 2.0)
        (fl/ 16.0 4.0)
        (fl/ 60.0 5.0 4.0)
        (fl/ 60.0 5.0 2.0 2.0))
)

(
  "Flonum basic apply"
  (0.0 2.0 4.0 9.0 -5.0 1.0 7.0 5040.0 1.0 0.5 4.0 3.0 3.0)
  (list (apply fl+ '())
        (apply fl+ '(2.0))
        (apply fl+ '(1.0 2.0 -3.0 4.0))
        (apply fl- '(-9.0))
        (apply fl- '(10.0 8.0 7.0))
        (apply fl* '())
        (apply fl* '(7.0))
        (apply fl* '(2.0 3.0 4.0 5.0 6.0 7.0))
        (apply fl/ '(1.0))
        (apply fl/ '(2.0))
        (apply fl/ '(16.0 4.0))
        (apply fl/ '(60.0 5.0 4.0))
        (apply fl/ '(60.0 5.0 2.0 2.0)))
)

(
  "Flonum comparisons"
  (#t #t #f #t #t #f #f #f #f #t #t #f #t #t #f #t)
  (list (fl= 5.0 (fl+ 1.5 3.5))
        (fl= 1.0 1.0 1.0 1.0)
        (fl= 1.0 1.0 2.0 1.0 1.0)
        (fl< 3.0 6.0)
        (fl< 1.0 2.0 3.0 4.0)
        (fl< 1.0 2.0 0.0 3.0 4.0)
        (fl> 3.0 6.0)
        (fl> 1.0 2.0 3.0 4.0)
        (fl> 1.0 2.0 0.0 3.0 4.0)
        (fl> 12.0 7.0 3.0 -1.0 -4.0)
        (fl<= 3.0 3.0 4.0 4.0 4.0)
        (fl<= 3.0 2.0 4.0 5.0)
        (fl<= 1.0 2.0)
        (fl>= 3.0 3.0)
        (fl>= 3.0 4.0)
        (fl>= 10.0 10.0 9.0 8.0 8.0 8.0 7.0))
)

(
  "Flonum comparisons apply"
  (#t #t #f #t #t #f #f #f #f #t #t #f #t #t #f #t)
  (list (apply fl= (list 5.0 (fl+ 1.5 3.5)))
        (apply fl= '(1.0 1.0 1.0 1.0))
        (apply fl= '(1.0 1.0 2.0 1.0 1.0))
        (apply fl< '(3.0 6.0))
        (apply fl< '(1.0 2.0 3.0 4.0))
        (apply fl< '(1.0 2.0 0.0 3.0 4.0))
        (apply fl> '(3.0 6.0))
        (apply fl> '(1.0 2.0 3.0 4.0))
        (apply fl> '(1.0 2.0 0.0 3.0 4.0))
        (apply fl> '(12.0 7.0 3.0 -1.0 -4.0))
        (apply fl<= '(3.0 3.0 4.0 4.0 4.0))
        (apply fl<= '(3.0 2.0 4.0 5.0))
        (apply fl<= '(1.0 2.0))
        (apply fl>= '(3.0 3.0))
        (apply fl>= '(3.0 4.0))
        (apply fl>= '(10.0 10.0 9.0 8.0 8.0 8.0 7.0)))
)

(
  "Generic arithmetic with fast paths"
  (7 -1 12 3/4 2 5.5 -0.5 7.5 0.5 2.5 4.0 2.0 3.75 2.5 -0.0 -0.0 10 4 24 1)
  (let ((x 3) (y 4) (z 2.5) (w 2))
    (list (+ x y) (- x y) (* x y) (/ x y) (/ y w) (+ x z) (- z x) (* x z) (/ z 5)
          (+ z 0) (+ 1.5 z) (- 4.5 z) (* 1.5 z) (/ 5.0 w) (+ -0.0 -0.0) (+ -0.0)
          (+ 1 2 x y) (- 10 x w 1) (* x y w) (/ 12 x y)))
)

(
  "Generic arithmetic overflow"
  (9223372036854775808 -9223372036854775809 18446744073709551614 9223372036854775808
   -9223372036854775807 4611686018427387904)
  (let ((max 9223372036854775807) (min -9223372036854775808) (one 1))
    (list (+ max one) (- min one) (* max 2) (/ min -1) (+ min one) (/ min -2)))
)

(
  "Generic arithmetic with other numbers"
  (5/6 1/6 1/6 3/2 123456789012345678901234567891 1.5+2.i 1 5/2)
  (let ((a 1/2) (b 1/3) (big 123456789012345678901234567890) (c 1.+2.i) (d 1/2))
    (list (+ a b) (- a b) (* a b) (/ a b) (+ big 1) (+ c 0.5) (+ a d) (+ 2 a)))
)

(
  "Generic arithmetic errors"
  (error error error error error error error error error error)
  (let ((x 1) (s 'a) (z 0))
    (map (lambda (thunk) (guard (e (#t 'error)) (thunk)))
         (list (lambda () (+ x s)) (lambda () (- s x)) (lambda () (* "x" x))
               (lambda () (/ x z)) (lambda () (/ x s)) (lambda () (< x s))
               (lambda () (= s x)) (lambda () (> #f x)) (lambda () (<= x 'b))
               (lambda () (>= x "s")))))
)

(
  "Generic comparisons with fast paths"
  (#t #f #f #t #t #f #t #f #f #f #f #f #f #t #t #f #f #t #t #t)
  (let ((x 3) (y 4) (u 3.0) (v 4.5) (nan +nan.0) (big 9007199254740993) (bigf 9007199254740992.0))
    (list (< x y) (> x y) (= x y) (<= x x) (>= y x)
          (< v u) (= x u) (< y u) (> x v) (= nan nan) (< nan x) (> u nan) (<= nan nan)
          (> big bigf) (= 9007199254740992 bigf) (= big bigf) (< +inf.0 x) (< -inf.0 u)
          (< x y 5) (= x u 3)))
)

(
  "Floor division with remainder zero"
  ((-2 0) -2 0 0 0 1 -1 (-2.0 0.0) 0.0 0 -61728394506172839450617283945 0 -1 (-4 1) (-4 -1))
  (list (call-with-values (lambda () (floor/ 4 -2)) list) (floor-quotient 4 -2)
        (floor-remainder 4 -2) (modulo 4 -2) (modulo 0 -5) (modulo -7 2) (modulo 7 -2)
        (call-with-values (lambda () (floor/ 4.0 -2.0)) list) (modulo 4.0 -2.0)
        (modulo 123456789012345678901234567890 -2)
        (floor-quotient 123456789012345678901234567890 -2)
        (fxmodulo 4 -2) (fxmodulo 7 -2)
        (call-with-values (lambda () (floor/ -7 2)) list)
        (call-with-values (lambda () (floor/ 7 -2)) list))
)

(
  "Arithmetic with the smallest fixnum"
  (9223372036854775808 9223372036854775808 9223372036854775808 -1/9223372036854775808
   -9223372036854775808/3 -3/9223372036854775808 -9223372036854775808/3
   -9223372036854775808/3 -9223372036854775808/3 9223372036854775808 0 0
   (9223372036854775808 0) (9223372036854775808 0) (-4611686018427387904 -1)
   -3074457345618258603 9223372036854775808 27670116110564327424)
  (let ((min -9223372036854775808))
    (list (- min) (abs min) (magnitude min) (/ min) (/ min 3) (/ 3 min) (* min 1/3)
          (* -4611686018427387904 2/3) (+ -4611686018427387904/3 -4611686018427387904/3)
          (quotient min -1) (remainder min -1) (modulo min -1)
          (call-with-values (lambda () (truncate/ min -1)) list)
          (call-with-values (lambda () (floor/ min -1)) list)
          (call-with-values (lambda () (floor/ 9223372036854775807 -2)) list)
          (floor-quotient min 3) (gcd min min) (lcm min 3)))
)

(
  "Fixnum operations with the smallest fixnum"
  (-9223372036854775808 -9223372036854775808 -9223372036854775808 0 0)
  (let ((min -9223372036854775808) (m1 -1))
    (list (fxabs min) (fx/ min -1) (fx/ min m1) (fxremainder min -1) (fxmodulo min -1)))
)

(
  "Division by exact zero"
  (error error error error error +inf.0)
  (map (lambda (thunk) (guard (e (#t 'error)) (thunk)))
       (list (lambda () (/ 123456789012345678901234567890 0)) (lambda () (/ 1/3 0))
             (lambda () (/ 123456789012345678901234567890/7 0)) (lambda () (fxremainder 1 0))
             (lambda () (fxmodulo 1 0)) (lambda () (/ 1/3 0.0))))
)

(
  "Rational arithmetic with large components"
  (1/3 9000000007/21 -1 9223372036854775807/9223372036854775806)
  (list (* 9223372036854775807/2 2/9223372036854775807 1/3) (+ 3000000000/7 1/3)
        (- 9223372036854775806/3 3074457345618258603)
        (/ 9223372036854775807/2 9223372036854775806/2 1/2 2))
)
