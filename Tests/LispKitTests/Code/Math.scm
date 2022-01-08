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
