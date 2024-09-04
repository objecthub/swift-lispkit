;;; LISPKIT CONTROL REGRESSION TEST SUITE
;;;
;;; This is the test suite for the procedures `if-let*` and `when-let*`
;;; of library `(lispkit control)`.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2024 Matthias Zenger. All rights reserved.
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

(import (lispkit base)
        (lispkit test))

(test-begin "LispKit Control")

(test-group "Single clause"
  (test 1 (if-let* () 1))
  (test 1 (if-let* () 1 2))
  (test 1 (let ((x #t)) (if-let* (x) (+ 0 1) (+ 1 1))))
  (test 2 (let ((x #f)) (if-let* (x) (+ 0 1) (+ 1 1))))
  (test 1 (let ((x #t)) (if-let* ((x)) (+ 0 1) (+ 1 1))))
  (test 1 (let ((x 10)) (if-let* (((= (+ x 1) 11))) (+ 0 1))))
  (test 2 (let ((x 11)) (if-let* (((= (+ x 1) 11))) (+ 0 1) (+ 1 1))))
  (test 17 (let ((x 10)) (if-let* ((y (+ 6 1))) (+ x y) (- 1 1))))
  (test 0 (let ((x 10)) (if-let* ((y #f)) (+ x y) (- 1 1))))
  (test 20 (let ((x 10)) (if-let* ((y x)) (+ x y))))
)

(test-group "Multiple clauses"
  (test 1 (let ((x #t)(y #f)) (if-let* (x ((not y))) 1 2)))
  (test 1 (let ((x #t)(y #f)) (if-let* ((z (= 1 1)) x ((not y)) z) 1 2)))
  (test 1 (let ((x #t)(y #f)) (if-let* ((z (= 1 1)) x (y #t) ((and y z))) 1 2)))
  (test 2 (let ((x #t)(y #f)) (if-let* ((z (= 1 1)) x (y #t) ((and y z))(r #f)) 1 2)))
  (test 10 (if-let* ((a (+ 1 2))(b (+ a 3))(c (+ b 4))) c 0))
  (test 0 (if-let* ((a (+ 1 2))(b #f)(c (+ b 4))) c 0))
  (test 3 (if-let* ((a 1)(b (if-let* ((c 2)(d (+ c 1))) d 0))) b -1))
)

(test-group "When-if*"
  (test 2 (let ((x #t)(y #f)) (when-let* ((z (= 1 1)) x (y #t) ((and y z))) 1 2)))
  (test 4 (when-let* ((a 1)(b (if-let* ((c 2)(d (+ c 1))) d 0))) b (+ b 1)))
)

(test-end)
