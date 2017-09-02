;;; Approximate Pi
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2017 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
;;; except in compliance with the License. You may obtain a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed under the
;;; License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
;;; either express or implied. See the License for the specific language governing permissions
;;; and limitations under the License.


;;; Returns a string representation of pi with `n` decimal digits
(define (pi-as-string n)
  (pi->string (approx-pi n)))

;;; Returns a list of `n` decimal digits of pi
(define (approx-pi n)
  (let ((u 0) (y 0) (j (+ n 2)))
    (do ((q 1   (* 10 q i (- (* 2 i) 1)))
         (r 180 (* 10 (- (+ (* q (- (* 5 i) 2)) r) (* y t)) u))
         (t 60  (* t u))
         (i 2   (+ i 1))
         (res '() (cons y res)))
        ((> i j) (reverse res))
      (set! u (* 3 (+ (* 3 i) 1) (+ (* 3 i) 2)))
      (set! y (floor-quotient (+ (* q (- (* 27 i) 12)) (* 5 r)) (* 5 t))))))

;;; Converts a list of decimal digits into a string representation of pi
(define (pi->string xs)
  (let ((out (open-output-string)))
    (write-string "3." out)
    (for-each (lambda (x) (write x out)) (cdr xs))
    (get-output-string out)))
