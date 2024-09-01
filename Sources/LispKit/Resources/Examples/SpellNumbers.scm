;;; Spell numbers
;;;
;;; This example code implements a procedure `spell-integer` which takes an
;;; integer argument and returns a sentence containing that number spelled
;;; out in words as a string. The algorithm is based on the Racket solution
;;; for the Rosetta Code task at https://rosettacode.org/wiki/Number_names .
;;;
;;; Example usage:
;;;   (spell-integer 4377901047)
;;;   ==> "four billion, three hundred and seventy-seven million,
;;;        nine hundred and one thousand, forty-seven"
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2019 Matthias Zenger. All rights reserved.
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

(import (lispkit base))

(define smalls #("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"
                 "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen"
                 "seventeen" "eighteen" "nineteen"))

(define larges #("hundred" "thousand" "million" "billion" "trillion" "quadrillion"
                 "quintillion" "sextillion" "septillion" "octillion" "nonillion"
                 "decillion" "undecillion" "duodecillion" "tredecillion"
                 "quattuordecillion" "quindecillion" "sexdecillion" "septendecillion"
                 "octodecillion" "novemdecillion" "vigintillion"))

(define tens   #("zero" "ten" "twenty" "thirty" "forty" "fifty" "sixty" "seventy"
                 "eighty" "ninety"))

(define (spell-integer n)
  (let ((step (lambda (div suffix separator spell)
                (let* ((q (quotient n div))
                       (r (remainder n div))
                       (s (if suffix (string-append (spell q) " " suffix) (spell q))))
                  (if (zero? r) s (string-append s separator (spell-integer r)))))))
    (cond ((negative? n)
            (string-append "minus " (spell-integer (- n))))
          ((< n 20)
            (vector-ref smalls n))
          ((< n 100)
            (step 10 #f "-" (lambda (x) (vector-ref tens x))))
          ((< n 1000)
            (step 100 (vector-ref larges 0) " and " spell-integer))
          (else
            (let loop ((m 1000000)
                       (d 1000)
                       (z 1))
              (cond ((>= z (vector-length larges))
                      (error "spell-integer: number too big: $0" n))
                    ((< n m)
                      (step d (vector-ref larges z) ", " spell-integer))
                    (else
                      (loop (* m 1000) (* d 1000) (+ z 1)))))))))
