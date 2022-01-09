;;; Compute edit distances between strings
;;;
;;; This is example code implementing the Levenshtein distance between two strings.
;;; A second procedure computes, given a string `s` and a list of strings `xs`, a list of
;;; strings from `xs` with the smallest edit distance to `s`.
;;; 
;;; Examples:
;;;    - (edit-distance "similar" "similar")
;;;    - (edit-distance "similar" "similarly")
;;;    - (edit-distance "same" "sand")
;;;    - (edit-distance "saturday" "sunday")
;;;    - (find-nearest-edits "same" '("sand" "band" "sann"))
;;; 
;;; Some of this code was originally implemented by Alex Shinn.
;;; Copyright © 2019 Alex Shinn. All rights reserved.
;;; BSD-style license: http://synthcode.com/license.txt
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2020 Matthias Zenger. All rights reserved.
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

(import (lispkit base))

;; Computes the Levenshtein distance between two strings. The Levenshtein distance
;; between two words is the minimum number of single-character edits (insertions,
;; deletions or substitutions) required to change one word into the other.
;; This procedure uses a dynamic programming approach and runs in quadratic time and
;; linear memory.

(define (edit-distance s1 s2)
  (let* ((len1 (string-length s1))
         (len2 (string-length s2))
         (vec  (make-vector (fx1+ len1) 0)))
    (do ((i 0 (fx1+ i)))
        ((fx> i len1))
      (vector-set! vec i i))
    (do ((sc2 0 (fx1+ sc2)))
        ((>= sc2 len2) (vector-ref vec len1))
      (vector-set! vec 0 (fx1+ sc2))
      (do ((ch2 (string-ref s2 sc2))
           (sc1 0 (fx1+ sc1))
           (ldiag sc2))
          ((fx>= sc1 len1))
        (let ((odiag (vector-ref vec (fx1+ sc1)))
              (ch1 (string-ref s1 sc1)))
          (vector-set! vec (fx1+ sc1) (min (fx1+ (vector-ref vec (fx1+ sc1)))
                                           (fx1+ (vector-ref vec sc1))
                                           (fx+ ldiag (if (eqv? ch1 ch2) 0 1))))
          (set! ldiag odiag))))))

;; Returns a list of strings in `strls` with the smallest edit distance to `str`,
;; preserving order. If `maxdist` is provided and positive, only return if
;; the edits are less or equal to that distance.

(define (find-nearest-edits str strls . args)
  (let-optionals args ((maxdist (fx1- fx-greatest)))
    (let lp ((ls strls)
             (dist (fx1+ maxdist))
             (res '()))
      (if (null? ls)
          (reverse res)
          (let ((ed (edit-distance str (car ls))))
            (cond ((fx= ed dist)
                    (lp (cdr ls) dist (cons (car ls) res)))
                  ((fx< ed dist)
                    (lp (cdr ls) ed (list (car ls))))
                  (else
                    (lp (cdr ls) dist res))))))))

