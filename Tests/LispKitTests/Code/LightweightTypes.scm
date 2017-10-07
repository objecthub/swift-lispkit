;;; LightweightTypes.scm
;;; Regression test data
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2017 ObjectHub. All rights reserved.
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

;; The following tests are derived from SRFI-137
;; See http://srfi.schemers.org/srfi-137/srfi-137.html
;;
;; Copyright © 2016 John Cowan, Marc Nieper-Wißkirchen (2003). All Rights Reserved.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;; software and associated documentation files (the "Software"), to deal in the Software
;; without restriction, including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
;; to whom the Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all copies or
;; substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
;; PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
;; FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
;; OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

(
  "make-type disjoint procedures"
  #f
  (let-values (((make-reia1 . reia1*) (make-type 'reia))
               ((make-reia2 . reia2*) (make-type 'reia)))
    (eq? make-reia1 make-reia2))
)

(
  "make-type disjoint types"
  (#f #t)
  (let-values (((make-reia1 . reia1*) (make-type 'reia))
               ((make-reia2 . reia2*) (make-type 'reia)))
    (list (equal? (make-reia1 1) (make-reia2 1))
          (equal? (make-reia1 '(one two)) (make-reia1 '(one two)))))
)

(
  "make-type predicates and subtypes"
  (#t #t #f #f #t)
  (let*-values
    (((make-reia reia? reia-ref make-reia-subtype) (make-type 'reia))
     ((make-daughter daughter? daughter-ref make-daughter-subtype) (make-reia-subtype 'daughter))
     ((make-son son? son-ref make-son-subtype) (make-reia-subtype 'son))
     ((make-grand-daughter grand-daughter? grand-daughter-ref make-grand-daughter-subtype)
       (make-daughter-subtype 'grand-daughter)))
    (list (reia? (make-reia #f))
          (reia? (make-daughter #f))
          (daughter? (make-reia #f))
          (son? (make-daughter #f))
          (reia? (make-grand-daughter #f))))
)

(
  "make-type instance payload"
  payload
  (let-values (((make-reia reia? reia-ref . rest) (make-type 'reia)))
    (reia-ref (make-reia 'payload)))
)
