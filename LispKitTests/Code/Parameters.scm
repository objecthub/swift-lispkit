;;; Parameters.scm
;;; Regression test data
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2016 ObjectHub. All rights reserved.
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
  "Global parameter definition"
  1
  (define p (make-parameter 1))
  (p)
)

(
  "Global parameter value modification"
  (1 2 2)
  (list (p) (begin (p 2) (p)) (p))
)

(
  "Global parameter definition with converter"
  101
  (define (inc n) (+ n 1))
  (define q (make-parameter 100 inc))
  (q)
)

(
  "Global parameter value modification with converter"
  (101 201 201)
  (list (q) (begin (q 200) (q)) (q))
)

(
  "Parameterization"
  ((2 201) (10 1001) (2 201))
  (define (foo) (list (p) (q)))
  (list (foo) (parameterize ((p 10)(q 1000)) (foo)) (foo))
)

(
  "Parameterization and local modification"
  ((3 203) ((11 1003) (12 1005)) (4 205))
  (define (bar) (p (+ (p) 1))(q (+ (q) 1))(list (p) (q)))
  (list (bar) (parameterize ((p 10)(q 1000)) (list (bar) (bar))) (bar))
)
