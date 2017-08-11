;;; SRFI35.scm
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

(
  "Conditions 1"
  (#t #t #f "V1" "a1")
  (import (srfi 35))
  (define-condition-type &c &condition c? (x c-x))
  (define-condition-type &c1 &c c1? (a c1-a))
  (define-condition-type &c2 &c c2? (b c2-b))
  (define v1 (make-condition &c1 'x "V1" 'a "a1"))
  (list (c? v1) (c1? v1) (c2? v1) (c-x v1) (c1-a v1))
)

(
  "Conditions 2"
  (#t #f #t "V2" "b2")
  (define v2 (condition (&c2 (x "V2") (b "b2"))))
  (list (c? v2) (c1? v2) (c2? v2) (c-x v2) (c2-b v2))
)

(
  "Conditions 3"
  (#t #t #t "V3/1" "a3" "b3")
  (define v3 (condition (&c1 (x "V3/1") (a "a3")) (&c2 (b "b3"))))
  (list (c? v3) (c1? v3) (c2? v3) (c-x v3) (c1-a v3) (c2-b v3))
)

(
  "Conditions 4"
  (#t #t #t "V1" "a1" "b2")
  (define v4 (make-compound-condition v1 v2))
  (list (c? v4) (c1? v4) (c2? v4) (c-x v4) (c1-a v4) (c2-b v4))
)

(
  "Conditions 5"
  (#t #t #t "V2" "a3" "b2")
  (define v5 (make-compound-condition v2 v3))
  (list (c? v5) (c1? v5) (c2? v5) (c-x v5) (c1-a v5) (c2-b v5))
)
