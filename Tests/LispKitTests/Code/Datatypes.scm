;;; Datatypes.scm
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
  "Binary tree type"
  (1700 3 60)
  (import (lispkit datatype))
  (define-datatype binary-tree
    (leaf val)
    (node left right))
  (define bt1 (leaf 17))
  (define bt2 (node (node (leaf 1) (leaf 2)) (leaf 3)))
  (define bt3 (node (node (leaf 1) (node (leaf 2) (leaf 4))) (leaf 5)))
  (define (match-bt bt)
    (match bt
      ((node (node (leaf x) (leaf y)) _) (+ x y))
      ((node (node (leaf x) _) (leaf y)) (* (+ x y) 10))
      ((leaf x)                          (* x 100))
      (else                              0)))
  (list (match-bt bt1)
        (match-bt bt2)
        (match-bt bt3))
)

(
  "List type"
  (0 2 5 50 1000)
  (define-datatype my-list my-list?
    (nil)
    (chain a b))
  (define ml1 (nil))
  (define ml2 (chain 2 (nil)))
  (define ml3 (chain 2 (chain 3 (nil))))
  (define ml4 (chain 2 (chain 3 (chain 4 (nil)))))
  (define (match-ml ml)
    (match ml
      ((nil)                     0)
      ((chain a (nil))           a)
      ((chain a (chain b (nil))) (+ a b))
      ((chain a (chain b _))     (* (+ a b) 10))
      (else                      1000)))
  (list (match-ml ml1)
        (match-ml ml2)
        (match-ml ml3)
        (match-ml ml4)
        (match-ml bt2))
)

(
  "Datatype predicates"
  (#t #t #f #f #f #f)
  (list (my-list? ml1) (my-list? ml4) (my-list? bt1) (my-list? bt3) (my-list? 12) (my-list? '(a)))
)
