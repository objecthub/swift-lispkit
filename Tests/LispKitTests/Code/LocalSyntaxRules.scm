;;; LocalSyntaxRules.scm
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
  "Local when"
  now
  (let-syntax
    ((when (syntax-rules ()
             ((when test stmt1 stmt2 ...) (if test (begin stmt1 stmt2 ...))))))
    (let ((if #t)) (when if (set! if 'now)) if))
)

(
  "Outer"
  outer
  (let
    ((x 'outer))
    (let-syntax
      ((m (syntax-rules () ((m) x))))
      (let ((x 'inner)) (m))))
)

(
  "Local my-or"
  7
  (letrec-syntax
    ((my-or
       (syntax-rules ()
         ((my-or) #f)
         ((my-or e) e)
         ((my-or e1 e2 ...) (let ((temp e1)) (if temp temp (my-or e2 ...)))))))
   (let ((x #f)
         (y 7)
         (temp 8)
         (let odd?)
         (if even?))
     (my-or x (let temp) (if y) y)))
)

(
  "Local vector match"
  bar
  (letrec-syntax
    ((letv
       (syntax-rules ()
         ((_ #((var val) ...) exp exp* ...) (let ((var val) ...) exp exp* ...)))))
    (letv #((foo 'bar)) foo))
)
