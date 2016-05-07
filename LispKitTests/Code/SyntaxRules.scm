;;; SyntaxRules.scm
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
  "Define syntax `for`"
  for
  (define-syntax for
    (syntax-rules (in from)
      ((for element in list body ...) (map (lambda (element) body ...) list))
      ((for element from (x ...) body ...) (map (lambda (element) body ...) (list x ...)))))
)

(
  "For ... in"
  (1 2 3)
  (for x in '(1 2 3) x)
)

(
  "For ... from"
  (1 2 3)
  (for x from (1 2 3) x)
)

(
  "Define syntax `while`"
  while
  (define-syntax while
    (syntax-rules ()
      ((while condition body ...) (let loop () (if condition (begin body ... (loop)) #f)))))
)

(
  "Sum of 1..5 with `while`"
  15
  (define x 0)
  (define y 0)
  (while (< x 5) (set! x (+ x 1)) (set! y (+ y x)))
  y
)

(
  "Define syntax `myor`"
  myor
  (define-syntax myor
    (syntax-rules ()
      ((myor) #f)
      ((myor e) e)
      ((myor e1 e2 ...) (let ((temp e1)) (if temp temp (myor e2 ...))))))
)

(
  "Instantiate myor 1"
  #f
  (myor #f #f #f #f)
)

(
  "Instantiate myor 2"
  #f
  (myor #f)
)

(
  "Instantiate myor 3"
  #t
  (myor #f (equal? 0 1) #t #f)
)

(
  "Instantiate myor 4"
  #t
  (myor (equal? 1 1) #f)
)

(
  "Cond1"
  100
  (define-syntax cond1
    (syntax-rules (=> else)
      ((cond1 test => fun)
       (let ((exp test))
         (if exp (fun exp) #f)))
      ((cond1 test exp exp* ...)
       (if test (begin exp exp* ...)))
      ((cond1 else exp exp* ...)
       (begin exp exp* ...))))
  (define (square x) (* x x))
  (cond1 10 => square)
)
