;;; CallCC.scm
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
  "Call/cc in tail call"
  20
  (define cont 0)
  (define (foo) (call-with-current-continuation (lambda (c) (set! cont c) 1)))
  (foo)
  (+ (cont 20) (cont 300))
)

(
  "Call/cc in regular call"
  1020
  (define cont 0)
  (define (foo) (+ (call-with-current-continuation (lambda (c) (set! cont c) 1)) 1000))
  (foo)
  (+ (cont 20) (cont 300))
)

(
  "Call/cc with function"
  27
  (define cont 0)
  (define (foo)
    (+ ((call-with-current-continuation (lambda (c)
                                          (set! cont c)
                                          (lambda (x) x))) 1) 20))
  (foo)
  (cont (lambda (x) 7))
)

(
  "Escaping continuation"
  (120 -1 2)
  (define (mult l)
    (call-with-current-continuation
      (lambda (exit) (let iter ((l l))
                       (if (null? l)
                           1
                           (if (= (car l) 0) (exit -1) (* (car l) (iter (cdr l)))))))))
  (list (mult '(2 3 4 5)) (mult '(2 3 4 0 5 4 3 2 1)) (mult '(2)))
)
