;;; Default Prelude for LispKit
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

(define-syntax for
  (syntax-rules (in from)
    ((for element in list body ...) (map (lambda (element) body ...) list))
    ((for element from (x ...) body ...) (map (lambda (element) body ...) (list x ...)))))

(define-syntax while
  (syntax-rules ()
    ((while condition body ...)
       (let loop ()
         (if condition (begin body ... (loop)) #f)))))

(define random
  (let ((a 69069)
        (c 1)
        (m (expt 2 32))
        (seed 19380110))
    (lambda new-seed
      (if (pair? new-seed)
          (set! seed (car new-seed))
          (set! seed (modulo (+ (* seed a) c) m)))
      (inexact (/ seed m)))))

(define random-integer
  (case-lambda
    ((hi)
      (exact (floor (* (random) hi))))
    ((lo hi)
      (+ lo (exact (floor (* (random) (- hi lo))))))))
