;;; MathUtil.scm
;;; Regression test data
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
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
  "Hyperbolic functions"
  ((0 1 0 0 0)
   #t
   #t
   #t
   #t
   (1.0 -1.0))
  (import (lispkit math util))
  (define (close? x y) (< (abs (- x y)) 1e-12))
  (list (list (sinh 0) (cosh 0) (tanh 0) (asinh 0) (atanh 0))
        (close? (sinh 1.0) 1.1752011936438014)
        (close? (cosh 1.0) 1.5430806348152437)
        (close? (tanh 1.0) 0.7615941559557649)
        (and (close? (asinh (sinh 0.7)) 0.7)
             (close? (acosh (cosh 1.3)) 1.3)
             (close? (atanh (tanh 0.4)) 0.4))
        (list (tanh 100) (tanh -100)))
)
