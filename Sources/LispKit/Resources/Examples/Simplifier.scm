;;; Symbolic Algebraic Simplification
;;; 
;;; The code in the subdirectory `paip` implements the pattern matcher and the
;;; algebraic simplifier described in chapter 6.2 and 8 of Peter Norvig's book
;;; "Paradigms of Artificial Intelligence Programming: Case Studies in Common
;;; Lisp" (1992). It was ported from the original Common Lisp code which has
;;; been published under an MIT license at https://github.com/norvig/paip-lisp .
;;;
;;; This program is simply loading the libraries (from a custom location) and
;;; executing a few test cases for procedures `infix->prefix` and `simp`.
;;; More details about the code and its usage can be found at:
;;; https://github.com/norvig/paip-lisp/blob/main/docs/chapter6.md and
;;; https://github.com/norvig/paip-lisp/blob/main/docs/chapter8.md .
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2021 Matthias Zenger. All rights reserved.
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

;; Make LispKit aware where the custom libraries are located
(load (path (source-directory) "paip/util.sld"))
(load (path (source-directory) "paip/patmatch.sld"))
(load (path (source-directory) "paip/simplifier.sld"))

;; Import the symbolic simplifier
(import (paip simplifier))

(define-syntax display-trans
  (syntax-rules ()
    ((_ label f expr)
      (begin
        (display label)
        (display " ")
        (display (quote expr))
        (newline)
        (display "   ⟹ ")
        (display (f (quote expr)))
        (newline)))))

(display-trans "[PRE]" infix->prefix
  (1 + 2 * sin (1 / x)))
(display-trans "[PRE]" infix->prefix
  ((x + 1) ^ 2 * 3))
(display-trans "[PRE]" infix->prefix
  (Int 3 * (x + 2) d x))
(display-trans "[SIM]" simp
  (1 + 2 + x + 3 + 4))
(display-trans "[SIM]" simp
  (4 * (9 / x) * x - 10))
(display-trans "[SIM]" simp
  (9 - 3 * (x + x) * x))
(display-trans "[SIM]" simp
  (d ((cos x) / x) / d x))
(display-trans "[SIM]" simp
  (d ((a * x ^ 2 + b * x + c) / x) / d x))
(display-trans "[SIM]" simp
  (Int x * sin(x ^ 2) d x))
(display-trans "[SIM]" simp
  (Int (3 * x + 2) ^ -2/3 d x))
(display-trans "[SIM]" simp
  (Int sin(x) / (1 + cos(x)) d x))
(display-trans "[SIM]" simp
  (Int 8 * x ^ 2 / (x ^ 3 + 2) ^ 3 d x))
