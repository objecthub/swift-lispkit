;;; SRFI 272 SHOW REGRESSION TEST SUITE
;;;
;;; This is the test suite for library `(srfi 272 show)`.
;;;
;;; The expected results of the "golden" tests were computed with the SRFI 272
;;; sample implementation running on Chibi Scheme, adapted to the LispKit semantics
;;; of pairs (pairs are values without identity and are thus never labeled).
;;;
;;; Copyright © 2026 Sergei Egorov. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;; this software and associated documentation files (the "Software"), to deal in the
;;; Software without restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so, subject to the
;;; following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2026 Matthias Zenger. All rights reserved.

(import (lispkit base)
        (lispkit test)
        (srfi 272 show)
        (srfi 166))

(load "Tests/SRFI-272-Support")

(test-begin "SRFI 272: Pretty printing with SRFI 166")

(test-group "Golden tests"

(define func
  '(define (fold kons knil ls)
     (let lp ((ls ls) (acc knil))
       (if (null? ls) acc (lp (cdr ls) (kons (car ls) acc))))))
(define doc
  (string-append
    "The fundamental list iterator.  Applies KONS to each "
    "element of LS and the result of the previous application, "
    "beginning with KNIL.  With KONS as CONS and KNIL as '(), "
    "equivalent to REVERSE."))
(test-equal "pprinted"
  "(define (fold kons knil ls)\n  (let lp ((ls ls) (acc knil))\n    (if (null? ls) acc (lp (cdr ls) (kons (car ls) acc)))))\n"
  (show #f (pprinted func)))
(test-equal "pprinted with width"
  "(define (fold kons knil ls)\n  (let lp ((ls ls) (acc knil))\n    (if (null? ls)\n        acc\n        (lp (cdr ls)\n            (kons (car ls) acc)))))\n"
  (show #f (with ((width 30)) (pprinted func))))
(test-equal "pprinted, width overrides pp-width"
  "(define (fold kons knil ls)\n  (let lp ((ls ls) (acc knil))\n    (if (null? ls)\n        acc\n        (lp (cdr ls)\n            (kons (car ls) acc)))))\n"
  (show #f (with ((width 30)) (pprinted func pp-width 100))))
(test-equal "pprinted with keywords"
  "(define-syntax set*!\n  (lambda (x)\n    (letrec\n      ([unwrap-exp\n        (lambda (x)\n          (let ([x (unwrap-syntax x)])\n            (if (pair? x) (cons (car x) (unwrap-exp (cdr x))) ...)))])\n      (let ([sets (map unwrap-exp (cdr (unwrap-exp x)))])\n        (let\n          ([ids (map car sets)]\n           [vals (map cadr sets)]\n           [temps (map (lambda (x) (generate-identifier)) sets)])\n          `(,'let\n            ,(map list temps ...)\n            ,@(map (lambda (id temp) `(,'set! ,id ,temp)) ids ...)\n            ...))))))\n"
  (show #f (pprinted sexp4 pp-length 3 pp-brackets #t)))
(test-equal "pprinted*"
  "(define-syntax set*! (lambda (x) (letrec (...) (...))))\n"
  (show #f (pprinted* sexp4 pp-length 3 (list pp-level 3))))
(test-equal "pprinted in sequence"
  "value: (a b c)\nend"
  (show #f "value: " (pprinted '(a b c)) "end"))
(test-equal "pprinted, columnar"
  "(define (fold kons knil ls)           ; The fundamental list iterator.\n  (let lp ([ls ls] [acc knil])        ; Applies KONS to each element of LS\n    (if (null? ls)                    ; and the result of the previous\n        acc                           ; application, beginning with KNIL.\n        (lp (cdr ls)                  ; \n            (kons (car ls) acc)))))   ; \n"
  (show #f (columnar 37 (pprinted func pp-brackets #t) " ; "
               (joined displayed
                       '("The fundamental list iterator."
                         "Applies KONS to each element of LS"
                         "and the result of the previous"
                         "application, beginning with KNIL.")
                       nl))))
(test-equal "pprinted, no newline"
  "[(a b c)]"
  (show #f "[" (pprinted '(a b c) pp-newline #f) "]")))

(test-end)
