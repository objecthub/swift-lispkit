;;; SRFI 272 INTERMEDIATE REGRESSION TEST SUITE
;;;
;;; This is the test suite for library `(srfi 272 intermediate)`.
;;;
;;; The first part of this test suite is based on the test suite of the SRFI 272
;;; sample implementation. Inputs with datum labels are read via `read-labeled`
;;; (see "SRFI-272-Support.scm"); tests whose inputs contain circular lists are
;;; skipped since such lists cannot be represented in LispKit.
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
        (srfi 272 intermediate)
        (lispkit gvector))

(load "Tests/SRFI-272-Support")

(define (pp-test llen input expected)
  (let ((obj (read-labeled input)))
    (if (eq? obj unrepresentable)
        (srfi-272-skip!)
        (test-equal input expected (pp->string obj pp-width llen)))))

(define (test-cut level length input expected)
  (let ((obj (read-labeled input)))
    (if (eq? obj unrepresentable)
        (srfi-272-skip!)
        (test-equal (string-append input " [level " (write-to-string level)
                                   ", length " (write-to-string length) "]")
                    expected
                    (parameterize ((pp-level level) (pp-length length))
                      (pp->string obj))))))

(test-begin "SRFI 272: Intermediate pretty printing")

(test-group "Tests of the sample implementation"

(pp-test 40 "1" "1\n")
(pp-test 40 "'(a b)" "'(a b)\n")
(pp-test 40 "'(a . b)" "'(a . b)\n")
(pp-test 40 "`(,a ,@b)" "`(,a ,@b)\n")
(pp-test 80
  "(let ((x 1) (y 2) (zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz 3)) (display x) (display y))"
  "(let\n  ((x 1)\n   (y 2)\n   (zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz 3))\n  (display x)\n  (display y))\n")

; graph cycles tests (default mode)
(pp-test 40 "#0=(a . #0#)" "#0=(a . #0#)\n")
(pp-test 40 "(1 . #0=(2 . #0#))" "(1 . #0=(2 . #0#))\n")
(pp-test 40 "#0=(1 #0# 3)" "#0=(1 #0# 3)\n")
(pp-test 40 "(#0=(1 #0# 3) #0#)" "(#0=(1 #0# 3) #0#)\n")
(pp-test 40 "(#0=(1 . #0#) #1=(1 . #1#))"
  "(#0=(1 . #0#) #1=(1 . #1#))\n")
(pp-test 40 "(#0=(a b . #0#) '#1=(a b a b . #1#))"
  "(#0=(a b . #0#) '#1=(a b a b . #1#))\n")
(pp-test 40 "(#0=(1 . 2) #1=(1 . 2) #2=(3 . 4) #0# #1# #2#)"
  "((1 . 2) (1 . 2) (3 . 4) (1 . 2) (1 . 2)\n  (3 . 4))\n")
(pp-test 40 "#0=((1 . 2) (1 . 2) (3 . 4) . #0#)"
  "#0=((1 . 2) (1 . 2) (3 . 4) . #0#)\n")
(pp-test 40 "#0=#(#0#)" "#0=#(#0#)\n")
(pp-test 40 "#0=#(1 #0#)" "#0=#(1 #0#)\n")
(pp-test 40 "#0=#(1 #0# 3)" "#0=#(1 #0# 3)\n")
(pp-test 40 "(#0=#(1 #0# 3))" "(#0=#(1 #0# 3))\n")
(pp-test 40 "#0=#(#0# 2 #0#)" "#0=#(#0# 2 #0#)\n")
(pp-test 100 "#0=(a . #0#)" "#0=(a . #0#)\n")
(pp-test 100 "(#0=(a . #0#) #0#)" "(#0=(a . #0#) #0#)\n")
(pp-test 100 "#0=#(#0#)" "#0=#(#0#)\n")
(pp-test 100 "(#0=(a b) #0#)" "((a b) (a b))\n")
(pp-test 100 "#0=(#1=(a) #1# . #0#)" "#0=((a) (a) . #0#)\n")
(pp-test 100 "(#0=(a . #0#) #1=(b c) #1#)"
  "(#0=(a . #0#) (b c) (b c))\n")
(pp-test 100 "#0=(a . (#1=(b) . #0#))" "#0=(a (b) . #0#)\n")
(pp-test 100 "#0=(#(#1=(a b) #1#) . #0#)"
  "#0=(#((a b) (a b)) . #0#)\n")
(pp-test 100 "(#0=(a . #0#) #1=#(#1#))"
  "(#0=(a . #0#) #1=#(#1#))\n")
(pp-test 100 "#0=(#1=(a . #0#) #1#)"
  "#0=((a . #0#) (a . #0#))\n")
(pp-test 100 "(1 2 3)" "(1 2 3)\n")
(pp-test 100 "(#0=(a) #0#)" "((a) (a))\n")
(pp-test 100 "#0=(a . #0#)" "#0=(a . #0#)\n")
(pp-test 100 "(#0=(a) #1=(b . #1#) #0#)"
  "((a) #0=(b . #0#) (a))\n")
(pp-test 100 "#(#0=#(1) #0#)" "#(#(1) #(1))\n")
(pp-test 100 "#0=#(#0#)" "#0=#(#0#)\n")
(pp-test 100 "(#0=(1 . #0#) #1=(2 . #1#))"
  "(#0=(1 . #0#) #1=(2 . #1#))\n")
(pp-test 100 "#0=(#1=(a . #0#) . #1#)"
  "#0=((a . #0#) a . #0#)\n")
(pp-test 100 "(#0=(a . #0#) #0#)" "(#0=(a . #0#) #0#)\n")
(pp-test 100 "#((#0=(a) #1=(b) #0#) #1#)"
  "#(((a) (b) (a)) (b))\n")

; shared graph tests
(parameterize ((pp-graph #t))
  (pp-test 40 "#0=(a . #0#)" "#0=(a . #0#)\n")
  (pp-test 40 "(1 . #0=(2 . #0#))" "(1 . #0=(2 . #0#))\n")
  (pp-test 40 "#0=(1 #0# 3)" "#0=(1 #0# 3)\n")
  (pp-test 40 "(#0=(1 #0# 3) #0#)" "(#0=(1 #0# 3) #0#)\n")
  (pp-test 40 "(#0=(1 . #0#) #1=(1 . #1#))"
    "(#0=(1 . #0#) #1=(1 . #1#))\n")
  (pp-test 40 "(#0=(a b . #0#) '#1=(a b a b . #1#))"
    "(#0=(a b . #0#) '#1=(a b a b . #1#))\n")
  ; LispKit: pairs are values without identity and are never labeled
  (pp-test 40 "(#0=(1 . 2) #1=(1 . 2) #2=(3 . 4) #0# #1# #2#)"
    "((1 . 2) (1 . 2) (3 . 4) (1 . 2) (1 . 2)\n  (3 . 4))\n")
  (pp-test 40 "#0=((1 . 2) (1 . 2) (3 . 4) . #0#)"
    "#0=((1 . 2) (1 . 2) (3 . 4) . #0#)\n")
  (pp-test 40 "#0=#(#0#)" "#0=#(#0#)\n")
  (pp-test 40 "#0=#(1 #0#)" "#0=#(1 #0#)\n")
  (pp-test 40 "#0=#(1 #0# 3)" "#0=#(1 #0# 3)\n")
  (pp-test 40 "(#0=#(1 #0# 3))" "(#0=#(1 #0# 3))\n")
  (pp-test 40 "#0=#(#0# 2 #0#)" "#0=#(#0# 2 #0#)\n")
  (pp-test 100 "#0=(a . #0#)" "#0=(a . #0#)\n")
  (pp-test 100 "(#0=(a . #0#) #0#)" "(#0=(a . #0#) #0#)\n")
  (pp-test 100 "#0=#(#0#)" "#0=#(#0#)\n")
  ; LispKit: pairs are values without identity and are never labeled
  (pp-test 100 "(#0=(a b) #0#)" "((a b) (a b))\n")
  (pp-test 100 "#0=(#1=(a) #1# . #0#)"
    "#0=(#1=(a) #1# . #0#)\n")
  (pp-test 100 "(#0=(a . #0#) #1=(b c) #1#)"
    "(#0=(a . #0#) #1=(b c) #1#)\n")
  (pp-test 100 "#0=(a . (#1=(b) . #0#))" "#0=(a (b) . #0#)\n")
  (pp-test 100 "#0=(#(#1=(a b) #1#) . #0#)"
    "#0=(#(#1=(a b) #1#) . #0#)\n")
  (pp-test 100 "(#0=(a . #0#) #1=#(#1#))"
    "(#0=(a . #0#) #1=#(#1#))\n")
  (pp-test 100 "#0=(#1=(a . #0#) #1#)"
    "#0=(#1=(a . #0#) #1#)\n")
  (pp-test 100 "(1 2 3)" "(1 2 3)\n")
  ; LispKit: pairs are values without identity and are never labeled
  (pp-test 100 "(#0=(a) #0#)" "((a) (a))\n")
  (pp-test 100 "#0=(a . #0#)" "#0=(a . #0#)\n")
  (pp-test 100 "(#0=(a) #1=(b . #1#) #0#)"
    "(#0=(a) #1=(b . #1#) #0#)\n")
  (pp-test 100 "#(#0=#(1) #0#)" "#(#0=#(1) #0#)\n")
  (pp-test 100 "#0=#(#0#)" "#0=#(#0#)\n")
  (pp-test 100 "(#0=(1 . #0#) #1=(2 . #1#))"
    "(#0=(1 . #0#) #1=(2 . #1#))\n")
  (pp-test 100 "#0=(#1=(a . #0#) . #1#)"
    "#0=(#1=(a . #0#) . #1#)\n")
  (pp-test 100 "(#0=(a . #0#) #0#)" "(#0=(a . #0#) #0#)\n")
  ; LispKit: pairs are values without identity and are never labeled
  (pp-test 100 "#((#0=(a) #1=(b) #0#) #1#)"
    "#(((a) (b) (a)) (b))\n"))

; limits tests

(test-cut 0 10 "a" "a\n")
(test-cut 0 10 "(a b c)" "(...)\n")
(test-cut 0 10 "#(a b c)" "#(...)\n")
(test-cut 1 10 "(a b c)" "(a b c)\n")
(test-cut 1 10 "((a b) c)" "((...) c)\n")
(test-cut 1 10 "#((a b) c)" "#((...) c)\n")
(test-cut 1 2 "(a b c d)" "(a b ...)\n")
(test-cut 1 2 "#(a b c d)" "#(a b ...)\n")
(test-cut 1 2 "(a b . c)" "(a b . c)\n")
(test-cut 2 10 "((a b) (c d))" "((a b) (c d))\n")
(test-cut 2 10 "(((a)) b)" "(((...)) b)\n")
(test-cut 2 10 "#(#(a b) #(c #(d)))"
  "#(#(a b) #(c #(...)))\n")
(test-cut 2 2 "((a b c) (d e f) (g h i))"
  "((a b ...) (d e ...) ...)\n")
(test-cut 2 2 "#((a b c) (d e f) (g h i))"
  "#((a b ...) (d e ...) ...)\n")
(test-cut 2 3 "(a (b c d e) f g)" "(a (b c d ...) f ...)\n")
(test-cut 3 10 "(a b c d . e)" "(a b c d . e)\n")
(test-cut 3 10 "#(a (b . c) #(d e (f g)))"
  "#(a (b . c) #(d e (f g)))\n")
(test-cut 2 10 "#(a (b . c) #(d e (f g)))"
  "#(a (b . c) #(d e (...)))\n")
(test-cut 1 10 "(a . b)" "(a . b)\n")
(test-cut 1 1 "(a . b)" "(a . b)\n")
(test-cut 2 10 "((a . b) . c)" "((a . b) . c)\n")
(test-cut 2 1 "((a . b) . c)" "((a . b) . c)\n")
(test-cut 3 2 "(a b c . d)" "(a b ...)\n")
(test-cut 0 #f "(a b c)" "(...)\n")

(test-cut 1 #f "(a (b c) d)" "(a (...) d)\n")
(test-cut 2 #f "(a (b (c)) d)" "(a (b (...)) d)\n")
(test-cut 0 #f "#(1 2 3)" "#(...)\n")
(test-cut 1 #f "#(1 #(2) 3)" "#(1 #(...) 3)\n")
(test-cut 1 #f "(a b . #(c))" "(a b . #(...))\n")

(test-cut #f 0 "(a b c)" "(...)\n")
(test-cut #f 1 "(a b c)" "(a ...)\n")
(test-cut #f 3 "(a b c)" "(a b c)\n")
(test-cut #f 0 "#(1 2 3)" "#(...)\n")
(test-cut #f 2 "#(1 2 3 4)" "#(1 2 ...)\n")
(test-cut #f 1 "(a b . c)" "(a ...)\n")
(test-cut #f 2 "(a b . c)" "(a b . c)\n")
(test-cut #f 2 "(a b c . d)" "(a b ...)\n")
(test-cut #f 1 "((a b) (c d))" "((a ...) ...)\n")

(test-cut 1 1 "((a b) (c d))" "((...) ...)\n")
(test-cut 2 1 "(((a) b) c)" "(((...) ...) ...)\n")
(test-cut 0 0 "(a b c)" "(...)\n")
(test-cut 1 2 "((a b c) (d e f) (g h i))"
  "((...) (...) ...)\n")
(test-cut 1 2 "#(#(a b c) #(d e f) #(g h i))"
  "#(#(...) #(...) ...)\n")
(test-cut 1 #f "(a b . #(c d))" "(a b . #(...))\n")
(test-cut 1 #f "((a . b) . c)" "((...) . c)\n")


(parameterize ((pp-graph #t))
  (test-cut 0 #f "#0=(a . #0#)" "(...)\n")
  (test-cut 1 #f "#0=(a . #0#)" "#0=(a . #0#)\n")
  (test-cut 1 1 "#0=(a b c . #0#)" "(a ...)\n")
  ; LispKit: pairs are values without identity and are never labeled
  (test-cut 2 2 "(#0=(a b) #0# #0#)" "((a b) (a b) ...)\n")
  (test-cut 1 2 "(#0=(a b) #0# #0#)" "((...) (...) ...)\n")
  (test-cut 2 #f "#0=(#(a #0#) b)" "(#(a (...)) b)\n")
  (test-cut 1 #f "#0=(#(a #0#) b)" "(#(...) b)\n")
  (test-cut 2 1 "#0=(#(a #0#) b)" "(#(a ...) ...)\n")
  ; LispKit: pairs are values without identity and are never labeled
  (test-cut 2 #f "(#0=(a . b) #0# . #0#)"
    "((a . b) (a . b) a . b)\n")
  (test-cut 1 #f "(#0=(a . b) #0# . #0#)"
    "((...) (...) a . b)\n")
  (test-cut 3 2 "#0=((a . b) (#0# . c) d)"
    "#0=((a . b) (#0# . c) ...)\n")
  (test-cut 1 1 "#0=((a . b) (#0# . c) d)" "((...) ...)\n")
  (test-cut 2 #f "#0=(#(#0#) . #0#)" "#0=(#(#0#) . #0#)\n")
  (test-cut 1 #f "#0=(#(#0#) . #0#)" "#0=(#(...) . #0#)\n")
  ; this is what Chez does:
  ;(test-cut 2 2 "#0=(#1=(a b) #1# . #0#)" "#0=(#1=(a b) #1# . #0#)\n")
  ; I believe we're entitled to this:
  (test-cut 2 2 "#0=(#1=(a b) #1# . #0#)"
    "(#0=(a b) #0# ...)\n")
  ; this is what Chez does:
  ;(test-cut 1 2 "#0=(#1=(a b) #1# . #0#)" "#0=((...) (...) . #0#)\n")
  ; I believe we're entitled to this:
  (test-cut 1 2 "#0=(#1=(a b) #1# . #0#)" "((...) (...) ...)\n")
  (test-cut 2 #f "#0=(a . (#1=#(b #0#) . #1#))"
    "(a #0=#(b (...)) . #0#)\n")
  (test-cut 1 #f "#0=(a . (#1=#(b #0#) . #1#))"
    "(a #(...) . #(...))\n")
  (test-cut 2 1 "#0=(a . (#1=#(b #0#) . #1#))" "(a ...)\n")
  (test-cut 3 3 "(#0=(a . #0#) #1=#(#1#) #2=(b c) #2#)"
    "(#0=(a . #0#) #1=#(#1#) (b c) ...)\n")
  (test-cut 0 #f "#0=(a . #0#)" "(...)\n")
  (test-cut 1 #f "#0=(a . #0#)" "#0=(a . #0#)\n")
  (test-cut 1 #f "(#0=(a b) #0#)" "((...) (...))\n")
  (test-cut #f 1 "(#0=(a b c) #0#)" "((a ...) ...)\n")
  (test-cut 0 #f "#0=#(#0#)" "#(...)\n")
  (test-cut 1 #f "#0=#(#0#)" "#(#(...))\n")
  (test-cut 1 #f "(#0=(a) #1=(#0#) #1#)"
    "((...) (...) (...))\n")
  (test-cut #f 1 "(a #0=(b) #0#)" "(a ...)\n")
  (test-cut #f 2 "(a #0=(b) c #0#)" "(a (b) ...)\n")
  (test-cut 2 #f "#0=(a #1=(b . #0#) . #1#)"
    "#0=(a #1=(b . #0#) . #1#)\n")
  (test-cut 1 #f "(#0=(a . b) #0#)" "((...) (...))\n")
  (test-cut #f 1 "#0=(a b . #0#)" "(a ...)\n")
  ; this is what Chez does:
  ;(test-cut #f 2 "#0=(a b . #0#)" "#0=(a b . #0#)\n")
  ; I believe we're entitled to this:
  (test-cut #f 2 "#0=(a b . #0#)" "(a b ...)\n")
  (test-cut 0 #f "(#0=() #0#)" "(...)\n")
  (test-cut 1 #f "(#0=() #0#)" "(() ())\n")
  (test-cut 1 1 "#(#0=#(a b c) #0#)" "#(#(...) ...)\n")
  (test-cut #f 1 "#(#0=#(a b c) #0#)" "#(#(a ...) ...)\n")
  (test-cut 1 2 "(#0=(a) b c . #0#)" "((...) b ...)\n")
  (test-cut 1 1 "((#0=(a) #0#) (#1=(b) #1#))" "((...) ...)\n")
  (test-cut 1 #f "#0=#(#1=(a . #0#) #1#)" "#((...) (...))\n")
  (test-cut 0 0
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))"
    "(...)\n")
  (test-cut 0 1
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))"
    "(...)\n")
  (test-cut 0 2
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))"
    "(...)\n")
  (test-cut 0 3
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))"
    "(...)\n")
  (test-cut 1 0
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))"
    "(...)\n")
  (test-cut 1 1
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))"
    "(a ...)\n")
  (test-cut 1 2
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))"
    "(a (...) ...)\n")
  (test-cut 1 3
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))"
    "(a (...) (...) ...)\n")
  (test-cut 2 0
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))"
    "(...)\n")
  (test-cut 2 1
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))"
    "(a ...)\n")
  (test-cut 2 2
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))"
    "(a (b . #(...)) ...)\n")
  (test-cut 2 3
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))"
    "(a (b . #(...)) (#(...) . d) ...)\n")
  (test-cut 3 0
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))"
    "(...)\n")
  (test-cut 3 1
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))"
    "(a ...)\n")
  (test-cut 3 2
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))"
    "(a #0=(b . #(#0# c)) ...)\n")
  (test-cut 3 3
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))"
    "(a #0=(b . #1=#(#0# c)) (#1# . d) ...)\n")
  (test-cut 0 0 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)"
    "(...)\n")
  (test-cut 0 1 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)"
    "(...)\n")
  (test-cut 0 2 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)"
    "(...)\n")
  (test-cut 0 3 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)"
    "(...)\n")
  (test-cut 1 0 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)"
    "(...)\n")
  (test-cut 1 1 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)"
    "((...) ...)\n")
  (test-cut 1 2 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)"
    "((...) #(...) ...)\n")
  (test-cut 1 3 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)"
    "((...) #(...) (...) ...)\n")
  (test-cut 2 0 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)"
    "(...)\n")
  (test-cut 2 1 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)"
    "((a ...) ...)\n")
  (test-cut 2 2 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)"
    "((a (...)) #(c d ...) ...)\n")
  (test-cut 2 3 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)"
    "#0=((a (...)) #(c d (...)) (e . #0#) ...)\n")
  (test-cut 3 0 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)"
    "(...)\n")
  (test-cut 3 1 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)"
    "((a ...) ...)\n")
  (test-cut 3 2 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)"
    "((a (b)) #(c d ...) ...)\n")
  (test-cut 3 3 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)"
    "#0=(#1=(a (b)) #(c d #1#) (e . #0#) ...)\n"))

; for histerical reasons, read macros don't increment level
(test-cut 3 4 "'`,,@(3 . #(a b c d e f g))))"
  "'`,,@(3 . #(a b c d ...))\n")
; NB: Chez 9.5.4 hangs on all four tests below!
(test-cut 0 0 "#0='#0#" "'...\n")
(test-cut 0 1 "#0='#0#" "'...\n")
(test-cut 1 0 "#0='#0#" "#0='#0#\n")
(test-cut 1 1 "#0='#0#" "#0='#0#\n")

; LispKit: boxes increment level and are printed like `write` prints them
(test-cut 3 4 "#&#&#&#&(3 . #(a b c d e f g))))"
  "#<box #<box #<box #<box ...>>>>\n")
(test-cut 0 0 "#0=#&#0#" "#<box ...>\n")
(test-cut 0 1 "#0=#&#0#" "#<box ...>\n")
(test-cut 1 0 "#0=#&#0#" "#<box ...>\n")
(test-cut 1 1 "#0=#&#0#" "#<box #<box ...>>\n")
(srfi-272-report-skipped "SRFI 272 intermediate"))

(test-group "Golden tests"

(define cut-sample '(if (member x y) (+ (car x) 3) '(foo . #(a b c d "Baz"))))
(define bv30 '#u8(0 10 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30))
(test-equal "sexp1"
  "(define (ast-pretty a . opt-port)\n  (define (width x)\n    (cond\n      ((string? x) (+ (string-length x) 2))\n      ((char? x) 3)\n      ((number? x) 5)\n      ((symbol? x) (string-length (symbol->string x)))\n      ((boolean? x) 2)\n      ((null? x) 2)\n      (else 10)))\n  (define (ast-width a)\n    (cond\n      ((ast-atom? a) (width (ast-atom->val a)))\n      ((ast-null? a) 2)\n      ((ast-pair? a)\n       (if (ast-list? a)\n           (let loop ((a a) (sum 0) (count 0))\n             (if (ast-null? a)\n                 (+ sum count 1)\n                 (loop (ast-cdr a) (+ sum (ast-width (ast-car a))) (+ count 1))))\n           (+ 1 (ast-width (ast-car a)) 3 (ast-width (ast-cdr a)) 1)))\n      (else 0)))\n  (define (ast-width-hello a)\n    (display \"hello!\\n\")\n    (cond\n      ((ast-atom? a) (width (ast-atom->val a)))\n      ((ast-null? a) 2)\n      ((ast-pair? a)\n       (if (ast-list? a)\n           (let loop ((a a) (sum 0) (count 0))\n             (if (ast-null? a)\n                 (+ sum count 1)\n                 (loop (ast-cdr a) (+ sum (ast-width (ast-car a))) (+ count 1))))\n           (+ 1 (ast-width (ast-car a)) 3 (ast-width (ast-cdr a)) 1)))\n      (else\n       (cond\n         ((foo) => (lambda (v) (bar v 0)))\n         (else (begin (newline) (if a #t #f)))))))\n  (define (break? cur-col width max-col)\n    (> (+ cur-col width) max-col))\n  (define (print-indent n port)\n    (do ((i 0 (+ i 1))) ((= i n)) (display \"  \" port)))\n  (define (pretty a col indent port max-col)\n    (cond\n      ((ast-atom? a)\n       (let ((w (width (ast-atom->val a))))\n         (write (ast-atom->val a) port)\n         (+ col w)))\n      ((ast-null? a) (display \"()\" port) (+ col 2))\n      ((ast-pair? a)\n       (if (ast-list? a)\n           (let*\n             ((total-width (ast-width a))\n              (remaining (- max-col col))\n              (inline? (<= total-width remaining)))\n             (if inline?\n                 (begin\n                   (display \"(\" port)\n                   (let loop ((first? #t) (rest a) (cur-col (+ col 1)))\n                     (if (ast-null? rest)\n                         (begin (display \")\" port) (+ cur-col 1))\n                         (let*\n                           ((elem-width (ast-width (ast-car rest)))\n                            (new-col\n                             (if first?\n                                 (pretty (ast-car rest) cur-col indent port\n                                   max-col)\n                                 (begin\n                                   (display \" \" port)\n                                   (pretty (ast-car rest) (+ cur-col 1) indent\n                                     port max-col)))))\n                           (loop #f (ast-cdr rest) new-col)))))\n                 (begin\n                   (display \"(\" port)\n                   (let ((new-indent (+ indent 1)))\n                     (let loop ((rest a) (cur-col (* new-indent 2)))\n                       (if (ast-null? rest)\n                           (begin (display \")\" port) (+ cur-col 1))\n                           (let*\n                             ((elem (ast-car rest))\n                              (next-col\n                               (pretty elem cur-col new-indent port max-col)))\n                             (unless (ast-null? (ast-cdr rest))\n                               (newline port)\n                               (print-indent new-indent port))\n                             (loop (ast-cdr rest) (* new-indent 2)))))))))\n           (begin\n             (display \"(\" port)\n             (let\n               ((car-col (pretty (ast-car a) (+ col 1) indent port max-col)))\n               (display \" . \" port)\n               (let\n                 ((cdr-col\n                   (pretty (ast-cdr a) (+ car-col 3) indent port max-col)))\n                 (display \")\" port)\n                 (+ cdr-col 1))))))))\n  (let\n    ((port\n      (if (null? opt-port) (current-output-port) (car opt-port))))\n    (pretty a 0 0 port 80)\n    (newline port)))\n"
  (pp->string sexp1))
(test-equal "sexp2, width 90"
  "(define (%read port simple? ci?)\n  (define-syntax r-error\n    (syntax-rules () ((_ p msg a ...) (read-error p msg a ...))))\n  (define fold-case? (or ci? (port-fold-case? port)))\n  (define shared '())\n  (define (make-shared-ref loc) (lambda () (unbox loc)))\n  (define (shared-ref? form) (procedure? form))\n  (define (patch-ref! form)\n    (if (procedure? form) (patch-ref! (form)) form))\n  (define (patch-shared! form)\n    (cond\n      ((pair? form)\n       (if (procedure? (car form))\n           (set-car! form (patch-ref! (car form)))\n           (patch-shared! (car form)))\n       (if (procedure? (cdr form))\n           (set-cdr! form (patch-ref! (cdr form)))\n           (patch-shared! (cdr form))))\n      ((vector? form)\n       (let loop ((i 0))\n         (when (fx<? i (vector-length form))\n           (let ((fi (vector-ref form i)))\n             (if (procedure? fi)\n                 (vector-set! form i (patch-ref! fi))\n                 (patch-shared! fi)))\n           (loop (fx+ i 1)))))\n      ((box? form)\n       (if (procedure? (unbox form))\n           (set-box! form (patch-shared! (unbox form)))\n           (patch-shared! (unbox form))))))\n  (define (patch-shared form) (patch-shared! form) form)\n  (define reader-token-marker #f)\n  (define close-paren #f)\n  (define close-bracket #f)\n  (define dot #f)\n  (define ()\n    (let ((rtm (list 'reader-token)))\n      (set! reader-token-marker rtm)\n      (set! close-paren (cons rtm \"right parenthesis\"))\n      (set! close-bracket (cons rtm \"right bracket\"))\n      (set! dot (cons rtm \"\\\" . \\\"\"))))\n  (define (reader-token? form)\n    (and (pair? form) (eq? (car form) reader-token-marker)))\n  (define (char-hex-digit? c)\n    (let ((scalar-value (char->integer c)))\n      (or (and (>= scalar-value 48) (<= scalar-value 57))\n          (and (>= scalar-value 65) (<= scalar-value 70))\n          (and (>= scalar-value 97) (<= scalar-value 102)))))\n  (define (char-delimiter? c)\n    (or (char-whitespace? c) (char=? c #\\)) (char=? c #\\()\n        (char=? c #\\]) (char=? c #\\[) (char=? c #\\\") (char=? c #\\|)\n        (char=? c #\\;)))\n  (define (sub-read-carefully p)\n    (let ((form (sub-read p)))\n      (cond\n        ((eof-object? form) (r-error p \"unexpected end of file\"))\n        ((reader-token? form)\n         (r-error p \"unexpected token:\" (cdr form)))\n        (else form))))\n  (define (sub-read-shebang p)\n    (if (eqv? (peek-char p) #\\space)\n        (string->symbol (string-trim-whitespace (read-line p)))\n        (sub-read-carefully p)))\n  (define (sub-read p)\n    (let ((c (%read-ahead fold-case? p)))\n      (cond\n        ((not (char? c)) c)\n        ((char=? c #\\() (sub-read-list c p close-paren #t))\n        ((char=? c #\\)) close-paren)\n        ((char=? c #\\[) (sub-read-list c p close-bracket #t))\n        ((char=? c #\\]) close-bracket)\n        ((char=? c #\\.) dot)\n        ((char=? c #\\') (list 'quote (sub-read-carefully p)))\n        ((char=? c #\\`) (list 'quasiquote (sub-read-carefully p)))\n        ((char=? c #\\n)\n         (r-error p\n           \"unsupported number syntax (implementation restriction)\"))\n        ((char=? c #\\d) (r-error p \"invalid delimiter\"))\n        ((char=? c #\\y) (r-error p \"unsupported symbol syntax\"))\n        ((char=? c #\\z) (r-error p \"invalid token\"))\n        ((char=? c #\\,)\n         (let ((next (peek-char p)))\n           (cond\n             ((eof-object? next) (r-error p \"end of file after ,\"))\n             ((char=? next #\\@)\n              (read-char p)\n              (list 'unquote-splicing (sub-read-carefully p)))\n             (else (list 'unquote (sub-read-carefully p))))))\n        ((char=? c #\\\")\n         (let loop ((l '()))\n           (let ((c (read-char p)))\n             (cond\n               ((eof-object? c) (r-error p \"end of file within a string\"))\n               ((char=? c #\\\\)\n                (let ((e (sub-read-strsym-char-escape p 'string)))\n                  (loop (if e (cons e l) l))))\n               ((char=? c #\\\") (list->string (reverse! l)))\n               (else (loop (cons c l)))))))\n        ((char=? c #\\|)\n         (let loop ((l '()))\n           (let ((c (read-char p)))\n             (cond\n               ((eof-object? c)\n                (r-error p \"end of file within a |symbol|\"))\n               ((char=? c #\\\\)\n                (let ((e (sub-read-strsym-char-escape p 'symbol)))\n                  (loop (if e (cons e l) l))))\n               ((char=? c #\\|)\n                (string->symbol (list->string (reverse! l))))\n               (else (loop (cons c l)))))))\n        ((char=? c #\\#)\n         (let ((c (peek-char p)))\n           (cond\n             ((eof-object? c) (r-error p \"end of file after #\"))\n             ((char=? c #\\!)\n              (read-char p)\n              (let ((name (sub-read-shebang p)))\n                (case name\n                  ((fold-case no-fold-case)\n                   (set! fold-case? (eq? name 'fold-case))\n                   (set-port-fold-case! p fold-case?)\n                   (sub-read p))\n                  (else\n                   (if (symbol? name)\n                       (symbol->shebang name)\n                       (r-error p \"unexpected name after #!\" name))))))\n             ((or (char-ci=? c #\\t) (char-ci=? c #\\f) (char-ci=? c #\\s)\n                  (char-ci=? c #\\u))\n              (let ((name (sub-read-carefully p)))\n                (case name\n                  ((t true) #t)\n                  ((f false) #f)\n                  ((u8) (list->numvector (sub-read-numerical-list p name) 0))\n                  ((s8) (list->numvector (sub-read-numerical-list p name) 1))\n                  ((u16) (list->numvector (sub-read-numerical-list p name) 2))\n                  ((s16) (list->numvector (sub-read-numerical-list p name) 3))\n                  ((f32)\n                   (list->numvector (sub-read-numerical-list p name) 10))\n                  ((f64)\n                   (list->numvector (sub-read-numerical-list p name) 11))\n                  (else (r-error p \"unexpected name after #\" name)))))\n             ((char=? c #\\&) (read-char p) (box (sub-read-carefully p)))\n             ((char=? c #\\;)\n              (read-char p)\n              (sub-read-carefully p)\n              (sub-read p))\n             ((char=? c #\\|)\n              (read-char p)\n              (let recur ()\n                (let ((next (read-char p)))\n                  (cond\n                    ((eof-object? next) (r-error p \"end of file in #| comment\"))\n                    ((char=? next #\\|)\n                     (let ((next (peek-char p)))\n                       (cond\n                         ((eof-object? next) (r-error p \"end of file in #| comment\"))\n                         ((char=? next #\\#) (read-char p))\n                         (else (recur)))))\n                    ((char=? next #\\#)\n                     (let ((next (peek-char p)))\n                       (cond\n                         ((eof-object? next) (r-error p \"end of file in #| comment\"))\n                         ((char=? next #\\|) (read-char p) (recur) (recur))\n                         (else (recur)))))\n                    (else (recur)))))\n              (sub-read p))\n             ((char=? c #\\()\n              (read-char p)\n              (list->vector (sub-read-list c p close-paren #f)))\n             ((char=? c #\\\\)\n              (read-char p)\n              (let ((c (peek-char p)))\n                (cond\n                  ((eof-object? c) (r-error p \"end of file after #\\\\\"))\n                  ((char=? #\\x c)\n                   (read-char p)\n                   (if (char-delimiter? (peek-char p))\n                       c\n                       (sub-read-x-char-escape p #f)))\n                  ((char-alphabetic? c)\n                   (let ((name (sub-read-carefully p)))\n                     (if (= (string-length (symbol->string name)) 1)\n                         c\n                         (case name\n                           ((null) (integer->char 0))\n                           ((space) #\\space)\n                           ((backspace) #\\backspace)\n                           ((tab) #\\tab)\n                           ((newline linefeed) #\\newline)\n                           ((return) #\\return)\n                           (else (r-error p \"unknown #\\\\ name\" name))))))\n                  (else (read-char p) c))))\n             ((char-numeric? c)\n              (when simple?\n                (r-error p \"#N=/#N# notation is not allowed in this mode\"))\n              (let loop ((l '()))\n                (let ((c (read-char p)))\n                  (cond\n                    ((eof-object? c)\n                     (r-error p \"end of file within a #N notation\"))\n                    ((char-numeric? c) (loop (cons c l)))\n                    ((char=? c #\\#)\n                     (let*\n                       ((s (list->string (reverse! l))) (n (string->number s)))\n                       (cond\n                         ((and (fixnum? n) (assq n shared)) => cdr)\n                         (else (r-error p \"unknown #n# reference:\" s)))))\n                    ((char=? c #\\=)\n                     (let*\n                       ((s (list->string (reverse! l))) (n (string->number s)))\n                       (cond\n                         ((not (fixnum? n)) (r-error p \"invalid #n= reference:\" s))\n                         ((assq n shared) (r-error p \"duplicate #n= tag:\" n)))\n                       (let ((loc (box #f)))\n                         (set! shared (cons (cons n (make-shared-ref loc)) shared))\n                         (let ((form (sub-read-carefully p)))\n                           (cond\n                             ((shared-ref? form)\n                              (r-error p \"#n= has another label as target\" s))\n                             (else (set-box! loc form) form))))))\n                    (else (r-error p \"invalid terminator for #N notation\"))))))\n             (else (r-error p \"unknown # syntax\" c)))))\n        (else (r-error p \"illegal character read\" c)))))\n  (define (sub-read-list c p close-token dot?)\n    (let ((form (sub-read p)))\n      (if (eq? form dot)\n          (r-error p \"missing car -- ( immediately followed by .\")\n          (let recur ((form form))\n            (cond\n              ((eof-object? form)\n               (r-error p \"eof inside list -- unbalanced parentheses\"))\n              ((eq? form close-token) '())\n              ((eq? form dot)\n               (if dot?\n                   (let*\n                     ((last-form (sub-read-carefully p))\n                      (another-form (sub-read p)))\n                     (if (eq? another-form close-token)\n                         last-form\n                         (r-error p \"randomness after form after dot\" another-form)))\n                   (r-error p \"dot in #(...)\")))\n              ((reader-token? form)\n               (r-error p \"error inside list --\" (cdr form)))\n              (else (cons form (recur (sub-read p)))))))))\n  (define (sub-read-numerical-list p ts)\n    (unless (eq? (read-char p) #\\()\n      (r-error p (format \"invalid ~avector syntax\" ts)))\n    (let recur ((form (sub-read p)))\n      (cond\n        ((eof-object? form)\n         (r-error p (format \"eof inside ~avector\" ts)))\n        ((eq? form close-paren) '())\n        ((reader-token? form)\n         (r-error p (format \"error inside ~avector --\" ts) (cdr form)))\n        ((or (and (eq? ts 'u8) (fixnum? form) (fx<=? 0 form 255))\n             (and (eq? ts 's8) (fixnum? form) (fx<=? -128 form 127))\n             (and (eq? ts 'u16) (fixnum? form) (fx<=? 0 form 65535))\n             (and (eq? ts 's16) (fixnum? form) (fx<=? -32768 form 32767))\n             (and (eq? ts 'f32) (flonum? form))\n             (and (eq? ts 'f64) (flonum? form)))\n         (cons form (recur (sub-read p))))\n        (else\n         (r-error p (format \"invalid ~a inside ~avector --\" ts ts)\n           form)))))\n  (define (sub-read-strsym-char-escape p what)\n    (let ((c (read-char p)))\n      (if (eof-object? c) (r-error p \"end of file within a\" what))\n      (cond\n        ((or (char=? c #\\\\) (char=? c #\\\") (char=? c #\\|)) c)\n        ((char=? c #\\a) #\\alarm)\n        ((char=? c #\\b) #\\backspace)\n        ((char=? c #\\t) #\\tab)\n        ((char=? c #\\n) #\\newline)\n        ((char=? c #\\r) #\\return)\n        ((char=? c #\\x) (sub-read-x-char-escape p #t))\n        ((and (eq? what 'string) (char-whitespace? c))\n         (let loop ((gotnl (char=? c #\\newline)) (nc (peek-char p)))\n           (cond\n             ((or (eof-object? nc) (not (char-whitespace? nc)))\n              (if gotnl #f (r-error p \"no newline in line ending escape\")))\n             ((and gotnl (char=? nc #\\newline)) #f)\n             (else\n              (read-char p)\n              (loop (or gotnl (char=? nc #\\newline)) (peek-char p))))))\n        (else (r-error p \"invalid char escape in\" what ': c)))))\n  (define (sub-read-x-char-escape p in-string?)\n    (define (rev-digits->char l)\n      (if (null? l)\n          (r-error p \"\\\\x escape sequence is too short\")\n          (integer->char\n            (string->fixnum (list->string (reverse! l)) 16))))\n    (let loop ((c (peek-char p)) (l '()) (cc 0))\n      (cond\n        ((eof-object? c)\n         (if in-string?\n             (r-error p \"end of file within a string\")\n             (rev-digits->char l)))\n        ((and in-string? (char=? c #\\;))\n         (read-char p)\n         (rev-digits->char l))\n        ((and (not in-string?) (char-delimiter? c))\n         (rev-digits->char l))\n        ((not (char-hex-digit? c))\n         (r-error p \"unexpected char in \\\\x escape sequence\" c))\n        ((> cc 2) (r-error p \"\\\\x escape sequence is too long\"))\n        (else\n         (read-char p)\n         (loop (peek-char p) (cons c l) (+ cc 1))))))\n  (let ((form (sub-read port)))\n    (if (not (reader-token? form))\n        (if (null? shared) form (patch-shared form))\n        (r-error port \"unexpected token:\" (cdr form)))))\n"
  (pp->string sexp2 pp-width 90))
(test-equal "sexp3, width 50"
  "(cond\n  ((equal? (token-value tok) \"remove\")\n   (continue nc ti\n     (lambda (tree)\n       (unless (moore-lookup-node tree s)\n         (p-error ti\n           \"notation for ~s does not exist\" s))\n       (moore-remove-handler tree s))))\n  ((equal? (token-value tok) \"additive\")\n   (let-parse-result\n     (((hd nc ti)\n        (parse-null nc (ti-pop ti 'lpar) rbp)))\n     (define op (string->symbol s))\n     (define hd (operand-token->expression tk))\n     (define-values (a . b) `(,a ,@b))\n     (define-values (ti comb)\n       (if (eq? (token-type (ti-car ti)) 'comma)\n           (values\n             (ti-pop (ti-cdr (ti-cdr ti)) 'rpar)\n             list)\n           (values (ti-pop ti 'rpar) nary)))\n     (continue\n       (notation-conf-add-led nc op infix-led\n         bp:add bp:add hd comb)\n       ti (add-auto-handler s))))\n  (else\n   (p-error ti \"unknown modifier for notation: ~a\"\n     (token-value tok))))\n"
  (pp->string sexp3 pp-width 50))
(test-equal "sexp4, width 40"
  "(define-syntax set*!\n  (lambda (x)\n    (letrec\n      ((unwrap-exp\n        (lambda (x)\n          (let ((x (unwrap-syntax x)))\n            (if (pair? x)\n                (cons (car x)\n                  (unwrap-exp (cdr x)))\n                x)))))\n      (let\n        ((sets\n          (map unwrap-exp\n               (cdr (unwrap-exp x)))))\n        (let\n          ((ids (map car sets))\n           (vals (map cadr sets))\n           (temps\n            (map (lambda (x)\n                   (generate-identifier))\n                 sets)))\n          `(,'let\n            ,(map list temps vals)\n            ,@(map\n               (lambda\n                (id temp)\n                `(,'set! ,id ,temp))\n               ids\n               temps)\n            #f))))))\n"
  (pp->string sexp4 pp-width 40))
(test-equal "sexp5, width 70"
  "(define (normalize-definition t syntax-definition?)\n  (cond\n    ((pair? (cdr t))\n     (let ((_ (car t)) (head (cadr t)) (body (cddr t)))\n       (cond\n         ((and (identifier? head) (pair? body) (null? (cdr body)))\n          `(,_ ,head unquote body))\n         ((and (pair? head) (identifier? (car head))\n               (formals? (cdr head)))\n          (let ((r (make-primitive-renaming-procedure)))\n            (if syntax-definition?\n                `(,_\n                  ,(car head)\n                  ,(let\n                    ((transformer (r (symbolic-name (car head)))))\n                    `((,(r 'lambda)\n                       (,transformer)\n                       (,(r 'lambda)\n                        (,(r 'form))\n                        (,(r 'apply) ,transformer ,(r 'form))))\n                      (,(r 'lambda)\n                       (,(r 'dummy) unquote (cdr head))\n                       unquote\n                       body))))\n                `(,_\n                  ,(car head)\n                  (,(r 'lambda) ,(cdr head) unquote body)))))\n         (else (syntax-error \"Syntax error in definition:\" t)))))\n    (else (syntax-error \"Syntax error in definition:\" t))))\n"
  (pp->string sexp5 pp-width 70))
(test-equal "level 0, length 1"
  "(...)\n"
  (pp->string cut-sample pp-level 0 pp-length 1))
(test-equal "level 1, length 1"
  "(if ...)\n"
  (pp->string cut-sample pp-level 1 pp-length 1))
(test-equal "level 1, length 2"
  "(if (...) ...)\n"
  (pp->string cut-sample pp-level 1 pp-length 2))
(test-equal "level 1, length 3"
  "(if (...) (...) ...)\n"
  (pp->string cut-sample pp-level 1 pp-length 3))
(test-equal "level 1, length 4"
  "(if (...) (...) '...)\n"
  (pp->string cut-sample pp-level 1 pp-length 4))
(test-equal "level 2, length 1"
  "(if ...)\n"
  (pp->string cut-sample pp-level 2 pp-length 1))
(test-equal "level 2, length 2"
  "(if (member x ...) ...)\n"
  (pp->string cut-sample pp-level 2 pp-length 2))
(test-equal "level 2, length 3"
  "(if (member x y) (+ (...) 3) ...)\n"
  (pp->string cut-sample pp-level 2 pp-length 3))
(test-equal "level 3, length 2"
  "(if (member x ...) ...)\n"
  (pp->string cut-sample pp-level 3 pp-length 2))
(test-equal "level 3, length 3"
  "(if (member x y) (+ (car x) 3) ...)\n"
  (pp->string cut-sample pp-level 3 pp-length 3))
(test-equal "level 3, length 4"
  "(if (member x y) (+ (car x) 3) '(foo . #(a b c d ...)))\n"
  (pp->string cut-sample pp-level 3 pp-length 4))
(test-equal "level 3, length 5"
  "(if (member x y) (+ (car x) 3) '(foo . #(a b c d \"Baz\")))\n"
  (pp->string cut-sample pp-level 3 pp-length 5))
(test-equal "bytevector, width 20"
  "#u8(0 10 2 3 4 5 6 7\n    8 9 10 11 12 13\n    14 15 16 17 18\n    19 20 21 22 23\n    24 25 26 27 28\n    29 30)\n"
  (pp->string bv30 pp-width 20))
(test-equal "bytevector, radix 2"
  "#u8(#b0 #b1010 #b10\n    #b11 #b100 #b101\n    #b110 #b111\n    #b1000 #b1001\n    #b1010 #b1011\n    #b1100 #b1101\n    #b1110 #b1111\n    #b10000 #b10001\n    #b10010 #b10011\n    #b10100 #b10101\n    #b10110 #b10111\n    #b11000 #b11001\n    #b11010 #b11011\n    #b11100 #b11101\n    #b11110)\n"
  (pp->string bv30 pp-width 20 pp-radix 2))
(test-equal "bytevector, radix 8"
  "#u8(#o0 #o12 #o2 #o3\n    #o4 #o5 #o6 #o7\n    #o10 #o11 #o12\n    #o13 #o14 #o15\n    #o16 #o17 #o20\n    #o21 #o22 #o23\n    #o24 #o25 #o26\n    #o27 #o30 #o31\n    #o32 #o33 #o34\n    #o35 #o36)\n"
  (pp->string bv30 pp-width 20 pp-radix 8))
(test-equal "bytevector, radix 10"
  "#u8(0 10 2 3 4 5 6 7\n    8 9 10 11 12 13\n    14 15 16 17 18\n    19 20 21 22 23\n    24 25 26 27 28\n    29 30)\n"
  (pp->string bv30 pp-width 20 pp-radix 10))
(test-equal "bytevector, radix 16"
  "#u8(#x0 #xa #x2 #x3\n    #x4 #x5 #x6 #x7\n    #x8 #x9 #xa #xb\n    #xc #xd #xe #xf\n    #x10 #x11 #x12\n    #x13 #x14 #x15\n    #x16 #x17 #x18\n    #x19 #x1a #x1b\n    #x1c #x1d #x1e)\n"
  (pp->string bv30 pp-width 20 pp-radix 16))
(test-equal "radix 16"
  "(#xff #x-10 #x7/3 #x0 1.5 (#x7f))\n"
  (pp->string '(255 -16 7/3 0 1.5 (#x7f)) pp-radix 16))
(test-equal "radix 8"
  "#(#o377 #o-20 #o7/3 #o0)\n"
  (pp->string '#(255 -16 7/3 0) pp-radix 8))
(test-equal "radix 2"
  "(#b11111111 #b-10000 #b1/10)\n"
  (pp->string '(255 -16 1/2) pp-radix 2))
(test-equal "pp with port and keywords"
  "(define-syntax set*!\n  (lambda (x)\n    (letrec\n      ((unwrap-exp\n        (lambda (x)\n          (let\n            ((x\n              (unwrap-syntax x)))\n            (if (pair? x)\n                (cons (car x)\n                  (unwrap-exp\n                    (cdr x)))\n                ...)))))\n      (let\n        ((sets\n          (map unwrap-exp\n               (cdr (unwrap-exp\n                      x)))))\n        (let\n          ((ids\n            (map car sets))\n           (vals\n            (map cadr sets))\n           (temps\n            (map (lambda (x)\n                   (generate-identifier))\n                 sets)))\n          `(,'let\n            ,(map\n              list\n              temps\n              ...)\n            ,@(map\n               (lambda\n                (id temp)\n                `(,'set!\n                  ,id\n                  ,temp))\n               ids\n               ...)\n            ...))))))\n"
  (let ((p (open-output-string)))
      (pp sexp4 p pp-width 30 pp-length 3)
      (get-output-string p)))
(test-equal "pp*"
  "(define-syntax set*!\n  (lambda (x)\n    (letrec\n      ((unwrap-exp\n        (lambda (x)\n          (let\n            ((x\n              (unwrap-syntax x)))\n            (if (pair? x)\n                (cons (car x)\n                  (unwrap-exp\n                    (cdr x)))\n                ...)))))\n      (let\n        ((sets\n          (map unwrap-exp\n               (cdr (unwrap-exp\n                      x)))))\n        (let\n          ((ids\n            (map car sets))\n           (vals\n            (map cadr sets))\n           (temps\n            (map (lambda (x)\n                   (generate-identifier))\n                 sets)))\n          `(,'let\n            ,(map\n              list\n              temps\n              ...)\n            ,@(map\n               (lambda\n                (id temp)\n                `(,'set!\n                  ,id\n                  ,temp))\n               ids\n               ...)\n            ...))))))\n"
  (let ((p (open-output-string)))
      (pp* sexp4 p pp-width 30 (list pp-length 3 pp-width 60))
      (get-output-string p)))
(test-equal "pp* without port"
  "(a b ...)\n"
  (capture (lambda () (pp* '(a b c d e) pp-length 2 '()))))
(test-equal "pp to current output port"
  "(define (f x) (* x x))\n"
  (capture (lambda () (pp '(define (f x) (* x x))))))
(define shared-vec (let ((v (vector 1 2))) (list v v)))
(define cyclic-vec (let ((v (vector 1 2 3))) (vector-set! v 1 v) v))
(test-equal "pprint, shared vector"
  "(#(1 2) #(1 2))\n"
  (capture (lambda () (pprint shared-vec))))
(test-equal "pprint-shared, shared vector"
  "(#0=#(1 2) #0#)\n"
  (capture (lambda () (pprint-shared shared-vec))))
(test-equal "pprint-simple, shared vector"
  "(#(1 2) #(1 2))\n"
  (capture (lambda () (pprint-simple shared-vec))))
(test-equal "pprint, cyclic vector"
  "#0=#(1 #0# 3)\n"
  (let ((p (open-output-string))) (pprint cyclic-vec p pp-width 20) (get-output-string p)))
(test-equal "pprint-shared, cyclic vector"
  "#0=#(1 #0# 3)\n"
  (let ((p (open-output-string))) (pprint-shared cyclic-vec p) (get-output-string p)))
(test-equal "pprint overrides pp-graph"
  "(#(1 2) #(1 2))\n"
  (let ((p (open-output-string))) (pprint shared-vec p pp-graph #t) (get-output-string p)))
(test-equal "pp with pp-graph"
  "(#0=#(1 2) #0#)\n"
  (pp->string shared-vec pp-graph #t))
(test-equal "pp with pp-circle #f, pp-graph #f"
  "(#(1 2) #(1 2))\n"
  (pp->string shared-vec pp-circle #f pp-graph #f))
(test-equal "pretty-style of unknown symbol"
  #f
  (pretty-style 'unknown-form))
(test-equal "pretty-style returns a style"
  #f
  (not (pretty-style 'let)))
(test-equal "unknown form"
  "(my-when (> x 100)\n  (display \"x is large\")\n  (newline) (set! x (- x 100)))\n"
  (pp->string '(my-when (> x 100) (display "x is large") (newline) (set! x (- x 100))) pp-width 30))
(pretty-style 'my-when (pretty-style 'when))
(test-equal "pretty-style copied from when"
  "(my-when (> x 100)\n  (display \"x is large\")\n  (newline)\n  (set! x (- x 100)))\n"
  (pp->string '(my-when (> x 100) (display "x is large") (newline) (set! x (- x 100))) pp-width 30))
(pretty-style 'my-when #f)
(test-equal "pretty-style reset"
  "(my-when (> x 100)\n  (display \"x is large\")\n  (newline) (set! x (- x 100)))\n"
  (pp->string '(my-when (> x 100) (display "x is large") (newline) (set! x (- x 100))) pp-width 30))
(write-file (temp-path "srfi-272-a.scm")
  ";; Header comment\n;; second line\n\n(define (f x) (let ((y (* x x))) (if (> y 100) (display \"large\") (display \"small\")) (newline) y))\n; between\n\n\n(f 10)\n(define lst '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30))\n")
(test-equal "pprint-file to current output port"
  ";; Header comment\n;; second line\n\n(define (f x)\n  (let ((y (* x x)))\n    (if (> y 100) (display \"large\") (display \"small\"))\n    (newline)\n    y))\n; between\n\n\n(f 10)\n(define lst\n  '(1\n    2\n    3\n    4\n    5\n    6\n    7\n    8\n    9\n    10\n    11\n    12\n    13\n    14\n    15\n    16\n    17\n    18\n    19\n    20\n    21\n    22\n    23\n    24\n    25\n    26\n    27\n    28\n    29\n    30))\n"
  (capture (lambda () (pprint-file (temp-path "srfi-272-a.scm")))))
(test-equal "pprint-file to file"
  ";; Header comment\n;; second line\n\n(define (f x)\n  (let ((y (* x x)))\n    (if (> y 100) (display \"large\") (display \"small\"))\n    (newline)\n    y))\n; between\n\n\n(f 10)\n(define lst\n  '(1\n    2\n    3\n    4\n    5\n    6\n    7\n    8\n    9\n    10\n    11\n    12\n    13\n    14\n    15\n    16\n    17\n    18\n    19\n    20\n    21\n    22\n    23\n    24\n    25\n    26\n    27\n    28\n    29\n    30))\n"
  (begin
      (pprint-file (temp-path "srfi-272-a.scm") (temp-path "srfi-272-b.scm"))
      (read-file (temp-path "srfi-272-b.scm"))))
(delete-file (temp-path "srfi-272-a.scm"))
(delete-file (temp-path "srfi-272-b.scm"))
(write-file (temp-path "srfi-272-c.scm") "(define (f x) (let ((y (* x x))) (if (> y 100) (display \"large\") (display \"small\")) (newline) y))\n")
(test-equal "pprint-file with keyword arguments"
  "(define (f x)\n  (let ((y (* x x)))\n    (if (> y 100)\n        (display \"large\")\n        (display \"small\"))\n    (newline)\n    y))\n"
  (capture (lambda () (pprint-file (temp-path "srfi-272-c.scm") pp-width 30))))
(test-equal "pprint-file to file with keyword arguments"
  "(define (f x) ...)\n"
  (begin
      (pprint-file (temp-path "srfi-272-c.scm") (temp-path "srfi-272-d.scm") pp-width 40 pp-length 2)
      (read-file (temp-path "srfi-272-d.scm"))))
(delete-file (temp-path "srfi-272-c.scm"))
(delete-file (temp-path "srfi-272-d.scm")))

(test-group "LispKit-specific tests"

(test-equal "boxes" "(#<box 1> #<box (a b)>)\n" (pp->string (list (box 1) (box '(a b)))))
(test-equal "boxes, line breaks" "(#<box alpha>\n  #<box beta>\n  #<box gamma>\n  #<box delta>)\n"
  (pp->string (list (box 'alpha) (box 'beta) (box 'gamma) (box 'delta)) pp-width 20))
(test-equal "cyclic box" "#0=#<box (1 #0#)>\n"
  (pp->string (let ((b (box #f))) (set-box! b (list 1 b)) b)))
(test-equal "shared boxes" "(#0=#<box 1> #0#)\n"
  (pp->string (let ((b (box 1))) (list b b)) pp-graph #t))
(test-equal "mutable pairs" "#<pair 1 #<pair 2 ()>>\n" (pp->string (mcons 1 (mcons 2 '()))))
(test-equal "cyclic mutable pair" "#0=#<pair 1 #0#>\n"
  (pp->string (let ((m (mcons 1 '()))) (set-mcdr! m m) m)))
(test-equal "growable vector" "#g(1 2 3)\n" (pp->string (list->gvector '(1 2 3))))
(test-equal "cyclic growable vector" "#0=#g(1 2 #0#)\n"
  (pp->string (let ((g (list->gvector '(1 2)))) (gvector-add! g g) g)))
(test-equal "pairs are never labeled" "((1 2) (1 2))\n"
  (pp->string (let ((l (list 1 2))) (list l l)) pp-graph #t))
(test-equal "vectors are labeled" "(#0=#(1 2) #0#)\n"
  (pp->string (let ((v (vector 1 2))) (list v v)) pp-graph #t))
(test-equal "cycle through pair and vector" "(#0=#((#0#) 2))\n"
  (pp->string (let* ((v (vector #f 2)) (p (list v))) (vector-set! v 0 p) p)))
(test-equal "invalid pp-width" 'error
  (guard (e (#t 'error)) (pp->string 'x pp-width 0)))
(test-equal "invalid keyword argument list" 'error
  (guard (e (#t 'error)) (pp->string 'x pp-width)))
(test-equal "boxes, level" "#<box #<box #<box ...>>>\n"
  (pp->string (box (box (box (box 1)))) pp-level 2))
(test-equal "growable vector, length" "#g(1 2 ...)\n"
  (pp->string (list->gvector '(1 2 3 4 5)) pp-length 2))
(test-equal "mutable pairs, length" "#<pair a ...>\n"
  (pp->string (mcons 'a (mcons 'b (mcons 'c '()))) pp-length 1))
(test-equal "boxes, radix" "#(#<box #x1> #<box #x2>)\n"
  (pp->string (vector (box 1) (box 2)) pp-radix 16))
(test-equal "invalid pp-radix" 'error (guard (e (#t 'error)) (pp->string 1 pp-radix 3)))
(test-equal "invalid pp-level" 'error (guard (e (#t 'error)) (pp->string 1 pp-level -1))))

(test-end)
