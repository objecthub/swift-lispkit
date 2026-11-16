;;; SRFI 272 FANCY REGRESSION TEST SUITE
;;;
;;; This is the test suite for library `(srfi 272 fancy)`.
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
        (srfi 272 fancy)
        (lispkit gvector)
        (srfi 272 colorize)
        (prefix (srfi 272 advanced) adv:))

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

(test-begin "SRFI 272: Fancy pretty printing")

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
(pp-test 100 "#0=(#(#1=(a b) #1#) . #0#)" "#0=(#((a b) (a b)) . #0#)\n")
(pp-test 100 "(#0=(a . #0#) #1=#(#1#))" "(#0=(a . #0#) #1=#(#1#))\n")
(pp-test 100 "#0=(#1=(a . #0#) #1#)" "#0=((a . #0#) (a . #0#))\n")
(pp-test 100 "(1 2 3)" "(1 2 3)\n")
(pp-test 100 "(#0=(a) #0#)" "((a) (a))\n")
(pp-test 100 "#0=(a . #0#)" "#0=(a . #0#)\n")
(pp-test 100 "(#0=(a) #1=(b . #1#) #0#)" "((a) #0=(b . #0#) (a))\n")
(pp-test 100 "#(#0=#(1) #0#)" "#(#(1) #(1))\n")
(pp-test 100 "#0=#(#0#)" "#0=#(#0#)\n")
(pp-test 100 "(#0=(1 . #0#) #1=(2 . #1#))"
  "(#0=(1 . #0#) #1=(2 . #1#))\n")
(pp-test 100 "#0=(#1=(a . #0#) . #1#)" "#0=((a . #0#) a . #0#)\n")
(pp-test 100 "(#0=(a . #0#) #0#)" "(#0=(a . #0#) #0#)\n")
(pp-test 100 "#((#0=(a) #1=(b) #0#) #1#)" "#(((a) (b) (a)) (b))\n")

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
  (pp-test 100 "#0=(#1=(a) #1# . #0#)" "#0=(#1=(a) #1# . #0#)\n")
  (pp-test 100 "(#0=(a . #0#) #1=(b c) #1#)"
    "(#0=(a . #0#) #1=(b c) #1#)\n")
  (pp-test 100 "#0=(a . (#1=(b) . #0#))" "#0=(a (b) . #0#)\n")
  (pp-test 100 "#0=(#(#1=(a b) #1#) . #0#)"
    "#0=(#(#1=(a b) #1#) . #0#)\n")
  (pp-test 100 "(#0=(a . #0#) #1=#(#1#))" "(#0=(a . #0#) #1=#(#1#))\n")
  (pp-test 100 "#0=(#1=(a . #0#) #1#)" "#0=(#1=(a . #0#) #1#)\n")
  (pp-test 100 "(1 2 3)" "(1 2 3)\n")
  ; LispKit: pairs are values without identity and are never labeled
  (pp-test 100 "(#0=(a) #0#)" "((a) (a))\n")
  (pp-test 100 "#0=(a . #0#)" "#0=(a . #0#)\n")
  (pp-test 100 "(#0=(a) #1=(b . #1#) #0#)" "(#0=(a) #1=(b . #1#) #0#)\n")
  (pp-test 100 "#(#0=#(1) #0#)" "#(#0=#(1) #0#)\n")
  (pp-test 100 "#0=#(#0#)" "#0=#(#0#)\n")
  (pp-test 100 "(#0=(1 . #0#) #1=(2 . #1#))"
    "(#0=(1 . #0#) #1=(2 . #1#))\n")
  (pp-test 100 "#0=(#1=(a . #0#) . #1#)" "#0=(#1=(a . #0#) . #1#)\n")
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
(test-cut 2 10 "#(#(a b) #(c #(d)))" "#(#(a b) #(c #(...)))\n")
(test-cut 2 2 "((a b c) (d e f) (g h i))" "((a b ...) (d e ...) ...)\n")
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
(test-cut 1 2 "((a b c) (d e f) (g h i))" "((...) (...) ...)\n")
(test-cut 1 2 "#(#(a b c) #(d e f) #(g h i))" "#(#(...) #(...) ...)\n")
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
  (test-cut 2 #f "(#0=(a . b) #0# . #0#)" "((a . b) (a . b) a . b)\n")
  (test-cut 1 #f "(#0=(a . b) #0# . #0#)" "((...) (...) a . b)\n")
  (test-cut 3 2 "#0=((a . b) (#0# . c) d)" "#0=((a . b) (#0# . c) ...)\n")
  (test-cut 1 1 "#0=((a . b) (#0# . c) d)" "((...) ...)\n")
  (test-cut 2 #f "#0=(#(#0#) . #0#)" "#0=(#(#0#) . #0#)\n")
  (test-cut 1 #f "#0=(#(#0#) . #0#)" "#0=(#(...) . #0#)\n")
  ; this is what Chez does:
  ;(test-cut 2 2 "#0=(#1=(a b) #1# . #0#)" "#0=(#1=(a b) #1# . #0#)\n")
  ; I believe we're entitled to this:
  (test-cut 2 2 "#0=(#1=(a b) #1# . #0#)" "(#0=(a b) #0# ...)\n")
  ; this is what Chez does:
  ;(test-cut 1 2 "#0=(#1=(a b) #1# . #0#)" "#0=((...) (...) . #0#)\n")
  ; I believe we're entitled to this:
  (test-cut 1 2 "#0=(#1=(a b) #1# . #0#)" "((...) (...) ...)\n")
  (test-cut 2 #f "#0=(a . (#1=#(b #0#) . #1#))"
    "(a #0=#(b (...)) . #0#)\n")
  (test-cut 1 #f "#0=(a . (#1=#(b #0#) . #1#))" "(a #(...) . #(...))\n")
  (test-cut 2 1 "#0=(a . (#1=#(b #0#) . #1#))" "(a ...)\n")
  (test-cut 3 3 "(#0=(a . #0#) #1=#(#1#) #2=(b c) #2#)"
    "(#0=(a . #0#) #1=#(#1#) (b c) ...)\n")
  (test-cut 0 #f "#0=(a . #0#)" "(...)\n")
  (test-cut 1 #f "#0=(a . #0#)" "#0=(a . #0#)\n")
  (test-cut 1 #f "(#0=(a b) #0#)" "((...) (...))\n")
  (test-cut #f 1 "(#0=(a b c) #0#)" "((a ...) ...)\n")
  (test-cut 0 #f "#0=#(#0#)" "#(...)\n")
  (test-cut 1 #f "#0=#(#0#)" "#(#(...))\n")
  (test-cut 1 #f "(#0=(a) #1=(#0#) #1#)" "((...) (...) (...))\n")
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
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))" "(...)\n")
  (test-cut 0 1
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))" "(...)\n")
  (test-cut 0 2
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))" "(...)\n")
  (test-cut 0 3
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))" "(...)\n")
  (test-cut 1 0
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))" "(...)\n")
  (test-cut 1 1
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))" "(a ...)\n")
  (test-cut 1 2
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))"
    "(a (...) ...)\n")
  (test-cut 1 3
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))"
    "(a (...) (...) ...)\n")
  (test-cut 2 0
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))" "(...)\n")
  (test-cut 2 1
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))" "(a ...)\n")
  (test-cut 2 2
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))"
    "(a (b . #(...)) ...)\n")
  (test-cut 2 3
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))"
    "(a (b . #(...)) (#(...) . d) ...)\n")
  (test-cut 3 0
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))" "(...)\n")
  (test-cut 3 1
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))" "(a ...)\n")
  (test-cut 3 2
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))"
    "(a #0=(b . #(#0# c)) ...)\n")
  (test-cut 3 3
    "#0=(a #1=(b . #2=#(#1# c)) (#2# . d) #0# . #3=(e . #3#))"
    "(a #0=(b . #1=#(#0# c)) (#1# . d) ...)\n")
  (test-cut 0 0 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)" "(...)\n")
  (test-cut 0 1 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)" "(...)\n")
  (test-cut 0 2 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)" "(...)\n")
  (test-cut 0 3 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)" "(...)\n")
  (test-cut 1 0 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)" "(...)\n")
  (test-cut 1 1 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)"
    "((...) ...)\n")
  (test-cut 1 2 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)"
    "((...) #(...) ...)\n")
  (test-cut 1 3 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)"
    "((...) #(...) (...) ...)\n")
  (test-cut 2 0 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)" "(...)\n")
  (test-cut 2 1 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)"
    "((a ...) ...)\n")
  (test-cut 2 2 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)"
    "((a (...)) #(c d ...) ...)\n")
  (test-cut 2 3 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)"
    "#0=((a (...)) #(c d (...)) (e . #0#) ...)\n")
  (test-cut 3 0 "#0=(#1=(a (b)) #2=#(c d #1#) (e . #0#) f)" "(...)\n")
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
(srfi-272-report-skipped "SRFI 272 fancy"))

(test-group "Golden tests"

(define cut-sample '(if (member x y) (+ (car x) 3) '(foo . #(a b c d "Baz"))))
(define bv30 '#u8(0 10 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30))
(define small-fun '(define (f x) (let ((y (* x x))) (if (> y 100) (display "large") (display "small")) (newline) y)))
(test-equal "sexp1, code, brackets"
  "(define (ast-pretty a . opt-port)\n  (define (width x)\n    (cond [(string? x) (+ (string-length x) 2)]\n          [(char? x) 3]\n          [(number? x) 5]\n          [(symbol? x) (string-length (symbol->string x))]\n          [(boolean? x) 2]\n          [(null? x) 2]\n          [else 10]))\n  (define (ast-width a)\n    (cond [(ast-atom? a) (width (ast-atom->val a))]\n          [(ast-null? a) 2]\n          [(ast-pair? a)\n           (if (ast-list? a)\n               (let loop ([a a] [sum 0] [count 0])\n                 (if (ast-null? a)\n                     (+ sum count 1)\n                     (loop (ast-cdr a) (+ sum (ast-width (ast-car a)))\n                           (+ count 1))))\n               (+ 1 (ast-width (ast-car a)) 3 (ast-width (ast-cdr a)) 1))]\n          [else 0]))\n  (define (ast-width-hello a)\n    (display \"hello!\\n\")\n    (cond [(ast-atom? a) (width (ast-atom->val a))]\n          [(ast-null? a) 2]\n          [(ast-pair? a)\n           (if (ast-list? a)\n               (let loop ([a a] [sum 0] [count 0])\n                 (if (ast-null? a)\n                     (+ sum count 1)\n                     (loop (ast-cdr a) (+ sum (ast-width (ast-car a)))\n                           (+ count 1))))\n               (+ 1 (ast-width (ast-car a)) 3 (ast-width (ast-cdr a)) 1))]\n          [else\n           (cond [(foo) => (lambda (v) (bar v 0))]\n                 [else (begin (newline) (if a #t #f))])]))\n  (define (break? cur-col width max-col)\n    (> (+ cur-col width) max-col))\n  (define (print-indent n port)\n    (do ([i 0 (+ i 1)]) [(= i n)] (display \"  \" port)))\n  (define (pretty a col indent port max-col)\n    (cond [(ast-atom? a)\n           (let ([w (width (ast-atom->val a))])\n             (write (ast-atom->val a) port)\n             (+ col w))]\n          [(ast-null? a) (display \"()\" port) (+ col 2)]\n          [(ast-pair? a)\n           (if (ast-list? a)\n               (let* ([total-width (ast-width a)]\n                      [remaining (- max-col col)]\n                      [inline? (<= total-width remaining)])\n                 (if inline?\n                     (begin\n                       (display \"(\" port)\n                       (let loop ([first? #t] [rest a] [cur-col (+ col 1)])\n                         (if (ast-null? rest)\n                             (begin (display \")\" port) (+ cur-col 1))\n                             (let*\n                               ([elem-width (ast-width (ast-car rest))]\n                                [new-col\n                                 (if first?\n                                     (pretty (ast-car rest) cur-col indent port\n                                       max-col)\n                                     (begin\n                                       (display \" \" port)\n                                       (pretty (ast-car rest) (+ cur-col 1)\n                                         indent port max-col)))])\n                               (loop #f (ast-cdr rest) new-col)))))\n                     (begin\n                       (display \"(\" port)\n                       (let ([new-indent (+ indent 1)])\n                         (let loop ([rest a] [cur-col (* new-indent 2)])\n                           (if (ast-null? rest)\n                               (begin (display \")\" port) (+ cur-col 1))\n                               (let*\n                                 ([elem (ast-car rest)]\n                                  [next-col\n                                   (pretty elem cur-col new-indent port max-col)])\n                                 (unless (ast-null? (ast-cdr rest))\n                                   (newline port)\n                                   (print-indent new-indent port))\n                                 (loop (ast-cdr rest) (* new-indent 2)))))))))\n               (begin\n                 (display \"(\" port)\n                 (let\n                   ([car-col (pretty (ast-car a) (+ col 1) indent port max-col)])\n                   (display \" . \" port)\n                   (let\n                     ([cdr-col\n                       (pretty (ast-cdr a) (+ car-col 3) indent port max-col)])\n                     (display \")\" port)\n                     (+ cdr-col 1)))))]))\n  (let\n    ([port\n      (if (null? opt-port) (current-output-port) (car opt-port))])\n    (pretty a 0 0 port 80)\n    (newline port)))\n"
  (pp->string sexp1 pp-code #t pp-brackets #t pp-max-tab 5))
(test-equal "sexp2, code, brackets, width 90"
  "(define (%read port simple? ci?)\n  (define-syntax r-error\n    (syntax-rules () [(_ p msg a ...) (read-error p msg a ...)]))\n  (define fold-case? (or ci? (port-fold-case? port)))\n  (define shared '())\n  (define (make-shared-ref loc) (lambda () (unbox loc)))\n  (define (shared-ref? form) (procedure? form))\n  (define (patch-ref! form)\n    (if (procedure? form) (patch-ref! (form)) form))\n  (define (patch-shared! form)\n    (cond [(pair? form)\n           (if (procedure? (car form))\n               (set-car! form (patch-ref! (car form)))\n               (patch-shared! (car form)))\n           (if (procedure? (cdr form))\n               (set-cdr! form (patch-ref! (cdr form)))\n               (patch-shared! (cdr form)))]\n          [(vector? form)\n           (let loop ([i 0])\n             (when (fx<? i (vector-length form))\n               (let ([fi (vector-ref form i)])\n                 (if (procedure? fi)\n                     (vector-set! form i (patch-ref! fi))\n                     (patch-shared! fi)))\n               (loop (fx+ i 1))))]\n          [(box? form)\n           (if (procedure? (unbox form))\n               (set-box! form (patch-shared! (unbox form)))\n               (patch-shared! (unbox form)))]))\n  (define (patch-shared form) (patch-shared! form) form)\n  (define reader-token-marker #f)\n  (define close-paren #f)\n  (define close-bracket #f)\n  (define dot #f)\n  (define ()\n    (let ([rtm (list 'reader-token)])\n      (set! reader-token-marker rtm)\n      (set! close-paren (cons rtm \"right parenthesis\"))\n      (set! close-bracket (cons rtm \"right bracket\"))\n      (set! dot (cons rtm \"\\\" . \\\"\"))))\n  (define (reader-token? form)\n    (and (pair? form) (eq? (car form) reader-token-marker)))\n  (define (char-hex-digit? c)\n    (let ([scalar-value (char->integer c)])\n      (or (and (>= scalar-value 48) (<= scalar-value 57))\n          (and (>= scalar-value 65) (<= scalar-value 70))\n          (and (>= scalar-value 97) (<= scalar-value 102)))))\n  (define (char-delimiter? c)\n    (or (char-whitespace? c) (char=? c #\\)) (char=? c #\\()\n        (char=? c #\\]) (char=? c #\\[) (char=? c #\\\") (char=? c #\\|)\n        (char=? c #\\;)))\n  (define (sub-read-carefully p)\n    (let ([form (sub-read p)])\n      (cond [(eof-object? form) (r-error p \"unexpected end of file\")]\n            [(reader-token? form)\n             (r-error p \"unexpected token:\" (cdr form))]\n            [else form])))\n  (define (sub-read-shebang p)\n    (if (eqv? (peek-char p) #\\space)\n        (string->symbol (string-trim-whitespace (read-line p)))\n        (sub-read-carefully p)))\n  (define (sub-read p)\n    (let ([c (%read-ahead fold-case? p)])\n      (cond [(not (char? c)) c]\n            [(char=? c #\\() (sub-read-list c p close-paren #t)]\n            [(char=? c #\\)) close-paren]\n            [(char=? c #\\[) (sub-read-list c p close-bracket #t)]\n            [(char=? c #\\]) close-bracket]\n            [(char=? c #\\.) dot]\n            [(char=? c #\\') (list 'quote (sub-read-carefully p))]\n            [(char=? c #\\`) (list 'quasiquote (sub-read-carefully p))]\n            [(char=? c #\\n)\n             (r-error p\n               \"unsupported number syntax (implementation restriction)\")]\n            [(char=? c #\\d) (r-error p \"invalid delimiter\")]\n            [(char=? c #\\y) (r-error p \"unsupported symbol syntax\")]\n            [(char=? c #\\z) (r-error p \"invalid token\")]\n            [(char=? c #\\,)\n             (let ([next (peek-char p)])\n               (cond [(eof-object? next) (r-error p \"end of file after ,\")]\n                     [(char=? next #\\@)\n                      (read-char p)\n                      (list 'unquote-splicing (sub-read-carefully p))]\n                     [else (list 'unquote (sub-read-carefully p))]))]\n            [(char=? c #\\\")\n             (let loop ([l '()])\n               (let ([c (read-char p)])\n                 (cond [(eof-object? c) (r-error p \"end of file within a string\")]\n                       [(char=? c #\\\\)\n                        (let ([e (sub-read-strsym-char-escape p 'string)])\n                          (loop (if e (cons e l) l)))]\n                       [(char=? c #\\\") (list->string (reverse! l))]\n                       [else (loop (cons c l))])))]\n            [(char=? c #\\|)\n             (let loop ([l '()])\n               (let ([c (read-char p)])\n                 (cond [(eof-object? c)\n                        (r-error p \"end of file within a |symbol|\")]\n                       [(char=? c #\\\\)\n                        (let ([e (sub-read-strsym-char-escape p 'symbol)])\n                          (loop (if e (cons e l) l)))]\n                       [(char=? c #\\|)\n                        (string->symbol (list->string (reverse! l)))]\n                       [else (loop (cons c l))])))]\n            [(char=? c #\\#)\n             (let ([c (peek-char p)])\n               (cond [(eof-object? c) (r-error p \"end of file after #\")]\n                     [(char=? c #\\!)\n                      (read-char p)\n                      (let ([name (sub-read-shebang p)])\n                        (case name\n                          [(fold-case no-fold-case)\n                           (set! fold-case? (eq? name 'fold-case))\n                           (set-port-fold-case! p fold-case?)\n                           (sub-read p)]\n                          [else\n                           (if (symbol? name)\n                               (symbol->shebang name)\n                               (r-error p \"unexpected name after #!\" name))]))]\n                     [(or (char-ci=? c #\\t) (char-ci=? c #\\f) (char-ci=? c #\\s)\n                          (char-ci=? c #\\u))\n                      (let ([name (sub-read-carefully p)])\n                        (case name\n                          [(t true) #t]\n                          [(f false) #f]\n                          [(u8) (list->numvector (sub-read-numerical-list p name) 0)]\n                          [(s8) (list->numvector (sub-read-numerical-list p name) 1)]\n                          [(u16) (list->numvector (sub-read-numerical-list p name) 2)]\n                          [(s16) (list->numvector (sub-read-numerical-list p name) 3)]\n                          [(f32)\n                           (list->numvector (sub-read-numerical-list p name) 10)]\n                          [(f64)\n                           (list->numvector (sub-read-numerical-list p name) 11)]\n                          [else (r-error p \"unexpected name after #\" name)]))]\n                     [(char=? c #\\&) (read-char p) (box (sub-read-carefully p))]\n                     [(char=? c #\\;)\n                      (read-char p)\n                      (sub-read-carefully p)\n                      (sub-read p)]\n                     [(char=? c #\\|)\n                      (read-char p)\n                      (let recur ()\n                        (let ([next (read-char p)])\n                          (cond [(eof-object? next)\n                                 (r-error p \"end of file in #| comment\")]\n                                [(char=? next #\\|)\n                                 (let ([next (peek-char p)])\n                                   (cond [(eof-object? next)\n                                          (r-error p \"end of file in #| comment\")]\n                                         [(char=? next #\\#) (read-char p)]\n                                         [else (recur)]))]\n                                [(char=? next #\\#)\n                                 (let ([next (peek-char p)])\n                                   (cond [(eof-object? next)\n                                          (r-error p \"end of file in #| comment\")]\n                                         [(char=? next #\\|) (read-char p) (recur) (recur)]\n                                         [else (recur)]))]\n                                [else (recur)])))\n                      (sub-read p)]\n                     [(char=? c #\\()\n                      (read-char p)\n                      (list->vector (sub-read-list c p close-paren #f))]\n                     [(char=? c #\\\\)\n                      (read-char p)\n                      (let ([c (peek-char p)])\n                        (cond [(eof-object? c) (r-error p \"end of file after #\\\\\")]\n                              [(char=? #\\x c)\n                               (read-char p)\n                               (if (char-delimiter? (peek-char p))\n                                   c\n                                   (sub-read-x-char-escape p #f))]\n                              [(char-alphabetic? c)\n                               (let ([name (sub-read-carefully p)])\n                                 (if (= (string-length (symbol->string name)) 1)\n                                     c\n                                     (case name\n                                       [(null) (integer->char 0)]\n                                       [(space) #\\space]\n                                       [(backspace) #\\backspace]\n                                       [(tab) #\\tab]\n                                       [(newline linefeed) #\\newline]\n                                       [(return) #\\return]\n                                       [else (r-error p \"unknown #\\\\ name\" name)])))]\n                              [else (read-char p) c]))]\n                     [(char-numeric? c)\n                      (when simple?\n                        (r-error p \"#N=/#N# notation is not allowed in this mode\"))\n                      (let loop ([l '()])\n                        (let ([c (read-char p)])\n                          (cond [(eof-object? c)\n                                 (r-error p \"end of file within a #N notation\")]\n                                [(char-numeric? c) (loop (cons c l))]\n                                [(char=? c #\\#)\n                                 (let* ([s (list->string (reverse! l))]\n                                        [n (string->number s)])\n                                   (cond [(and (fixnum? n) (assq n shared)) => cdr]\n                                         [else (r-error p \"unknown #n# reference:\" s)]))]\n                                [(char=? c #\\=)\n                                 (let* ([s (list->string (reverse! l))]\n                                        [n (string->number s)])\n                                   (cond [(not (fixnum? n))\n                                          (r-error p \"invalid #n= reference:\" s)]\n                                         [(assq n shared)\n                                          (r-error p \"duplicate #n= tag:\" n)])\n                                   (let ([loc (box #f)])\n                                     (set! shared\n                                       (cons (cons n (make-shared-ref loc)) shared))\n                                     (let ([form (sub-read-carefully p)])\n                                       (cond [(shared-ref? form)\n                                              (r-error p \"#n= has another label as target\"\n                                                s)]\n                                             [else (set-box! loc form) form]))))]\n                                [else (r-error p \"invalid terminator for #N notation\")])))]\n                     [else (r-error p \"unknown # syntax\" c)]))]\n            [else (r-error p \"illegal character read\" c)])))\n  (define (sub-read-list c p close-token dot?)\n    (let ([form (sub-read p)])\n      (if (eq? form dot)\n          (r-error p \"missing car -- ( immediately followed by .\")\n          (let recur ([form form])\n            (cond [(eof-object? form)\n                   (r-error p \"eof inside list -- unbalanced parentheses\")]\n                  [(eq? form close-token) '()]\n                  [(eq? form dot)\n                   (if dot?\n                       (let* ([last-form (sub-read-carefully p)]\n                              [another-form (sub-read p)])\n                         (if (eq? another-form close-token)\n                             last-form\n                             (r-error p \"randomness after form after dot\" another-form)))\n                       (r-error p \"dot in #(...)\"))]\n                  [(reader-token? form)\n                   (r-error p \"error inside list --\" (cdr form))]\n                  [else (cons form (recur (sub-read p)))])))))\n  (define (sub-read-numerical-list p ts)\n    (unless (eq? (read-char p) #\\()\n      (r-error p (format \"invalid ~avector syntax\" ts)))\n    (let recur ([form (sub-read p)])\n      (cond [(eof-object? form)\n             (r-error p (format \"eof inside ~avector\" ts))]\n            [(eq? form close-paren) '()]\n            [(reader-token? form)\n             (r-error p (format \"error inside ~avector --\" ts) (cdr form))]\n            [(or (and (eq? ts 'u8) (fixnum? form) (fx<=? 0 form 255))\n                 (and (eq? ts 's8) (fixnum? form) (fx<=? -128 form 127))\n                 (and (eq? ts 'u16) (fixnum? form) (fx<=? 0 form 65535))\n                 (and (eq? ts 's16) (fixnum? form) (fx<=? -32768 form 32767))\n                 (and (eq? ts 'f32) (flonum? form))\n                 (and (eq? ts 'f64) (flonum? form)))\n             (cons form (recur (sub-read p)))]\n            [else\n             (r-error p (format \"invalid ~a inside ~avector --\" ts ts)\n               form)])))\n  (define (sub-read-strsym-char-escape p what)\n    (let ([c (read-char p)])\n      (if (eof-object? c) (r-error p \"end of file within a\" what))\n      (cond [(or (char=? c #\\\\) (char=? c #\\\") (char=? c #\\|)) c]\n            [(char=? c #\\a) #\\alarm]\n            [(char=? c #\\b) #\\backspace]\n            [(char=? c #\\t) #\\tab]\n            [(char=? c #\\n) #\\newline]\n            [(char=? c #\\r) #\\return]\n            [(char=? c #\\x) (sub-read-x-char-escape p #t)]\n            [(and (eq? what 'string) (char-whitespace? c))\n             (let loop ([gotnl (char=? c #\\newline)] [nc (peek-char p)])\n               (cond [(or (eof-object? nc) (not (char-whitespace? nc)))\n                      (if gotnl #f (r-error p \"no newline in line ending escape\"))]\n                     [(and gotnl (char=? nc #\\newline)) #f]\n                     [else\n                      (read-char p)\n                      (loop (or gotnl (char=? nc #\\newline)) (peek-char p))]))]\n            [else (r-error p \"invalid char escape in\" what ': c)])))\n  (define (sub-read-x-char-escape p in-string?)\n    (define (rev-digits->char l)\n      (if (null? l)\n          (r-error p \"\\\\x escape sequence is too short\")\n          (integer->char\n            (string->fixnum (list->string (reverse! l)) 16))))\n    (let loop ([c (peek-char p)] [l '()] [cc 0])\n      (cond [(eof-object? c)\n             (if in-string?\n                 (r-error p \"end of file within a string\")\n                 (rev-digits->char l))]\n            [(and in-string? (char=? c #\\;))\n             (read-char p)\n             (rev-digits->char l)]\n            [(and (not in-string?) (char-delimiter? c))\n             (rev-digits->char l)]\n            [(not (char-hex-digit? c))\n             (r-error p \"unexpected char in \\\\x escape sequence\" c)]\n            [(> cc 2) (r-error p \"\\\\x escape sequence is too long\")]\n            [else\n             (read-char p)\n             (loop (peek-char p) (cons c l) (+ cc 1))])))\n  (let ([form (sub-read port)])\n    (if (not (reader-token? form))\n        (if (null? shared) form (patch-shared form))\n        (r-error port \"unexpected token:\" (cdr form)))))\n"
  (pp->string sexp2 pp-code #t pp-width 90 pp-brackets #t pp-max-tab 5))
(test-equal "sexp2, 15 lines"
  "(define (%read port simple? ci?)\n  (define-syntax r-error\n    (syntax-rules () [(_ p msg a ...) (read-error p msg a ...)]))\n  (define fold-case? (or ci? (port-fold-case? port)))\n  (define shared '())\n  (define (make-shared-ref loc) (lambda () (unbox loc)))\n  (define (shared-ref? form) (procedure? form))\n  (define (patch-ref! form)\n    (if (procedure? form) (patch-ref! (form)) form))\n  (define (patch-shared! form)\n    (cond [(pair? form)\n           (if (procedure? (car form))\n               (set-car! form (patch-ref! (car form)))\n               (patch-shared! (car form)))\n           (if (procedure? (cdr form)) ..)])))\n"
  (pp->string sexp2 pp-code #t pp-width 90 pp-brackets #t pp-max-tab 5 pp-lines 15))
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
(test-equal "sexp1, colors"
  "(\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1mast-pretty\x1b;[0m \x1b;[38;2;229;233;240ma\x1b;[0m . \x1b;[38;2;229;233;240mopt-port\x1b;[0m)\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1mwidth\x1b;[0m \x1b;[38;2;229;233;240mx\x1b;[0m)\n    (\x1b;[38;2;129;161;193mcond\x1b;[0m [(string? x) (+ (string-length x) \x1b;[38;2;180;142;173m2\x1b;[0m)]\n          [(char? x) \x1b;[38;2;180;142;173m3\x1b;[0m]\n          [(number? x) \x1b;[38;2;180;142;173m5\x1b;[0m]\n          [(symbol? x) (string-length (symbol->string x))]\n          [(boolean? x) \x1b;[38;2;180;142;173m2\x1b;[0m]\n          [(null? x) \x1b;[38;2;180;142;173m2\x1b;[0m]\n          [\x1b;[38;2;129;161;193melse\x1b;[0m \x1b;[38;2;180;142;173m10\x1b;[0m]))\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1mast-width\x1b;[0m \x1b;[38;2;229;233;240ma\x1b;[0m)\n    (\x1b;[38;2;129;161;193mcond\x1b;[0m [(ast-atom? a) (width (ast-atom->val a))]\n          [(ast-null? a) \x1b;[38;2;180;142;173m2\x1b;[0m]\n          [(ast-pair? a)\n           (\x1b;[38;2;129;161;193mif\x1b;[0m (ast-list? a)\n               (\x1b;[38;2;129;161;193mlet\x1b;[0m \x1b;[38;2;229;233;240mloop\x1b;[0m ([\x1b;[38;2;229;233;240ma\x1b;[0m a] [\x1b;[38;2;229;233;240msum\x1b;[0m \x1b;[38;2;180;142;173m0\x1b;[0m] [\x1b;[38;2;229;233;240mcount\x1b;[0m \x1b;[38;2;180;142;173m0\x1b;[0m])\n                 (\x1b;[38;2;129;161;193mif\x1b;[0m (ast-null? a)\n                     (+ sum count \x1b;[38;2;180;142;173m1\x1b;[0m)\n                     (loop (ast-cdr a) (+ sum (ast-width (ast-car a))) (+ count \x1b;[38;2;180;142;173m1\x1b;[0m))))\n               (+ \x1b;[38;2;180;142;173m1\x1b;[0m (ast-width (ast-car a)) \x1b;[38;2;180;142;173m3\x1b;[0m (ast-width (ast-cdr a)) \x1b;[38;2;180;142;173m1\x1b;[0m))]\n          [\x1b;[38;2;129;161;193melse\x1b;[0m \x1b;[38;2;180;142;173m0\x1b;[0m]))\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1mast-width-hello\x1b;[0m \x1b;[38;2;229;233;240ma\x1b;[0m)\n    (display \x1b;[38;2;163;190;140m\"hello!\\n\"\x1b;[0m)\n    (\x1b;[38;2;129;161;193mcond\x1b;[0m [(ast-atom? a) (width (ast-atom->val a))]\n          [(ast-null? a) \x1b;[38;2;180;142;173m2\x1b;[0m]\n          [(ast-pair? a)\n           (\x1b;[38;2;129;161;193mif\x1b;[0m (ast-list? a)\n               (\x1b;[38;2;129;161;193mlet\x1b;[0m \x1b;[38;2;229;233;240mloop\x1b;[0m ([\x1b;[38;2;229;233;240ma\x1b;[0m a] [\x1b;[38;2;229;233;240msum\x1b;[0m \x1b;[38;2;180;142;173m0\x1b;[0m] [\x1b;[38;2;229;233;240mcount\x1b;[0m \x1b;[38;2;180;142;173m0\x1b;[0m])\n                 (\x1b;[38;2;129;161;193mif\x1b;[0m (ast-null? a)\n                     (+ sum count \x1b;[38;2;180;142;173m1\x1b;[0m)\n                     (loop (ast-cdr a) (+ sum (ast-width (ast-car a))) (+ count \x1b;[38;2;180;142;173m1\x1b;[0m))))\n               (+ \x1b;[38;2;180;142;173m1\x1b;[0m (ast-width (ast-car a)) \x1b;[38;2;180;142;173m3\x1b;[0m (ast-width (ast-cdr a)) \x1b;[38;2;180;142;173m1\x1b;[0m))]\n          [\x1b;[38;2;129;161;193melse\x1b;[0m\n           (\x1b;[38;2;129;161;193mcond\x1b;[0m [(foo) \x1b;[38;2;129;161;193m=>\x1b;[0m (\x1b;[38;2;129;161;193mlambda\x1b;[0m (\x1b;[38;2;229;233;240mv\x1b;[0m) (bar v \x1b;[38;2;180;142;173m0\x1b;[0m))]\n                 [\x1b;[38;2;129;161;193melse\x1b;[0m (\x1b;[38;2;129;161;193mbegin\x1b;[0m (newline) (\x1b;[38;2;129;161;193mif\x1b;[0m a \x1b;[38;2;143;188;187m#t\x1b;[0m \x1b;[38;2;143;188;187m#f\x1b;[0m))])]))\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1mbreak?\x1b;[0m \x1b;[38;2;229;233;240mcur-col\x1b;[0m \x1b;[38;2;229;233;240mwidth\x1b;[0m \x1b;[38;2;229;233;240mmax-col\x1b;[0m)\n    (> (+ cur-col width) max-col))\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1mprint-indent\x1b;[0m \x1b;[38;2;229;233;240mn\x1b;[0m \x1b;[38;2;229;233;240mport\x1b;[0m)\n    (\x1b;[38;2;129;161;193mdo\x1b;[0m ([\x1b;[38;2;229;233;240mi\x1b;[0m \x1b;[38;2;180;142;173m0\x1b;[0m (+ i \x1b;[38;2;180;142;173m1\x1b;[0m)]) [(= i n)] (display \x1b;[38;2;163;190;140m\"  \"\x1b;[0m port)))\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1mpretty\x1b;[0m \x1b;[38;2;229;233;240ma\x1b;[0m \x1b;[38;2;229;233;240mcol\x1b;[0m \x1b;[38;2;229;233;240mindent\x1b;[0m \x1b;[38;2;229;233;240mport\x1b;[0m \x1b;[38;2;229;233;240mmax-col\x1b;[0m)\n    (\x1b;[38;2;129;161;193mcond\x1b;[0m [(ast-atom? a)\n           (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mw\x1b;[0m (width (ast-atom->val a))])\n             (write (ast-atom->val a) port)\n             (+ col w))]\n          [(ast-null? a) (display \x1b;[38;2;163;190;140m\"()\"\x1b;[0m port) (+ col \x1b;[38;2;180;142;173m2\x1b;[0m)]\n          [(ast-pair? a)\n           (\x1b;[38;2;129;161;193mif\x1b;[0m (ast-list? a)\n               (\x1b;[38;2;129;161;193mlet*\x1b;[0m ([\x1b;[38;2;229;233;240mtotal-width\x1b;[0m (ast-width a)]\n                      [\x1b;[38;2;229;233;240mremaining\x1b;[0m (- max-col col)]\n                      [\x1b;[38;2;229;233;240minline?\x1b;[0m (<= total-width remaining)])\n                 (\x1b;[38;2;129;161;193mif\x1b;[0m inline?\n                     (\x1b;[38;2;129;161;193mbegin\x1b;[0m\n                       (display \x1b;[38;2;163;190;140m\"(\"\x1b;[0m port)\n                       (\x1b;[38;2;129;161;193mlet\x1b;[0m \x1b;[38;2;229;233;240mloop\x1b;[0m ([\x1b;[38;2;229;233;240mfirst?\x1b;[0m \x1b;[38;2;143;188;187m#t\x1b;[0m] [\x1b;[38;2;229;233;240mrest\x1b;[0m a] [\x1b;[38;2;229;233;240mcur-col\x1b;[0m (+ col \x1b;[38;2;180;142;173m1\x1b;[0m)])\n                         (\x1b;[38;2;129;161;193mif\x1b;[0m (ast-null? rest)\n                             (\x1b;[38;2;129;161;193mbegin\x1b;[0m (display \x1b;[38;2;163;190;140m\")\"\x1b;[0m port) (+ cur-col \x1b;[38;2;180;142;173m1\x1b;[0m))\n                             (\x1b;[38;2;129;161;193mlet*\x1b;[0m\n                               ([\x1b;[38;2;229;233;240melem-width\x1b;[0m (ast-width (ast-car rest))]\n                                [\x1b;[38;2;229;233;240mnew-col\x1b;[0m\n                                 (\x1b;[38;2;129;161;193mif\x1b;[0m first?\n                                     (pretty (ast-car rest) cur-col indent port max-col)\n                                     (\x1b;[38;2;129;161;193mbegin\x1b;[0m\n                                       (display \x1b;[38;2;163;190;140m\" \"\x1b;[0m port)\n                                       (pretty (ast-car rest) (+ cur-col \x1b;[38;2;180;142;173m1\x1b;[0m) indent port\n                                         max-col)))])\n                               (loop \x1b;[38;2;143;188;187m#f\x1b;[0m (ast-cdr rest) new-col)))))\n                     (\x1b;[38;2;129;161;193mbegin\x1b;[0m\n                       (display \x1b;[38;2;163;190;140m\"(\"\x1b;[0m port)\n                       (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mnew-indent\x1b;[0m (+ indent \x1b;[38;2;180;142;173m1\x1b;[0m)])\n                         (\x1b;[38;2;129;161;193mlet\x1b;[0m \x1b;[38;2;229;233;240mloop\x1b;[0m ([\x1b;[38;2;229;233;240mrest\x1b;[0m a] [\x1b;[38;2;229;233;240mcur-col\x1b;[0m (* new-indent \x1b;[38;2;180;142;173m2\x1b;[0m)])\n                           (\x1b;[38;2;129;161;193mif\x1b;[0m (ast-null? rest)\n                               (\x1b;[38;2;129;161;193mbegin\x1b;[0m (display \x1b;[38;2;163;190;140m\")\"\x1b;[0m port) (+ cur-col \x1b;[38;2;180;142;173m1\x1b;[0m))\n                               (\x1b;[38;2;129;161;193mlet*\x1b;[0m\n                                 ([\x1b;[38;2;229;233;240melem\x1b;[0m (ast-car rest)]\n                                  [\x1b;[38;2;229;233;240mnext-col\x1b;[0m (pretty elem cur-col new-indent port max-col)])\n                                 (\x1b;[38;2;129;161;193munless\x1b;[0m (ast-null? (ast-cdr rest))\n                                   (newline port)\n                                   (print-indent new-indent port))\n                                 (loop (ast-cdr rest) (* new-indent \x1b;[38;2;180;142;173m2\x1b;[0m)))))))))\n               (\x1b;[38;2;129;161;193mbegin\x1b;[0m\n                 (display \x1b;[38;2;163;190;140m\"(\"\x1b;[0m port)\n                 (\x1b;[38;2;129;161;193mlet\x1b;[0m\n                   ([\x1b;[38;2;229;233;240mcar-col\x1b;[0m (pretty (ast-car a) (+ col \x1b;[38;2;180;142;173m1\x1b;[0m) indent port max-col)])\n                   (display \x1b;[38;2;163;190;140m\" . \"\x1b;[0m port)\n                   (\x1b;[38;2;129;161;193mlet\x1b;[0m\n                     ([\x1b;[38;2;229;233;240mcdr-col\x1b;[0m\n                       (pretty (ast-cdr a) (+ car-col \x1b;[38;2;180;142;173m3\x1b;[0m) indent port max-col)])\n                     (display \x1b;[38;2;163;190;140m\")\"\x1b;[0m port)\n                     (+ cdr-col \x1b;[38;2;180;142;173m1\x1b;[0m)))))]))\n  (\x1b;[38;2;129;161;193mlet\x1b;[0m\n    ([\x1b;[38;2;229;233;240mport\x1b;[0m\n      (\x1b;[38;2;129;161;193mif\x1b;[0m (null? opt-port) (current-output-port) (car opt-port))])\n    (pretty a \x1b;[38;2;180;142;173m0\x1b;[0m \x1b;[38;2;180;142;173m0\x1b;[0m port \x1b;[38;2;180;142;173m80\x1b;[0m)\n    (newline port)))\n"
  (parameterize ((sgr-support-tier 2))
      (pp->string sexp1 pp-code #t pp-width 90 pp-brackets #t pp-max-tab 5 pp-color #t)))
(test-equal "sexp2, colors"
  "(\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1m%read\x1b;[0m \x1b;[38;2;229;233;240mport\x1b;[0m \x1b;[38;2;229;233;240msimple?\x1b;[0m \x1b;[38;2;229;233;240mci?\x1b;[0m)\n  (\x1b;[38;2;129;161;193mdefine-syntax\x1b;[0m \x1b;[1mr-error\x1b;[0m\n    (\x1b;[38;2;129;161;193msyntax-rules\x1b;[0m () [(_ p msg a ...) (read-error p msg a ...)]))\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m \x1b;[1mfold-case?\x1b;[0m (\x1b;[38;2;129;161;193mor\x1b;[0m ci? (port-fold-case? port)))\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m \x1b;[1mshared\x1b;[0m \x1b;[38;2;143;188;187m'()\x1b;[0m)\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1mmake-shared-ref\x1b;[0m \x1b;[38;2;229;233;240mloc\x1b;[0m) (\x1b;[38;2;129;161;193mlambda\x1b;[0m () (unbox loc)))\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1mshared-ref?\x1b;[0m \x1b;[38;2;229;233;240mform\x1b;[0m) (procedure? form))\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1mpatch-ref!\x1b;[0m \x1b;[38;2;229;233;240mform\x1b;[0m)\n    (\x1b;[38;2;129;161;193mif\x1b;[0m (procedure? form) (patch-ref! (form)) form))\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1mpatch-shared!\x1b;[0m \x1b;[38;2;229;233;240mform\x1b;[0m)\n    (\x1b;[38;2;129;161;193mcond\x1b;[0m [(pair? form)\n           (\x1b;[38;2;129;161;193mif\x1b;[0m (procedure? (car form))\n               (set-car! form (patch-ref! (car form)))\n               (patch-shared! (car form)))\n           (\x1b;[38;2;129;161;193mif\x1b;[0m (procedure? (cdr form))\n               (set-cdr! form (patch-ref! (cdr form)))\n               (patch-shared! (cdr form)))]\n          [(vector? form)\n           (\x1b;[38;2;129;161;193mlet\x1b;[0m \x1b;[38;2;229;233;240mloop\x1b;[0m ([\x1b;[38;2;229;233;240mi\x1b;[0m \x1b;[38;2;180;142;173m0\x1b;[0m])\n             (\x1b;[38;2;129;161;193mwhen\x1b;[0m (fx<? i (vector-length form))\n               (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mfi\x1b;[0m (vector-ref form i)])\n                 (\x1b;[38;2;129;161;193mif\x1b;[0m (procedure? fi)\n                     (vector-set! form i (patch-ref! fi))\n                     (patch-shared! fi)))\n               (loop (fx+ i \x1b;[38;2;180;142;173m1\x1b;[0m))))]\n          [(box? form)\n           (\x1b;[38;2;129;161;193mif\x1b;[0m (procedure? (unbox form))\n               (set-box! form (patch-shared! (unbox form)))\n               (patch-shared! (unbox form)))]))\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1mpatch-shared\x1b;[0m \x1b;[38;2;229;233;240mform\x1b;[0m) (patch-shared! form) form)\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m \x1b;[1mreader-token-marker\x1b;[0m \x1b;[38;2;143;188;187m#f\x1b;[0m)\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m \x1b;[1mclose-paren\x1b;[0m \x1b;[38;2;143;188;187m#f\x1b;[0m)\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m \x1b;[1mclose-bracket\x1b;[0m \x1b;[38;2;143;188;187m#f\x1b;[0m)\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m \x1b;[1mdot\x1b;[0m \x1b;[38;2;143;188;187m#f\x1b;[0m)\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m ()\n    (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mrtm\x1b;[0m (list \x1b;[38;2;143;188;187m'reader-token\x1b;[0m)])\n      (\x1b;[38;2;129;161;193mset!\x1b;[0m reader-token-marker rtm)\n      (\x1b;[38;2;129;161;193mset!\x1b;[0m close-paren (cons rtm \x1b;[38;2;163;190;140m\"right parenthesis\"\x1b;[0m))\n      (\x1b;[38;2;129;161;193mset!\x1b;[0m close-bracket (cons rtm \x1b;[38;2;163;190;140m\"right bracket\"\x1b;[0m))\n      (\x1b;[38;2;129;161;193mset!\x1b;[0m dot (cons rtm \x1b;[38;2;163;190;140m\"\\\" . \\\"\"\x1b;[0m))))\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1mreader-token?\x1b;[0m \x1b;[38;2;229;233;240mform\x1b;[0m)\n    (\x1b;[38;2;129;161;193mand\x1b;[0m (pair? form) (eq? (car form) reader-token-marker)))\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1mchar-hex-digit?\x1b;[0m \x1b;[38;2;229;233;240mc\x1b;[0m)\n    (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mscalar-value\x1b;[0m (char->integer c)])\n      (\x1b;[38;2;129;161;193mor\x1b;[0m (\x1b;[38;2;129;161;193mand\x1b;[0m (>= scalar-value \x1b;[38;2;180;142;173m48\x1b;[0m) (<= scalar-value \x1b;[38;2;180;142;173m57\x1b;[0m))\n          (\x1b;[38;2;129;161;193mand\x1b;[0m (>= scalar-value \x1b;[38;2;180;142;173m65\x1b;[0m) (<= scalar-value \x1b;[38;2;180;142;173m70\x1b;[0m))\n          (\x1b;[38;2;129;161;193mand\x1b;[0m (>= scalar-value \x1b;[38;2;180;142;173m97\x1b;[0m) (<= scalar-value \x1b;[38;2;180;142;173m102\x1b;[0m)))))\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1mchar-delimiter?\x1b;[0m \x1b;[38;2;229;233;240mc\x1b;[0m)\n    (\x1b;[38;2;129;161;193mor\x1b;[0m (char-whitespace? c) (char=? c \x1b;[38;2;163;190;140m#\\)\x1b;[0m) (char=? c \x1b;[38;2;163;190;140m#\\(\x1b;[0m)\n        (char=? c \x1b;[38;2;163;190;140m#\\]\x1b;[0m) (char=? c \x1b;[38;2;163;190;140m#\\[\x1b;[0m) (char=? c \x1b;[38;2;163;190;140m#\\\"\x1b;[0m) (char=? c \x1b;[38;2;163;190;140m#\\|\x1b;[0m)\n        (char=? c \x1b;[38;2;163;190;140m#\\;\x1b;[0m)))\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1msub-read-carefully\x1b;[0m \x1b;[38;2;229;233;240mp\x1b;[0m)\n    (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mform\x1b;[0m (sub-read p)])\n      (\x1b;[38;2;129;161;193mcond\x1b;[0m [(eof-object? form) (r-error p \x1b;[38;2;163;190;140m\"unexpected end of file\"\x1b;[0m)]\n            [(reader-token? form)\n             (r-error p \x1b;[38;2;163;190;140m\"unexpected token:\"\x1b;[0m (cdr form))]\n            [\x1b;[38;2;129;161;193melse\x1b;[0m form])))\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1msub-read-shebang\x1b;[0m \x1b;[38;2;229;233;240mp\x1b;[0m)\n    (\x1b;[38;2;129;161;193mif\x1b;[0m (eqv? (peek-char p) \x1b;[38;2;163;190;140m#\\space\x1b;[0m)\n        (string->symbol (string-trim-whitespace (read-line p)))\n        (sub-read-carefully p)))\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1msub-read\x1b;[0m \x1b;[38;2;229;233;240mp\x1b;[0m)\n    (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mc\x1b;[0m (%read-ahead fold-case? p)])\n      (\x1b;[38;2;129;161;193mcond\x1b;[0m [(not (char? c)) c]\n            [(char=? c \x1b;[38;2;163;190;140m#\\(\x1b;[0m) (sub-read-list c p close-paren \x1b;[38;2;143;188;187m#t\x1b;[0m)]\n            [(char=? c \x1b;[38;2;163;190;140m#\\)\x1b;[0m) close-paren]\n            [(char=? c \x1b;[38;2;163;190;140m#\\[\x1b;[0m) (sub-read-list c p close-bracket \x1b;[38;2;143;188;187m#t\x1b;[0m)]\n            [(char=? c \x1b;[38;2;163;190;140m#\\]\x1b;[0m) close-bracket]\n            [(char=? c \x1b;[38;2;163;190;140m#\\.\x1b;[0m) dot]\n            [(char=? c \x1b;[38;2;163;190;140m#\\'\x1b;[0m) (list \x1b;[38;2;143;188;187m'quote\x1b;[0m (sub-read-carefully p))]\n            [(char=? c \x1b;[38;2;163;190;140m#\\`\x1b;[0m) (list \x1b;[38;2;143;188;187m'quasiquote\x1b;[0m (sub-read-carefully p))]\n            [(char=? c \x1b;[38;2;163;190;140m#\\n\x1b;[0m)\n             (r-error p\n               \x1b;[38;2;163;190;140m\"unsupported number syntax (implementation restriction)\"\x1b;[0m)]\n            [(char=? c \x1b;[38;2;163;190;140m#\\d\x1b;[0m) (r-error p \x1b;[38;2;163;190;140m\"invalid delimiter\"\x1b;[0m)]\n            [(char=? c \x1b;[38;2;163;190;140m#\\y\x1b;[0m) (r-error p \x1b;[38;2;163;190;140m\"unsupported symbol syntax\"\x1b;[0m)]\n            [(char=? c \x1b;[38;2;163;190;140m#\\z\x1b;[0m) (r-error p \x1b;[38;2;163;190;140m\"invalid token\"\x1b;[0m)]\n            [(char=? c \x1b;[38;2;163;190;140m#\\,\x1b;[0m)\n             (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mnext\x1b;[0m (peek-char p)])\n               (\x1b;[38;2;129;161;193mcond\x1b;[0m [(eof-object? next) (r-error p \x1b;[38;2;163;190;140m\"end of file after ,\"\x1b;[0m)]\n                     [(char=? next \x1b;[38;2;163;190;140m#\\@\x1b;[0m)\n                      (read-char p)\n                      (list \x1b;[38;2;143;188;187m'unquote-splicing\x1b;[0m (sub-read-carefully p))]\n                     [\x1b;[38;2;129;161;193melse\x1b;[0m (list \x1b;[38;2;143;188;187m'unquote\x1b;[0m (sub-read-carefully p))]))]\n            [(char=? c \x1b;[38;2;163;190;140m#\\\"\x1b;[0m)\n             (\x1b;[38;2;129;161;193mlet\x1b;[0m \x1b;[38;2;229;233;240mloop\x1b;[0m ([\x1b;[38;2;229;233;240ml\x1b;[0m \x1b;[38;2;143;188;187m'()\x1b;[0m])\n               (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mc\x1b;[0m (read-char p)])\n                 (\x1b;[38;2;129;161;193mcond\x1b;[0m [(eof-object? c) (r-error p \x1b;[38;2;163;190;140m\"end of file within a string\"\x1b;[0m)]\n                       [(char=? c \x1b;[38;2;163;190;140m#\\\\\x1b;[0m)\n                        (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240me\x1b;[0m (sub-read-strsym-char-escape p \x1b;[38;2;143;188;187m'string\x1b;[0m)])\n                          (loop (\x1b;[38;2;129;161;193mif\x1b;[0m e (cons e l) l)))]\n                       [(char=? c \x1b;[38;2;163;190;140m#\\\"\x1b;[0m) (list->string (reverse! l))]\n                       [\x1b;[38;2;129;161;193melse\x1b;[0m (loop (cons c l))])))]\n            [(char=? c \x1b;[38;2;163;190;140m#\\|\x1b;[0m)\n             (\x1b;[38;2;129;161;193mlet\x1b;[0m \x1b;[38;2;229;233;240mloop\x1b;[0m ([\x1b;[38;2;229;233;240ml\x1b;[0m \x1b;[38;2;143;188;187m'()\x1b;[0m])\n               (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mc\x1b;[0m (read-char p)])\n                 (\x1b;[38;2;129;161;193mcond\x1b;[0m [(eof-object? c)\n                        (r-error p \x1b;[38;2;163;190;140m\"end of file within a |symbol|\"\x1b;[0m)]\n                       [(char=? c \x1b;[38;2;163;190;140m#\\\\\x1b;[0m)\n                        (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240me\x1b;[0m (sub-read-strsym-char-escape p \x1b;[38;2;143;188;187m'symbol\x1b;[0m)])\n                          (loop (\x1b;[38;2;129;161;193mif\x1b;[0m e (cons e l) l)))]\n                       [(char=? c \x1b;[38;2;163;190;140m#\\|\x1b;[0m)\n                        (string->symbol (list->string (reverse! l)))]\n                       [\x1b;[38;2;129;161;193melse\x1b;[0m (loop (cons c l))])))]\n            [(char=? c \x1b;[38;2;163;190;140m#\\#\x1b;[0m)\n             (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mc\x1b;[0m (peek-char p)])\n               (\x1b;[38;2;129;161;193mcond\x1b;[0m [(eof-object? c) (r-error p \x1b;[38;2;163;190;140m\"end of file after #\"\x1b;[0m)]\n                     [(char=? c \x1b;[38;2;163;190;140m#\\!\x1b;[0m)\n                      (read-char p)\n                      (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mname\x1b;[0m (sub-read-shebang p)])\n                        (\x1b;[38;2;129;161;193mcase\x1b;[0m name\n                          [(\x1b;[38;2;143;188;187mfold-case\x1b;[0m \x1b;[38;2;143;188;187mno-fold-case\x1b;[0m)\n                           (\x1b;[38;2;129;161;193mset!\x1b;[0m fold-case? (eq? name \x1b;[38;2;143;188;187m'fold-case\x1b;[0m))\n                           (set-port-fold-case! p fold-case?)\n                           (sub-read p)]\n                          [\x1b;[38;2;129;161;193melse\x1b;[0m\n                           (\x1b;[38;2;129;161;193mif\x1b;[0m (symbol? name)\n                               (symbol->shebang name)\n                               (r-error p \x1b;[38;2;163;190;140m\"unexpected name after #!\"\x1b;[0m name))]))]\n                     [(\x1b;[38;2;129;161;193mor\x1b;[0m (char-ci=? c \x1b;[38;2;163;190;140m#\\t\x1b;[0m) (char-ci=? c \x1b;[38;2;163;190;140m#\\f\x1b;[0m) (char-ci=? c \x1b;[38;2;163;190;140m#\\s\x1b;[0m)\n                          (char-ci=? c \x1b;[38;2;163;190;140m#\\u\x1b;[0m))\n                      (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mname\x1b;[0m (sub-read-carefully p)])\n                        (\x1b;[38;2;129;161;193mcase\x1b;[0m name\n                          [(\x1b;[38;2;143;188;187mt\x1b;[0m \x1b;[38;2;143;188;187mtrue\x1b;[0m) \x1b;[38;2;143;188;187m#t\x1b;[0m]\n                          [(\x1b;[38;2;143;188;187mf\x1b;[0m \x1b;[38;2;143;188;187mfalse\x1b;[0m) \x1b;[38;2;143;188;187m#f\x1b;[0m]\n                          [(\x1b;[38;2;143;188;187mu8\x1b;[0m) (list->numvector (sub-read-numerical-list p name) \x1b;[38;2;180;142;173m0\x1b;[0m)]\n                          [(\x1b;[38;2;143;188;187ms8\x1b;[0m) (list->numvector (sub-read-numerical-list p name) \x1b;[38;2;180;142;173m1\x1b;[0m)]\n                          [(\x1b;[38;2;143;188;187mu16\x1b;[0m) (list->numvector (sub-read-numerical-list p name) \x1b;[38;2;180;142;173m2\x1b;[0m)]\n                          [(\x1b;[38;2;143;188;187ms16\x1b;[0m) (list->numvector (sub-read-numerical-list p name) \x1b;[38;2;180;142;173m3\x1b;[0m)]\n                          [(\x1b;[38;2;143;188;187mf32\x1b;[0m)\n                           (list->numvector (sub-read-numerical-list p name) \x1b;[38;2;180;142;173m10\x1b;[0m)]\n                          [(\x1b;[38;2;143;188;187mf64\x1b;[0m)\n                           (list->numvector (sub-read-numerical-list p name) \x1b;[38;2;180;142;173m11\x1b;[0m)]\n                          [\x1b;[38;2;129;161;193melse\x1b;[0m (r-error p \x1b;[38;2;163;190;140m\"unexpected name after #\"\x1b;[0m name)]))]\n                     [(char=? c \x1b;[38;2;163;190;140m#\\&\x1b;[0m) (read-char p) (box (sub-read-carefully p))]\n                     [(char=? c \x1b;[38;2;163;190;140m#\\;\x1b;[0m)\n                      (read-char p)\n                      (sub-read-carefully p)\n                      (sub-read p)]\n                     [(char=? c \x1b;[38;2;163;190;140m#\\|\x1b;[0m)\n                      (read-char p)\n                      (\x1b;[38;2;129;161;193mlet\x1b;[0m \x1b;[38;2;229;233;240mrecur\x1b;[0m ()\n                        (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mnext\x1b;[0m (read-char p)])\n                          (\x1b;[38;2;129;161;193mcond\x1b;[0m [(eof-object? next)\n                                 (r-error p \x1b;[38;2;163;190;140m\"end of file in #| comment\"\x1b;[0m)]\n                                [(char=? next \x1b;[38;2;163;190;140m#\\|\x1b;[0m)\n                                 (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mnext\x1b;[0m (peek-char p)])\n                                   (\x1b;[38;2;129;161;193mcond\x1b;[0m [(eof-object? next)\n                                          (r-error p \x1b;[38;2;163;190;140m\"end of file in #| comment\"\x1b;[0m)]\n                                         [(char=? next \x1b;[38;2;163;190;140m#\\#\x1b;[0m) (read-char p)]\n                                         [\x1b;[38;2;129;161;193melse\x1b;[0m (recur)]))]\n                                [(char=? next \x1b;[38;2;163;190;140m#\\#\x1b;[0m)\n                                 (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mnext\x1b;[0m (peek-char p)])\n                                   (\x1b;[38;2;129;161;193mcond\x1b;[0m [(eof-object? next)\n                                          (r-error p \x1b;[38;2;163;190;140m\"end of file in #| comment\"\x1b;[0m)]\n                                         [(char=? next \x1b;[38;2;163;190;140m#\\|\x1b;[0m) (read-char p) (recur) (recur)]\n                                         [\x1b;[38;2;129;161;193melse\x1b;[0m (recur)]))]\n                                [\x1b;[38;2;129;161;193melse\x1b;[0m (recur)])))\n                      (sub-read p)]\n                     [(char=? c \x1b;[38;2;163;190;140m#\\(\x1b;[0m)\n                      (read-char p)\n                      (list->vector (sub-read-list c p close-paren \x1b;[38;2;143;188;187m#f\x1b;[0m))]\n                     [(char=? c \x1b;[38;2;163;190;140m#\\\\\x1b;[0m)\n                      (read-char p)\n                      (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mc\x1b;[0m (peek-char p)])\n                        (\x1b;[38;2;129;161;193mcond\x1b;[0m [(eof-object? c) (r-error p \x1b;[38;2;163;190;140m\"end of file after #\\\\\"\x1b;[0m)]\n                              [(char=? \x1b;[38;2;163;190;140m#\\x\x1b;[0m c)\n                               (read-char p)\n                               (\x1b;[38;2;129;161;193mif\x1b;[0m (char-delimiter? (peek-char p))\n                                   c\n                                   (sub-read-x-char-escape p \x1b;[38;2;143;188;187m#f\x1b;[0m))]\n                              [(char-alphabetic? c)\n                               (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mname\x1b;[0m (sub-read-carefully p)])\n                                 (\x1b;[38;2;129;161;193mif\x1b;[0m (= (string-length (symbol->string name)) \x1b;[38;2;180;142;173m1\x1b;[0m)\n                                     c\n                                     (\x1b;[38;2;129;161;193mcase\x1b;[0m name\n                                       [(\x1b;[38;2;143;188;187mnull\x1b;[0m) (integer->char \x1b;[38;2;180;142;173m0\x1b;[0m)]\n                                       [(\x1b;[38;2;143;188;187mspace\x1b;[0m) \x1b;[38;2;163;190;140m#\\space\x1b;[0m]\n                                       [(\x1b;[38;2;143;188;187mbackspace\x1b;[0m) \x1b;[38;2;163;190;140m#\\backspace\x1b;[0m]\n                                       [(\x1b;[38;2;143;188;187mtab\x1b;[0m) \x1b;[38;2;163;190;140m#\\tab\x1b;[0m]\n                                       [(\x1b;[38;2;143;188;187mnewline\x1b;[0m \x1b;[38;2;143;188;187mlinefeed\x1b;[0m) \x1b;[38;2;163;190;140m#\\newline\x1b;[0m]\n                                       [(\x1b;[38;2;143;188;187mreturn\x1b;[0m) \x1b;[38;2;163;190;140m#\\return\x1b;[0m]\n                                       [\x1b;[38;2;129;161;193melse\x1b;[0m (r-error p \x1b;[38;2;163;190;140m\"unknown #\\\\ name\"\x1b;[0m name)])))]\n                              [\x1b;[38;2;129;161;193melse\x1b;[0m (read-char p) c]))]\n                     [(char-numeric? c)\n                      (\x1b;[38;2;129;161;193mwhen\x1b;[0m simple?\n                        (r-error p \x1b;[38;2;163;190;140m\"#N=/#N# notation is not allowed in this mode\"\x1b;[0m))\n                      (\x1b;[38;2;129;161;193mlet\x1b;[0m \x1b;[38;2;229;233;240mloop\x1b;[0m ([\x1b;[38;2;229;233;240ml\x1b;[0m \x1b;[38;2;143;188;187m'()\x1b;[0m])\n                        (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mc\x1b;[0m (read-char p)])\n                          (\x1b;[38;2;129;161;193mcond\x1b;[0m [(eof-object? c)\n                                 (r-error p \x1b;[38;2;163;190;140m\"end of file within a #N notation\"\x1b;[0m)]\n                                [(char-numeric? c) (loop (cons c l))]\n                                [(char=? c \x1b;[38;2;163;190;140m#\\#\x1b;[0m)\n                                 (\x1b;[38;2;129;161;193mlet*\x1b;[0m ([\x1b;[38;2;229;233;240ms\x1b;[0m (list->string (reverse! l))]\n                                        [\x1b;[38;2;229;233;240mn\x1b;[0m (string->number s)])\n                                   (\x1b;[38;2;129;161;193mcond\x1b;[0m [(\x1b;[38;2;129;161;193mand\x1b;[0m (fixnum? n) (assq n shared)) \x1b;[38;2;129;161;193m=>\x1b;[0m cdr]\n                                         [\x1b;[38;2;129;161;193melse\x1b;[0m (r-error p \x1b;[38;2;163;190;140m\"unknown #n# reference:\"\x1b;[0m s)]))]\n                                [(char=? c \x1b;[38;2;163;190;140m#\\=\x1b;[0m)\n                                 (\x1b;[38;2;129;161;193mlet*\x1b;[0m ([\x1b;[38;2;229;233;240ms\x1b;[0m (list->string (reverse! l))]\n                                        [\x1b;[38;2;229;233;240mn\x1b;[0m (string->number s)])\n                                   (\x1b;[38;2;129;161;193mcond\x1b;[0m [(not (fixnum? n))\n                                          (r-error p \x1b;[38;2;163;190;140m\"invalid #n= reference:\"\x1b;[0m s)]\n                                         [(assq n shared)\n                                          (r-error p \x1b;[38;2;163;190;140m\"duplicate #n= tag:\"\x1b;[0m n)])\n                                   (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mloc\x1b;[0m (box \x1b;[38;2;143;188;187m#f\x1b;[0m)])\n                                     (\x1b;[38;2;129;161;193mset!\x1b;[0m shared\n                                       (cons (cons n (make-shared-ref loc)) shared))\n                                     (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mform\x1b;[0m (sub-read-carefully p)])\n                                       (\x1b;[38;2;129;161;193mcond\x1b;[0m [(shared-ref? form)\n                                              (r-error p \x1b;[38;2;163;190;140m\"#n= has another label as target\"\x1b;[0m\n                                                s)]\n                                             [\x1b;[38;2;129;161;193melse\x1b;[0m (set-box! loc form) form]))))]\n                                [\x1b;[38;2;129;161;193melse\x1b;[0m (r-error p \x1b;[38;2;163;190;140m\"invalid terminator for #N notation\"\x1b;[0m)])))]\n                     [\x1b;[38;2;129;161;193melse\x1b;[0m (r-error p \x1b;[38;2;163;190;140m\"unknown # syntax\"\x1b;[0m c)]))]\n            [\x1b;[38;2;129;161;193melse\x1b;[0m (r-error p \x1b;[38;2;163;190;140m\"illegal character read\"\x1b;[0m c)])))\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1msub-read-list\x1b;[0m \x1b;[38;2;229;233;240mc\x1b;[0m \x1b;[38;2;229;233;240mp\x1b;[0m \x1b;[38;2;229;233;240mclose-token\x1b;[0m \x1b;[38;2;229;233;240mdot?\x1b;[0m)\n    (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mform\x1b;[0m (sub-read p)])\n      (\x1b;[38;2;129;161;193mif\x1b;[0m (eq? form dot)\n          (r-error p \x1b;[38;2;163;190;140m\"missing car -- ( immediately followed by .\"\x1b;[0m)\n          (\x1b;[38;2;129;161;193mlet\x1b;[0m \x1b;[38;2;229;233;240mrecur\x1b;[0m ([\x1b;[38;2;229;233;240mform\x1b;[0m form])\n            (\x1b;[38;2;129;161;193mcond\x1b;[0m [(eof-object? form)\n                   (r-error p \x1b;[38;2;163;190;140m\"eof inside list -- unbalanced parentheses\"\x1b;[0m)]\n                  [(eq? form close-token) \x1b;[38;2;143;188;187m'()\x1b;[0m]\n                  [(eq? form dot)\n                   (\x1b;[38;2;129;161;193mif\x1b;[0m dot?\n                       (\x1b;[38;2;129;161;193mlet*\x1b;[0m ([\x1b;[38;2;229;233;240mlast-form\x1b;[0m (sub-read-carefully p)]\n                              [\x1b;[38;2;229;233;240manother-form\x1b;[0m (sub-read p)])\n                         (\x1b;[38;2;129;161;193mif\x1b;[0m (eq? another-form close-token)\n                             last-form\n                             (r-error p \x1b;[38;2;163;190;140m\"randomness after form after dot\"\x1b;[0m another-form)))\n                       (r-error p \x1b;[38;2;163;190;140m\"dot in #(...)\"\x1b;[0m))]\n                  [(reader-token? form)\n                   (r-error p \x1b;[38;2;163;190;140m\"error inside list --\"\x1b;[0m (cdr form))]\n                  [\x1b;[38;2;129;161;193melse\x1b;[0m (cons form (recur (sub-read p)))])))))\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1msub-read-numerical-list\x1b;[0m \x1b;[38;2;229;233;240mp\x1b;[0m \x1b;[38;2;229;233;240mts\x1b;[0m)\n    (\x1b;[38;2;129;161;193munless\x1b;[0m (eq? (read-char p) \x1b;[38;2;163;190;140m#\\(\x1b;[0m)\n      (r-error p (format \x1b;[38;2;163;190;140m\"invalid ~avector syntax\"\x1b;[0m ts)))\n    (\x1b;[38;2;129;161;193mlet\x1b;[0m \x1b;[38;2;229;233;240mrecur\x1b;[0m ([\x1b;[38;2;229;233;240mform\x1b;[0m (sub-read p)])\n      (\x1b;[38;2;129;161;193mcond\x1b;[0m [(eof-object? form)\n             (r-error p (format \x1b;[38;2;163;190;140m\"eof inside ~avector\"\x1b;[0m ts))]\n            [(eq? form close-paren) \x1b;[38;2;143;188;187m'()\x1b;[0m]\n            [(reader-token? form)\n             (r-error p (format \x1b;[38;2;163;190;140m\"error inside ~avector --\"\x1b;[0m ts) (cdr form))]\n            [(\x1b;[38;2;129;161;193mor\x1b;[0m (\x1b;[38;2;129;161;193mand\x1b;[0m (eq? ts \x1b;[38;2;143;188;187m'u8\x1b;[0m) (fixnum? form) (fx<=? \x1b;[38;2;180;142;173m0\x1b;[0m form \x1b;[38;2;180;142;173m255\x1b;[0m))\n                 (\x1b;[38;2;129;161;193mand\x1b;[0m (eq? ts \x1b;[38;2;143;188;187m's8\x1b;[0m) (fixnum? form) (fx<=? \x1b;[38;2;180;142;173m-128\x1b;[0m form \x1b;[38;2;180;142;173m127\x1b;[0m))\n                 (\x1b;[38;2;129;161;193mand\x1b;[0m (eq? ts \x1b;[38;2;143;188;187m'u16\x1b;[0m) (fixnum? form) (fx<=? \x1b;[38;2;180;142;173m0\x1b;[0m form \x1b;[38;2;180;142;173m65535\x1b;[0m))\n                 (\x1b;[38;2;129;161;193mand\x1b;[0m (eq? ts \x1b;[38;2;143;188;187m's16\x1b;[0m) (fixnum? form) (fx<=? \x1b;[38;2;180;142;173m-32768\x1b;[0m form \x1b;[38;2;180;142;173m32767\x1b;[0m))\n                 (\x1b;[38;2;129;161;193mand\x1b;[0m (eq? ts \x1b;[38;2;143;188;187m'f32\x1b;[0m) (flonum? form))\n                 (\x1b;[38;2;129;161;193mand\x1b;[0m (eq? ts \x1b;[38;2;143;188;187m'f64\x1b;[0m) (flonum? form)))\n             (cons form (recur (sub-read p)))]\n            [\x1b;[38;2;129;161;193melse\x1b;[0m\n             (r-error p (format \x1b;[38;2;163;190;140m\"invalid ~a inside ~avector --\"\x1b;[0m ts ts)\n               form)])))\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1msub-read-strsym-char-escape\x1b;[0m \x1b;[38;2;229;233;240mp\x1b;[0m \x1b;[38;2;229;233;240mwhat\x1b;[0m)\n    (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mc\x1b;[0m (read-char p)])\n      (\x1b;[38;2;129;161;193mif\x1b;[0m (eof-object? c) (r-error p \x1b;[38;2;163;190;140m\"end of file within a\"\x1b;[0m what))\n      (\x1b;[38;2;129;161;193mcond\x1b;[0m [(\x1b;[38;2;129;161;193mor\x1b;[0m (char=? c \x1b;[38;2;163;190;140m#\\\\\x1b;[0m) (char=? c \x1b;[38;2;163;190;140m#\\\"\x1b;[0m) (char=? c \x1b;[38;2;163;190;140m#\\|\x1b;[0m)) c]\n            [(char=? c \x1b;[38;2;163;190;140m#\\a\x1b;[0m) \x1b;[38;2;163;190;140m#\\alarm\x1b;[0m]\n            [(char=? c \x1b;[38;2;163;190;140m#\\b\x1b;[0m) \x1b;[38;2;163;190;140m#\\backspace\x1b;[0m]\n            [(char=? c \x1b;[38;2;163;190;140m#\\t\x1b;[0m) \x1b;[38;2;163;190;140m#\\tab\x1b;[0m]\n            [(char=? c \x1b;[38;2;163;190;140m#\\n\x1b;[0m) \x1b;[38;2;163;190;140m#\\newline\x1b;[0m]\n            [(char=? c \x1b;[38;2;163;190;140m#\\r\x1b;[0m) \x1b;[38;2;163;190;140m#\\return\x1b;[0m]\n            [(char=? c \x1b;[38;2;163;190;140m#\\x\x1b;[0m) (sub-read-x-char-escape p \x1b;[38;2;143;188;187m#t\x1b;[0m)]\n            [(\x1b;[38;2;129;161;193mand\x1b;[0m (eq? what \x1b;[38;2;143;188;187m'string\x1b;[0m) (char-whitespace? c))\n             (\x1b;[38;2;129;161;193mlet\x1b;[0m \x1b;[38;2;229;233;240mloop\x1b;[0m ([\x1b;[38;2;229;233;240mgotnl\x1b;[0m (char=? c \x1b;[38;2;163;190;140m#\\newline\x1b;[0m)] [\x1b;[38;2;229;233;240mnc\x1b;[0m (peek-char p)])\n               (\x1b;[38;2;129;161;193mcond\x1b;[0m [(\x1b;[38;2;129;161;193mor\x1b;[0m (eof-object? nc) (not (char-whitespace? nc)))\n                      (\x1b;[38;2;129;161;193mif\x1b;[0m gotnl \x1b;[38;2;143;188;187m#f\x1b;[0m (r-error p \x1b;[38;2;163;190;140m\"no newline in line ending escape\"\x1b;[0m))]\n                     [(\x1b;[38;2;129;161;193mand\x1b;[0m gotnl (char=? nc \x1b;[38;2;163;190;140m#\\newline\x1b;[0m)) \x1b;[38;2;143;188;187m#f\x1b;[0m]\n                     [\x1b;[38;2;129;161;193melse\x1b;[0m\n                      (read-char p)\n                      (loop (\x1b;[38;2;129;161;193mor\x1b;[0m gotnl (char=? nc \x1b;[38;2;163;190;140m#\\newline\x1b;[0m)) (peek-char p))]))]\n            [\x1b;[38;2;129;161;193melse\x1b;[0m (r-error p \x1b;[38;2;163;190;140m\"invalid char escape in\"\x1b;[0m what \x1b;[38;2;143;188;187m':\x1b;[0m c)])))\n  (\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1msub-read-x-char-escape\x1b;[0m \x1b;[38;2;229;233;240mp\x1b;[0m \x1b;[38;2;229;233;240min-string?\x1b;[0m)\n    (\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1mrev-digits->char\x1b;[0m \x1b;[38;2;229;233;240ml\x1b;[0m)\n      (\x1b;[38;2;129;161;193mif\x1b;[0m (null? l)\n          (r-error p \x1b;[38;2;163;190;140m\"\\\\x escape sequence is too short\"\x1b;[0m)\n          (integer->char\n            (string->fixnum (list->string (reverse! l)) \x1b;[38;2;180;142;173m16\x1b;[0m))))\n    (\x1b;[38;2;129;161;193mlet\x1b;[0m \x1b;[38;2;229;233;240mloop\x1b;[0m ([\x1b;[38;2;229;233;240mc\x1b;[0m (peek-char p)] [\x1b;[38;2;229;233;240ml\x1b;[0m \x1b;[38;2;143;188;187m'()\x1b;[0m] [\x1b;[38;2;229;233;240mcc\x1b;[0m \x1b;[38;2;180;142;173m0\x1b;[0m])\n      (\x1b;[38;2;129;161;193mcond\x1b;[0m [(eof-object? c)\n             (\x1b;[38;2;129;161;193mif\x1b;[0m in-string?\n                 (r-error p \x1b;[38;2;163;190;140m\"end of file within a string\"\x1b;[0m)\n                 (rev-digits->char l))]\n            [(\x1b;[38;2;129;161;193mand\x1b;[0m in-string? (char=? c \x1b;[38;2;163;190;140m#\\;\x1b;[0m))\n             (read-char p)\n             (rev-digits->char l)]\n            [(\x1b;[38;2;129;161;193mand\x1b;[0m (not in-string?) (char-delimiter? c))\n             (rev-digits->char l)]\n            [(not (char-hex-digit? c))\n             (r-error p \x1b;[38;2;163;190;140m\"unexpected char in \\\\x escape sequence\"\x1b;[0m c)]\n            [(> cc \x1b;[38;2;180;142;173m2\x1b;[0m) (r-error p \x1b;[38;2;163;190;140m\"\\\\x escape sequence is too long\"\x1b;[0m)]\n            [\x1b;[38;2;129;161;193melse\x1b;[0m\n             (read-char p)\n             (loop (peek-char p) (cons c l) (+ cc \x1b;[38;2;180;142;173m1\x1b;[0m))])))\n  (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mform\x1b;[0m (sub-read port)])\n    (\x1b;[38;2;129;161;193mif\x1b;[0m (not (reader-token? form))\n        (\x1b;[38;2;129;161;193mif\x1b;[0m (null? shared) form (patch-shared form))\n        (r-error port \x1b;[38;2;163;190;140m\"unexpected token:\"\x1b;[0m (cdr form)))))\n"
  (parameterize ((sgr-support-tier 2))
      (pp->string sexp2 pp-code #t pp-width 90 pp-brackets #t pp-max-tab 5 pp-color #t)))
(test-equal "sexp3, colors"
  "(\x1b;[38;2;129;161;193mcond\x1b;[0m [(equal? (token-value tok) \x1b;[38;2;163;190;140m\"remove\"\x1b;[0m)\n       (continue nc ti\n         (\x1b;[38;2;129;161;193mlambda\x1b;[0m (\x1b;[38;2;229;233;240mtree\x1b;[0m)\n           (\x1b;[38;2;129;161;193munless\x1b;[0m (moore-lookup-node tree s)\n             (p-error ti \x1b;[38;2;163;190;140m\"notation for ~s does not exist\"\x1b;[0m s))\n           (moore-remove-handler tree s)))]\n      [(equal? (token-value tok) \x1b;[38;2;163;190;140m\"additive\"\x1b;[0m)\n       (let-parse-result\n         (((hd nc ti) (parse-null nc (ti-pop ti \x1b;[38;2;143;188;187m'lpar\x1b;[0m) rbp)))\n         (\x1b;[38;2;129;161;193mdefine\x1b;[0m \x1b;[1mop\x1b;[0m (string->symbol s))\n         (\x1b;[38;2;129;161;193mdefine\x1b;[0m \x1b;[1mhd\x1b;[0m (operand-token->expression tk))\n         (\x1b;[38;2;129;161;193mdefine-values\x1b;[0m (\x1b;[1ma\x1b;[0m . \x1b;[1mb\x1b;[0m) \x1b;[38;2;143;188;187m`(,\x1b;[0ma\x1b;[38;2;143;188;187m\x1b;[0m \x1b;[38;2;143;188;187m,@\x1b;[0mb\x1b;[38;2;143;188;187m)\x1b;[0m)\n         (\x1b;[38;2;129;161;193mdefine-values\x1b;[0m (\x1b;[1mti\x1b;[0m \x1b;[1mcomb\x1b;[0m)\n           (\x1b;[38;2;129;161;193mif\x1b;[0m (eq? (token-type (ti-car ti)) \x1b;[38;2;143;188;187m'comma\x1b;[0m)\n               (values (ti-pop (ti-cdr (ti-cdr ti)) \x1b;[38;2;143;188;187m'rpar\x1b;[0m) list)\n               (values (ti-pop ti \x1b;[38;2;143;188;187m'rpar\x1b;[0m) nary)))\n         (continue\n           (notation-conf-add-led nc op infix-led bp:add bp:add hd comb)\n           ti (add-auto-handler s)))]\n      [\x1b;[38;2;129;161;193melse\x1b;[0m\n       (p-error ti \x1b;[38;2;163;190;140m\"unknown modifier for notation: ~a\"\x1b;[0m\n         (token-value tok))])\n"
  (parameterize ((sgr-support-tier 2))
      (pp->string sexp3 pp-code #t pp-width 90 pp-brackets #t pp-max-tab 5 pp-color #t)))
(pretty-style 'let-parse-result (pretty-style 'let-values))
(test-equal "sexp3, colors, custom style"
  "(\x1b;[38;2;129;161;193mcond\x1b;[0m [(equal? (token-value tok) \x1b;[38;2;163;190;140m\"remove\"\x1b;[0m)\n       (continue nc ti\n         (\x1b;[38;2;129;161;193mlambda\x1b;[0m (\x1b;[38;2;229;233;240mtree\x1b;[0m)\n           (\x1b;[38;2;129;161;193munless\x1b;[0m (moore-lookup-node tree s)\n             (p-error ti \x1b;[38;2;163;190;140m\"notation for ~s does not exist\"\x1b;[0m s))\n           (moore-remove-handler tree s)))]\n      [(equal? (token-value tok) \x1b;[38;2;163;190;140m\"additive\"\x1b;[0m)\n       (\x1b;[38;2;129;161;193mlet-parse-result\x1b;[0m\n         ([(\x1b;[38;2;229;233;240mhd\x1b;[0m \x1b;[38;2;229;233;240mnc\x1b;[0m \x1b;[38;2;229;233;240mti\x1b;[0m) (parse-null nc (ti-pop ti \x1b;[38;2;143;188;187m'lpar\x1b;[0m) rbp)])\n         (\x1b;[38;2;129;161;193mdefine\x1b;[0m \x1b;[1mop\x1b;[0m (string->symbol s))\n         (\x1b;[38;2;129;161;193mdefine\x1b;[0m \x1b;[1mhd\x1b;[0m (operand-token->expression tk))\n         (\x1b;[38;2;129;161;193mdefine-values\x1b;[0m (\x1b;[1ma\x1b;[0m . \x1b;[1mb\x1b;[0m) \x1b;[38;2;143;188;187m`(,\x1b;[0ma\x1b;[38;2;143;188;187m\x1b;[0m \x1b;[38;2;143;188;187m,@\x1b;[0mb\x1b;[38;2;143;188;187m)\x1b;[0m)\n         (\x1b;[38;2;129;161;193mdefine-values\x1b;[0m (\x1b;[1mti\x1b;[0m \x1b;[1mcomb\x1b;[0m)\n           (\x1b;[38;2;129;161;193mif\x1b;[0m (eq? (token-type (ti-car ti)) \x1b;[38;2;143;188;187m'comma\x1b;[0m)\n               (values (ti-pop (ti-cdr (ti-cdr ti)) \x1b;[38;2;143;188;187m'rpar\x1b;[0m) list)\n               (values (ti-pop ti \x1b;[38;2;143;188;187m'rpar\x1b;[0m) nary)))\n         (continue\n           (notation-conf-add-led nc op infix-led bp:add bp:add hd comb)\n           ti (add-auto-handler s)))]\n      [\x1b;[38;2;129;161;193melse\x1b;[0m\n       (p-error ti \x1b;[38;2;163;190;140m\"unknown modifier for notation: ~a\"\x1b;[0m\n         (token-value tok))])\n"
  (parameterize ((sgr-support-tier 2))
      (pp->string sexp3 pp-code #t pp-width 90 pp-brackets #t pp-max-tab 5 pp-color #t)))
(pretty-style 'let-parse-result #f)
(test-equal "sexp4, colors"
  "(\x1b;[38;2;129;161;193mdefine-syntax\x1b;[0m \x1b;[1mset*!\x1b;[0m\n  (\x1b;[38;2;129;161;193mlambda\x1b;[0m (\x1b;[38;2;229;233;240mx\x1b;[0m)\n    (\x1b;[38;2;129;161;193mletrec\x1b;[0m\n      ([\x1b;[38;2;229;233;240munwrap-exp\x1b;[0m\n        (\x1b;[38;2;129;161;193mlambda\x1b;[0m (\x1b;[38;2;229;233;240mx\x1b;[0m)\n          (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mx\x1b;[0m (unwrap-syntax x)])\n            (\x1b;[38;2;129;161;193mif\x1b;[0m (pair? x) (cons (car x) (unwrap-exp (cdr x))) x)))])\n      (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240msets\x1b;[0m (map unwrap-exp (cdr (unwrap-exp x)))])\n        (\x1b;[38;2;129;161;193mlet\x1b;[0m\n          ([\x1b;[38;2;229;233;240mids\x1b;[0m (map car sets)]\n           [\x1b;[38;2;229;233;240mvals\x1b;[0m (map cadr sets)]\n           [\x1b;[38;2;229;233;240mtemps\x1b;[0m (map (\x1b;[38;2;129;161;193mlambda\x1b;[0m (\x1b;[38;2;229;233;240mx\x1b;[0m) (generate-identifier)) sets)])\n          \x1b;[38;2;143;188;187m`(,\x1b;[0m\x1b;[38;2;143;188;187m'let\x1b;[0m\x1b;[38;2;143;188;187m\x1b;[0m\n            \x1b;[38;2;143;188;187m,\x1b;[0m(map list temps vals)\x1b;[38;2;143;188;187m\x1b;[0m\n            \x1b;[38;2;143;188;187m,@\x1b;[0m(map (\x1b;[38;2;129;161;193mlambda\x1b;[0m (\x1b;[38;2;229;233;240mid\x1b;[0m \x1b;[38;2;229;233;240mtemp\x1b;[0m) \x1b;[38;2;143;188;187m`(,\x1b;[0m\x1b;[38;2;143;188;187m'set!\x1b;[0m\x1b;[38;2;143;188;187m\x1b;[0m \x1b;[38;2;143;188;187m,\x1b;[0mid\x1b;[38;2;143;188;187m\x1b;[0m \x1b;[38;2;143;188;187m,\x1b;[0mtemp\x1b;[38;2;143;188;187m)\x1b;[0m) ids temps)\x1b;[38;2;143;188;187m\x1b;[0m\n            \x1b;[38;2;143;188;187m#f)\x1b;[0m)))))\n"
  (parameterize ((sgr-support-tier 2))
      (pp->string sexp4 pp-code #t pp-width 90 pp-brackets #t pp-max-tab 5 pp-color #t)))
(test-equal "sexp5, colors"
  "(\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1mnormalize-definition\x1b;[0m \x1b;[38;2;229;233;240mt\x1b;[0m \x1b;[38;2;229;233;240msyntax-definition?\x1b;[0m)\n  (\x1b;[38;2;129;161;193mcond\x1b;[0m [(pair? (cdr t))\n         (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240m_\x1b;[0m (car t)] [\x1b;[38;2;229;233;240mhead\x1b;[0m (cadr t)] [\x1b;[38;2;229;233;240mbody\x1b;[0m (cddr t)])\n           (\x1b;[38;2;129;161;193mcond\x1b;[0m [(\x1b;[38;2;129;161;193mand\x1b;[0m (identifier? head) (pair? body) (null? (cdr body)))\n                  \x1b;[38;2;143;188;187m`(,\x1b;[0m_\x1b;[38;2;143;188;187m\x1b;[0m \x1b;[38;2;143;188;187m,\x1b;[0mhead\x1b;[38;2;143;188;187m\x1b;[0m \x1b;[38;2;143;188;187munquote\x1b;[0m \x1b;[38;2;143;188;187mbody)\x1b;[0m]\n                 [(\x1b;[38;2;129;161;193mand\x1b;[0m (pair? head) (identifier? (car head)) (formals? (cdr head)))\n                  (\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mr\x1b;[0m (make-primitive-renaming-procedure)])\n                    (\x1b;[38;2;129;161;193mif\x1b;[0m syntax-definition?\n                        \x1b;[38;2;143;188;187m`(,\x1b;[0m_\x1b;[38;2;143;188;187m\x1b;[0m\n                          \x1b;[38;2;143;188;187m,\x1b;[0m(car head)\x1b;[38;2;143;188;187m\x1b;[0m\n                          \x1b;[38;2;143;188;187m,\x1b;[0m(\x1b;[38;2;129;161;193mlet\x1b;[0m ([\x1b;[38;2;229;233;240mtransformer\x1b;[0m (r (symbolic-name (car head)))])\n                             \x1b;[38;2;143;188;187m`((,\x1b;[0m(r \x1b;[38;2;143;188;187m'lambda\x1b;[0m)\x1b;[38;2;143;188;187m\x1b;[0m\n                                \x1b;[38;2;143;188;187m(,\x1b;[0mtransformer\x1b;[38;2;143;188;187m)\x1b;[0m\n                                \x1b;[38;2;143;188;187m(,\x1b;[0m(r \x1b;[38;2;143;188;187m'lambda\x1b;[0m)\x1b;[38;2;143;188;187m\x1b;[0m\n                                 \x1b;[38;2;143;188;187m(,\x1b;[0m(r \x1b;[38;2;143;188;187m'form\x1b;[0m)\x1b;[38;2;143;188;187m)\x1b;[0m\n                                 \x1b;[38;2;143;188;187m(,\x1b;[0m(r \x1b;[38;2;143;188;187m'apply\x1b;[0m)\x1b;[38;2;143;188;187m\x1b;[0m \x1b;[38;2;143;188;187m,\x1b;[0mtransformer\x1b;[38;2;143;188;187m\x1b;[0m \x1b;[38;2;143;188;187m,\x1b;[0m(r \x1b;[38;2;143;188;187m'form\x1b;[0m)\x1b;[38;2;143;188;187m)))\x1b;[0m\n                               \x1b;[38;2;143;188;187m(,\x1b;[0m(r \x1b;[38;2;143;188;187m'lambda\x1b;[0m)\x1b;[38;2;143;188;187m\x1b;[0m\n                                \x1b;[38;2;143;188;187m(,\x1b;[0m(r \x1b;[38;2;143;188;187m'dummy\x1b;[0m)\x1b;[38;2;143;188;187m\x1b;[0m \x1b;[38;2;143;188;187munquote\x1b;[0m \x1b;[38;2;143;188;187m(cdr\x1b;[0m \x1b;[38;2;143;188;187mhead))\x1b;[0m\n                                \x1b;[38;2;143;188;187munquote\x1b;[0m\n                                \x1b;[38;2;143;188;187mbody))\x1b;[0m)\x1b;[38;2;143;188;187m)\x1b;[0m\n                        \x1b;[38;2;143;188;187m`(,\x1b;[0m_\x1b;[38;2;143;188;187m\x1b;[0m \x1b;[38;2;143;188;187m,\x1b;[0m(car head)\x1b;[38;2;143;188;187m\x1b;[0m \x1b;[38;2;143;188;187m(,\x1b;[0m(r \x1b;[38;2;143;188;187m'lambda\x1b;[0m)\x1b;[38;2;143;188;187m\x1b;[0m \x1b;[38;2;143;188;187m,\x1b;[0m(cdr head)\x1b;[38;2;143;188;187m\x1b;[0m \x1b;[38;2;143;188;187munquote\x1b;[0m \x1b;[38;2;143;188;187mbody))\x1b;[0m))]\n                 [\x1b;[38;2;129;161;193melse\x1b;[0m (syntax-error \x1b;[38;2;163;190;140m\"Syntax error in definition:\"\x1b;[0m t)]))]\n        [\x1b;[38;2;129;161;193melse\x1b;[0m (syntax-error \x1b;[38;2;163;190;140m\"Syntax error in definition:\"\x1b;[0m t)]))\n"
  (parameterize ((sgr-support-tier 2))
      (pp->string sexp5 pp-code #t pp-width 90 pp-brackets #t pp-max-tab 5 pp-color #t)))
(test-equal "sexp4, colors, tier 0"
  "(\x1b;[34mdefine-syntax\x1b;[0m \x1b;[1mset*!\x1b;[0m\n  (\x1b;[34mlambda\x1b;[0m (\x1b;[37mx\x1b;[0m)\n    (\x1b;[34mletrec\x1b;[0m\n      ((\x1b;[37munwrap-exp\x1b;[0m\n        (\x1b;[34mlambda\x1b;[0m (\x1b;[37mx\x1b;[0m)\n          (\x1b;[34mlet\x1b;[0m ((\x1b;[37mx\x1b;[0m (unwrap-syntax x)))\n            (\x1b;[34mif\x1b;[0m (pair? x)\n                (cons (car x) (unwrap-exp (cdr x)))\n                x)))))\n      (\x1b;[34mlet\x1b;[0m ((\x1b;[37msets\x1b;[0m (map unwrap-exp (cdr (unwrap-exp x)))))\n        (\x1b;[34mlet\x1b;[0m\n          ((\x1b;[37mids\x1b;[0m (map car sets))\n           (\x1b;[37mvals\x1b;[0m (map cadr sets))\n           (\x1b;[37mtemps\x1b;[0m\n            (map (\x1b;[34mlambda\x1b;[0m (\x1b;[37mx\x1b;[0m) (generate-identifier)) sets)))\n          \x1b;[96m`(,\x1b;[0m\x1b;[96m'let\x1b;[0m\x1b;[96m\x1b;[0m\n            \x1b;[96m,\x1b;[0m(map list temps vals)\x1b;[96m\x1b;[0m\n            \x1b;[96m,@\x1b;[0m(map (\x1b;[34mlambda\x1b;[0m (\x1b;[37mid\x1b;[0m \x1b;[37mtemp\x1b;[0m) \x1b;[96m`(,\x1b;[0m\x1b;[96m'set!\x1b;[0m\x1b;[96m\x1b;[0m \x1b;[96m,\x1b;[0mid\x1b;[96m\x1b;[0m \x1b;[96m,\x1b;[0mtemp\x1b;[96m)\x1b;[0m)\n                   ids temps)\x1b;[96m\x1b;[0m\n            \x1b;[96m#f)\x1b;[0m)))))\n"
  (parameterize ((sgr-support-tier 0))
      (pp->string sexp4 pp-width 60 pp-color #t)))
(test-equal "sexp4, colors, tier 1"
  "(\x1b;[38;5;109mdefine-syntax\x1b;[0m \x1b;[1mset*!\x1b;[0m\n  (\x1b;[38;5;109mlambda\x1b;[0m (\x1b;[38;5;254mx\x1b;[0m)\n    (\x1b;[38;5;109mletrec\x1b;[0m\n      ((\x1b;[38;5;254munwrap-exp\x1b;[0m\n        (\x1b;[38;5;109mlambda\x1b;[0m (\x1b;[38;5;254mx\x1b;[0m)\n          (\x1b;[38;5;109mlet\x1b;[0m ((\x1b;[38;5;254mx\x1b;[0m (unwrap-syntax x)))\n            (\x1b;[38;5;109mif\x1b;[0m (pair? x)\n                (cons (car x) (unwrap-exp (cdr x)))\n                x)))))\n      (\x1b;[38;5;109mlet\x1b;[0m ((\x1b;[38;5;254msets\x1b;[0m (map unwrap-exp (cdr (unwrap-exp x)))))\n        (\x1b;[38;5;109mlet\x1b;[0m\n          ((\x1b;[38;5;254mids\x1b;[0m (map car sets))\n           (\x1b;[38;5;254mvals\x1b;[0m (map cadr sets))\n           (\x1b;[38;5;254mtemps\x1b;[0m\n            (map (\x1b;[38;5;109mlambda\x1b;[0m (\x1b;[38;5;254mx\x1b;[0m) (generate-identifier)) sets)))\n          \x1b;[38;5;109m`(,\x1b;[0m\x1b;[38;5;109m'let\x1b;[0m\x1b;[38;5;109m\x1b;[0m\n            \x1b;[38;5;109m,\x1b;[0m(map list temps vals)\x1b;[38;5;109m\x1b;[0m\n            \x1b;[38;5;109m,@\x1b;[0m(map (\x1b;[38;5;109mlambda\x1b;[0m (\x1b;[38;5;254mid\x1b;[0m \x1b;[38;5;254mtemp\x1b;[0m) \x1b;[38;5;109m`(,\x1b;[0m\x1b;[38;5;109m'set!\x1b;[0m\x1b;[38;5;109m\x1b;[0m \x1b;[38;5;109m,\x1b;[0mid\x1b;[38;5;109m\x1b;[0m \x1b;[38;5;109m,\x1b;[0mtemp\x1b;[38;5;109m)\x1b;[0m)\n                   ids temps)\x1b;[38;5;109m\x1b;[0m\n            \x1b;[38;5;109m#f)\x1b;[0m)))))\n"
  (parameterize ((sgr-support-tier 1))
      (pp->string sexp4 pp-width 60 pp-color #t)))
(test-equal "sexp4, colors, tier 3"
  "(\x1b;[38;2;129;161;193mdefine-syntax\x1b;[0m \x1b;[1mset*!\x1b;[0m\n  (\x1b;[38;2;129;161;193mlambda\x1b;[0m (\x1b;[38;2;229;233;240mx\x1b;[0m)\n    (\x1b;[38;2;129;161;193mletrec\x1b;[0m\n      ((\x1b;[38;2;229;233;240munwrap-exp\x1b;[0m\n        (\x1b;[38;2;129;161;193mlambda\x1b;[0m (\x1b;[38;2;229;233;240mx\x1b;[0m)\n          (\x1b;[38;2;129;161;193mlet\x1b;[0m ((\x1b;[38;2;229;233;240mx\x1b;[0m (unwrap-syntax x)))\n            (\x1b;[38;2;129;161;193mif\x1b;[0m (pair? x)\n                (cons (car x) (unwrap-exp (cdr x)))\n                x)))))\n      (\x1b;[38;2;129;161;193mlet\x1b;[0m ((\x1b;[38;2;229;233;240msets\x1b;[0m (map unwrap-exp (cdr (unwrap-exp x)))))\n        (\x1b;[38;2;129;161;193mlet\x1b;[0m\n          ((\x1b;[38;2;229;233;240mids\x1b;[0m (map car sets))\n           (\x1b;[38;2;229;233;240mvals\x1b;[0m (map cadr sets))\n           (\x1b;[38;2;229;233;240mtemps\x1b;[0m\n            (map (\x1b;[38;2;129;161;193mlambda\x1b;[0m (\x1b;[38;2;229;233;240mx\x1b;[0m) (generate-identifier)) sets)))\n          \x1b;[38;2;143;188;187m`(,\x1b;[0m\x1b;[38;2;143;188;187m'let\x1b;[0m\x1b;[38;2;143;188;187m\x1b;[0m\n            \x1b;[38;2;143;188;187m,\x1b;[0m(map list temps vals)\x1b;[38;2;143;188;187m\x1b;[0m\n            \x1b;[38;2;143;188;187m,@\x1b;[0m(map (\x1b;[38;2;129;161;193mlambda\x1b;[0m (\x1b;[38;2;229;233;240mid\x1b;[0m \x1b;[38;2;229;233;240mtemp\x1b;[0m) \x1b;[38;2;143;188;187m`(,\x1b;[0m\x1b;[38;2;143;188;187m'set!\x1b;[0m\x1b;[38;2;143;188;187m\x1b;[0m \x1b;[38;2;143;188;187m,\x1b;[0mid\x1b;[38;2;143;188;187m\x1b;[0m \x1b;[38;2;143;188;187m,\x1b;[0mtemp\x1b;[38;2;143;188;187m)\x1b;[0m)\n                   ids temps)\x1b;[38;2;143;188;187m\x1b;[0m\n            \x1b;[38;2;143;188;187m#f)\x1b;[0m)))))\n"
  (parameterize ((sgr-support-tier 3))
      (pp->string sexp4 pp-width 60 pp-color #t)))
(test-equal "sexp4, colors, no tier"
  "(define-syntax set*!\n  (lambda (x)\n    (letrec\n      ((unwrap-exp\n        (lambda (x)\n          (let ((x (unwrap-syntax x)))\n            (if (pair? x)\n                (cons (car x) (unwrap-exp (cdr x)))\n                x)))))\n      (let ((sets (map unwrap-exp (cdr (unwrap-exp x)))))\n        (let\n          ((ids (map car sets))\n           (vals (map cadr sets))\n           (temps\n            (map (lambda (x) (generate-identifier)) sets)))\n          `(,'let\n            ,(map list temps vals)\n            ,@(map (lambda (id temp) `(,'set! ,id ,temp))\n                   ids temps)\n            #f))))))\n"
  (parameterize ((sgr-support-tier #f))
      (pp->string sexp4 pp-width 60 pp-color #t)))
(test-equal "sexp1, data"
  "(define\n (ast-pretty a . opt-port)\n (define\n  (width x)\n  (cond\n   ((string? x) (+ (string-length x) 2))\n   ((char? x) 3)\n   ((number? x) 5)\n   ((symbol? x) (string-length (symbol->string x)))\n   ((boolean? x) 2)\n   ((null? x) 2)\n   (else 10)))\n (define\n  (ast-width a)\n  (cond\n   ((ast-atom? a) (width (ast-atom->val a)))\n   ((ast-null? a) 2)\n   ((ast-pair? a)\n    (if\n     (ast-list? a)\n     (let\n      loop\n      ((a a) (sum 0) (count 0))\n      (if\n       (ast-null? a)\n       (+ sum count 1)\n       (loop\n        (ast-cdr a)\n        (+ sum (ast-width (ast-car a)))\n        (+ count 1))))\n     (+\n      1\n      (ast-width (ast-car a))\n      3\n      (ast-width (ast-cdr a))\n      1)))\n   (else 0)))\n (define\n  (ast-width-hello a)\n  (display \"hello!\\n\")\n  (cond\n   ((ast-atom? a) (width (ast-atom->val a)))\n   ((ast-null? a) 2)\n   ((ast-pair? a)\n    (if\n     (ast-list? a)\n     (let\n      loop\n      ((a a) (sum 0) (count 0))\n      (if\n       (ast-null? a)\n       (+ sum count 1)\n       (loop\n        (ast-cdr a)\n        (+ sum (ast-width (ast-car a)))\n        (+ count 1))))\n     (+\n      1\n      (ast-width (ast-car a))\n      3\n      (ast-width (ast-cdr a))\n      1)))\n   (else\n    (cond\n     ((foo) => (lambda (v) (bar v 0)))\n     (else (begin (newline) (if a #t #f)))))))\n (define\n  (break? cur-col width max-col)\n  (> (+ cur-col width) max-col))\n (define\n  (print-indent n port)\n  (do ((i 0 (+ i 1))) ((= i n)) (display \"  \" port)))\n (define\n  (pretty a col indent port max-col)\n  (cond\n   ((ast-atom? a)\n    (let\n     ((w (width (ast-atom->val a))))\n     (write (ast-atom->val a) port)\n     (+ col w)))\n   ((ast-null? a) (display \"()\" port) (+ col 2))\n   ((ast-pair? a)\n    (if\n     (ast-list? a)\n     (let*\n      ((total-width (ast-width a))\n       (remaining (- max-col col))\n       (inline? (<= total-width remaining)))\n      (if\n       inline?\n       (begin\n        (display \"(\" port)\n        (let\n         loop\n         ((first? #t) (rest a) (cur-col (+ col 1)))\n         (if\n          (ast-null? rest)\n          (begin (display \")\" port) (+ cur-col 1))\n          (let*\n           ((elem-width (ast-width (ast-car rest)))\n            (new-col\n             (if\n              first?\n              (pretty\n               (ast-car rest)\n               cur-col\n               indent\n               port\n               max-col)\n              (begin\n               (display \" \" port)\n               (pretty\n                (ast-car rest)\n                (+ cur-col 1)\n                indent\n                port\n                max-col)))))\n           (loop #f (ast-cdr rest) new-col)))))\n       (begin\n        (display \"(\" port)\n        (let\n         ((new-indent (+ indent 1)))\n         (let\n          loop\n          ((rest a) (cur-col (* new-indent 2)))\n          (if\n           (ast-null? rest)\n           (begin (display \")\" port) (+ cur-col 1))\n           (let*\n            ((elem (ast-car rest))\n             (next-col\n              (pretty elem cur-col new-indent port max-col)))\n            (unless\n             (ast-null? (ast-cdr rest))\n             (newline port)\n             (print-indent new-indent port))\n            (loop (ast-cdr rest) (* new-indent 2)))))))))\n     (begin\n      (display \"(\" port)\n      (let\n       ((car-col\n         (pretty (ast-car a) (+ col 1) indent port max-col)))\n       (display \" . \" port)\n       (let\n        ((cdr-col\n          (pretty\n           (ast-cdr a)\n           (+ car-col 3)\n           indent\n           port\n           max-col)))\n        (display \")\" port)\n        (+ cdr-col 1))))))))\n (let\n  ((port\n    (if\n     (null? opt-port)\n     (current-output-port)\n     (car opt-port))))\n  (pretty a 0 0 port 80)\n  (newline port)))\n"
  (pp->string sexp1 pp-code #f pp-width 60))
(test-equal "sexp5, width 50"
  "(define\n  (normalize-definition t syntax-definition?)\n  (cond\n    ((pair? (cdr t))\n     (let\n       ((_ (car t))\n        (head (cadr t))\n        (body (cddr t)))\n       (cond\n         ((and (identifier? head) (pair? body)\n               (null? (cdr body)))\n          `(,_ ,head unquote body))\n         ((and (pair? head)\n               (identifier? (car head))\n               (formals? (cdr head)))\n          (let\n            ((r\n              (make-primitive-renaming-procedure)))\n            (if syntax-definition?\n                `(,_\n                  ,(car head)\n                  ,(let\n                     ((transformer\n                       (r (symbolic-name\n                            (car head)))))\n                     `((,(r 'lambda)\n                        (,transformer)\n                        (,(r 'lambda)\n                         (,(r 'form))\n                         (,(r 'apply)\n                          ,transformer\n                          ,(r 'form))))\n                       (,(r 'lambda)\n                        (,(r 'dummy)\n                         unquote\n                         (cdr head))\n                        unquote\n                        body))))\n                `(,_\n                  ,(car head)\n                  (,(r 'lambda)\n                   ,(cdr head)\n                   unquote\n                   body)))))\n         (else\n          (syntax-error\n            \"Syntax error in definition:\" t)))))\n    (else\n     (syntax-error \"Syntax error in definition:\" t))))\n"
  (pp->string sexp5 pp-width 50))
(test-equal "pp-newline #f"
  "(define (f x)\n  (let ((y (* x x)))\n    (if (> y 100) (display \"large\") (display \"small\"))\n    (newline)\n    y))"
  (pp->string small-fun pp-newline #f))
(test-equal "pp-pretty #f"
  "(define-syntax set*! (lambda (x) (letrec ((unwrap-exp (lambda (x) (let ((x (unwrap-syntax x))) (if (pair? x) (cons (car x) (unwrap-exp (cdr x))) x))))) (let ((sets (map unwrap-exp (cdr (unwrap-exp x))))) (let ((ids (map car sets)) (vals (map cadr sets)) (temps (map (lambda (x) (generate-identifier)) sets))) `(,'let ,(map list temps vals) ,@(map (lambda (id temp) `(,'set! ,id ,temp)) ids temps) #f))))))"
  (pp->string sexp4 pp-pretty #f))
(test-equal "pp-pretty #f, pp-length 2"
  "(define-syntax set*! ...)"
  (pp->string sexp4 pp-pretty #f pp-length 2))
(test-equal "pp-indent 10"
  "(define (f x)\n            (let ((y (* x x)))\n              (if (> y 100)\n                  (display \"large\")\n                  (display \"small\"))\n              (newline)\n              y))\n"
  (pp->string small-fun pp-indent 10 pp-width 50))
(test-equal "pp-tab 0"
  "(define (f x)\n (let ((y (* x x)))\n  (if (> y 100)\n      (display \"large\")\n      (display \"small\"))\n  (newline)\n  y))\n"
  (pp->string small-fun pp-tab 0 pp-width 40))
(test-equal "pp-tab 3"
  "(define (f x)\n    (let ((y (* x x)))\n        (if (> y 100)\n            (display \"large\")\n            (display \"small\"))\n        (newline)\n        y))\n"
  (pp->string small-fun pp-tab 3 pp-width 40))
(test-equal "pp-max-tab 0"
  "(define (f x)\n  (let ((y (* x x)))\n    (if\n      (> y 100)\n      (display \"large\")\n      (display \"small\"))\n    (newline)\n    y))\n"
  (pp->string small-fun pp-max-tab 0 pp-width 40))
(test-equal "pp-max-tab 8"
  "(define (f x)\n  (let ((y (* x x)))\n    (if (> y 100)\n        (display \"large\")\n        (display \"small\"))\n    (newline)\n    y))\n"
  (pp->string small-fun pp-max-tab 8 pp-width 40))
(test-equal "pp-inline-width 20"
  "(define (f x)\n  (let ((y (* x x)))\n    (if (> y 100)\n        (display \"large\")\n        (display \"small\"))\n    (newline)\n    y))\n"
  (pp->string small-fun pp-inline-width 20))
(test-equal "pp-inline-width #f"
  "(cond\n  ((equal? (token-value tok) \"remove\")\n   (continue nc ti\n     (lambda (tree)\n       (unless (moore-lookup-node tree s)\n         (p-error ti \"notation for ~s does not exist\" s))\n       (moore-remove-handler tree s))))\n  ((equal? (token-value tok) \"additive\")\n   (let-parse-result (((hd nc ti) (parse-null nc (ti-pop ti 'lpar) rbp)))\n     (define op (string->symbol s)) (define hd (operand-token->expression tk))\n     (define-values (a . b) `(,a ,@b))\n     (define-values (ti comb)\n       (if (eq? (token-type (ti-car ti)) 'comma)\n           (values (ti-pop (ti-cdr (ti-cdr ti)) 'rpar) list)\n           (values (ti-pop ti 'rpar) nary)))\n     (continue (notation-conf-add-led nc op infix-led bp:add bp:add hd comb) ti\n       (add-auto-handler s))))\n  (else (p-error ti \"unknown modifier for notation: ~a\" (token-value tok))))\n"
  (pp->string sexp3 pp-inline-width #f))
(test-equal "pp-miser-width #f"
  "(define (normalize-definition t syntax-definition?)\n  (cond\n    ((pair? (cdr t))\n     (let ((_ (car t)) (head (cadr t)) (body (cddr t)))\n       (cond\n         ((and (identifier? head) (pair? body)\n               (null? (cdr body)))\n          `(,_ ,head unquote body))\n         ((and (pair? head) (identifier? (car head))\n               (formals? (cdr head)))\n          (let ((r (make-primitive-renaming-procedure)))\n            (if syntax-definition?\n                `(,_\n                  ,(car head)\n                  ,(let\n                     ((transformer\n                       (r (symbolic-name (car head)))))\n                     `((,(r 'lambda)\n                        (,transformer)\n                        (,(r 'lambda)\n                         (,(r 'form))\n                         (,(r 'apply)\n                          ,transformer\n                          ,(r 'form))))\n                       (,(r 'lambda)\n                        (,(r 'dummy) unquote (cdr head))\n                        unquote\n                        body))))\n                `(,_\n                  ,(car head)\n                  (,(r 'lambda) ,(cdr head) unquote body)))))\n         (else\n          (syntax-error \"Syntax error in definition:\" t)))))\n    (else (syntax-error \"Syntax error in definition:\" t))))\n"
  (pp->string sexp5 pp-miser-width #f pp-width 60))
(test-equal "pp-miser-width 40"
  "(define (normalize-definition t syntax-definition?)\n  (cond\n    ((pair? (cdr t))\n     (let ((_ (car t)) (head (cadr t)) (body (cddr t)))\n       (cond\n         ((and (identifier? head) (pair? body)\n               (null? (cdr body)))\n          `(,_ ,head unquote body))\n         ((and (pair? head) (identifier? (car head))\n               (formals? (cdr head)))\n          (let ((r (make-primitive-renaming-procedure)))\n            (if syntax-definition?\n                `(,_\n                  ,(car head)\n                  ,(let\n                    ((transformer\n                      (r (symbolic-name (car head)))))\n                    `((,(r 'lambda)\n                       (,transformer)\n                       (,(r 'lambda)\n                        (,(r 'form))\n                        (,(r 'apply)\n                         ,transformer\n                         ,(r 'form))))\n                      (,(r 'lambda)\n                       (,(r 'dummy) unquote (cdr head))\n                       unquote\n                       body))))\n                `(,_\n                  ,(car head)\n                  (,(r 'lambda) ,(cdr head) unquote body)))))\n         (else\n          (syntax-error \"Syntax error in definition:\" t)))))\n    (else (syntax-error \"Syntax error in definition:\" t))))\n"
  (pp->string sexp5 pp-miser-width 40 pp-width 60))
(test-equal "pp-brackets"
  "(define (f x)\n  (let ([y (* x x)])\n    (if (> y 100)\n        (display \"large\")\n        (display \"small\"))\n    (newline)\n    y))\n"
  (pp->string small-fun pp-brackets #t pp-width 40))
(test-equal "pp-level-stub #"
  "(if (member x y) (+ # 3) '(foo . #))\n"
  (pp->string cut-sample pp-level 2 pp-level-stub "#"))
(test-equal "pp-length-stub"
  "#u8(0 10 2 3 4 <...>)\n"
  (pp->string bv30 pp-length 5 pp-length-stub "<...>"))
(test-equal "pp-lines 3"
  "(define-syntax set*!\n  (lambda (x)\n    (letrec ..)))\n"
  (pp->string sexp4 pp-lines 3))
(test-equal "pp-lines-stub"
  "(define-syntax set*!\n  (lambda (x)\n    (letrec <more>)))\n"
  (pp->string sexp4 pp-lines 3 pp-lines-stub "<more>"))
(test-equal "pp-lines-stub #t"
  "(define-syntax set*!\n  (lambda (x)\n    (letrec ~~)))\n"
  (pp->string sexp4 pp-lines 3 pp-lines-stub #t pp-length-stub "~~"))
(test-equal "pp-lines-stub #f"
  "(define-syntax set*!\n  (lambda (x)\n    (letrec ...)))\n"
  (pp->string sexp4 pp-lines 3 pp-lines-stub #f))
(test-equal "pp-radix 16 in code"
  "(list #xff #x-10 #x7/3 1.5 #\\a \"x\")\n"
  (pp->string '(list 255 -16 7/3 1.5 #\a "x") pp-radix 16))
(test-equal "pp-radix 2 in code"
  "(vector #b101 #b110)\n"
  (pp->string '(vector 5 6) pp-radix 2))
(test-equal "pp-emit"
  "(DEFINE (F X)\n  (LET ((Y (* X X)))\n    (IF (> Y 100)\n        (DISPLAY \"LARGE\")\n        (DISPLAY \"SMALL\"))\n    (NEWLINE)\n    Y))\n"
  (pp->string small-fun pp-width 40
      pp-emit (lambda (s port) (write-string (string-upcase s) port))))
(test-equal "pp-emit with colors"
  "(\x1b;[34mDEFINE\x1b;[0m (\x1b;[1mF\x1b;[0m \x1b;[37mX\x1b;[0m)\n  (\x1b;[34mLET\x1b;[0m ((\x1b;[37mY\x1b;[0m (* X X)))\n    (\x1b;[34mIF\x1b;[0m (> Y \x1b;[35m100\x1b;[0m)\n        (DISPLAY \x1b;[32m\"LARGE\"\x1b;[0m)\n        (DISPLAY \x1b;[32m\"SMALL\"\x1b;[0m))\n    (NEWLINE)\n    Y))\n"
  (parameterize ((sgr-support-tier 0))
      (pp->string small-fun pp-width 40 pp-color #t
        pp-emit (lambda (s port) (write-string (string-upcase s) port)))))
(test-equal "pp-tint drops colors"
  #t
  (string=? (pp->string sexp3 pp-color #t pp-tint (lambda (s port) #f))
              (pp->string sexp3)))
(test-equal "pp-tint"
  "(|define| (|f| |x|)\n  (|let| ((|y| (* x x)))\n    (|if| (> y |100|)\n        (display |\"large\"|)\n        (display |\"small\"|))\n    (newline)\n    y))\n"
  (parameterize ((sgr-support-tier 0))
      (pp->string small-fun pp-width 40 pp-color #t
        pp-tint (lambda (s port) (write-string "|" port)))))
(test-equal "pp-color with custom mapper"
  "(<keyword>define</keyword> (<defined>f</defined> <formal>x</formal>)\n  (<keyword>let</keyword> ((<formal>y</formal> (* x x)))\n    (<keyword>if</keyword> (> y <number>100</number>)\n        (display <string>\"large\"</string>)\n        (display <string>\"small\"</string>))\n    (newline)\n    y))\n"
  (pp->string small-fun pp-width 40
      pp-color (lambda (sc start?)
                 (if start?
                     (string-append "<" (symbol->string sc) ">")
                     (string-append "</" (symbol->string sc) ">")))))
(test-equal "pp-code #f with colors"
  "(define\n (f x)\n (let\n  ((y (* x x)))\n  (if\n   (> y 100)\n   (display \"large\")\n   (display \"small\"))\n  (newline)\n  y))\n"
  (parameterize ((sgr-support-tier 0))
      (pp->string small-fun pp-width 40 pp-code #f pp-color #t)))
(define shared-vec (let ((v (vector 1 2))) (list v v)))
(define shared-str (let ((s (string #\a #\b))) (list s s)))
(define cyclic-vec (let ((v (vector 1 2 3))) (vector-set! v 1 v) v))
(test-equal "pprint, shared vector"
  "(#(1 2) #(1 2))\n"
  (capture (lambda () (pprint shared-vec))))
(test-equal "pprint-shared, shared vector"
  "(#0=#(1 2) #0#)\n"
  (capture (lambda () (pprint-shared shared-vec))))
(test-equal "pprint-shared, shared string"
  "(#0=\"ab\" #0#)\n"
  (capture (lambda () (pprint-shared shared-str))))
(test-equal "pprint-simple, shared vector"
  "(#(1\n   2)\n #(1\n   2))\n"
  (capture (lambda () (pprint-simple shared-vec pp-width 5))))
(test-equal "pprint, cyclic vector"
  "#0=#(1 #0# 3)\n"
  (let ((p (open-output-string))) (pprint cyclic-vec p pp-width 20) (get-output-string p)))
(test-equal "pprint-shared, cyclic vector, level 1"
  "#(1 #(...) 3)\n"
  (let ((p (open-output-string))) (pprint-shared cyclic-vec p pp-level 1) (get-output-string p)))
(test-equal "pp*"
  "(define-syntax set*!\n  (lambda (x)\n    (letrec\n      ((unwrap-exp\n        (lambda (x)\n          (let\n           ((x\n             (unwrap-syntax x)))\n           (if\n            (pair? x)\n            (cons (car x)\n             (unwrap-exp\n              (cdr x)))\n            ...)))))\n      (let\n        ((sets\n          (map unwrap-exp\n           (cdr (unwrap-exp x)))))\n        (let\n          ((ids\n            (map car sets))\n           (vals\n            (map cadr sets))\n           (temps\n            (map\n             (lambda (x)\n              (generate-identifier))\n             sets)))\n          `(,'let\n            ,(map list temps\n              ...)\n            ,@(map\n               (lambda\n                (id temp)\n                `(,'set!\n                  ,id\n                  ,temp))\n               ids ...)\n            ...))))))\n"
  (let ((p (open-output-string)))
      (pp* sexp4 p pp-width 30 (list pp-length 3 pp-width 60))
      (get-output-string p)))
(test-equal "lookup-pp-style"
  '(_ e . body)
  (lookup-pp-style (pp-styles) 'when))
(test-equal "lookup-pp-style, unknown"
  #f
  (lookup-pp-style (pp-styles) 'unknown-form))
(test-equal "add-pp-style is functional"
  '(#f (_ e . body))
  (let* ((s0 (pp-styles))
           (s1 (add-pp-style s0 'my-form '(_ e . body))))
      (list (lookup-pp-style s0 'my-form) (lookup-pp-style s1 'my-form))))
(test-equal "add-pp-style removes"
  #f
  (lookup-pp-style (add-pp-style (pp-styles) 'when #f) 'when))
(test-equal "pp-styles parameter"
  "(my-define (f x y)\n (display x)\n (display y)\n (+ x y))\n"
  (parameterize ((pp-styles (add-pp-style (pp-styles) 'my-define (pretty-style 'define))))
      (pp->string '(my-define (f x y) (display x) (display y) (+ x y)) pp-width 20)))
(test-equal "pp-styles keyword"
  "(my-define (f x y)\n (display x)\n (display y)\n (+ x y))\n"
  (pp->string '(my-define (f x y) (display x) (display y) (+ x y)) pp-width 20
      pp-styles (add-pp-style (pp-styles) 'my-define (pretty-style 'define))))
(test-equal "unknown form"
  "(my-define (f x y)\n (display x)\n (display y) (+ x y))\n"
  (pp->string '(my-define (f x y) (display x) (display y) (+ x y)) pp-width 20))
(pretty-style 'my-loop '(_ i? fc* . body))
(test-equal "pretty-style with explicit style"
  "(my-loop lp ((i 0) (acc '()))\n  (if (> i 10)\n      acc\n      (lp (+ i 1) (cons i acc))))\n"
  (pp->string '(my-loop lp ((i 0) (acc '())) (if (> i 10) acc (lp (+ i 1) (cons i acc)))) pp-width 30))
(test-equal "pretty-style lookup"
  '(_ i? fc* . body)
  (pretty-style 'my-loop))
(pretty-style 'my-loop #f)
(test-equal "pretty-style removed"
  #f
  (pretty-style 'my-loop))
(test-equal "style dc*, lc*, fc*, ec*"
  "(my-form\n (a 1)\n ((b) 2)\n (c (d e) f))\n"
  (pp->string '(my-form (a 1) ((b) 2) (c (d e) f)) pp-width 12
      pp-styles (add-pp-style (pp-styles) 'my-form '(_ (l . fc*) . ec*))))
(test-equal "style h*, spread"
  "(my-lib (foo bar)\n (export x)\n\n (import y)\n\n (begin z))\n"
  (pp->string '(my-lib (foo bar) (export x) (import y) (begin z)) pp-width 20
      pp-styles (add-pp-style (pp-styles) 'my-lib '(_ h* . spread))))
(define-record-type point (make-point x y) point? (x point-x) (y point-y))
(define-record-type bag (make-bag items) bag? (items bag-items))
(define-record-type nums (make-nums vec) nums? (vec nums-vec))
(define (tilde? x) (and (pair? x) (eq? (car x) 'tilde) (pair? (cdr x)) (null? (cddr x))))
(define point-hook
  (atom-pp-hook #f
    (lambda (p) 12)
    (lambda (p radix)
      (list "#<point " 'number (number->string (point-x p) radix) " "
            'number (number->string (point-y p) radix) ">"))))
(define bag-hook (glst-pp-hook "#[" bag-items make-bag "]"))
(define nums-hook
  (bvec-pp-hook "#n(" (lambda (n) (vector-length (nums-vec n)))
                (lambda (n i) (vector-ref (nums-vec n) i)) ")"))
(define tilde-hook (rmac-pp-hook "~" cadr (lambda (x) (list 'tilde x))))
(define test-hooks
  (add-pp-hook
    (add-pp-hook
      (add-pp-hook
        (add-pp-hook (pp-hooks) point? point-hook)
        bag? bag-hook)
      nums? nums-hook)
    tilde? tilde-hook))
(test-equal "lookup-pp-hook"
  #t
  (eq? (lookup-pp-hook test-hooks bag?) bag-hook))
(test-equal "lookup-pp-hook, unknown"
  #f
  (lookup-pp-hook test-hooks pair?))
(test-equal "atom-pp-hook"
  "(#<point 1 2> #<point 10 20>)\n"
  (pp->string (list (make-point 1 2) (make-point 10 20)) pp-hooks test-hooks))
(test-equal "atom-pp-hook with radix"
  "(#<point a 14>)\n"
  (pp->string (list (make-point 10 20)) pp-hooks test-hooks pp-radix 16))
(test-equal "atom-pp-hook with colors"
  "(\x1b;[96m#<point \x1b;[35m10\x1b;[0m \x1b;[35m20\x1b;[0m>\x1b;[0m)\n"
  (parameterize ((sgr-support-tier 0))
      (pp->string (list (make-point 10 20)) pp-hooks test-hooks pp-color #t)))
(test-equal "glst-pp-hook"
  "#[alpha\n  beta\n  gamma\n  delta\n  epsilon\n  zeta\n  eta\n  theta]\n"
  (pp->string (make-bag '(alpha beta gamma delta epsilon zeta eta theta)) pp-hooks test-hooks
      pp-width 30))
(test-equal "glst-pp-hook, length"
  "#[alpha beta ...]\n"
  (pp->string (make-bag '(alpha beta gamma delta)) pp-hooks test-hooks pp-length 2))
(test-equal "glst-pp-hook, level"
  "(#[...])\n"
  (pp->string (list (make-bag '(alpha (beta) gamma))) pp-hooks test-hooks pp-level 1))
(test-equal "bvec-pp-hook"
  "#n(1 2 3 4 5\n   6 7 8 9\n   10 11 12)\n"
  (pp->string (make-nums (vector 1 2 3 4 5 6 7 8 9 10 11 12)) pp-hooks test-hooks pp-width 12))
(test-equal "bvec-pp-hook, radix"
  "#n(#xa #xb #xc #xff)\n"
  (pp->string (make-nums (vector 10 11 12 255)) pp-hooks test-hooks pp-radix 16))
(test-equal "rmac-pp-hook"
  "(a ~b ~(c d))\n"
  (pp->string '(a (tilde b) (tilde (c d))) pp-hooks test-hooks))
(test-equal "rmac-pp-hook in code"
  "(define (f x)\n ~(g x))\n"
  (pp->string '(define (f x) (tilde (g x))) pp-hooks test-hooks pp-width 15))
(test-equal "hooks removed"
  "(a (tilde b))\n"
  (pp->string '(a (tilde b)) pp-hooks (add-pp-hook test-hooks tilde? #f)))
(test-equal "hook-returning test"
  "(#<point 1 2>)\n"
  (pp->string (list (make-point 1 2))
      pp-hooks (add-pp-hook (pp-hooks) (lambda (x) (and (point? x) point-hook)) #t)))
(test-equal "lookup of hook-returning test"
  #t
  (let ((test (lambda (x) #f)))
      (lookup-pp-hook (add-pp-hook (pp-hooks) test #t) test)))
(pretty-hook bag? bag-hook)
(test-equal "pretty-hook lookup"
  #t
  (eq? (pretty-hook bag?) bag-hook))
(test-equal "pretty-hook"
  "#[1 2 3]\n"
  (pp->string (make-bag '(1 2 3))))
(pretty-hook bag? #f)
(test-equal "pretty-hook removed"
  #f
  (pretty-hook bag?))
(test-equal "make-pprint-generator"
  '("(define-syntax set*!\n" "  (lambda (x)\n" "    (letrec\n" "      ((unwrap-exp\n" "        (lambda (x)\n" "          (let ((x (unwrap-syntax x)))\n" "            (if (pair? x)\n" "                (cons (car x) (unwrap-exp (cdr x)))\n" "                x)))))\n" "      (let\n" "        ((sets\n" "          (map unwrap-exp (cdr (unwrap-exp x)))))\n" "        (let\n" "          ((ids (map car sets))\n" "           (vals (map cadr sets))\n" "           (temps\n" "            (map (lambda (x)\n" "                   (generate-identifier))\n" "                 sets)))\n" "          `(,'let\n" "            ,(map list temps vals)\n" "            ,@(map (lambda (id temp)\n" "                     `(,'set! ,id ,temp))\n" "                   ids temps)\n" "            #f))))))\n")
  (generator->list (make-pprint-generator sexp4 pp-width 50)))
(test-equal "make-pprint-generator, no newline"
  '("(define (f x)\n" "  (let ((y (* x x)))\n" "    (if (> y 100)\n" "        (display \"large\")\n" "        (display \"small\"))\n" "    (newline)\n" "    y))")
  (generator->list (make-pprint-generator small-fun pp-width 40 pp-newline #f)))
(test-equal "make-pprint-generator, colors"
  '("(\x1b;[34mdefine\x1b;[0m (\x1b;[1mf\x1b;[0m \x1b;[37mx\x1b;[0m)\n" "  (\x1b;[34mlet\x1b;[0m ((\x1b;[37my\x1b;[0m (* x x)))\n" "    (\x1b;[34mif\x1b;[0m (> y \x1b;[35m100\x1b;[0m)\n" "        (display \x1b;[32m\"large\"\x1b;[0m)\n" "        (display \x1b;[32m\"small\"\x1b;[0m))\n" "    (newline)\n" "    y))\n")
  (parameterize ((sgr-support-tier 0))
      (generator->list (make-pprint-generator small-fun pp-width 40 pp-color #t))))
(test-equal "make-pprint-generator, colors filtered"
  '("(\x1b;[34mdefine\x1b;[0m (\x1b;[1mf\x1b;[0m \x1b;[37mx\x1b;[0m)\n" "  (\x1b;[34mlet\x1b;[0m ((\x1b;[37my\x1b;[0m (* x x)))\n" "    (\x1b;[34mif\x1b;[0m (> y \x1b;[35m100\x1b;[0m)\n" "        (display \x1b;[32m\"large\"\x1b;[0m)\n" "        (display \x1b;[32m\"small\"\x1b;[0m))\n" "    (newline)\n" "    y))\n")
  (parameterize ((sgr-support-tier 0))
      (generator->list (make-pprint-generator small-fun pp-width 40 pp-color #t
                         pp-tint (lambda (s port) #f)))))
(test-equal "make-pprint-generator, atom"
  '("42\n")
  (generator->list (make-pprint-generator 42)))
(write-file (temp-path "srfi-272-e.scm")
  ";; -*- mode: scheme; fill-column: 40; pp-max-tab: 2 -*-\n;; Header comment\n\n(define (f x) (let ((y (* x x))) (if (> y 100) (display \"large\") (display \"small\")) (newline) y))\n; between\n\n\n(f 10) ; trailing\n   ;; indented comment\n(define lst '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30))\n")
(test-equal "pprint-file"
  ";; -*- mode: scheme; fill-column: 40; pp-max-tab: 2 -*-\n;; Header comment\n\n(define (f x)\n  (let ((y (* x x)))\n    (if\n      (> y 100)\n      (display \"large\")\n      (display \"small\"))\n    (newline)\n    y))\n; between\n\n\n(f 10) ; trailing\n   ;; indented comment\n(define lst\n  '(1\n    2\n    3\n    4\n    5\n    6\n    7\n    8\n    9\n    10\n    11\n    12\n    13\n    14\n    15\n    16\n    17\n    18\n    19\n    20\n    21\n    22\n    23\n    24\n    25\n    26\n    27\n    28\n    29\n    30))\n"
  (capture (lambda () (pprint-file (temp-path "srfi-272-e.scm")))))
(test-equal "pprint-file, keyword arguments override magic comment"
  ";; -*- mode: scheme; fill-column: 40; pp-max-tab: 2 -*-\n;; Header comment\n\n(define (f x)\n  (let ((y (* x x)))\n    (if (> y 100) (display \"large\") (display \"small\"))\n    (newline)\n    y))\n; between\n\n\n(f 10) ; trailing\n   ;; indented comment\n(define lst\n  '(1\n    2\n    3\n    4\n    5\n    6\n    7\n    8\n    9\n    10\n    11\n    12\n    13\n    14\n    15\n    16\n    17\n    18\n    19\n    20\n    21\n    22\n    23\n    24\n    25\n    26\n    27\n    28\n    29\n    30))\n"
  (capture (lambda () (pprint-file (temp-path "srfi-272-e.scm") pp-width 70))))
(test-equal "pprint-file, no decorations"
  "(define (f x)\n  (let ((y (* x x)))\n    (if (> y 100) (display \"large\") (display \"small\"))\n    (newline)\n    y))\n\n(f 10)\n\n(define lst\n  '(1\n    2\n    3\n    4\n    5\n    6\n    7\n    8\n    9\n    10\n    11\n    12\n    13\n    14\n    15\n    16\n    17\n    18\n    19\n    20\n    21\n    22\n    23\n    24\n    25\n    26\n    27\n    28\n    29\n    30))\n\n"
  (capture (lambda () (pprint-file (temp-path "srfi-272-e.scm") pp-decorate #f))))
(test-equal "pprint-file, colors"
  "\x1b;[90m;; -*- mode: scheme; fill-column: 40; pp-max-tab: 2 -*-\x1b;[0m\n\x1b;[90m;; Header comment\x1b;[0m\n\n(\x1b;[34mdefine\x1b;[0m (\x1b;[1mf\x1b;[0m \x1b;[37mx\x1b;[0m)\n  (\x1b;[34mlet\x1b;[0m ((\x1b;[37my\x1b;[0m (* x x)))\n    (\x1b;[34mif\x1b;[0m\n      (> y \x1b;[35m100\x1b;[0m)\n      (display \x1b;[32m\"large\"\x1b;[0m)\n      (display \x1b;[32m\"small\"\x1b;[0m))\n    (newline)\n    y))\n\x1b;[90m; between\x1b;[0m\n\n\n(f \x1b;[35m10\x1b;[0m) \x1b;[90m; trailing\x1b;[0m\n   \x1b;[90m;; indented comment\x1b;[0m\n(\x1b;[34mdefine\x1b;[0m \x1b;[1mlst\x1b;[0m\n  \x1b;[96m'(1\x1b;[0m\n    \x1b;[96m2\x1b;[0m\n    \x1b;[96m3\x1b;[0m\n    \x1b;[96m4\x1b;[0m\n    \x1b;[96m5\x1b;[0m\n    \x1b;[96m6\x1b;[0m\n    \x1b;[96m7\x1b;[0m\n    \x1b;[96m8\x1b;[0m\n    \x1b;[96m9\x1b;[0m\n    \x1b;[96m10\x1b;[0m\n    \x1b;[96m11\x1b;[0m\n    \x1b;[96m12\x1b;[0m\n    \x1b;[96m13\x1b;[0m\n    \x1b;[96m14\x1b;[0m\n    \x1b;[96m15\x1b;[0m\n    \x1b;[96m16\x1b;[0m\n    \x1b;[96m17\x1b;[0m\n    \x1b;[96m18\x1b;[0m\n    \x1b;[96m19\x1b;[0m\n    \x1b;[96m20\x1b;[0m\n    \x1b;[96m21\x1b;[0m\n    \x1b;[96m22\x1b;[0m\n    \x1b;[96m23\x1b;[0m\n    \x1b;[96m24\x1b;[0m\n    \x1b;[96m25\x1b;[0m\n    \x1b;[96m26\x1b;[0m\n    \x1b;[96m27\x1b;[0m\n    \x1b;[96m28\x1b;[0m\n    \x1b;[96m29\x1b;[0m\n    \x1b;[96m30)\x1b;[0m)\n"
  (parameterize ((sgr-support-tier 0))
      (capture (lambda () (pprint-file (temp-path "srfi-272-e.scm") pp-color #t)))))
(test-equal "pprint-file to file"
  ";; -*- mode: scheme; fill-column: 40; pp-max-tab: 2 -*-\n;; Header comment\n\n(define (f x)\n  (let ([y (* x x)])\n    (if\n      (> y 100)\n      (display \"large\")\n      (display \"small\"))\n    (newline)\n    y))\n; between\n\n\n(f 10) ; trailing\n   ;; indented comment\n(define lst\n  '(1\n    2\n    3\n    4\n    5\n    6\n    7\n    8\n    9\n    10\n    11\n    12\n    13\n    14\n    15\n    16\n    17\n    18\n    19\n    20\n    21\n    22\n    23\n    24\n    25\n    26\n    27\n    28\n    29\n    30))\n"
  (begin
      (pprint-file (temp-path "srfi-272-e.scm") (temp-path "srfi-272-f.scm") pp-brackets #t)
      (read-file (temp-path "srfi-272-f.scm"))))
(delete-file (temp-path "srfi-272-e.scm"))
(delete-file (temp-path "srfi-272-f.scm"))
(write-file (temp-path "srfi-272-g.scm")
  ";; -*- mode: scheme; fill-column: 50; pp-max-tab: 2; pp-brackets: t -*-\n;; * pp-styles: my-let := let; my-when := (_ e . body)\n\n(define (f x) ; doubles the square\n  ;; compute the square\n  (my-let ((y (* x x))) ; local\n    (my-when (> y 100) (display \"large\")) ; check\n    (* 2 y)))\n\n; between\n(f 10) ; trailing\n#| block comment |#\n(g #;(ignored) (a b) #\\x41 #t #x1F |sym bol| 'q `(qq ,u ,@v))\n")
(test-equal "fancy pprint-file"
  ";; -*- mode: scheme; fill-column: 50; pp-max-tab: 2; pp-brackets: t -*-\n;; * pp-styles: my-let := let; my-when := (_ e . body)\n\n(define (f x)\n  (my-let ([y (* x x)])\n    (my-when (> y 100) (display \"large\")) ; check\n    (* 2 y)))\n\n; between\n(f 10) ; trailing\n(g (a b) #\\A #t 31 |sym bol| 'q `(qq ,u ,@v))\n"
  (capture (lambda () (pprint-file (temp-path "srfi-272-g.scm")))))
(test-equal "fancy pprint-file, no decorations"
  "(define (f x)\n  (my-let ((y (* x x))) (my-when (> y 100) (display \"large\"))\n    (* 2 y)))\n\n(f 10)\n\n(g (a b) #\\A #t 31 |sym bol| 'q `(qq ,u ,@v))\n\n"
  (capture (lambda () (pprint-file (temp-path "srfi-272-g.scm") pp-decorate #f))))
(test-equal "fancy pprint-file, colors"
  "\x1b;[38;2;76;86;106m;; -*- mode: scheme; fill-column: 50; pp-max-tab: 2; pp-brackets: t -*-\x1b;[0m\n\x1b;[38;2;76;86;106m;; * pp-styles: my-let := let; my-when := (_ e . body)\x1b;[0m\n\n(\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1mf\x1b;[0m \x1b;[38;2;229;233;240mx\x1b;[0m)\n  (\x1b;[38;2;129;161;193mmy-let\x1b;[0m ([\x1b;[38;2;229;233;240my\x1b;[0m (* x x)])\n    (\x1b;[38;2;129;161;193mmy-when\x1b;[0m (> y \x1b;[38;2;180;142;173m100\x1b;[0m) (display \x1b;[38;2;163;190;140m\"large\"\x1b;[0m)) \x1b;[38;2;76;86;106m; check\x1b;[0m\n    (* \x1b;[38;2;180;142;173m2\x1b;[0m y)))\n\n\x1b;[38;2;76;86;106m; between\x1b;[0m\n(f \x1b;[38;2;180;142;173m10\x1b;[0m) \x1b;[38;2;76;86;106m; trailing\x1b;[0m\n(g (a b) \x1b;[38;2;163;190;140m#\\A\x1b;[0m \x1b;[38;2;143;188;187m#t\x1b;[0m \x1b;[38;2;180;142;173m31\x1b;[0m |sym bol| \x1b;[38;2;143;188;187m'q\x1b;[0m \x1b;[38;2;143;188;187m`(qq\x1b;[0m \x1b;[38;2;143;188;187m,\x1b;[0mu\x1b;[38;2;143;188;187m\x1b;[0m \x1b;[38;2;143;188;187m,@\x1b;[0mv\x1b;[38;2;143;188;187m)\x1b;[0m)\n"
  (parameterize ((sgr-support-tier 2))
      (capture (lambda () (pprint-file (temp-path "srfi-272-g.scm") pp-color #t)))))
(test-equal "fancy pprint-file, narrow"
  ";; -*- mode: scheme; fill-column: 50; pp-max-tab: 2; pp-brackets: t -*-\n;; * pp-styles: my-let := let; my-when := (_ e . body)\n\n(define (f x)\n  (my-let ([y (* x x)])\n    (my-when (> y 100)\n     (display \"large\")) ; check\n    (* 2 y)))\n\n; between\n(f 10) ; trailing\n(g (a b) #\\A #t 31\n   |sym bol| 'q\n   `(qq ,u ,@v))\n"
  (capture (lambda () (pprint-file (temp-path "srfi-272-g.scm") pp-width 25))))
(test-equal "fancy pprint-file to file"
  ";; -*- mode: scheme; fill-column: 50; pp-max-tab: 2; pp-brackets: t -*-\n;; * pp-styles: my-let := let; my-when := (_ e . body)\n\n(define (f x)\n  (my-let ([y (* x x)])\n    (my-when (> y 100) (display \"large\")) ; check\n    (* 2 y)))\n\n; between\n(f 10) ; trailing\n(g (a b) #\\A #t 31 |sym bol| 'q `(qq ,u ,@v))\n"
  (begin
      (pprint-file (temp-path "srfi-272-g.scm") (temp-path "srfi-272-h.scm") pp-width 60)
      (read-file (temp-path "srfi-272-h.scm"))))
(test-equal "pprint-file/html"
  "<!DOCTYPE html>\n<html>\n<head>\n<style>\n  body { background-color: #1c1f26; color: #d4d4d4; margin: 0; padding: 2em; }\n  pre {\n    font-family: 'Consolas', 'Monaco', 'Andale Mono', 'Ubuntu Mono', monospace;\n    font-size: 10pt; line-height: 2.75ex; white-space: pre-wrap;\n  }\n  .comment { color: #4c566a; }\n  .char { color: #a3be8c; }\n  .string { color: #a3be8c; }\n  .escape { color: #ebcb8b; }\n  .formal { color: #e5e9f0; }\n  .defined { color: #eceff4; font-weight: bold; }\n  .keyword { color: #81a1c1; }\n  .number { color: #b48ead; }\n  .literal { color: #8fbcbb; }\n  .meta { color: #88c0d0; }\n  .directive { color: #b48ead; }\n  .paren { color: #e5e9f0; }\n  .warning { color: #bf616a; }\n</style>\n</head>\n<body>\n<pre>;; -*- mode: scheme; fill-column: 50; pp-max-tab: 2; pp-brackets: t -*-\n;; * pp-styles: my-let := let; my-when := (_ e . body)\n\n(<span class=\"keyword\">define</span> (<span class=\"defined\">f</span> <span class=\"formal\">x</span>)\n  (<span class=\"keyword\">my-let</span> ([<span class=\"formal\">y</span> (* x x)])\n    (<span class=\"keyword\">my-when</span> (&gt; y <span class=\"number\">100</span>) (display <span class=\"string\">\"large\"</span>)) <span class=\"comment\">; check</span>\n    (* <span class=\"number\">2</span> y)))\n\n; between\n(f <span class=\"number\">10</span>) ; trailing\n(g (a b) <span class=\"char\">#\\A</span> <span class=\"literal\">#t</span> <span class=\"number\">31</span> |sym bol| <span class=\"literal\">'q</span> <span class=\"literal\">`(qq</span> <span class=\"literal\">,</span>u<span class=\"literal\"></span> <span class=\"literal\">,@</span>v<span class=\"literal\">)</span>)\n</pre>\n</body>\n</html>\n"
  (capture (lambda () (pprint-file/html (temp-path "srfi-272-g.scm")))))
(test-equal "pprint-file/html, colors"
  "<!DOCTYPE html>\n<html>\n<head>\n<style>\n  body { background-color: #1c1f26; color: #d4d4d4; margin: 0; padding: 2em; }\n  pre {\n    font-family: 'Consolas', 'Monaco', 'Andale Mono', 'Ubuntu Mono', monospace;\n    font-size: 10pt; line-height: 2.75ex; white-space: pre-wrap;\n  }\n  .comment { color: #4c566a; }\n  .char { color: #a3be8c; }\n  .string { color: #a3be8c; }\n  .escape { color: #ebcb8b; }\n  .formal { color: #e5e9f0; }\n  .defined { color: #eceff4; font-weight: bold; }\n  .keyword { color: #81a1c1; }\n  .number { color: #b48ead; }\n  .literal { color: #8fbcbb; }\n  .meta { color: #88c0d0; }\n  .directive { color: #b48ead; }\n  .paren { color: #e5e9f0; }\n  .warning { color: #bf616a; }\n</style>\n</head>\n<body>\n<pre><span class=\"comment\">;; -*- mode: scheme; fill-column: 50; pp-max-tab: 2; pp-brackets: t -*-</span>\n<span class=\"comment\">;; * pp-styles: my-let := let; my-when := (_ e . body)</span>\n\n(<span class=\"keyword\">define</span> (<span class=\"defined\">f</span> <span class=\"formal\">x</span>)\n  (<span class=\"keyword\">my-let</span> ([<span class=\"formal\">y</span> (* x x)])\n    (<span class=\"keyword\">my-when</span> (&gt; y <span class=\"number\">100</span>) (display <span class=\"string\">\"large\"</span>)) <span class=\"comment\">; check</span>\n    (* <span class=\"number\">2</span> y)))\n\n<span class=\"comment\">; between</span>\n(f <span class=\"number\">10</span>) <span class=\"comment\">; trailing</span>\n(g (a b) <span class=\"char\">#\\A</span> <span class=\"literal\">#t</span> <span class=\"number\">31</span> |sym bol| <span class=\"literal\">'q</span> <span class=\"literal\">`(qq</span> <span class=\"literal\">,</span>u<span class=\"literal\"></span> <span class=\"literal\">,@</span>v<span class=\"literal\">)</span>)\n</pre>\n</body>\n</html>\n"
  (capture (lambda () (pprint-file/html (temp-path "srfi-272-g.scm") pp-color #t))))
(test-equal "pprint-file/html, no decorations, colors"
  "<!DOCTYPE html>\n<html>\n<head>\n<style>\n  body { background-color: #1c1f26; color: #d4d4d4; margin: 0; padding: 2em; }\n  pre {\n    font-family: 'Consolas', 'Monaco', 'Andale Mono', 'Ubuntu Mono', monospace;\n    font-size: 10pt; line-height: 2.75ex; white-space: pre-wrap;\n  }\n  .comment { color: #4c566a; }\n  .char { color: #a3be8c; }\n  .string { color: #a3be8c; }\n  .escape { color: #ebcb8b; }\n  .formal { color: #e5e9f0; }\n  .defined { color: #eceff4; font-weight: bold; }\n  .keyword { color: #81a1c1; }\n  .number { color: #b48ead; }\n  .literal { color: #8fbcbb; }\n  .meta { color: #88c0d0; }\n  .directive { color: #b48ead; }\n  .paren { color: #e5e9f0; }\n  .warning { color: #bf616a; }\n</style>\n</head>\n<body>\n<pre>(<span class=\"keyword\">define</span> (<span class=\"defined\">f</span> <span class=\"formal\">x</span>)\n  (my-let ((y (* x x))) (my-when (&gt; y <span class=\"number\">100</span>) (display <span class=\"string\">\"large\"</span>))\n    (* <span class=\"number\">2</span> y)))\n\n(f <span class=\"number\">10</span>)\n\n(g (a b) <span class=\"char\">#\\A</span> <span class=\"literal\">#t</span> <span class=\"number\">31</span> |sym bol| <span class=\"literal\">'q</span> <span class=\"literal\">`(qq</span> <span class=\"literal\">,</span>u<span class=\"literal\"></span> <span class=\"literal\">,@</span>v<span class=\"literal\">)</span>)\n\n</pre>\n</body>\n</html>\n"
  (capture (lambda () (pprint-file/html (temp-path "srfi-272-g.scm") pp-color #t pp-decorate #f))))
(test-equal "pprint-file/html to file"
  "<!DOCTYPE html>\n<html>\n<head>\n<style>\n  body { background-color: #1c1f26; color: #d4d4d4; margin: 0; padding: 2em; }\n  pre {\n    font-family: 'Consolas', 'Monaco', 'Andale Mono', 'Ubuntu Mono', monospace;\n    font-size: 10pt; line-height: 2.75ex; white-space: pre-wrap;\n  }\n  .comment { color: #4c566a; }\n  .char { color: #a3be8c; }\n  .string { color: #a3be8c; }\n  .escape { color: #ebcb8b; }\n  .formal { color: #e5e9f0; }\n  .defined { color: #eceff4; font-weight: bold; }\n  .keyword { color: #81a1c1; }\n  .number { color: #b48ead; }\n  .literal { color: #8fbcbb; }\n  .meta { color: #88c0d0; }\n  .directive { color: #b48ead; }\n  .paren { color: #e5e9f0; }\n  .warning { color: #bf616a; }\n</style>\n</head>\n<body>\n<pre>;; -*- mode: scheme; fill-column: 50; pp-max-tab: 2; pp-brackets: t -*-\n;; * pp-styles: my-let := let; my-when := (_ e . body)\n\n(<span class=\"keyword\">define</span> (<span class=\"defined\">f</span> <span class=\"formal\">x</span>)\n  (<span class=\"keyword\">my-let</span> ([<span class=\"formal\">y</span> (* x x)])\n    (<span class=\"keyword\">my-when</span> (&gt; y <span class=\"number\">100</span>) (display <span class=\"string\">\"large\"</span>)) <span class=\"comment\">; check</span>\n    (* <span class=\"number\">2</span> y)))\n\n; between\n(f <span class=\"number\">10</span>) ; trailing\n(g (a b) <span class=\"char\">#\\A</span> <span class=\"literal\">#t</span> <span class=\"number\">31</span> |sym bol| <span class=\"literal\">'q</span> <span class=\"literal\">`(qq</span> <span class=\"literal\">,</span>u<span class=\"literal\"></span> <span class=\"literal\">,@</span>v<span class=\"literal\">)</span>)\n</pre>\n</body>\n</html>\n"
  (begin
      (pprint-file/html (temp-path "srfi-272-g.scm") (temp-path "srfi-272-h.html"))
      (read-file (temp-path "srfi-272-h.html"))))
(test-equal "styles from style comments are file-scoped"
  #f
  (pretty-style 'my-let))
(write-file (temp-path "srfi-272-i.scm")
  "(let loop [(i 0) (acc '())] ; brackets are read by the decorated reader\n  (if (> i 3) acc (loop (+ i 1) (cons [list i] acc))))\n")
(test-equal "fancy pprint-file, brackets in source"
  "(let loop ((i 0) (acc '()))\n  (if (> i 3)\n      acc\n      (loop (+ i 1) (cons (list i) acc))))\n"
  (capture (lambda () (pprint-file (temp-path "srfi-272-i.scm") pp-width 40))))
(test-equal "fancy pprint-file, brackets in source and output"
  "(let loop ([i 0] [acc '()])\n  (if (> i 3)\n      acc\n      (loop (+ i 1) (cons (list i) acc))))\n"
  (capture (lambda () (pprint-file (temp-path "srfi-272-i.scm") pp-width 40 pp-brackets #t))))
(delete-file (temp-path "srfi-272-i.scm"))
(delete-file (temp-path "srfi-272-g.scm"))
(delete-file (temp-path "srfi-272-h.scm"))
(delete-file (temp-path "srfi-272-h.html"))
(write-file (temp-path "srfi-272-j.scm")
  ";; Top-level comment\n(define (transfer! from to amount) ; c1: after head\n  ;; c2: before first body form\n  (check-balance from amount) ; c3: after first body form\n  ;; c4: before second body form\n  (withdraw! from amount)\n\n  ;; c5: before third body form, after empty line\n  (deposit! to amount)) ; c6: after last form\n; c7: between top-level forms\n(transfer! a b 100) ; c8: after top-level form\n(define (report)\n  (display \"a\")\n  (newline)\n  (display \"b\")\n  (newline) ; second newline\n  (display \"c\")\n  (newline)) ; done\n")
(test-equal "comments"
  ";; Top-level comment\n(define (transfer! from to amount)\n  (check-balance from amount) ; c3: after first body form\n  ;; c4: before second body form\n  (withdraw! from amount)\n\n  ;; c5: before third body form, after empty line\n  (deposit! to amount)) ; c6: after last form\n; c7: between top-level forms\n(transfer! a b 100) ; c8: after top-level form\n(define (report)\n  (display \"a\")\n  (newline)\n  (display \"b\")\n  (newline) ; second newline\n  (display \"c\")\n  (newline)) ; done\n"
  (capture (lambda () (pprint-file (temp-path "srfi-272-j.scm")))))
(test-equal "comments, html"
  "<!DOCTYPE html>\n<html>\n<head>\n<style>\n  body { background-color: #1c1f26; color: #d4d4d4; margin: 0; padding: 2em; }\n  pre {\n    font-family: 'Consolas', 'Monaco', 'Andale Mono', 'Ubuntu Mono', monospace;\n    font-size: 10pt; line-height: 2.75ex; white-space: pre-wrap;\n  }\n  .comment { color: #4c566a; }\n  .char { color: #a3be8c; }\n  .string { color: #a3be8c; }\n  .escape { color: #ebcb8b; }\n  .formal { color: #e5e9f0; }\n  .defined { color: #eceff4; font-weight: bold; }\n  .keyword { color: #81a1c1; }\n  .number { color: #b48ead; }\n  .literal { color: #8fbcbb; }\n  .meta { color: #88c0d0; }\n  .directive { color: #b48ead; }\n  .paren { color: #e5e9f0; }\n  .warning { color: #bf616a; }\n</style>\n</head>\n<body>\n<pre><span class=\"comment\">;; Top-level comment</span>\n(<span class=\"keyword\">define</span> (<span class=\"defined\">transfer!</span> <span class=\"formal\">from</span> <span class=\"formal\">to</span> <span class=\"formal\">amount</span>)\n  (check-balance from amount) <span class=\"comment\">; c3: after first body form</span>\n  <span class=\"comment\">;; c4: before second body form</span>\n  (withdraw! from amount)\n\n  <span class=\"comment\">;; c5: before third body form, after empty line</span>\n  (deposit! to amount)) <span class=\"comment\">; c6: after last form</span>\n<span class=\"comment\">; c7: between top-level forms</span>\n(transfer! a b <span class=\"number\">100</span>) <span class=\"comment\">; c8: after top-level form</span>\n(<span class=\"keyword\">define</span> (<span class=\"defined\">report</span>)\n  (display <span class=\"string\">\"a\"</span>)\n  (newline)\n  (display <span class=\"string\">\"b\"</span>)\n  (newline) <span class=\"comment\">; second newline</span>\n  (display <span class=\"string\">\"c\"</span>)\n  (newline)) <span class=\"comment\">; done</span>\n</pre>\n</body>\n</html>\n"
  (capture (lambda () (pprint-file/html (temp-path "srfi-272-j.scm") pp-color #t))))
(test-equal "comments, colors"
  "\x1b;[90m;; Top-level comment\x1b;[0m\n(\x1b;[34mdefine\x1b;[0m (\x1b;[1mtransfer!\x1b;[0m \x1b;[37mfrom\x1b;[0m \x1b;[37mto\x1b;[0m \x1b;[37mamount\x1b;[0m)\n  (check-balance from amount) \x1b;[90m; c3: after first body form\x1b;[0m\n  \x1b;[90m;; c4: before second body form\x1b;[0m\n  (withdraw! from amount)\n\n  \x1b;[90m;; c5: before third body form, after empty line\x1b;[0m\n  (deposit! to amount)) \x1b;[90m; c6: after last form\x1b;[0m\n\x1b;[90m; c7: between top-level forms\x1b;[0m\n(transfer! a b \x1b;[35m100\x1b;[0m) \x1b;[90m; c8: after top-level form\x1b;[0m\n(\x1b;[34mdefine\x1b;[0m (\x1b;[1mreport\x1b;[0m)\n  (display \x1b;[32m\"a\"\x1b;[0m)\n  (newline)\n  (display \x1b;[32m\"b\"\x1b;[0m)\n  (newline) \x1b;[90m; second newline\x1b;[0m\n  (display \x1b;[32m\"c\"\x1b;[0m)\n  (newline)) \x1b;[90m; done\x1b;[0m\n"
  (parameterize ((sgr-support-tier 0))
      (capture (lambda () (pprint-file (temp-path "srfi-272-j.scm") pp-color #t)))))
(delete-file (temp-path "srfi-272-j.scm")))

(test-group "LispKit-specific tests"

(test-equal "boxes" "(#<box 1> #<box (a b)>)\n" (pp->string (list (box 1) (box '(a b)))))
(test-equal "boxes, line breaks" "(#<box alpha>\n #<box beta>\n #<box gamma>\n #<box delta>)\n"
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
(test-equal "invalid pp-level" 'error (guard (e (#t 'error)) (pp->string 1 pp-level -1)))
(test-equal "invalid style object" 'error
  (guard (e (#t 'error)) (add-pp-style (pp-styles) 'foo '(bar))))
(test-equal "invalid pp-color" 'error
  (guard (e (#t 'error)) (pp->string 1 pp-color 'red)))
(test-equal "invalid pp-lines" 'error
  (guard (e (#t 'error)) (pp->string 1 pp-lines 0)))
(test-equal "pp-lines, exact fit" "(a\n b\n c)\n"
  (pp->string '(a b c) pp-width 3 pp-lines 3))
(test-equal "pp-lines-stub #t" "(define-syntax set*!\n  (lambda (x)\n    (letrec ...)))\n"
  (pp->string sexp4 pp-lines 3 pp-lines-stub #t))
(test-equal "LispKit hooks are registered" #t
  (and (lookup-pp-hook (pp-hooks) box?) (lookup-pp-hook (pp-hooks) mpair?)
       (lookup-pp-hook (pp-hooks) gvector?) #t))
; like vectors, boxes are printed as data, i.e. without colors
(test-equal "boxes in code, colors" "(f #<box 1>)\n"
  (parameterize ((sgr-support-tier 0))
    (pp->string (list 'f (box 1)) pp-color #t)))
(test-equal "fancy shares bindings with advanced" #t
  (and (eq? pp adv:pp) (eq? pp-width adv:pp-width) (eq? pp-styles adv:pp-styles)
       (eq? pp-hooks adv:pp-hooks) (eq? make-pprint-generator adv:make-pprint-generator)
       (not (eq? pprint-file adv:pprint-file))))
(define-record-type srfi-272-node (make-srfi-272-node val next) srfi-272-node?
  (val srfi-272-node-val) (next srfi-272-node-next set-srfi-272-node-next!))
(test-equal "hashtable" "#<hashtable (a (1 2 3))>\n"
  (pp->string (let ((h (make-equal-hashtable))) (hashtable-set! h 'a '(1 2 3)) h)))
(test-equal "empty hashtable" "#<hashtable>\n" (pp->string (make-equal-hashtable)))
(test-equal "cyclic hashtable" "#0=#<hashtable (self #0#)>\n"
  (pp->string (let ((h (make-eq-hashtable))) (hashtable-set! h 'self h) h)))
(test-equal "cycle through vector and hashtable" "#0=#(#<hashtable (v #0#)> 1)\n"
  (pp->string (let ((h (make-eq-hashtable)) (v (vector #f 1)))
                (vector-set! v 0 h) (hashtable-set! h 'v v) v)))
(test-equal "shared hashtables" "(#0=#<hashtable (a 1)> #0#)\n"
  (pp->string (let ((h (make-eq-hashtable))) (hashtable-set! h 'a 1) (list h h)) pp-graph #t))
(test-equal "record" "#<record srfi-272-node (val 1) (next #f)>\n"
  (pp->string (make-srfi-272-node 1 #f)))
(test-equal "cyclic record" "#0=#<record srfi-272-node (val 1) (next #0#)>\n"
  (pp->string (let ((n (make-srfi-272-node 1 #f))) (set-srfi-272-node-next! n n) n)))
(test-equal "shared records" "(#0=#<record srfi-272-node (val 1) (next #f)> #0#)\n"
  (pp->string (let ((n (make-srfi-272-node 1 #f))) (list n n)) pp-graph #t))
(test-equal "records, line breaks"
  "#<record srfi-272-node\n         (val (alpha beta gamma delta))\n         (next #f)>\n"
  (pp->string (make-srfi-272-node '(alpha beta gamma delta) #f) pp-width 40))
(test-equal "records, length" "#<record srfi-272-node (val 1) ...>\n"
  (pp->string (make-srfi-272-node 1 #f) pp-length 2))
(test-equal "records, level" "#<record srfi-272-node (val 1) (next #<record ...>)>\n"
  (pp->string (make-srfi-272-node 1 (make-srfi-272-node 2 #f)) pp-level 2))
(test-equal "records, level 1" "(#<record ...>)\n"
  (pp->string (list (make-srfi-272-node 1 #f)) pp-level 1)))

(test-end)
