;;; SRFI 272 BASIC REGRESSION TEST SUITE
;;;
;;; This is the test suite for library `(srfi 272 basic)`.
;;;
;;; The first part of this test suite is based on the test suite of the SRFI 272
;;; sample implementation. Inputs with datum labels are read via `read-labeled`
;;; (see "SRFI-272-Support.scm"); tests whose inputs contain circular lists are
;;; skipped since such lists cannot be represented in LispKit.
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
        (srfi 272 basic)
        (lispkit gvector))

(load "Tests/SRFI-272-Support")

(define (pp-test llen input expected)
  (let ((obj (read-labeled input)))
    (if (eq? obj unrepresentable)
        (srfi-272-skip!)
        (test-equal input expected (pp->string obj pp-width llen)))))

(test-begin "SRFI 272: Basic pretty printing")

(test-group "Tests of the sample implementation"

(pp-test 40 "1" "1\n")
(pp-test 40 "'(a b)" "'(a b)\n")
(pp-test 40 "'(a . b)" "'(a . b)\n")
(pp-test 40 "`(,a ,@b)" "`(,a ,@b)\n")
(pp-test 80
  "(let ((x 1) (y 2) (zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz 3)) (display x) (display y))"
  "(let\n ((x 1) (y 2) (zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz 3))\n (display x)\n (display y))\n")

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
  "((1 . 2)\n (1 . 2)\n (3 . 4)\n (1 . 2)\n (1 . 2)\n (3 . 4))\n")
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

; LispKit: boxes are printed like `write` prints them
(pp-test 100 "#0=#&(#1=(a) #1# . #0#)"
  "#0=#<box ((a) (a) . #0#)>\n")
; bytevectors
(pp-test 10 "#u8(1 2 3 4 5 6 7 8 9 10)"
  "#u8(1\n    2\n    3\n    4\n    5\n    6\n    7\n    8\n    9\n    10)\n")

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
    "((1 . 2)\n (1 . 2)\n (3 . 4)\n (1 . 2)\n (1 . 2)\n (3 . 4))\n")
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

(newline)
(srfi-272-report-skipped "SRFI 272 basic"))

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
  (guard (e (#t 'error)) (pp->string 'x pp-width))))

(test-end)
