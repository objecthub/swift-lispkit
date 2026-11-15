;;; SRFI 272 COLORIZE REGRESSION TEST SUITE
;;;
;;; This is the test suite for library `(srfi 272 colorize)`.
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
        (srfi 272 colorize))

(load "Tests/SRFI-272-Support")

(test-begin "SRFI 272: Color support")

(test-group "Golden tests"

(test-equal "sgr0"
  '("31" "91" "31;1" "31;4" "31;1;4;44" "97;104")
  (list (sgr0 1) (sgr0 9) (sgr0 1 #t) (sgr0 1 #f #t) (sgr0 1 #t #t 4) (sgr0 15 #f #f 12)))
(test-equal "sgr1"
  '("38;5;0" "38;5;255;1" "38;5;100;4;48;5;20")
  (list (sgr1 0) (sgr1 255 #t) (sgr1 100 #f #t 20)))
(test-equal "sgr2"
  '("38;2;16;32;48" "38;2;255;255;255;1;4;3;9;48;2;0;0;0")
  (list (sgr2 #x102030) (sgr2 #xFFFFFF #t #t #t #t #x000000)))
(test-equal "sgr3"
  '("38;2;16;32;48" "38;2;16;32;48;1;2;4:3;3;9;48;2;64;80;96" "38;2;0;0;0;4:5;58:2:255:0:0" "38;2;0;0;0;21" "38;2;0;0;0;24")
  (list (sgr3 #x102030) (sgr3 #x102030 #t #t 'curly #t #t #x405060)
                       (sgr3 0 #f #f '(dashed . #xFF0000)) (sgr3 0 #f #f 2) (sgr3 0 #f #f 'off)))
(test-equal "make-asb"
  '(#("1" "1" "1" "1") #("1" "2" "2" "2") #("1" "2" "3" "3") #("1" "2" "3" "4"))
  (list (make-asb "1") (make-asb "1" "2") (make-asb "1" "2" "3") (make-asb "1" "2" "3" "4")))
(test-equal "asb->sgr-string"
  '("\x1b;[31m" "\x1b;[38;5;131m" "\x1b;[38;2;191;97;106m" "\x1b;[38;2;191;97;106;1m" "")
  (let ((asb (make-asb (sgr0 1) (sgr1 131) (sgr2 #xBF616A) (sgr3 #xBF616A #t))))
      (list (asb->sgr-string asb 0) (asb->sgr-string asb 1) (asb->sgr-string asb 2)
            (asb->sgr-string asb 3) (asb->sgr-string asb #f))))
(test-equal "asb->sgr-string with parameter"
  "\x1b;[38;5;131m"
  (parameterize ((sgr-support-tier 1)) (asb->sgr-string (make-asb "31" "38;5;131"))))
(test-equal "default-asb-palette"
  '(#("30" "38;5;237" "38;2;59;66;82" "38;2;59;66;82") #("31" "38;5;131" "38;2;191;97;106" "38;2;191;97;106") #("32" "38;5;144" "38;2;163;190;140" "38;2;163;190;140") #("33" "38;5;186" "38;2;235;203;139" "38;2;235;203;139") #("34" "38;5;109" "38;2;129;161;193" "38;2;129;161;193") #("35" "38;5;139" "38;2;180;142;173" "38;2;180;142;173") #("36" "38;5;110" "38;2;136;192;208" "38;2;136;192;208") #("37" "38;5;254" "38;2;229;233;240" "38;2;229;233;240") #("90" "38;5;240" "38;2;76;86;106" "38;2;76;86;106") #("31" "38;5;131" "38;2;191;97;106" "38;2;191;97;106") #("32" "38;5;144" "38;2;163;190;140" "38;2;163;190;140") #("33" "38;5;186" "38;2;235;203;139" "38;2;235;203;139") #("34" "38;5;109" "38;2;129;161;193" "38;2;129;161;193") #("35" "38;5;139" "38;2;180;142;173" "38;2;180;142;173") #("96" "38;5;109" "38;2;143;188;187" "38;2;143;188;187") #("97" "38;5;255" "38;2;236;239;244" "38;2;236;239;244"))
  (let loop ((i 0) (acc '()))
      (if (= i 16) (reverse acc) (loop (+ i 1) (cons (asb-palette-ref default-asb-palette i) acc)))))
(test-equal "make-asb-palette with 16 bundles"
  '#("13" "13" "13" "13")
  (asb-palette-ref (make-asb-palette (make-asb "0") (make-asb "1") (make-asb "2") (make-asb "3")
                                       (make-asb "4") (make-asb "5") (make-asb "6") (make-asb "7")
                                       (make-asb "8") (make-asb "9") (make-asb "10") (make-asb "11")
                                       (make-asb "12") (make-asb "13") (make-asb "14") (make-asb "15"))
                     13))
(test-equal "semantic-color-mapper?"
  '(#t #f)
  (list (semantic-color-mapper? default-semantic-color-mapper)
                                         (semantic-color-mapper? 'foo)))
(define semantic-colors
  '(comment char string escape char-escape string-escape char-meta string-meta symbol-escape
    formal variable defined keyword number literal literal-meta meta directive paren bracket
    warning unknown))
(test-equal "start strings, tier 0"
  '("\x1b;[90m" "\x1b;[32m" "\x1b;[32m" "\x1b;[33m" "\x1b;[33m" "\x1b;[33m" "\x1b;[32m" "\x1b;[32m" "\x1b;[33m" "\x1b;[37m" "" "\x1b;[1m" "\x1b;[34m" "\x1b;[35m" "\x1b;[96m" "\x1b;[36m" "\x1b;[36m" "\x1b;[35m" "\x1b;[37m" "" "\x1b;[31m" "")
  (map (lambda (sc) (semantic-color->start-string sc default-semantic-color-mapper 0)) semantic-colors))
(test-equal "start strings, tier 1"
  '("\x1b;[38;5;240m" "\x1b;[38;5;144m" "\x1b;[38;5;144m" "\x1b;[38;5;186m" "\x1b;[38;5;186m" "\x1b;[38;5;186m" "\x1b;[38;5;144m" "\x1b;[38;5;144m" "\x1b;[38;5;186m" "\x1b;[38;5;254m" "" "\x1b;[1m" "\x1b;[38;5;109m" "\x1b;[38;5;139m" "\x1b;[38;5;109m" "\x1b;[38;5;110m" "\x1b;[38;5;110m" "\x1b;[38;5;139m" "\x1b;[38;5;254m" "" "\x1b;[38;5;131m" "")
  (map (lambda (sc) (semantic-color->start-string sc default-semantic-color-mapper 1)) semantic-colors))
(test-equal "start strings, tier 2"
  '("\x1b;[38;2;76;86;106m" "\x1b;[38;2;163;190;140m" "\x1b;[38;2;163;190;140m" "\x1b;[38;2;235;203;139m" "\x1b;[38;2;235;203;139m" "\x1b;[38;2;235;203;139m" "\x1b;[38;2;163;190;140m" "\x1b;[38;2;163;190;140m" "\x1b;[38;2;235;203;139m" "\x1b;[38;2;229;233;240m" "" "\x1b;[1m" "\x1b;[38;2;129;161;193m" "\x1b;[38;2;180;142;173m" "\x1b;[38;2;143;188;187m" "\x1b;[38;2;136;192;208m" "\x1b;[38;2;136;192;208m" "\x1b;[38;2;180;142;173m" "\x1b;[38;2;229;233;240m" "" "\x1b;[38;2;191;97;106m" "")
  (map (lambda (sc) (semantic-color->start-string sc default-semantic-color-mapper 2)) semantic-colors))
(test-equal "start strings, tier 3"
  '("\x1b;[38;2;76;86;106m" "\x1b;[38;2;163;190;140m" "\x1b;[38;2;163;190;140m" "\x1b;[38;2;235;203;139m" "\x1b;[38;2;235;203;139m" "\x1b;[38;2;235;203;139m" "\x1b;[38;2;163;190;140m" "\x1b;[38;2;163;190;140m" "\x1b;[38;2;235;203;139m" "\x1b;[38;2;229;233;240m" "" "\x1b;[1m" "\x1b;[38;2;129;161;193m" "\x1b;[38;2;180;142;173m" "\x1b;[38;2;143;188;187m" "\x1b;[38;2;136;192;208m" "\x1b;[38;2;136;192;208m" "\x1b;[38;2;180;142;173m" "\x1b;[38;2;229;233;240m" "" "\x1b;[38;2;191;97;106m" "")
  (map (lambda (sc) (semantic-color->start-string sc default-semantic-color-mapper 3)) semantic-colors))
(test-equal "start strings, no tier"
  '("" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "")
  (map (lambda (sc) (semantic-color->start-string sc default-semantic-color-mapper #f)) semantic-colors))
(test-equal "end strings"
  '("\x1b;[0m" "\x1b;[0m" "\x1b;[0m" "\x1b;[0m" "")
  (map (lambda (tier) (semantic-color->end-string 'comment default-semantic-color-mapper tier)) '(0 1 2 3 #f)))
(test-equal "start string with parameter"
  "\x1b;[34m"
  (parameterize ((sgr-support-tier 0)) (semantic-color->start-string 'keyword)))
(test-equal "custom palette mapper"
  '("\x1b;[31m" "\x1b;[32m" "\x1b;[0m")
  (let ((cm (make-semantic-color-mapper default-asb-palette
                (lambda (sc pal) (if (eq? sc 'keyword) (asb-palette-ref pal 1) (asb-palette-ref pal 2))))))
      (list (semantic-color->start-string 'keyword cm 0) (semantic-color->start-string 'number cm 0)
            (semantic-color->end-string 'number cm 0))))
(test-equal "string mapper"
  '("<keyword>" "</>" "<keyword>")
  (let ((cm (lambda (sc start?) (if start? (string-append "<" (symbol->string sc) ">") "</>"))))
      (list (semantic-color->start-string 'keyword cm 0) (semantic-color->end-string 'keyword cm 0)
            (semantic-color->start-string 'keyword cm #f)))))

(test-group "LispKit-specific tests"

(test-equal "make-asb-palette with 8 bundles"
  '("0" "7" "0" "7")
  (let ((pal (make-asb-palette (make-asb "0") (make-asb "1") (make-asb "2") (make-asb "3")
                               (make-asb "4") (make-asb "5") (make-asb "6") (make-asb "7"))))
    (map (lambda (i) (vector-ref (asb-palette-ref pal i) 0)) '(0 7 8 15))))
(test-equal "detect-sgr-tier" #t (and (memv (detect-sgr-tier) '(#f 0 1 2 3)) #t))
(test-equal "sgr-support-tier" #t (and (memv (sgr-support-tier) '(#f 0 1 2 3)) #t))
(test-equal "sgr-support-tier is a parameter" 3
  (parameterize ((sgr-support-tier 3)) (sgr-support-tier)))
(test-equal "invalid color" 'error (guard (e (#t 'error)) (sgr0 16)))
(test-equal "invalid tier" 'error (guard (e (#t 'error)) (asb->sgr-string (make-asb "1") 4))))

(test-end)
