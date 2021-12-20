;;; SRFI 166 REGRESSION TEST SUITE
;;;
;;; This is the test suite for SRFI 166.
;;;
;;; Copyright © 2020 Marc Nieper-Wißkirchen. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify,
;;; merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies
;;; or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
;;; PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT
;;; OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2021 Matthias Zenger. All rights reserved.

(import (lispkit base)
        (lispkit test)
        (srfi 166))

(define-syntax test-show
  (syntax-rules ()
    ((test-show expected formatter)
      (test-equal expected (show #f formatter)))
    ((test-show name expected formatter)
      (test-equal name expected (show #f formatter)))))

(test-begin "SRFI 166: Monadic Formatting")

;;; Basic tests.

(test-show "hi" "hi")
(test-show "\"hi\"" (written "hi"))
(test-show "\"hi \\\"bob\\\"\"" (written "hi \"bob\""))
(test-show "\"hello\\nworld\"" (written "hello\nworld"))
(test-show "#(1 2 3)" (written '#(1 2 3)))
(test-show "(1 2 3)" (written '(1 2 3)))
(test-show "(1 2 . 3)" (written '(1 2 . 3)))
(test-show "ABC" (upcased "abc"))
(test-show "abc" (downcased "ABC"))

(test-show "a    b" (each "a" (space-to 5) "b"))
(test-show "ab" (each "a" (space-to 0) "b"))

(test-show "abc     def" (each "abc" (tab-to) "def"))
(test-show "abc  def" (each "abc" (tab-to 5) "def"))
(test-show "abcdef" (each "abc" (tab-to 3) "def"))
(test-show "abc\ndef\n" (each "abc" nl "def" nl))
(test-show "abc\ndef\n" (each "abc" fl "def" nl fl))
(test-show "abc\ndef\n" (each "abc" fl "def" fl fl))

(test-show "ab" (each "a" nothing "b"))

;;; Escaping.

(test-show "hi, bob!" (escaped "hi, bob!"))
(test-show "hi, \\\"bob!\\\"" (escaped "hi, \"bob!\""))
(test-show "hi, \\'bob\\'" (escaped "hi, 'bob'" #\'))
(test-show "hi, ''bob''" (escaped "hi, 'bob'" #\' #\'))
(test-show "hi, ''bob''" (escaped "hi, 'bob'" #\' #f))
(test-show "line1\\nline2\\nkapow\\a\\n"
           (escaped "line1\nline2\nkapow\a\n"
                    #\" #\\
                    (lambda (c) (case c ((#\newline) #\n) ((#\alarm) #\a) (else #f)))))

(test-show "bob" (maybe-escaped "bob" char-whitespace?))
(test-show "\"hi, bob!\""
           (maybe-escaped "hi, bob!" char-whitespace?))
(test-show "\"foo\\\"bar\\\"baz\"" (maybe-escaped "foo\"bar\"baz" char-whitespace?))
(test-show "'hi, ''bob'''" (maybe-escaped "hi, 'bob'" (lambda (c) #f) #\' #f))
(test-show "\\" (maybe-escaped "\\" (lambda (c) #f) #\' #f))
(test-show "''''" (maybe-escaped "'" (lambda (c) #f) #\' #f))

;;; Numbers.

(test-show "-1" -1)
(test-show "0" 0)
(test-show "1" 1)
(test-show "10" 10)
(test-show "100" 100)
(test-show "-1" (numeric -1))
(test-show "0" (numeric 0))
(test-show "1" (numeric 1))
(test-show "10" (numeric 10))
(test-show "100" (numeric 100))
(test-show "57005" #xDEAD)
(test-show "#xdead" (with ((radix 16)) #xDEAD))
(test-show "#xdead1234" (with ((radix 16)) #xDEAD1234))
(test-show "de.ad"
           (with ((radix 16) (precision 2)) (numeric (/ #xDEAD #x100))))
(test-show "d.ead"
           (with ((radix 16) (precision 3)) (numeric (/ #xDEAD #x1000))))
(test-show "0.dead"
           (with ((radix 16) (precision 4)) (numeric (/ #xDEAD #x10000))))
(test-show "1g"
           (with ((radix 17)) (numeric 33)))

(test-show "3.14159" 3.14159)
(test-show "3.14" (with ((precision 2)) 3.14159))
(test-show "3.14" (with ((precision 2)) 3.14))
(test-show "3.00" (with ((precision 2)) 3.))
(test-show "1.10" (with ((precision 2)) 1.099))
(test-show "0.00" (with ((precision 2)) 1e-17))
(test-show "0.0000000010" (with ((precision 10)) 1e-9))
(test-show "0.0000000000" (with ((precision 10)) 1e-17))
(test-show "0.000004" (with ((precision 6)) 0.000004))
(test-show "0.0000040" (with ((precision 7)) 0.000004))
(test-show "0.00000400" (with ((precision 8)) 0.000004))
(test-show "1.00" (with ((precision 2)) .997554209949891))
(test-show "1.00" (with ((precision 2)) .99755420))
(test-show "1.00" (with ((precision 2)) .99755))
(test-show "1.00" (with ((precision 2)) .997))
(test-show "0.99" (with ((precision 2)) .99))
(test-show "-15" (with ((precision 0)) -14.99995999999362))

(test-show "   3.14159" (with ((decimal-align 5)) (numeric 3.14159)))
(test-show "  31.4159" (with ((decimal-align 5)) (numeric 31.4159)))
(test-show " 314.159" (with ((decimal-align 5)) (numeric 314.159)))
(test-show "3141.59" (with ((decimal-align 5)) (numeric 3141.59)))
(test-show "31415.9" (with ((decimal-align 5)) (numeric 31415.9)))
(test-show "  -3.14159" (with ((decimal-align 5)) (numeric -3.14159)))
(test-show " -31.4159" (with ((decimal-align 5)) (numeric -31.4159)))
(test-show "-314.159" (with ((decimal-align 5)) (numeric -314.159)))
(test-show "-3141.59" (with ((decimal-align 5)) (numeric -3141.59)))
(test-show "-31415.9" (with ((decimal-align 5)) (numeric -31415.9)))

(test-show "333.333333333333333333333333333333"
           (with ((precision 30)) (numeric 1000/3)))
(test-show  "33.333333333333333333333333333333"
            (with ((precision 30)) (numeric 100/3)))
(test-show   "3.333333333333333333333333333333"
             (with ((precision 30)) (numeric 10/3)))
(test-show   "0.333333333333333333333333333333"
             (with ((precision 30)) (numeric 1/3)))
(test-show   "0.033333333333333333333333333333"
             (with ((precision 30)) (numeric 1/30)))
(test-show   "0.003333333333333333333333333333"
             (with ((precision 30)) (numeric 1/300)))
(test-show   "0.000333333333333333333333333333"
             (with ((precision 30)) (numeric 1/3000)))
(test-show   "0.666666666666666666666666666667"
             (with ((precision 30)) (numeric 2/3)))
(test-show   "0.090909090909090909090909090909"
             (with ((precision 30)) (numeric 1/11)))
(test-show   "1.428571428571428571428571428571"
             (with ((precision 30)) (numeric 10/7)))
(test-show "0.123456789012345678901234567890"
           (with ((precision 30))
                 (numeric (/  123456789012345678901234567890
                              1000000000000000000000000000000))))
(test-show  " 333.333333333333333333333333333333"
            (with ((precision 30) (decimal-align 5)) (numeric 1000/3)))
(test-show  "  33.333333333333333333333333333333"
            (with ((precision 30) (decimal-align 5)) (numeric 100/3)))
(test-show  "   3.333333333333333333333333333333"
            (with ((precision 30) (decimal-align 5)) (numeric 10/3)))
(test-show  "   0.333333333333333333333333333333"
            (with ((precision 30) (decimal-align 5)) (numeric 1/3)))


(test-show "11.75" (with ((precision 2)) (/ 47 4)))
(test-show "-11.75" (with ((precision 2)) (/ -47 4)))

(test-show "(#x11 #x22 #x33)" (with ((radix 16)) '(#x11 #x22 #x33)))

(test-show "299792458" (with ((comma-rule 3)) 299792458))
(test-show "299,792,458" (with ((comma-rule 3)) (numeric 299792458)))
(test-show "299.792.458"
           (with ((comma-rule 3) (comma-sep #\.)) (numeric 299792458)))
(test-show "299.792.458,0"
           (with ((precision 1) (comma-rule 3) (comma-sep #\.))
                 (numeric 299792458.0)))

(test-show "100,000" (with ((comma-rule 3)) (numeric 100000)))
(test-show "100,000.0"
           (with ((comma-rule 3) (precision 1)) (numeric 100000)))
(test-show "100,000.00"
           (with ((comma-rule 3) (precision 2)) (numeric 100000)))

(test-show "0" (numeric 0 2))
(test-show "0" (numeric 0 10))
(test-show "0" (numeric 0 36))

(test-show "0" (numeric 0 2))
(test-show "0" (numeric 0 10))
(test-show "0" (numeric 0 36))

(test-show "1" (numeric 1 2))
(test-show "1" (numeric 1 10))
(test-show "1" (numeric 1 36))

(test-show "0" (numeric 0.0 10 0))
(test-show "0" (numeric 0.0 9 0))

(test-show "0.0000000000000001" (with ((precision 16)) (numeric 1e-25 36)))
(test-show "100000000000000000000000000000000000000000000000000000000000000000000000000000000"
           (numeric (expt 2.0 80) 2))

(test-show "10" (numeric 2 2))
(test-show "1001" (numeric 9 2))
(test-show "1001.01" (numeric 9.25 2))

(test-show "11" (numeric 4 3))
(test-show "10" (numeric 3 3))
(test-show "1001" (numeric 28 3))
(test-show "1001.01" (numeric #i253/9 3 2))

(test-show "zzz" (numeric (- (* 36 36 36) 1) 36))

(test-show "1.1250" (numeric 9/8 10 4))
(test-show "1.125" (numeric 9/8 10 3))
(test-show "1.12" (numeric 9/8 10 2))
(test-show "1.1" (numeric 9/8 10 1))
(test-show "1" (numeric 9/8 10 0))

(test-show "1.1250" (numeric #i9/8 10 4))
(test-show "1.125" (numeric #i9/8 10 3))
(test-show "1.12" (numeric #i9/8 10 2))
(test-show "1.1" (numeric #i9/8 10 1))
(test-show "1" (numeric #i9/8 10 0))

(test-show "1.1230" (numeric 91/64 4 4))
(test-show "1.123" (numeric 91/64 4 3))
(test-show "1.13" (numeric 91/64 4 2))
(test-show "1.2" (numeric 91/64 4 1))
(test-show "1" (numeric 91/64 4 0))

(test-show "1.1230" (numeric #i91/64 4 4))
(test-show "1.123" (numeric #i91/64 4 3))
(test-show "1.13" (numeric #i91/64 4 2))
(test-show "1.2" (numeric #i91/64 4 1))
(test-show "1" (numeric #i91/64 4 0))

(test-show "+1" (numeric 1 10 #f #t))
(test-show "+1" (with ((sign-rule #t)) (numeric 1)))
(test-show "-0" (with ((sign-rule #t)) (numeric -0.0)))
(test-show "+0" (with ((sign-rule #t)) (numeric +0.0)))

(test-show "1,234,567" (numeric 1234567 10 #f #f 3))
(test-show "567" (numeric 567 10 #f #f 3))
(test-show "1,23,45,67" (numeric 1234567 10 #f #f 2))

(test-show "1|234|567" (numeric 1234567 10 #f #f 3 #\|))
(test-show "1&234&567" (with ((comma-sep #\&)) (numeric 1234567 10 #f #f 3)))
(test-show "1*234*567" (with ((comma-sep #\&)) (numeric 1234567 10 #f #f 3 #\*)))
(test-show "567" (numeric 567 10 #f #f 3 #\|))
(test-show "1,23,45,67" (numeric 1234567 10 #f #f 2))

(test-show "1_5" (with ((decimal-sep #\_)) (numeric 1.5)))
(test-show "1,5" (with ((comma-sep #\.)) (numeric 1.5)))
(test-show "1,5" (numeric 1.5 10 #f #f #f #\.))
(test-show "1%5" (numeric 1.5 10 #f #f #f #\. #\%))

(test-show "1.00+2.00i"
           (with ((precision 2)) (string->number "1+2i")))
(test-show "3.14+2.00i"
           (with ((precision 2)) (string->number "3.14159+2i")))

(test-show "1,234,567" (numeric/comma 1234567))

(test-show "608" (numeric/si 608))
(test-show "3.9Ki" (numeric/si 3986))
(test-show "4k" (numeric/si 3986 1000))
(test-show "1.2M" (numeric/si 1.23e6 1000))
(test-show "123k" (numeric/si 1.23e5 1000))
(test-show "12.3k" (numeric/si 1.23e4 1000))
(test-show "1.2k" (numeric/si 1.23e3 1000))
(test-show "123" (numeric/si 1.23e2 1000))
(test-show "12.3" (numeric/si 1.23e1 1000))
(test-show "1.2" (numeric/si 1.23 1000))
(test-show "1.2 " (numeric/si 1.23 1000 " "))
(test-show "123m" (numeric/si 0.123 1000))
(test-show "12.3m" (numeric/si 1.23e-2 1000))
(test-show "1.2m" (numeric/si 1.23e-3 1000))
(test-show "123μ" (numeric/si 1.23e-4 1000))
(test-show "12.3μ" (numeric/si 1.23e-5 1000))
(test-show "1.2μ" (numeric/si 1.23e-6 1000))
(test-show "1.2 μ" (numeric/si 1.23e-6 1000 " "))

(test-show "1.23" (numeric/fitted 4 1.2345 10 2))
(test-show "1.00" (numeric/fitted 4 1 10 2))
(test-show "#.##" (numeric/fitted 4 12.345 10 2))

;;; Concatenation.

(test-show "1 2 3" (joined each '(1 2 3) " "))

(test-show ":abc:123"
           (joined/prefix
             (lambda (x) (trimmed/right 3 x))
             '("abcdef" "123456")
             ":"))

(test-show "abc\n123\n"
           (joined/suffix
             (lambda (x) (trimmed/right 3 x))
             '("abcdef" "123456")
             nl))

(test-show "lions, tigers, and bears"
           (joined/last
             each
             (lambda (x) (each "and " x))
             '(lions tigers bears)
             ", "))

(test-show "lions, tigers, or bears"
           (joined/dot
             each
             (lambda (x) (each "or " x))
             '(lions tigers . bears)
             ", "))

;;; Padding and Trimming.

(test-show "abc  " (padded/right 5 "abc"))
(test-show "  abc" (padded 5 "abc"))
(test-show "abcdefghi" (padded 5 "abcdefghi"))
(test-show " abc " (padded/both 5 "abc"))
(test-show " abc  " (padded/both 6 "abc"))
(test-show "abcde" (padded/right 5 "abcde"))
(test-show "abcdef" (padded/right 5 "abcdef"))

(test-show "abc" (trimmed/right 3 "abcde"))
(test-show "abc" (trimmed/right 3 "abcd"))
(test-show "abc" (trimmed/right 3 "abc"))
(test-show "ab" (trimmed/right 3 "ab"))
(test-show "a" (trimmed/right 3 "a"))
(test-show "cde" (trimmed 3 "abcde"))
(test-show "bcd" (trimmed/both 3 "abcde"))
(test-show "bcdef" (trimmed/both 5 "abcdefgh"))
(test-show "abc" (trimmed/lazy 3 "abcde"))
(test-show "abc" (trimmed/lazy 3 "abc\nde"))

(test-show "prefix: abc" (each "prefix: " (trimmed/right 3 "abcde")))
(test-show "prefix: cde" (each "prefix: " (trimmed 3 "abcde")))
(test-show "prefix: bcd" (each "prefix: " (trimmed/both 3 "abcde")))
(test-show "prefix: abc" (each "prefix: " (trimmed/lazy 3 "abcde")))
(test-show "prefix: abc" (each "prefix: " (trimmed/lazy 3 "abc\nde")))

(test-show "abc :suffix" (each (trimmed/right 3 "abcde") " :suffix"))
(test-show "cde :suffix" (each (trimmed 3 "abcde") " :suffix"))
(test-show "bcd :suffix" (each (trimmed/both 3 "abcde") " :suffix"))
(test-show "abc :suffix" (each (trimmed/lazy 3 "abcde") " :suffix"))
(test-show "abc :suffix" (each (trimmed/lazy 3 "abc\nde") " :suffix"))

(test-show "abc" (trimmed/lazy 10 (trimmed/lazy 3 "abcdefghijklmnopqrstuvwxyz")))
(test-show "abc" (trimmed/lazy 3 (trimmed/lazy 10 "abcdefghijklmnopqrstuvwxyz")))

(test-show "abcde"
           (with ((ellipsis "...")) (trimmed/right 5 "abcde")))
(test-show "ab..."
           (with ((ellipsis "...")) (trimmed/right 5 "abcdef")))
(test-show "abc..."
           (with ((ellipsis "...")) (trimmed/right 6 "abcdefg")))
(test-show "abcde"
           (with ((ellipsis "...")) (trimmed 5 "abcde")))
(test-show "...ef"
           (with ((ellipsis "...")) (trimmed 5 "abcdef")))
(test-show "...efg"
           (with ((ellipsis "...")) (trimmed 6 "abcdefg")))
(test-show "abcdefg"
           (with ((ellipsis "...")) (trimmed/both 7 "abcdefg")))
(test-show "...d..."
           (with ((ellipsis "...")) (trimmed/both 7 "abcdefgh")))
(test-show "...e..."
           (with ((ellipsis "...")) (trimmed/both 7 "abcdefghi")))

(test-show "abc  " (fitted/right 5 "abc"))
(test-show "  abc" (fitted 5 "abc"))
(test-show " abc " (fitted/both 5 "abc"))
(test-show "abcde" (fitted/right 5 "abcde"))
(test-show "abcde" (fitted 5 "abcde"))
(test-show "abcde" (fitted/both 5 "abcde"))
(test-show "abcde" (fitted/right 5 "abcdefgh"))
(test-show "defgh" (fitted 5 "abcdefgh"))
(test-show "bcdef" (fitted/both 5 "abcdefgh"))

(test-show "prefix: abc   :suffix"
           (each "prefix: " (fitted/right 5 "abc") " :suffix"))
(test-show "prefix:   abc :suffix"
           (each "prefix: " (fitted 5 "abc") " :suffix"))
(test-show "prefix:  abc  :suffix"
           (each "prefix: " (fitted/both 5 "abc") " :suffix"))
(test-show "prefix: abcde :suffix"
           (each "prefix: " (fitted/right 5 "abcde") " :suffix"))
(test-show "prefix: abcde :suffix"
           (each "prefix: " (fitted 5 "abcde") " :suffix"))
(test-show "prefix: abcde :suffix"
           (each "prefix: " (fitted/both 5 "abcde") " :suffix"))
(test-show "prefix: abcde :suffix"
           (each "prefix: " (fitted/right 5 "abcdefgh") " :suffix"))
(test-show "prefix: defgh :suffix"
           (each "prefix: " (fitted 5 "abcdefgh") " :suffix"))
(test-show "prefix: bcdef :suffix"
           (each "prefix: " (fitted/both 5 "abcdefgh") " :suffix"))

;;; Shared structures.

; (test-show "#0=#(1 #0#)"
;            (written (let ((ones (vector 1 1)))
;                       (vector-set! ones 1 ones)
;                       ones)))
; (test-show "(0 . #0=(1 . #0#))"
;            (written (let ((ones (list 1)))
;                       (set-cdr! ones ones)
;                       (cons 0 ones))))
; (test-show "(sym . #0=(sym . #0#))"
;            (written (let ((syms (list 'sym)))
;                       (set-cdr! syms syms)
;                       (cons 'sym syms))))
; (test-show "(#0=(1 . #0#) #1=(2 . #1#))"
;            (written (let ((ones (list 1))
;                           (twos (list 2)))
;                       (set-cdr! ones ones)
;                       (set-cdr! twos twos)
;                       (list ones twos))))
; (test-show "(#0=(1 . #0#) #0#)"
;            (written (let ((ones (list 1)))
;                       (set-cdr! ones ones)
;                       (list ones ones))))
; (test-show "((1) (1))"
;            (written (let ((ones (list 1)))
;                       (list ones ones))))
; 
; (test-show "(#0=(1) #0#)"
;            (written-shared (let ((ones (list 1)))
;                              (list ones ones))))

;;; Columnar Formatting.

(test-show "hello\nworld\n"
           (with ((width 8)) (wrapped "hello world")))
(test-show "\n" (wrapped "    "))
(test-show
  "The  quick\nbrown  fox\njumped\nover   the\nlazy dog\n"
  (with ((width 10))
        (justified "The quick brown fox jumped over the lazy dog")))

(test-show "The fundamental list iterator.\nApplies KONS to each element of\nLS and the result of the previous\napplication, beginning with KNIL.\nWith KONS as CONS and KNIL as '(),\nequivalent to REVERSE.\n"
           (with ((width 36))
                 (wrapped "The fundamental list iterator.  Applies KONS to each element of LS and the result of the previous application, beginning with KNIL.  With KONS as CONS and KNIL as '(), equivalent to REVERSE.")))

(test-show
  "- Item 1: The text here is\n          indented according\n          to the space \"Item\n          1\" takes, and one\n          does not known what\n          goes here.\n"
  (columnar 9 (each "- Item 1:") " " (with ((width 20)) (wrapped "The text here is indented according to the space \"Item 1\" takes, and one does not known what goes here."))))

(test-show
  "- Item 1: The text here is\n          indented according\n          to the space \"Item\n          1\" takes, and one\n          does not known what\n          goes here.\n"
  (columnar 9 (each "- Item 1:\n") " " (with ((width 20)) (wrapped "The text here is indented according to the space \"Item 1\" takes, and one does not known what goes here."))))

(test-show
  "- Item 1: The-text-here-is\n--------- indented-according\n--------- to-the-space-\"Item\n--------- 1\"-takes,-and-one\n--------- does-not-known-what\n--------- goes-here.\n"
  (with ((pad-char #\-)) (columnar 9 (each "- Item 1:\n") " " (with ((width 20)) (wrapped "The text here is indented according to the space \"Item 1\" takes, and one does not known what goes here.")))))

(test-show
  "a   | 123\nbc  | 45\ndef | 6\n"
  (with ((width 20))
        (tabular (each "a\nbc\ndef\n") " | "
                 (each "123\n45\n6\n"))))

(test-show "1234567890\nabcdefghij\nkl\n"
           (with ((width 10))
                 (wrapped/char "12345" "67890abcdefghijkl")))

; (test-show
;   "   1 first line\n   2 second line\n   3 third line\n"
;   (columnar 4 'right 'infinite (line-numbers) " " (from-file "show.txt")))

;;; Color.

(test-show "\x1B;[0;31mred\x1B;[0m" (as-red "red"))
(test-show "\x1B;[0;31mred\x1B;[0;34mblue\x1B;[0;31mred\x1B;[0m"
           (as-red "red" (as-blue "blue") "red"))


;;; Unicode.
(test-show "〜日本語〜"
           (with ((pad-char #\〜)) (padded/both 5 "日本語")))
(test-show "日本語"
           (as-unicode (with ((pad-char #\〜)) (padded/both 5 "日本語"))))

(test-end)
