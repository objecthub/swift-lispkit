;;; SRFI 272 MEASURE REGRESSION TEST SUITE
;;;
;;; This is the test suite for library `(srfi 272 measure)`.
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
        (srfi 272 measure)
        (srfi 272 advanced))

(load "Tests/SRFI-272-Support")

(test-begin "SRFI 272: Character width measurement")

(test-group "Golden tests"

(define cw (char-width-procedure))
(test-equal "ascii"
  '(1 1 1 1 1 1)
  (map cw (string->list "aZ0 ~!")))
(test-equal "control characters"
  '(#f #f #f #f #f)
  (map cw (list (integer->char 0) (integer->char 7) (integer->char 10) (integer->char 127) (integer->char 159))))
(test-equal "combining characters"
  '(0 0 0 0 0)
  (map cw (list (integer->char #x300) (integer->char #x36F) (integer->char #x20D0) (integer->char #x200B) (integer->char #xFEFF))))
(test-equal "wide characters"
  '(2 2 2 2 2)
  (map cw (list (integer->char #x4E00) (integer->char #x3042) (integer->char #xAC00) (integer->char #xFF01) (integer->char #x231A))))
(test-equal "narrow characters"
  '(1 1 1 1 1)
  (map cw (list (integer->char #xA0) (integer->char #xE9) (integer->char #x3B1) (integer->char #x416) (integer->char #xFF61))))
(test-equal "wide strings"
  "(\"一二三\" \"四五六\"\n \"七八九\")\n"
  (pp->string (list (string (integer->char #x4E00) (integer->char #x4E8C) (integer->char #x4E09))
                      (string (integer->char #x56DB) (integer->char #x4E94) (integer->char #x516D))
                      (string (integer->char #x4E03) (integer->char #x516B) (integer->char #x4E5D)))
                pp-width 20))
(test-equal "custom char width procedure"
  "(abc def\n  ghi jkl)\n"
  (parameterize ((char-width-procedure (lambda (c) 4)))
      (pp->string '(abc def ghi jkl) pp-width 30))))

(test-group "LispKit-specific tests"

(test-equal "surrogate pairs (emoji)" 2
  (let ((cw (char-width-procedure)))
    (apply + (map cw (string->list "😀")))))
(test-equal "text and emoji presentation" '(1 2 2 1)
  (map (char-width-procedure) (list #\x2600 #\x231A #\x2B50 #\x2702)))
(test-equal "zero-width characters" '(0 0 0 0)
  (map (char-width-procedure) (list #\x200D #\x200E #\x094D #\x0E31)))
(test-equal "non-printable characters" '(#f #f #f)
  (map (char-width-procedure) (list #\x0 #\x1B #\x85)))
(test-equal "characters outside the BMP are measured as a whole"
  "(\"𝐀𝐀𝐀𝐀\" \"𝐀𝐀𝐀𝐀\")\n"
  (pp->string (list "𝐀𝐀𝐀𝐀" "𝐀𝐀𝐀𝐀") pp-width 16))
(test-equal "emoji sequences are measured as a whole"
  "(\"👨‍👩‍👧👍🏽\" \"👨‍👩‍👧👍🏽\")\n"
  (pp->string (list "👨‍👩‍👧👍🏽" "👨‍👩‍👧👍🏽") pp-width 15))
(test-equal "wide strings break lines"
  "(\"日本語日本語\"\n \"日本語日本語\")\n"
  (pp->string (list "日本語日本語" "日本語日本語") pp-width 20 pp-code #f))
(test-equal "custom procedures are used per character"
  "(\"𝐀𝐀𝐀𝐀\"\n \"𝐀𝐀𝐀𝐀\")\n"
  (parameterize ((char-width-procedure (lambda (c) 1)))
    (pp->string (list "𝐀𝐀𝐀𝐀" "𝐀𝐀𝐀𝐀") pp-width 16 pp-code #f))))

(test-end)
