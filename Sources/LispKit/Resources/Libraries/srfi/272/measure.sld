;;; SRFI 272 MEASURE
;;; Pretty Printing: measuring the width of characters
;;;
;;; Library `(srfi 272 measure)` exports the parameter object `char-width-procedure`
;;; which holds the procedure used by the `(srfi 272 intermediate)`,
;;; `(srfi 272 advanced)` and `(srfi 272 fancy)` pretty printers to determine the
;;; number of terminal columns a character occupies. The procedure accepts a character
;;; and returns either 0, 1, or 2, or #f if the character is not printable. Users can
;;; plug in their own procedure by changing the value of the parameter.
;;;
;;; In LispKit, the default procedure is based on `string-display-width` of library
;;; `(lispkit string)`, which implements the Unicode East Asian Width and emoji
;;; presentation rules. Since LispKit strings are sequences of UTF-16 code units,
;;; characters outside of the basic multilingual plane are represented by two
;;; surrogate characters; the default procedure assigns width 1 to each of them.
;;; As long as the default procedure is in use, the pretty printers therefore
;;; measure whole strings via `string-display-width`, which handles such characters
;;; as well as grapheme clusters (e.g. emoji sequences) correctly.
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

(define-library (srfi 272 measure)

  (export char-width-procedure)

  (import (scheme base)
          (only (lispkit string) string-display-width))

  (begin

    ;; Returns #f (not printable), 0, 1, or 2. The sample implementation of SRFI 272
    ;; uses a table-based approximation of `wcwidth()`; LispKit relies on
    ;; `string-display-width` instead.
    (define (char-width ch)
      (let ((cp (char->integer ch)))
        (cond ((<= 32 cp 126) 1)     ; fast track
              ((< cp 32) #f)         ; C0
              ((<= 127 cp 159) #f)   ; DEL/C1
              (else (string-display-width (string ch))))))

    ;; Users can plug in their custom procedures
    (define char-width-procedure (make-parameter char-width))
  )
)
