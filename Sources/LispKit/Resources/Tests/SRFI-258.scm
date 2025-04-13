;;; SRFI 258 REGRESSION TEST SUITE
;;;
;;; This is the test suite for SRFI 258.
;;;
;;; Copyright © 2025 Wolfgang Corcoran-Mathe. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2025 Matthias Zenger. All rights reserved.

(import (lispkit base)
        (lispkit test)
        (srfi 258))

(test-begin "SRFI 258: Uninterned symbols")

(test-assert "Usym is symbol? (S->US)"
  (symbol? (string->uninterned-symbol "x")))

(test-assert "Usym is symbol? (GUS)"
  (symbol? (generate-uninterned-symbol)))

(test-assert "Usym is unique (S->US)"
  (not (eqv? (string->uninterned-symbol "x")
             (string->uninterned-symbol "x"))))

(test-assert "Usym is unique (GUS)"
  (not (eqv? (generate-uninterned-symbol)
             (generate-uninterned-symbol))))

(test-assert "Usym is eqv? to itself (S->US)"
  (let ((x (string->uninterned-symbol "x")))
    (eqv? x x)))

(test-assert "Usym is eqv? to itself (GUS)"
  (let ((x (generate-uninterned-symbol)))
    (eqv? x x)))

(test-assert "Usym is not symbol-interned? (S->US)"
  (not (symbol-interned? (string->uninterned-symbol "x"))))

(test-assert "Usym is not symbol-interned? (GUS)"
  (not (symbol-interned? (generate-uninterned-symbol))))

(test-assert "generate-uninterned-symbol with string prefix"
  (let* ((prefix "perfection")
         (g (generate-uninterned-symbol prefix)))
    (equal? prefix
            (substring (symbol->string g)
                       0
                       (string-length prefix)))))

(test-assert "generate-uninterned-symbol with symbol prefix"
  (let* ((prefix 'perfection)
         (ps (symbol->string prefix))
         (g (generate-uninterned-symbol prefix)))
    (equal? ps
            (substring (symbol->string g)
                       0
                       (string-length ps)))))

(test-end)
