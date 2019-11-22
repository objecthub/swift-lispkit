;;; SRFI 175 REGRESSION TEST SUITE
;;;
;;; This is the test suite for SRFI 175.
;;;
;;; Copyright © 2019 Lassi Kortela. All rights reserved.
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
;;;   Copyright © 2019 Matthias Zenger. All rights reserved.

(import (lispkit base)
        (lispkit test)
        (srfi 175))

(test-begin "SRFI 175: ASCII Character Library")

(define-syntax want
  (syntax-rules ()
    ((_ right-answer (proc args ...))
     (test right-answer (proc args ...)))))

(want #f (ascii-codepoint? -1))
(want #t (ascii-codepoint? 0))
(want #t (ascii-codepoint? #x7f))
(want #f (ascii-codepoint? #x80))

(want #t (ascii-char? (integer->char 0)))
(want #t (ascii-char? (integer->char #x7f)))
(want #f (ascii-char? (integer->char #x80)))

(want #t (ascii-string? ""))
(want #t (ascii-string? "a"))
(want #t (ascii-string? "a b c"))
(want #f (ascii-string? "å b o"))
(want #t (ascii-string? (make-string 1 (integer->char #x7f))))
(want #f (ascii-string? (make-string 1 (integer->char #x80))))

(want #t (ascii-bytevector? (string->utf8 "")))
(want #t (ascii-bytevector? (string->utf8 "a")))
(want #t (ascii-bytevector? (string->utf8 "a b c")))
(want #f (ascii-bytevector? (string->utf8 "å b o")))
(want #t (ascii-bytevector?
          (string->utf8 (make-string 1 (integer->char #x7f)))))
(want #f (ascii-bytevector?
          (string->utf8 (make-string 1 (integer->char #x80)))))

(want #t (ascii-display? #\space))
(want #f (ascii-display? #\tab))
(want #f (ascii-display? #\newline))
(want #f (ascii-display? (integer->char #x0d)))

(want #t (ascii-space-or-tab? #\space))
(want #t (ascii-space-or-tab? #\tab))
(want #f (ascii-space-or-tab? #\newline))
(want #f (ascii-display? (integer->char #x0d)))

(let ((lowers "abcdefghijklmnopqrstuvwxyz")
      (uppers "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  (let loop ((i 0))
    (when (< i 26)
      (let ((lower (string-ref lowers i))
            (upper (string-ref uppers i)))
        (want (list upper upper lower lower)
              (list (ascii-upcase upper)
                    (ascii-upcase lower)
                    (ascii-downcase upper)
                    (ascii-downcase lower)))
        (loop (+ i 1))))))

(let loop ((cc 0))
  (when (< cc #x80)
    (unless (ascii-alphabetic? cc)
      (want (list cc cc) (list (ascii-upcase cc) (ascii-downcase cc))))
    (loop (+ cc 1))))

(let loop ((cc 0))
  (when (< cc #x80)
    (want #f (ascii-char? cc))
    (want #t (ascii-char? (integer->char cc)))
    (cond ((ascii-alphabetic? cc)
           (want #t (and (ascii-upper-case? (ascii-upcase cc))
                         (ascii-lower-case? (ascii-downcase cc))
                         (ascii-alphanumeric? cc)
                         (ascii-display? cc)))
           (want #f (or (ascii-lower-case? (ascii-upcase cc))
                        (ascii-upper-case? (ascii-downcase cc))
                        (ascii-punctuation? cc)
                        (ascii-control? cc)
                        (ascii-numeric? cc 10)
                        (ascii-whitespace? cc)
                        (ascii-space-or-tab? cc))))
          ((ascii-control? cc)
           (want #f (or (ascii-display? cc) (ascii-punctuation? cc)))
           (want cc
                 (ascii-display->control
                  (ascii-control->display cc)))
           (want (integer->char cc)
                 (ascii-display->control
                  (ascii-control->display (integer->char cc)))))
          ((ascii-open-bracket cc)
           (want #f (ascii-close-bracket cc))
           (want cc (ascii-mirror-bracket (ascii-mirror-bracket cc)))
           (want cc (ascii-open-bracket
                     (ascii-mirror-bracket
                      (ascii-close-bracket
                       (ascii-mirror-bracket cc))))))
          ((ascii-close-bracket cc)
           (want #f (ascii-open-bracket cc))
           (want cc (ascii-mirror-bracket (ascii-mirror-bracket cc)))
           (want cc (ascii-close-bracket
                     (ascii-mirror-bracket
                      (ascii-open-bracket
                       (ascii-mirror-bracket cc)))))))
    (loop (+ cc 1))))

(let outer ((a 0))
  (when (< a 26)
    (let inner ((b 0))
      (if (= b 26)
          (outer (+ a 1))
          (begin (want (list (= a b) (< a b) (<= a b) (> a b) (>= a b))
                       (list (ascii-ci=?
                                 (ascii-nth-lower-case a)
                                 (ascii-nth-upper-case b))
                             (ascii-ci<?
                                 (ascii-nth-lower-case a)
                                 (ascii-nth-upper-case b))
                             (ascii-ci<=?
                                 (ascii-nth-lower-case a)
                                 (ascii-nth-upper-case b))
                             (ascii-ci>?
                                 (ascii-nth-lower-case a)
                                 (ascii-nth-upper-case b))
                             (ascii-ci>=?
                                 (ascii-nth-lower-case a)
                                 (ascii-nth-upper-case b))))
                 (inner (+ b 1)))))))

(ascii-ci>? #\A #\_)
(ascii-ci>? #\Z #\_)

(want #f (ascii-char? -1))
(want #f (ascii-char? #x80))
(want #f (ascii-char? (integer->char #x80)))

(want #f (ascii-control? -1))
(want #t (ascii-control? #x00))
(want #t (ascii-control? #x1f))
(want #f (ascii-control? #x20))
(want #f (ascii-control? #x7e))
(want #t (ascii-control? #x7f))
(want #f (ascii-control? #x80))

(want 0 (ascii-digit-value #\0 10))
(want 0 (ascii-digit-value #\0 1))
(want #f (ascii-digit-value #\0 0))
(want #f (ascii-digit-value #\0 -1))
(want 7 (ascii-digit-value #\7 8))
(want #f (ascii-digit-value #\7 7))
(want #f (ascii-digit-value #\: 10))

(want 0 (ascii-upper-case-value #\A 0 26))
(want 25 (ascii-upper-case-value #\Z 0 26))
(want #f (ascii-upper-case-value #\Z 0 25))

(want 0 (ascii-lower-case-value #\a 0 26))
(want 25 (ascii-lower-case-value #\z 0 26))
(want #f (ascii-lower-case-value #\z 0 25))

(want 0 (ascii-lower-case-value #\a 0 1))
(want #f (ascii-lower-case-value #\a 0 0))
(want #f (ascii-lower-case-value #\a 0 -1))
(want 9001 (ascii-lower-case-value #\b 9000 2))

(want #f (ascii-nth-digit -1))
(want #\0 (ascii-nth-digit 0))
(want #\9 (ascii-nth-digit 9))
(want #f (ascii-nth-digit 10))

(want #\Z (ascii-nth-upper-case -1))
(want #\A (ascii-nth-upper-case 0))
(want #\Z (ascii-nth-upper-case 25))
(want #\A (ascii-nth-upper-case 26))

(want #\z (ascii-nth-lower-case -1))
(want #\a (ascii-nth-lower-case 0))
(want #\z (ascii-nth-lower-case 25))
(want #\a (ascii-nth-lower-case 26))

(define (count-matching predicates value)
  (let loop ((ps predicates) (n 0))
    (if (null? ps) n (loop (cdr ps) (if ((car ps) value) (+ n 1) n)))))

(define (union? whole . parts)
  (let check ((cc 0))
    (or (= cc #x80)
        (if (and (whole cc) (not (= 1 (count-matching parts cc))))
            #f (check (+ cc 1))))))

(define (subset? small-set . bigger-sets)
  (let check ((cc 0))
    (or (= cc #x80)
        (if (and (small-set cc) (= 0 (count-matching bigger-sets cc)))
            #f (check (+ cc 1))))))

(define (disjoint? . predicates)
  (let check ((cc 0))
    (or (= cc #x80) (and (<= (count-matching predicates cc) 1)
                         (check (+ cc 1))))))

(define (decimal-numeric? x) (ascii-numeric? x 10))

(want #t (union? ascii-alphanumeric? ascii-alphabetic? decimal-numeric?))
(want #t (union? ascii-alphabetic? ascii-upper-case? ascii-lower-case?))

(want #t (subset? ascii-space-or-tab?  ascii-whitespace?))
(want #t (subset? ascii-punctuation?   ascii-display?))
(want #t (subset? ascii-upper-case?    ascii-alphabetic? ascii-display?))
(want #t (subset? ascii-lower-case?    ascii-alphabetic? ascii-display?))
(want #t (subset? ascii-alphabetic?    ascii-alphanumeric? ascii-display?))
(want #t (subset? decimal-numeric?     ascii-alphanumeric? ascii-display?))
(want #t (subset? ascii-alphanumeric?  ascii-display?))

(want #t (disjoint? ascii-control? ascii-display?))
(want #t (disjoint? ascii-whitespace?
                    ascii-punctuation?
                    ascii-upper-case?
                    ascii-lower-case?
                    decimal-numeric?))
(want #t (disjoint? ascii-control?
                    ascii-punctuation?
                    ascii-upper-case?
                    ascii-lower-case?
                    decimal-numeric?))

(test-end)

