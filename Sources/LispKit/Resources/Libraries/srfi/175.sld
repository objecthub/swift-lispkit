;;; SRFI 175
;;; ASCII Character Library
;;;
;;; This SRFI defines ASCII-only equivalents to many of the character procedures in
;;; standard Scheme plus a few extra ones. Recent Scheme standards are based around
;;; Unicode but the significant syntactic elements in many file formats and network
;;; protocols are all ASCII. Such low-level code can run faster and its behavior
;;; can be easier to understand when it uses ASCII primitives.
;;;
;;; Copyright © 2019 Lassi Kortela. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software, and to permit
;;; persons to whom the Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies or
;;; substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;;; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2019 Matthias Zenger. All rights reserved.

(define-library (srfi 175)

  (export ascii-codepoint?
          ascii-bytevector?
          ascii-char?
          ascii-string?
          ascii-control?
          ascii-display?
          ascii-whitespace?
          ascii-space-or-tab?
          ascii-punctuation?
          ascii-upper-case?
          ascii-lower-case?
          ascii-alphabetic?
          ascii-alphanumeric?
          ascii-numeric?
          ascii-digit-value
          ascii-upper-case-value
          ascii-lower-case-value
          ascii-nth-digit
          ascii-nth-upper-case
          ascii-nth-lower-case
          ascii-upcase
          ascii-downcase
          ascii-control->display
          ascii-display->control
          ascii-open-bracket
          ascii-close-bracket
          ascii-mirror-bracket
          ascii-ci=?
          ascii-ci<?
          ascii-ci>?
          ascii-ci<=?
          ascii-ci>=?
          ascii-string-ci=?
          ascii-string-ci<?
          ascii-string-ci>?
          ascii-string-ci<=?
          ascii-string-ci>=?)

  (import (lispkit base))

  (begin

    (define (ensure-int x)
      (if (char? x) (char->integer x) x))

    (define (base-offset-limit x base offset limit)
      (let ((cc (ensure-int x)))
        (and (>= cc base) (< cc (+ base limit))
             (+ offset (- cc base)))))

    (define (char->int->char map-int char)
      (let ((int (map-int (char->integer char))))
        (and int (integer->char int))))

    (define (int->char->int map-char int)
      (let ((char (map-char (integer->char int))))
        (and char (char->integer char))))

    (define (ascii-codepoint? x)
      (and (integer? x) (exact? x) (<= 0 x #x7f)))

    (define (ascii-char? x)
      (and (char? x) (< (char->integer x) #x80)))

    (define (ascii-bytevector? x)
      (and (bytevector? x)
           (let check ((i (- (bytevector-length x) 1)))
             (or (< i 0) (and (< (bytevector-u8-ref x i) #x80)
                              (check (- i 1)))))))

    (define (ascii-string? x)
      (and (string? x)
           (call-with-port
            (open-input-string x)
            (lambda (in)
              (let check ()
                (let ((char (read-char in)))
                  (or (eof-object? char)
                      (and (< (char->integer char) #x80) (check)))))))))

    (define (ascii-control? x)
      (let ((cc (ensure-int x)))
        (or (<= 0 cc #x1f) (= cc #x7f))))

    (define (ascii-display? x)
      (let ((cc (ensure-int x)))
        (<= #x20 cc #x7e)))

    (define (ascii-whitespace? x)
      (let ((cc (ensure-int x)))
        (cond ((< cc #x09) #f)
              ((< cc #x0e) #t)
              (else (= cc #x20)))))

    (define (ascii-space-or-tab? x)
      (let ((cc (ensure-int x)))
        (case cc ((#x09 #x20) #t) (else #f))))

    (define (ascii-punctuation? x)
      (let ((cc (ensure-int x)))
        (or (<= #x21 cc #x2f)
            (<= #x3a cc #x40)
            (<= #x5b cc #x60)
            (<= #x7b cc #x7e))))

    (define (ascii-upper-case? x)
      (let ((cc (ensure-int x)))
        (<= #x41 cc #x5a)))

    (define (ascii-lower-case? x)
      (let ((cc (ensure-int x)))
        (<= #x61 cc #x7a)))

    (define (ascii-alphabetic? x)
      (let ((cc (ensure-int x)))
        (or (<= #x41 cc #x5a)
            (<= #x61 cc #x7a))))

    (define (ascii-alphanumeric? x)
      (let ((cc (ensure-int x)))
        (or (<= #x30 cc #x39)
            (<= #x41 cc #x5a)
            (<= #x61 cc #x7a))))

    (define (ascii-numeric? x radix)
      (not (not (ascii-digit-value x radix))))

    (define (ascii-digit-value x limit)
      (base-offset-limit x #x30 0 (min limit 10)))

    (define (ascii-upper-case-value x offset limit)
      (base-offset-limit x #x41 offset (min limit 26)))

    (define (ascii-lower-case-value x offset limit)
      (base-offset-limit x #x61 offset (min limit 26)))

    (define (ascii-nth-digit n)
      (and (<= 0 n 9) (integer->char (+ #x30 n))))

    (define (ascii-nth-upper-case n)
      (integer->char (+ #x41 (modulo n 26))))

    (define (ascii-nth-lower-case n)
      (integer->char (+ #x61 (modulo n 26))))

    (define (ascii-upcase x)
      (if (char? x)
          (integer->char (ascii-upcase (char->integer x)))
          (or (ascii-lower-case-value x #x41 26) x)))

    (define (ascii-downcase x)
      (if (char? x)
          (integer->char (ascii-downcase (char->integer x)))
          (or (ascii-upper-case-value x #x61 26) x)))

    (define (ascii-control->display x)
      (if (char? x)
          (char->int->char ascii-control->display x)
          (or (and (<= 0 x #x1f) (+ x #x40))
              (and (= x #x7f) #x3f))))

    (define (ascii-display->control x)
      (if (char? x)
          (char->int->char ascii-display->control x)
          (or (and (<= #x40 x #x5f) (- x #x40))
              (and (= x #x3f) #x7f))))

    (define (ascii-open-bracket char)
      (case char
        ((#\( #\[ #\{ #\<) char)
        (else (and (integer? char) (int->char->int ascii-open-bracket char)))))

    (define (ascii-close-bracket char)
      (case char
        ((#\) #\] #\} #\>) char)
        (else (and (integer? char) (int->char->int ascii-close-bracket char)))))

    (define (ascii-mirror-bracket char)
      (case char
        ((#\() #\))
        ((#\)) #\()
        ((#\[) #\])
        ((#\]) #\[)
        ((#\{) #\})
        ((#\}) #\{)
        ((#\<) #\>)
        ((#\>) #\<)
        (else (and (integer? char) (int->char->int ascii-mirror-bracket char)))))

    (define (ascii-ci-cmp char1 char2)
      (let ((cc1 (ensure-int char1))
            (cc2 (ensure-int char2)))
        (when (<= #x41 cc1 #x5a) (set! cc1 (+ cc1 #x20)))
        (when (<= #x41 cc2 #x5a) (set! cc2 (+ cc2 #x20)))
        (cond ((< cc1 cc2) -1)
              ((> cc1 cc2) 1)
              (else 0))))

    (define (ascii-ci=? char1 char2)
      (= (ascii-ci-cmp char1 char2) 0))

    (define (ascii-ci<? char1 char2)
      (< (ascii-ci-cmp char1 char2) 0))

    (define (ascii-ci>? char1 char2)
      (> (ascii-ci-cmp char1 char2) 0))

    (define (ascii-ci<=? char1 char2)
      (<= (ascii-ci-cmp char1 char2) 0))

    (define (ascii-ci>=? char1 char2)
      (>= (ascii-ci-cmp char1 char2) 0))

    (define (ascii-string-ci-cmp string1 string2)
      (call-with-port
       (open-input-string string1)
       (lambda (in1)
         (call-with-port
          (open-input-string string2)
          (lambda (in2)
            (let loop ()
              (let ((char1 (read-char in1))
                    (char2 (read-char in2)))
                (cond ((eof-object? char1) (if (eof-object? char2) 0 -1))
                      ((eof-object? char2) 1)
                      (else (let ((cc1 (char->integer char1))
                                  (cc2 (char->integer char2)))
                              (when (<= #x41 cc1 #x5a) (set! cc1 (+ cc1 #x20)))
                              (when (<= #x41 cc2 #x5a) (set! cc2 (+ cc2 #x20)))
                              (cond ((< cc1 cc2) -1)
                                    ((> cc1 cc2) 1)
                                    (else (loop)))))))))))))

    (define (ascii-string-ci=? string1 string2)
      (= (ascii-string-ci-cmp string1 string2) 0))

    (define (ascii-string-ci<? string1 string2)
      (< (ascii-string-ci-cmp string1 string2) 0))

    (define (ascii-string-ci>? string1 string2)
      (> (ascii-string-ci-cmp string1 string2) 0))

    (define (ascii-string-ci<=? string1 string2)
      (<= (ascii-string-ci-cmp string1 string2) 0))

    (define (ascii-string-ci>=? string1 string2)
      (>= (ascii-string-ci-cmp string1 string2) 0))
  )
)

