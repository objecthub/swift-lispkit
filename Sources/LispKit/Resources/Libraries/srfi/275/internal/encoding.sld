;;; SRFI 275 INTERNAL ENCODING
;;; URIs, IRIs, and basic paths: UTF-8 and percent-encoding
;;;
;;; This library is internal to the implementation of SRFI 275. It implements a strict
;;; UTF-8 codec for sequences of percent-encoded bytes (overlong encodings, surrogates,
;;; incomplete sequences, and stray continuation bytes are rejected) and the generic
;;; routine for turning arbitrary text into the canonical form of a URI or IRI
;;; component: literal characters are retained if they are permitted in the component,
;;; all percent-encoded bytes are written with uppercase hexadecimal digits and are
;;; validated, and (optionally) characters which are not permitted are escaped.
;;;
;;; Author of spec: Duncan Guthrie
;;;
;;; Copyright © 2026 Duncan Guthrie. All rights reserved.
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

(define-library (srfi 275 internal encoding)

  (import (scheme base)
          (scheme cxr)
          (srfi 14)
          (srfi 275 internal chars))

  (export fail
          escape-codepoint
          cset-contains-cp?
          canonicalize-component
          ascii-downcase-literals
          percent-encode
          percent-decode)

  (begin

    ;; Signals an error. `who` is the name of the procedure raising the error.
    (define (fail who msg . irritants)
      (apply error (string-append who ": " msg) irritants))

    ;;; UTF-8

    (define hex-chars "0123456789ABCDEF")

    (define (byte->escape b)
      (string #\% (string-ref hex-chars (quotient b 16)) (string-ref hex-chars (remainder b 16))))

    ;; Returns the UTF-8 encoding of a Unicode scalar value as a list of bytes
    (define (codepoint->utf8-bytes cp)
      (define (cont shift)
        (+ #x80 (remainder (quotient cp shift) 64)))
      (cond ((< cp #x80) (list cp))
            ((< cp #x800) (list (+ #xC0 (quotient cp 64)) (cont 1)))
            ((< cp #x10000) (list (+ #xE0 (quotient cp 4096)) (cont 64) (cont 1)))
            (else (list (+ #xF0 (quotient cp 262144)) (cont 4096) (cont 64) (cont 1)))))

    ;; Returns the percent-encoded UTF-8 encoding of a Unicode scalar value
    (define (escape-codepoint cp)
      (apply string-append (map byte->escape (codepoint->utf8-bytes cp))))

    (define (write-byte-escape! b out)
      (write-char #\% out)
      (write-char (string-ref hex-chars (quotient b 16)) out)
      (write-char (string-ref hex-chars (remainder b 16)) out))

    ;; Writes the percent-encoded UTF-8 encoding of a Unicode scalar value to a port
    (define (write-escaped-codepoint! cp out)
      (cond ((< cp #x80)
             (write-byte-escape! cp out))
            ((< cp #x800)
             (write-byte-escape! (+ #xC0 (quotient cp 64)) out)
             (write-byte-escape! (+ #x80 (remainder cp 64)) out))
            ((< cp #x10000)
             (write-byte-escape! (+ #xE0 (quotient cp 4096)) out)
             (write-byte-escape! (+ #x80 (remainder (quotient cp 64) 64)) out)
             (write-byte-escape! (+ #x80 (remainder cp 64)) out))
            (else
             (write-byte-escape! (+ #xF0 (quotient cp 262144)) out)
             (write-byte-escape! (+ #x80 (remainder (quotient cp 4096) 64)) out)
             (write-byte-escape! (+ #x80 (remainder (quotient cp 64) 64)) out)
             (write-byte-escape! (+ #x80 (remainder cp 64)) out))))

    ;; Decodes a list of bytes strictly as UTF-8. Returns a list of code points or #f
    ;; if the bytes are not well-formed.
    (define (utf8-bytes->codepoints bytes)
      (define (cont? b) (<= #x80 b #xBF))
      (define (low b) (- b #x80))
      (let loop ((bs bytes) (acc '()))
        (if (null? bs)
            (reverse acc)
            (let ((b (car bs)) (r (cdr bs)))
              (cond
                ((< b #x80) (loop r (cons b acc)))
                ((<= #xC2 b #xDF)
                 (and (pair? r) (cont? (car r))
                      (loop (cdr r) (cons (+ (* (- b #xC0) 64) (low (car r))) acc))))
                ((<= #xE0 b #xEF)
                 (and (pair? r) (pair? (cdr r)) (cont? (car r)) (cont? (cadr r))
                      (let ((cp (+ (* (- b #xE0) 4096) (* (low (car r)) 64) (low (cadr r)))))
                        (and (>= cp #x800)
                             (not (<= #xD800 cp #xDFFF))
                             (loop (cddr r) (cons cp acc))))))
                ((<= #xF0 b #xF4)
                 (and (pair? r) (pair? (cdr r)) (pair? (cddr r))
                      (cont? (car r)) (cont? (cadr r)) (cont? (caddr r))
                      (let ((cp (+ (* (- b #xF0) 262144) (* (low (car r)) 4096)
                                   (* (low (cadr r)) 64) (low (caddr r)))))
                        (and (<= #x10000 cp #x10FFFF)
                             (loop (cdddr r) (cons cp acc))))))
                (else #f))))))

    ;;; Character sets and code points

    ;; Is the code point `cp` a member of the character set `cset`? As character sets
    ;; contain UTF-16 code units, a supplementary character is considered a member if
    ;; both elements of its surrogate pair are.
    (define (cset-contains-cp? cset cp)
      (if (< cp #x10000)
          (char-set-contains? cset (integer->char cp))
          (let ((c (- cp #x10000)))
            (and (char-set-contains? cset (integer->char (+ #xD800 (quotient c #x400))))
                 (char-set-contains? cset (integer->char (+ #xDC00 (remainder c #x400))))))))

    ;;; Canonicalization

    ;; Number of bytes of the UTF-8 sequence which starts with byte `b`, or #f if `b` cannot
    ;; start a sequence
    (define (utf8-sequence-length b)
      (cond ((< b #x80) 1)
            ((<= #xC2 b #xDF) 2)
            ((<= #xE0 b #xEF) 3)
            ((<= #xF0 b #xF4) 4)
            (else #f)))

    ;; Turns `str` into the canonical text of a URI/IRI component.
    ;;
    ;;   allowed?  predicate on code points: characters permitted literally
    ;;   escape?   if true, characters which are not allowed are percent-encoded,
    ;;             otherwise they are rejected
    ;;   decode?   #f, or a predicate on code points: percent-encoded characters for
    ;;             which the predicate and `allowed?` hold are replaced by the literal
    ;;             characters
    ;;   offset    position of `str` in the original text (for error messages only)
    ;;
    ;; Every `%` starts a percent-encoded byte. The percent-encoded bytes have to form
    ;; well-formed UTF-8 sequences. They are processed one character at a time, so that
    ;; arbitrarily long runs of percent-encoded bytes are handled in constant space.
    (define (canonicalize-component who str allowed? escape? decode? offset)
      (let* ((cps (string->codepoints str))
             (n (vector-length cps))
             (out (open-output-string)))
        (define (hex-at i)
          (and (< i n) (cp-hex-value (vector-ref cps i))))
        ;; The value of the percent-encoded byte at position i, or #f
        (define (escaped-byte-at i)
          (and (< i n)
               (eqv? (vector-ref cps i) 37)
               (let ((h (hex-at (+ i 1))) (l (hex-at (+ i 2))))
                 (and h l (+ (* h 16) l)))))
        (define (emit-sequence! start bytes)
          (let ((decoded (utf8-bytes->codepoints bytes)))
            (cond ((not decoded)
                   (fail who "invalid UTF-8 in percent-encoded bytes" str (+ offset start)))
                  (decode?
                   (for-each (lambda (cp)
                               (if (and (allowed? cp) (decode? cp))
                                   (write-string (codepoint->string cp) out)
                                   (write-escaped-codepoint! cp out)))
                             decoded))
                  (else
                   (for-each (lambda (b) (write-string (byte->escape b) out)) bytes)))))
        ;; Processes the percent-encoded UTF-8 sequence starting at position i and returns
        ;; the position after it
        (define (escaped-sequence! i)
          (let ((b0 (escaped-byte-at i)))
            (unless b0
              (fail who "invalid percent-encoding" str (+ offset i)))
            (let ((len (utf8-sequence-length b0)))
              (unless len
                (fail who "invalid UTF-8 in percent-encoded bytes" str (+ offset i)))
              (let gather ((k 1) (j (+ i 3)) (bytes (list b0)))
                (cond ((= k len)
                       (emit-sequence! i (reverse bytes))
                       j)
                      ((escaped-byte-at j)
                       => (lambda (b) (gather (+ k 1) (+ j 3) (cons b bytes))))
                      ((and (< j n) (eqv? (vector-ref cps j) 37))
                       (fail who "invalid percent-encoding" str (+ offset j)))
                      (else
                        (fail who "invalid UTF-8 in percent-encoded bytes" str (+ offset i))))))))
        (let loop ((i 0))
          (when (< i n)
            (let ((cp (vector-ref cps i)))
              (cond
                ((eqv? cp 37)
                 (loop (escaped-sequence! i)))
                ((allowed? cp)
                 (write-string (codepoint->string cp) out)
                 (loop (+ i 1)))
                ((not escape?)
                 (fail who "illegal character" (codepoint->string cp) (+ offset i) str))
                ((not (codepoint-valid? cp))
                 (fail who "invalid character" str (+ offset i)))
                (else
                 (write-escaped-codepoint! cp out)
                 (loop (+ i 1)))))))
        (get-output-string out)))

    ;; Converts ASCII letters to lowercase, except within percent-encoded bytes
    (define (ascii-downcase-literals str)
      (let* ((n (string-length str))
             (out (make-string n)))
        (let loop ((i 0))
          (when (< i n)
            (let ((c (string-ref str i)))
              (cond ((char=? c #\%)
                     (let ((m (min n (+ i 3))))
                       (do ((k i (+ k 1))) ((= k m))
                         (string-set! out k (string-ref str k)))
                       (loop m)))
                    ((char<=? #\A c #\Z)
                     (string-set! out i (integer->char (+ (char->integer c) 32)))
                     (loop (+ i 1)))
                    (else
                     (string-set! out i c)
                     (loop (+ i 1)))))))
        out))

    ;;; Direct encoding and decoding

    ;; Escapes all characters of `str` which are in `cset`, as well as `%`.
    (define (percent-encode who str cset)
      (let ((out (open-output-string)))
        (vector-for-each
          (lambda (cp)
            (cond ((eqv? cp 37)
                   (write-string "%25" out))
                  ((cset-contains-cp? cset cp)
                   (unless (codepoint-valid? cp)
                     (fail who "invalid character" str))
                   (write-escaped-codepoint! cp out))
                  (else
                   (write-string (codepoint->string cp) out))))
          (string->codepoints str))
        (get-output-string out)))

    ;; Decodes all percent-encoded characters of `str` which are in `cset`. The
    ;; remaining percent-encoded bytes are retained.
    (define (percent-decode who str cset)
      (canonicalize-component who
                              str
                              (lambda (cp) #t)
                              #f
                              (lambda (cp) (cset-contains-cp? cset cp))
                              0))
  )
)
