;;; LISPKIT JSON
;;;
;;; This is a simple library for reading and writing data represented via the JSON format.
;;; The code is based on the SchemeSpheres JSON parser. This library will not work well for
;;; very large files. Data is mapped between JSON and Scheme in the following way:
;;;
;;;   - Keys are internally represented as symbols.
;;;   - JSON objects are represented as LispKit hashtables
;;;   - JSON arrays are represented as lists
;;;   - JSON strings are represented as LispKit strings
;;;   - JSON numbers are represented as LispKit numbers
;;;   - `true` in JSON is mappend to `#t`
;;;   - `false` in JSON is mapped to `#f`
;;;   - `null` in JSON is represented as json-null record type
;;; 
;;; Copyright information:
;;;   json.scm - JSON reader and writer
;;;   Homepage: https://notabug.org/pangolinturtle/json-r7rs
;;;   Copyright (c) 2011-2014 by Marc Feeley, All Rights Reserved.
;;;   Copyright (c) 2015 by Jason K. MacDuffie
;;;
;;;   Based on SchemeSpheres JSON parser:
;;;
;;;   The MIT License (MIT)
;;;   Copyright (c) 2012-2015 Alvaro Castro-Castilla
;;;
;;;   Permission is hereby granted, free of charge, to any person obtaining a copy
;;;   of this software and associated documentation files (the "Software"), to deal
;;;   in the Software without restriction, including without limitation the rights
;;;   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;   copies of the Software, and to permit persons to whom the Software is
;;;   furnished to do so, subject to the following conditions:
;;;
;;;   The above copyright notice and this permission notice shall be included in
;;;   all copies or substantial portions of the Software.
;;;
;;;   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;;   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;;   THE SOFTWARE.
;;;
;;; Adaptation to LispKit
;;;   Copyright © 2017 Matthias Zenger. All rights reserved.

(define-library (lispkit json deprecated)

  (export json-read
          json-read-string
          json-read-file
          json-write
          json-write-string
          json-write-file
          json-null
          json-null?)

  (import (lispkit base))

  (begin

    ;; json-null is implemented as a record type
    (define-record-type json-null-type
      (make-json-null)
      json-null?)

    ;; `(json-null)`: Returns a record of the type json-null, for which
    ;; (eq? (json-null) (json-null)) always returns #t.
    (define json-null
      (let ((result (make-json-null)))
        (lambda () result)))

    (define (digit? c radix)
      (and (char? c)
           (let ((n (cond ((and (char>=? c #\0) (char<=? c #\9))
                             (- (char->integer c) (char->integer #\0)))
                          ((and (char>=? c #\a) (char<=? c #\z))
                             (+ 10 (- (char->integer c) (char->integer #\a))))
                          ((and (char>=? c #\A) (char<=? c #\Z))
                             (+ 10 (- (char->integer c) (char->integer #\A))))
                          (else
                             999))))
             (and (< n radix) n))))

    ;;
    ;; JSON reader
    ;;

    (define (accum c i str)
      (string-set! str i c)
      str)

    (define (create-object props)
      (alist->eq-hashtable props))

    ;; `(json-read [port])`: Reads a JSON file from port and converts it to Scheme data.
    ;; port defaults to (current-input-port).
    (define (json-read . port-option)
      (letrec*
        ((port (if (null? port-option)
                   (current-input-port)
                   (if (null? (cdr port-option))
                       (car port-option)
                       (error "json-read: too many arguments"))))
         (rd (lambda () (read-char port)))
         (pk (lambda () (peek-char port)))
         (space (lambda ()
           (let ((c (pk)))
             (if (and (char? c) (char<=? c #\space))
                 (begin (rd) (space))))))
         (parse-value (lambda ()
           (space)
           (let ((c (pk)))
             (if (not (char? c))
                 (error "parse-value: eof while parsing" c)
                 (cond ((eqv? c #\{)  (parse-object))
                       ((eqv? c #\[)  (parse-array))
                       ((eqv? c #\")  (parse-string))
                       ((or (eqv? c #\-) (digit? c 10)) (parse-number))
                       ((eqv? c #\f)
                         (rd)
                         (if (not (and (eqv? (rd) #\a) (eqv? (rd) #\l)
                                       (eqv? (rd) #\s) (eqv? (rd) #\e)))
                             (error "parse-value: invalid literal")
                             #f))
                       ((eqv? c #\t)
                         (rd)
                         (if (not (and (eqv? (rd) #\r) (eqv? (rd) #\u) (eqv? (rd) #\e)))
                             (error "parse-value: invalid literal")
                             #t))
                       ((eqv? c #\n)
                         (rd)
                         (if (not (and (eqv? (rd) #\u)
                                       (eqv? (rd) #\l)
                                       (eqv? (rd) #\l)))
                             (error "parse-value: invalid literal")
                             (json-null)))
                       (else (error "parse-value: JSON could not be decoded")))))))
           (parse-object (lambda ()
             (rd) ;; skip #\{
             (space)
             (if (eqv? (pk) #\})
                 (begin (rd) (create-object '()))
                 (let loop ((rev-elements '()))
                   (let ((str (if (not (eqv? (pk) #\"))
                                  (error "parse-object: key did not begin with quote")
                                  (parse-string))))
                     (begin
                       (space)
                       (if (not (eqv? (pk) #\:))
                           (error "parse-object: key not followed by a colon")
                           (begin
                             (rd)
                             (space)
                             (let ((val (parse-value)))
                               (let ((new-rev-elements
                                      (cons (cons (string->symbol str) val) rev-elements)))
                                 (space)
                                 (let ((c (pk)))
                                   (cond ((eqv? c #\})
                                          (rd)
                                          (create-object
                                           (reverse new-rev-elements)))
                                         ((eqv? c #\,)
                                          (rd)
                                          (space)
                                          (loop new-rev-elements))
                                         (else
                                          (error "parse-object: invalid character in JSON object")))
                                                                                         )))))))))))
           (parse-array (lambda ()
             (rd) ;; skip #\[
             (space)
             (if (eqv? (pk) #\])
                 (begin (rd) '())
                 (let ((x (parse-value)))
                   (let loop ((rev-elements (list x)))
                     (space)
                     (let ((c (pk)))
                       (cond ((eqv? c #\])
                              (rd)
                              (reverse rev-elements))
                             ((eqv? c #\,)
                              (rd)
                              (let ((y (parse-value)))
                                (loop (cons y rev-elements))))
                             (else
                              (error "parse-array: invalid character in JSON array")))))))))
           (parse-str (lambda (pos)
             (let ((c (rd)))
               (cond ((eqv? c #\")
                      (make-string pos))
                     ((eqv? c #\\)
                      (let ((x (rd)))
                        (if (eqv? x #\u)
                            (let loop ((n 0) (i 4))
                              (if (> i 0)
                                  (let ((h (rd)))
                                    (cond ((not (char? h))
                                           (error "parse-string: eof while reading string"))
                                          ((digit? h 16)
                                           =>
                                           (lambda (d)
                                             (loop (+ (* n 16) d) (- i 1))))
                                          (else
                                           (error "parse-string: invalid Unicode escape"))))
                                  (accum (integer->char n) pos (parse-str (+ pos 1)))))
                            (let ((e (assv x json-string-escapes)))
                              (if e
                                  (accum (cdr e) pos (parse-str (+ pos 1)))
                                  (error "parse-string: unrecognized escape character"))))))
                     ((char? c)
                      (accum c pos (parse-str (+ pos 1))))
                     (else
                      (error "parse-string: eof while reading string"))))))
           (parse-string (lambda ()
             (rd) ;; skip #\"
             (parse-str 0)))
           (parse-number (lambda ()
             (letrec* ((sign-part (lambda ()
                         (let ((c (pk)))
                           (if (eqv? c #\-)
                               (begin (rd) (accum c 0 (after-sign-part 1)))
                               (after-sign-part 0)))))
                       (after-sign-part (lambda (pos)
                         (let ((c (pk)))
                           (if (eqv? c #\0)
                               (begin (rd) (accum c pos (after-zero-part (+ pos 1))))
                               (after-first-digit pos)))))
                       (after-zero-part (lambda (pos)
                         (let ((c (pk)))
                           (if (eqv? c #\.)
                               (begin (rd) (accum c pos (decimals-part (+ pos 1))))
                               (if (or (eqv? c #\e) (eqv? c #\E))
                                   (begin (rd) (accum c pos (exponent-sign-part (+ pos 1))))
                                   (done pos))))))
                       (after-first-digit (lambda (pos)
                         (if (not (digit? (pk) 10))
                             (error "parse-number: non-digit following a sign")
                             (integer-part pos))))
                       (integer-part (lambda (pos)
                         (let ((c (pk)))
                           (if (digit? c 10)
                               (begin (rd) (accum c pos (integer-part (+ pos 1))))
                               (if (eqv? c #\.)
                                   (begin (rd) (accum c pos (decimals-part (+ pos 1))))
                                   (exponent-part pos))))))
                       (decimals-part (lambda (pos)
                         (let ((c (pk)))
                           (if (digit? c 10)
                               (begin (rd) (accum c pos (after-first-decimal-digit (+ pos 1))))
                               (error "parse-number: non-digit following a decimal point")))))
                       (after-first-decimal-digit (lambda (pos)
                         (let ((c (pk)))
                           (if (digit? c 10)
                               (begin (rd) (accum c pos (after-first-decimal-digit (+ pos 1))))
                               (exponent-part pos)))))
                       (exponent-part (lambda (pos)
                         (let ((c (pk)))
                           (if (or (eqv? c #\e) (eqv? c #\E))
                               (begin (rd) (accum c pos (exponent-sign-part (+ pos 1))))
                               (done pos)))))
                       (exponent-sign-part (lambda (pos)
                         (let ((c (pk)))
                           (if (or (eqv? c #\-) (eqv? c #\+))
                               (begin (rd) (accum c pos (exponent-after-sign-part (+ pos 1))))
                               (exponent-after-sign-part pos)))))
                       (exponent-after-sign-part (lambda (pos)
                         (if (not (digit? (pk) 10))
                             (error "parse-number: non-digit following an exponent mark")
                             (exponent-integer-part pos))))
                       (exponent-integer-part (lambda (pos)
                         (let ((c (pk)))
                           (if (digit? c 10)
                               (begin (rd) (accum c pos (exponent-integer-part (+ pos 1))))
                               (done pos)))))
                       (done (lambda (pos)
                         (make-string pos))))
               (string->number (sign-part))))))
        (let ((value (parse-value)))
          (let loop ((next-char (read-char port)))
             (if (eof-object? next-char)
                 value
                 (if (member next-char '(#\space #\newline #\tab #\return))
                     (loop (read-char port))
                     (error "json-read: extra data")))))))

    ;;
    ;; JSON writer
    ;;

    ;; `(json-write value [port])`: Converts value to a JSON file and writes it to port.
    ;; port defaults to (current-output-port).
    (define (json-write obj . port-option)
      (letrec*
        ((port (if (null? port-option)
                   (current-output-port)
                   (if (null? (cdr port-option))
                       (car port-option)
                       (error "json-read: too many arguments"))))
         (wr-string (lambda (s)
           (display #\" port)
           (let loop ((i 0) (j 0))
             (if (< j (string-length s))
                 (let* ((c          (string-ref s j))
                        (n          (char->integer c))
                        (ctrl-char? (or (<= n 31) (>= n 127)))
                        (x          (cond ((or (char=? c #\\) (char=? c #\")) c)
                                          ((and ctrl-char? (assv c reverse-json-string-escapes))
                                             => cdr)
                                          (else #f)))
                        (j+1        (+ j 1)))
                   (if (or x ctrl-char?)
                       (begin
                         (display (substring s i j) port)
                         (display #\\ port)
                         (if x
                             (begin
                               (display x port)
                               (loop j+1 j+1))
                             (begin
                               (display #\u port)
                               (display (substring (number->string (+ n #x10000) 16) 1 5) port)
                               (loop j+1 j+1))))
                       (loop i j+1)))
                 (begin
                   (display (substring s i j) port)
                   (display #\" port))))))
         (wr-prop (lambda (prop)
           (wr (symbol->string (car prop)))
           (display ":" port)
           (wr (cdr prop))))
         (wr-object (lambda (obj)
           (wr-props (hashtable->alist obj))))
         (wr-props (lambda (lst)
           (display "{" port)
           (if (pair? lst)
               (begin (wr-prop (car lst))
                      (let loop ((lst (cdr lst)))
                        (if (pair? lst)
                            (begin
                              (display "," port)
                              (wr-prop (car lst))
                              (loop (cdr lst)))))))
           (display "}" port)))
         (wr-array (lambda (obj)
           (display "[" port)
           (let loop ((not-first #f) (l obj))
              (if (not (null? l))
                  (begin
                    (if not-first (display "," port))
                    (wr (car l))
                    (loop #t (cdr l)))))
           (display "]" port)))
         (wr (lambda (obj)
           (cond ((number? obj)     (write (if (integer? obj) obj (inexact obj)) port))
                 ((string? obj)     (wr-string obj))
                 ((boolean? obj)    (display (if obj "true" "false") port))
                 ((json-null? obj)  (display "null" port))
                 ((list? obj)       (wr-array obj))
                 ((hashtable? obj)  (wr-object obj))
                 (else              (error "json-write: unwritable object" obj))))))
        (wr obj)))

    (define json-string-escapes
      '((#\" . #\")
        (#\\ . #\\)
        (#\/ . #\/)
        (#\b . #\x08)
        (#\t . #\x09)
        (#\n . #\x0A)
        (#\v . #\x0B)
        (#\f . #\x0C)
        (#\r . #\x0D)))

    (define reverse-json-string-escapes
      (map (lambda (x) (cons (cdr x) (car x))) json-string-escapes))

    ;;
    ;; Procedures for reading/writing for strings/files
    ;;

    ;; `(json-read-string str)`: Reads a JSON string str and returns the corresponding Scheme data.
    (define (json-read-string s)
      (let* ((p (open-input-string s))
             (result (json-read p)))
        (close-input-port p)
        result))

    ;; `(json-read-file filepath)`: Reads a JSON file at filepath and returns the
    ;; corresponding Scheme data.
    (define (json-read-file filepath)
      (let* ((p (open-input-file filepath))
             (result (json-read p)))
        (close-input-port p)
        result))

    ;; `(json-write-string value [prettify] [space-char space-count])`: Writes value to a new
    ;; JSON string which is returned. prettify writes the JSON in pretty-printed form, and
    ;; defaults to #f. space-char and space-count specify the indentation for pretty-printing,
    ;; and default to #\tab and 1.
    (define (json-write-string value . prettify-options)
      (define prettify (if (null? prettify-options)
                           #f
                           (car prettify-options)))
      (define space-char (if (< (length prettify-options) 2)
                             #\tab
                             (list-ref prettify-options 1)))
      (define space-count (if (< (length prettify-options) 3)
                              1
                              (list-ref prettify-options 2)))
      (define p (open-output-string))
      (json-write value p)
      (let ((result (get-output-string p)))
        (close-output-port p)
        (if prettify
            (json-prettify result space-char space-count)
            result)))

    ;; `(json-write-file value filepath [prettify] [space-char space-count])`: Writes value
    ;; to a JSON file at filepath. The optional arguments are the same as json-write-string
    ;; and have the same defaults.
    (define (json-write-file value filepath . prettify-options)
      (define prettify (if (null? prettify-options)
                           #f
                           (car prettify-options)))
      (define p (open-output-file filepath))
      (if prettify
          (display (apply json-write-string (cons value prettify-options)) p)
          (json-write value p))
      (close-output-port p))

    ;;
    ;; Prettify procedure
    ;;

    (define (json-prettify str space-char space-count)
      (define (add-spaces l level)
        (let loop ((i 0) (result l))
          (if (< i (* level space-count))
              (loop (+ i 1) (cons space-char result))
              result)))
      (define (is-empty slist char-look)
        (let loop ((l slist))
          (if (null? slist)
              #f
              (case (car l)
                ((#\])                              (if (equal? char-look #\[) (cdr l) #f))
                ((#\})                              (if (equal? char-look #\{) (cdr l) #f))
                ((#\space #\newline #\tab #\return) (loop (cdr l)))
                (else                               #f)))))
      (let loop ((l (string->list str))
                 (level 0)
                 (slist '())
                 (in-string #f))
        (cond ((null? l)
                 (list->string (reverse (cons #\newline slist))))
              ((equal? (car l) #\")
                 (loop (cdr l)
                       level
                       (cons (car l) slist)
                       (if (and (not (null? slist)) (equal? (car slist) #\\))
                           in-string
                           (not in-string))))
              (in-string
                (loop (cdr l) level (cons (car l) slist) #t))
              (else
                (case (car l)
                  ((#\[ #\{)
                     (if (is-empty (cdr l) (car l))
                         (loop (is-empty (cdr l) (car l))
                               level
                               (cons (if (equal? (car l) #\[) #\] #\}) (cons (car l) slist))
                               #f)
                         (loop (cdr l)
                               (+ level 1)
                               (add-spaces (cons #\newline (cons (car l) slist)) (+ level 1))
                               #f)))
                  ((#\] #\})
                     (loop (cdr l)
                           (- level 1)
                           (cons (car l) (add-spaces (cons #\newline slist) (- level 1)))
                           #f))
                  ((#\,)
                     (loop (cdr l)
                           level
                           (add-spaces (cons #\newline (cons (car l) slist)) level)
                           #f))
                  ((#\:)
                     (loop (cdr l)
                           level
                           (cons #\space (cons (car l) slist)) #f))
                  ((#\space #\newline #\tab #\return)
                     (loop (cdr l) level slist #f))
                  (else
                     (loop (cdr l) level (cons (car l) slist) #f)))))))
  )
)
