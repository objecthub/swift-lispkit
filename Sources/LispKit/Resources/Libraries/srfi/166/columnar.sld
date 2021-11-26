;;; SRFI 166 COLUMNAR
;;; Monadic Formatting
;;;
;;; A library of procedures for formatting Scheme objects to text in various ways, and
;;; for easily concatenating, composing and extending these formatters efficiently
;;; without resorting to capturing and manipulating intermediate strings. This SRFI is
;;; an updated version of SRFI 159, primarily with the difference that state variables
;;; are hygienic.
;;;
;;; Copyright © 2020 Marc Nieper-Wißkirchen. All rights reserved.
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
;;;   Copyright © 2021 Matthias Zenger. All rights reserved.

(define-library (srfi 166 columnar)
  
  (export columnar
          tabular
          wrapped
          wrapped/list
          wrapped/char
          justified
          from-file
          line-numbers)
  
  (import (scheme base)
          (scheme case-lambda)
          (scheme file)
          (srfi 1)
          (srfi 8)
          (srfi 133)
          (srfi 158)
          (rename (srfi 165)
            (computation-bind bind)
            (computation-bind/forked bind/forked)
            (computation-environment-copy environment-copy)
            (computation-local local)
            (computation-pure return)
            (computation-sequence sequence))
          (srfi 166 base))
  
  (begin
    (define (word-wrap width len-vec)
      (let* ((n (vector-length len-vec))
       ;; (vector-ref total-cost I) is the total cost of all lines
       ;; starting with a line starting with word I.
       (total-cost (make-vector n +inf.0))
       ;; (vector-ref last-in-line I) is the index of the last word
       ;; in a line starting with word I.
       (last-in-line (make-vector n (- n 1))))
        (do ((i (- n 1) (- i 1)))
      ((< i 0))
          (do ((j i (+ j 1))
         (curr-len (vector-ref len-vec i)
             (+ curr-len (if (= j (- n 1))
                 0
                 (+ (vector-ref len-vec (+ j 1)) 1)))))
        ((or (>= j n)
             (> curr-len width)))
      (let ((cost (if (= j (- n 1))
          0
          (+ (expt (- width curr-len) 3)
             (vector-ref total-cost (+ j 1))))))
        (when (< cost (vector-ref total-cost i))
          (vector-set! total-cost i cost)
          (vector-set! last-in-line i j)))))
        (let loop ((i 0))
          (if (>= i n)
        '()
        (let ((j (vector-ref last-in-line i)))
          (cons j (loop (+ j 1))))))))
  )
  
  (begin ; "columnar.scm"
    (define (string-split/words str sep?)
      (let loop ((chars (string->list str)) (word '()))
        (cond ((null? chars)
         (list (list->string (reverse word))))
        ((sep? (car chars))
         (if (null? word)
             (loop (cdr chars) '())
             (cons (list->string (reverse word))
             (loop (cdr chars) '()))))
        (else
         (loop (cdr chars) (cons (car chars) word))))))
    
    (define (spaces n)
      (make-computation
       (lambda (format)
         (do ((n n (- n 1)))
       ((not (positive? n)))
           (format #\space)))))
    
    (define (repeated str)
      (make-computation
       (lambda (format)
         (let loop ()
           (format str)
           (format nl)
           (loop)))))
    
    (define (hidden str)
      (fn ((string-width string-width))
        (make-computation
         (lambda (format)
           (string-for-each
      (lambda (ch)
        (if (char=? ch #\newline)
            (format (fn ((%row row))
          (with! (row (+ %row 1))
                 (col 0))))
            (format (fn ((%col col))
          (with! (col (+ %col (string-width (string ch)))))))))
      str)))))
    
    (define (with-output-generator fmt)
      (make-computation
       (lambda (format)
         (make-coroutine-generator
          (lambda (yield)
      (format (with ((output
          (lambda (str)
            (yield str)
            (hidden str))))
          (bind/forked fmt))))))))
    
    (define (make-line-generator gen)
      (make-coroutine-generator
       (lambda (yield)
         (let loop ((line '()))
           (let ((str (gen)))
       (cond ((eof-object? str)
        (unless (null? line)
          (yield (list->string (reverse line)))))
             ((string=? str "")
        (loop line))
             (else
        (let loop/chars ((chars (string->list str)) (line line))
          (cond ((null? chars)
           (loop line))
          ((char=? (car chars) #\newline)
           (yield (list->string (reverse line)))
           (loop/chars (cdr chars) '()))
          (else
           (loop/chars (cdr chars) (cons (car chars) line))))))))))))
    
    (define-record-type <column>
      (%make-column content infinite? justification width)
      column?
      (content column-content column-set-content!)
      (infinite? column-infinite? column-set-infinite?!)
      (justification column-justification column-set-justification!)
      (width column-width column-set-width!)
      (actual-width column-actual-width column-set-actual-width!)
      (formatted column-formatted column-set-formatted!))
    
    (define (make-column)
      (%make-column #f #f 'left #f))
    
    (define (make-string-column str string-width)
      (%make-column (repeated str) #t 'left (string-width str)))
    
    (define (with-output-generators fmt*)
      (sequence (map with-output-generator fmt*)))
    
    (define (columnar* cols)
      (bind (with-output-generators
       (map (lambda (%col)
        (with ((col 0)
               (width (column-width %col)))
                      (column-content %col)))
            cols))
        (lambda (gens)
          (let ((gens (map make-line-generator gens))
          (infinite-count (count column-infinite? cols)))
      (let loop ()
        (let* ((lines (map (lambda (gen) (gen)) gens))
         (running-count (count string? lines)))
          (if (>= infinite-count running-count)
        nothing
        (each (each-in-list (map (lambda (col line)
                 ((column-formatted col)
                  (if (eof-object? line) "" line)))
               cols lines))
              #\newline
              (loop)))))))))
    
    (define (parse-columns string-width args)
      (let loop ((args args))
        (cond ((null? args)
         '())
        ((string? (car args))
         (cons (make-string-column (car args) string-width)
         (loop (cdr args))))
        (else
         (let ((col (make-column)))
           (let loop/col ((args args))
             (case (car args)
         ((infinite)
          (column-set-infinite?! col #t)
          (loop/col (cdr args)))
         ((left center right)
          => (lambda (justification)
               (column-set-justification! col justification)
               (loop/col (cdr args))))
         (else
          (cond
           ((number? (car args))
            (column-set-width! col (car args))
            (loop/col (cdr args)))
           (else
            (let ((content (car args)))
              (column-set-content! col content)
              (cons col
              (loop (cdr args))))))))))))))
    
    (define (fixed-width? width)
      (and (integer? width) (positive? width)))
    
    (define (fractional-width? width)
      (and width (not (fixed-width? width))))
    
    (define (proportional-width? width)
      (not width))
    
    (define (set-actual-widths! total-width columns)
      (let loop ((cols columns) (remaining-width total-width) (proportional-count 0))
        (if (null? cols)
      (let loop ((cols columns) (proportional-width remaining-width))
        (if (null? cols)
            (let loop ((cols columns)
           (proportional-width proportional-width)
           (proportional-count proportional-count))
        (unless (zero? proportional-count)
          (let ((width (column-width (car cols))))
            (cond ((proportional-width? width)
             (let ((width (quotient (+ proportional-width proportional-count -1)
                  proportional-count)))
               (column-set-actual-width! (car cols) width)
               (loop (cdr cols)
               (- proportional-width width)
               (- proportional-count 1))))
            (else
             (loop (cdr cols) proportional-width proportional-count))))))
            (let ((width (column-width (car cols))))
        (cond ((fractional-width? width)
               (let ((actual-width (exact (truncate (* width remaining-width)))))
           (column-set-actual-width! (car cols) actual-width)
           (loop (cdr cols) (- proportional-width actual-width))))
              (else
               (loop (cdr cols) proportional-width))))))
      (let ((width (column-width (car cols))))
        (cond ((fixed-width? width)
         (column-set-actual-width! (car cols) width)
         (loop (cdr cols) (- remaining-width width) proportional-count))
        ((fractional-width? width)
         (loop (cdr cols) remaining-width proportional-count))
        (else
         (loop (cdr cols) remaining-width (+ 1 proportional-count))))))))
    
    (define (columnar . args)
      (if (null? args)
          nothing
          (fn ((width width)
         (string-width string-width))
      (let* ((cols (parse-columns string-width args)))
        (set-actual-widths! width cols)
        (let loop ((cols cols))
          (let ((width (column-actual-width (car cols))))
            (cond ((null? (cdr cols))
             (column-set-formatted! (car cols)
                  (let ((trimmed
                   (case (column-justification (car cols))
                     ((left) trimmed/right)
                     ((center) trimmed/both)
                     ((right) trimmed))))
                    (lambda (fmt)
                (trimmed width fmt)))))
            (else
             (column-set-formatted! (car cols)
                  (let ((padded
                   (case (column-justification (car cols))
                     ((left) padded/right)
                     ((center) padded/both)
                     ((right) padded))))
                    (lambda (fmt)
                (padded width fmt))))
             (loop (cdr cols))))))
        (columnar* cols)))))
    
    (define (tabular . args)
      (fn ((string-width string-width))
        (bind
      (let loop ((args args) (infinite? #f) (width #f))
        (cond ((null? args)
         (return '()))
        ((number? (car args))
         (loop (cdr args) infinite? (car args)))
        ((eq? 'infinite (car args))
         (bind (loop (cdr args) #t width)
           (lambda (cols)
             (return (cons (car args) cols)))))
        ((or (symbol? (car args)) (string? (car args)))
         (bind (loop (cdr args) infinite? width)
           (lambda (cols)
             (return (cons (car args) cols)))))
        (else
         (bind (loop (cdr args) #f #f)
           (lambda (cols)
             (if infinite?
           (return (if width
                 (cons* width (car args) cols)
                 (cons (car args) cols)))
           (bind (best-width string-width (car args) width)
             (lambda (fmt width)
               (return (cons* width fmt cols))))))))))
          (lambda (cols)
      (apply columnar cols)))))
    
    (define (best-width string-width fmt width)
      (call-with-output fmt
        (lambda (str)
          (let ((min-width (max-line-width string-width str)))
      (return (displayed str)
        (if (and (integer? width) (positive? width))
            (max width min-width)
            min-width))))))
    
    (define (max-line-width string-width str)
      (let loop ((chars (string->list str)) (m 0))
        (let loop/line ((chars chars) (n 0))
          (cond ((null? chars)
           (max m n))
          ((char=? #\newline (car chars))
           (loop (cdr chars) (max m n)))
          (else
           (loop/line (cdr chars)
          (+ n (string-width (string (car chars))))))))))
    
    (define (wrapped/list str*)
      (fn ((width width)
           (string-width string-width)
           (pad-char pad-char))
          (joined/suffix (lambda (str*)
               (joined displayed str* pad-char))
             (wrap-fold-right-words width string-width
                  cons '() str*)
             #\newline)))
    
    (define (wrapped . fmt*)
      (call-with-output (each-in-list fmt*)
        (lambda (str)
          (fn ((word-separator? word-separator?))
      (wrapped/list (string-split/words str word-separator?))))))
    
    (define (wrapped/char . fmt*)
      (fn ((width width)
           (string-width string-width))
        (bind (with-output-generator (each-in-list fmt*))
          (lambda (gen)
      (let loop ((line '()) (n 0))
        (let ((str (gen)))
          (if (eof-object? str)
        (if (null? line)
            nothing
            (each (displayed (list->string (reverse line)))
            #\newline))
        (let loop/chars ((chars (string->list str)) (line line) (n n))
          (if (null? chars)
              (loop line n)
              (let ((i (string-width (string (car chars)))))
          (if (and (> n 0) (> (+ n i) width))
              (each (displayed (list->string (reverse line)))
              #\newline
              (loop/chars (cdr chars) (list (car chars)) i))
              (loop/chars (cdr chars) (cons (car chars) line) (+ n i)))))))))))))
    
    (define (justified . fmt*)
      (call-with-output (each-in-list fmt*)
        (lambda (str)
          (fn ((word-separator? word-separator?)
         (width width)
         (string-width string-width))
      (joined/last (justifier string-width width)
             justify-last
             (wrap-fold-right-words width string-width
                  cons '()
                  (string-split/words str word-separator?))
             #\newline)))))
    
    (define (justify-last fmt*)
      (each (joined displayed fmt* #\space) #\newline))
    
    (define (justifier string-width width)
      (lambda (words)
        (cond
         ((null? words)
          nl)
         ((null? (cdr words))
          (displayed (car words)))
         (else
          (let*-values (((char-count) (fold (lambda (word n)
                 (+ n (string-width word)))
               0 words))
            ((word-count) (length words))
            ((space-count) (max 0 (- width char-count)))
            ((q r) (truncate/ space-count (- word-count 1))))
      (each (car words)
            (let loop ((words (cdr words)) (i 0))
        (if (null? words)
            nothing
            (each (spaces (+ q (if (< i r) 1 0)))
            (car words)
            (loop (cdr words) (+ i 1)))))))))))
    
    (define (wrap-fold-right-words max-width string-width proc seed words)
      (let loop ((last-in-line*
            (word-wrap max-width
           (vector-map string-width
                 (list->vector words))))
           (words words)
           (i 0))
        (if (null? last-in-line*)
      seed
      (receive (seed line)
          (let ((last-in-line (car last-in-line*)))
            (let loop/word ((words words) (i i))
        (if (<= i last-in-line)
            (receive (seed line)
          (loop/word (cdr words) (+ i 1))
              (values seed
                (cons (car words) line)))
            (values (loop (cdr last-in-line*) words i)
              '()))))
        (proc line seed)))))
    
    (define (from-file filename)
      (make-computation
       (lambda (format)
         (call-with-input-file filename
          (lambda (port)
       (let loop ()
         (let ((line (read-line port)))
           (unless (eof-object? line)
             (format line)
             (format #\newline)
             (loop)))))))))
    
    (define line-numbers
      (case-lambda
       (()
        (line-numbers 1))
       ((start)
        (joined/range displayed start #f #\newline))))
  )
)
