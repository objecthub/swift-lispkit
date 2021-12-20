;;; SRFI 166 BASE
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

(define-library (srfi 166 base)
  
  (export show
          each
          each-in-list
          call-with-output
          displayed
          written
          written-shared
          written-simply
          numeric
          numeric/comma
          numeric/si
          numeric/fitted
          nothing
          nl
          fl
          space-to
          tab-to
          escaped
          maybe-escaped
          padded
          padded/right
          padded/both
          trimmed
          trimmed/right
          trimmed/both
          trimmed/lazy
          fitted
          fitted/right
          fitted/both
          joined
          joined/prefix
          joined/suffix
          joined/last
          joined/dot
          joined/range
          upcased
          downcased
          fn
          with
          with!
          forked
          port
          row
          col
          width
          output
          writer
          string-width
          pad-char
          ellipsis
          radix
          precision
          decimal-sep
          decimal-align
          sign-rule
          comma-rule
          comma-sep
          word-separator?
          ambiguous-is-wide?
          output-default)
  
  (import (scheme base)
          (scheme case-lambda)
          (scheme char)
          (scheme complex)
          (scheme inexact)
          (scheme write)
          (srfi 1)
          (srfi 8)
          (except (srfi 125) string-hash string-ci-hash)
          (srfi 128)
          (srfi 133)
          (srfi 145)
          (rename (srfi 165)
                  (computation-bind bind)
                  (computation-fn fn)
                  (computation-forked forked)
                  (computation-pure return)
                  (computation-each each)
                  (computation-each-in-list each-in-list)
                  (computation-with with)
                  (computation-with! with!)))
  
  (begin
    
    (define (output-default str)
      (fn ((port port)
           (%col col)
           (%row row)
           (string-width string-width))
          (write-string str port)
          (let ((len (string-length str)))
            (let loop ((i 0) (j 0) (%row %row) (%col %col))
              (cond
                ((= i len)
                  (with! (row %row) (col (+ %col (string-width (substring str j len))))))
                ((char=? (string-ref str i) #\newline)
                  (loop (+ i 1) (+ i 1) (+ %row 1) 0))
                (else
                  (loop (+ i 1) j %row %col)))))))
    
    (define-computation-type make-show-environment run
                             port
                             (col 0)
                             (row 0)
                             (width 78)
                             (radix 10)
                             (pad-char #\space)
                             (output output-default)
                             (word-separator? char-whitespace?)
                             (string-width string-length)
                             (ellipsis "")
                             ambiguous-is-wide?
                             decimal-align
                             decimal-sep
                             comma-sep
                             comma-rule
                             sign-rule
                             precision
                             writer)
    
    (define abbreviations
      '((quote . "'")
        (quasiquote . "`")
        (unquote . ",")
        (unquote-splicing . ",@")
        (syntax . "#'")
        (quasisyntax . "#`")
        (unsyntax . "#,")
        (unsyntax-splicing . "#,@")))
    
    (define (do-output str)
      (fn ((output output)) ((or output output-default) str)))
    
    (define (show out . args)
      (let ((fmt (each-in-list args)))
        (cond
          ((output-port? out)
            (%show out fmt))
          ((eq? #t out)
            (%show (current-output-port) fmt))
          ((eq? #f out)
            (let ((out (open-output-string)))
              (%show out fmt)
              (get-output-string out)))
          (else
            (assume #f)))))
    
    (define (%show out fmt)
      (run (with ((port out)
                  (default-computation displayed))
                 fmt)))
    
    (define nothing (return (if #f #f)))
    
    (define (displayed obj)
      (cond ((procedure? obj) obj)
            ((string? obj) (do-output obj))
            ((char? obj) (do-output (string obj)))
            (else (written obj))))
    
    (define (written obj)
      (fn ((writer writer))
          ((or writer written-default) obj)))
    
    (define (call-with-output producer consumer)
      (let ((out (open-output-string)))
        (forked (with ((port out)
                       (output output-default))
                      producer)
                (fn () (consumer (get-output-string out))))))
    
    (define (try-fitted fmt . fail*)
      (let loop ((fmt fmt) (fail* fail*))
        (if (null? fail*)
            fmt
            (%try-fitted fmt
                         (fn ()
                             (loop (car fail*) (cdr fail*)))))))
    
    (define (%try-fitted fmt fail)
      (fn ((width width)
           (string-width string-width))
          (let ((out (open-output-string)))
            (call-with-current-continuation
              (lambda (abort)
                (forked
                  (with ((port out)
                         (output
                           (lambda (str)
                             (fn ((col col))
                                 (let loop ((chars (string->list str)) (col col))
                                   (cond ((null? chars)
                                           (output-default str))
                                         ((char=? (car chars) #\newline)
                                           (loop (cdr chars) 0))
                                         (else
                                           (let ((col
                                                   (+ col
                                                      (string-width (string (car chars))))))
                                             (if (> col width)
                                                 (abort fail)
                                                 (loop (cdr chars) col))))))))))
                        fmt)
                  (displayed (get-output-string out))))))))
    
    (define (write-to-string obj)
      (let ((out (open-output-string)))
        (write-simple obj out)
        (get-output-string out)))
    
    (define escaped
      (case-lambda
        ((str)
          (escaped str #\"))
                   ((str quote-ch)
                    (escaped str quote-ch #\\))
                   ((str quote-ch esc-ch)
                    (escaped str quote-ch esc-ch (lambda (ch) #f)))
                   ((str quote-ch esc-ch renamer)
                    (make-computation
                      (lambda (format)
                        (string-for-each
                          (lambda (ch)
                            (if esc-ch
                                (cond ((or (and (char=? ch quote-ch) quote-ch)
                                           (and (char=? ch esc-ch) esc-ch)
                                           (renamer ch))
                                        => (lambda (ch)
                                             (format (each esc-ch ch))))
                                      (else
                                        (format ch)))
                                (if (char=? ch quote-ch)
                                    (format (each quote-ch quote-ch))
                                    (format ch))))
                          str))))))
    
    (define maybe-escaped
      (case-lambda
        ((str pred)
          (maybe-escaped str pred #\"))
                         ((str pred quote-ch)
                          (maybe-escaped str pred quote-ch #\\))
                         ((str pred quote-ch esc-ch)
                          (maybe-escaped str pred quote-ch esc-ch (lambda (ch) #f)))
                         ((str pred quote-ch esc-ch renamer)
                          (call-with-current-continuation
                            (lambda (return)
                              (string-for-each
                                (lambda (ch)
                                  (when (or (char=? ch quote-ch)
                                            (eqv? ch esc-ch)
                                            (renamer ch)
                                            (pred ch))
                                    (return (each quote-ch
                                                  (escaped str quote-ch esc-ch renamer)
                                                  quote-ch))))
                                str)
                           (return (displayed str)))))))
    
    (define (datum-fold proc seed obj)
      (let ((store (make-hash-table (make-eq-comparator))))
        (let walk! ((obj obj))
          (when (or (pair? obj) (vector? obj))
            (let ((walk? #f))
              (hash-table-update! store obj values
                                  (lambda ()
                                    (set! walk? #t)
                                    'processing)
                                  (lambda (val)
                                    (case val
                                          ((processing)
                                            'cycle)
                                          ((structured)
                                            'shared)
                                          (else
                                            val))))
              (when walk?
                (if (pair? obj)
                    (begin (walk! (car obj))
                           (walk! (cdr obj)))
                    (vector-for-each walk! obj)))
              (hash-table-update! store
                                  obj
                                  (lambda (val)
                                    (case val
                                          ((processing)
                                            'structured)
                                          (else
                                            val)))))))
        (let loop ((seed seed) (obj obj))
          (if (or (pair? obj) (vector? obj))
              (let ((state #f))
                (hash-table-update! store
                                    obj
                                    (lambda (val)
                                      (set! state val)
                                      'seen))
                (cond ((eq? state 'seen)
                        (proc seed obj 'seen))
                      ((pair? obj)
                        (loop (loop (proc seed obj state) (car obj)) (cdr obj)))
                      ((vector? obj)
                        (vector-fold loop (proc seed obj state) obj))))
              (proc seed obj 'simple)))))
    
    (define (make-label-table obj simple? shared?)
      (let ((labels (make-hash-table (make-eq-comparator))))
        (unless simple?
          (datum-fold (lambda (count obj state)
                        (case state
                              ((shared)
                                (if shared?
                                    (begin
                                      (hash-table-set! labels obj count)
                                      (+ count 1))
                                    count))
                              ((cycle)
                                (hash-table-set! labels obj count)
                                (+ count 1))
                              (else
                                count)))
                      0 obj))
        labels))
    
    (define (labelled count obj labels proc)
      (cond ((hash-table-ref/default labels obj #f)
              => (lambda (i)
                   (if (< i count)
                       (each "#" (number->string i) "#" (return count))
                       (each "#" (number->string i) "=" (proc (+ count 1))))))
            (else
              (proc count))))
    
    (define (labelled/tail count obj labels proc)
      (cond ((hash-table-ref/default labels obj #f)
              => (lambda (i)
                   (if (< i count)
                       (each ". #" (number->string i) "#" (return count))
                       (each ". #" (number->string i) "=("
                             (bind (proc (+ count 1))
                                   (lambda (count)
                                     (each ")" (return count))))))))
            (else
              (proc count))))
    
    (define (signed? n)
      (or (negative? n)
          (and (not (eqv? 0.0 -0.0))
               (eqv? n -0.0))))
    
    (define (intersperse-commas chars comma comma-sep)
      (let loop ((chars (reverse chars)) (i 0) (result '()))
        (if (null? chars)
            result
            (if (= i comma)
                (loop (cdr chars) 1 (cons (car chars)
                                          (cons comma-sep result)))
                (loop (cdr chars) (+ i 1) (cons (car chars) result))))))
    
    (define (digit n rad)
      (integer->char (+ n (if (< n 10)
                              (char->integer #\0)
                              (- (char->integer #\a) 10)))))
    
    (define (digits x rad prec)
      (let ((%prec (or prec 6)))
        (let loop ((x (round (* (exact x) (expt rad %prec))))
                   (i 0)
                   (fractional-digits '()))
          (if (= i %prec)
              (let loop ((x x) (digits '()))
                (if (zero? x)
                    (if (null? digits)
                        (values '(#\0) fractional-digits)
                        (values digits fractional-digits))
                    (receive (q r) (truncate/ x rad)
                             (loop q (cons (digit r rad) digits)))))
              (receive (q r) (floor/ x rad)
                       (loop q (+ i 1) (if (or prec
                                               (not (zero? r))
                                               (not (null? fractional-digits)))
                                           (cons (digit r rad) fractional-digits)
                                           '())))))))
    
    (define (numeric num . args)
      (apply %numeric #f num args))
    
    (define %numeric
      (case-lambda
        ((width num radix prec sign comma comma-sep decimal-sep %decimal-align)
          (with ((decimal-align %decimal-align))
                (%numeric width num radix prec sign comma comma-sep decimal-sep)))
        ((width num radix prec sign comma comma-sep %decimal-sep)
          (with ((decimal-sep %decimal-sep))
                (%numeric width num radix prec sign comma comma-sep)))
        ((width num radix prec sign comma %comma-sep)
          (with ((comma-sep %comma-sep))
                (%numeric width num radix prec sign comma)))
        ((width num radix prec sign comma)
          (with ((comma-rule comma))
                (%numeric width num radix prec sign)))
        ((width num radix prec sign)
          (with ((sign-rule sign))
                (%numeric width num radix prec)))
        ((width num radix prec)
          (with ((precision prec))
                (%numeric width num radix)))
        ((width num %radix)
          (with ((radix %radix))
                (%numeric width num)))
        ((width num)
          (fn ((radix radix)
               (precision precision)
               (sign sign-rule)
               (comma comma-rule)
               (comma-sep comma-sep)
               (decimal-sep decimal-sep)
               (decimal-align decimal-align))
              (let* ((comma-sep (or comma-sep #\,))
                     (decimal-sep (or decimal-sep
                                      (if (char=? comma-sep #\.) #\, #\.))))
                (define (add-commas n)
                  (receive (digits fractional-digits) (digits n radix precision)
                           (values (if comma
                                       (intersperse-commas digits comma comma-sep)
                                       digits)
                                   fractional-digits)))
                (define (add-sign n sign)
                  (receive (digits fractional-digits) (add-commas (abs n))
                           (cond ((signed? n)
                                   (if (pair? sign)
                                       (values (cons (car sign) digits)
                                               fractional-digits
                                               (displayed (cdr sign)))
                                       (values (cons #\- digits)
                                               fractional-digits
                                               nothing)))
                                 ((eq? sign #t)
                                   (values (cons #\+ digits) fractional-digits nothing))
                                 (else
                                   (values digits fractional-digits nothing)))))
                (define (numeric/finite n sign)
                  (receive (digits fractional-digits postfix) (add-sign n sign)
                           (each (if decimal-align
                                     (spaces (- decimal-align (length digits) 1) #\space)
                                     nothing)
                                 (list->string digits)
                                 (if (null? fractional-digits)
                                     nothing
                                     decimal-sep)
                                 (list->string fractional-digits)
                                 postfix)))
                (define (numeric/real n sign)
                  (if (finite? n)
                      (numeric/finite n sign)
                      (displayed (number->string n))))
                (let ((fmt (if (real? num)
                               (numeric/real num sign)
                               (each (numeric/real (real-part num) sign)
                                     (numeric/real (imag-part num) #t)
                                     "i"))))
                  (if width
                      (call-with-output fmt
                                        (lambda (str)
                                          (if (> (string-length str) width)
                                              (if (and precision (positive? precision))
                                                  (each (spaces (- width precision 1) #\#)
                                                        decimal-sep
                                                        (spaces precision #\#))
                                                  (spaces width #\#))
                                              (displayed str))))
                      fmt)))))))
    
    (define (numeric/comma number . args)
      (fn ((comma comma-rule))
          (with ((comma-rule (or comma 3)))
                (apply numeric number args))))
    
    (define numeric/si
      (case-lambda
        ((num)
          (numeric/si num 1024))
        ((num base)
          (numeric/si num base nothing))
        ((num base separator)
          (cond ((zero? num)
                  (each "0" separator))
                ((negative? num)
                  (each "-" (%numeric/si (- num) base separator)))
                (else
                  (%numeric/si num base separator))))))
    
    (define (%numeric/si num base separator)
      (let* ((k (min (exact ((if (< num 1) ceiling floor)
                             (/ (abs (log num)) (log base))))
                     8))
             (d (/ num (expt base (if (< num 1) (- k) k))))
             (n (/ (round (* d 10)) 10)))
        (each (number->string ((if (integer? n) exact inexact) n))
              separator
              (vector-ref (if (> num 1)
                              '#("" "" "M" "G" "T" "E" "P" "Z" "Y")
                              '#("" "m" "μ" "n" "p" "f" "a" "z" "y"))
                          k)
              (cond ((and (> num 1) (= k 1))
                      (if (= base 1024) "Ki" "k"))
                    ((and (= base 1024) (>= k 1))
                      "i")
                    (else
                      nothing)))))
    
    (define (numeric/fitted width num . args)
      (apply %numeric width num args))
    
    (define (make-written/number %radix precision)
      (cond
        ((and precision (or (not %radix) (= %radix 10)))
          (lambda (n)
            (numeric n)))
        ((assoc %radix '((2 . "#b") (8 . "#o") (16 . "#x")) =)
          => (lambda (pair)
               (lambda (n)
                 (if (exact? n)
                     (each (cdr pair) (number->string n (car pair)))
                     (with ((radix %radix)) (numeric n))))))
        (else
          (lambda (n)
            (displayed (number->string n))))))
    
    ;; TODO: Use pretty?
    
    (define (written/pair pretty? obj count do-write labels)
      (bind
        (each
          "("
          (let loop/list ((count count) (obj obj))
            (bind (return count)
                  (lambda (count)
                    (do-write count (car obj)))
                  (lambda (count)
                    (let ((obj (cdr obj)))
                      (cond ((null? obj)
                              (return count))
                            ((pair? obj)
                              (each
                                " "
                                (labelled/tail count obj labels
                                               (lambda (count)
                                                 (loop/list count
                                                            obj)))))
                            (else
                              (bind (each
                                      " . "
                                      (do-write count obj))))))))))
        (lambda (count)
          (each ")" (return count)))))
    
    ;; FIXME
    (define written/list written/pair)
    
    (define (written/vector pretty? obj count do-write labels)
      (each
        #\#
        (written/list pretty? (vector->list obj) count do-write labels)))
    
    (define (do-write pretty? obj simple? shared?)
      (fn ((radix radix)
           (precision precision))
          (let ((written/number (make-written/number radix precision))
                (labels (make-label-table obj simple? shared?)))
            (let loop ((count 0) (obj obj))
              (labelled
                count obj labels
                (lambda (count)
                  (cond ((pair? obj)
                          (written/pair pretty? obj count loop labels))
                        ((vector? obj)
                          (written/vector pretty? obj count loop labels))
                        ((number? obj)
                          (each (written/number obj)
                                (return count)))
                        (else
                          (each (displayed (write-to-string obj))
                                (return count))))))))))
    
    (define (written-default obj)
      (do-write #f obj #f #f))
    
    (define (written-shared obj)
      (do-write #f obj #f #t))
    
    (define (written-simply obj)
      (do-write #f obj #f #f))
    
    (define (pretty-default obj)
      (do-write #t obj #f #f))
    
    (define (pretty-shared obj)
      (do-write #t obj #f #t))
    
    (define (pretty-simply obj)
      (do-write #t obj #f #f))
    
    (define (with-output-mapper mapper fmt)
      (fn ((%output output))
          (with ((output (lambda (str)
                           (with ((output %output))
                                 (%output (mapper str))))))
                fmt)))
    
    (define nl
      (displayed "\n"))
    
    (define fl
      (fn ((col col))
          (if (zero? col) nothing nl)))
    
    (define tab-to
      (case-lambda
        (()
          (tab-to 8))
        ((tab-width)
          (fn ((i col) (pad-char pad-char))
              (let loop ((i i))
                (if (positive? (modulo i tab-width))
                    (each pad-char
                          (fn () (loop (+ i 1))))
                    nothing))))))
    
    (define (spaces n pad-char)
      (make-computation
        (lambda (format)
          (do ((n n (- n 1)))
            ((not (positive? n)))
            (format pad-char)))))
    
    (define (space-to target-col)
      (fn ((i col) (pad-char pad-char))
          (spaces (- target-col i) pad-char)))
    
    (define (transformed* transformer fmt*)
      (with-output-mapper transformer (each-in-list fmt*)))
    
    (define (upcased . fmt*)
      (transformed* string-upcase fmt*))
    
    (define (downcased . fmt*)
      (transformed* string-downcase fmt*))
    
    ;;; Concatenation.
    
    (define joined
      (case-lambda
        ((mapper list)
          (joined mapper list ""))
        ((mapper list sep)
          (make-computation
            (lambda (format)
              (do ((list list (cdr list)) (%sep nothing sep))
                ((null? list))
                (format %sep)
                (format (mapper (car list)))))))))
    
    (define joined/prefix
      (case-lambda
        ((mapper list)
          (joined/prefix mapper list ""))
        ((mapper list sep)
          (make-computation
            (lambda (format)
              (do ((list list (cdr list)))
                ((null? list))
                (format sep)
                (format (mapper (car list)))))))))
    
    (define joined/suffix
      (case-lambda
        ((mapper list)
          (joined/suffix mapper list ""))
        ((mapper list sep)
          (make-computation
            (lambda (format)
              (do ((list list (cdr list)))
                ((null? list))
                (format (mapper (car list)))
                (format sep)))))))
    
    (define joined/last
      (case-lambda
        ((mapper last-mapper list)
          (joined/last mapper list ""))
        ((mapper last-mapper list sep)
          (make-computation
            (lambda (format)
              (do ((list list (cdr list)) (%sep nothing sep))
                ((or (null? list)
                     (and (null? (cdr list))
                          (begin
                            (format %sep)
                            (format (last-mapper (car list)))
                            #t))))
                (format %sep)
                (format (mapper (car list)))))))))
    
    (define joined/dot
      (case-lambda
        ((mapper dot-mapper list)
          (joined/dot mapper dot-mapper list ""))
        ((mapper dot-mapper list sep)
          (make-computation
            (lambda (format)
              (do ((list list (cdr list)) (%sep nothing sep))
                ((or (null? list)
                     (and (not (pair? list))
                          (begin
                            (format %sep)
                            (format (dot-mapper list))
                            #t))))
                (format %sep)
                (format (mapper (car list)))))))))
    
    (define joined/range
      (case-lambda
        ((mapper start)
          (joined/range mapper start #f))
        ((mapper start end)
          (joined/range mapper start end ""))
        ((mapper start end sep)
          (make-computation
            (lambda (format)
              (do ((i start (+ i 1)) (%sep nothing sep))
                ((and end (>= i end)))
                (format %sep)
                (format (mapper i))))))))
    
    ;;; Padding and Trimming.
    
    (define (padded width . fmt*)
      (call-with-output (each-in-list fmt*)
                        (lambda (str)
                          (fn ((string-width string-width)
                               (pad-char pad-char))
                              (each (spaces (- width (string-width str)) pad-char)
                                    str)))))
    
    (define (padded/right width . fmt*)
      (fn ((pad-char pad-char)
           (start-col col))
          (each (each-in-list fmt*)
                (fn ((end-col col))
                    (spaces (- width (- end-col start-col)) pad-char)))))
    
    (define (padded/both width . fmt*)
      (call-with-output (each-in-list fmt*)
                        (lambda (str)
                          (fn ((string-width string-width)
                               (pad-char pad-char))
                              (let* ((space (max (- width (string-width str)) 0))
                                     (space/2 (quotient space 2)))
                                (each (spaces space/2 pad-char)
                                      str
                                      (spaces (if (even? space)
                                                  space/2
                                                  (+ 1 space/2)) pad-char)))))))
    
    (define (%trimmed ellipsis-width width trimmer . fmt*)
      (fn ((string-width string-width))
          (let loop ((fmt* fmt*) (n 0) (str ""))
            (if (null? fmt*)
                (displayed str)
                (call-with-output (car fmt*)
                                  (lambda (s)
                                    (let ((str (string-append str s))
                                          (n (+ n (string-width s))))
                                      (if (> n width)
                                          (trimmer str n (- width ellipsis-width))
                                          (loop (cdr fmt*) n str)))))))))
    
    (define (trimmed width . fmt*)
      (fn ((ellipsis ellipsis)
           (string-width string-width))
          (let ((ellipsis (or ellipsis "")))
            (apply %trimmed
                   (string-width ellipsis)
                   width
                   (lambda (str actual-width max-width)
                     (let loop ((chars (string->list str)) (i 0) (n actual-width))
                       (if (<= n max-width)
                           (each ellipsis (string-copy str i))
                           (loop (cdr chars)
                                 (+ i 1)
                                 (- n (string-width (string (car chars))))))))
                   fmt*))))
    
    (define (trimmed/right width . fmt*)
      (fn ((ellipsis ellipsis)
           (string-width string-width))
          (let ((ellipsis (or ellipsis "")))
            (apply %trimmed
                   (string-width ellipsis)
                   width
                   (lambda (str actual-width max-width)
                     (let loop ((chars (reverse (string->list str)))
                                (i (string-length str))
                                (n actual-width))
                       (if (<= n max-width)
                           (each (substring str 0 i) ellipsis)
                           (loop (cdr chars)
                                 (- i 1)
                                 (- n (string-width (string (car chars))))))))
                   fmt*))))
    
    (define (trimmed/both width . fmt*)
      (fn ((ellipsis ellipsis)
           (string-width string-width))
          (let ((ellipsis (or ellipsis "")))
            (apply %trimmed
                   (* (string-width ellipsis) 2)
                   width
                   (lambda (str actual-width max-width)
                     (let ((chars (string->list str)))
                       (let loop ((chars chars)
                                  (chars/reversed (reverse chars))
                                  (i 0)
                                  (j (string-length str))
                                  (n actual-width))
                         (if (<= n max-width)
                             (each ellipsis (substring str i j) ellipsis)
                             (let ((n (- n (string-width (string (car chars/reversed)))))
                                   (j (- j 1)))
                               (if (<= n max-width)
                                   (each ellipsis (substring str i j) ellipsis)
                                   (loop (cdr chars)
                                         (cdr chars/reversed)
                                         (+ i 1)
                                         j
                                         (- n (string-width (string (car chars)))))))))))
                   fmt*))))
    
    (define (trimmed/lazy width . fmt*)
      (fn ((%output output)
           (string-width string-width))
          (call-with-current-continuation
            (lambda (return)
              (let ((n 0))
                (with ((output (lambda (str)
                                 (let ((actual-width (string-width str))
                                       (max-width (- width n)))
                                   (when (> actual-width max-width)
                                     (return
                                       (%output
                                         (let loop ((chars (reverse (string->list str)))
                                                    (i (string-length str))
                                                    (n actual-width))
                                           (if (<= n max-width)
                                               (substring str 0 i)
                                               (loop (cdr chars)
                                                     (- i 1)
                                                     (- n (string-width (string (car chars))))))))))
                                   (set! n (+ n actual-width))
                                   (%output str)))))
                      (each-in-list fmt*)))))))
    
    (define (fitted width . fmt*)
      (padded width (apply trimmed width fmt*)))
    
    (define (fitted/right width . fmt*)
      (padded/right width (apply trimmed/right width fmt*)))
    
    (define (fitted/both width . fmt*)
      (padded/both width (apply trimmed/both width fmt*)))
  )
)
