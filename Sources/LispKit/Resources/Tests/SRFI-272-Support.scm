;;; SRFI 272 REGRESSION TEST SUITE: SUPPORT CODE
;;;
;;; This file is loaded by the SRFI 272 test suites. It provides procedure
;;; `read-labeled` which reads a datum from a string supporting datum labels
;;; (`#N=` and `#N#`), something the LispKit reader does not support. Shared
;;; structures are built bottom-up. Since pairs are immutable in LispKit,
;;; circular references can only be established via mutable objects like vectors
;;; and boxes. If a datum contains a cycle that goes through immutable pairs only,
;;; `read-labeled` returns `unrepresentable`. The SRFI 272 test suites skip tests
;;; based on such inputs and report the number of skipped tests. `read-labeled`
;;; also supports syntax `#&datum` for creating LispKit boxes.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may
;;; not use this file except in compliance with the License. You may obtain
;;; a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(import (lispkit base))

;; Marker returned by `read-labeled` for data that cannot be represented

(define unrepresentable (list 'unrepresentable))

;; Number of tests skipped because of unrepresentable input data

(define srfi-272-skipped-tests 0)

(define (srfi-272-skip!)
  (set! srfi-272-skipped-tests (+ srfi-272-skipped-tests 1)))

(define (srfi-272-report-skipped name)
  (if (> srfi-272-skipped-tests 0)
      (begin
        (display "[SKIP] ")
        (display srfi-272-skipped-tests)
        (display " ")
        (display name)
        (display " tests use circular lists, which cannot be represented in LispKit")
        (newline)))
  (set! srfi-272-skipped-tests 0))

;; Reads a datum from string `str`, supporting datum labels. The string is first
;; parsed into a syntax tree, which is then used to construct the datum. Vectors that
;; carry a datum label are allocated before their elements are constructed, such that
;; references to them can be resolved anywhere. Forward references to other labeled
;; data are resolved by patching vector slots once the labeled datum is complete.

(define (read-labeled str)
  (let ((p (open-input-string str))
        (nodes (make-eqv-hashtable))    ; label -> syntax tree
        (built (make-eqv-hashtable)))   ; label -> datum or placeholder
    (define (delimiter? c)
      (or (eof-object? c) (char-whitespace? c)
          (memv c '(#\( #\) #\[ #\] #\" #\; #\|))))
    (define (scan-token prefix)
      (let loop ((cs (reverse (string->list prefix))))
        (if (delimiter? (peek-char p))
            (list->string (reverse cs))
            (loop (cons (read-char p) cs)))))
    (define (atom str)
      (list 'atom (read (open-input-string str))))
    (define (skip-ws)
      (let ((c (peek-char p)))
        (cond ((eof-object? c))
              ((char-whitespace? c) (read-char p) (skip-ws))
              ((char=? c #\;) (read-line p) (skip-ws)))))
    (define (read-delimited close)
      ; reads the rest of a string/symbol literal including the closing char
      (let loop ((cs (list close)))
        (let ((c (read-char p)))
          (cond ((char=? c #\\) (loop (cons (read-char p) (cons c cs))))
                ((char=? c close) (list->string (reverse (cons c cs))))
                (else (loop (cons c cs)))))))
    (define (parse-seq close)
      ; returns a list of syntax trees and the syntax tree of the tail
      (skip-ws)
      (let ((c (peek-char p)))
        (cond ((eqv? c close)
                (read-char p)
                (values '() '(atom ())))
              ((and (eqv? c #\.)
                    (begin (read-char p) (delimiter? (peek-char p))))
                (let ((tail (parse)))
                  (skip-ws)
                  (read-char p)
                  (values '() tail)))
              (else
                (let ((x (if (eqv? c #\.) (atom (scan-token ".")) (parse))))
                  (let-values (((xs tail) (parse-seq close)))
                    (values (cons x xs) tail)))))))
    (define (parse-list close)
      (let-values (((xs tail) (parse-seq close)))
        (list 'list xs tail)))
    (define (parse-vector)
      (let-values (((xs tail) (parse-seq #\))))
        (list 'vector xs)))
    (define (parse)
      (skip-ws)
      (let ((c (read-char p)))
        (case c
          ((#\() (parse-list #\)))
          ((#\[) (parse-list #\]))
          ((#\') (list 'list (list '(atom quote) (parse)) '(atom ())))
          ((#\`) (list 'list (list '(atom quasiquote) (parse)) '(atom ())))
          ((#\,) (if (eqv? (peek-char p) #\@)
                     (begin
                       (read-char p)
                       (list 'list (list '(atom unquote-splicing) (parse)) '(atom ())))
                     (list 'list (list '(atom unquote) (parse)) '(atom ()))))
          ((#\") (atom (read-delimited #\")))
          ((#\|) (atom (read-delimited #\|)))
          ((#\#)
            (let ((c (peek-char p)))
              (cond ((char=? c #\() (read-char p) (parse-vector))
                    ((char=? c #\&) (read-char p) (list 'box (parse)))
                    ((char=? c #\u)
                      (read-char p) (read-char p) (read-char p)
                      (let-values (((xs tail) (parse-seq #\))))
                        (list 'atom (apply bytevector (map cadr xs)))))
                    ((char=? c #\\)
                      (read-char p)
                      (let ((c (read-char p)))
                        (atom (scan-token (string #\# #\\ c)))))
                    ((char-numeric? c) (parse-label 0))
                    (else (atom (scan-token "#"))))))
          (else (atom (scan-token (string c)))))))
    (define (parse-label n)
      (let ((c (read-char p)))
        (cond ((char-numeric? c)
                (parse-label (+ (* n 10) (- (char->integer c) (char->integer #\0)))))
              ((char=? c #\=)
                (let ((node (parse)))
                  (hashtable-set! nodes n node)
                  (list 'label n node)))
              ((char=? c #\#)
                (list 'ref n))
              (else (error "invalid datum label" n c)))))
    (define (build node)
      (case (car node)
        ((atom) (cadr node))
        ((ref) (hashtable-ref built (cadr node) #f))
        ((list) (let* ((xs (build-all (cadr node)))
                       (tail (build (caddr node))))
                  (append xs tail)))
        ((vector) (list->vector (build-all (cadr node))))
        ((box) (box (build (cadr node))))
        ((label)
          (let ((n (cadr node))
                (node (caddr node)))
            (case (car node)
              ((vector)
                (let ((v (make-vector (length (cadr node)))))
                  (hashtable-set! built n v)
                  (do ((i 0 (+ i 1))
                       (xs (cadr node) (cdr xs)))
                      ((null? xs) v)
                    (vector-set! v i (build (car xs))))))
              ((box)
                (let ((b (box #f)))
                  (hashtable-set! built n b)
                  (set-box! b (build (cadr node)))
                  b))
              (else
                (let ((ph (list 'placeholder n)))
                  (hashtable-set! built n ph)
                  (let ((x (build node)))
                    (hashtable-set! built n x)
                    (patch! x ph x)
                    x))))))
        (else (error "unknown node" node))))
    (define (build-all nodes)
      ; builds the given syntax trees strictly from left to right
      (let loop ((nodes nodes) (acc '()))
        (if (null? nodes)
            (reverse acc)
            (let ((x (build (car nodes))))
              (loop (cdr nodes) (cons x acc))))))
    (define (patch! x ph obj)
      ; replaces placeholder `ph` with `obj` within `x`; fails if `ph` is stored
      ; in an immutable pair
      (let ((visited (make-eq-hashtable)))
        (let walk ((x x))
          (if (not (hashtable-contains? visited x))
              (begin
                (hashtable-set! visited x #t)
                (cond ((eq? x ph)
                        (raise unrepresentable))
                      ((pair? x)
                        (walk (car x))
                        (walk (cdr x)))
                      ((box? x)
                        (if (eq? (unbox x) ph)
                            (set-box! x obj)
                            (walk (unbox x))))
                      ((vector? x)
                        (do ((i 0 (+ i 1)))
                            ((= i (vector-length x)))
                          (if (eq? (vector-ref x i) ph)
                              (vector-set! x i obj)
                              (walk (vector-ref x i)))))))))))
    (guard (e ((eq? e unrepresentable) unrepresentable))
      (build (parse)))))

;; Helper procedures used by the test suites

(define tmp-dir (car (system-directory 'temporary)))

(define (pp->string obj . args)
  (let ((p (open-output-string)))
    (apply pp obj p args)
    (get-output-string p)))

(define (capture thunk)
  (let ((p (open-output-string)))
    (parameterize ((current-output-port p)) (thunk))
    (get-output-string p)))

(define (temp-path name)
  (string-append tmp-dir name))

(define (write-file path content)
  (call-with-output-file path (lambda (port) (write-string content port))))

(define (read-file path)
  (call-with-input-file path
    (lambda (port)
      (let loop ((acc '()))
        (let ((c (read-char port)))
          (if (eof-object? c) (list->string (reverse acc)) (loop (cons c acc))))))))

(define (generator->list gen)
  (let loop ((acc '()))
    (let ((x (gen)))
      (if (eof-object? x) (reverse acc) (loop (cons x acc))))))

;; Sample code used by the test suites (from the SRFI 272 sample implementation)

(define sexp1
  '(define
    (ast-pretty a . opt-port)
    (define
     (width x)
     (cond
      ((string? x) (+ (string-length x) 2))
      ((char? x) 3)
      ((number? x) 5)
      ((symbol? x) (string-length (symbol->string x)))
      ((boolean? x) 2)
      ((null? x) 2)
      (else 10)))
    (define
     (ast-width a)
     (cond
      ((ast-atom? a) (width (ast-atom->val a)))
      ((ast-null? a) 2)
      ((ast-pair? a)
       (if
        (ast-list? a)
        (let
         loop
         ((a a) (sum 0) (count 0))
         (if
          (ast-null? a)
          (+ sum count 1)
          (loop (ast-cdr a) (+ sum (ast-width (ast-car a))) (+ count 1))))
        (+ 1 (ast-width (ast-car a)) 3 (ast-width (ast-cdr a)) 1)))
      (else 0)))
    (define
     (ast-width-hello a)
     (display "hello!\n")
     (cond
      ((ast-atom? a) (width (ast-atom->val a)))
      ((ast-null? a) 2)
      ((ast-pair? a)
       (if
        (ast-list? a)
        (let
         loop
         ((a a) (sum 0) (count 0))
         (if
          (ast-null? a)
          (+ sum count 1)
          (loop (ast-cdr a) (+ sum (ast-width (ast-car a))) (+ count 1))))
        (+ 1 (ast-width (ast-car a)) 3 (ast-width (ast-cdr a)) 1)))
      (else
       (cond
        ((foo) => (lambda (v) (bar v 0)))
        (else (begin (newline) (if a #t #f)))))))
    (define (break? cur-col width max-col) (> (+ cur-col width) max-col))
    (define
     (print-indent n port)
     (do ((i 0 (+ i 1))) ((= i n)) (display "  " port)))
    (define
     (pretty a col indent port max-col)
     (cond
      ((ast-atom? a)
       (let
        ((w (width (ast-atom->val a))))
        (write (ast-atom->val a) port)
        (+ col w)))
      ((ast-null? a) (display "()" port) (+ col 2))
      ((ast-pair? a)
       (if
        (ast-list? a)
        (let*
         ((total-width (ast-width a))
          (remaining (- max-col col))
          (inline? (<= total-width remaining)))
         (if
          inline?
          (begin
           (display "(" port)
           (let
            loop
            ((first? #t) (rest a) (cur-col (+ col 1)))
            (if
             (ast-null? rest)
             (begin (display ")" port) (+ cur-col 1))
             (let*
              ((elem-width (ast-width (ast-car rest)))
               (new-col
                (if
                 first?
                 (pretty (ast-car rest) cur-col indent port max-col)
                 (begin
                  (display " " port)
                  (pretty (ast-car rest) (+ cur-col 1) indent port max-col)))))
              (loop #f (ast-cdr rest) new-col)))))
          (begin
           (display "(" port)
           (let
            ((new-indent (+ indent 1)))
            (let
             loop
             ((rest a) (cur-col (* new-indent 2)))
             (if
              (ast-null? rest)
              (begin (display ")" port) (+ cur-col 1))
              (let*
               ((elem (ast-car rest))
                (next-col (pretty elem cur-col new-indent port max-col)))
               (unless
                (ast-null? (ast-cdr rest))
                (newline port)
                (print-indent new-indent port))
               (loop (ast-cdr rest) (* new-indent 2)))))))))
        (begin
         (display "(" port)
         (let
          ((car-col (pretty (ast-car a) (+ col 1) indent port max-col)))
          (display " . " port)
          (let
           ((cdr-col (pretty (ast-cdr a) (+ car-col 3) indent port max-col)))
           (display ")" port)
           (+ cdr-col 1))))))))
    (let
     ((port (if (null? opt-port) (current-output-port) (car opt-port))))
     (pretty a 0 0 port 80)
     (newline port))))

(define sexp2
  '(define
    (%read port simple? ci?)
    (define-syntax
     r-error
     (syntax-rules () ((_ p msg a ...) (read-error p msg a ...))))
    (define fold-case? (or ci? (port-fold-case? port)))
    (define shared '())
    (define (make-shared-ref loc) (lambda () (unbox loc)))
    (define (shared-ref? form) (procedure? form))
    (define
     (patch-ref! form)
     (if (procedure? form) (patch-ref! (form)) form))
    (define
     (patch-shared! form)
     (cond
      ((pair? form)
       (if
        (procedure? (car form))
        (set-car! form (patch-ref! (car form)))
        (patch-shared! (car form)))
       (if
        (procedure? (cdr form))
        (set-cdr! form (patch-ref! (cdr form)))
        (patch-shared! (cdr form))))
      ((vector? form)
       (let
        loop
        ((i 0))
        (when
         (fx<? i (vector-length form))
         (let
          ((fi (vector-ref form i)))
          (if
           (procedure? fi)
           (vector-set! form i (patch-ref! fi))
           (patch-shared! fi)))
         (loop (fx+ i 1)))))
      ((box? form)
       (if
        (procedure? (unbox form))
        (set-box! form (patch-shared! (unbox form)))
        (patch-shared! (unbox form))))))
    (define (patch-shared form) (patch-shared! form) form)

    (define reader-token-marker #f)
    (define close-paren #f)
    (define close-bracket #f)
    (define dot #f)
    (define
     ()
     (let
      ((rtm (list 'reader-token)))
      (set! reader-token-marker rtm)
      (set! close-paren (cons rtm "right parenthesis"))
      (set! close-bracket (cons rtm "right bracket"))
      (set! dot (cons rtm "\" . \""))))

    (define
     (reader-token? form)
     (and (pair? form) (eq? (car form) reader-token-marker)))

    (define
     (char-hex-digit? c)
     (let
      ((scalar-value (char->integer c)))
      (or
       (and (>= scalar-value 48) (<= scalar-value 57))
       (and (>= scalar-value 65) (<= scalar-value 70))
       (and (>= scalar-value 97) (<= scalar-value 102)))))

    (define
     (char-delimiter? c)
     (or
      (char-whitespace? c)
      (char=? c #\))
      (char=? c #\()
      (char=? c #\])
      (char=? c #\[)
      (char=? c #\")
      (char=? c #\|)
      (char=? c #\;)))

    (define
     (sub-read-carefully p)
     (let
      ((form (sub-read p)))
      (cond
       ((eof-object? form) (r-error p "unexpected end of file"))
       ((reader-token? form) (r-error p "unexpected token:" (cdr form)))
       (else form))))

    (define
     (sub-read-shebang p)
     (if
      (eqv? (peek-char p) #\space)
      (string->symbol (string-trim-whitespace (read-line p)))
      (sub-read-carefully p)))

    (define
     (sub-read p)
     ; bumped code using %read-ahead builtin/instruction
     (let
      ((c (%read-ahead fold-case? p)))
      (cond
       ((not (char? c)) c) ; handled by %read-ahead
       ((char=? c #\() (sub-read-list c p close-paren #t))
       ((char=? c #\)) close-paren)
       ((char=? c #\[) (sub-read-list c p close-bracket #t))
       ((char=? c #\]) close-bracket)
       ((char=? c #\.) dot)
       ((char=? c #\') (list 'quote (sub-read-carefully p)))
       ((char=? c #\`) (list 'quasiquote (sub-read-carefully p)))
       ((char=? c #\n)
        (r-error p "unsupported number syntax (implementation restriction)"))
       ((char=? c #\d) (r-error p "invalid delimiter"))
       ((char=? c #\y) (r-error p "unsupported symbol syntax"))
       ((char=? c #\z) (r-error p "invalid token"))
       ((char=? c #\,)
        (let
         ((next (peek-char p)))
         (cond
          ((eof-object? next) (r-error p "end of file after ,"))
          ((char=? next #\@)
           (read-char p)
           (list 'unquote-splicing (sub-read-carefully p)))
          (else (list 'unquote (sub-read-carefully p))))))
       ((char=? c #\")
        (let
         loop
         ((l '()))
         (let
          ((c (read-char p)))
          (cond
           ((eof-object? c) (r-error p "end of file within a string"))
           ((char=? c #\\)
            (let
             ((e (sub-read-strsym-char-escape p 'string)))
             (loop (if e (cons e l) l))))
           ((char=? c #\") (list->string (reverse! l)))
           (else (loop (cons c l)))))))
       ((char=? c #\|)
        (let
         loop
         ((l '()))
         (let
          ((c (read-char p)))
          (cond
           ((eof-object? c) (r-error p "end of file within a |symbol|"))
           ((char=? c #\\)
            (let
             ((e (sub-read-strsym-char-escape p 'symbol)))
             (loop (if e (cons e l) l))))
           ((char=? c #\|) (string->symbol (list->string (reverse! l))))
           (else (loop (cons c l)))))))
       ((char=? c #\#)
        (let
         ((c (peek-char p)))
         (cond
          ((eof-object? c) (r-error p "end of file after #"))
          ((char=? c #\!)
           (read-char p)
           (let
            ((name (sub-read-shebang p)))
            (case
             name
             ((fold-case no-fold-case)
              (set! fold-case? (eq? name 'fold-case))
              (set-port-fold-case! p fold-case?)
              (sub-read p))
             (else
              (if
               (symbol? name)
               (symbol->shebang name)
               (r-error p "unexpected name after #!" name))))))
          ((or
            (char-ci=? c #\t)
            (char-ci=? c #\f)
            (char-ci=? c #\s)
            (char-ci=? c #\u))
           (let
            ((name (sub-read-carefully p)))
            (case
             name
             ((t true) #t)
             ((f false) #f)
             ((u8) (list->numvector (sub-read-numerical-list p name) 0))
             ((s8) (list->numvector (sub-read-numerical-list p name) 1))
             ((u16) (list->numvector (sub-read-numerical-list p name) 2))
             ((s16) (list->numvector (sub-read-numerical-list p name) 3))
             ((f32) (list->numvector (sub-read-numerical-list p name) 10))
             ((f64) (list->numvector (sub-read-numerical-list p name) 11))
             (else (r-error p "unexpected name after #" name)))))
          ((char=? c #\&) (read-char p) (box (sub-read-carefully p)))
          ((char=? c #\;) (read-char p) (sub-read-carefully p) (sub-read p))
          ((char=? c #\|)
           (read-char p)
           (let
            recur
            ()
            (let
             ((next (read-char p)))
             (cond
              ((eof-object? next) (r-error p "end of file in #| comment"))
              ((char=? next #\|)
               (let
                ((next (peek-char p)))
                (cond
                 ((eof-object? next) (r-error p "end of file in #| comment"))
                 ((char=? next #\#) (read-char p))
                 (else (recur)))))
              ((char=? next #\#)
               (let
                ((next (peek-char p)))
                (cond
                 ((eof-object? next) (r-error p "end of file in #| comment"))
                 ((char=? next #\|) (read-char p) (recur) (recur))
                 (else (recur)))))
              (else (recur)))))
           (sub-read p))
          ((char=? c #\() ;)
           (read-char p)
           (list->vector (sub-read-list c p close-paren #f)))
          ((char=? c #\\)
           (read-char p)
           (let
            ((c (peek-char p)))
            (cond
             ((eof-object? c) (r-error p "end of file after #\\"))
             ((char=? #\x c)
              (read-char p)
              (if (char-delimiter? (peek-char p)) c (sub-read-x-char-escape p #f)))
             ((char-alphabetic? c)
              (let
               ((name (sub-read-carefully p)))
               (if
                (= (string-length (symbol->string name)) 1)
                c
                (case
                 name
                 ((null) (integer->char 0))
                 ((space) #\space)
                 ;[(alarm) #\alarm]
                 ((backspace) #\backspace)
                 ((tab) #\tab)
                 ((newline linefeed) #\newline)
                 ((return) #\return)
                 (else (r-error p "unknown #\\ name" name))))))
             (else (read-char p) c))))
          ((char-numeric? c)
           (when
            simple?
            (r-error p "#N=/#N# notation is not allowed in this mode"))
           (let
            loop
            ((l '()))
            (let
             ((c (read-char p)))
             (cond
              ((eof-object? c) (r-error p "end of file within a #N notation"))
              ((char-numeric? c) (loop (cons c l)))
              ((char=? c #\#)
               (let*
                ((s (list->string (reverse! l))) (n (string->number s)))
                (cond
                 ((and (fixnum? n) (assq n shared)) => cdr)
                 (else (r-error p "unknown #n# reference:" s)))))
              ((char=? c #\=)
               (let*
                ((s (list->string (reverse! l))) (n (string->number s)))
                (cond
                 ((not (fixnum? n)) (r-error p "invalid #n= reference:" s))
                 ((assq n shared) (r-error p "duplicate #n= tag:" n)))
                (let
                 ((loc (box #f)))
                 (set! shared (cons (cons n (make-shared-ref loc)) shared))
                 (let
                  ((form (sub-read-carefully p)))
                  (cond
                   ((shared-ref? form) (r-error p "#n= has another label as target" s))
                   (else (set-box! loc form) form))))))
              (else (r-error p "invalid terminator for #N notation"))))))
          (else (r-error p "unknown # syntax" c)))))
       (else (r-error p "illegal character read" c)))))

    (define
     (sub-read-list c p close-token dot?)
     (let
      ((form (sub-read p)))
      (if
       (eq? form dot)
       (r-error p "missing car -- ( immediately followed by .") ;)
       (let
        recur
        ((form form))
        (cond
         ((eof-object? form)
          (r-error p "eof inside list -- unbalanced parentheses"))
         ((eq? form close-token) '())
         ((eq? form dot)
          (if
           dot?
           (let*
            ((last-form (sub-read-carefully p)) (another-form (sub-read p)))
            (if
             (eq? another-form close-token)
             last-form
             (r-error p "randomness after form after dot" another-form)))
           (r-error p "dot in #(...)")))
         ((reader-token? form) (r-error p "error inside list --" (cdr form)))
         (else (cons form (recur (sub-read p)))))))))

    (define
     (sub-read-numerical-list p ts)
     (unless
      (eq? (read-char p) #\()
      (r-error p (format "invalid ~avector syntax" ts)))
     (let
      recur
      ((form (sub-read p)))
      (cond
       ((eof-object? form) (r-error p (format "eof inside ~avector" ts)))
       ((eq? form close-paren) '())
       ((reader-token? form)
        (r-error p (format "error inside ~avector --" ts) (cdr form)))
       ((or
         (and (eq? ts 'u8) (fixnum? form) (fx<=? 0 form 255))
         (and (eq? ts 's8) (fixnum? form) (fx<=? -128 form 127))
         (and (eq? ts 'u16) (fixnum? form) (fx<=? 0 form 65535))
         (and (eq? ts 's16) (fixnum? form) (fx<=? -32768 form 32767))
         (and (eq? ts 'f32) (flonum? form))
         (and (eq? ts 'f64) (flonum? form)))
        (cons form (recur (sub-read p))))
       (else (r-error p (format "invalid ~a inside ~avector --" ts ts) form)))))

    (define
     (sub-read-strsym-char-escape p what)
     (let
      ((c (read-char p)))
      (if (eof-object? c) (r-error p "end of file within a" what))
      (cond
       ((or (char=? c #\\) (char=? c #\") (char=? c #\|)) c)
       ((char=? c #\a) #\alarm)
       ((char=? c #\b) #\backspace)
       ((char=? c #\t) #\tab)
       ((char=? c #\n) #\newline)
       ((char=? c #\r) #\return)
       ((char=? c #\x) (sub-read-x-char-escape p #t))
       ((and (eq? what 'string) (char-whitespace? c))
        (let
         loop
         ((gotnl (char=? c #\newline)) (nc (peek-char p)))
         (cond
          ((or (eof-object? nc) (not (char-whitespace? nc)))
           (if gotnl #f (r-error p "no newline in line ending escape")))
          ((and gotnl (char=? nc #\newline)) #f)
          (else
           (read-char p)
           (loop (or gotnl (char=? nc #\newline)) (peek-char p))))))
       (else (r-error p "invalid char escape in" what ': c)))))

    (define
     (sub-read-x-char-escape p in-string?)
     (define
      (rev-digits->char l)
      (if
       (null? l)
       (r-error p "\\x escape sequence is too short")
       (integer->char (string->fixnum (list->string (reverse! l)) 16))))
     (let
      loop
      ((c (peek-char p)) (l '()) (cc 0))
      (cond
       ((eof-object? c)
        (if
         in-string?
         (r-error p "end of file within a string")
         (rev-digits->char l)))
       ((and in-string? (char=? c #\;)) (read-char p) (rev-digits->char l))
       ((and (not in-string?) (char-delimiter? c)) (rev-digits->char l))
       ((not (char-hex-digit? c))
        (r-error p "unexpected char in \\x escape sequence" c))
       ((> cc 2) (r-error p "\\x escape sequence is too long"))
       (else (read-char p) (loop (peek-char p) (cons c l) (+ cc 1))))))

    ; body of %read
    (let
     ((form (sub-read port)))
     (if
      (not (reader-token? form))
      (if (null? shared) form (patch-shared form))
      (r-error port "unexpected token:" (cdr form))))))

(define sexp3
  '(cond
    ((equal? (token-value tok) "remove")
     (continue
      nc
      ti
      (lambda
       (tree)
       (unless
        (moore-lookup-node tree s)
        (p-error ti "notation for ~s does not exist" s))
       (moore-remove-handler tree s))))
    ((equal? (token-value tok) "additive")
     (let-parse-result
      (((hd nc ti) (parse-null nc (ti-pop ti 'lpar) rbp)))
      (define op (string->symbol s))
      (define hd (operand-token->expression tk))
      (define-values (a . b) `(,a ,@b))
      (define-values
       (ti comb)
       (if
        (eq? (token-type (ti-car ti)) 'comma)
        (values (ti-pop (ti-cdr (ti-cdr ti)) 'rpar) list)
        (values (ti-pop ti 'rpar) nary)))
      (continue
       (notation-conf-add-led nc op infix-led bp:add bp:add hd comb)
       ti
       (add-auto-handler s))))
    (else
     (p-error ti "unknown modifier for notation: ~a" (token-value tok)))))

(define sexp4
  '(define-syntax
    set*!
    (lambda
     (x)
     (letrec
      ((unwrap-exp
        (lambda
         (x)
         (let
          ((x (unwrap-syntax x)))
          (if (pair? x) (cons (car x) (unwrap-exp (cdr x))) x)))))
      (let
       ((sets (map unwrap-exp (cdr (unwrap-exp x)))))
       (let
        ((ids (map car sets))
         (vals (map cadr sets))
         (temps (map (lambda (x) (generate-identifier)) sets)))
        `(,'let
          ,(map list temps vals)
          ,@(map (lambda (id temp) `(,'set! ,id ,temp)) ids temps)
          #f)))))))

(define sexp5
  '(define
    (normalize-definition t syntax-definition?)
    (cond
     ((pair? (cdr t))
      (let
       ((_ (car t)) (head (cadr t)) (body (cddr t)))
       (cond
        ((and (identifier? head) (pair? body) (null? (cdr body)))
         `(,_ ,head unquote body))
        ((and (pair? head) (identifier? (car head)) (formals? (cdr head)))
         (let
          ((r (make-primitive-renaming-procedure)))
          (if
           syntax-definition?
           `(,_
             ,(car head)
             ,(let
               ((transformer (r (symbolic-name (car head)))))
               `((,(r 'lambda)
                  (,transformer)
                  (,(r 'lambda)
                   (,(r 'form))
                   (,(r 'apply) ,transformer ,(r 'form))))
                 (,(r 'lambda) (,(r 'dummy) unquote (cdr head)) unquote body))))
           `(,_ ,(car head) (,(r 'lambda) ,(cdr head) unquote body)))))
        (else (syntax-error "Syntax error in definition:" t)))))
     (else (syntax-error "Syntax error in definition:" t)))))
