;;; LISPKIT PRETTIFY
;;; 
;;; Functions for pretty-printing s-expressions. This library is based on "genwrite.scm"
;;; by Marc Feeley and uses some extensions from SLIB written by Aubrey Jaffer. The
;;; signatures of the various exported functions are compatible to the original signatures.
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2017 Matthias Zenger. All rights reserved.
;;; 
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
;;; except in compliance with the License. You may obtain a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed under the
;;; License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
;;; either express or implied. See the License for the specific language governing permissions
;;; and limitations under the License.
;;; 
;;; The core of the implementation is based on code from "genwrite.scm":
;;;   Copyright © 1991, Marc Feeley
;;;   Author: Marc Feeley (feeley@iro.umontreal.ca)
;;;   Distribution restrictions: none
;;; 
;;; Pretty-print->string and pretty-print-file are based on SLIB: "pp.scm", "ppfile.scm"
;;;   Copyright © 1993, 1994 Aubrey Jaffer
;;;   
;;;   Permission to copy this software, to modify it, to redistribute it,
;;;   to distribute modified versions, and to use it for any purpose is
;;;   granted, subject to the following restrictions and understandings.
;;; 
;;;   1. Any copy made of this software must include this copyright notice in full.
;;; 
;;;   2. I have made no warranty or representation that the operation of this software will
;;;      be error-free, and I am under no obligation to provide any services, by way of
;;;      maintenance, update, or otherwise.
;;; 
;;;   3. In conjunction with products arising from the use of this material, there shall be
;;;      no use of my name in any advertising, promotional, or sales literature without prior
;;;      written consent in each case.

(define-library (lispkit prettify)
  
  (export pretty-print
          pretty-print->string
          pretty-print-file
          generic-write
          *pp-width*
          *pp-indent*
          *pp-max-call-head-width*
          *max-expr-width*)
  
  (import (scheme base))

  (begin
    
    (define *pp-width* (make-parameter 80))
    
    (define *pp-indent* (make-parameter 2))

    (define *pp-max-call-head-width* (make-parameter 5))
    
    (define *max-expr-width* (make-parameter 50))
    
    (define pretty-print
      (case-lambda
        ((obj)
          (generic-write obj #f (*pp-width*) (lambda (s) (display s) #t)))
        ((obj port)
          (generic-write obj #f (*pp-width*) (lambda (s) (display s port) #t)))
        ((obj port width)
          (generic-write obj #f width (lambda (s) (display s port) #t)))))
    
    (define pretty-print->string
      (case-lambda
        ((obj)
          (let ((result '()))
            (generic-write obj #f (*pp-width*) (lambda (str) (set! result (cons str result)) #t))
            (reverse-string-append result)))
        ((obj width)
          (let ((result '()))
            (generic-write obj #f width (lambda (str) (set! result (cons str result)) #t))
            (reverse-string-append result)))))
    
    (define pretty-print-file
      (case-lambda
        ((ifile)
          (pprint-filter-file ifile identity (current-output-port)))
        ((ifile port)
          (pprint-filter-file ifile identity port))))
    
    (define (pprint-filter-file inport filter . optarg)
      ((lambda (fun)
         (if (input-port? inport)
             (fun inport)
             (call-with-input-file inport fun)))
       (lambda (port)
         ((lambda (fun)
            (let ((outport (if (null? optarg) (current-output-port) (car optarg))))
              (if (output-port? outport)
                  (fun outport)
                  (call-with-output-file outport fun))))
           (lambda (oport)
             (letrec ((lp (lambda (c)
                            (cond ((eof-object? c))
                                  ((char-whitespace? c) (display (read-char port) oport)
                                                        (lp (peek-char port)))
                                  ((char=? #\; c)       (cmt c))
                                  (else                 (sx)))))
                      (cmt (lambda (c)
                             (cond ((eof-object? c))
                                   ((char=? #\newline c) (display (read-char port) oport)
                                                         (lp (peek-char port)))
                                   (else                 (display (read-char port) oport)
                                                         (cmt (peek-char port))))))
                      (sx (lambda ()
                            (let ((o (read port)))
                              (cond ((eof-object? o))
                                    (else (pretty-print (filter o) oport)
                                          ;; pretty-print seems to have extra newline
                                          (let ((c (peek-char port)))
                                            (cond ((eqv? #\newline c)
                                                    (read-char port)
                                                    (set! c (peek-char port))))
                                            (lp c))))))))
                (lp (peek-char port))))))))
    
    (define (generic-write obj display? width output)

      (define (read-macro? l)
        (let ((head (car l))
              (tail (cdr l)))
          (case head
            ((quote quasiquote unquote unquote-splicing) (and (pair? tail) (null? (cdr tail))))
            (else                                        #f))))

      (define read-macro-body cadr)
      
      (define (read-macro-prefix l)
        (case (car l)
          ((quote)            "'")
          ((quasiquote)       "`")
          ((unquote)          ",")
          ((unquote-splicing) ",@")))

      (define (out str col)
        (and col (output str) (+ col (string-length str))))

      (define (wr obj col)
        (cond ((pair? obj)        (wr-expr obj col))
              ((null? obj)        (wr-lst obj col))
              ((vector? obj)      (wr-lst (vector->list obj) (out "#" col)))
              ((boolean? obj)     (out (if obj "#t" "#f") col))
              ((number? obj)      (out (number->string obj) col))
              ((symbol? obj)      (out (symbol->string obj) col))
              ((procedure? obj)   (out "#[procedure]" col))
              ((string? obj)      (if display?
                                      (out obj col)
                                      (let loop ((i 0) (j 0) (col (out "\"" col)))
                                        (if (and col (< j (string-length obj)))
                                          (let ((c (string-ref obj j)))
                                            (if (or (char=? c #\\)
                                                    (char=? c #\"))
                                              (loop j
                                                    (+ j 1)
                                                    (out "\\"
                                                         (out (string-copy obj i j)
                                                              col)))
                                              (loop i (+ j 1) col)))
                                          (out "\""
                                               (out (string-copy obj i j) col))))))
              ((char? obj)        (if display?
                                      (out (make-string 1 obj) col)
                                      (out (case obj
                                             ((#\space)   "space")
                                             ((#\newline) "newline")
                                             (else        (make-string 1 obj)))
                                           (out "#\\" col))))
              ((input-port? obj)  (out "#[input-port]" col))
              ((output-port? obj) (out "#[output-port]" col))
              ((eof-object? obj)  (out "#[eof-object]" col))
              (else               (out "#[unknown]" col))))

      (define (wr-expr expr col)
        (if (read-macro? expr)
            (wr (read-macro-body expr) (out (read-macro-prefix expr) col))
            (wr-lst expr col)))

      (define (wr-lst l col)
        (if (pair? l)
            (let loop ((l (cdr l))
                       (col (and col (wr (car l) (out "(" col)))))
              (cond ((not col)  col)
                    ((pair? l)  (loop (cdr l) (wr (car l) (out " " col))))
                    ((null? l)  (out ")" col))
                    (else       (out ")" (wr l (out " . " col))))))
            (out "()" col)))
      
      (define (pp obj col)
        (pr obj col 0 pp-expr))
      
      (define (spaces n col)
        (if (> n 0)
            (if (> n 7)
                (spaces (- n 8) (out "        " col))
                (out (string-copy "        " 0 n) col))
            col))

      (define (indent to col)
        (and col
             (if (< to col)
               (and (out newline-str col) (spaces to 0))
               (spaces (- to col) col))))

      (define (pr obj col extra pp-pair)
        (if (or (pair? obj) (vector? obj)) ; may have to split on multiple lines
          (let ((result '())
                (left (min (+ (- (- width col) extra) 1) (*max-expr-width*))))
            (generic-write obj display? #f
                           (lambda (str)
                             (set! result (cons str result))
                             (set! left (- left (string-length str)))
                             (> left 0)))
            (if (> left 0)	      ; all can be printed on one line
              (out (reverse-string-append result) col)
              (if (pair? obj)
                  (pp-pair obj col extra)
                  (pp-list (vector->list obj) (out "#" col) extra pp-expr))))
          (wr obj col)))

      (define (pp-expr expr col extra)
        (if (read-macro? expr)
          (pr (read-macro-body expr)
              (out (read-macro-prefix expr) col)
              extra
              pp-expr)
          (let ((head (car expr)))
            (if (symbol? head)
              (let ((proc (style head)))
                (if proc
                  (proc expr col extra)
                  (if (> (string-length (symbol->string head))
                         (*pp-max-call-head-width*))
                    (pp-general expr col extra #f #f #f pp-expr)
                    (pp-call expr col extra pp-expr))))
              (pp-list expr col extra pp-expr)))))

      ; (head item1 item2 item3)
      (define (pp-call expr col extra pp-item)
        (let ((col* (wr (car expr) (out "(" col))))
          (and col
               (pp-down (cdr expr) col* (+ col* 1) extra pp-item))))

      ; (item1 item2 item3)
      (define (pp-list l col extra pp-item)
        (let ((col (out "(" col)))
          (pp-down l col col extra pp-item)))

      (define (pp-down l col1 col2 extra pp-item)
        (let loop ((l l) (col col1))
          (and col
               (cond ((pair? l)
                       (let ((rest (cdr l)))
                         (let ((extra (if (null? rest) (+ extra 1) 0)))
                           (loop rest
                                 (pr (car l) (indent col2 col) extra pp-item)))))
                     ((null? l)
                       (out ")" col))
                     (else
                       (out ")"
                            (pr l
                                (indent col2 (out "." (indent col2 col)))
                                (+ extra 1)
                                pp-item)))))))

      (define (pp-general expr col extra named? pp-1 pp-2 pp-3)
        (let* ((head (car expr))
               (rest (cdr expr))
               (col* (wr head (out "(" col))))
          (if (and named? (pair? rest))
              (let* ((name  (car rest))
                     (rest  (cdr rest))
                     (col** (wr name (out " " col*))))
                (tail1 pp-1 pp-2 pp-3 extra rest (+ col (*pp-indent*)) col** (+ col** 1)))
              (tail1 pp-1 pp-2 pp-3 extra rest (+ col (*pp-indent*)) col* (+ col* 1)))))

      (define (pp-expr-list l col extra)
        (pp-list l col extra pp-expr))

      (define (pp-LAMBDA expr col extra)
        (pp-general expr col extra #f pp-expr-list #f pp-expr))

      (define (pp-IF expr col extra)
        (pp-general expr col extra #f pp-expr #f pp-expr))

      (define (pp-COND expr col extra)
        (pp-call expr col extra pp-expr-list))

      (define (pp-CASE expr col extra)
        (pp-general expr col extra #f pp-expr #f pp-expr-list))

      (define (pp-AND expr col extra)
        (pp-call expr col extra pp-expr))

      (define (pp-LET expr col extra)
        (let* ((rest (cdr expr))
               (named? (and (pair? rest) (symbol? (car rest)))))
          (pp-general expr col extra named? pp-expr-list #f pp-expr)))

      (define (pp-BEGIN expr col extra)
        (pp-general expr col extra #f #f #f pp-expr))

      (define (pp-DO expr col extra)
        (pp-general expr col extra #f pp-expr-list pp-expr-list pp-expr))

      (define (tail1 pp-1 pp-2 pp-3 extra rest col1 col2 col3)
        (if (and pp-1 (pair? rest))
            (let* ((val1  (car rest))
                   (rest  (cdr rest))
                   (extra (if (null? rest) (+ extra 1) 0)))
              (tail2 pp-2 pp-3 extra rest col1 (pr val1 (indent col3 col2) extra pp-1) col3))
            (tail2 pp-2 pp-3 extra rest col1 col2 col3)))
  
      (define (tail2 pp-2 pp-3 extra rest col1 col2 col3)
        (if (and pp-2 (pair? rest))
            (let* ((val1  (car rest))
                   (rest  (cdr rest))
                   (extra (if (null? rest) (+ extra 1) 0)))
              (tail3 pp-3 extra rest col1 (pr val1 (indent col3 col2) extra pp-2)))
            (tail3 pp-3 extra rest col1 col2)))

      (define (tail3 pp-3 extra rest col1 col2)
        (pp-down rest col2 col1 extra pp-3))

      ;;; define formatting style (change these to suit your style)
      
      (define (style head)
        (case head
          ((lambda let* letrec define) pp-LAMBDA)
          ((if set!)                   pp-IF)
          ((cond)                      pp-COND)
          ((case)                      pp-CASE)
          ((and or)                    pp-AND)
          ((let)                       pp-LET)
          ((begin)                     pp-BEGIN)
          ((do)                        pp-DO)
          (else                        #f)))

      (if width
          (out newline-str (pp obj 0))
          (wr obj 0)))
    
    (define (reverse-string-append l)
      (define (rev-string-append l i)
        (if (pair? l)
          (let* ((str (car l))
                 (len (string-length str))
                 (result (rev-string-append (cdr l) (+ i len))))
            (let loop ((j 0) (k (- (- (string-length result) i) len)))
              (if (< j len)
                  (begin
                    (string-set! result k (string-ref str j))
                    (loop (+ j 1) (+ k 1)))
                  result)))
          (make-string i)))
      (rev-string-append l 0))
      
    (define newline-str (make-string 1 #\newline))
  )
)
