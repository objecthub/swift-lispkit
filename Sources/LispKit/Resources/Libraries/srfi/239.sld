;;; SRFI 239
;;; Destructuring Lists
;;;
;;; This SRFI provides `list-case`, the syntactic fundamental list destructor.
;;; The list data type is the initial algebra with constructors `(cons obj list)`
;;; and `()`. Destructing a list step-wise thus means testing whether the list
;;; is a pair or (), and retrieving the car and cdr of the pair in the first
;;; case. In practise, this often leads to Scheme code of the following form
;;; (even the sample implementation of this SRFI contains such code):
;;;
;;; ```
;;; (let ((ls <list-expression>))
;;;   (cond ((pair? ls)
;;;           (let ((head (car ls)) ((tail (cdr ls))))
;;;             ...))
;;;         ((null? ls)
;;;           ...)
;;;         (else
;;;           (assert #f))))
;;; ```
;;;
;;; This SRFI defines syntax that abstracts over this (and also generalizes
;;; destructuring to improper lists):
;;;
;;; ```
;;; (list-case <list-expression>
;;;   ((head . tail)
;;;     ...)
;;;   (()
;;;     ...)
;;;   (_
;;;     (assert #f)))
;;; ```
;;;
;;; Copyright © 2023 Robby Zambito. All rights reserved.
;;; Copyright © 2022 Marc Nieper-Wißkirchen. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; Adaptation to LispKit
;;;   Copyright © 2023 Matthias Zenger. All rights reserved.

(define-library (srfi 239)
  
  (export list-case)
  
  (import (lispkit base))
  
  (begin
    
    (define-syntax duplicate-clause
      (syntax-rules ()
        ((_ c) (syntax-error "duplicate clause in list-case" c))))
    
    (define-syntax list-case
      (syntax-rules ()
        ((list-case expr clauses ...)
          (letrec-syntax
            ((make-clauses
              (syntax-rules ::: (_ pair null dotted matched)
                ((make-clauses obj p n d)
                  (error "list-case: no matching clause" obj))
                
                ;; pair clauses
                ((make-clauses obj (matched pair) n d ((_ . _) body1 ::: body2) remaining :::)
                  (duplicate-clause '(_ . _)))
                
                ((make-clauses obj pair n d ((_ . _) body1 ::: body2) remaining :::)
                  (if (pair? obj)
                      (begin body1 ::: body2)
                      (make-clauses obj (matched pair) n d remaining :::)))
                
                ((make-clauses obj (matched pair) n d ((head . _) body1 ::: body2) remaining :::)
                  (duplicate-clause '(head . _)))
                
                ((make-clauses obj pair n d ((head . _) body1 ::: body2) remaining :::)
                  (if (pair? obj)
                      (let ((head (car obj)))
                        body1 ::: body2)
                      (make-clauses obj (matched pair) n d remaining :::)))
                
                ((make-clauses obj (matched pair) n d ((_ . tail) body1 ::: body2) remaining :::)
                  (duplicate-clause '(_ . tail)))
                
                ((make-clauses obj pair n d ((_ . tail) body1 ::: body2) remaining :::)
                  (if (pair? obj)
                      (let ((tail (cdr obj)))
                        body1 ::: body2)
                      (make-clauses obj (matched pair) n d remaining :::)))
                
                ((make-clauses obj (matched pair) n d ((head . tail) body1 ::: body2) remaining :::)
                  (duplicate-clause '(head . tail)))
                
                ((make-clauses obj pair n d ((head . tail) body1 ::: body2) remaining :::)
                  (if (pair? obj)
                      (let ((head (car obj)) (tail (cdr obj)))
                        body1 ::: body2)
                      (make-clauses obj (matched pair) n d remaining :::)))
                
                ;; null clauses
                ((make-clauses obj p (matched null) d (() body1 ::: body2) remaining :::)
                  (duplicate-clause '()))
                
                ((make-clauses obj p null d (() body1 ::: body2) remaining :::)
                  (if (null? obj)
                      (begin body1 ::: body2)
                      (make-clauses obj p (matched null) d remaining :::)))
                
                ;; dotted clauses
                ((make-clauses obj p n (matched dotted) (_ body1 ::: body2) remaining :::)
                  (duplicate-clause '_))
                
                ((make-clauses obj p n dotted (_ body1 ::: body2) remaining :::)
                  (if (and (not (null? obj))
                           (not (pair? obj)))
                      (begin body1 ::: body2)
                      (make-clauses obj p n (matched dotted) remaining :::)))
                
                ((make-clauses obj p n (matched dotted) (x body1 ::: body2) remaining :::)
                  (duplicate-clause 'x))
                
                ((make-clauses obj p n dotted (x body1 ::: body2) remaining :::)
                  (if (and (not (null? obj))
                           (not (pair? obj)))
                      (let ((x obj))
                        body1 ::: body2)
                      (make-clauses obj p n (matched dotted) remaining :::))))))
            (let ((obj expr))         ;only evaluate expr once
              (make-clauses obj pair null dotted clauses ...))))))
  )
)
