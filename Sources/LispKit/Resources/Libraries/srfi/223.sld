;;; SRFI 223
;;; Generalized binary search procedures
;;;
;;; Generalized procedures for binary search of vector-like data structures are provided
;;; which can be applied to any sequence type, including ones defined by the user,
;;; together with applications of these procedures for Scheme’s built-in vector and
;;; bytevector types.
;;;
;;; Copyright © 2021 Daphne Preston-Kendal. All rights reserved.
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
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
;;; PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
;;; FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
;;; OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;
;;; Adaptation to LispKit
;;;   Copyright © 2021 Matthias Zenger. All rights reserved.

(define-library (srfi 223)
  
  (export bisection
          bisect-left
          bisect-right
          vector-bisect-left
          vector-bisect-right)
  
  (import (lispkit base))
  
  (begin
    
    (define (bisect-left a val ref less? lo hi)
      (if (>= lo hi) lo
          (let ((mid (floor-quotient (+ lo hi) 2)))
            (if (less? (ref a mid) val)
                (bisect-left a val ref less? (+ mid 1) hi)
                (bisect-left a val ref less? lo mid)))))
    
    (define (bisect-right a val ref less? lo hi)
      (if (>= lo hi) lo
          (let ((mid (floor-quotient (+ lo hi) 2)))
            (if (less? val (ref a mid))
                (bisect-right a val ref less? lo mid)
                (bisect-right a val ref less? (+ mid 1) hi)))))
    
    (define bisection
      (case-lambda
        ((ref lo-hi-proc)
          (values
            (case-lambda
              ((a val less?)
                (let-values (((lo hi) (lo-hi-proc a)))
                  (bisect-left a val ref less? lo hi)))
              ((a val less? lo hi)
                (bisect-left a val ref less? lo hi)))
            (case-lambda
              ((a val less?)
                (let-values (((lo hi) (lo-hi-proc a)))
                  (bisect-right a val ref less? lo hi)))
              ((a val less? lo hi)
                (bisect-right a val ref less? lo hi)))))
        ((ref)
          (bisection ref
                     (lambda (a)
                       (error "both lo and hi arguments must be given to this procedure"))))))
    
    (define-values (vector-bisect-left vector-bisect-right)
      (bisection vector-ref (lambda (v) (values 0 (vector-length v)))))
  )
)
