;;; SRFI 143
;;; Fixnums
;;;
;;; This SRFI describes arithmetic procedures applicable to a limited range of exact
;;; integers only. These procedures are semantically similar to the corresponding
;;; generic-arithmetic procedures, but allow more efficient implementations.
;;; 
;;; Fixnums are an implementation-defined subset of the exact integers. The fixnum range
;;; is defined as a closed interval [-2w-1, 2w-1-1], where w is an integer greater than or
;;; equal to 24. Every mathematical integer within an implementation's fixnum range must
;;; correspond to an exact integer that is representable within the implementation.
;;; A fixnum is an exact integer whose value lies within this fixnum range.
;;; 
;;; Fixnum operations perform integer arithmetic on their fixnum arguments. If any argument
;;; is not a fixnum, or if the mathematical result is not representable as a fixnum,
;;; it is an error: this is known as the fixnum rule. In particular, this means that fixnum
;;; operations may return a mathematically incorrect fixnum in these situations without
;;; raising an error. Consequently, when this SRFI says things like "fx+ is semantically
;;; equivalent to +", the phrase "except for the effects of the fixnum rule" is to be
;;; understood.
;;; 
;;; Author of spec: John Cowan
;;; 
;;; Copyright © 2021 Matthias Zenger. All rights reserved.
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

(define-library (srfi 143)
  
  (export fx-width
          fx-greatest
          fx-least
          fixnum?
          fx=?
          fx<?
          fx>?
          fx<=?
          fx>=?
          fxzero?
          fxpositive?
          fxnegative?
          fxodd?
          fxeven?
          fxmax
          fxmin
          fxneg
          fx+
          fx-
          fx*
          fxquotient
          fxremainder
          fxabs
          fxsquare
          fxsqrt
          fx+/carry
          fx-/carry
          fx*/carry
          fxnot
          fxand
          fxior
          fxxor
          fxarithmetic-shift
          fxarithmetic-shift-left
          fxarithmetic-shift-right
          fxbit-count
          fxlength
          fxif
          fxbit-set?
          fxcopy-bit
          fxfirst-set-bit
          fxbit-field
          fxbit-field-rotate
          fxbit-field-reverse)
  
  (import (rename (lispkit base) (fxmax lispkit:fxmax)
                                 (fxmin lispkit:fxmin)
                                 (fxsqrt lispkit:fxsqrt)
                                 (fxbit-set? lispkit:fxbit-set?)
                                 (fxcopy-bit lispkit:fxcopy-bit)
                                 (fxbit-count lispkit:fxbit-count)))
  
  ;; Generic implementation of carry functions from the R6RS standard.
  ;; These implementations of fx+/carry, fx-/carry, and fx*/carry are very inefficient,
  ;; and should be replaced by proper-subset? assembly language operations if at all possible.
  (begin
    
    (define exp-width (expt 2 fx-width))
    
    (define (fx+/carry i j k)
      (let*-values (((s) (+ i j k))
                    ((q r) (balanced/ s exp-width)))
        (values r q)))
    
    (define (fx-/carry i j k)
      (let*-values (((d) (- i j k))
                    ((q r) (balanced/ d exp-width)))
        (values r q)))
    
    (define (fx*/carry i j k)
      (let*-values (((s) (+ (* i j) k))
                    ((q r) (balanced/ s exp-width)))
        (values r q)))
    
    ;;; Helper functions from SRFI 151
    
    (define (floor-/+ n d)
      (let ((n (- 0 n)))
        (let ((q (quotient n d)) (r (remainder n d)))
          (if (zero? r)
              (values (- 0 q) r)
              (values (- (- 0 q) 1) (- d r))))))
    
    (define (ceiling-/- n d)
      (let ((n (- 0 n)) (d (- 0 d)))
        (let ((q (quotient n d)) (r (remainder n d)))
          (if (zero? r)
              (values q r)
              (values (+ q 1) (- d r))))))
    
    (define (euclidean/ n d)
      (if (and (exact-integer? n) (exact-integer? d))
          (cond ((and (negative? n) (negative? d)) (ceiling-/- n d))
                ((negative? n) (floor-/+ n d))
                ((negative? d)
                  (let ((d (- 0 d)))
                    (values (- 0 (quotient n d)) (remainder n d))))
                (else (values (quotient n d) (remainder n d))))
          (let ((q (if (negative? d) (ceiling (/ n d)) (floor (/ n d)))))
            (values q (- n (* d q))))))
    
    (define (balanced/ x y)
      (call-with-values
        (lambda () (euclidean/ x y))
        (lambda (q r)
          (cond ((< r (abs (/ y 2)))
                  (values q r))
                ((> y 0)
                  (values (+ q 1) (- x (* (+ q 1) y))))
                (else
                  (values (- q 1) (- x (* (- q 1) y))))))))
  )
  
  ;; Implementation based on existing fixnum procedures provided by library
  ;; `(lispkit math)`.
  (begin
    
    (define (fx=? i j . ks)
      (if (null? ks)
          (fx= i j)
          (and (fx= i j) (apply fx=? j ks))))
    
    (define (fx<? i j . ks)
      (if (null? ks)
          (fx< i j)
          (and (fx< i j) (apply fx<? j ks))))
    
    (define (fx>? i j . ks)
      (if (null? ks)
          (fx> i j)
          (and (fx> i j) (apply fx>? j ks))))
    
    (define (fx<=? i j . ks)
      (if (null? ks)
          (fx<= i j)
          (and (fx<= i j) (apply fx<=? j ks))))
    
    (define (fx>=? i j . ks)
      (if (null? ks)
          (fx>= i j)
          (and (fx>= i j) (apply fx>=? j ks))))
    
    (define (fxmax i j . ks)
      (if (null? ks)
          (lispkit:fxmax i j)
          (lispkit:fxmax (lispkit:fxmax i j) (apply fxmax j ks))))
    
    (define (fxmin i j . ks)
      (if (null? ks)
          (lispkit:fxmin i j)
          (lispkit:fxmin (lispkit:fxmin i j) (apply fxmin j ks))))
    
    (define (fxneg i)
      (fx- 0 i))
    
    (define (fxsquare i)
      (fx* i i))
    
    (define (fxquotient i j)
      (quotient i j))
    
    (define (fxsqrt i)
      (exact-integer-sqrt i))
    
    (define (fxfirst-set-bit i)
      (fxfirst-bit-set i))
    
    (define (fxbit-set? i j)
      (bit-set? j i))
      
    (define (fxbit-count i)
      (bit-count i))
    
    (define (fxcopy-bit index to bool)
      (copy-bit to index (if (eq? bool #t) 1 (if (eq? bool #f) 0 bool))))
    
    (define (mask start end)
      (bitwise-not (arithmetic-shift -1 (- end start))))
    
    (define (fxbit-field n start end)
      (bitwise-and (mask start end) (arithmetic-shift n (- start))))
    
    (define (fxbit-field-rotate n count start end)
      (define width (- end start))
      (set! count (modulo count width))
      (let ((mask (bitwise-not (arithmetic-shift -1 width))))
        (define zn (bitwise-and mask (arithmetic-shift n (- start))))
        (bitwise-ior (arithmetic-shift
                       (bitwise-ior (bitwise-and mask (arithmetic-shift zn count))
                                    (arithmetic-shift zn (- count width)))
                       start)
                     (bitwise-and (bitwise-not (arithmetic-shift mask start)) n))))
    
    (define (bit-reverse k n)
      (do ((m (if (negative? n) (bitwise-not n) n) (arithmetic-shift m -1))
           (k (+ -1 k) (+ -1 k))
           (rvs 0 (bitwise-ior (arithmetic-shift rvs 1) (bitwise-and 1 m))))
        ((negative? k) (if (negative? n) (bitwise-not rvs) rvs))))
    
    (define (fxbit-field-reverse n start end)
      (define width (- end start))
      (let ((mask (bitwise-not (arithmetic-shift -1 width))))
        (define zn (bitwise-and mask (arithmetic-shift n (- start))))
        (bitwise-ior (arithmetic-shift (bit-reverse width zn) start)
                     (bitwise-and (bitwise-not (arithmetic-shift mask start)) n))))
  )
)
