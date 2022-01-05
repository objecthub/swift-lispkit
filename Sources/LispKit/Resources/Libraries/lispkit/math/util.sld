;;; LISPKIT MATH UTIL
;;;
;;; Library providing mathematical utilities.
;;;
;;; Author: Matthias Zenger
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

(define-library (lispkit math util)
  
  (export sgn
          numbers
          sum
          product
          minimum
          maximum
          conjugate
          degrees->radians
          radians->degrees
          prime?
          make-nan
          nan-negative?
          nan-quiet?
          nan-payload
          nan=?)
  
  (import (lispkit base)
          (lispkit internal))
  
  (begin
    
    ;; Returns `#t` independent of argument `x`.
    (define (true x) #t)
    
    ;; Implements the sign/signum function. Returns -1 if `x` is negative, 0 (or a
    ;; signed zero, when inexact) if `x` is zero, and 1 if `x` is a positive number.
    ;; `sgn` fails if `x` is not a real number.
    (define (sgn x)
      (cond ((nan? x)     +nan.0)
            ((eq? x -0.0)   -0.0)
            ((zero? x)       0.0)
            ((negative? x)  -1.0)
            (else            1.0)))
    
    ;; Returns a list of numbers by iterating from integer `lo` to integer `hi`
    ;; (both inclusive) and applying function `f` to each integer in the range
    ;; for which `guard` returns true.
    ;; The default guard always returns true. The default for `f` is `identity`.
    (define numbers
      (case-lambda
        ((lo hi)
           (numbers lo hi true identity))
        ((lo hi f)
           (numbers lo hi true f))
        ((lo hi guard f)
           (assert (integer? lo) (integer? hi))
           (if (<= lo hi)
               (do ((i hi (- i 1))
                    (xs '() (if (guard i)
                                (let ((x (f i)))
                                  (if x (cons (if (eq? x #t) i x) xs) xs))
                                xs)))
                   ((< i lo) xs))
               '()))))
    
    ;; Returns the sum of all numbers of list `xs`. This procedure fails if there is
    ;; an element in `xs` which is not a number.
    (define (sum xs)
      (do ((xs xs (cdr xs))
           (acc 0 (+ acc (car xs))))
          ((null? xs) acc)))
    
    ;; Returns the product of all numbers of list `xs`. This procedure fails if there is
    ;; an element in `xs` which is not a number.
    (define (product xs)
      (do ((xs xs (cdr xs))
           (acc 1 (* acc (car xs))))
          ((null? xs) acc)))
    
    ;; Returns the minimum of all numbers of list `xs`. This procedure fails if there is
    ;; an element in `xs` which is not a number.
    (define (minimum xs)
      (if (null? xs)
          #f
          (do ((xs (cdr xs) (cdr xs))
               (m (car xs) (if (< (car xs) m) (car xs) m)))
              ((null? xs) m))))
    
    ;; Returns the maximum of all numbers of list `xs`. This procedure fails if there is
    ;; an element in `xs` which is not a number.
    (define (maximum xs)
      (if (null? xs)
          #f
          (do ((xs (cdr xs) (cdr xs))
               (m (car xs) (if (> (car xs) m) (car xs) m)))
              ((null? xs) m))))
    
    ;; Conjugates number `x`. For real numbers `x`, `conjugate` returns `x`, otherwise
    ;; `x` is being returned with the opposite sign for the imaginary part.
    (define (conjugate x)
      (if (real? x)
          x
          (make-rectangular (real-part x) (- (imag-part x)))))
    
    ;; Converts degrees into radians.
    (define (degrees->radians x)
      (/ (* x pi) 180))

    ;; Converts radians into degrees.
    (define (radians->degrees x)
      (/ (* x 180) pi))
    
    ;; Returns `#t` if integer `n` is a prime number, `#f` otherwise.
    (define (prime? n)
      (cond ((= n 0)   #f)
            ((= n 1)   #f)
            ((= n 2)   #t)
            ((even? n) #f)
            (else      (do ((d 3 (+ d 2)))
                           ((or (> (square d) n) (zero? (remainder n d))) (> (square d) n))))))
    
    ;; Returns `#t` if `n` is a negative NaN, `#f` otherwise.
    (define (nan-negative? n)
      (and (nan? n) (flnegative? n)))
    
    ;; Returns `#t` if both `n1` and `n2` are NaNs and their representation is equal.
    (define (nan=? n1 n2)
      (and (nan? n1) (flbits=? n1 n2)))
  )
)
