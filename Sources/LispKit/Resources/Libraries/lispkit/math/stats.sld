;;; LISPKIT MATH STATS
;;;
;;; Implementation of a number of statistical functions. The functions compute statistics,
;;; meaning summary values for collections of samples, and functions for managing sequences
;;; of samples. Most of the functions that compute statistics accept a list of real
;;; numbers corresponding to sample values.
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

(define-library (lispkit math stats)
  
  (export mode
          mean
          range
          variance
          stddev
          absdev
          skewness
          kurtosis
          quantile
          percentile
          median
          interquartile-range
          five-number-summary
          covariance
          correlation)
  
  (import (lispkit base)
          (lispkit math util))
  
  (begin
    
    ;; Computes the mode of a set of numbers `xs`. The mode is the value that appears most
    ;; often in `xs`. `xs` is a proper list of numeric values. `=` is used as the equality
    ;; operator.
    (define (mode xs)
      (let loop ((ys (sort < xs))
                 (m #f)
                 (nm 0)
                 (v #f)
                 (nv 0))
        (cond ((null? ys)
                (if (> nv nm) v m))
              ((and v (= (car ys) v))
                (loop (cdr ys) m nm v (+ nv 1)))
              ((> nv nm)
                (loop (cdr ys) v nv (car ys) 1))
              (else
                (loop (cdr ys) m nm (car ys) 1)))))
    
    ;; Computes the arithmetic mean of a set of numbers `xs`. `xs` is a proper list of
    ;; numeric values.
    (define (mean xs)
      (/ (sum xs) (length xs)))
    
    ;; Computes the range of a set of numbers `xs`, i.e. the difference between the largest
    ;; and the smallest value. `xs` is a proper list of numeric values which are ordered
    ;; using the `<` relation.
    (define (range xs)
      (- (maximum xs) (minimum xs)))
    
    ;; Computes the variance for a set of numbers `xs`, optionally applying
    ;; bias correction. `xs` is a proper list of numeric values. Bias correction gets
    ;; enabled by setting `bias` to `#t`. Alternatively, it is possible to provide a
    ;; a positive integer, which is used instead of the number of elements in `xs`.
    (define (variance xs . args)
      (let-optionals args ((bias #f))
        (let ((m (mean xs)))
          (/ (sum (map (lambda (x) (square (- x m))) xs))
             (if (integer? bias)
                 bias
                 (- (length xs) (if bias 1 0)))))))
    
    ;; Computes the standard deviation for a set of numbers `xs`, optionally applying
    ;; bias correction. `xs` is a proper list of numeric values. Bias correction gets
    ;; enabled by setting `bias` to `#t`. Alternatively, it is possible to provide a
    ;; a positive integer, which is used instead of the number of elements in `xs`.
    (define (stddev xs . args)
      (let-optionals args ((bias #f))
        (sqrt (variance xs bias))))
    
    ;; Computes the average absolute difference between the numbers in list `xs` and
    ;; `(median xs)`.
    (define (absdev xs)
      (if (zero? n)
          +nan.0
          (let ((m (mean xs)))
            (max 0 (/ (sum (map (lambda (x) (abs (- x m))) xs)) (length xs))))))
    
    ;; Computes the skewness for a set of numbers `xs`, optionally applying
    ;; bias correction. `xs` is a proper list of numeric values. Bias correction gets
    ;; enabled by setting `bias` to `#t`. Alternatively, it is possible to provide a
    ;; a positive integer, which is used instead of the number of elements in `xs`.
    (define (skewness xs . args)
      (let-optionals args ((bias #f))
        (let ((n (length xs))
              (m (mean xs))
              (s (stddev xs)))
          (* (/ (fold-left (lambda (z x) (+ z (/ (expt (- x m) 3) n))) 0 xs)
                (expt s 3))
             (if bias
                 (let ((n (if (integer? bias) bias n)))
                   (/ (sqrt (* n (- n 1))) (- n 2)))
                 1)))))
    
    ;; Computes the kurtosis for a set of numbers `xs`, optionally applying
    ;; bias correction. `xs` is a proper list of numeric values. Bias correction gets
    ;; enabled by setting `bias` to `#t`. Alternatively, it is possible to provide a
    ;; a positive integer, which is used instead of the number of elements in `xs`.
    (define (kurtosis xs . args)
      (let-optionals args ((bias #f))
        (let ((n (length xs))
              (m (mean xs))
              (s (stddev xs bias)))
          (/ (fold-left (lambda (z x) (+ z (expt (- x m) 4))) 0 xs)
             (* (expt s 4)
                (if (integer? bias)
                    bias
                    (if bias (- n 1) n)))))))
    
    ;; Computes the p-quantile for a set of numbers `xs` (also known as the
    ;; _inverse cumulative distribution_). `p` is a real number between 0 and
    ;; 1.0. For instance, the 0.5-quantile corresponds to the median.
    (define (quantile xs p)
      (let ((np (* p (- (length xs) 1)))
            (ys (sort < xs)))
        (mean (list (list-ref ys (exact (floor np)))
                    (list-ref ys (exact (ceiling np)))))))
    
    ;; Computes the percentile for a set of numbers `xs` and a given percentage `pct`.
    ;; `pct` is a number between 0 and 100. For instance, the 90th percentile corresponds
    ;; to the 0.9-quantile.
    (define (percentile xs pct)
      (assert (>= pct 0) (<= pct 100))
      (quantile xs (/ pct 100)))
    
    ;; Computes the median for a set of numbers `xs`.
    (define (median xs)
      (quantile xs 0.5))
    
    ;; Returns the interquartile range for a given set of numbers `xs`. `xs` is a proper
    ;; list of numeric values. The interquartile range is the difference between the
    ;; 0.75-quantile and the 0.25-quantile.
    (define (interquartile-range xs)
      (- (quantile xs 0.75) (quantile xs 0.25)))
    
    ;; Returns a list of 5 statistics describing the set of numbers `xs`: the minimum value,
    ;; the lower quartile, the median, the upper quartile, and the maximum value.
    (define (five-number-summary xs)
      (map (lambda (p) (quantile xs p)) '(0 0.25 0.5 0.75 1)))
      
    ;; Computes the covariance of two sets of numbers `xs` and `ys`. Both `xs` and
    ;; `ys` are proper lists of numbers. Bias correction can be enabled by setting
    ;; `bias` to `#t`. Alternatively, it is possible to provide a positive integer,
    ;; which is used instead of the number of elements in `xs`.
    (define (covariance xs ys . args)
      (let-optionals args ((bias #f))
        (assert (= (length xs) (length ys)))
        (let ((xm (mean xs))
              (ym (mean ys)))
          (/ (fold-left (lambda (z x y) (+ z (* (- x xm) (- y ym)))) 0 xs ys)
             (if (integer? bias)
                 bias
                 (- (length xs) (if bias 1 0)))))))
    
    ;; Computes the correlation of two sets of numbers `xs` and `ys` in form of 
    ;; the _Pearson product-moment correlation coefficient_. Both `xs` and
    ;; `ys` are proper lists of numbers. Bias correction can be enabled by setting
    ;; `bias` to `#t`. Alternatively, it is possible to provide a positive integer,
    ;; which is used instead of the number of elements in `xs`.
    (define (correlation xs ys . args)
      (let-optionals args ((bias #f))
        (/ (covariance xs ys bias)
           (* (stddev xs bias) (stddev ys bias)))))
  )
)
