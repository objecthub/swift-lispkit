;;; SRFI 228
;;; Composing Comparators
;;;
;;; A typical pattern for defining comparison behaviour for record types and
;;; other user-defined Scheme types is some form of lexicographical order over
;;; their member slots. The comparator abstraction of SRFI 128 has useful
;;; potential for reducing the code bulk of defining such comparison behaviours.
;;; This library is intended to take advantage of that potential.
;;;
;;; Copyright © 2021 Daphne Preston-Kendal. All rights reserved.
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
;;;   Copyright © 2022 Matthias Zenger. All rights reserved.

(define-library (srfi 228)
  
  (export make-wrapper-comparator
          make-product-comparator
          make-sum-comparator
          comparator-one
          comparator-zero)
  
  (import (except (lispkit base) copy-bit bit-set?)
          (srfi 1)
          (srfi 128)
          (srfi 151))
  
  (begin
    
    (define (make-wrapper-comparator type-test unwrap contents-comparator)
      (make-comparator
        type-test
        (lambda (a b)
          ((comparator-equality-predicate contents-comparator)
           (unwrap a)
           (unwrap b)))
        (if (comparator-ordering-predicate contents-comparator)
            (lambda (a b)
              ((comparator-ordering-predicate contents-comparator)
               (unwrap a)
               (unwrap b)))
            #f)
        (if (comparator-hash-function contents-comparator)
            (lambda (x)
              ((comparator-hash-function contents-comparator) x))
            #f)))
    
    (define (make-product-comparator . comparators)
      (if (null? comparators)
          comparator-one
          (let* ((type-tests
                   (delete-duplicates
                     (map comparator-type-test-predicate comparators)
                     eq?))
                 (type-test
                   (lambda (val)
                     (every (lambda (test) (test val)) type-tests))))
            (make-comparator
              type-test
              (lambda (a b)
                (every (lambda (cmp)
                         ((comparator-equality-predicate cmp) a b))
                       comparators))
              (if (every comparator-ordered? comparators)
                  (lambda (a b)
                    (let loop ((cmps comparators))
                      (cond ((null? cmps) #f)
                            (((comparator-ordering-predicate (car cmps)) a b) #t)
                            (((comparator-equality-predicate (car cmps)) a b) (loop (cdr cmps)))
                            (else #f))))
                  #f)
              (if (every comparator-hashable? comparators)
                  (lambda (x)
                    (fold bitwise-xor
                          0
                          (map (lambda (cmp)
                                 ((comparator-hash-function cmp) x))
                               comparators)))
                  #f)))))
    
    (define (comparator-index comparators val)
      (list-index
        (lambda (cmp)
          ((comparator-type-test-predicate cmp) val))
        comparators))
    
    (define (make-sum-comparator . comparators)
      (if (null? comparators)
          comparator-zero
          (make-comparator
            (lambda (x)
              (any
                (lambda (cmp)
                  ((comparator-type-test-predicate cmp) x))
                comparators))
            (lambda (a b)
              (let ((a-cmp-idx (comparator-index comparators a))
                    (b-cmp-idx (comparator-index comparators b)))
                (if (not (= a-cmp-idx b-cmp-idx))
                    #f
                    (let ((cmp (list-ref comparators a-cmp-idx)))
                      ((comparator-equality-predicate cmp) a b)))))
            (if (every comparator-ordered? comparators)
                (lambda (a b)
                  (let ((a-cmp-idx (comparator-index comparators a))
                        (b-cmp-idx (comparator-index comparators b)))
                    (cond ((< a-cmp-idx b-cmp-idx) #t)
                          ((> a-cmp-idx b-cmp-idx) #f)
                          (else
                            (let ((cmp (list-ref comparators a-cmp-idx)))
                              ((comparator-ordering-predicate cmp) a b))))))
                #f)
            (if (every comparator-hashable? comparators)
                (lambda (x)
                  (let ((cmp (find (lambda (cmp) ((comparator-type-test-predicate cmp) x))
                                   comparators)))
                    ((comparator-hash-function cmp) x)))
                #f))))
    
    (define comparator-one
      (make-comparator
        (lambda (x) #t)
        (lambda (a b) #t)
        (lambda (a b) #f)
        (lambda (x) 0)))
    
    (define comparator-zero
      (make-comparator
        (lambda (x) #f)
        (lambda (a b) (error "can't compare" a b))
        (lambda (a b) (error "can't compare" a b))
        (lambda (x) (error "can't hash" x))))
  )
)
