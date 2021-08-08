;;; Math tools
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2017-2019 Matthias Zenger. All rights reserved.
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

(import (lispkit base))

;; Returns the factorial decomposition of integer `n` in form of a list of prime numbers
(define (factors n)
  (cond ((negative? n) (cons -1 (factors (- n))))
        ((< n 3)       (list n))
        ((even? n)     (cons 2 (factors (/ n 2))))
        (else          (let loop ((divisor 3) (n n))
                         (if (> (square divisor) n)
                             (list n)
                             (if (zero? (modulo n divisor))
                                 (cons divisor (loop divisor (/ n divisor)))
                                 (loop (+ divisor 2) n)))))))

;; Returns `#t` if integer `n` is a prime number, `#f` otherwise.
(define (prime? n)
  (cond ((= n 0)   #f)
        ((= n 1)   #f)
        ((= n 2)   #t)
        ((even? n) #f)
        (else      (let loop ((d 3))
                     (cond ((> (square d) n)        #t)
                           ((zero? (remainder n d)) #f)
                           (else                    (loop (+ d 2))))))))

;; Returns the `n`-th Fibonacci number.
(define (fib n)
  (if (fx< n 2)
      n
      (fx+ (fib (fx1- n)) (fib (fx- n 2)))))

;; Returns the `n`-th Fibonacci number via a tail-recursive algorithm.
(define (fast-fib n)
  (define (iter n a b)
    (if (fx< n 3)
        b
        (iter (fx1- n) b (fx+ a b))))
  (if (fx< n 2)
      n
      (iter n 1 1)))

;; Returns the factorial of integer `n`
(define (fac n)
  (if (= n 0)
      1
      (* n (fac (- n 1)))))

;; Returns a list of all permutations of the given list `xs`
(define (permutations xs)
  (cond ((null? xs)
          (list (list)))
        ((null? (cdr xs))
          (list xs))
        (else
          (let splice ((l '())
                       (m (car xs))
                       (r (cdr xs)))
            (append (map (lambda (x) (cons m x)) (permutations (append l r)))
                    (if (null? r) '() (splice (cons m l) (car r) (cdr r))))))))

;; Returns all `k`-combinations of elements in `xs` as a list.
(define (combination k xs)
  (cond ((zero? k)
          (list (list)))
        ((null? xs)
          xs)
        (else
          (append (map (lambda (y) (cons (car xs) y)) (combination (- k 1) (cdr xs)))
                  (combination k (cdr xs))))))

;; Returns all possible combinations of elements in `xs` as a list.
(define (combinations xs)
  (if (null? xs)
      (list xs)
      (let* ((head (car xs))
             (s (combinations (cdr xs)))
             (v (map (lambda (x) (cons head x)) s)))
        (append s v))))
