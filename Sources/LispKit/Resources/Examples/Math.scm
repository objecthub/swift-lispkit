;;; Math tools
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


;;; Returns the factorial decomposition of integer `n` in form of a list of prime numbers
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

;;; Returns `#t` if integer `n` is a prime number, `#f` otherwise.
(define (prime? n)
  (cond ((= n 0)   #f)
        ((= n 1)   #f)
	      ((= n 2)   #t)
	      ((even? n) #f)
	      (else      (let loop ((d 3))
                     (cond ((> (square d) n)        #t)
                           ((zero? (remainder n d)) #f)
                           (else                    (loop (+ d 2))))))))

;;; Returns the `n`-th Fibonacci number
(define (fib n)
  (if (fx< n 2)
      n
      (fx+ (fxfib (fx1- n)) (fxfib (fx- n 2)))))

;;; Returns the factorial of integer `n`
(define (fac n)
  (if (= n 0)
      1
      (* n (fac (- n 1)))))
