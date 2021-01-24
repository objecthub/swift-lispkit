;;; SRFI 155 REGRESSION TEST SUITE
;;;
;;; This is the test suite for SRFI 155.
;;;
;;; Copyright © 2017 Marc Nieper-Wißkirchen. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify,
;;; merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies
;;; or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
;;; PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT
;;; OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2021 Matthias Zenger. All rights reserved.

(import (except (lispkit base) delay delay-force)
        (lispkit test)
        (srfi 154)
        (srfi 155))

(define integers
  (let next ((n 0))
    (delay (cons n (next (+ n 1))))))

(define (head stream)
  (car (force stream)))

(define (tail stream)
  (cdr (force stream)))

(define (stream-filter p? s)
  (delay-force
    (if (null? (force s))
        (delay '())
        (let ((h (car (force s)))
              (t (cdr (force s))))
          (if (p? h)
              (delay (cons h (stream-filter p? t)))
              (stream-filter p? t))))))

(define count 0)

(define p
  (delay (begin (set! count (+ count 1))
                (if (> count x)
                    count
                    (force p)))))

(define x 5)

(test-begin "SRFI 155: Promises")

(test-equal 3 (force (delay (+ 1 2))))
(test-equal '(3 3)
            (let ((p (delay (+ 1 2))))
              (list (force p) (force p))))
(test-equal 2 (head (tail (tail integers))))
(test-equal 5 (head (tail (tail (stream-filter odd? integers)))))

(test-assert (promise? p))
(test-equal 6 (force p))
(test-assert (promise? p))
(test-equal 6 (begin (set! x 10)
                     (force p)))

(test-equal "Dynamic extents"
            '((1 2) 2)
            (let ((x (make-parameter 1)))
              (let ((p
                      (delay-force (delay (list (x)
                                                (with-dynamic-extent (forcing-extent)
                                                                     (lambda () (x)))))))
                    (q (delay (with-dynamic-extent (forcing-extent) (lambda () (x))))))
                (parameterize
                  ((x 2))
                  (list
                    (force (delay-force p))
                    (force q))))))

(test-equal "Nested calls"
            150
            (let ()
              (define integers
                (let next ((n 0))
                  (delay (cons n (next (+ n 1))))))
              
              (define (xtake stream n)
                (let loop ((s stream) (r '()) (n n))
                  (if (= n 0)
                      (reverse r)
                      (loop (cdr (force s))
                            (cons (car (force s)) r)
                            (- n 1)))))
              
              (length (xtake integers 150))))

(test-end)
