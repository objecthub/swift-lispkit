;;; Stream Examples
;;;
;;; This program illustrates the usage of the `(lispkit stream)` package. This package
;;; provides an API based on SRFI 41 for the `stream` datatype. A stream is a sequential
;;; data structure whose elements are computed only on demand. Streams get constructed
;;; with list-like constructors. A stream is either _null_ or is a _pair_ with a stream
;;; in its cdr. Since elements of a stream are computed only when accessed, streams can
;;; be infinite. Once computed, the value of a stream element is cached in case it is
;;; needed again.
;;; 
;;; The examples below are adapted from Abelson and Sussman's book on "Structure and
;;; Interpretation of Computer Programs". Abelson and Sussman discuss streams at length,
;;; giving a strong justification for their use. The streams they provide are represented
;;; as a cons pair with a promise to return a stream in its cdr; for instance, a stream
;;; with elements the first three counting numbers is represented conceptually as
;;; `(cons 1 (delay (cons 2 (delay (cons 3 (delay '()))))))`. Wadler, Taha and MacQueen
;;; describe such streams as "odd" because, regardless of their length, the parity of
;;; the number of constructors `(delay, cons, '())` in the stream is odd.
;;;
;;; The streams provided by `(lispkit stream)` differ from those of Abelson and Sussman,
;;; being represented as promises that contain a cons pair with a stream in its cdr;
;;; for instance, the stream with elements the first three counting numbers is represented
;;; conceptually as `(delay (cons 1 (delay (cons 2 (delay (cons 3 (delay '())))))))`.
;;; This is an "even" stream because the parity of the number of constructors in the
;;; stream is even.
;;;
;;; Even streams are more complex than odd streams in both definition and usage, but they
;;; offer a strong benefit: they fix the off-by-one error of odd streams. For instance,
;;; an expression like `(stream->list 4 (stream-map / (stream-from 4 -1)))` evaluates to
;;; `(1/4 1/3 1/2 1)` using even streams but fails with a divide-by-zero error using odd
;;; streams, because the next element in the stream, which will be 1/0, is evaluated before
;;; it is accessed.
;;;
;;; When used effectively, the primary benefit of streams is improved modularity. Consider
;;; a process that takes a sequence of items, operating on each in turn. If the operation
;;; is complex, it may be useful to split it into two or more procedures in which the
;;; partially-processed sequence is an intermediate result. If that sequence is stored as
;;; a list, the entire intermediate result must reside in memory all at once; however,
;;; if the intermediate result is stored as a stream, it can be generated piecemeal,
;;; using only as much memory as required by a single item. This leads to a programming
;;; style that uses many small operators, each operating on the sequence of items as a
;;; whole, similar to a pipeline of unix commands.
;;;
;;; In addition to improved modularity, streams permit a clear exposition of backtracking
;;; algorithms using the “stream of successes” technique, and they can be used to model
;;; generators and co-routines. The implicit memoization of streams makes them useful for
;;; building persistent data structures, and the laziness of streams permits some
;;; multi-pass algorithms to be executed in a single pass. Savvy programmers use streams
;;; to enhance their programs in countless ways.
;;;
;;; There is an obvious space/time trade-off between lists and streams; lists take more
;;; space, but streams take more time. Streams are appropriate when the sequence is truly
;;; infinite, when the space savings are needed, or when they offer a clearer exposition
;;; of the algorithms that operate on the sequence.
;;;
;;;
;;; Author of the SRFI 41 spec and the rationale provided above: Philip L. Bewig
;;; Copyright © 2007 Philip L. Bewig. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify,
;;; merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;;
;;; Author of the example code below: Matthias Zenger
;;; Copyright © 2019 Matthias Zenger. All rights reserved.
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

(import (lispkit base)
        (lispkit stream))

;; Infinite stream of odd numbers, generated via procedure `stream-from`.
(define odd-stream (stream-from 1 2))

;; Infinite stream of integers representing all powers of two, generated via procedure
;; `stream-iterate`.
(define two-power-stream
  (stream-iterate (lambda (x) (* x 2)) 1))

;; Infinite stream of all Fibonacci numbers, recursively defined via `stream-let`.
(define fib-stream
  (stream-let fib ((a 0) (b 1))
    (stream-cons a (fib b (+ a b)))))

;; Infinite stream of all prime numbers, recursively defined via a stream generator
;; `sieve` which uses `stream-filter` to remove all multiples of a prime number.
(define prime-stream
  (stream-let sieve ((strm (stream-from 2)))
    (stream-cons
      (stream-car strm)
      (sieve (stream-filter
               (lambda (x) (not (= (remainder x (stream-car strm)) 0)))
               (stream-cdr strm))))))

;; Procedure for scaling up/down values of a stream `str` by `factor`.
(define (stream-scale strm factor)
  (stream-map (lambda (x) (* x factor)) strm))

;; Procedure for merging two sorted streams `strm1` and strm2` into one.
(define-stream (stream-merge strm1 strm2)
  (cond ((stream-null? strm1) strm2)
        ((stream-null? strm2) strm1)
        (else
          (let ((x1 (stream-car strm1))
                (x2 (stream-car strm2)))
            (cond ((< x1 x2)
                    (stream-cons x1 (stream-merge (stream-cdr strm1) strm2)))
                  ((> x1 x2)
                    (stream-cons x2 (stream-merge strm1 (stream-cdr strm2))))
                  (else
                    (stream-cons x1 (stream-merge (stream-cdr strm1) (stream-cdr strm2)))))))))

;; Infinite stream of all hamming numbers, recursively defined using merging and scaling
;; of streams.
(define hamming-stream
  (stream-cons 1 (stream-merge (stream-merge (stream-scale hamming-stream 2)
                                             (stream-scale hamming-stream 3))
                               (stream-scale hamming-stream 5))))

(define (sqrt-improve guess x)
  (/ (+ guess (/ x guess)) 2))

;; Infinite stream converging towards the square root of `x`.
(define (sqrt-stream x)
  (define guesses (stream-cons 1.0 (stream-map (lambda (guess) (sqrt-improve guess x)) guesses)))
  guesses)

(define-stream (partial-sums strm)
  (stream-cdr (stream-scan (lambda (base x) (+ base x)) 0 strm)))

(define-stream (pi-summands n)
  (stream-cons (/ 1.0 n) (stream-map - (pi-summands (+ n 2)))))

;; Infinite stream converging towards pi.
(define pi-stream
  (stream-scale (partial-sums (pi-summands 1)) 4))

;; Example printing the first 100 prime numbers

(display "The first 100 prime numbers: ")
(display-stream prime-stream 100)
(newline)
