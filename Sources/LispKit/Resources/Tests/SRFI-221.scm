;;; SRFI 221 REGRESSION TEST SUITE
;;;
;;; This is the test suite for SRFI 221.
;;;
;;; Copyright © 2020 Arvydas Silanskas. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2021 Matthias Zenger. All rights reserved.

(import (lispkit base)
        (except (lispkit test) test-equal)
        (srfi 41)
        (srfi 158)
        (srfi 221))

(define-syntax test-equal
  (syntax-rules ()
    ((_ args ...) (test args ...))))

(define test-g-equal
  (case-lambda
    ((g-expect g-actual)
     (test-equal
       (generator->list g-expect)
       (generator->list g-actual)))
    ((g-expect g-actual take-count)
     (test-g-equal
       (gtake g-expect take-count)
       (gtake g-actual take-count)))))

(test-begin "SRFI 221: Generator/accumulator sub-library")

(test-group
  "accumulate-generated-values"
  (define expect '(1 2 3 4))
  (define actual
    (accumulate-generated-values
      (list-accumulator)
      (generator 1 2 3 4)))
  (test-equal expect actual))

(test-group
  "genumerate"

  ;; test normal case
  (test-g-equal (generator '(0 . a) '(1 . b) '(2 . c))
                (genumerate (generator 'a 'b 'c)))

  ;; test empty
  (test-g-equal (generator)
                (genumerate (generator)))

  ;; infinite case with take
  (test-g-equal (generator '(0 . a) '(1 . b) '(2 . c))
                (genumerate (circular-generator 'a 'b 'c))
                3)
 )

(test-group
  "gcompose-left"

  (test-g-equal
    (generator 1 2 3 4)
    (gcompose-left
      (lambda () (make-range-generator 1))
      (lambda (g) (gtake g 4))))

  (test-g-equal
    (generator 1 2 3 4)
    (gcompose-left
      (lambda () (generator 1 2 3 4)))))

(test-group
  "gcompose-right"

  (test-g-equal
    (generator 1 2 3 4)
    (gcompose-right
      (lambda (g) (gtake g 4))
      (lambda () (make-range-generator 1))))

  (test-g-equal
    (generator 1 2 3 4)
    (gcompose-right
      (lambda () (generator 1 2 3 4)))))

(test-group
  "gchoice"

  ;; test normal
  (test-g-equal
    (generator 1 2 1 3)
    (gchoice
      (generator 0 1 0 2)
      (circular-generator 1)
      (circular-generator 2)
      (circular-generator 3)))

  ;; test exhausted source
  (test-g-equal
    (generator 1 2 3)
    (gchoice
      (generator 0 0 0 0 0 1 1 2)
      (generator 1)
      (generator 2)
      (generator 3)))

  ;; test exhausted source (choice generator won't be exhausted)
  (test-g-equal
   (generator 1 3 5 2 4 6)
   (gchoice
    (make-unfold-generator (lambda (_) #f)
                           (lambda (x) (modulo x 3))
                           (lambda (x) (+ x 1))
                           0)
    (generator 1 2)
    (generator 3 4)
    (generator 5 6))))

(test-group "generator->stream"
  
  (define (test-stream-equal str1 str2)
    (if (stream-null? str1)
        (test-assert (stream-null? str2))
        (begin
          (test-equal (stream-car str1) (stream-car str2))
          (test-stream-equal (stream-cdr str1) (stream-cdr str2)))))

  ;; test normal
  (test-stream-equal
    (stream 1 2 3)
    (generator->stream (generator 1 2 3)))

  ;; test infinite with take
  (test-stream-equal
    (stream 1 2 3)
    (stream-take 3 (generator->stream (circular-generator 1 2 3)))))

(test-group
  "stream->generator"
  ;; test normal
  (test-g-equal
    (generator 1 2 3)
    (stream->generator (stream 1 2 3)))

  ;; test infinite with take
  (test-g-equal
    (circular-generator 1 2 3)
    (stream->generator (stream-constant 1 2 3))
    20))

(test-end)

