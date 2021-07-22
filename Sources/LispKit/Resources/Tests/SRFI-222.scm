;;; SRFI 222 REGRESSION TEST SUITE
;;;
;;; This is the test suite for SRFI 222.
;;;
;;; Copyright © 2021 Arvydas Silanskas. All rights reserved.
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
        (lispkit test)
        (srfi 222))

(define (test-compound-equal c1 c2)
  (test-equal
    (compound-subobjects c1)
    (compound-subobjects c2)))

(test-begin "SRFI 222: Generator/accumulator sub-library")

(test-group "make-compound, compound?, compound-subobjects"
  (define (test c)
    (test-assert (compound? c))
    (test-equal (compound-subobjects c) '(1 2 3)))
  (test (make-compound 1 2 3))
  (test (make-compound (make-compound 1) (make-compound 2) 3))

  (test-assert (not (compound? (list '(1 2 3))))))

(test-group "compound-length"
  (test-equal 3 (compound-length (make-compound 1 2 3)))
  (test-equal 1 (compound-length 'test)))

(test-group "compound-ref"
  (test-equal 1 (compound-ref (make-compound 1 2 3) 0))
  (test-equal 1 (compound-ref 1 0)))

(test-group "compound-map"
  (define c (make-compound 1 2 3))

  (test-compound-equal
    (make-compound 2 3 4)
    (compound-map (lambda (e) (+ 1 e)) c))

  (test-compound-equal
    (make-compound 0 2 0 3 0 4)
    (compound-map (lambda (e) (make-compound 0 (+ 1 e))) c))

  (test-compound-equal
    (make-compound 2)
    (compound-map (lambda (e) (+ 1 e)) 1)))

(test-group "compound-map->list"
  (define c (make-compound 1 2 3))
  (test-equal
    (compound-map->list (lambda (e) (+ 1 e)) c)
    (list 2 3 4))
  (test-equal
    (compound-map->list (lambda (e) (+ 1 e)) 1)
    (list 2)))

(test-group "compound-filter"
  (define c (make-compound 1 2 3))
  (test-compound-equal
    (compound-filter (lambda (e) (= e 2)) c)
    (make-compound 2))
  (test-compound-equal
    (compound-filter (lambda (e) (= e 2)) 2)
    (make-compound 2))
  (test-compound-equal
    (compound-filter (lambda (e) (= e 2)) 1)
    (make-compound)))

(test-group "compound-predicate"
  (define c1 (make-compound 1 2))
  (define c2 (make-compound 3 4))
  (define (pred1 obj)
    (equal? obj 'a))
  (define (pred3 obj)
    (equal? obj 1))
  (test-assert (compound-predicate pred1 'a))
  (test-assert (not (compound-predicate pred1 c1)))
  (test-assert (not (compound-predicate pred1 c2)))
  (test-assert (not (compound-predicate pred3 'a)))
  (test-assert (compound-predicate pred3 c1))
  (test-assert (not (compound-predicate pred3 c2))))

(test-group "compound-access"
  (define (pred obj)
    (and (number? obj) (= obj 2)))
  (define (accessor obj)
    (+ 1 obj))
  (test-equal
    (compound-access pred accessor 0 (make-compound 1 2 3))
    3)
  (test-equal
    (compound-access pred accessor 0 (make-compound 1 3))
    0)
  (test-equal
    (compound-access pred accessor 0 1)
    0)
  (test-equal
    (compound-access pred accessor 0 2)
    3))

(test-group "examples in spec"

  ;; The following definitions are referenced in later examples
  (define-record-type <student>
    (student admission-year gpa)
    student?
    (admission-year admission-year)
    (gpa gpa))       ; grade-point average

  (define-record-type <teacher>
    (teacher hire-year salary)
    teacher?
    (hire-year hire-year)     ; integer year
    (salary salary))  ; annualized

  (define alyssa (student 1986 4.0))

  (define guy (teacher 1981 25000))

  ;; These definitions are referenced in later examples
  (define george
    (make-compound
      'teaching-assistant
      (student 1979 3.8)
      (teacher 1983 1000)))

  (define (teaching-assistant? obj)
    (eq? obj 'teaching-assistant))

  (test-assert (not (compound? alyssa)))
  (let ((subobjs (compound-subobjects alyssa)))
    (test-assert (list? subobjs))
    (test-equal 1 (length subobjs))
    (test-assert (student? (car subobjs)))
    (test-equal 1986 (admission-year (car subobjs)))
    (test-equal 4.0 (gpa (car subobjs))))
  (let ((subobjs (compound-subobjects george)))
    (test-assert (list? subobjs))
    (test-equal 3 (length subobjs))
    (test-equal 'teaching-assistant (car subobjs))
    (test-assert (student? (cadr subobjs)))
    (test-equal 1979 (admission-year (cadr subobjs)))
    (test-equal 3.8 (gpa (cadr subobjs)))
    (test-assert (teacher? (list-ref subobjs 2)))
    (test-equal 1983 (hire-year (list-ref subobjs 2)))
    (test-equal 1000 (salary (list-ref subobjs 2))))

  (test-equal 1 (compound-length alyssa))

  (test-assert (student? (compound-ref alyssa 0)))
  (test-assert (teacher? (compound-ref george 2)))

  (test-equal
    '(-1 -2 -3 -4 -5)
    (compound-subobjects (compound-map - (make-compound 1 2 3 4 5))))
  (test-equal
    '(-1 -2 -3 -4 -5)
    (compound-map->list - (make-compound 1 2 3 4 5)))

  (test-equal
    '()
    (compound-subobjects (compound-filter teacher? alyssa)))
  (let ((subobjs (compound-subobjects (compound-filter teacher? george))))
    (test-equal 1 (length subobjs))
    (test-assert (teacher? (car subobjs))))

  (test-assert (compound-predicate student? alyssa))
  (test-assert (compound-predicate student? george))
  (test-assert (compound-predicate teacher? george))
  (test-assert (compound-predicate teacher? guy))
  (test-assert (not (compound-predicate teaching-assistant? alyssa)))
  (test-assert (not (compound-predicate teaching-assistant? guy)))
  (test-assert (compound-predicate teaching-assistant? george))

  (let ()
    (define (uni-member-hire-year obj)
      (compound-access teacher? hire-year #f obj))

    (test-equal #f (uni-member-hire-year alyssa))
    (test-equal 1981 (uni-member-hire-year guy))
    (test-equal 1983 (uni-member-hire-year george))
    (test-equal #f (uni-member-hire-year (make-compound '(27 42 98) 'fire!)))))

(test-end)
