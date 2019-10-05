;;; LISPKIT COMPARATOR
;;;
;;; This library implements comparators which bundle a type test predicate, an equality
;;; predicate, an optional ordering predicate, and a optional hash function into a
;;; single object. By packaging these procedures together, they can be treated as a single
;;; parameter for use in the implementation of data structures.
;;;
;;; This library implements a subset of SRFI 128. While the implementation differs, it is
;;; compatible to the corresponding SRFI 128 spec.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2019 Matthias Zenger. All rights reserved.
;;;
;;;
;;; Some portions of this code are from the reference implementation of SRFI 128:
;;; Copyright © 2015-2019 John Cowan. All Rights Reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

(define-library (lispkit comparator)

  (export comparator?
          comparator-ordered?
          comparator-hashable?
          comparator-type-test-predicate
          comparator-equality-predicate
          comparator-ordering-predicate
          comparator-hash-function
          comparator-test-type
          comparator-check-type
          comparator-hash
          comparator-max
          comparator-min
          comparator-max-in-list
          comparator-min-in-list
          make-comparator
          make-pair-comparator
          make-list-comparator
          make-vector-comparator
          comparator-if<=>
          if<=>
          =?
          <?
          >?
          <=?
          >=?
          boolean-comparator
          real-comparator
          char-comparator
          char-ci-comparator
          string-comparator
          string-ci-comparator
          eq-comparator
          eqv-comparator
          equal-comparator)

  (import (lispkit base))

  (begin

    (define-record-type comparator
      (make-raw-comparator type-test equality ordering hash ordering? hash?)
      comparator?
      (type-test comparator-type-test-predicate)
      (equality comparator-equality-predicate)
      (ordering comparator-ordering-predicate)
      (hash comparator-hash-function)
      (ordering? comparator-ordered?)
      (hash? comparator-hashable?))

    (define (make-comparator type-test equality ordering hash)
      (make-raw-comparator
        (if (eq? type-test #t) (lambda (x) #t) type-test)
        (if (eq? equality #t) (lambda (x y) (eqv? (ordering x y) 0)) equality)
        (if ordering ordering (lambda (x y) (error "ordering not supported")))
        (if hash hash (lambda (x y) (error "hashing not supported")))
        (if ordering #t #f)
        (if hash #t #f)))

    (define (comparator-test-type comparator obj)
      ((comparator-type-test-predicate comparator) obj))

    (define (comparator-check-type comparator obj)
      (if (comparator-test-type comparator obj)
          #t
          (error "comparator type check failed" comparator obj)))

    (define (comparator-hash comparator obj)
      ((comparator-hash-function comparator) obj))

    (define (comparator-max-in-list comp list)
      (let ((< (comparator-ordering-predicate comp)))
        (let loop ((max (car list)) (list (cdr list)))
          (if (null? list)
            max
            (if (< max (car list))
              (loop (car list) (cdr list))
              (loop max (cdr list)))))))

    (define (comparator-min-in-list comp list)
      (let ((< (comparator-ordering-predicate comp)))
        (let loop ((min (car list)) (list (cdr list)))
          (if (null? list)
            min
            (if (< min (car list))
              (loop min (cdr list))
              (loop (car list) (cdr list)))))))

    (define (comparator-max comp . args)
      (comparator-max-in-list comp args))

    (define (comparator-min comp . args)
      (comparator-min-in-list comp args))

    (define (make-pair-comparator car-comparator cdr-comparator)
       (make-comparator (make-pair-type-test car-comparator cdr-comparator)
                        (make-pair=? car-comparator cdr-comparator)
                        (make-pair<? car-comparator cdr-comparator)
                        (make-pair-hash car-comparator cdr-comparator)))

    (define (make-pair-type-test car-comparator cdr-comparator)
      (lambda (obj)
        (and (pair? obj)
             (comparator-test-type car-comparator (car obj))
             (comparator-test-type cdr-comparator (cdr obj)))))

    (define (make-pair=? car-comparator cdr-comparator)
       (lambda (a b)
         (and ((comparator-equality-predicate car-comparator) (car a) (car b))
              ((comparator-equality-predicate cdr-comparator) (cdr a) (cdr b)))))

    (define (make-pair<? car-comparator cdr-comparator)
       (lambda (a b)
          (if (=? car-comparator (car a) (car b))
              (<? cdr-comparator (cdr a) (cdr b))
              (<? car-comparator (car a) (car b)))))

    (define (make-pair-hash car-comparator cdr-comparator)
      (combine-hash (comparator-hash car-comparator (car obj))
                    (comparator-hash cdr-comparator (cdr obj))))

    (define (make-list-comparator element-comparator type-test empty? . args)
      (let-optionals args ((head car)
                           (tail cdr))
        (make-comparator (make-list-type-test element-comparator type-test empty? head tail)
                         (make-list=? element-comparator type-test empty? head tail)
                         (make-list<? element-comparator type-test empty? head tail)
                         (make-list-hash element-comparator type-test empty? head tail))))

    (define (make-list-type-test element-comparator type-test empty? head tail)
      (lambda (obj)
        (and
          (type-test obj)
          (let ((elem-type-test (comparator-type-test-predicate element-comparator)))
            (let loop ((obj obj))
              (cond ((empty? obj)                      #t)
                    ((not (elem-type-test (head obj))) #f)
                    (else                              (loop (tail obj)))))))))

    (define (make-list=? element-comparator type-test empty? head tail)
      (lambda (a b)
        (let ((elem=? (comparator-equality-predicate element-comparator)))
          (let loop ((a a) (b b))
            (cond ((and (empty? a) (empty? b) #t))
                  ((empty? a)                 #f)
                  ((empty? b)                 #f)
                  ((elem=? (head a) (head b)) (loop (tail a) (tail b)))
                  (else                       #f))))))

    (define (make-list<? element-comparator type-test empty? head tail)
      (lambda (a b)
        (let ((elem=? (comparator-equality-predicate element-comparator))
              (elem<? (comparator-ordering-predicate element-comparator)))
          (let loop ((a a) (b b))
            (cond ((and (empty? a) (empty? b)) #f)
                  ((empty? a)                  #t)
                  ((empty? b)                  #f)
                  ((elem=? (head a) (head b))  (loop (tail a) (tail b)))
                  ((elem<? (head a) (head b))  #t)
                  (else                        #f))))))

    (define (make-list-hash element-comparator type-test empty? head tail)
      (lambda (obj)
        (let ((hash (comparator-hash-function element-comparator)))
          (let loop ((obj obj)
                     (buf '())
                     (n 100))
            (cond ((empty? obj)  (apply combine-hash buf))
                  ((fxzero? n)   (loop obj (list (apply combine-hash buf)) 100))
                  (else          (loop (tail obj) (cons (hash (head obj)) buf) (fx1- n))))))))

    (define (make-vector-comparator element-comparator type-test . args)
      (let-optionals args ((length vector-length)
                           (ref vector-ref))
        (make-comparator
          (make-vector-type-test element-comparator type-test length ref)
          (make-vector=? element-comparator type-test length ref)
          (make-vector<? element-comparator type-test length ref)
          (make-vector-hash element-comparator type-test length ref))))

    (define (make-vector-type-test element-comparator type-test length ref)
      (lambda (obj)
        (and
          (type-test obj)
          (let ((elem-type-test (comparator-type-test-predicate element-comparator))
                (len (length obj)))
            (let loop ((n 0))
              (cond
                ((= n len)                          #t)
                ((not (elem-type-test (ref obj n))) #f)
                (else                               (loop (+ n 1)))))))))

    (define (make-vector=? element-comparator type-test length ref)
       (lambda (a b)
         (and
           (= (length a) (length b))
           (let ((elem=? (comparator-equality-predicate element-comparator))
                 (len (length b)))
             (let loop ((n 0))
               (cond ((= n len)                    #t)
                     ((elem=? (ref a n) (ref b n)) (loop (+ n 1)))
                     (else                         #f)))))))

    (define (make-vector<? element-comparator type-test length ref)
       (lambda (a b)
         (cond
           ((< (length a) (length b)) #t)
           ((> (length a) (length b)) #f)
            (else
             (let ((elem=? (comparator-equality-predicate element-comparator))
                   (elem<? (comparator-ordering-predicate element-comparator))
                   (len    (length a)))
             (let loop ((n 0))
               (cond ((= n len)                    #f)
                     ((elem=? (ref a n) (ref b n)) (loop (+ n 1)))
                     ((elem<? (ref a n) (ref b n)) #t)
                     (else                         #f))))))))

    (define (make-vector-hash element-comparator type-test length ref)
      (lambda (obj)
        (let ((elem-hash (comparator-hash-function element-comparator))
              (len (length obj)))
          (let loop ((i 0)
                     (buf '())
                     (n 100))
            (cond ((fx= i len) (apply combine-hash buf))
                  ((fxzero? n) (loop i (list (apply combine-hash buf)) 100))
                  (else        (loop (fx1+ i) (cons (hash (ref obj i)) buf) (fx1- n))))))))

    (define (binary=? comparator a b)
      ((comparator-equality-predicate comparator) a b))

    (define (binary<? comparator a b)
      ((comparator-ordering-predicate comparator) a b))

    (define (binary>? comparator a b)
      (binary<? comparator b a))

    (define (binary<=? comparator a b)
      (not (binary>? comparator a b)))

    (define (binary>=? comparator a b)
      (not (binary<? comparator a b)))

    (define (=? comparator a b . objs)
      (let loop ((a a) (b b) (objs objs))
        (and (binary=? comparator a b)
             (if (null? objs) #t (loop b (car objs) (cdr objs))))))

    (define (<? comparator a b . objs)
      (let loop ((a a) (b b) (objs objs))
        (and (binary<? comparator a b)
             (if (null? objs) #t (loop b (car objs) (cdr objs))))))

    (define (>? comparator a b . objs)
      (let loop ((a a) (b b) (objs objs))
        (and (binary>? comparator a b)
             (if (null? objs) #t (loop b (car objs) (cdr objs))))))

    (define (<=? comparator a b . objs)
      (let loop ((a a) (b b) (objs objs))
        (and (binary<=? comparator a b)
             (if (null? objs) #t (loop b (car objs) (cdr objs))))))

    (define (>=? comparator a b . objs)
      (let loop ((a a) (b b) (objs objs))
        (and (binary>=? comparator a b)
             (if (null? objs) #t (loop b (car objs) (cdr objs))))))

    (define boolean-comparator
      (make-comparator boolean? boolean=? (lambda (x y) (and (not x) y)) boolean-hash))

    (define real-comparator
      (make-comparator real? = < number-hash))

    (define char-comparator
      (make-comparator char? char=? char<? char-hash))

    (define char-ci-comparator
      (make-comparator char? char-ci=? char-ci<? char-ci-hash))

    (define string-comparator
      (make-comparator string? string=? string<? string-hash))

    (define string-ci-comparator
      (make-comparator string? string-ci=? string-ci<? string-ci-hash))

    (define (less eq)
      (define (cmp a b)
        (cond ((null? a)    (pair? b))
              ((complex? a) (if (= (real-part a) (real-part b))
                                (< (imag-part a) (imag-part b))
                                (< (real-part a) (real-part b))))
              ((boolean? a) (and (not a) b))
              ((number? a)  (< a b))
              ((symbol? a)  (string<? (symbol->string a) (symbol->string b)))
              ((string? a)  (string<? a b))
              ((pair? a)    (cond ((null? b)            #f)
                                  ((eq (car a) (car b)) (cmp (cdr a) (cdr b)))
                                  (else                 (cmp (car a) (car b)))))
              ((vector? a)  (cond
                              ((< (vector-length a) (vector-length b)) #t)
                              ((> (vector-length a) (vector-length b)) #f)
                              (else
                                (let ((len (vector-length a)))
                                (let loop ((n 0))
                                  (cond ((= n len)                               #f)
                                        ((eq (vector-ref a n) (vector-ref b n))  (loop (+ n 1)))
                                        ((cmp (vector-ref a n) (vector-ref b n)) #t)
                                        (else                                    #f)))))))
              (else (error "cannot compare $0 and $1" a b))))
      cmp)

    (define eq-comparator
      (make-comparator #t eq? (less eq?) eq-hash))

    (define eqv-comparator
      (make-comparator #t eqv? (less eqv?) eqv-hash))

    (define equal-comparator
      (make-comparator #t equal? (less equal?) equal-hash))

    (define-syntax comparator-if<=>
      (syntax-rules ()
        ((_ a b less equal greater)
          (comparator-if<=> equal-comparator a b less equal greater))
        ((_ comparator a b less equal greater)
          (cond ((=? comparator a b) equal)
                ((<? comparator a b) less)
                (else                greater)))))

    (define-syntax if<=>
      (syntax-rules ()
        ((if<=> a b less equal greater)
          (comparator-if<=> equal-comparator a b less equal greater))))
  )
)

