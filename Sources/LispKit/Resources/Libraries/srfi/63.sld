;;; SRFI 63
;;; Homogeneous and Heterogeneous Arrays
;;;
;;; This SRFI supersedes SRFI 47, "Array". It has the following objectives:
;;;   - Synthesizes array concepts from Common-Lisp and Alan Bawden's "array.scm",
;;;   - Incorporates all the uniform vector types from SFRI-4 "Homogeneous numeric
;;;     vector datatypes",
;;;   - Adds a boolean uniform array type,
;;;   - Adds 16.bit and 128.bit floating-point uniform-array types,
;;;   - Adds decimal floating-point uniform-array types, and
;;;   - Adds array types of (dual) floating-point complex numbers.
;;;
;;; Multi-dimensional arrays subsume homogeneous vectors as the one-dimensional case,
;;; obviating the need for SRFI 4.
;;;
;;; "array.scm" Arrays for Scheme
;;; Copyright © 2001, 2003 Aubrey Jaffer
;;;
;;; Permission to copy this software, to modify it, to redistribute it,
;;; to distribute modified versions, and to use it for any purpose is
;;; granted, subject to the following restrictions and understandings.
;;;
;;; 1. Any copy made of this software must include this copyright notice in full.
;;;
;;; 2. I have made no warranty or representation that the operation of this software
;;;    will be error-free, and I am under no obligation to provide any services, by
;;;    way of maintenance, update, or otherwise.
;;;
;;; 3.  In conjunction with products arising from the use of this material, there
;;;     shall be no use of my name in any advertising, promotional, or sales literature
;;;     without prior written consent in each case.
;;;
;;; Adaptation to LispKit
;;;   Copyright © 2017 Matthias Zenger. All rights reserved.

(define-library (srfi 63)
  (export array?
          equal?
          array-rank
          array-dimensions
          make-array
          make-shared-array
          list->array
          array->list
          vector->array
          array->vector
          array-in-bounds?
          array-ref
          array-set!
          a:floc128b
          a:floc64b
          a:floc32b
          a:floc16b
          a:flor128b
          a:flor64b
          a:flor32b
          a:flor16b
          a:fixz64b
          a:fixz32b
          a:fixz16b
          a:fixz8b
          a:fixn64b
          a:fixn32b
          a:fixn16b
          a:fixn8b
          a:bool)

  (import (except (lispkit base) equal?))

  (begin

    (define-record-type <array>
      (array:construct dimensions scales offset store)
      array:array?
      (dimensions dimensions)
      (scales scales)
      (offset offset)
      (store store))

    ;; Returns a list of dimensions for the given array.
    ;;
    ;; Example:
    ;;   (array-dimensions (make-array '#() 3 5))
    ;;   ==> (3 5)
    (define (array-dimensions array)
      (cond ((vector? array) (list (vector-length array)))
            ((string? array) (list (string-length array)))
            (else            (dimensions array))))

    (define (array:scales array)
      (cond ((vector? array) (list 1))
            ((string? array) (list 1))
            (else            (scales array))))

    (define (array:store array)
      (cond ((vector? array) array)
            ((string? array) array)
            (else            (store array))))

    (define (array:offset array)
      (cond ((vector? array) 0)
            ((string? array) 0)
            (else            (offset array))))

    ;; Returns #t if `obj` is an array, and #f otherwise.
    (define (array? obj)
      (or (vector? obj) (string? obj) (array:array? obj)))

    ;; Returns #t if `obj1` and `obj2` have the same rank and dimensions and the
    ;; corresponding elements of `obj1` and `obj2` are `equal?`.
    ;; This function recursively compares the contents of pairs, vectors, strings, and
    ;; arrays, applying `eqv?` on other objects such as numbers and symbols. A rule of
    ;; thumb is that objects are generally `eual?` if they print the same. `equal?` may
    ;; fail to terminate if its arguments are circular data structures.
    ;;
    ;; Note: Arrays are not disjoint from other Scheme types. Vectors and possibly strings
    ;; also satisfy `array?`. A disjoint array predicate can be written:
    ;;
    ;; (define (strict-array? obj)
    ;;   (and (array? obj) (not (string? obj)) (not (vector? obj))))
    ;;
    ;; Examples:
    ;;   (equal? 'a 'a)                                   ==> #t
    ;;   (equal? '(a) '(a))                               ==> #t
    ;;   (equal? '(a (b) c) '(a (b) c))                   ==> #t
    ;;   (equal? "abc" "abc")                             ==> #t
    ;;   (equal? 2 2)                                     ==> #t
    ;;   (equal? (make-vector 5 'a) (make-vector 5 'a))   ==> #t
    ;;   (equal? (make-array (a:fixN32b 4) 5 3)
    ;;           (make-array (a:fixN32b 4) 5 3))          ==> #t
    ;;   (equal? (make-array '#(foo) 3 3)
    ;;           (make-array '#(foo) 3 3))                ==> #t
    ;;   (equal? (lambda (x) x) (lambda (y) y)).          ==> ? (undefined)
    (define (equal? obj1 obj2)
      (cond ((eqv? obj1 obj2)
              #t)
            ((or (pair? obj1) (pair? obj2))
              (and (pair? obj1) (pair? obj2)
                   (equal? (car obj1) (car obj2))
                   (equal? (cdr obj1) (cdr obj2))))
            ((or (string? obj1) (string? obj2))
              (and (string? obj1) (string? obj2)
                   (string=? obj1 obj2)))
            ((or (vector? obj1) (vector? obj2))
              (and (vector? obj1) (vector? obj2)
                   (equal? (vector-length obj1) (vector-length obj2))
                   (do ((idx (+ -1 (vector-length obj1)) (+ -1 idx)))
                       ((or (negative? idx)
                            (not (equal? (vector-ref obj1 idx)
                                         (vector-ref obj2 idx))))
                        (negative? idx)))))
            ((or (array? obj1) (array? obj2))
              (and (array? obj1) (array? obj2)
                   (equal? (array-dimensions obj1) (array-dimensions obj2))
                   (equal? (array:store obj1) (array:store obj2))))
            (else
              #f)))

    ;; Returns the number of dimensions of `obj`.  If `obj` is not an array, 0 is
    ;; returned.
    (define (array-rank obj)
      (if (array? obj) (length (array-dimensions obj)) 0))

    ;; Creates and returns an array of type `prototype` with dimensions `dimensions`
    ;; and filled with elements from `prototype`. `prototype` must be an array, vector,
    ;; or string. The implementation-dependent type of the returned array will be the
    ;; same as the type of `prototype`, except if that would be a vector or string with
    ;; rank not equal to one, in which case some variety of array will be returned.
    ;;
    ;; If argument `prototype` has no elements, then the initial contents of the returned
    ;; array are unspecified. Otherwise, the returned array will be filled with the
    ;; element at the origin of `prototype`.
    (define (make-array prototype . dimensions)
      (let* ((tcnt (apply * dimensions))
             (store (if (string? prototype)
                        (case (string-length prototype)
                          ((0)  (make-string tcnt))
                          (else (make-string tcnt (string-ref prototype 0))))
                        (let ((pdims (array-dimensions prototype)))
                          (case (apply * pdims)
                            ((0)  (make-vector tcnt))
                            (else (make-vector tcnt
                                               (apply array-ref prototype
                                                      (map (lambda (x) 0) pdims)))))))))
        (define (loop dims scales)
          (if (null? dims)
              (array:construct dimensions (cdr scales) 0 store)
              (loop (cdr dims) (cons (* (car dims) (car scales)) scales))))
        (loop (reverse dimensions) (list 1))))

    ;; `make-shared-array` can be used to create shared subarrays of other
    ;; arrays. `mapper` is a function that translates coordinates in the new array into
    ;; coordinates in the old array.  A `mapper` must be linear, and its range must stay
    ;; within the bounds of the old array, but it can be otherwise arbitrary.
    ;;
    ;; Examples:
    ;;   (define fred (make-array '#(#f) 8 8))
    ;;   (define freds-diagonal (make-shared-array fred (lambda (i) (list i i)) 8))
    ;;   (array-set! freds-diagonal 'foo 3)
    ;;   (array-ref fred 3 3)                    ==> foo
    ;;   (define freds-center
    ;;     (make-shared-array fred (lambda (i j) (list (+ 3 i) (+ 3 j))) 2 2))
    ;;   (array-ref freds-center 0 0).           ==> foo
    (define (make-shared-array array mapper . dimensions)
      (let ((odl   (array:scales array))
            (rank  (length dimensions))
            (shape (map (lambda (dim) (if (list? dim) dim (list 0 (- dim 1)))) dimensions)))
        (do ((idx (+ -1 rank) (+ -1 idx))
             (uvt (append (cdr (vector->list (make-vector rank 0))) (list 1))
                  (append (cdr uvt) '(0)))
             (uvts '() (cons uvt uvts)))
            ((negative? idx)
             (let ((ker0 (apply + (map * odl (apply mapper uvt)))))
               (array:construct
                 (map (lambda (dim) (+ 1 (- (cadr dim) (car dim)))) shape)
                 (map (lambda (uvt) (- (apply + (map * odl (apply mapper uvt))) ker0)) uvts)
                 (apply + (array:offset array) (map * odl (apply mapper (map car shape))))
                 (array:store array)))))))

    ;; `list->array` returns an array of rank `rank` and type `proto` consisting of all
    ;; the elements, in row-major order, of `lst`. When `rank` is 0, `lst` is the lone
    ;; array element; not necessarily a list.
    ;; `lst` must be a rank-nested list consisting of all the elements, in
    ;; row-major order, of the array to be created.
    ;;
    ;; Examples:
    ;;   (list->array 2 '#() '((1 2) (3 4)))        ==> #2A((1 2) (3 4))
    ;;   (list->array 0 '#() 3)                     ==> #0A 3
    (define (list->array rank proto lst)
      (let* ((dimensions (do ((shp '() (cons (length row) shp))
                              (row lst (car lst))
                              (rnk (+ -1 rank) (+ -1 rnk)))
                             ((negative? rnk) (reverse shp))))
             (nra (apply make-array proto dimensions)))
        (define (l2ra dims idxs row)
          (cond ((null? dims)
                 (apply array-set! nra row (reverse idxs)))
                (else
                 (if (not (eqv? (car dims) (length row)))
                     (error "Array not rectangular:" dims dimensions))
                 (do ((idx 0 (+ 1 idx))
                      (row row (cdr row)))
                     ((>= idx (car dims)))
                   (l2ra (cdr dims) (cons idx idxs) (car row))))))
        (l2ra dimensions '() lst)
        nra))

    ;; Returns a rank-nested list consisting of all the elements, in row-major order,
    ;; of `ra`. In the case of a rank-0 array, `array->list` returns the single element.
    ;;
    ;; Examples:
    ;;   (array->list #2A((ho ho ho) (ho oh oh)))    ==> ((ho ho ho) (ho oh oh))
    ;;   (array->list #0A ho)                        ==> ho
    (define (array->list ra)
      (define (ra2l dims idxs)
        (if (null? dims)
            (apply array-ref ra (reverse idxs))
            (do ((lst '() (cons (ra2l (cdr dims) (cons idx idxs)) lst))
                 (idx (+ -1 (car dims)) (+ -1 idx)))
                ((negative? idx) lst))))
      (ra2l (array-dimensions ra) '()))

    ;; `vector->array` returns an array of type @2 consisting of all the elements, in
    ;; row-major order, of @1.  In the case of a rank-0 array, @1 has a single element.
    ;; `vect` must be a vector of length equal to the product of exact nonnegative
    ;; integers `dimensions`.
    ;;
    ;; Examples:
    ;;   (vector->array #(1 2 3 4) #() 2 2)     ==> #2A((1 2) (3 4))
    ;;   (vector->array '#(3) '#())             ==> #0A 3
    (define (vector->array vect prototype . dimensions)
      (let ((vdx (vector-length vect)))
        (if (not (eqv? vdx (apply * dimensions)))
            (error "Vector length does not equal product of dimensions:"
                   vdx dimensions))
        (let ((ra (apply make-array prototype dimensions)))
          (define (v2ra dims idxs)
            (cond ((null? dims)
                   (set! vdx (+ -1 vdx))
                   (apply array-set! ra (vector-ref vect vdx) (reverse idxs)))
                  (else
                   (do ((idx (+ -1 (car dims)) (+ -1 idx)))
                       ((negative? idx) vect)
                     (v2ra (cdr dims) (cons idx idxs))))))
          (v2ra dimensions '())
          ra)))

    ;; Returns a new vector consisting of all the elements of `ra` in row-major order.
    ;;
    ;; Examples:
    ;;   (array->vector #2A ((1 2)( 3 4)))      ==> #(1 2 3 4)
    ;;   (array->vector #0A ho)                 ==> #(ho)
    (define (array->vector ra)
      (let* ((dims (array-dimensions ra))
             (vdx  (apply * dims))
             (vect (make-vector vdx)))
        (define (ra2v dims idxs)
          (if (null? dims)
              (let ((val (apply array-ref ra (reverse idxs))))
                (set! vdx (+ -1 vdx))
                (vector-set! vect vdx val)
                vect)
              (do ((idx (+ -1 (car dims)) (+ -1 idx)))
                  ((negative? idx) vect)
                (ra2v (cdr dims) (cons idx idxs)))))
        (ra2v dims '())))

    (define (array:in-bounds? array indices)
      (do ((bnds (array-dimensions array) (cdr bnds))
           (idxs indices (cdr idxs)))
          ((or (null? bnds)
               (null? idxs)
               (not (integer? (car idxs)))
               (not (< -1 (car idxs) (car bnds))))
           (and (null? bnds) (null? idxs)))))

    ;; Returns #t if its arguments would be acceptable to `array-ref`.
    (define (array-in-bounds? array . indices)
      (array:in-bounds? array indices))

    ;; Returns the (`indices`, ...) element of `array`.
    (define (array-ref array . indices)
      (let ((store (array:store array)))
        (or (array:in-bounds? array indices)
            (error "Bad indices:" indices))
        ((if (string? store) string-ref vector-ref)
         store (apply + (array:offset array) (map * (array:scales array) indices)))))

    ;; Stores `obj` in the (`indices`, ...) element of `array`. The value returned
    ;; by `array-set!` is unspecified.
    (define (array-set! array obj . indices)
      (let ((store (array:store array)))
        (or (array:in-bounds? array indices)
            (error "Bad indices:" indices))
        ((if (string? store) string-set! vector-set!)
         store (apply + (array:offset array) (map * (array:scales array) indices))
         obj)))

    ;; These functions return a prototypical uniform-array enclosing the
    ;; optional argument (which must be of the correct type). If the
    ;; uniform-array type is supported by the implementation, then it is
    ;; returned; defaulting to the next larger precision type; resorting
    ;; finally to vector.

    (define (make-prototype-checker name pred? creator)
      (lambda args
        (case (length args)
          ((1)  (if (pred? (car args))
                    (creator (car args))
                    (error "Incompatible type:" name (car args))))
          ((0)  (creator))
          (else (error "Wrong number of arguments:" name args)))))

    (define (integer-bytes?? n)
      (lambda (obj)
        (and (integer? obj)
             (exact? obj)
             (or (negative? n) (not (negative? obj)))
             (do ((num obj (quotient num 256))
                  (n (+ -1 (abs n)) (+ -1 n)))
                 ((or (zero? num) (negative? n)) (zero? num))))))

    ;; Returns an inexact 128.bit flonum complex uniform-array prototype.
    (define a:floc128b (make-prototype-checker 'a:floc128b complex? vector))

    ;; Returns an inexact 64.bit flonum complex uniform-array prototype.
    (define a:floc64b (make-prototype-checker 'a:floc64b complex? vector))

    ;; Returns an inexact 32.bit flonum complex uniform-array prototype.
    (define a:floc32b (make-prototype-checker 'a:floc32b complex? vector))

    ;; Returns an inexact 16.bit flonum complex uniform-array prototype.
    (define a:floc16b (make-prototype-checker 'a:floc16b complex? vector))

    ;; Returns an inexact 128.bit flonum real uniform-array prototype.
    (define a:flor128b (make-prototype-checker 'a:flor128b real? vector))

    ;; Returns an inexact 64.bit flonum real uniform-array prototype.
    (define a:flor64b (make-prototype-checker 'a:flor64b real? vector))

    ;; Returns an inexact 32.bit flonum real uniform-array prototype.
    (define a:flor32b (make-prototype-checker 'a:flor32b real? vector))

    ;; Returns an inexact 16.bit flonum real uniform-array prototype.
    (define a:flor16b (make-prototype-checker 'a:flor16b real? vector))

    ;; Returns an exact 128.bit decimal flonum rational uniform-array prototype.
    (define a:flor128b (make-prototype-checker 'a:flor128b real? vector))

    ;; Returns an exact 64.bit decimal flonum rational uniform-array prototype.
    (define a:flor64b (make-prototype-checker 'a:flor64b real? vector))

    ;; Returns an exact 32.bit decimal flonum rational uniform-array prototype.
    (define a:flor32b (make-prototype-checker 'a:flor32b real? vector))

    ;; Returns an exact binary fixnum uniform-array prototype with at least
    ;; 64 bits of precision.
    (define a:fixz64b (make-prototype-checker 'a:fixz64b (integer-bytes?? -8) vector))

    ;; Returns an exact binary fixnum uniform-array prototype with at least
    ;; 32 bits of precision.
    (define a:fixz32b (make-prototype-checker 'a:fixz32b (integer-bytes?? -4) vector))

    ;; Returns an exact binary fixnum uniform-array prototype with at least
    ;; 16 bits of precision.
    (define a:fixz16b (make-prototype-checker 'a:fixz16b (integer-bytes?? -2) vector))

    ;; Returns an exact binary fixnum uniform-array prototype with at least
    ;; 8 bits of precision.
    (define a:fixz8b (make-prototype-checker 'a:fixz8b (integer-bytes?? -1) vector))

    ;; Returns an exact non-negative binary fixnum uniform-array prototype with at
    ;; least 64 bits of precision.
    (define a:fixn64b (make-prototype-checker 'a:fixn64b (integer-bytes?? 8) vector))

    ;; Returns an exact non-negative binary fixnum uniform-array prototype with at
    ;; least 32 bits of precision.
    (define a:fixn32b (make-prototype-checker 'a:fixn32b (integer-bytes?? 4) vector))

    ;; Returns an exact non-negative binary fixnum uniform-array prototype with at
    ;; least 16 bits of precision.
    (define a:fixn16b (make-prototype-checker 'a:fixn16b (integer-bytes?? 2) vector))

    ;; Returns an exact non-negative binary fixnum uniform-array prototype with at
    ;; least 8 bits of precision.
    (define a:fixn8b (make-prototype-checker 'a:fixn8b (integer-bytes?? 1) vector))

    ;;Returns a boolean uniform-array prototype.
    (define a:bool (make-prototype-checker 'a:bool boolean? vector))
  )
)

