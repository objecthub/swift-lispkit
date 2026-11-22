;;; SRFI 231
;;; Intervals and Generalized Arrays
;;;
;;; This SRFI specifies an array mechanism for Scheme. Arrays as defined here are quite
;;; general; at their most basic, an array is simply a mapping, or function, from multi-indices
;;; of exact integers i0,...,id-1 to Scheme values. The set of multi-indices i0,...,id-1 that
;;; are valid for a given array form the domain of the array. In this SRFI, each array's domain
;;; consists of the cross product of intervals of exact integers [l0,u0) x ... x [ld-1,ud-1)
;;; of Zd, d-tuples of integers. Thus, we introduce a data type called d-intervals, or more
;;; briefly intervals, that encapsulates this notion. (We borrow this terminology from, e.g.,
;;; Elias Zakon's Basic Concepts of Mathematics.) Specialized variants of arrays provide
;;; portable programs with efficient representations for common use cases.
;;;
;;; Copyright © 2016, 2018, 2020, 2021, 2022 Bradley J Lucier. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify,
;;; merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to the following
;;; conditions:
;;; The above copyright notice and this permission notice (including the next paragraph)
;;; shall be included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;;; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2026 Matthias Zenger. All rights reserved.
;;;
;;; Implementation notes:
;;;   This is a new implementation of the SRFI 231 API for LispKit; it follows the semantics
;;;   of the sample implementation of the SRFI, but not its code, which relies on features
;;;   specific to Gambit Scheme.
;;;
;;;   - Intervals, storage classes and arrays are objects of distinct types. They are printed
;;;     compactly; intervals show their lower and upper bounds.
;;;   - Specialized arrays store their elements in a body (a vector, string or bytevector)
;;;     and map multi-indices to positions in the body via an affine index mapping that is
;;;     represented by a vector of coefficients. Transformations like `array-extract`,
;;;     `array-translate`, `array-permute`, `array-reverse`, `array-sample`, `array-curry`
;;;     and `specialized-array-reshape` compute the coefficients of the new array directly.
;;;     `specialized-array-share` determines them by calling the index mapping at the lower
;;;     corner of the new domain and at its neighbours along every axis; it signals an error
;;;     if the new array would refer to elements outside of the domain of the original array.
;;;   - `array-getter` and `array-setter` return the procedures passed to `make-array`. The
;;;     getters and setters of all other generalized arrays, e.g. those returned by `array-map`
;;;     or `array-translate`, check the multi-index. Safe specialized arrays check multi-indices
;;;     and the values stored in them; unsafe specialized arrays don't. Internally, the library
;;;     uses unchecked getters and setters.
;;;   - Procedures that need all elements of an array, e.g. `array-copy`, `array->list` or
;;;     `array-fold-right`, collect them in chunks of vectors instead of long temporary lists,
;;;     which are expensive to release in LispKit.
;;;   - LispKit has no homogeneous vectors besides bytevectors. `u8-storage-class` uses
;;;     bytevectors and `char-storage-class` uses strings as bodies. All other storage classes
;;;     for numbers use vectors as bodies; their checkers restrict the values that can be
;;;     stored. `f32-storage-class`, `f16-storage-class` and `c64-storage-class` round the
;;;     stored values to the precision of the corresponding floating-point format.
;;;     `f8-storage-class` is #f, like in the sample implementation.
;;;   - All procedures whose names do not end with "!" are implemented in a call/cc-safe way.

(define-library (srfi 231)

  (export ;; Miscellaneous procedures
          translation?
          permutation?
          index-rotate
          index-first
          index-last
          index-swap
          ;; Intervals
          make-interval
          interval?
          interval-dimension
          interval-lower-bound
          interval-upper-bound
          interval-width
          interval-lower-bounds->list
          interval-upper-bounds->list
          interval-lower-bounds->vector
          interval-upper-bounds->vector
          interval=
          interval-widths
          interval-volume
          interval-empty?
          interval-subset?
          interval-contains-multi-index?
          interval-projections
          interval-for-each
          interval-fold-left
          interval-fold-right
          interval-dilate
          interval-intersect
          interval-translate
          interval-permute
          interval-scale
          interval-cartesian-product
          ;; Storage classes
          make-storage-class
          storage-class?
          storage-class-getter
          storage-class-setter
          storage-class-checker
          storage-class-maker
          storage-class-copier
          storage-class-length
          storage-class-default
          storage-class-data?
          storage-class-data->body
          generic-storage-class
          char-storage-class
          s8-storage-class
          s16-storage-class
          s32-storage-class
          s64-storage-class
          u1-storage-class
          u8-storage-class
          u16-storage-class
          u32-storage-class
          u64-storage-class
          f8-storage-class
          f16-storage-class
          f32-storage-class
          f64-storage-class
          c64-storage-class
          c128-storage-class
          ;; Arrays
          specialized-array-default-safe?
          specialized-array-default-mutable?
          make-array
          array?
          array-domain
          array-getter
          array-dimension
          mutable-array?
          array-setter
          array-freeze!
          array-empty?
          make-specialized-array
          make-specialized-array-from-data
          specialized-array?
          array-storage-class
          array-indexer
          array-body
          array-safe?
          array-packed?
          specialized-array-share
          array-copy
          array-copy!
          array-curry
          array-extract
          array-tile
          array-translate
          array-permute
          array-reverse
          array-sample
          array-outer-product
          array-inner-product
          array-map
          array-for-each
          array-fold-left
          array-fold-right
          array-reduce
          array-any
          array-every
          array->list
          list->array
          array->list*
          list*->array
          array->vector
          vector->array
          vector*->array
          array->vector*
          array-assign!
          array-stack
          array-stack!
          array-decurry
          array-decurry!
          array-append
          array-append!
          array-block
          array-block!
          array-ref
          array-set!
          specialized-array-reshape)

  (import (lispkit base))

  (begin

    ;; === Utilities ===

    (define (fail who msg . irritants)
      (apply error (string-append who ": " msg) irritants))

    (define (vector-all? pred vec)
      (let ((n (vector-length vec)))
        (let loop ((i 0))
          (or (= i n) (and (pred (vector-ref vec i)) (loop (+ i 1)))))))

    (define (vector-all2? pred v1 v2)
      (let ((n (vector-length v1)))
        (let loop ((i 0))
          (or (= i n) (and (pred (vector-ref v1 i) (vector-ref v2 i)) (loop (+ i 1)))))))

    (define (list-all? pred lst)
      (or (null? lst) (and (pred (car lst)) (list-all? pred (cdr lst)))))

    (define (boolean-or-fail who obj)
      (if (not (boolean? obj))
          (fail who "argument is not a boolean:" obj)))

    (define (nonneg-integer? x)
      (and (exact-integer? x) (>= x 0)))

    (define (list-take lst n)
      (let loop ((lst lst) (n n) (res '()))
        (if (= n 0) (reverse res) (loop (cdr lst) (- n 1) (cons (car lst) res)))))

    (define (list-drop lst n)
      (if (= n 0) lst (list-drop (cdr lst) (- n 1))))

    ;; Returns the elements of vector `vec` from index `start` to `end` as a list
    (define (subvector->list vec start end)
      (do ((i (- end 1) (- i 1))
           (res '() (cons (vector-ref vec i) res)))
          ((< i start) res)))

    ;; === Miscellaneous procedures ===

    (define (translation? obj)
      (and (vector? obj) (vector-all? exact-integer? obj)))

    (define (permutation? obj)
      (and (vector? obj)
           (let* ((n (vector-length obj))
                  (seen (make-vector n #f)))
             (let loop ((i 0))
               (or (= i n)
                   (let ((x (vector-ref obj i)))
                     (and (exact-integer? x)
                          (< -1 x n)
                          (not (vector-ref seen x))
                          (begin (vector-set! seen x #t) (loop (+ i 1))))))))))

    (define (index-rotate n k)
      (if (not (nonneg-integer? n))
          (fail "index-rotate" "first argument is not a nonnegative exact integer:" n))
      (if (not (and (exact-integer? k) (<= 0 k n)))
          (fail "index-rotate" "second argument is not an exact integer between 0 and n:" k))
      (list->vector (append (iota (- n k) k) (iota k))))

    (define (index-first n k)
      (if (not (and (exact-integer? n) (positive? n)))
          (fail "index-first" "first argument is not a positive exact integer:" n))
      (if (not (and (exact-integer? k) (< -1 k n)))
          (fail "index-first" "second argument is not an exact integer between 0 and n-1:" k))
      (list->vector (cons k (append (iota k) (iota (- n k 1) (+ k 1))))))

    (define (index-last n k)
      (if (not (and (exact-integer? n) (positive? n)))
          (fail "index-last" "first argument is not a positive exact integer:" n))
      (if (not (and (exact-integer? k) (< -1 k n)))
          (fail "index-last" "second argument is not an exact integer between 0 and n-1:" k))
      (list->vector (append (iota k) (iota (- n k 1) (+ k 1)) (list k))))

    (define (index-swap n i j)
      (if (not (and (exact-integer? n) (positive? n)))
          (fail "index-swap" "first argument is not a positive exact integer:" n))
      (if (not (and (exact-integer? i) (< -1 i n)))
          (fail "index-swap" "second argument is not an exact integer between 0 and n-1:" i))
      (if (not (and (exact-integer? j) (< -1 j n)))
          (fail "index-swap" "third argument is not an exact integer between 0 and n-1:" j))
      (let ((res (list->vector (iota n))))
        (vector-set! res i j)
        (vector-set! res j i)
        res))

    ;; === Intervals ===

    ;; An interval is an object of a new type whose payload is a list containing a vector of
    ;; lower bounds and a vector of upper bounds; these vectors are never mutated.
    (define-values (interval-type-tag new-interval interval? interval-payload make-interval-subtype)
      (make-type 'interval))

    (define-syntax lbs
      (syntax-rules () ((_ iv) (car (interval-payload iv)))))
    (define-syntax ubs
      (syntax-rules () ((_ iv) (cadr (interval-payload iv)))))

    (define (%interval lb ub)
      (new-interval (list lb ub)))

    (define (check-interval who obj)
      (if (not (interval? obj))
          (fail who "argument is not an interval:" obj)))

    (define make-interval
      (case-lambda
        ((ub)
          (if (not (and (vector? ub) (vector-all? nonneg-integer? ub)))
              (fail "make-interval" "argument is not a vector of nonnegative exact integers:" ub))
          (%interval (make-vector (vector-length ub) 0) (vector-copy ub)))
        ((lb ub)
          (if (not (translation? lb))
              (fail "make-interval" "first argument is not a vector of exact integers:" lb))
          (if (not (translation? ub))
              (fail "make-interval" "second argument is not a vector of exact integers:" ub))
          (if (not (= (vector-length lb) (vector-length ub)))
              (fail "make-interval" "bound vectors have different lengths:" lb ub))
          (if (not (vector-all2? <= lb ub))
              (fail "make-interval" "lower bounds exceed upper bounds:" lb ub))
          (%interval (vector-copy lb) (vector-copy ub)))))

    (define (interval-dimension iv)
      (check-interval "interval-dimension" iv)
      (vector-length (lbs iv)))

    (define (check-axis who iv k)
      (if (not (and (exact-integer? k) (< -1 k (vector-length (lbs iv)))))
          (fail who "invalid axis:" k)))

    (define (interval-lower-bound iv k)
      (check-interval "interval-lower-bound" iv)
      (check-axis "interval-lower-bound" iv k)
      (vector-ref (lbs iv) k))

    (define (interval-upper-bound iv k)
      (check-interval "interval-upper-bound" iv)
      (check-axis "interval-upper-bound" iv k)
      (vector-ref (ubs iv) k))

    (define (interval-width iv k)
      (check-interval "interval-width" iv)
      (check-axis "interval-width" iv k)
      (- (vector-ref (ubs iv) k) (vector-ref (lbs iv) k)))

    (define (interval-lower-bounds->list iv)
      (check-interval "interval-lower-bounds->list" iv)
      (vector->list (lbs iv)))

    (define (interval-upper-bounds->list iv)
      (check-interval "interval-upper-bounds->list" iv)
      (vector->list (ubs iv)))

    (define (interval-lower-bounds->vector iv)
      (check-interval "interval-lower-bounds->vector" iv)
      (vector-copy (lbs iv)))

    (define (interval-upper-bounds->vector iv)
      (check-interval "interval-upper-bounds->vector" iv)
      (vector-copy (ubs iv)))

    (define (%widths iv)
      (vector-map - (ubs iv) (lbs iv)))

    (define (interval-widths iv)
      (check-interval "interval-widths" iv)
      (%widths iv))

    (define (%volume iv)
      (let ((lb (lbs iv)) (ub (ubs iv)))
        (do ((k (- (vector-length lb) 1) (- k 1))
             (vol 1 (* vol (- (vector-ref ub k) (vector-ref lb k)))))
            ((< k 0) vol))))

    (define (%empty? iv)
      (let ((lb (lbs iv)) (ub (ubs iv)))
        (let loop ((k (- (vector-length lb) 1)))
          (and (>= k 0) (or (= (vector-ref lb k) (vector-ref ub k)) (loop (- k 1)))))))

    (define (interval-volume iv)
      (check-interval "interval-volume" iv)
      (%volume iv))

    (define (interval-empty? iv)
      (check-interval "interval-empty?" iv)
      (%empty? iv))

    (define (%interval= iv1 iv2)
      (and (equal? (lbs iv1) (lbs iv2)) (equal? (ubs iv1) (ubs iv2))))

    (define (interval= iv1 iv2)
      (check-interval "interval=" iv1)
      (check-interval "interval=" iv2)
      (%interval= iv1 iv2))

    (define (%dimension iv)
      (vector-length (lbs iv)))

    (define (%subset? iv1 iv2)
      (and (vector-all2? >= (lbs iv1) (lbs iv2))
           (vector-all2? <= (ubs iv1) (ubs iv2))))

    (define (interval-subset? iv1 iv2)
      (check-interval "interval-subset?" iv1)
      (check-interval "interval-subset?" iv2)
      (if (not (= (%dimension iv1) (%dimension iv2)))
          (fail "interval-subset?" "intervals have different dimensions:" iv1 iv2))
      (%subset? iv1 iv2))

    ;; Returns #t if the multi-index given as a list is contained in the interval
    (define (%contains? iv idx)
      (let ((lb (lbs iv)) (ub (ubs iv)))
        (let loop ((k 0) (idx idx))
          (or (null? idx)
              (let ((i (car idx)))
                (and (<= (vector-ref lb k) i)
                     (< i (vector-ref ub k))
                     (loop (+ k 1) (cdr idx))))))))

    (define (interval-contains-multi-index? iv . idx)
      (check-interval "interval-contains-multi-index?" iv)
      (if (not (= (length idx) (%dimension iv)))
          (fail "interval-contains-multi-index?" "multi-index has wrong length:" idx))
      (if (not (list-all? exact-integer? idx))
          (fail "interval-contains-multi-index?" "multi-index is not a list of exact integers:" idx))
      (%contains? iv idx))

    (define (interval-projections iv right-dimension)
      (check-interval "interval-projections" iv)
      (let ((d (%dimension iv)))
        (if (not (and (exact-integer? right-dimension) (<= 0 right-dimension d)))
            (fail "interval-projections" "invalid right dimension:" right-dimension))
        (let ((k (- d right-dimension)))
          (values (%interval (vector-copy (lbs iv) 0 k) (vector-copy (ubs iv) 0 k))
                  (%interval (vector-copy (lbs iv) k) (vector-copy (ubs iv) k))))))

    ;; Folds `op` over the results of applying `f` to all multi-indices of the interval with
    ;; lower bounds `lb` and upper bounds `ub` in lexicographical order. This is done without
    ;; mutating state such that it is safe to re-enter continuations captured by `f`.
    (define (%fold-left f op id lb ub)
      (case (vector-length lb)
        ((0)
          (op id (f)))
        ((1)
          (let ((u0 (vector-ref ub 0)))
            (let loop0 ((i (vector-ref lb 0)) (acc id))
              (if (< i u0) (loop0 (+ i 1) (op acc (f i))) acc))))
        ((2)
          (let ((u0 (vector-ref ub 0)) (l1 (vector-ref lb 1)) (u1 (vector-ref ub 1)))
            (let loop0 ((i (vector-ref lb 0)) (acc id))
              (if (< i u0)
                  (loop0 (+ i 1)
                         (let loop1 ((j l1) (acc acc))
                           (if (< j u1) (loop1 (+ j 1) (op acc (f i j))) acc)))
                  acc))))
        ((3)
          (let ((u0 (vector-ref ub 0))
                (l1 (vector-ref lb 1)) (u1 (vector-ref ub 1))
                (l2 (vector-ref lb 2)) (u2 (vector-ref ub 2)))
            (let loop0 ((i (vector-ref lb 0)) (acc id))
              (if (< i u0)
                  (loop0 (+ i 1)
                         (let loop1 ((j l1) (acc acc))
                           (if (< j u1)
                               (loop1 (+ j 1)
                                      (let loop2 ((k l2) (acc acc))
                                        (if (< k u2) (loop2 (+ k 1) (op acc (f i j k))) acc)))
                               acc)))
                  acc))))
        (else
          (let ((d (vector-length lb)))
            (let dim ((k 0) (rev '()) (acc id))
              (if (= k d)
                  (op acc (apply f (reverse rev)))
                  (let ((u (vector-ref ub k)))
                    (let loop ((i (vector-ref lb k)) (acc acc))
                      (if (< i u) (loop (+ i 1) (dim (+ k 1) (cons i rev) acc)) acc)))))))))

    ;; Like `%fold-left` for `f` taking a list of indices
    (define (%fold-left/list f op id lb ub)
      (let ((d (vector-length lb)))
        (let dim ((k 0) (rev '()) (acc id))
          (if (= k d)
              (op acc (f (reverse rev)))
              (let ((u (vector-ref ub k)))
                (let loop ((i (vector-ref lb k)) (acc acc))
                  (if (< i u) (loop (+ i 1) (dim (+ k 1) (cons i rev) acc)) acc)))))))

    ;; Calls `f` on the multi-indices of the interval in lexicographical order, stopping at
    ;; the first call that returns a true value; returns this value, or #f.
    (define (%find f lb ub)
      (let ((d (vector-length lb)))
        (let dim ((k 0) (rev '()))
          (if (= k d)
              (apply f (reverse rev))
              (let ((u (vector-ref ub k)))
                (let loop ((i (vector-ref lb k)))
                  (and (< i u) (or (dim (+ k 1) (cons i rev)) (loop (+ i 1))))))))))

    ;; Calls `f` on the multi-indices of the interval in lexicographical order, stopping at
    ;; the first call that returns #f. Returns #f in this case, otherwise the result of the
    ;; last call, or #t if `f` was never called.
    (define (%every f lb ub)
      (let ((d (vector-length lb)))
        (let dim ((k 0) (rev '()) (last #t))
          (if (= k d)
              (apply f (reverse rev))
              (let ((u (vector-ref ub k)))
                (let loop ((i (vector-ref lb k)) (last last))
                  (if (< i u)
                      (let ((res (dim (+ k 1) (cons i rev) last)))
                        (and res (loop (+ i 1) res)))
                      last)))))))

    (define (ignore-result acc x) acc)

    (define (interval-for-each f iv)
      (if (not (procedure? f))
          (fail "interval-for-each" "first argument is not a procedure:" f))
      (check-interval "interval-for-each" iv)
      (if (not (%empty? iv))
          (%fold-left f ignore-result #f (lbs iv) (ubs iv)))
      (if #f #f))

    (define (interval-fold-left f op id iv)
      (if (not (procedure? f))
          (fail "interval-fold-left" "first argument is not a procedure:" f))
      (if (not (procedure? op))
          (fail "interval-fold-left" "second argument is not a procedure:" op))
      (check-interval "interval-fold-left" iv)
      (if (%empty? iv)
          id
          (%fold-left f op id (lbs iv) (ubs iv))))

    ;; Collecting values in lexicographical order: since long lists are expensive to release
    ;; in LispKit, values are collected in chunks, which are vectors that are never mutated
    ;; after they were created. The accumulator is a vector #(count partial chunks total),
    ;; which is never mutated either, such that collecting values is call/cc safe.

    (define chunk-size 1024)

    (define empty-accumulator (vector 0 '() '() 0))

    (define (accumulate acc x)
      (let ((count (vector-ref acc 0))
            (partial (cons x (vector-ref acc 1)))
            (chunks (vector-ref acc 2))
            (total (+ (vector-ref acc 3) 1)))
        (if (= count (- chunk-size 1))
            (vector 0 '() (cons (list->vector (reverse partial)) chunks) total)
            (vector (+ count 1) partial chunks total))))

    ;; Returns a new vector with the values collected by `acc`
    (define (accumulator->vector acc)
      (let ((res (make-vector (vector-ref acc 3))))
        (let loop ((chunks (reverse (vector-ref acc 2))) (i 0))
          (if (pair? chunks)
              (begin
                (vector-copy! res i (car chunks))
                (loop (cdr chunks) (+ i chunk-size)))
              (do ((partial (vector-ref acc 1) (cdr partial))
                   (j (- (vector-ref acc 3) 1) (- j 1)))
                  ((null? partial) res)
                (vector-set! res j (car partial)))))))

    ;; Returns a new vector with the results of applying `f` to all multi-indices of the
    ;; interval with lower bounds `lb` and upper bounds `ub` in lexicographical order
    (define (%collect f lb ub)
      (accumulator->vector (%fold-left f accumulate empty-accumulator lb ub)))

    (define (interval-fold-right f op id iv)
      (if (not (procedure? f))
          (fail "interval-fold-right" "first argument is not a procedure:" f))
      (if (not (procedure? op))
          (fail "interval-fold-right" "second argument is not a procedure:" op))
      (check-interval "interval-fold-right" iv)
      (if (%empty? iv)
          id
          ;; Evaluate `f` at all multi-indices first, then apply `op` from right to left
          (let ((items (%collect f (lbs iv) (ubs iv))))
            (do ((i (- (vector-length items) 1) (- i 1))
                 (acc id (op (vector-ref items i) acc)))
                ((< i 0) acc)))))

    (define (interval-dilate iv lower-diffs upper-diffs)
      (check-interval "interval-dilate" iv)
      (if (not (and (translation? lower-diffs) (= (vector-length lower-diffs) (%dimension iv))))
          (fail "interval-dilate" "second argument is not a translation of the right dimension:"
                lower-diffs))
      (if (not (and (translation? upper-diffs) (= (vector-length upper-diffs) (%dimension iv))))
          (fail "interval-dilate" "third argument is not a translation of the right dimension:"
                upper-diffs))
      (let ((lb (vector-map + (lbs iv) lower-diffs))
            (ub (vector-map + (ubs iv) upper-diffs)))
        (if (not (vector-all2? <= lb ub))
            (fail "interval-dilate" "resulting lower bounds exceed upper bounds:" lb ub))
        (%interval lb ub)))

    (define (interval-intersect iv . ivs)
      (check-interval "interval-intersect" iv)
      (for-each (lambda (other)
                  (check-interval "interval-intersect" other)
                  (if (not (= (%dimension other) (%dimension iv)))
                      (fail "interval-intersect" "intervals have different dimensions:" iv other)))
                ivs)
      (let ((lb (apply vector-map max (lbs iv) (map (lambda (x) (lbs x)) ivs)))
            (ub (apply vector-map min (ubs iv) (map (lambda (x) (ubs x)) ivs))))
        (and (vector-all2? <= lb ub) (%interval lb ub))))

    (define (interval-translate iv translation)
      (check-interval "interval-translate" iv)
      (if (not (translation? translation))
          (fail "interval-translate" "second argument is not a translation:" translation))
      (if (not (= (vector-length translation) (%dimension iv)))
          (fail "interval-translate" "translation has the wrong dimension:" translation))
      (%translate iv translation))

    (define (%translate iv translation)
      (%interval (vector-map + (lbs iv) translation) (vector-map + (ubs iv) translation)))

    (define (interval-permute iv perm)
      (check-interval "interval-permute" iv)
      (if (not (permutation? perm))
          (fail "interval-permute" "second argument is not a permutation:" perm))
      (if (not (= (vector-length perm) (%dimension iv)))
          (fail "interval-permute" "permutation has the wrong dimension:" perm))
      (%permute iv perm))

    (define (%permute iv perm)
      (let ((lb (lbs iv)) (ub (ubs iv)))
        (%interval (vector-map (lambda (k) (vector-ref lb k)) perm)
                   (vector-map (lambda (k) (vector-ref ub k)) perm))))

    (define (positive-integer? x)
      (and (exact-integer? x) (positive? x)))

    (define (interval-scale iv scales)
      (check-interval "interval-scale" iv)
      (if (not (vector-all? zero? (lbs iv)))
          (fail "interval-scale" "lower bounds of interval are not all zero:" iv))
      (if (not (and (vector? scales) (vector-all? positive-integer? scales)))
          (fail "interval-scale" "second argument is not a vector of positive exact integers:"
                scales))
      (if (not (= (vector-length scales) (%dimension iv)))
          (fail "interval-scale" "scales have the wrong dimension:" scales))
      (%scale iv scales))

    (define (%scale iv scales)
      (let ((ub (vector-map (lambda (u s) (quotient (+ u s -1) s)) (ubs iv) scales)))
        (%interval (make-vector (vector-length ub) 0) ub)))

    (define (interval-cartesian-product . ivs)
      (for-each (lambda (iv) (check-interval "interval-cartesian-product" iv)) ivs)
      (%product ivs))

    (define (%product ivs)
      (%interval (apply vector-append (map (lambda (iv) (lbs iv)) ivs))
                 (apply vector-append (map (lambda (iv) (ubs iv)) ivs))))

    ;; === Storage classes ===

    ;; A storage class is an object of a new type whose payload is a procedure returning a
    ;; vector of its components.
    (define-values (sc-type-tag new-storage-class storage-class? sc-payload make-sc-subtype)
      (make-type 'storage-class))

    (define (make-storage-class getter setter checker maker copier length default data? data->body)
      (let ((components (vector getter setter checker maker copier length default data? data->body)))
        (new-storage-class (lambda () components))))

    (define (sc-ref who sc i)
      (if (not (storage-class? sc))
          (fail who "argument is not a storage class:" sc))
      (vector-ref ((sc-payload sc)) i))

    (define (storage-class-getter sc) (sc-ref "storage-class-getter" sc 0))
    (define (storage-class-setter sc) (sc-ref "storage-class-setter" sc 1))
    (define (storage-class-checker sc) (sc-ref "storage-class-checker" sc 2))
    (define (storage-class-maker sc) (sc-ref "storage-class-maker" sc 3))
    (define (storage-class-copier sc) (sc-ref "storage-class-copier" sc 4))
    (define (storage-class-length sc) (sc-ref "storage-class-length" sc 5))
    (define (storage-class-default sc) (sc-ref "storage-class-default" sc 6))
    (define (storage-class-data? sc) (sc-ref "storage-class-data?" sc 7))
    (define (storage-class-data->body sc) (sc-ref "storage-class-data->body" sc 8))

    ;; Unchecked accessors of storage class components
    (define-syntax sc-getter
      (syntax-rules () ((_ sc) (vector-ref ((sc-payload sc)) 0))))
    (define-syntax sc-setter
      (syntax-rules () ((_ sc) (vector-ref ((sc-payload sc)) 1))))
    (define-syntax sc-checker
      (syntax-rules () ((_ sc) (vector-ref ((sc-payload sc)) 2))))
    (define-syntax sc-maker
      (syntax-rules () ((_ sc) (vector-ref ((sc-payload sc)) 3))))
    (define-syntax sc-length
      (syntax-rules () ((_ sc) (vector-ref ((sc-payload sc)) 5))))
    (define-syntax sc-default
      (syntax-rules () ((_ sc) (vector-ref ((sc-payload sc)) 6))))

    (define (accept-all x) #t)

    (define generic-storage-class
      (make-storage-class
        vector-ref
        vector-set!
        accept-all
        make-vector
        vector-copy!
        vector-length
        #f
        vector?
        (lambda (data)
          (if (vector? data)
              data
              (fail "storage-class-data->body" "data is not a vector:" data)))))

    (define char-storage-class
      (make-storage-class
        string-ref
        string-set!
        char?
        make-string
        string-copy!
        string-length
        #\null
        string?
        (lambda (data)
          (if (string? data)
              data
              (fail "storage-class-data->body" "data is not a string:" data)))))

    (define (u8? x)
      (and (fixnum? x) (<= 0 x 255)))

    (define u8-storage-class
      (make-storage-class
        bytevector-u8-ref
        bytevector-u8-set!
        u8?
        make-bytevector
        bytevector-copy!
        bytevector-length
        0
        bytevector?
        (lambda (data)
          (if (bytevector? data)
              data
              (fail "storage-class-data->body" "data is not a bytevector:" data)))))

    ;; Storage classes whose bodies are vectors containing only values accepted by `checker`.
    ;; `convert` maps a value accepted by `checker` to the value that is stored. Data can be
    ;; used as a body if it is a vector of values that are not changed by `convert`.
    (define (make-vector-storage-class checker default convert)
      (let ((data? (lambda (data)
                     (and (vector? data)
                          (vector-all? (if convert
                                           (lambda (x) (and (checker x) (eqv? (convert x) x)))
                                           checker)
                                       data)))))
        (make-storage-class
          vector-ref
          (if convert
              (lambda (body i val) (vector-set! body i (convert val)))
              vector-set!)
          checker
          (if convert
              (lambda (n val) (make-vector n (convert val)))
              make-vector)
          vector-copy!
          vector-length
          default
          data?
          (lambda (data)
            (if (data? data)
                data
                (fail "storage-class-data->body" "data is not a vector of suitable values:"
                      data))))))

    (define (integer-checker lo hi)
      (lambda (x) (and (exact-integer? x) (<= lo x hi))))

    (define u1-storage-class (make-vector-storage-class (integer-checker 0 1) 0 #f))
    (define u16-storage-class (make-vector-storage-class (integer-checker 0 65535) 0 #f))
    (define u32-storage-class
      (make-vector-storage-class (integer-checker 0 (- (expt 2 32) 1)) 0 #f))
    (define u64-storage-class
      (make-vector-storage-class (integer-checker 0 (- (expt 2 64) 1)) 0 #f))
    (define s8-storage-class (make-vector-storage-class (integer-checker -128 127) 0 #f))
    (define s16-storage-class (make-vector-storage-class (integer-checker -32768 32767) 0 #f))
    (define s32-storage-class
      (make-vector-storage-class (integer-checker (- (expt 2 31)) (- (expt 2 31) 1)) 0 #f))
    (define s64-storage-class
      (make-vector-storage-class (integer-checker (- (expt 2 63)) (- (expt 2 63) 1)) 0 #f))

    ;; Rounds flonum `x` to the nearest value of a binary floating-point format with `p` bits
    ;; of precision and exponents between `emin` and `emax` (ties to even)
    (define (round-to-format x p emin emax)
      (if (or (nan? x) (infinite? x) (zero? x))
          x
          (let* ((r (exact (abs x)))
                 (e0 (- (integer-length (numerator r)) (integer-length (denominator r))))
                 (e (if (< r (expt 2 e0)) (- e0 1) e0))
                 (q (expt 2 (- (max e emin) (- p 1))))
                 (v (* (round (/ r q)) q))
                 (res (if (>= v (expt 2 (+ emax 1))) +inf.0 (inexact v))))
            (if (negative? x) (- res) res))))

    (define (round-to-f32 x) (round-to-format x 24 -126 127))
    (define (round-to-f16 x) (round-to-format x 11 -14 15))

    (define f64-storage-class (make-vector-storage-class flonum? 0.0 #f))
    (define f32-storage-class (make-vector-storage-class flonum? 0.0 round-to-f32))
    (define f16-storage-class (make-vector-storage-class flonum? 0.0 round-to-f16))
    (define f8-storage-class #f)

    (define (inexact-complex? x)
      (and (number? x) (inexact? x)))

    (define c128-storage-class
      (make-vector-storage-class inexact-complex? 0.0+0.0i #f))
    (define c64-storage-class
      (make-vector-storage-class
        inexact-complex?
        0.0+0.0i
        (lambda (z)
          (make-rectangular (round-to-f32 (inexact (real-part z)))
                            (round-to-f32 (inexact (imag-part z)))))))

    (define (check-storage-class who obj)
      (if (not (storage-class? obj))
          (fail who "argument is not a storage class:" obj)))

    ;; === Parameters ===

    (define specialized-array-default-safe?
      (make-parameter #f (lambda (x)
                           (boolean-or-fail "specialized-array-default-safe?" x)
                           x)))

    (define specialized-array-default-mutable?
      (make-parameter #t (lambda (x)
                           (boolean-or-fail "specialized-array-default-mutable?" x)
                           x)))

    ;; === Arrays ===

    ;; An array is an object of a new type whose payload is a procedure returning the state
    ;; of the array, a vector with the following components:
    ;;   0. the domain (an interval)
    ;;   1. the getter (checking its arguments for generalized and safe specialized arrays)
    ;;   2. the unchecked getter
    ;;   3. the setter (or #f for immutable arrays)
    ;;   4. the unchecked setter (or #f for immutable arrays)
    ;;   5. the storage class (or #f for generalized arrays)
    ;;   6. the body (specialized arrays only)
    ;;   7. the coefficients c0, c1, ..., cd of the indexer (specialized arrays only): the
    ;;      multi-index i0, ..., id-1 is stored at index c0 + c1 * i0 + ... + cd * id-1
    ;;   8. safe? (specialized arrays only)
    ;;   9. packed? (#t, #f, or 'unknown)
    (define-values (array-type-tag new-array array? array-payload make-array-subtype)
      (make-type 'array))

    (define (%make-array domain getter ugetter setter usetter sc body coeffs safe?)
      (let ((state (vector domain getter ugetter setter usetter sc body coeffs safe? 'unknown)))
        (new-array (lambda () state))))

    (define-syntax state
      (syntax-rules () ((_ a) ((array-payload a)))))
    (define-syntax a-domain
      (syntax-rules () ((_ a) (vector-ref (state a) 0))))
    (define-syntax a-getter
      (syntax-rules () ((_ a) (vector-ref (state a) 1))))
    (define-syntax a-ugetter
      (syntax-rules () ((_ a) (vector-ref (state a) 2))))
    (define-syntax a-setter
      (syntax-rules () ((_ a) (vector-ref (state a) 3))))
    (define-syntax a-usetter
      (syntax-rules () ((_ a) (vector-ref (state a) 4))))
    (define-syntax a-sc
      (syntax-rules () ((_ a) (vector-ref (state a) 5))))
    (define-syntax a-body
      (syntax-rules () ((_ a) (vector-ref (state a) 6))))
    (define-syntax a-coeffs
      (syntax-rules () ((_ a) (vector-ref (state a) 7))))
    (define-syntax a-safe?
      (syntax-rules () ((_ a) (vector-ref (state a) 8))))

    (define (check-array who obj)
      (if (not (array? obj))
          (fail who "argument is not an array:" obj)))

    (define (check-mutable-array who obj)
      (if (not (and (array? obj) (a-setter obj)))
          (fail who "argument is not a mutable array:" obj)))

    (define (check-specialized-array who obj)
      (if (not (and (array? obj) (a-sc obj)))
          (fail who "argument is not a specialized array:" obj)))

    (define (%dim a)
      (vector-length (lbs (a-domain a))))

    ;; --- Checking getters and setters ---

    (define (index-error who domain idx)
      (fail who "multi-index is not in the domain of the array:" idx domain))

    (define (check-index who domain idx)
      (if (not (and (= (length idx) (%dimension domain))
                    (list-all? exact-integer? idx)
                    (%contains? domain idx)))
          (index-error who domain idx)))

    (define (in-range? i l u)
      (and (exact-integer? i) (<= l i) (< i u)))

    ;; Wraps the unchecked getter `ugetter` of an array with domain `domain` such that the
    ;; multi-index is checked
    (define (checked-getter domain ugetter)
      (let ((lb (lbs domain)) (ub (ubs domain)))
        (if (%empty? domain)
            (lambda idx (fail "array-getter" "array is empty:" domain))
            (case (vector-length lb)
              ((0) (lambda () (ugetter)))
              ((1)
                (let ((l0 (vector-ref lb 0)) (u0 (vector-ref ub 0)))
                  (lambda (i)
                    (if (in-range? i l0 u0)
                        (ugetter i)
                        (index-error "array-getter" domain (list i))))))
              ((2)
                (let ((l0 (vector-ref lb 0)) (u0 (vector-ref ub 0))
                      (l1 (vector-ref lb 1)) (u1 (vector-ref ub 1)))
                  (lambda (i j)
                    (if (and (in-range? i l0 u0) (in-range? j l1 u1))
                        (ugetter i j)
                        (index-error "array-getter" domain (list i j))))))
              ((3)
                (let ((l0 (vector-ref lb 0)) (u0 (vector-ref ub 0))
                      (l1 (vector-ref lb 1)) (u1 (vector-ref ub 1))
                      (l2 (vector-ref lb 2)) (u2 (vector-ref ub 2)))
                  (lambda (i j k)
                    (if (and (in-range? i l0 u0) (in-range? j l1 u1) (in-range? k l2 u2))
                        (ugetter i j k)
                        (index-error "array-getter" domain (list i j k))))))
              (else
                (lambda idx
                  (check-index "array-getter" domain idx)
                  (apply ugetter idx)))))))

    ;; Wraps the unchecked setter `usetter` of an array with domain `domain` such that the
    ;; multi-index and, if `checker` is a procedure, the value are checked
    (define (checked-setter domain usetter checker)
      (let ((lb (lbs domain))
            (ub (ubs domain))
            (check-value (lambda (v)
                           (if (and checker (not (checker v)))
                               (fail "array-setter" "value cannot be stored in array:" v)))))
        (if (%empty? domain)
            (lambda (v . idx) (fail "array-setter" "array is empty:" domain))
            (case (vector-length lb)
              ((0) (lambda (v) (check-value v) (usetter v)))
              ((1)
                (let ((l0 (vector-ref lb 0)) (u0 (vector-ref ub 0)))
                  (lambda (v i)
                    (if (not (in-range? i l0 u0))
                        (index-error "array-setter" domain (list i)))
                    (check-value v)
                    (usetter v i))))
              ((2)
                (let ((l0 (vector-ref lb 0)) (u0 (vector-ref ub 0))
                      (l1 (vector-ref lb 1)) (u1 (vector-ref ub 1)))
                  (lambda (v i j)
                    (if (not (and (in-range? i l0 u0) (in-range? j l1 u1)))
                        (index-error "array-setter" domain (list i j)))
                    (check-value v)
                    (usetter v i j))))
              (else
                (lambda (v . idx)
                  (check-index "array-setter" domain idx)
                  (check-value v)
                  (apply usetter v idx)))))))

    ;; --- Generalized arrays ---

    (define (%make-generalized domain ugetter usetter)
      (%make-array domain
                   (checked-getter domain ugetter)
                   ugetter
                   (and usetter (checked-setter domain usetter #f))
                   usetter
                   #f #f #f #f))

    ;; The getter and setter of arrays created by `make-array` are the given procedures; it is
    ;; up to them to check their arguments
    (define make-array
      (case-lambda
        ((domain getter)
          (check-interval "make-array" domain)
          (if (not (procedure? getter))
              (fail "make-array" "getter is not a procedure:" getter))
          (%make-array domain getter getter #f #f #f #f #f #f))
        ((domain getter setter)
          (check-interval "make-array" domain)
          (if (not (procedure? getter))
              (fail "make-array" "getter is not a procedure:" getter))
          (if (not (procedure? setter))
              (fail "make-array" "setter is not a procedure:" setter))
          (%make-array domain getter getter setter setter #f #f #f #f))))

    ;; --- Specialized arrays ---

    ;; Returns an unchecked getter for a specialized array
    (define (body-getter ref body coeffs)
      (let ((c0 (vector-ref coeffs 0)))
        (case (vector-length coeffs)
          ((1) (lambda () (ref body c0)))
          ((2)
            (let ((c1 (vector-ref coeffs 1)))
              (lambda (i) (ref body (+ c0 (* c1 i))))))
          ((3)
            (let ((c1 (vector-ref coeffs 1)) (c2 (vector-ref coeffs 2)))
              (lambda (i j) (ref body (+ c0 (* c1 i) (* c2 j))))))
          ((4)
            (let ((c1 (vector-ref coeffs 1)) (c2 (vector-ref coeffs 2)) (c3 (vector-ref coeffs 3)))
              (lambda (i j k) (ref body (+ c0 (* c1 i) (* c2 j) (* c3 k))))))
          (else
            (let ((indexer (coeffs->indexer coeffs)))
              (lambda idx (ref body (apply indexer idx))))))))

    ;; Returns an unchecked setter for a specialized array
    (define (body-setter set body coeffs)
      (let ((c0 (vector-ref coeffs 0)))
        (case (vector-length coeffs)
          ((1) (lambda (v) (set body c0 v)))
          ((2)
            (let ((c1 (vector-ref coeffs 1)))
              (lambda (v i) (set body (+ c0 (* c1 i)) v))))
          ((3)
            (let ((c1 (vector-ref coeffs 1)) (c2 (vector-ref coeffs 2)))
              (lambda (v i j) (set body (+ c0 (* c1 i) (* c2 j)) v))))
          ((4)
            (let ((c1 (vector-ref coeffs 1)) (c2 (vector-ref coeffs 2)) (c3 (vector-ref coeffs 3)))
              (lambda (v i j k) (set body (+ c0 (* c1 i) (* c2 j) (* c3 k)) v))))
          (else
            (let ((indexer (coeffs->indexer coeffs)))
              (lambda (v . idx) (set body (apply indexer idx) v)))))))

    ;; Returns the indexer procedure for the given coefficients
    (define (coeffs->indexer coeffs)
      (let ((n (vector-length coeffs)) (c0 (vector-ref coeffs 0)))
        (case n
          ((1) (lambda () c0))
          ((2) (let ((c1 (vector-ref coeffs 1)))
                 (lambda (i) (+ c0 (* c1 i)))))
          ((3) (let ((c1 (vector-ref coeffs 1)) (c2 (vector-ref coeffs 2)))
                 (lambda (i j) (+ c0 (* c1 i) (* c2 j)))))
          (else
            (lambda idx
              (let loop ((k 1) (idx idx) (pos c0))
                (cond ((null? idx)
                        (if (= k n) pos (fail "array-indexer" "too few indices")))
                      ((= k n)
                        (fail "array-indexer" "too many indices"))
                      (else
                        (loop (+ k 1) (cdr idx) (+ pos (* (vector-ref coeffs k) (car idx))))))))))))

    (define (%make-specialized domain sc body coeffs safe? mutable?)
      (let ((ugetter (body-getter (sc-getter sc) body coeffs))
            (usetter (and mutable? (body-setter (sc-setter sc) body coeffs))))
        (%make-array domain
                     (if safe? (checked-getter domain ugetter) ugetter)
                     ugetter
                     (and usetter (if safe? (checked-setter domain usetter (sc-checker sc)) usetter))
                     usetter
                     sc body coeffs safe?)))

    ;; Returns the coefficients of the lexicographical mapping of `domain` onto the
    ;; interval [0, volume)
    (define (default-coeffs domain)
      (let* ((lb (lbs domain))
             (ub (ubs domain))
             (d (vector-length lb))
             (coeffs (make-vector (+ d 1) 0)))
        (do ((k (- d 1) (- k 1))
             (stride 1 (* stride (- (vector-ref ub k) (vector-ref lb k))))
             (offset 0 (- offset (* stride (vector-ref lb k)))))
            ((< k 0) (vector-set! coeffs 0 offset) coeffs)
          (vector-set! coeffs (+ k 1) stride))))

    (define (check-value who sc v)
      (if (not ((sc-checker sc) v))
          (fail who "value cannot be stored in the storage class:" v)))

    (define (optional-args who args specialized)
      ;; Returns the storage class, mutable? and safe? arguments with defaults
      (let* ((sc (if (pair? args)
                     (car args)
                     (if specialized (a-sc specialized) generic-storage-class)))
             (args (if (pair? args) (cdr args) '()))
             (mutable? (if (pair? args)
                           (car args)
                           (if specialized
                               (and (a-setter specialized) #t)
                               (specialized-array-default-mutable?))))
             (args (if (pair? args) (cdr args) '()))
             (safe? (if (pair? args)
                        (car args)
                        (if specialized (a-safe? specialized) (specialized-array-default-safe?)))))
        (check-storage-class who sc)
        (boolean-or-fail who mutable?)
        (boolean-or-fail who safe?)
        (values sc mutable? safe?)))

    (define (make-specialized-array domain . args)
      (check-interval "make-specialized-array" domain)
      (let* ((sc (if (pair? args) (car args) generic-storage-class))
             (args (if (pair? args) (cdr args) '()))
             (sc (begin (check-storage-class "make-specialized-array" sc) sc))
             (init (if (pair? args) (car args) (sc-default sc)))
             (args (if (pair? args) (cdr args) '()))
             (safe? (if (pair? args) (car args) (specialized-array-default-safe?))))
        (check-value "make-specialized-array" sc init)
        (boolean-or-fail "make-specialized-array" safe?)
        (%make-specialized domain sc ((sc-maker sc) (%volume domain) init)
                           (default-coeffs domain) safe? #t)))

    (define (make-specialized-array-from-data data . args)
      (let-values (((sc mutable? safe?) (optional-args "make-specialized-array-from-data" args #f)))
        (if (not ((storage-class-data? sc) data))
            (fail "make-specialized-array-from-data" "data cannot be used with storage class:"
                  data))
        (let* ((body ((storage-class-data->body sc) data))
               (n ((sc-length sc) body))
               (domain (%interval (vector 0) (vector n))))
          (%make-specialized domain sc body (vector 0 1) safe? mutable?))))

    ;; Returns a new specialized array whose elements are stored in a fresh body in
    ;; lexicographical order, taking them from the vector `elems` starting at index `start`
    (define (vector->specialized who domain elems start sc mutable? safe?)
      (let* ((n (%volume domain))
             (body ((sc-maker sc) n (sc-default sc)))
             (set (sc-setter sc))
             (checker (sc-checker sc)))
        (do ((i 0 (+ i 1)))
            ((= i n))
          (let ((v (vector-ref elems (+ start i))))
            (if (not (checker v))
                (fail who "value cannot be stored in the storage class:" v))
            (set body i v)))
        (%make-specialized domain sc body (default-coeffs domain) safe? mutable?)))

    ;; Returns a new specialized array with domain `domain` whose elements are computed by
    ;; calling `ugetter` on all multi-indices in lexicographical order. If `call/cc-safe?` is
    ;; true, the elements are collected before they are stored.
    (define (tabulate-array who domain ugetter sc mutable? safe? call/cc-safe?)
      (if call/cc-safe?
          (vector->specialized who domain
                               (if (%empty? domain)
                                   (vector)
                                   (%collect ugetter (lbs domain) (ubs domain)))
                               0 sc mutable? safe?)
          (let* ((n (%volume domain))
                 (body ((sc-maker sc) n (sc-default sc)))
                 (set (sc-setter sc))
                 (checker (sc-checker sc)))
            (if (> n 0)
                (%fold-left ugetter
                            (lambda (i v)
                              (if (not (checker v))
                                  (fail who "value cannot be stored in the storage class:" v))
                              (set body i v)
                              (+ i 1))
                            0
                            (lbs domain)
                            (ubs domain)))
            (%make-specialized domain sc body (default-coeffs domain) safe? mutable?))))

    ;; Returns a specialized array that shares the body of the specialized array `a` with the
    ;; new domain and coefficients
    (define (share a domain coeffs)
      (%make-specialized domain (a-sc a) (a-body a) coeffs (a-safe? a) (and (a-setter a) #t)))

    (define (specialized-array? obj)
      (and (array? obj) (a-sc obj) #t))

    (define (array-domain a)
      (check-array "array-domain" a)
      (a-domain a))

    (define (array-getter a)
      (check-array "array-getter" a)
      (a-getter a))

    (define (array-dimension a)
      (check-array "array-dimension" a)
      (%dim a))

    (define (mutable-array? obj)
      (and (array? obj) (a-setter obj) #t))

    (define (array-setter a)
      (check-mutable-array "array-setter" a)
      (a-setter a))

    (define (array-freeze! a)
      (check-array "array-freeze!" a)
      (vector-set! (state a) 3 #f)
      (vector-set! (state a) 4 #f)
      a)

    (define (array-empty? a)
      (check-array "array-empty?" a)
      (%empty? (a-domain a)))

    (define (array-storage-class a)
      (check-specialized-array "array-storage-class" a)
      (a-sc a))

    (define (array-indexer a)
      (check-specialized-array "array-indexer" a)
      (coeffs->indexer (a-coeffs a)))

    (define (array-body a)
      (check-specialized-array "array-body" a)
      (a-body a))

    (define (array-safe? a)
      (check-specialized-array "array-safe?" a)
      (a-safe? a))

    (define (array-packed? a)
      (check-specialized-array "array-packed?" a)
      (let ((packed (vector-ref (state a) 9)))
        (if (boolean? packed)
            packed
            (let ((packed (compute-packed (a-domain a) (a-coeffs a))))
              (vector-set! (state a) 9 packed)
              packed))))

    (define (compute-packed domain coeffs)
      (or (%empty? domain)
          (let ((lb (lbs domain)) (ub (ubs domain)))
            (let loop ((k (- (vector-length lb) 1)) (stride 1))
              (or (< k 0)
                  (let ((w (- (vector-ref ub k) (vector-ref lb k))))
                    (if (= w 1)
                        (loop (- k 1) stride)
                        (and (= (vector-ref coeffs (+ k 1)) stride)
                             (loop (- k 1) (* stride w))))))))))

    ;; Calls `proc` with the multi-index `idx` (a list) and returns the resulting multi-index
    ;; of the original array as a list after checking that it has length `d`
    (define (apply-mapping proc idx d)
      (call-with-values
        (lambda () (apply proc idx))
        (lambda res
          (if (not (and (= (length res) d) (list-all? exact-integer? res)))
              (fail "specialized-array-share" "mapping does not return a valid multi-index:" res))
          res)))

    (define (specialized-array-share a new-domain proc)
      (check-specialized-array "specialized-array-share" a)
      (check-interval "specialized-array-share" new-domain)
      (if (not (procedure? proc))
          (fail "specialized-array-share" "third argument is not a procedure:" proc))
      (let* ((domain (a-domain a))
             (d (%dimension domain))
             (nd (%dimension new-domain))
             (coeffs (make-vector (+ nd 1) 0)))
        (if (> (%volume new-domain) (%volume domain))
            (fail "specialized-array-share" "new domain has more elements than the array:"
                  new-domain))
        (if (not (%empty? new-domain))
            ;; Determine the affine mapping by calling `proc` at the lower corner `lo` of the
            ;; new domain and at `lo + e_k` for every axis k of width > 1
            (let* ((nlb (lbs new-domain))
                   (nub (ubs new-domain))
                   (lo (vector->list nlb))
                   (base (list->vector (apply-mapping proc lo d)))
                   (lin (make-vector (* nd d) 0)))  ; lin[k * d + i]
              (do ((k 0 (+ k 1)))
                  ((= k nd))
                (if (> (- (vector-ref nub k) (vector-ref nlb k)) 1)
                    (let ((js (apply-mapping proc
                                             (let loop ((i (- nd 1)) (res '()))
                                               (if (< i 0)
                                                   res
                                                   (loop (- i 1)
                                                         (cons (if (= i k)
                                                                   (+ (vector-ref nlb i) 1)
                                                                   (vector-ref nlb i))
                                                               res))))
                                             d)))
                      (do ((i 0 (+ i 1))
                           (js js (cdr js)))
                          ((= i d))
                        (vector-set! lin (+ (* k d) i) (- (car js) (vector-ref base i)))))))
              ;; Check that the corners of the new domain map into the domain of `a`
              (do ((i 0 (+ i 1)))
                  ((= i d))
                (do ((k 0 (+ k 1))
                     (mn (vector-ref base i))
                     (mx (vector-ref base i)))
                    ((= k nd)
                     (if (not (and (<= (vector-ref (lbs domain) i) mn)
                                   (< mx (vector-ref (ubs domain) i))))
                         (fail "specialized-array-share"
                               "mapping exceeds the domain of the array:" domain)))
                  (let ((delta (* (vector-ref lin (+ (* k d) i))
                                  (- (vector-ref nub k) (vector-ref nlb k) 1))))
                    (if (negative? delta) (set! mn (+ mn delta)) (set! mx (+ mx delta))))))
              ;; Compose the mapping with the indexer of `a`
              (let* ((ocoeffs (a-coeffs a))
                     (pos (do ((i 0 (+ i 1))
                               (pos (vector-ref ocoeffs 0)
                                    (+ pos (* (vector-ref ocoeffs (+ i 1)) (vector-ref base i)))))
                              ((= i d) pos))))
                (do ((k 0 (+ k 1))
                     (offset pos))
                    ((= k nd) (vector-set! coeffs 0 offset))
                  (let ((c (do ((i 0 (+ i 1))
                                (c 0 (+ c (* (vector-ref ocoeffs (+ i 1))
                                             (vector-ref lin (+ (* k d) i))))))
                               ((= i d) c))))
                    (vector-set! coeffs (+ k 1) c)
                    (set! offset (- offset (* c (vector-ref nlb k)))))))))
        (share a new-domain coeffs)))

    ;; --- Copying ---

    (define (%array-copy who a args call/cc-safe?)
      (check-array who a)
      (let-values (((sc mutable? safe?) (optional-args who args (and (a-sc a) a))))
        (tabulate-array who (a-domain a) (a-ugetter a) sc mutable? safe? call/cc-safe?)))

    (define (array-copy a . args)
      (%array-copy "array-copy" a args #t))

    (define (array-copy! a . args)
      (%array-copy "array-copy!" a args #f))

    ;; --- Transformations ---

    ;; Returns a getter or setter for a generalized array that transforms the multi-index
    ;; via `transform` (a procedure mapping a list to a list) before calling `proc`
    (define (transform-getter proc transform)
      (lambda idx (apply proc (transform idx))))

    (define (transform-setter proc transform)
      (and proc (lambda (v . idx) (apply proc v (transform idx)))))

    (define (transformed a domain transform)
      (%make-generalized domain
                         (transform-getter (a-ugetter a) transform)
                         (transform-setter (a-usetter a) transform)))

    (define (array-curry a inner-dimension)
      (check-array "array-curry" a)
      (let ((d (%dim a)))
        (if (not (and (exact-integer? inner-dimension) (<= 0 inner-dimension d)))
            (fail "array-curry" "invalid inner dimension:" inner-dimension))
        (let-values (((outer inner) (interval-projections (a-domain a) inner-dimension)))
          (let ((k (- d inner-dimension)))
            (%make-generalized
              outer
              (if (a-sc a)
                  (let ((coeffs (a-coeffs a)))
                    (lambda outer-idx
                      (let ((c (make-vector (+ inner-dimension 1) 0)))
                        (do ((j 0 (+ j 1))
                             (idx outer-idx (cdr idx))
                             (c0 (vector-ref coeffs 0)
                                 (+ c0 (* (vector-ref coeffs (+ j 1)) (car idx)))))
                            ((= j k) (vector-set! c 0 c0)))
                        (do ((j 1 (+ j 1)))
                            ((> j inner-dimension))
                          (vector-set! c j (vector-ref coeffs (+ k j))))
                        (share a inner c))))
                  (let ((ugetter (a-ugetter a))
                        (usetter (a-usetter a)))
                    (lambda outer-idx
                      (%make-generalized
                        inner
                        (lambda inner-idx (apply ugetter (append outer-idx inner-idx)))
                        (and usetter
                             (lambda (v . inner-idx)
                               (apply usetter v (append outer-idx inner-idx))))))))
              #f)))))

    (define (%extract a domain)
      (if (a-sc a)
          (share a domain (a-coeffs a))
          (%make-generalized domain (a-ugetter a) (a-usetter a))))

    (define (array-extract a domain)
      (check-array "array-extract" a)
      (check-interval "array-extract" domain)
      (if (not (= (%dimension domain) (%dim a)))
          (fail "array-extract" "interval has the wrong dimension:" domain))
      (if (not (%subset? domain (a-domain a)))
          (fail "array-extract" "interval is not a subset of the domain of the array:" domain))
      (%extract a domain))

    (define (array-tile a s)
      (check-array "array-tile" a)
      (let* ((domain (a-domain a))
             (d (%dim a))
             (lb (lbs domain))
             (ub (ubs domain)))
        (if (not (and (vector? s) (= (vector-length s) d)))
            (fail "array-tile" "second argument is not a vector of the right length:" s))
        (do ((k 0 (+ k 1)))
            ((= k d))
          (let ((sk (vector-ref s k))
                (w (- (vector-ref ub k) (vector-ref lb k))))
            (if (not (if (= w 0)
                         (and (vector? sk) (> (vector-length sk) 0) (vector-all? zero? sk)
                              (vector-all? exact-integer? sk))
                         (or (positive-integer? sk)
                             (and (vector? sk)
                                  (vector-all? nonneg-integer? sk)
                                  (= (apply + (vector->list sk)) w)))))
                (fail "array-tile" "invalid slice specification for axis:" k sk))))
        ;; For every axis, cuts is either the slice width, or a vector of cut positions
        (let* ((cuts (vector-map
                       (lambda (sk l)
                         (if (vector? sk)
                             (let* ((n (vector-length sk))
                                    (res (make-vector (+ n 1) l)))
                               (do ((i 0 (+ i 1)))
                                   ((= i n) res)
                                 (vector-set! res (+ i 1) (+ (vector-ref res i) (vector-ref sk i)))))
                             sk))
                       s lb))
               (tiles (vector-map
                        (lambda (c l u)
                          (if (vector? c)
                              (- (vector-length c) 1)
                              (quotient (+ (- u l) c -1) c)))
                        cuts lb ub)))
          (%make-generalized
            (%interval (make-vector d 0) tiles)
            (lambda idx
              (let ((tlb (make-vector d))
                    (tub (make-vector d)))
                (do ((k 0 (+ k 1))
                     (idx idx (cdr idx)))
                    ((= k d))
                  (let ((c (vector-ref cuts k)) (j (car idx)))
                    (if (vector? c)
                        (begin
                          (vector-set! tlb k (vector-ref c j))
                          (vector-set! tub k (vector-ref c (+ j 1))))
                        (let ((l (+ (vector-ref lb k) (* j c))))
                          (vector-set! tlb k l)
                          (vector-set! tub k (min (+ l c) (vector-ref ub k)))))))
                (%extract a (%interval tlb tub))))
            #f))))

    (define (array-translate a translation)
      (check-array "array-translate" a)
      (if (not (translation? translation))
          (fail "array-translate" "second argument is not a translation:" translation))
      (if (not (= (vector-length translation) (%dim a)))
          (fail "array-translate" "translation has the wrong dimension:" translation))
      (let ((domain (%translate (a-domain a) translation)))
        (if (a-sc a)
            (let* ((coeffs (vector-copy (a-coeffs a)))
                   (d (vector-length translation)))
              (do ((k 0 (+ k 1)))
                  ((= k d))
                (vector-set! coeffs 0 (- (vector-ref coeffs 0)
                                         (* (vector-ref coeffs (+ k 1))
                                            (vector-ref translation k)))))
              (share a domain coeffs))
            (let ((tl (vector->list translation)))
              (transformed a domain (lambda (idx) (map - idx tl)))))))

    (define (inverse-permutation perm)
      (let* ((n (vector-length perm))
             (res (make-vector n)))
        (do ((i 0 (+ i 1)))
            ((= i n) res)
          (vector-set! res (vector-ref perm i) i))))

    (define (array-permute a perm)
      (check-array "array-permute" a)
      (if (not (permutation? perm))
          (fail "array-permute" "second argument is not a permutation:" perm))
      (if (not (= (vector-length perm) (%dim a)))
          (fail "array-permute" "permutation has the wrong dimension:" perm))
      (let ((domain (%permute (a-domain a) perm)))
        (if (a-sc a)
            (let* ((ocoeffs (a-coeffs a))
                   (d (vector-length perm))
                   (coeffs (make-vector (+ d 1) (vector-ref ocoeffs 0))))
              (do ((j 0 (+ j 1)))
                  ((= j d))
                (vector-set! coeffs (+ j 1) (vector-ref ocoeffs (+ (vector-ref perm j) 1))))
              (share a domain coeffs))
            (let ((inv (inverse-permutation perm)))
              (transformed a domain
                           (lambda (idx)
                             (let ((v (list->vector idx)))
                               (map (lambda (k) (vector-ref v k)) (vector->list inv)))))))))

    (define (array-reverse a . args)
      (check-array "array-reverse" a)
      (let* ((d (%dim a))
             (flip (if (pair? args) (car args) (make-vector d #t))))
        (if (not (and (vector? flip) (vector-all? boolean? flip)))
            (fail "array-reverse" "second argument is not a vector of booleans:" flip))
        (if (not (= (vector-length flip) d))
            (fail "array-reverse" "vector of booleans has the wrong length:" flip))
        (let* ((domain (a-domain a))
               (lb (lbs domain))
               (ub (ubs domain)))
          (if (a-sc a)
              (let ((coeffs (vector-copy (a-coeffs a))))
                (do ((k 0 (+ k 1)))
                    ((= k d))
                  (if (vector-ref flip k)
                      (let ((c (vector-ref coeffs (+ k 1))))
                        (vector-set! coeffs 0 (+ (vector-ref coeffs 0)
                                                 (* c (+ (vector-ref lb k) (vector-ref ub k) -1))))
                        (vector-set! coeffs (+ k 1) (- c)))))
                (share a domain coeffs))
              (let ((flips (vector->list flip))
                    (ls (vector->list lb))
                    (us (vector->list ub)))
                (transformed a domain
                             (lambda (idx)
                               (map (lambda (i f l u) (if f (- (+ l u -1) i) i))
                                    idx flips ls us))))))))

    (define (array-sample a scales)
      (check-array "array-sample" a)
      (let ((domain (a-domain a)))
        (if (not (vector-all? zero? (lbs domain)))
            (fail "array-sample" "lower bounds of the domain of the array are not zero:" domain))
        (if (not (and (vector? scales) (vector-all? positive-integer? scales)))
            (fail "array-sample" "second argument is not a vector of positive exact integers:"
                  scales))
        (if (not (= (vector-length scales) (%dim a)))
            (fail "array-sample" "scales have the wrong dimension:" scales))
        (let ((new-domain (%scale domain scales)))
          (if (a-sc a)
              (let ((coeffs (vector-copy (a-coeffs a))))
                (do ((k 0 (+ k 1)))
                    ((= k (vector-length scales)))
                  (vector-set! coeffs (+ k 1) (* (vector-ref coeffs (+ k 1))
                                                 (vector-ref scales k))))
                (share a new-domain coeffs))
              (let ((sl (vector->list scales)))
                (transformed a new-domain (lambda (idx) (map * idx sl))))))))

    ;; --- Combining arrays ---

    (define (array-outer-product op a1 a2)
      (if (not (procedure? op))
          (fail "array-outer-product" "first argument is not a procedure:" op))
      (check-array "array-outer-product" a1)
      (check-array "array-outer-product" a2)
      (let ((g1 (a-ugetter a1))
            (g2 (a-ugetter a2))
            (d1 (%dim a1)))
        (%make-generalized (%product (list (a-domain a1) (a-domain a2)))
                           (lambda idx
                             (op (apply g1 (list-take idx d1)) (apply g2 (list-drop idx d1))))
                           #f)))

    (define (array-inner-product a f g b)
      (check-array "array-inner-product" a)
      (if (not (procedure? f))
          (fail "array-inner-product" "second argument is not a procedure:" f))
      (if (not (procedure? g))
          (fail "array-inner-product" "third argument is not a procedure:" g))
      (check-array "array-inner-product" b)
      (let ((da (%dim a)) (db (%dim b)))
        (if (= da 0)
            (fail "array-inner-product" "first argument has dimension zero:" a))
        (if (= db 0)
            (fail "array-inner-product" "fourth argument has dimension zero:" b))
        (let ((dom-a (a-domain a)) (dom-b (a-domain b)))
          (if (not (and (= (vector-ref (lbs dom-a) (- da 1)) (vector-ref (lbs dom-b) 0))
                        (= (vector-ref (ubs dom-a) (- da 1)) (vector-ref (ubs dom-b) 0))))
              (fail "array-inner-product"
                    "bounds of the last axis of the first array and of the first axis of the second array differ:"
                    dom-a dom-b))
          (if (= (vector-ref (lbs dom-b) 0) (vector-ref (ubs dom-b) 0))
              (fail "array-inner-product" "width of the first axis of the fourth argument is zero:"
                    dom-b))
          (array-outer-product
            (lambda (x y) (array-reduce f (array-map g x y)))
            (array-copy (array-curry a 1))
            (array-copy (array-curry (array-permute b (index-rotate db 1)) 1))))))

    (define (check-same-domains who a arrays)
      (for-each (lambda (b)
                  (check-array who b)
                  (if (not (%interval= (a-domain a) (a-domain b)))
                      (fail who "arrays have different domains:" (a-domain a) (a-domain b))))
                arrays))

    ;; Returns an unchecked getter for an array whose elements are lists of the elements of
    ;; the given arrays
    (define (combined-getter f arrays)
      (let ((getters (map (lambda (a) (a-ugetter a)) arrays)))
        (if (null? (cdr getters))
            (let ((g (car getters)))
              (case (%dim (car arrays))
                ((1) (lambda (i) (f (g i))))
                ((2) (lambda (i j) (f (g i j))))
                (else (lambda idx (f (apply g idx))))))
            (lambda idx
              (apply f (map (lambda (g) (apply g idx)) getters))))))

    (define (array-map f a . arrays)
      (if (not (procedure? f))
          (fail "array-map" "first argument is not a procedure:" f))
      (check-array "array-map" a)
      (check-same-domains "array-map" a arrays)
      (%make-generalized (a-domain a) (combined-getter f (cons a arrays)) #f))

    (define (array-for-each f a . arrays)
      (if (not (procedure? f))
          (fail "array-for-each" "first argument is not a procedure:" f))
      (check-array "array-for-each" a)
      (check-same-domains "array-for-each" a arrays)
      (let ((domain (a-domain a)))
        (if (not (%empty? domain))
            (%fold-left (combined-getter f (cons a arrays))
                        ignore-result #f (lbs domain) (ubs domain)))
        (if #f #f)))

    (define (array-fold-left op id a . arrays)
      (if (not (procedure? op))
          (fail "array-fold-left" "first argument is not a procedure:" op))
      (check-array "array-fold-left" a)
      (check-same-domains "array-fold-left" a arrays)
      (let ((domain (a-domain a)))
        (if (%empty? domain)
            id
            (if (null? arrays)
                (%fold-left (a-ugetter a) op id (lbs domain) (ubs domain))
                (%fold-left (combined-getter list (cons a arrays))
                            (lambda (acc elems) (apply op acc elems))
                            id (lbs domain) (ubs domain))))))

    (define (array-fold-right op id a . arrays)
      (if (not (procedure? op))
          (fail "array-fold-right" "first argument is not a procedure:" op))
      (check-array "array-fold-right" a)
      (check-same-domains "array-fold-right" a arrays)
      (let ((domain (a-domain a)))
        (if (%empty? domain)
            id
            (let ((items (%collect (if (null? arrays)
                                       (a-ugetter a)
                                       (combined-getter list (cons a arrays)))
                                   (lbs domain) (ubs domain))))
              (if (null? arrays)
                  (do ((i (- (vector-length items) 1) (- i 1))
                       (acc id (op (vector-ref items i) acc)))
                      ((< i 0) acc))
                  (do ((i (- (vector-length items) 1) (- i 1))
                       (acc id (apply op (append (vector-ref items i) (list acc)))))
                      ((< i 0) acc)))))))

    (define (array-reduce op a)
      (if (not (procedure? op))
          (fail "array-reduce" "first argument is not a procedure:" op))
      (check-array "array-reduce" a)
      (let ((domain (a-domain a))
            (first (list 'first)))
        (if (%empty? domain)
            (fail "array-reduce" "cannot reduce an empty array:" a))
        (%fold-left (a-ugetter a)
                    (lambda (acc x) (if (eq? acc first) x (op acc x)))
                    first (lbs domain) (ubs domain))))

    (define (array-any pred a . arrays)
      (if (not (procedure? pred))
          (fail "array-any" "first argument is not a procedure:" pred))
      (check-array "array-any" a)
      (check-same-domains "array-any" a arrays)
      (let ((domain (a-domain a)))
        (and (not (%empty? domain))
             (%find (combined-getter pred (cons a arrays)) (lbs domain) (ubs domain)))))

    (define (array-every pred a . arrays)
      (if (not (procedure? pred))
          (fail "array-every" "first argument is not a procedure:" pred))
      (check-array "array-every" a)
      (check-same-domains "array-every" a arrays)
      (let ((domain (a-domain a)))
        (or (%empty? domain)
            (%every (combined-getter pred (cons a arrays)) (lbs domain) (ubs domain)))))

    ;; --- Conversions ---

    (define (%array->vector a)
      (let ((domain (a-domain a)))
        (if (%empty? domain)
            (vector)
            (%collect (a-ugetter a) (lbs domain) (ubs domain)))))

    (define (array->list a)
      (check-array "array->list" a)
      (vector->list (%array->vector a)))

    (define (array->vector a)
      (check-array "array->vector" a)
      (%array->vector a))

    (define (list->array domain lst . args)
      (check-interval "list->array" domain)
      (if (not (list? lst))
          (fail "list->array" "second argument is not a list:" lst))
      (let-values (((sc mutable? safe?) (optional-args "list->array" args #f)))
        (if (not (= (length lst) (%volume domain)))
            (fail "list->array" "length of list does not match the volume of the interval:"
                  lst domain))
        (vector->specialized "list->array" domain (list->vector lst) 0 sc mutable? safe?)))

    (define (vector->array domain vec . args)
      (check-interval "vector->array" domain)
      (if (not (vector? vec))
          (fail "vector->array" "second argument is not a vector:" vec))
      (let-values (((sc mutable? safe?) (optional-args "vector->array" args #f)))
        (if (not (= (vector-length vec) (%volume domain)))
            (fail "vector->array" "length of vector does not match the volume of the interval:"
                  vec domain))
        (vector->specialized "vector->array" domain vec 0 sc mutable? safe?)))

    ;; Folds `f` over the elements of the list or vector `seq`
    (define (seq-fold f acc seq)
      (if (vector? seq)
          (let ((n (vector-length seq)))
            (do ((i 0 (+ i 1))
                 (acc acc (f acc (vector-ref seq i))))
                ((= i n) acc)))
          (do ((seq seq (cdr seq))
               (acc acc (f acc (car seq))))
              ((null? seq) acc))))

    ;; Returns the widths of the nested data structure for dimension `d`, or #f if it does
    ;; not have the right shape; `seq?` and `seq-length` abstract over lists and vectors
    (define (nested-widths d data seq? seq-length)
      (if (= d 0)
          '()
          (and (seq? data)
               (let ((len (seq-length data)))
                 (cond ((= len 0) '())
                       ((= d 1) (list len))
                       (else
                         (let ((sub (seq-fold (lambda (acc x)
                                                (and acc
                                                     (let ((w (nested-widths (- d 1) x seq? seq-length)))
                                                       (and w
                                                            (or (eq? acc 'none) (equal? acc w))
                                                            w))))
                                              'none
                                              data)))
                           (and sub (cons len sub)))))))))

    (define (nested->array who d data seq? seq-length args)
      (if (not (nonneg-integer? d))
          (fail who "first argument is not a nonnegative exact integer:" d))
      (let-values (((sc mutable? safe?) (optional-args who args #f)))
        (let ((widths (nested-widths d data seq? seq-length)))
          (if (not widths)
              (fail who "data does not have the right shape for the dimension:" data d))
          (let* ((ub (list->vector (append widths (make-list (- d (length widths)) 0))))
                 (domain (%interval (make-vector d 0) ub))
                 (elems (make-vector (%volume domain))))
            ;; Store the elements in lexicographical order in `elems`
            (if (not (%empty? domain))
                (let fill ((d d) (data data) (i 0))
                  (if (= d 0)
                      (begin (vector-set! elems i data) (+ i 1))
                      (seq-fold (lambda (i x) (fill (- d 1) x i)) i data))))
            (vector->specialized who domain elems 0 sc mutable? safe?)))))

    (define (list*->array d nested-list . args)
      (nested->array "list*->array" d nested-list list? length args))

    (define (vector*->array d nested-vector . args)
      (nested->array "vector*->array" d nested-vector vector? vector-length args))

    ;; Returns the nested lists (or vectors, if `vec?` is true) of the elements of `a`
    (define (array->nested a vec?)
      (let* ((domain (a-domain a))
             (g (a-ugetter a))
             (lb (lbs domain))
             (ub (ubs domain))
             (d (vector-length lb)))
        (if (= d 0)
            (g)
            (let rec ((k 0) (rev '()))
              (if (= k d)
                  (apply g (reverse rev))
                  (let ((u (vector-ref ub k)))
                    (let loop ((i (vector-ref lb k)) (acc empty-accumulator))
                      (if (< i u)
                          (loop (+ i 1) (accumulate acc (rec (+ k 1) (cons i rev))))
                          (let ((res (accumulator->vector acc)))
                            (if vec? res (vector->list res)))))))))))

    (define (array->list* a)
      (check-array "array->list*" a)
      (if (and (%empty? (a-domain a)) (= 0 (vector-ref (%widths (a-domain a)) 0)))
          '()
          (array->nested a #f)))

    (define (array->vector* a)
      (check-array "array->vector*" a)
      (if (and (%empty? (a-domain a)) (= 0 (vector-ref (%widths (a-domain a)) 0)))
          (vector)
          (array->nested a #t)))

    (define (array-assign! dest source)
      (check-mutable-array "array-assign!" dest)
      (check-array "array-assign!" source)
      (if (not (%interval= (a-domain dest) (a-domain source)))
          (fail "array-assign!" "destination and source have different domains:"
                (a-domain dest) (a-domain source)))
      (let ((domain (a-domain dest))
            (setter (a-usetter dest))
            (getter (a-ugetter source))
            (checker (and (a-sc dest) (sc-checker (a-sc dest)))))
        (if (not (%empty? domain))
            (%fold-left/list (lambda (idx)
                               (let ((v (apply getter idx)))
                                 (if (and checker (not (checker v)))
                                     (fail "array-assign!" "value cannot be stored in destination:" v))
                                 (apply setter v idx)))
                             ignore-result #f (lbs domain) (ubs domain)))
        (if #f #f)))

    ;; --- Stacking, appending, blocks ---

    (define (check-array-list who arrays)
      (if (not (and (list? arrays) (pair? arrays) (list-all? array? arrays)))
          (fail who "argument is not a nonempty list of arrays:" arrays)))

    (define (%array-stack who k arrays args call/cc-safe?)
      (check-array-list who arrays)
      (let* ((a (car arrays))
             (domain (a-domain a))
             (d (%dim a)))
        (if (not (and (exact-integer? k) (<= 0 k d)))
            (fail who "invalid axis:" k))
        (for-each (lambda (b)
                    (if (not (%interval= domain (a-domain b)))
                        (fail who "arrays have different domains:" domain (a-domain b))))
                  (cdr arrays))
        (let-values (((sc mutable? safe?) (optional-args who args #f)))
          (let* ((lb (vector->list (lbs domain)))
                 (ub (vector->list (ubs domain)))
                 (new-domain (%interval (list->vector (append (list-take lb k) (list 0) (list-drop lb k)))
                                        (list->vector (append (list-take ub k)
                                                              (list (length arrays))
                                                              (list-drop ub k)))))
                 (getters (list->vector (map (lambda (a) (a-ugetter a)) arrays))))
            (tabulate-array who
                      new-domain
                      (lambda idx
                        (let ((i (list-ref idx k)))
                          (apply (vector-ref getters i) (append (list-take idx k) (list-drop idx (+ k 1))))))
                      sc mutable? safe? call/cc-safe?)))))

    (define (array-stack k arrays . args)
      (%array-stack "array-stack" k arrays args #t))

    (define (array-stack! k arrays . args)
      (%array-stack "array-stack!" k arrays args #f))

    ;; Returns the index of the segment containing `i`, given a vector of increasing segment
    ;; boundaries `cuts` (binary search)
    (define (find-segment cuts i)
      (let loop ((lo 0) (hi (- (vector-length cuts) 1)))
        (if (<= (- hi lo) 1)
            lo
            (let ((mid (quotient (+ lo hi) 2)))
              (if (< i (vector-ref cuts mid)) (loop lo mid) (loop mid hi))))))

    (define (%array-append who k arrays args call/cc-safe?)
      (check-array-list who arrays)
      (let* ((a (car arrays))
             (d (%dim a)))
        (for-each (lambda (b)
                    (if (not (= (%dim b) d))
                        (fail who "arrays have different dimensions:" a b)))
                  (cdr arrays))
        (if (not (and (exact-integer? k) (< -1 k d)))
            (fail who "invalid axis:" k))
        (let ((domain (a-domain a)))
          (for-each (lambda (b)
                      (let ((bdom (a-domain b)))
                        (do ((i 0 (+ i 1)))
                            ((= i d))
                          (if (and (not (= i k))
                                   (not (and (= (vector-ref (lbs domain) i) (vector-ref (lbs bdom) i))
                                             (= (vector-ref (ubs domain) i) (vector-ref (ubs bdom) i)))))
                              (fail who "domains of arrays differ in other axes than the given axis:"
                                    domain bdom)))))
                    (cdr arrays))
          (let-values (((sc mutable? safe?) (optional-args who args #f)))
            (let* ((n (length arrays))
                   (cuts (make-vector (+ n 1) 0))
                   (getters (list->vector (map (lambda (b) (a-ugetter b)) arrays)))
                   (starts (list->vector (map (lambda (b) (vector-ref (lbs (a-domain b)) k)) arrays))))
              (do ((i 0 (+ i 1))
                   (bs arrays (cdr bs)))
                  ((= i n))
                (let ((bdom (a-domain (car bs))))
                  (vector-set! cuts (+ i 1) (+ (vector-ref cuts i)
                                               (- (vector-ref (ubs bdom) k) (vector-ref (lbs bdom) k))))))
              (let ((lb (vector-copy (lbs domain)))
                    (ub (vector-copy (ubs domain))))
                (vector-set! lb k 0)
                (vector-set! ub k (vector-ref cuts n))
                (tabulate-array who
                          (%interval lb ub)
                          (lambda idx
                            (let* ((i (list-ref idx k))
                                   (j (find-segment cuts i))
                                   (ik (+ (- i (vector-ref cuts j)) (vector-ref starts j))))
                              (apply (vector-ref getters j)
                                     (append (list-take idx k) (list ik) (list-drop idx (+ k 1))))))
                          sc mutable? safe? call/cc-safe?)))))))

    (define (array-append k arrays . args)
      (%array-append "array-append" k arrays args #t))

    (define (array-append! k arrays . args)
      (%array-append "array-append!" k arrays args #f))

    ;; Returns a specialized copy of the array of arrays `a` whose elements are all arrays
    (define (copy-array-of-arrays who a)
      (check-array who a)
      (if (%empty? (a-domain a))
          (fail who "argument is an empty array:" a))
      (let ((c (if (a-sc a) a (tabulate-array who (a-domain a) (a-ugetter a) generic-storage-class #f #f #t))))
        (if (not (array-every array? c))
            (fail who "argument is not an array of arrays:" a))
        c))

    (define (%array-decurry who a args call/cc-safe?)
      (let-values (((sc mutable? safe?) (optional-args who args #f)))
        (let* ((c (copy-array-of-arrays who a))
               (outer (a-domain c))
               (og (a-ugetter c))
               (od (%dimension outer))
               (first (apply og (vector->list (lbs outer))))
               (inner (a-domain first)))
          (array-for-each (lambda (x)
                            (if (not (%interval= (a-domain x) inner))
                                (fail who "array elements have different domains:" inner (a-domain x))))
                          c)
          (tabulate-array who
                    (%product (list outer inner))
                    (lambda idx
                      (apply (a-ugetter (apply og (list-take idx od))) (list-drop idx od)))
                    sc mutable? safe? call/cc-safe?))))

    (define (array-decurry a . args)
      (%array-decurry "array-decurry" a args #t))

    (define (array-decurry! a . args)
      (%array-decurry "array-decurry!" a args #f))

    (define (%array-block who a args call/cc-safe?)
      (let-values (((sc mutable? safe?) (optional-args who args #f)))
        (let* ((c (copy-array-of-arrays who a))
               (domain (a-domain c))
               (og (a-ugetter c))
               (d (%dimension domain))
               (lb (lbs domain))
               (ub (ubs domain)))
          (array-for-each (lambda (x)
                            (if (not (= (%dim x) d))
                                (fail who "array elements have the wrong dimension:" x)))
                          c)
          ;; Compute the cut positions along every axis from the widths of the arrays at the
          ;; "corner" positions of each axis, and verify that all arrays fit
          (let ((cuts
                  (let ((corner (vector->list lb)))
                    (list->vector
                      (map (lambda (k)
                             (let* ((n (- (vector-ref ub k) (vector-ref lb k)))
                                    (res (make-vector (+ n 1) 0)))
                               (do ((i 0 (+ i 1)))
                                   ((= i n) res)
                                 (let ((x (apply og (append (list-take corner k)
                                                            (list (+ (vector-ref lb k) i))
                                                            (list-drop corner (+ k 1))))))
                                   (vector-set! res (+ i 1)
                                                (+ (vector-ref res i)
                                                   (vector-ref (%widths (a-domain x)) k)))))))
                           (iota d))))))
            (%fold-left/list
              (lambda (idx)
                (let ((widths (%widths (a-domain (apply og idx)))))
                  (do ((k 0 (+ k 1))
                       (idx idx (cdr idx)))
                      ((= k d))
                    (let* ((j (- (car idx) (vector-ref lb k)))
                           (cut (vector-ref cuts k)))
                      (if (not (= (vector-ref widths k)
                                  (- (vector-ref cut (+ j 1)) (vector-ref cut j))))
                          (fail who "array elements do not fit together:" a))))))
              ignore-result #f lb ub)
            (tabulate-array
              who
              (%interval (make-vector d 0)
                         (vector-map (lambda (cut) (vector-ref cut (- (vector-length cut) 1))) cuts))
              (lambda idx
                ;; Find the block containing the multi-index and the position within it
                (let loop ((k 0) (idx idx) (bidx '()) (eidx '()))
                  (if (= k d)
                      (let* ((x (apply og (reverse bidx)))
                             (xlb (lbs (a-domain x))))
                        (apply (a-ugetter x)
                               (let loop2 ((k (- d 1)) (eidx eidx) (res '()))
                                 (if (< k 0)
                                     res
                                     (loop2 (- k 1) (cdr eidx)
                                            (cons (+ (car eidx) (vector-ref xlb k)) res))))))
                      (let* ((cut (vector-ref cuts k))
                             (i (car idx))
                             (j (find-segment cut i)))
                        (loop (+ k 1) (cdr idx)
                              (cons (+ j (vector-ref lb k)) bidx)
                              (cons (- i (vector-ref cut j)) eidx))))))
              sc mutable? safe? call/cc-safe?)))))

    (define (array-block a . args)
      (%array-block "array-block" a args #t))

    (define (array-block! a . args)
      (%array-block "array-block!" a args #f))

    ;; --- Element access ---

    (define (array-ref a . idx)
      (check-array "array-ref" a)
      (apply (a-getter a) idx))

    (define (array-set! a v . idx)
      (check-mutable-array "array-set!" a)
      (apply (a-setter a) v idx))

    ;; --- Reshaping ---

    (define (vector-filter-axes v sides)
      (list->vector
        (let loop ((k (- (vector-length v) 1)) (res '()))
          (if (< k 0)
              res
              (loop (- k 1) (if (= (vector-ref sides k) 1) res (cons (vector-ref v k) res)))))))

    ;; Returns the coefficients for the reshaped array, or #f if the array cannot be reshaped
    ;; without copying (following the algorithm used by NumPy)
    (define (reshape-coeffs a new-domain)
      (let* ((domain (a-domain a))
             (coeffs (a-coeffs a))
             (d (%dimension domain))
             (sides (%widths domain))
             (strides (vector-copy coeffs 1))
             (base (do ((k 0 (+ k 1))
                        (pos (vector-ref coeffs 0)
                             (+ pos (* (vector-ref coeffs (+ k 1)) (vector-ref (lbs domain) k)))))
                       ((= k d) pos)))
             (olddims (vector-filter-axes sides sides))
             (oldstrides (vector-filter-axes strides sides))
             (oldnd (vector-length olddims))
             (newdims (%widths new-domain))
             (newnd (vector-length newdims))
             (newstrides (make-vector newnd 0)))
        (and
          (let loop1 ((oi 0) (oj 1) (ni 0) (nj 1))
            (if (and (< ni newnd) (< oi oldnd))
                (let loop2 ((nj nj) (oj oj) (np (vector-ref newdims ni)) (op (vector-ref olddims oi)))
                  (if (not (= np op))
                      (if (< np op)
                          (loop2 (+ nj 1) oj (* np (vector-ref newdims nj)) op)
                          (loop2 nj (+ oj 1) np (* op (vector-ref olddims oj))))
                      (let loop3 ((ok oi))
                        (if (< ok (- oj 1))
                            (and (= (vector-ref oldstrides ok)
                                    (* (vector-ref olddims (+ ok 1))
                                       (vector-ref oldstrides (+ ok 1))))
                                 (loop3 (+ ok 1)))
                            (begin
                              (vector-set! newstrides (- nj 1) (vector-ref oldstrides (- oj 1)))
                              (do ((nk (- nj 1) (- nk 1)))
                                  ((<= nk ni))
                                (vector-set! newstrides (- nk 1)
                                             (* (vector-ref newstrides nk) (vector-ref newdims nk))))
                              (loop1 oj (+ oj 1) nj (+ nj 1)))))))
                #t))
          (let ((res (make-vector (+ newnd 1) 0))
                (nlb (lbs new-domain)))
            (do ((k 0 (+ k 1))
                 (c0 base (- c0 (* (vector-ref newstrides k) (vector-ref nlb k)))))
                ((= k newnd) (vector-set! res 0 c0) res)
              (vector-set! res (+ k 1) (vector-ref newstrides k)))))))

    (define (specialized-array-reshape a new-domain . args)
      (check-specialized-array "specialized-array-reshape" a)
      (check-interval "specialized-array-reshape" new-domain)
      (let ((copy-on-failure? (if (pair? args) (car args) #f)))
        (boolean-or-fail "specialized-array-reshape" copy-on-failure?)
        (if (not (= (%volume (a-domain a)) (%volume new-domain)))
            (fail "specialized-array-reshape" "volumes of the domains differ:"
                  (a-domain a) new-domain))
        (if (%empty? new-domain)
            (share a new-domain (make-vector (+ (%dimension new-domain) 1) 0))
            (let ((coeffs (reshape-coeffs a new-domain)))
              (cond (coeffs
                      (share a new-domain coeffs))
                    (copy-on-failure?
                      (let ((c (tabulate-array "specialized-array-reshape" (a-domain a) (a-ugetter a)
                                         (a-sc a) (and (a-setter a) #t) (a-safe? a) #f)))
                        (share c new-domain (reshape-coeffs c new-domain))))
                    (else
                      (fail "specialized-array-reshape"
                            "cannot reshape array to new domain without copying:"
                            (a-domain a) new-domain)))))))
  )
)
