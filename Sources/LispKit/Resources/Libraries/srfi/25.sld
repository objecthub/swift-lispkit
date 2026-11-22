;;; SRFI 25
;;; Multi-dimensional Array Primitives
;;;
;;; A core set of procedures for creating and manipulating heterogeneous multidimensional
;;; arrays. The design is consistent with the rest of Scheme and independent of other
;;; container data types. It provides easy sharing of parts of an array as other arrays
;;; without copying, encouraging a declarative style of programming.
;;;
;;; The specification is based on an original contribution by Alan Bawden in 1993.
;;;
;;; Copyright © 2001 Jussi Piitulainen. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify,
;;; merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to the following
;;; conditions:
;;; The above copyright notice and this permission notice shall be included in all copies
;;; or substantial portions of the Software.
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
;;;   This is a new implementation of the SRFI 25 API, not a port of the reference
;;;   implementation. An array is an object of a distinct type. Its elements are stored in a
;;;   backing vector, which is shared between an array and all arrays derived from it via
;;;   `share-array`. An affine index mapping, represented by a vector of coefficients, maps
;;;   valid indices of an array to positions in the backing vector. `share-array` composes
;;;   the user-provided affine mapping with the mapping of the original array. It calls the
;;;   mapping only at valid indices of the new array (its lower corner and its neighbours
;;;   along every dimension), and it signals an error if the new array would refer to
;;;   elements outside of the original array. Indices are always checked; an invalid index
;;;   is an error. Arrays are printed compactly, without their elements. Vectors are not
;;;   arrays.

(define-library (srfi 25)

  (export array?
          make-array
          shape
          array
          array-rank
          array-start
          array-end
          array-ref
          array-set!
          share-array)

  (import (lispkit base))

  (begin

    ;; === The array type ===

    ;; An array is an object of a new type whose payload is a procedure returning the state
    ;; of the array; this way, arrays are printed compactly instead of showing their contents.
    ;; The state is a vector with the following slots:
    ;;   0. the backing vector,
    ;;   1. the rank d of the array,
    ;;   2. a vector of length 2d with the bounds b0 e0 b1 e1 ... of the array, and
    ;;   3. a vector of length d + 1 with the coefficients n0 n1 ... nd of the index mapping:
    ;;      element k1 ... kd is stored at position n0 + n1 * k1 + ... + nd * kd of the
    ;;      backing vector.
    (define-values (array-type-tag new-array-object array? array-payload make-array-subtype)
      (make-type 'array))

    (define-syntax st-data
      (syntax-rules () ((_ st) (vector-ref st 0))))
    (define-syntax st-rank
      (syntax-rules () ((_ st) (vector-ref st 1))))
    (define-syntax st-bounds
      (syntax-rules () ((_ st) (vector-ref st 2))))
    (define-syntax st-coeffs
      (syntax-rules () ((_ st) (vector-ref st 3))))

    (define (new-array data rank bounds coeffs)
      (let ((state (vector data rank bounds coeffs)))
        (new-array-object (lambda () state))))

    ;; Returns the state of `obj`; signals an error if `obj` is not an array
    (define (state who obj)
      (if (array? obj)
          ((array-payload obj))
          (error (string-append who ": argument is not an array:") obj)))

    ;; === Shapes ===

    ;; Returns the number of elements of an array with the given bounds
    (define (bounds-size bounds)
      (do ((i 0 (+ i 2))
           (size 1 (* size (- (vector-ref bounds (+ i 1)) (vector-ref bounds i)))))
          ((>= i (vector-length bounds)) size)))

    ;; Returns a new array with the given bounds whose elements are stored in row major
    ;; order in a fresh backing vector; `fill` is a list containing the initial value of
    ;; all elements, or `()` if the elements are not initialized
    (define (fresh-array bounds fill)
      (let* ((rank (quotient (vector-length bounds) 2))
             (size (bounds-size bounds))
             (coeffs (make-vector (+ rank 1) 0)))
        (do ((k (- rank 1) (- k 1))
             (stride 1 (* stride (- (vector-ref bounds (+ k k 1)) (vector-ref bounds (+ k k)))))
             (offset 0 (- offset (* stride (vector-ref bounds (+ k k))))))
            ((< k 0) (vector-set! coeffs 0 offset))
          (vector-set! coeffs (+ k 1) stride))
        (new-array (if (pair? fill) (make-vector size (car fill)) (make-vector size))
                   rank
                   bounds
                   coeffs)))

    ;; Returns the element at row `r` and column `c` of the two-dimensional array with
    ;; state `st` without checking the index
    (define (ref2 st r c)
      (let ((coeffs (st-coeffs st)))
        (vector-ref (st-data st)
                    (+ (vector-ref coeffs 0) (* (vector-ref coeffs 1) r) (* (vector-ref coeffs 2) c)))))

    ;; Returns a new vector b0 e0 b1 e1 ... with the bounds specified by the shape `shp`;
    ;; signals an error if `shp` is not a valid shape
    (define (shape->bounds who shp)
      (if (not (array? shp))
          (error (string-append who ": argument is not a shape:") shp))
      (let* ((st ((array-payload shp)))
             (sb (st-bounds st)))
        (if (not (and (eqv? (st-rank st) 2)
                      (eqv? (vector-ref sb 0) 0)
                      (eqv? (vector-ref sb 2) 0)
                      (eqv? (vector-ref sb 3) 2)))
            (error (string-append who ": argument is not a shape:") shp))
        (let* ((rank (vector-ref sb 1))
               (bounds (make-vector (* 2 rank))))
          (do ((k 0 (+ k 1)))
              ((= k rank) bounds)
            (let ((b (ref2 st k 0))
                  (e (ref2 st k 1)))
              (if (not (and (exact-integer? b) (exact-integer? e) (<= b e)))
                  (error (string-append who ": invalid bounds in shape:") shp))
              (vector-set! bounds (+ k k) b)
              (vector-set! bounds (+ k k 1) e))))))

    (define (shape . bounds)
      (let ((n (length bounds)))
        (if (odd? n)
            (error "shape: odd number of bounds:" bounds))
        (let ((data (list->vector bounds)))
          (do ((i 0 (+ i 2)))
              ((>= i n))
            (let ((b (vector-ref data i))
                  (e (vector-ref data (+ i 1))))
              (if (not (and (exact-integer? b) (exact-integer? e) (<= b e)))
                  (error "shape: invalid bounds:" b e))))
          (new-array data 2 (vector 0 (quotient n 2) 0 2) (vector 0 2 1)))))

    ;; === Constructors ===

    (define (make-array shp . obj)
      (fresh-array (shape->bounds "make-array" shp) obj))

    (define (array shp . objs)
      (let* ((res (fresh-array (shape->bounds "array" shp) '()))
             (data (st-data ((array-payload res))))
             (size (vector-length data)))
        (do ((i 0 (+ i 1))
             (objs objs (cdr objs)))
            ((or (= i size) (null? objs))
             (if (or (< i size) (pair? objs))
                 (error "array: number of elements does not match shape:" shp))
             res)
          (vector-set! data i (car objs)))))

    ;; === Accessors ===

    (define (array-rank a)
      (st-rank (state "array-rank" a)))

    ;; Returns the bounds of the array with state `st`, after checking that `k` refers to
    ;; one of its dimensions
    (define (dim-bounds who st k)
      (if (not (and (exact-integer? k) (<= 0 k) (< k (st-rank st))))
          (error (string-append who ": invalid dimension:") k))
      (st-bounds st))

    (define (array-start a k)
      (let ((st (state "array-start" a)))
        (vector-ref (dim-bounds "array-start" st k) (+ k k))))

    (define (array-end a k)
      (let ((st (state "array-end" a)))
        (vector-ref (dim-bounds "array-end" st k) (+ k k 1))))

    ;; === Indexing ===

    ;; Returns the term contributed by index `i` along dimension `k` to the position in the
    ;; backing vector of the array with state `st`; signals an error if `i` is out of bounds
    (define (index-term who st k i)
      (let ((bounds (st-bounds st)))
        (if (not (and (exact-integer? i)
                      (<= (vector-ref bounds (+ k k)) i)
                      (< i (vector-ref bounds (+ k k 1)))))
            (error (string-append who ": index out of bounds:") i))
        (* (vector-ref (st-coeffs st) (+ k 1)) i)))

    (define (index-count-error who st)
      (error (string-append who ": wrong number of indices for array of rank")
             (st-rank st)))

    ;; Returns the position in the backing vector of the element of the array with state
    ;; `st` whose index is given by the first `n` elements of list `ks`
    (define (list-position who st ks n)
      (if (not (= n (st-rank st)))
          (index-count-error who st))
      (do ((k 0 (+ k 1))
           (ks ks (cdr ks))
           (pos (vector-ref (st-coeffs st) 0) (+ pos (index-term who st k (car ks)))))
          ((= k n) pos)))

    ;; Returns the position in the backing vector of the element of the array with state
    ;; `st` whose index is given by the index object `ind`, a vector or a 0-based
    ;; one-dimensional array
    (define (object-position who st ind)
      (let ((rank (st-rank st)))
        (if (vector? ind)
            (begin
              (if (not (= (vector-length ind) rank))
                  (index-count-error who st))
              (do ((k 0 (+ k 1))
                   (pos (vector-ref (st-coeffs st) 0)
                        (+ pos (index-term who st k (vector-ref ind k)))))
                  ((= k rank) pos)))
            (let* ((ist (state who ind))
                   (ib (st-bounds ist)))
              (if (not (and (eqv? (st-rank ist) 1) (eqv? (vector-ref ib 0) 0)))
                  (error (string-append who ": invalid index object:") ind))
              (if (not (= (vector-ref ib 1) rank))
                  (index-count-error who st))
              (let ((idata (st-data ist))
                    (i0 (vector-ref (st-coeffs ist) 0))
                    (i1 (vector-ref (st-coeffs ist) 1)))
                (do ((k 0 (+ k 1))
                     (pos (vector-ref (st-coeffs st) 0)
                          (+ pos (index-term who st k (vector-ref idata (+ i0 (* i1 k)))))))
                    ((= k rank) pos)))))))

    (define (index-object? obj)
      (or (vector? obj) (array? obj)))

    (define (array-ref a . ks)
      (let ((st (state "array-ref" a)))
        (vector-ref (st-data st)
                    (if (and (pair? ks) (null? (cdr ks)) (index-object? (car ks)))
                        (object-position "array-ref" st (car ks))
                        (list-position "array-ref" st ks (length ks))))))

    (define (array-set! a . args)
      (let ((st (state "array-set!" a)))
        (if (null? args)
            (error "array-set!: missing value to store in array" a))
        (let ((n (- (length args) 1)))
          (if (and (= n 1) (index-object? (car args)))
              (vector-set! (st-data st)
                           (object-position "array-set!" st (car args))
                           (cadr args))
              (vector-set! (st-data st)
                           (list-position "array-set!" st args n)
                           (list-ref args n))))))

    ;; === Sharing ===

    ;; Calls `proc` with the indices in vector `ks` and returns the resulting indices of the
    ;; original array, which has rank `rank`, as a vector
    (define (apply-mapping proc ks rank)
      (call-with-values
        (lambda () (apply proc (vector->list ks)))
        (lambda js
          (let ((res (list->vector js)))
            (if (not (= (vector-length res) rank))
                (error "share-array: mapping returns wrong number of indices:" js))
            (do ((i 0 (+ i 1)))
                ((= i rank) res)
              (if (not (exact-integer? (vector-ref res i)))
                  (error "share-array: mapping returns non-integer index:" (vector-ref res i))))))))

    (define (share-array a shp proc)
      (let* ((st (state "share-array" a))
             (bounds (shape->bounds "share-array" shp))
             (rank (quotient (vector-length bounds) 2))
             (orank (st-rank st))
             (obounds (st-bounds st))
             (ocoeffs (st-coeffs st))
             (coeffs (make-vector (+ rank 1) 0)))
        ;; The coefficients of an empty array are irrelevant since it has no valid index
        (if (> (bounds-size bounds) 0)
            ;; Determine the affine mapping by calling `proc` at the lower corner `lo` of
            ;; the new array and at `lo + e_k` for every dimension k of length > 1; the
            ;; mapping along dimensions of length 1 is irrelevant
            (let* ((lo (make-vector rank))
                   (lin (make-vector (* rank orank) 0)))  ; lin[k * orank + i]
              (do ((k 0 (+ k 1)))
                  ((= k rank))
                (vector-set! lo k (vector-ref bounds (+ k k))))
              (let ((base (apply-mapping proc lo orank)))
                (do ((k 0 (+ k 1)))
                    ((= k rank))
                  (if (> (- (vector-ref bounds (+ k k 1)) (vector-ref bounds (+ k k))) 1)
                      (let ((ks (vector-copy lo)))
                        (vector-set! ks k (+ (vector-ref ks k) 1))
                        (let ((js (apply-mapping proc ks orank)))
                          (do ((i 0 (+ i 1)))
                              ((= i orank))
                            (vector-set! lin (+ (* k orank) i)
                                         (- (vector-ref js i) (vector-ref base i))))))))
                ;; Check that the corners of the new array map into the original array
                (do ((i 0 (+ i 1)))
                    ((= i orank))
                  (do ((k 0 (+ k 1))
                       (mn (vector-ref base i))
                       (mx (vector-ref base i)))
                      ((= k rank)
                       (if (not (and (<= (vector-ref obounds (+ i i)) mn)
                                     (< mx (vector-ref obounds (+ i i 1)))))
                           (error "share-array: mapping exceeds bounds of array" a)))
                    (let ((d (* (vector-ref lin (+ (* k orank) i))
                                (- (vector-ref bounds (+ k k 1)) (vector-ref bounds (+ k k)) 1))))
                      (if (negative? d)
                          (set! mn (+ mn d))
                          (set! mx (+ mx d))))))
                ;; Compose the mapping with the index mapping of the original array
                (let ((pos (do ((i 0 (+ i 1))
                                (pos (vector-ref ocoeffs 0)
                                     (+ pos (* (vector-ref ocoeffs (+ i 1)) (vector-ref base i)))))
                               ((= i orank) pos))))
                  (do ((k 0 (+ k 1))
                       (offset pos))
                      ((= k rank) (vector-set! coeffs 0 offset))
                    (let ((c (do ((i 0 (+ i 1))
                                  (c 0 (+ c (* (vector-ref ocoeffs (+ i 1))
                                               (vector-ref lin (+ (* k orank) i))))))
                                 ((= i orank) c))))
                      (vector-set! coeffs (+ k 1) c)
                      (set! offset (- offset (* c (vector-ref lo k))))))))))
        (new-array (st-data st) rank bounds coeffs)))
  )
)
