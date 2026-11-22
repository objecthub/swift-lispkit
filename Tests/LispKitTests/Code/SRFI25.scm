;;; SRFI25.scm
;;; Regression test data
;;;
;;; This test suite is based on the SRFI 25 test suite by Jussi Piitulainen. It contains the
;;; tests of the SRFI 25 reference implementation, the examples of the SRFI document, and
;;; additional tests covering shared arrays, index objects, shapes and error handling.
;;;
;;; Copyright © 2001 Jussi Piitulainen. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
;;; to whom the Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies or
;;; substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
;;; PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
;;; FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
;;; OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2026 Matthias Zenger. All rights reserved.

(
  "Shape"
  #t
  (import (srfi 25))
  (and (array? (shape))
       (array? (shape -1 -1))
       (array? (shape -1 0))
       (array? (shape -1 1))
       (array? (shape 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)))
)

(
  "Make-array"
  #t
  (and (array? (make-array (shape)))
       (array? (make-array (shape) *))
       (array? (make-array (shape -1 -1)))
       (array? (make-array (shape -1 -1) *))
       (array? (make-array (shape -1 1)))
       (array? (make-array (shape 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4) *)))
)

(
  "Array"
  #t
  (and (array? (array (shape) *))
       (array? (array (shape -1 -1)))
       (array? (array (shape -1 1) * *))
       (array? (array (shape 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8) *)))
)

(
  "Array-rank of shape"
  (2 2 2 2)
  (list (array-rank (shape))
        (array-rank (shape -1 -1))
        (array-rank (shape -1 1))
        (array-rank (shape 1 2 3 4 5 6 7 8)))
)

(
  "Array-rank of make-array"
  (0 1 1 4)
  (list (array-rank (make-array (shape)))
        (array-rank (make-array (shape -1 -1)))
        (array-rank (make-array (shape -1 1)))
        (array-rank (make-array (shape 1 2 3 4 5 6 7 8))))
)

(
  "Array-rank of array"
  (0 1 1 4)
  (list (array-rank (array (shape) *))
        (array-rank (array (shape -1 -1)))
        (array-rank (array (shape -1 1) * *))
        (array-rank (array (shape 1 2 3 4 5 6 7 8) *)))
)

(
  "Array-start of shape"
  (0 0 0 0 0 0)
  (list (array-start (shape -1 -1) 0)
        (array-start (shape -1 -1) 1)
        (array-start (shape -1 1) 0)
        (array-start (shape -1 1) 1)
        (array-start (shape 1 2 3 4 5 6 7 8) 0)
        (array-start (shape 1 2 3 4 5 6 7 8) 1))
)

(
  "Array-end of shape"
  (1 2 1 2 4 2)
  (list (array-end (shape -1 -1) 0)
        (array-end (shape -1 -1) 1)
        (array-end (shape -1 1) 0)
        (array-end (shape -1 1) 1)
        (array-end (shape 1 2 3 4 5 6 7 8) 0)
        (array-end (shape 1 2 3 4 5 6 7 8) 1))
)

(
  "Array-start of make-array"
  (-1 -1 1 3 5 7)
  (list (array-start (make-array (shape -1 -1)) 0)
        (array-start (make-array (shape -1 1)) 0)
        (array-start (make-array (shape 1 2 3 4 5 6 7 8)) 0)
        (array-start (make-array (shape 1 2 3 4 5 6 7 8)) 1)
        (array-start (make-array (shape 1 2 3 4 5 6 7 8)) 2)
        (array-start (make-array (shape 1 2 3 4 5 6 7 8)) 3))
)

(
  "Array-end of make-array"
  (-1 1 2 4 6 8)
  (list (array-end (make-array (shape -1 -1)) 0)
        (array-end (make-array (shape -1 1)) 0)
        (array-end (make-array (shape 1 2 3 4 5 6 7 8)) 0)
        (array-end (make-array (shape 1 2 3 4 5 6 7 8)) 1)
        (array-end (make-array (shape 1 2 3 4 5 6 7 8)) 2)
        (array-end (make-array (shape 1 2 3 4 5 6 7 8)) 3))
)

(
  "Array-start of array"
  (-1 -1 1 3 5 7)
  (list (array-start (array (shape -1 -1)) 0)
        (array-start (array (shape -1 1) * *) 0)
        (array-start (array (shape 1 2 3 4 5 6 7 8) *) 0)
        (array-start (array (shape 1 2 3 4 5 6 7 8) *) 1)
        (array-start (array (shape 1 2 3 4 5 6 7 8) *) 2)
        (array-start (array (shape 1 2 3 4 5 6 7 8) *) 3))
)

(
  "Array-end of array"
  (-1 1 2 4 6 8)
  (list (array-end (array (shape -1 -1)) 0)
        (array-end (array (shape -1 1) * *) 0)
        (array-end (array (shape 1 2 3 4 5 6 7 8) *) 0)
        (array-end (array (shape 1 2 3 4 5 6 7 8) *) 1)
        (array-end (array (shape 1 2 3 4 5 6 7 8) *) 2)
        (array-end (array (shape 1 2 3 4 5 6 7 8) *) 3))
)

(
  "Array-ref of make-array with arguments"
  (a b c d)
  (list (array-ref (make-array (shape) 'a))
        (array-ref (make-array (shape -1 1) 'b) -1)
        (array-ref (make-array (shape -1 1) 'c) 0)
        (array-ref (make-array (shape 1 2 3 4 5 6 7 8) 'd) 1 3 5 7))
)

(
  "Array-ref of make-array with vector"
  (a b c d)
  (list (array-ref (make-array (shape) 'a) '#())
        (array-ref (make-array (shape -1 1) 'b) '#(-1))
        (array-ref (make-array (shape -1 1) 'c) '#(0))
        (array-ref (make-array (shape 1 2 3 4 5 6 7 8) 'd) '#(1 3 5 7)))
)

(
  "Array-ref of make-array with array"
  (a b c d)
  (list (array-ref (make-array (shape) 'a) (array (shape 0 0)))
        (array-ref (make-array (shape -1 1) 'b) (array (shape 0 1) -1))
        (array-ref (make-array (shape -1 1) 'c) (array (shape 0 1) 0))
        (array-ref (make-array (shape 1 2 3 4 5 6 7 8) 'd) (array (shape 0 4) 1 3 5 7)))
)

(
  "Array-set! of make-array with arguments"
  (a b c d)
  (let ((arr0 (make-array (shape) 'o))
        (arr1 (make-array (shape -1 1) 'o))
        (arr4 (make-array (shape 1 2 3 4 5 6 7 8) 'o)))
    (array-set! arr0 'a)
    (array-set! arr1 -1 'b)
    (array-set! arr1 0 'c)
    (array-set! arr4 1 3 5 7 'd)
    (list (array-ref arr0) (array-ref arr1 -1) (array-ref arr1 0) (array-ref arr4 1 3 5 7)))
)

(
  "Array-set! of make-array with vector"
  (a b c d)
  (let ((arr0 (make-array (shape) 'o))
        (arr1 (make-array (shape -1 1) 'o))
        (arr4 (make-array (shape 1 2 3 4 5 6 7 8) 'o)))
    (array-set! arr0 '#() 'a)
    (array-set! arr1 '#(-1) 'b)
    (array-set! arr1 '#(0) 'c)
    (array-set! arr4 '#(1 3 5 7) 'd)
    (list (array-ref arr0) (array-ref arr1 -1) (array-ref arr1 0) (array-ref arr4 1 3 5 7)))
)

(
  "Array-set! of make-array with array"
  (a b c d)
  (let ((arr0 (make-array (shape) 'o))
        (arr1 (make-array (shape -1 1) 'o))
        (arr4 (make-array (shape 1 2 3 4 5 6 7 8) 'o)))
    (array-set! arr0 (array (shape 0 0)) 'a)
    (array-set! arr1 (array (shape 0 1) -1) 'b)
    (array-set! arr1 (array (shape 0 1) 0) 'c)
    (array-set! arr4 (array (shape 0 4) 1 3 5 7) 'd)
    (list (array-ref arr0) (array-ref arr1 -1) (array-ref arr1 0) (array-ref arr4 1 3 5 7)))
)

;;; Share and change:
;;;
;;;  org     brk     swp            box
;;;
;;;   0 1     1 2     5 6
;;; 6 a b   2 a b   3 d c   0 2 4 6 8: e
;;; 7 c d   3 e f   4 f e
;;; 8 e f

(
  "Shared change: initial contents"
  ((a b c d e f) (a b e f) (d c f e) (e))
  (define org (array (shape 6 9 0 2) 'a 'b 'c 'd 'e 'f))
  (define brk (share-array org (shape 2 4 1 3)
                           (lambda (r k) (values (+ 6 (* 2 (- r 2))) (- k 1)))))
  (define swp (share-array org (shape 3 5 5 7)
                           (lambda (r k) (values (+ 7 (- r 3)) (- 1 (- k 5))))))
  (define box (share-array swp (shape 0 1 2 3 4 5 6 7 8 9) (lambda _ (values 4 6))))
  (define (org-contents)
    (list (array-ref org 6 0) (array-ref org 6 1)
          (array-ref org 7 0) (array-ref org 7 1)
          (array-ref org 8 0) (array-ref org 8 1)))
  (define (brk-contents)
    (list (array-ref brk 2 1) (array-ref brk 2 2)
          (array-ref brk 3 1) (array-ref brk 3 2)))
  (define (swp-contents)
    (list (array-ref swp 3 5) (array-ref swp 3 6)
          (array-ref swp 4 5) (array-ref swp 4 6)))
  (define (box-contents)
    (list (array-ref box 0 2 4 6 8)))
  (define (contents)
    (list (org-contents) (brk-contents) (swp-contents) (box-contents)))
  (contents)
)

(
  "Shared change: set org"
  ((x b c d e f) (x b e f) (d c f e) (e))
  (array-set! org 6 0 'x)
  (contents)
)

(
  "Shared change: set brk"
  ((x b c d y f) (x b y f) (d c f y) (y))
  (array-set! brk 3 1 'y)
  (contents)
)

(
  "Shared change: set swp"
  ((x b c d y z) (x b y z) (d c z y) (y))
  (array-set! swp 4 5 'z)
  (contents)
)

(
  "Shared change: set box"
  ((x b c d e z) (x b e z) (d c z e) (e))
  (array-set! box 0 2 4 6 8 'e)
  (contents)
)

(
  "Arrays copy the shape specification"
  (2 0 1 0 2 ? ! 1 10 12 1 10 12 1 10 12)
  (let* ((shp (shape 10 12))
         (arr (make-array shp))
         (ars (array shp * *))
         (art (share-array (make-array shp) shp (lambda (k) k))))
    (array-set! shp 0 0 '?)
    (array-set! shp 0 1 '!)
    (list (array-rank shp) (array-start shp 0) (array-end shp 0)
          (array-start shp 1) (array-end shp 1)
          (array-ref shp 0 0) (array-ref shp 0 1)
          (array-rank arr) (array-start arr 0) (array-end arr 0)
          (array-rank ars) (array-start ars 0) (array-end ars 0)
          (array-rank art) (array-start art 0) (array-end art 0)))
)

;;; Check that index arrays work even when they share
;;;
;;; arr       ixn
;;;   5  6      0 1
;;; 4 nw ne   0 4 6
;;; 5 sw se   1 5 4

(
  "Array access with sharing index array: reading"
  (nw ne nw se sw)
  (define arr (array (shape 4 6 5 7) 'nw 'ne 'sw 'se))
  (define ixn (array (shape 0 2 0 2) 4 6 5 4))
  (define col0 (share-array ixn (shape 0 2) (lambda (k) (values k 0))))
  (define row0 (share-array ixn (shape 0 2) (lambda (k) (values 0 k))))
  (define wor1 (share-array ixn (shape 0 2) (lambda (k) (values 1 (- 1 k)))))
  (define cod (share-array ixn (shape 0 2)
                           (lambda (k) (case k ((0) (values 1 0)) ((1) (values 0 1))))))
  (define box (share-array ixn (shape 0 2) (lambda (k) (values 1 0))))
  (list (array-ref arr col0) (array-ref arr row0) (array-ref arr wor1)
        (array-ref arr cod) (array-ref arr box))
)

(
  "Array access with sharing index array: writing"
  (ul ur ll lr xx)
  (array-set! arr col0 'ul)
  (array-set! arr row0 'ur)
  (array-set! arr cod 'lr)
  (array-set! arr box 'll)
  (let ((before (list (array-ref arr 4 5) (array-ref arr 4 6)
                      (array-ref arr 5 5) (array-ref arr 5 6))))
    (array-set! arr wor1 'xx)
    (append before (list (array-ref arr 4 5))))
)

;;; Check that shape arrays work even when they share
;;;
;;; arr             shp       shq       shr       shs
;;;    1  2  3  4      0  1      0  1      0  1      0  1
;;; 1 10 12 16 20   0 10 12   0 12 20   0 10 10   0 12 12
;;; 2 10 11 12 13   1 10 11   1 11 13   1 11 12   1 12 12
;;;                                     2 12 16
;;;                                     3 13 20

(
  "Sharing shape array"
  ((2 10 12 10 11) (2 12 20 11 13) (4 10 10 11 12 12 16 13 20) (2 12 12 12 12))
  (define arr (array (shape 1 3 1 5) 10 12 16 20 10 11 12 13))
  (define shp (share-array arr (shape 0 2 0 2) (lambda (r k) (values (+ r 1) (+ k 1)))))
  (define shq (share-array arr (shape 0 2 0 2) (lambda (r k) (values (+ r 1) (* 2 (+ 1 k))))))
  (define shr (share-array arr (shape 0 4 0 2) (lambda (r k) (values (- 2 k) (+ r 1)))))
  (define shs (share-array arr (shape 0 2 0 2) (lambda (r k) (values 2 3))))
  (define (bounds a)
    (do ((k (- (array-rank a) 1) (- k 1))
         (res '() (cons (array-start a k) (cons (array-end a k) res))))
        ((< k 0) (cons (array-rank a) res))))
  (list (bounds (make-array shp))
        (bounds (array shq * * * *  * * * *  * * * *  * * * *))
        (bounds (share-array (array (shape) *) shr (lambda _ (values))))
        (bounds (make-array shs)))
)

(
  "Sharing with sharing subshape"
  ((2 0 1 0 2 4 7) (1 4 7 1 2 3))
  (let* ((super (array (shape 4 7 4 7)
                       1 * *
                       * 2 *
                       * * 3))
         (subshape (share-array (array (shape 0 2 0 3)
                                       * 4 *
                                       * 7 *)
                                (shape 0 1 0 2)
                                (lambda (r k) (values k 1))))
         (sub (share-array super subshape (lambda (k) (values k k)))))
    (list (list (array-rank subshape)
                (array-start subshape 0) (array-end subshape 0)
                (array-start subshape 1) (array-end subshape 1)
                (array-ref subshape 0 0) (array-ref subshape 0 1))
          (list (array-rank sub) (array-start sub 0) (array-end sub 0)
                (array-ref sub 4) (array-ref sub 5) (array-ref sub 6))))
)

;;; Examples from the SRFI document

(
  "SRFI example: array-rank"
  2
  (array-rank (make-array (shape 1 2 3 4)))
)

(
  "SRFI example: array-ref with indices"
  cuatro
  (array-ref (array (shape 0 2 0 3)
                    'uno 'dos 'tres
                    'cuatro 'cinco 'seis)
             1 0)
)

(
  "SRFI example: array-ref with index objects"
  (3 1 4)
  (let ((a (array (shape 4 7 1 2) 3 1 4)))
    (list (array-ref a 4 1)
          (array-ref a (vector 5 1))
          (array-ref a (array (shape 0 2) 6 1))))
)

(
  "SRFI example: array-set!"
  huuhkaja
  (let ((a (make-array (shape 4 5 4 5 4 5))))
    (array-set! a 4 4 4 'huuhkaja)
    (array-ref a 4 4 4))
)

(
  "SRFI example: share-array"
  ((1 0 0 0) (0 1 0 0) (0 0 1 0) (0 0 0 1))
  (define i_4
    (let* ((i (make-array (shape 0 4 0 4) 0))
           (d (share-array i (shape 0 4) (lambda (k) (values k k)))))
      (do ((k 0 (+ k 1)))
          ((= k 4))
        (array-set! d k 1))
      i))
  (define (matrix->list m)
    (do ((r (- (array-end m 0) 1) (- r 1))
         (rows '()
               (cons (do ((c (- (array-end m 1) 1) (- c 1))
                          (row '() (cons (array-ref m r c) row)))
                         ((< c (array-start m 1)) row))
                     rows)))
        ((< r (array-start m 0)) rows)))
  (matrix->list i_4)
)

;;; Additional tests

(
  "Arrays are disjoint from other types"
  (#t #t #f #f #f #f #f)
  (list (array? (make-array (shape 0 2)))
        (array? (shape))
        (array? (vector 1 2))
        (array? '(1 2))
        (array? "ab")
        (array? 'array)
        (array? (lambda () #t)))
)

(
  "Shape contents"
  (2 3 0 2 (1 2 3 4 5 6))
  (let ((s (shape 1 2 3 4 5 6)))
    (list (array-rank s) (array-end s 0) (array-start s 1) (array-end s 1)
          (list (array-ref s 0 0) (array-ref s 0 1)
                (array-ref s 1 0) (array-ref s 1 1)
                (array-ref s 2 0) (array-ref s 2 1))))
)

(
  "Row-major order of array"
  ((a b c) (d e f) (g h i) (j k l))
  (let ((a (array (shape 1 5 -1 2) 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l)))
    (matrix->list a))
)

(
  "Three-dimensional array"
  (0 1 2 3 10 11 12 13 20 21 22 23 100 101 102 103 110 111 112 113 120 121 122 123)
  (define cube (make-array (shape 0 2 0 3 0 4) #f))
  (do ((i 0 (+ i 1))) ((= i 2))
    (do ((j 0 (+ j 1))) ((= j 3))
      (do ((k 0 (+ k 1))) ((= k 4))
        (array-set! cube i j k (+ (* 100 i) (* 10 j) k)))))
  (let loop ((i 1) (j 2) (k 3) (res '()))
    (cond ((< k 0) (loop i (- j 1) 3 res))
          ((< j 0) (loop (- i 1) 2 3 res))
          ((< i 0) res)
          (else (loop i j (- k 1) (cons (array-ref cube i j k) res)))))
)

(
  "Transposition via share-array"
  ((a d g j) (b e h k) (c f i l))
  (let* ((a (array (shape 0 4 0 3) 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l))
         (t (share-array a (shape 0 3 0 4) (lambda (r c) (values c r)))))
    (matrix->list t))
)

(
  "Reversal and offset via share-array"
  ((l k j) (i h g) (f e d) (c b a))
  (let* ((a (array (shape 0 4 0 3) 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l))
         (r (share-array a (shape 10 14 -3 0)
                         (lambda (r c) (values (- 13 r) (- -1 c))))))
    (matrix->list r))
)

(
  "Row, column and sub-matrix sharing"
  ((d e f) (b e h k) ((e f) (h i)))
  (let* ((a (array (shape 0 4 0 3) 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l))
         (row (share-array a (shape 0 3) (lambda (c) (values 1 c))))
         (col (share-array a (shape 0 4) (lambda (r) (values r 1))))
         (sub (share-array a (shape 1 3 1 3) (lambda (r c) (values r c)))))
    (list (list (array-ref row 0) (array-ref row 1) (array-ref row 2))
          (list (array-ref col 0) (array-ref col 1) (array-ref col 2) (array-ref col 3))
          (matrix->list sub)))
)

(
  "Sharing a shared array composes the mappings"
  (l i x c)
  (let* ((a (array (shape 0 4 0 3) 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l))
         (t (share-array a (shape 0 3 0 4) (lambda (r c) (values c r))))
         (rev (share-array t (shape 0 3 0 4) (lambda (r c) (values (- 2 r) (- 3 c)))))
         (col (share-array rev (shape 0 4) (lambda (k) (values 0 k)))))
    (array-set! col 2 'x)
    (list (array-ref col 0) (array-ref col 1) (array-ref a 1 2) (array-ref col 3)))
)

(
  "Writing through a shared array"
  ((0 0 0) (7 7 7) (0 0 0))
  (let* ((m (make-array (shape 0 3 0 3) 0))
         (row (share-array m (shape 0 3) (lambda (c) (values 1 c)))))
    (array-set! row 0 7)
    (array-set! row '#(1) 7)
    (array-set! row (array (shape 0 1) 2) 7)
    (matrix->list m))
)

(
  "Share-array calls the mapping only at valid indices"
  (5 6 7)
  (let* ((a (array (shape 0 3) 5 6 7))
         (b (share-array a (shape 10 13)
                         (lambda (k)
                           (if (and (<= 10 k) (< k 13)) (- k 10) (error "invalid index" k))))))
    (list (array-ref b 10) (array-ref b 11) (array-ref b 12)))
)

(
  "Sharing a rank-0 array"
  (z z)
  (let* ((a (array (shape) 'z))
         (b (share-array a (shape 0 2 3 5) (lambda (r c) (values)))))
    (list (array-ref b 0 3) (array-ref b 1 4)))
)

(
  "Rank-0 view of an array"
  (9 h)
  (let* ((a (array (shape 0 2 0 2) 1 2 3 9))
         (b (share-array a (shape) (lambda () (values 1 1)))))
    (let ((x (array-ref b)))
      (array-set! b 'h)
      (list x (array-ref a 1 1))))
)

(
  "Empty arrays"
  (1 3 3 0 2 5)
  (let ((a (make-array (shape 3 3 0 2)))
        (b (array (shape 0 2 5 5))))
    (list (array-rank (make-array (shape 3 3)))
          (array-start a 0) (array-end a 0) (array-start a 1) (array-end a 1)
          (array-end b 1)))
)

(
  "Sharing an empty array"
  (2 0 0)
  (let ((e (share-array (make-array (shape 0 0)) (shape 0 0 0 3) (lambda (r c) (error "never called")))))
    (list (array-rank e) (array-start e 0) (array-end e 0)))
)

(
  "Index object may be a shared one-dimensional array"
  (f c)
  (let* ((a (array (shape 0 4 0 3) 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l))
         (idx (array (shape 0 2 0 2) 1 2 0 0))
         (r0 (share-array idx (shape 0 2) (lambda (k) (values 0 k))))
         (c1 (share-array idx (shape 0 2) (lambda (k) (values (- 1 k) 1)))))
    (list (array-ref a r0) (array-ref a c1)))
)

(
  "Arrays can contain any objects"
  (#f () "s" #(1 2))
  (let ((a (array (shape 0 4) #f '() "s" (vector 1 2))))
    (list (array-ref a 0) (array-ref a 1) (array-ref a 2) (array-ref a 3)))
)

(
  "Storing a vector in a rank-0 array"
  #(1 2)
  (let ((a (make-array (shape) #f)))
    (array-set! a (vector 1 2))
    (array-ref a))
)

;;; Errors

(
  "Error: shape with odd number of bounds"
  <error>
  (shape 1 2 3)
)

(
  "Error: shape with decreasing bounds"
  <error>
  (shape 2 1)
)

(
  "Error: shape with inexact bounds"
  <error>
  (shape 0 2.0)
)

(
  "Error: shape with non-integer bounds"
  <error>
  (shape 0 'a)
)

(
  "Error: make-array with non-shape"
  <error>
  (make-array (array (shape 0 2) 0 1))
)

(
  "Error: make-array with vector"
  <error>
  (make-array '#(0 2))
)

(
  "Error: make-array with shape containing invalid bounds"
  <error>
  (let ((s (shape 0 2)))
    (array-set! s 0 1 -1)
    (make-array s))
)

(
  "Error: array with too few elements"
  <error>
  (array (shape 0 2 0 2) 1 2 3)
)

(
  "Error: array with too many elements"
  <error>
  (array (shape 0 2) 1 2 3)
)

(
  "Error: array-rank of non-array"
  <error>
  (array-rank (vector 1 2))
)

(
  "Error: array-start with invalid dimension"
  <error>
  (array-start (make-array (shape 0 2)) 1)
)

(
  "Error: array-end with negative dimension"
  <error>
  (array-end (make-array (shape 0 2)) -1)
)

(
  "Error: array-ref with index below lower bound"
  <error>
  (array-ref (make-array (shape 1 3) 0) 0)
)

(
  "Error: array-ref with index at upper bound"
  <error>
  (array-ref (make-array (shape 1 3) 0) 3)
)

(
  "Error: array-ref with too few indices"
  <error>
  (array-ref (make-array (shape 0 2 0 2) 0) 1)
)

(
  "Error: array-ref with too many indices"
  <error>
  (array-ref (make-array (shape 0 2) 0) 1 1)
)

(
  "Error: array-ref with inexact index"
  <error>
  (array-ref (make-array (shape 0 2) 0) 1.0)
)

(
  "Error: array-ref with index vector of wrong length"
  <error>
  (array-ref (make-array (shape 0 2 0 2) 0) '#(1))
)

(
  "Error: array-ref with index array that is not 0-based"
  <error>
  (array-ref (make-array (shape 0 2) 0) (array (shape 1 2) 0))
)

(
  "Error: array-ref with two-dimensional index array"
  <error>
  (array-ref (make-array (shape 0 2) 0) (array (shape 0 1 0 1) 0))
)

(
  "Error: array-ref of non-array"
  <error>
  (array-ref (vector 1 2) 0)
)

(
  "Error: array-ref of empty array"
  <error>
  (array-ref (make-array (shape 0 0) 0) 0)
)

(
  "Error: array-set! with index out of bounds"
  <error>
  (array-set! (make-array (shape 0 2) 0) 2 'x)
)

(
  "Error: array-set! without value"
  <error>
  (array-set! (make-array (shape) 0))
)

(
  "Error: array-set! with too few indices"
  <error>
  (array-set! (make-array (shape 0 2 0 2) 0) 1 'x)
)

(
  "Error: share-array with mapping exceeding the bounds"
  <error>
  (share-array (make-array (shape 0 3) 0) (shape 0 4) (lambda (k) k))
)

(
  "Error: share-array with mapping below the bounds"
  <error>
  (share-array (make-array (shape 0 3 0 3) 0) (shape 0 3) (lambda (k) (values (- k 1) 0)))
)

(
  "Error: share-array of a shared array exceeding its bounds"
  <error>
  (let* ((a (make-array (shape 0 4 0 4) 0))
         (sub (share-array a (shape 1 3 1 3) (lambda (r c) (values r c)))))
    (share-array sub (shape 0 2) (lambda (k) (values 3 (+ k 1)))))
)

(
  "Error: share-array with mapping returning wrong number of indices"
  <error>
  (share-array (make-array (shape 0 3 0 3) 0) (shape 0 3) (lambda (k) k))
)

(
  "Error: share-array with mapping returning non-integers"
  <error>
  (share-array (make-array (shape 0 3) 0) (shape 0 3) (lambda (k) 'a))
)
