;;; LISPKIT MATH MATRIX
;;;
;;; This library provides abstractions for representing vectors, matrices and
;;; for performing vector and matrix arithmetics. A matrix is a rectangular
;;; array of numbers. The library supports commong matrix operations such as
;;; matrix addition, subtraction, and multiplication. There are operations to
;;; create matrices and to manipulate matrix objects. Furthermore, there is
;;; support to compute matrix determinants and to transpose and invert matrices.
;;; Matrices can be transformed into reduced row echelon form and matrix ranks
;;; can be computed.
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2022 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not
;;; use this file except in compliance with the License. You may obtain a copy
;;; of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;;; License for the specific language governing permissions and limitations
;;; under the License.

(define-library (lispkit math matrix)
  
  (export matrix-type-tag
          make-matrix
          matrix
          identity-matrix
          matrix-copy
          matrix-eliminate
          matrix?
          matrix-vector?
          matrix-zero?
          matrix-square?
          matrix-identity?
          matrix-symmetric?
          matrix-dimensions?
          matrix=?
          matrix-size
          matrix-rows
          matrix-columns
          matrix-ref
          matrix-set!
          matrix-row 
          matrix-column
          matrix-row-swap!
          matrix-column-swap!
          matrix->vector
          matrix-row->list
          matrix-column->list
          matrix->list
          matrix-transpose
          matrix-sum!
          matrix-difference!
          matrix-add
          matrix-subtract
          matrix-mult
          matrix-minor
          matrix-cofactor
          matrix-determinant
          matrix-normalize
          matrix-inverse
          matrix-row-echelon!
          matrix-rank
          matrix-for-each
          matrix-fold
          matrix->string)
  
  (import (lispkit base))
  
  (begin
    
    ;; Matrix type.
    (define-values
      (matrix-type-tag new-matrix matrix? matrix-repr make-matrix-subtype)
      (make-type 'matrix))
    
    ;; Returns a new matrix with `m` rows and `n` columns.
    (define (make-matrix m n)
      (assert (fxpositive? m)
              (fxpositive? n))
      (let ((mrepr (make-vector (fx+ (fx* m n) 2) 0)))
        (vector-set! mrepr 0 m)
        (vector-set! mrepr 1 n)
        (new-matrix mrepr)))
    
    ;; Returns a new matrix consisting of the rows ms, args ... Rows can be specified
    ;; either as a list or a vector.
    (define (matrix ms . args)
      (if (null? args)
          (cond
            ((pair? ms)
              (if (pair? (car ms))
                  (let* ((m (length ms))
                         (n (length (car ms)))
                         (res (make-matrix m n))
                         (rrepr (matrix-repr res)))
                    (assert (every? (lambda (r) (fx= (length r) n)) (cdr ms)))
                    (do ((x 2 (fx1+ x))
                         (r (car ms) (if (pair? (cdr r)) (cdr r) (if (pair? rs) (car rs) '())))
                         (rs (cdr ms) (if (pair? (cdr r)) rs (if (pair? rs) (cdr rs) '()))))
                        ((and (null? r) (null? rs)) res)
                      (vector-set! rrepr x (car r))))
                  (matrix (list ms))))
            ((vector? ms)
              (if (vector? (vector-ref ms 0))
                  (let* ((m (vector-length ms))
                         (n (vector-length (vector-ref ms 0)))
                         (i 0)
                         (res (make-matrix m n))
                         (rrepr (matrix-repr res)))
                    (vector-for-each
                      (lambda (r)
                        (assert (fx= (vector-length r) n))
                        (vector-copy! rrepr (fx+ 2 (fx* i n)) r)
                        (set! i (fx1+ i)))
                      ms)
                    res)
                  (matrix (vector ms))))
            (else
              (matrix (list (list ms)))))
           (if (pair? ms)
               (matrix (cons ms args))
               (matrix (list->vector (cons ms args))))))
    
    ;; Returns a new identity matrix with `n` rows and columns.
    (define (identity-matrix n)
      (assert (fxpositive? n))
      (let ((mrepr (make-vector (fx+ (fx* n n) 2) 0)))
        (vector-set! mrepr 0 n)
        (vector-set! mrepr 1 n)
        (do ((i 0 (fx1+ i)))
            ((fx= i n))
          (vector-set! mrepr (matrix-repr-index mrepr i i) 1))
        (new-matrix mrepr)))
    
    ;; Returns a copy of `matrix`.
    (define (matrix-copy matrix)
      (let ((mrepr (matrix-repr matrix)))
        (new-matrix (vector-copy mrepr #t))))
    
    ;; Returns a copy of `matrix` with row `i` and column `j` removed.
    (define (matrix-eliminate matrix i j)
      (let* ((mrepr (matrix-repr matrix))
             (m (matrix-rows matrix))
             (n (matrix-columns matrix))
             (temp (make-matrix (fx1- m) (fx1- n)))
             (trepr (matrix-repr temp)))
        (assert (fx> m i -1)
                (fx> n j -1))
        (do ((x 2 (fx1+ x))
             (y 2))
            ((fx= x (vector-length mrepr)) temp)
          (if (not (or (fx= (fx/ (fx- x 2) n) i) (fx= (fxremainder (fx- x 2) n) j)))
              (begin
                (vector-set! trepr y (vector-ref mrepr x))
                (set! y (fx1+ y)))))))
    
    ;; Returns `#t` if `v` is a vector of numbers
    (define (matrix-vector? v)
      (and (vector? v)
           (fxpositive? (vector-length v))
           (do ((i (fx1- (vector-length v)) (fx1- i)))
               ((or (fxnegative? i) (not (number? (vector-ref v i)))) (fxnegative? i)))))
    
    ;; Returns `#t` if `matrix` is a zero matrix, i.e. all its elements are zero.
    (define (matrix-zero? matrix)
      (matrix-fold (lambda (z i j x) (and z (zero? x))) #t matrix))
    
    ;; Returns `#t` if `matrix` is a square matrix.
    (define (matrix-square? matrix)
      (fx= (matrix-rows matrix) (matrix-columns matrix)))
    
    ;; Returns `#t` if `matrix` is a identity matrix.
    (define (matrix-identity? matrix)
      (and (matrix-square? matrix)
           (matrix-fold
             (lambda (z i j x) (and z (if (fx= i j) (= x 1) (= x 0))))
             #t
             matrix)))
    
    ;; Returns `#t` if `matrix` is symmetric, i.e. `matrix` is equal to its
    ;; transposed matrix.
    (define (matrix-symmetric? matrix)
      (and (matrix-square? matrix)
           (matrix-fold (lambda (z i j x) (and z (= x (matrix-ref matrix j i)))) #t matrix)))
    
    ;; Returns `#t` if the given object is a matrix of the given dimensions
    (define (matrix-dimensions? obj m n)
      (and (matrix? obj)
           (fx= (matrix-rows obj) m)
           (fx= (matrix-columns obj) n)))
    
    ;; Returns `#t` if all matrices in list `args` are equal to `matrix`.
    (define (matrix=? matrix . args)
      (let ((mrepr (matrix-repr matrix)))
        (fold-left (lambda (z m) (and z (vector= = mrepr (matrix-repr m)))) #t args)))
    
    ;; Returns the size of the matrix as a pair whose car is the number of rows and cdr
    ;; is the number of columns.
    (define (matrix-size matrix)
      (cons (vector-ref (matrix-repr matrix) 0) (vector-ref (matrix-repr matrix) 1)))
    
    ;; Returns the number of rows in a matrix.
    (define (matrix-rows matrix)
      (vector-ref (matrix-repr matrix) 0))
  
    ;; Returns the number of columns in a matrix.
    (define (matrix-columns matrix)
      (vector-ref (matrix-repr matrix) 1))
  
    (define (matrix-repr-index mrepr i j)
      (fx+ (fx* (vector-ref mrepr 1) i) j 2))
    
    (define (matrix-repr-ref mrepr i j)
      (vector-ref mrepr (fx+ (fx* (vector-ref mrepr 1) i) j 2)))
  
    ;; Returns the j-th element of the i-th row.
    (define (matrix-ref matrix i j)
      (let ((mrepr (matrix-repr matrix)))
        (assert (fx> (vector-ref mrepr 0) i -1)
                (fx> (vector-ref mrepr 1) j -1))
        (matrix-repr-ref mrepr i j)))
    
    ;; Sets the j-th element of the i-th row to x
    (define (matrix-set! matrix i j x)
      (let ((mrepr (matrix-repr matrix)))
        (assert (fx> (vector-ref mrepr 0) i -1)
                (fx> (vector-ref mrepr 1) j -1))
        (vector-set! mrepr (matrix-repr-index mrepr i j) x)))
  
    ;; Returns row `i` of `matrix` as a vector.
    (define (matrix-row matrix i)
      (let* ((mrepr (matrix-repr matrix))
             (res (make-vector (vector-ref mrepr 1))))
        (assert (fx> (vector-ref mrepr 0) i -1))
        (do ((j (fx1- (vector-ref mrepr 1)) (fx1- j)))
            ((fxnegative? j) res)
          (vector-set! res j (vector-ref mrepr (matrix-repr-index mrepr i j))))))
    
    ;; Returns column `i` of `matrix` as a vector.
    (define (matrix-column matrix j)
      (let* ((mrepr (matrix-repr matrix))
             (res (make-vector (vector-ref mrepr 0))))
        (assert (fx> (vector-ref mrepr 1) j -1))
        (do ((i (fx1- (vector-ref mrepr 0)) (fx1- i)))
            ((fxnegative? i) res)
          (vector-set! res i (vector-ref mrepr (matrix-repr-index mrepr i j))))))
    
    ;; Returns `matrix` as a vector of vectors.
    (define (matrix->vector matrix)
      (do ((i (fx1- (matrix-rows matrix)) (fx1- i))
           (res (make-vector (matrix-rows matrix))))
          ((fxnegative? i) res)
        (vector-set! res i (matrix-row matrix i))))
    
    ;; Returns row `i` of `matrix` as a list
    (define (matrix-row->list matrix i)
      (let ((mrepr (matrix-repr matrix)))
        (assert (fx> (vector-ref mrepr 0) i -1))
        (do ((j (fx1- (vector-ref mrepr 1)) (fx1- j))
             (row '() (cons (vector-ref mrepr (matrix-repr-index mrepr i j)) row)))
            ((fxnegative? j) row))))
    
    ;; Returns column `j` of `matrix` as a list
    (define (matrix-column->list matrix j)
      (let ((mrepr (matrix-repr matrix)))
        (assert (fx> (vector-ref mrepr 1) j -1))
        (do ((i (fx1- (vector-ref mrepr 0)) (fx1- i))
             (col '() (cons (vector-ref mrepr (matrix-repr-index mrepr i j)) col)))
            ((fxnegative? i) col))))
  
    ;; Returns `matrix` as a list of lists
    (define (matrix->list matrix)
      (do ((i (fx1- (matrix-rows matrix)) (fx1- i))
           (res '() (cons (matrix-row->list matrix i) res)))
          ((fxnegative? i) res)))
    
    ;; Invokes `f` for every element of `matrix`. `f` is a function taking three
    ;; arguments: the row, the column, and the element at this position. The traversal
    ;; order is by row from left to right.
    (define (matrix-for-each f matrix)
      (let ((m (matrix-rows matrix))
            (n (matrix-columns matrix)))
        (do ((i 0 (if (= j (fx1- n)) (fx1+ i) i))
             (j 0 (if (= j (fx1- n)) 0 (fx1+ j))))
            ((= i m))
          (f i j (matrix-ref matrix i j)))))
    
    ;; Folds the matrix elements row by row from left to right, invoking `f` with
    ;; the accumulator, the row, the column and the element at this position.
    (define (matrix-fold f z matrix)
      (let ((m (matrix-rows matrix))
            (n (matrix-columns matrix)))
        (do ((i 0 (if (= j (fx1- n)) (fx1+ i) i))
             (j 0 (if (= j (fx1- n)) 0 (fx1+ j))))
            ((= i m) z)
          (set! z (f z i j (matrix-ref matrix i j))))))
    
    ;; Returns `matrix` in transposed form.
    (define (matrix-transpose matrix)
      (let ((res (make-matrix (matrix-columns matrix) (matrix-rows matrix))))
        (matrix-for-each (lambda (i j x) (matrix-set! res j i x)) matrix)
        res))

    ;; Sums up `matrix` and all matrices in `args` storing the result in `matrix`.
    (define (matrix-sum! matrix . args)
      (do ((m (matrix-rows matrix))
           (n (matrix-columns matrix))
           (ms args (cdr ms)))
          ((null? ms))
        (assert (matrix-dimensions? (car ms) m n))
        (matrix-for-each
          (lambda (i j x) (matrix-set! matrix i j (+ (matrix-ref matrix i j) x)))
          (car ms))))
    
    ;; Subtracts the matrices from list `args` from `matrix`, storing the result
    ;; in `matrix`.
    (define (matrix-difference! matrix . args)
      (do ((m (matrix-rows matrix))
           (n (matrix-columns matrix))
           (ms args (cdr ms)))
          ((null? ms))
        (assert (matrix-dimensions? (car ms) m n))
        (matrix-for-each
          (lambda (i j x) (matrix-set! matrix i j (- (matrix-ref matrix i j) x)))
          (car ms))))
    
    ;; Returns the sum of `matrix` and all matrices in list `args`. This procedure
    ;; also supports vector addition.
    (define (matrix-add matrix . args)
      (let ((m (matrix-normalize matrix)))
        (cond ((matrix? m)
                (let ((res (matrix-copy m)))
                  (apply matrix-sum! res args)
                  res))
              ((vector? m)
                (let ((res (vector-copy m #t)))
                  (for-each (lambda (v)
                              (vector-for-each/index
                                (lambda (i x) (vector-set! res i (+ (vector-ref res i) x)))
                                (matrix-normalize v)))
                            args)
                  res))
              (else
                (apply + m args)))))
    
    ;; Returns the difference of `matrix` and the matrices in list `args`. This procedure
    ;; also supports vector differences.
    (define (matrix-subtract matrix . args)
      (let ((m (matrix-normalize matrix)))
        (cond ((matrix? m)
                (let ((res (matrix-copy m)))
                  (apply matrix-difference! res args)
                  res))
              ((vector? m)
                (let ((res (vector-copy m #t)))
                  (for-each (lambda (v)
                              (vector-for-each/index
                                (lambda (i x) (vector-set! res i (- (vector-ref res i) x)))
                                (matrix-normalize v)))
                            args)
                  res))
              (else
                (apply - m args)))))
    
    ;; Returns a normalized version of `x`, representing 1*1 matrices as a number and
    ;; m*1 and 1*n matrices as a vector.
    (define (matrix-normalize x)
      (cond ((matrix? x)
              (cond ((fx= (matrix-rows x) 1)
                      (if (fx= (matrix-columns x) 1) (matrix-ref x 0 0) (matrix-row x 0)))
                    ((fx= (matrix-columns x) 1)
                      (matrix-column x 0))
                    (else
                      x)))
           ((vector? x)
             (if (fx= (vector-length x) 1) (vector-ref x 0) x))
           (else
             x)))
    
    (define (dot-prod v1 v2)
      (assert (fx= (vector-length v1) (vector-length v2)))
      (do ((i (fx1- (vector-length v1)) (fx1- i))
           (acc 0 (+ acc (* (vector-ref v1 i) (vector-ref v2 i)))))
          ((fxnegative? i) acc)))
    
    (define (sca-mult m x)
      (let* ((nr (matrix-rows m))
             (nc (matrix-columns m))
             (res (make-matrix nr nc)))
        (do ((i 0 (fx1+ i)))
            ((fx= i nr) res)
          (do ((j 0 (fx1+ j)))
              ((fx= j nc))
            (matrix-set! res i j (* x (matrix-ref m i j)))))))
        
    (define (mat-mult m1 m2)
      (let* ((m (matrix-rows m1))
             (n (matrix-rows m2))
             (o (matrix-columns m2))
             (res (make-matrix m o)))
        (assert (= (matrix-columns m1) n))
        (do ((i 0 (fx1+ i)))
            ((fx= i m) res)
          (do ((j 0 (fx1+ j)))
              ((fx= j o))
            (do ((k 0 (fx1+ k))
                 (acc 0 (+ acc (* (matrix-ref m1 i k) (matrix-ref m2 k j)))))
                ((fx= k n) (matrix-set! res i j acc)))))))
    
    (define (mult x y)
      (cond ((matrix? x)
              (cond ((matrix? y) (mat-mult x y))
                    ((vector? y) (mat-mult x (matrix-transpose (matrix y))))
                    (else        (sca-mult x y))))
            ((vector? x)
              (cond ((matrix? y) (mat-mult (matrix x) y))
                    ((vector? y) (dot-prod x y))
                    (else        (vector-map (lambda (z) (* z y)) x))))
            (else
              (cond ((matrix? y) (sca-mult y x))
                    ((vector? y) (vector-map (lambda (z) (* z x)) y))
                    (else        (* x y))))))
    
    ;; Returns the matrix product of `x` and the matrices in `args`. Can also be used
    ;; to compute the dot product of vectors.
    (define (matrix-mult x . args)
      (do ((ms args (cdr ms))
           (res (matrix-normalize x) (matrix-normalize (mult res (matrix-normalize (car ms))))))
          ((null? ms) res)))
    
    ;; Returns a minor of `matrix` by removing row `i` and column `j` and computing
    ;; the determinant of the remaining matrix. `matrix` needs to be a square matrix.
    (define (matrix-minor matrix i j) 
      (matrix-determinant (matrix-eliminate matrix i j)))
    
    ;; Returns a minor of `matrix` by removing row `i` and column `j` and computing
    ;; the determinant of the remaining matrix. The result is negative if the sum
    ;; of `i` and `j` is odd. `matrix` needs to be a square matrix.
    (define (matrix-cofactor matrix i j)
      (* (if (fxodd? (fx+ i j)) -1 1) (matrix-minor matrix i j)))
    
    ;; Returns the determinant of `matrix`. `matrix` needs to be a square matrix.
    (define (matrix-determinant matrix)
      (let ((n (matrix-columns matrix)))
        (assert (fx= (matrix-rows matrix) n))
        (case n
          ((1)  (matrix-ref matrix 0 0))
          ((2)  (- (* (matrix-ref matrix 0 0) (matrix-ref matrix 1 1))
                   (* (matrix-ref matrix 0 1) (matrix-ref matrix 1 0))))
          (else (do ((j 0 (fx1+ j))
                     (acc 0 (+ acc (* (matrix-ref matrix 0 j) (matrix-cofactor matrix 0 j)))))
                    ((fx= j n) acc))))))
    
    ;; Returns the inverse of `matrix` if it exists. If it does not exist, `#f` is
    ;; being returned. `matrix` needs to be a square matrix.
    (define (matrix-inverse matrix)
      (let* ((m (matrix-rows matrix))
             (n (matrix-columns matrix))
             (cfm (make-matrix m n)))
        (assert (fx= m n))
        (matrix-for-each
          (lambda (i j x) (matrix-set! cfm i j (matrix-cofactor matrix i j)))
          matrix)
        (let ((res (matrix-transpose cfm))
              (det (do ((j 0 (fx1+ j))
                        (acc 0 (+ acc (* (matrix-ref matrix 0 j) (matrix-ref cfm 0 j)))))
                       ((fx= j n) acc))))
          (if (zero? det)
              #f
              (matrix-mult res (/ 1 det))))))
    
    ;; Swaps columns `j` and `k` of `matrix`.
    (define (matrix-column-swap! matrix j k)
      (assert (fx> (matrix-columns matrix) j -1)
              (fx> (matrix-columns matrix) k -1))
      (do ((mrepr (matrix-repr matrix))
           (i (fx1- (matrix-rows matrix)) (fx1- i)))
          ((fxnegative? i))
        (vector-swap! mrepr (matrix-repr-index mrepr i j) (matrix-repr-index mrepr i k))))
    
    ;; Swaps rows `i` and `k` of `matrix`.
    (define (matrix-row-swap! matrix i k)
      (assert (fx> (matrix-rows matrix) i -1)
              (fx> (matrix-rows matrix) k -1))
      (do ((mrepr (matrix-repr matrix))
           (j (fx1- (matrix-columns matrix)) (fx1- j)))
          ((fxnegative? j))
        (vector-swap! mrepr (matrix-repr-index mrepr i j) (matrix-repr-index mrepr k j))))
    
    ;; Returns the reduced row echelon matrix of `matrix`.
    (define (matrix-row-echelon! matrix)
      (let ((m (matrix-rows matrix))
            (n (matrix-columns matrix)))
        (do ((i 0 (fx1+ i))
             (k (fx1- m)))
            ((fx> i k) (set! m (fx1+ k)))
          (cond
            ((do ((j (fx1- n) (fx1- j)))
                 ((or (fxnegative? j) (not (zero? (matrix-ref matrix i j)))) (fxnegative? j)))
              (matrix-row-swap! matrix i k)
              (set! k (fx1- k)))))
        (do ((p 0 (fx1+ p)))
            ((or (fx>= p m) (fx>= p n)))
          (do ((r 1 (fx1+ r)))
              ((or (not (zero? (matrix-ref matrix p p))) (fx>= (fx+ p r) m))
               (if (not (zero? (matrix-ref matrix p p)))
                   (do ((i 0 (fx1+ i)))
                       ((fx>= i m))
                     (cond
                       ((fx= i p)
                         (do ((x (/ 1 (matrix-ref matrix p p)))
                              (c p (fx1+ c)))
                             ((fx>= c n))
                           (matrix-set! matrix i c (* (matrix-ref matrix p c) x))))
                       ((not (zero? (matrix-ref matrix i p)))
                         (do ((x (- (/ (matrix-ref matrix i p) (matrix-ref matrix p p))))
                              (c p (fx1+ c)))
                             ((fx>= c n))
                           (matrix-set! matrix i c
                             (+ (* (matrix-ref matrix p c) x) (matrix-ref matrix i c)))))))))
            (matrix-row-swap! matrix p (fx+ p r))))))
    
    ;; Returns the rank of `matrix`.
    (define (matrix-rank matrix)
      (let ((m (matrix-copy matrix)))
        (matrix-row-echelon! m)
        (do ((i (fx1- (matrix-rows m)) (fx1- i))
             (res 0 (fx+ res (do ((j (fx1- (matrix-columns m)) (fx1- j)))
                                 ((or (fxnegative? j) (not (zero? (matrix-ref m i j))))
                                  (if (fxnegative? j) 0 1))))))
            ((fxnegative? i) res))))
    
    ;; Returns a multi-line string representation of `matrix`. The elements of `matrix`
    ;; are converted into a string by invocing `(number->string x clen prec noexp)`.
    (define (matrix->string matrix . args)
      (let-optionals args ((clen 1)
                           (prec #f)
                           (noexp #t))
        (let* ((m (matrix-rows matrix))
               (n (matrix-columns matrix))
               (lens (make-vector n 0))
               (strs (make-vector (fx* n m))))
          (matrix-for-each
            (lambda (i j x)
              (let* ((str (number->string x 10 clen prec noexp))
                     (len (string-length str)))
                (if (fx> len (vector-ref lens j))
                  (vector-set! lens j len))
                (vector-set! strs (fx+ (fx* n i) j) str)))
            matrix)
          (do ((i 0 (if (fx= j (fx1- n)) (fx1+ i) i))
               (j 0 (if (fx= j (fx1- n)) 0 (fx1+ j)))
               (s (make-string 0)))
              ((fx= i m) s)
            (string-append! s
              (cond ((fx> j 0) " ")
                    ((fx= n 1) "(")
                    ((fx= i 0) "⎛")
                    ((fx= i (fx1- m)) "⎝")
                    (else "⎜"))
              (string-pad-left (vector-ref strs (fx+ (fx* n i) j)) #\space (vector-ref lens j))
              (cond ((fx< j (fx1- n)) "")
                    ((fx= n 1) ")\n")
                    ((fx= i 0) "⎞\n")
                    ((fx= i (fx1- m)) "⎠\n")
                    (else "⎟\n")))))))
  )
)
