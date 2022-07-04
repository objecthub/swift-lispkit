;;; LISPKIT DISJOINT-SET
;;;
;;; Implementation of disjoint sets, a union-find data structure that tracks a set of
;;; elements partitioned into disjoint subsets.
;;;
;;; New empty disjoint sets are created with the functions `make-disjoint-set`,
;;; `make-eq-disjoint-set`, and `make-eqv-disjoint-set`. A new element is added to a
;;; disjoint set via `disjoint-set-make`. `disjoint-set-find` returns a canonical
;;; element representing the subset to which an element is belonging. `disjoint-set-union`
;;; unifies the two subsets given by two member elements. `disjoint-set-size` returns
;;; the number of subsets into which all elements are partitioned.
;;;
;;; Author: Matthias Zenger
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

(define-library (lispkit disjoint-set)

  (export make-eq-disjoint-set
          make-eqv-disjoint-set
          make-disjoint-set
          disjoint-set?
          disjoint-set-type-tag
          disjoint-set-make
          disjoint-set-find
          disjoint-set-union
          disjoint-set-size)

  (import (lispkit base)
          (lispkit comparator))

  (begin

    (define-values (disjoint-set-type-tag new-disjoint-set disjoint-set? disjoint-set-ref
                    make-disjoint-set-subtype)
      (make-type 'disjoint-set))
    
    (define (make-eq-disjoint-set)
      (new-disjoint-set (make-eq-hashtable)))

    (define (make-eqv-disjoint-set)
      (new-disjoint-set (make-eqv-hashtable)))

    (define (make-disjoint-set a . args)
      (let-optionals args ((equiv #f))
        (new-disjoint-set
          (if equiv
              (make-hashtable a equiv)
              (make-hashtable (comparator-hash-function a) (comparator-equality-predicate a))))))

    (define (disjoint-set-make ds x)
      (let ((ht (disjoint-set-ref ds)))
        (if (not (hashtable-contains? ht x))
            (hashtable-set! ht x (cons x 0)))))

    (define (disjoint-set-find ds x . args)
      (let-optionals args ((default #f))
        (let* ((ht (disjoint-set-ref ds))
               (equiv (hashtable-equivalence-function ht)))
          (let loop ((y x))
            (let ((yp (hashtable-ref ht y #f)))
              (if yp
                  (if (equiv y (car yp))
                      (car yp)
                      (let ((res (loop (car yp))))
                        (hashtable-set! ht y (cons res (cdr yp)))
                        res))
                  default))))))

    (define (disjoint-set-union ds x y)
      (let* ((ht (disjoint-set-ref ds))
             (xc (disjoint-set-find ds x))
             (yc (disjoint-set-find ds y))
             (xcp (hashtable-ref ht xc #f))
             (ycp (hashtable-ref ht yc #f)))
        (unless xcp
          (error "disjoint-set-union: $1 not found in $0" ds x))
        (unless ycp
          (error "disjoint-set-union: $1 not found in $0" ds y))
        (cond ((> (cdr xcp) (cdr ycp))
                (hashtable-set! ht yc (cons (car xcp) (cdr ycp))))
              ((< (cdr xcp) (cdr ycp))
                (hashtable-set! ht xc (cons (car ycp) (cdr xcp))))
              (else
                (hashtable-set! ht yc (cons (car xcp) (cdr ycp)))
                (hashtable-set! ht xc (cons (car xcp) (fx1+ (cdr xcp))))))))

    (define (disjoint-set-size ds)
      (let* ((ht (disjoint-set-ref ds))
             (nt (hashtable-empty-copy ht)))
        (hashtable-for-each (lambda (k v) (hashtable-set! nt (disjoint-set-find ds k) #t)) ht)
        (hashtable-size nt)))
  )
)

