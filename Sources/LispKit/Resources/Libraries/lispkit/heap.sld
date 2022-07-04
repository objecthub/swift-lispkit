;;; LISPKIT HEAP
;;; 
;;; Implementation of heaps.
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2017 Matthias Zenger. All rights reserved.
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
;;; 
;;; The core of the implementation is based on code from SLIB: priorque.scm
;;; Copyright © 1992, 1993, 1994, 1995, 1997 Aubrey Jaffer
;;;
;;; Permission to copy this software, to modify it, to redistribute it,
;;; to distribute modified versions, and to use it for any purpose is
;;; granted, subject to the following restrictions and understandings.
;;; 
;;; 1. Any copy made of this software must include this copyright notice in full.
;;; 
;;; 2. I have made no warranty or representation that the operation of this software will
;;;    be error-free, and I am under no obligation to provide any services, by way of
;;;    maintenance, update, or otherwise.
;;; 
;;; 3. In conjunction with products arising from the use of this material, there shall be
;;;    no use of my name in any advertising, promotional, or sales literature without prior
;;;    written consent in each case.

(define-library (lispkit heap)
  
  (export make-heap
          heap?
          heap-type-tag
          heap-empty?
          heap-size
          heap-max
          heap-add!
          heap-delete-max!
          heap-clear!
          heap-copy
          heap->vector
          heap->list
          heap->reversed-list
          list->heap!
          list->heap
          vector->heap)
  
  (import (lispkit base))

  ;;;; INTERNAL
  
  (begin

    (define-record-type heap
      (new-heap array size pred<?)
      heap?
      (array  heap-array heap-array-set!)
      (size   heap-size  heap-size-set!)
      (pred<? heap-pred<?))
    
    (define heap-type-tag (record-type-tag heap))
    
    ;; Set the size of the heap
    (define (set-size! hp size)
      (let ((arr (heap-array hp)))
        (when (> size (vector-length arr))
          (let ((narr (make-vector (+ size (quotient size 2)))))
            (do ((i (fx1- (vector-length arr)) (fx1- i)))
                ((negative? i) (heap-array-set! hp narr))
              (vector-set! narr i (vector-ref arr i))))))
      (heap-size-set! hp size))
    
    ;; Reference an element
    (define (heap-ref hp idx)
      (vector-ref (heap-array hp) (fx1- idx)))

    ;; Set an element
    (define (heap-set! hp idx value)
      (vector-set! (heap-array hp) (fx1- idx) value))

    ;; Exchange two elements
    (define (heap-exchange! hp idx-i idx-j)
      (let* ((i (fx1- idx-i))
             (j (fx1- idx-j))
             (arr (heap-array hp))
             (tmp (vector-ref arr i)))
        (vector-set! arr i (vector-ref arr j))
        (vector-set! arr j tmp)))
    
    ;; Parent index in heap
    (define (heap-parent i) (quotient i 2))
    
    ;; Left child in heap
    (define (heap-left i) (* 2 i))
    
    ;; Right child in heap
    (define (heap-right i) (fx1+ (* 2 i)))
    
    ;; Retain binary heap property
    (define (heapify a i)
      (let* ((l      (heap-left i))
             (r      (heap-right i))
             (pred<? (heap-pred<? a))
             (maxi   (if (and (<= l (heap-size a)) (pred<? (heap-ref a i) (heap-ref a l))) l i)))
        (if (and (<= r (heap-size a)) (pred<? (heap-ref a maxi) (heap-ref a r)))
            (set! maxi r))
        (if (not (= maxi i))
            (begin (heap-exchange! a i maxi)
                   (heapify a maxi)))))
  )
  
  ;;;; EXTERNAL
  
  (begin
    
    ;; Returns a binary max heap
    (define (make-heap pred<?)
      (new-heap (make-vector 4) 0 pred<?))
    
    ;; Returns true if the heap is empty
    (define (heap-empty? hp)
      (zero? (heap-size hp)))
      
    ;; Returns the item which is larger than all others according to the comparison function
    ;; `pred<?` in `hp`
    (define (heap-max hp)
      (heap-ref hp 1))
    
    ;; Inserts an item into the heap. The same item can be inserted multiple times.
    (define (heap-add! hp . elements)
      (list->heap! hp elements))
    
    ;; Returns the item which is larger than all others according to the
    ;; `pred<?` in `hp` and removes it from the heap. If there are no items on the heap,
    ;; an error is signaled.
    (define (heap-delete-max! hp)
      (if (heap-empty? hp)
          (error "heap underflow: $0" hp)
          (let ((max (heap-ref hp 1)))
            (heap-set! hp 1 (heap-ref hp (heap-size hp)))
            (set-size! hp (fx1- (heap-size hp)))
            (heapify hp 1)
            max)))
    
    ;; Removes all elements from `hp`
    (define (heap-clear! hp)
      (heap-array-set! hp (make-vector 4))
      (heap-size-set! hp 0))
    
    ;; Returns a copy of `hp`
    (define (heap-copy hp)
      (new-heap (vector-copy (heap-array hp)) (heap-size hp) (heap-pred<? hp)))
    
    ;; Returns a new vector containing all elements of the heap in descending order
    (define (heap->vector hp)
      (do ((vec (make-vector (heap-size hp)))
           (hpc (heap-copy hp))
           (i 0 (fx1+ i)))
          ((heap-empty? hpc) vec)
        (vector-set! vec i (heap-delete-max! hpc))))
        
    ;; Returns a list containing all elements of the heap in ascending order
    (define (heap->list hp)
      (reverse (heap->reversed-list hp)))
    
    ;; Returns a list containing all elements of the heap in descending order
    (define (heap->reversed-list hp)
      (do ((hpc (heap-copy hp))
           (acc '() (cons (heap-delete-max! hpc) acc)))
          ((heap-empty? hpc) acc)))
    
    ;; Inserts all the items from list `elements` into heap `hp`.
    (define (list->heap! hp elements)
      (let ((pred<? (heap-pred<? hp)))
        (do ((items elements (cdr items))
             (i (fx1+ (heap-size hp)) (fx1+ (heap-size hp))))
            ((null? items))
          (set-size! hp i)
          (do ()
              ((not (and (> i 1) (pred<? (heap-ref hp (heap-parent i)) (car items)))))
            (heap-set! hp i (heap-ref hp (heap-parent i)))
            (set! i (heap-parent i)))
          (heap-set! hp i (car items)))))
    
    ;; Makes a new heap from the given list elements and ordering function.
    (define (list->heap elements pred<?)
      (let ((hp (make-heap pred<?)))
        (list->heap! hp elements)
        hp))
    
    ;; Makes a new heap from the given vector of elements and ordering function.
    (define (vector->heap elements pred<?)
      (let ((hp (make-heap pred<?)))
        (vector-for-each (lambda (element) (heap-add! hp element)) elements)
        hp))
  )
)
