;;; LISPKIT SET
;;; 
;;; Implementation of sets that is compatible to the R6RS-style hashtables natively
;;; provided by LispKit.
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

(define-library (lispkit set)

  (export make-eq-set
          make-eqv-set
          make-equal-set
          make-set
          eq-set
          eqv-set
          equal-set
          set?
          set-mutable?
          set-empty?
          set=?
          disjoint?
          subset?
          proper-subset?
          set-contains?
          set-any?
          set-every?
          set-for-each
          set-size
          set-adjoin!
          set-delete!
          set-clear!
          set-copy
          set-filter
          set-filter!
          set-union
          set-union!
          set-intersection
          set-intersection!
          set-difference
          set-difference!
          set-equivalence-function
          set-hash-function
          set-elements
          set->list
          list->set!
          list->eq-set
          list->eqv-set
          list->equal-set)
  
  (import (lispkit base))
  
  (begin

    (define-values (new-set set? set-ref make-set-subtype) (make-type 'set))
    
    ;; Make a new empty set using `eq?` as equivalence function
    (define (make-eq-set)
      (new-set (make-eq-hashtable)))
  
    ;; Make a new empty set using `eqv?` as equivalence function
    (define (make-eqv-set)
      (new-set (make-eqv-hashtable)))
    
    ;; Make a new empty set using `equal?` as equivalence function
    (define (make-equal-set)
      (new-set (make-equal-hashtable)))
    
    ;; Make a new empty set using the given hash and equivalence function. A capacity can
    ;; be provided optionally.
    (define (make-set hash equiv . k)
      (if (pair? k)
          (new-set (make-hashtable hash equiv (car k)))
          (new-set (make-hashtable hash equiv))))
    
    ;; Make a new set using `eq?` as equivalence function. Initialize it with the values
    ;; from `elements`.
    (define (eq-set . elements)
      (list->eq-set elements))
  
    ;; Make a new set using `eqv?` as equivalence function. Initialize it with the values
    ;; from `elements`.
    (define (eqv-set . elements)
      (list->eqv-set elements))
  
    ;; Make a new set using `equal?` as equivalence function. Initialize it with the values
    ;; from `elements`.
    (define (equal-set . elements)
      (list->equal-set elements))
  
    ;; Returns true if set `s` is mutable.
    (define (set-mutable? s)
      (hashtable-mutable? (set-ref s)))
  
    ;; Returns true if set `s` is empty.
    (define (set-empty? s)
      (= 0 (hashtable-size (set-ref s))))
  
    ;; Returns true if set `s1` and `s2` are using the same equivalence function.
    (define (set-comparable? s1 s2)
      (eq? (hashtable-equivalence-function (set-ref s1))
           (hashtable-equivalence-function (set-ref s2))))
  
    ;; Returns true if set `s1` and `s2` are using the same equivalence function and contain
    ;; the same elements.
    (define (set=? s1 s2)
      (and (set-comparable? s1 s2)
           (= (set-size s1) (set-size s2))
           (set-every? s1 (lambda (x) (set-contains? s2 x)))))
    
    ;; Returns true if set `s1` and `s2` are disjoint.
    (define (disjoint? s1 s2)
      (and (set-comparable? s1 s2)
           (set-every? s1 (lambda (x) (not (set-contains? s2 x))))))
  
    ;; Returns true if set `s1` is a subset of `s2`.
    (define (subset? s1 s2)
      (and (set-comparable? s1 s2)
           (set-every? s1 (lambda (x) (set-contains? s2 x)))))
  
    ;; Returns true if set `s1` is a proper subset of `s2`.
    (define (proper-subset? s1 s2)
      (and (set-comparable? s1 s2)
           (< (set-size s1) (set-size s2))
           (set-every? s1 (lambda (x) (set-contains? s2 x)))))
    
    ;; Returns true if set `s` contains `element`.
    (define (set-contains? s element)
      (pair? (hashtable-get (set-ref s) element)))
    
    ;; Returns true if there is at least one element in set `s` for which procedure `proc`
    ;; returns true.
    (define (set-any? s proc)
      (let loop ((keys (hashtable-key-list (set-ref s))))
        (if (pair? keys)
            (if (proc (car keys)) #t (loop (cdr keys)))
            #f)))
    
    ;; Returns true if procedure `proc` returns true for all elements of set `s`.
    (define (set-every? s proc)
      (let loop ((keys (hashtable-key-list (set-ref s))))
        (if (pair? keys)
            (if (proc (car keys)) (loop (cdr keys)) #f)
            #t)))
    
    ;; Applies procedure `proc` to all elements of set `s` in an undefined order.
    (define (set-for-each s proc)
      (do ((keys (hashtable-key-list (set-ref s)) (cdr keys)))
          ((null? keys))
        (proc (car keys))))
    
    ;; Returns the number of elements in set `s`.
    (define (set-size s)
      (hashtable-size (set-ref s)))
    
    ;; Adds `elements` to the set `s`.
    (define (set-adjoin! s . elements)
      (list->set! s elements))
  
    ;; Deletes `elements` from the set `s`.
    (define (set-delete! s . elements)
      (let ((ht (set-ref s)))
        (for-each (lambda (element) (hashtable-delete! ht element)) elements)))

    ;; Clears set `s` and reserves a capacity of `k` elements.
    (define (set-clear! s . k)
      (if (pair? k)
        (hashtable-clear! (set-ref s) (car k))
        (hashtable-clear! (set-ref s))))
  
    ;; Copies set `s` creating an immutable copy if `mutable` is set to false or if `mutable`
    ;; is left out.
    (define (set-copy s . mutable)
      (if (pair? mutable)
          (new-set (hashtable-copy (set-ref s) (car mutable)))
          (new-set (hashtable-copy (set-ref s)))))
  
    ;; Creates a new set containing the elements of set `s` for which the procedure `pred`
    ;; returns true.
    (define (set-filter s pred)
      (let ((s2 (set-copy s #t)))
        (set-filter! s2 pred)
        s2))
    
    ;; Removes all elements from set `s` for which procedure `pred` returns false.
    (define (set-filter! s pred)
      (let ((ht (set-ref s)))
        (do ((keys (hashtable-key-list ht) (cdr keys)))
            ((null? keys))
          (if (not (pred (car keys))) (hashtable-delete! ht (car keys))))))
    
    ;; Creates a new set containing the union of `s1` and the sets in `rest`.
    (define (set-union s1 . rest)
      (let* ((s (set-copy s1 #t))
             (ht (set-ref s)))
        (do ((s2 rest (cdr s2)))
            ((null? s2) s)
          (hashtable-union! ht (set-ref (car s2))))))
    
    ;; Stores the union of set `s1` and the sets in `elements` in `s1`.
    (define (set-union! s1 . rest)
      (do ((s2 rest (cdr s2)))
          ((null? s2))
        (hashtable-union! (set-ref s1) (set-ref (car s2)))))

    ;; Creates a new set containing the intersection of `s1` and the sets in `rest`.
    (define (set-intersection s1 . rest)
      (let* ((s (set-copy s1 #t))
             (ht (set-ref s)))
        (do ((s2 rest (cdr s2)))
            ((null? s2) s)
          (hashtable-intersection! ht (set-ref (car s2))))))
          
    ;; Stores the intersection of set `s1` and the sets in `elements` in `s1`.
    (define (set-intersection! s1 . rest)
      (do ((s2 rest (cdr s2)))
          ((null? s2))
        (hashtable-intersection! (set-ref s1) (set-ref (car s2)))))

    ;; Creates a new set containing the difference of `s1` and the sets in `rest`.
    (define (set-difference s1 . rest)
      (let* ((s (set-copy s1 #t))
             (ht (set-ref s)))
        (do ((s2 rest (cdr s2)))
            ((null? s2) s)
          (hashtable-difference! ht (set-ref (car s2))))))
    
    ;; Stores the difference of set `s1` and the sets in `elements` in `s1`.
    (define (set-difference! s1 . rest)
      (do ((s2 rest (cdr s2)))
          ((null? s2))
        (hashtable-difference! (set-ref s1) (set-ref (car s2)))))

    ;; Returns the equivalence function used by set `s`.
    (define (set-equivalence-function s)
      (hashtable-equivalence-function (set-ref s)))
  
    ;; Returns the hash function used by set `s`.
    (define (set-hash-function s)
      (hashtable-hash-function (set-ref s)))
  
    ;; Returns the elements of set `s` as a vector.
    (define (set-elements s)
      (hashtable-keys (set-ref s)))
  
    ;; Returns the elements of set `s` as a list.
    (define (set->list s)
      (hashtable-key-list (set-ref s)))
    
    ;; Adds the values of list `elements` to set `s`.
    (define (list->set! s elements)
      (let ((ht (set-ref s)))
        (for-each (lambda (element) (hashtable-add! ht element '())) elements)))
  
    ;; Creates a new set using the equivalence function `eq?` from the values in list
    ;; `elements`.
    (define (list->eq-set elements)
      (let ((s (make-eq-set)))
        (list->set! s elements)
        s))
    
    ;; Creates a new set using the equivalence function `eqv?` from the values in list
    ;; `elements`.
    (define (list->eqv-set elements)
      (let ((s (make-eqv-set)))
        (list->set! s elements)
        s))
    
    ;; Creates a new set using the equivalence function `equal?` from the values in list
    ;; `elements`.
    (define (list->equal-set elements)
      (let ((s (make-equal-set)))
        (list->set! s elements)
        s))
  )
)
