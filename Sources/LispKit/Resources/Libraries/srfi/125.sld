;;; SRFI 125
;;; Intermediate hash tables
;;;
;;; This SRFI defines an interface to hash tables, which are widely recognized as a
;;; fundamental data structure for a wide variety of applications. A hash table is a
;;; data structure that:
;;;
;;;   - Is disjoint from all other types.
;;;   - Provides a mapping from objects known as keys to corresponding objects known
;;;     as values.
;;;   - Keys may be any Scheme objects in some kinds of hash tables, but are restricted
;;;     in other kinds.
;;;   - Values may be any Scheme objects.
;;;   - Has no intrinsic order for the key-value associations it contains.
;;;   - Provides an equality predicate which defines when a proposed key is the same as
;;;     an existing key. No table may contain more than one value for a given key.
;;;   - Provides a hash function which maps a candidate key into a non-negative exact
;;;     integer.
;;;   - Supports mutation as the primary means of setting the contents of a table.
;;;   - Provides key lookup and destructive update in (expected) amortized constant time,
;;;     provided a satisfactory hash function is available.
;;;   - Does not guarantee that whole-table operations work in the presence of concurrent
;;;     mutation of the whole hash table (values may be safely mutated).
;;;
;;; This SRFI aims to accomplish these goals:
;;;
;;;   1. to provide a consistent, generic and widely applicable API for hash tables
;;;   2. to improve code portability by providing a standard hash table facility
;;;      with guaranteed behaviour
;;;   3. to help the programmer by defining utility routines that account for the
;;;      most common situations of using hash tables.
;;;
;;; Specification:
;;;   Copyright © 2016 John Cowan, Will Clinger. All rights reserved.
;;;
;;; Implementation:
;;;   Copyright © 2018 Matthias Zenger. All rights reserved.
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

(define-library (srfi 125)

  (export make-hash-table
          hash-table
          hash-table-unfold
          alist->hash-table
          hash-table?
          hash-table-contains?
          hash-table-exists?
          hash-table-empty?
          hash-table=?
          hash-table-mutable?
          hash-table-ref
          hash-table-ref/default
          hash-table-set!
          hash-table-delete!
          hash-table-intern!
          hash-table-update!
          hash-table-update!/default
          hash-table-pop!
          hash-table-clear!
          hash-table-size
          hash-table-keys
          hash-table-values
          hash-table-entries
          hash-table-find
          hash-table-count
          hash-table-map
          hash-table-for-each
          hash-table-walk
          hash-table-map!
          hash-table-map->list
          hash-table-fold
          hash-table-prune!
          hash-table-copy
          hash-table-empty-copy
          hash-table->alist
          hash-table-union!
          hash-table-merge!
          hash-table-intersection!
          hash-table-difference!
          hash-table-xor!
          hash
          string-hash
          string-ci-hash
          hash-by-identity
          hash-table-equivalence-function
          hash-table-hash-function)

  (import (lispkit core)
          (lispkit control)
          (lispkit dynamic)
          (lispkit list)
          (lispkit math)
          (lispkit string)
          (except (lispkit hashtable) string-hash string-ci-hash symbol-hash equal-hash eq-hash)
          (except (srfi 128) string-hash string-ci-hash symbol-hash)
          (srfi 69 internal)
          (rename (except (srfi 69) string-hash string-ci-hash symbol-hash)
                  (make-hash-table        %make-hash-table)
                  (hash-table-ref         %hash-table-ref)
                  (hash-table-ref/default %hash-table-ref/default)
                  (hash-table-set!        %hash-table-set!)
                  (hash-table-delete!     %hash-table-delete!)
                  (hash-table-fold        %hash-table-fold)
                  (hash-table-copy        %hash-table-copy)
                  (alist->hash-table      %alist->hash-table)))

  (begin

    (define (make-hash-table comparison . args)
      (if (comparator? comparison)
          (let-optionals args ((size default-table-size))
            (%make-hash-table (comparator-equality-predicate comparison)
                              (comparator-hash-function comparison)
                              size))
          (let*-optionals args ((hash (appropriate-hash-function-for comparison))
                                (size default-table-size))
            (%make-hash-table comparison hash size))))

    (define (hash-table comparator . args)
      (let ((ht (make-hash-table comparator)))
        (do ((ls args (cddr ls)))
            ((null? ls) ht)
          (hash-table-set! ht (car ls) (cadr ls)))))

    (define (hash-table-unfold stop? mapper successor seed comparator . args)
      (let-optionals args ((size default-table-size))
        (let ((ht (make-hash-table comparator size)))
          (let lp ((acc seed))
            (if (stop? acc)
                ht
                (call-with-values (lambda () (mapper acc))
                                  (lambda (key value)
                                    (hash-table-set! ht key value)
                                    (lp (successor acc)))))))))

    (define (alist->hash-table alist comparison . args)
      (if (comparator? comparison)
          (let-optionals args ((size default-table-size))
            (%alist->hash-table alist
                                (comparator-equality-predicate comparison)
                                (comparator-hash-function comparison)
                                size))
          (let*-optionals args ((hash (appropriate-hash-function-for comparison))
                                (size default-table-size))
            (%alist->hash-table alist comparison hash size))))

    ;; (hash-table? obj) from (srfi 69)

    (define hash-table-contains? hash-table-exists?)

    (define (hash-table-empty? ht) (zero? (hash-table-size ht)))

    (define (hash-table=? value-cmp ht1 ht2)
      (and (= (hash-table-size ht1) (hash-table-size ht2))
           (let lp ((ls (hash-table-keys ht1)))
             (or (null? ls)
                 (let ((v1 (hash-table-ref/default ht1 (car ls) missing-key))
                       (v2 (hash-table-ref/default ht2 (car ls) missing-key)))
                   (and (not (eq? missing-key v1))
                        (not (eq? missing-key v2))
                        ((comparator-equality-predicate value-cmp) v1 v2)
                        (lp (cdr ls))))))))

    (define (hash-table-mutable? ht)
      (hashtable-mutable? (get-hashtable ht)))

    (define (hash-table-ref ht key . args)
      (let-optionals args ((failure #f)
                           (success #f))
        (let ((value (hashtable-ref (get-hashtable ht) key missing-key)))
          (cond ((eq? value missing-key)
                  (if (procedure? failure)
                      (failure)
                      (error "hash-table-ref: no value associated with" key)))
                (success
                  (success value))
                (else
                  value)))))

    (define (hash-table-ref/default ht key default)
      (hashtable-ref (get-hashtable ht) key default))

    (define (hash-table-set! ht . args)
      (do ((ls args (cddr ls)))
          ((null? ls))
        (%hash-table-set! ht (car ls) (cadr ls))))

    (define (hash-table-delete! ht . keys)
      (for-each (lambda (key) (%hash-table-delete! ht key)) keys))

    (define (hash-table-intern! ht key failure)
      (hash-table-ref ht key (lambda ()
                               (let ((res (failure)))
                                 (hash-table-set! ht key res)
                                 res))))

    ;; (hash-table-update! hash-table key updater [failure [success]]) from (srfi 69)
    ;; (hash-table-update!/default hash-table key updater default) from (srfi 69)

    (define (hash-table-pop! ht)
      (let* ((key (car (hash-table-keys ht)))
             (value (hash-table-ref ht key)))
        (hash-table-delete! ht key)
        (values key value)))

    (define (hash-table-clear! ht)
      (for-each (lambda (key) (hash-table-delete! ht key))
                (hash-table-keys ht)))

    ;; (hash-table-size hash-table) from (srfi 69)
    ;; (hash-table-keys hash-table) from (srfi 69)
    ;; (hash-table-values hash-table) from (srfi 69)

    (define (hash-table-entries ht)
      (values (hash-table-keys ht) (hash-table-values ht)))

    (define (hash-table-find proc ht failure)
      (call-with-current-continuation
        (lambda (return)
          (hash-table-for-each
            (lambda (key value)
              (let ((res (proc key value)))
                (if res (return res))))
            ht)
          (failure))))

    (define (hash-table-count proc ht)
      (let ((count 0))
        (hash-table-for-each
          (lambda (key value)
            (if (proc key value) (set! count (+ count 1))))
          ht)
        count))

    (define (hash-table-map proc cmp ht)
      (let ((ht2 (make-hash-table cmp)))
        (hash-table-for-each (lambda (key value) (hash-table-set! ht2 key (proc value))) ht)
        ht2))

    (define (hash-table-for-each proc ht)
      (hashtable-for-each proc (get-hashtable ht)))

    (define (hash-table-map! proc ht)
      (for-each (lambda (key value) (hash-table-set! ht key (proc key value)))
                (hash-table-keys ht)
                (hash-table-values ht)))

    (define (hash-table-map->list proc ht)
      (map (lambda (cell) (proc (car cell) (cdr cell))) (hash-table->alist ht)))

    (define (hash-table-fold a b c)
      (if (hash-table? a)
          (%hash-table-fold a b c)
          (%hash-table-fold c a b)))

    (define (hash-table-prune! proc ht)
      (for-each (lambda (key value) (if (proc key value) (hash-table-delete! ht key)))
                (hash-table-keys ht)
                (hash-table-values ht)))

    (define (hash-table-copy ht . args)
      (let-optionals args ((mutable #f))
        (new-hash-table (hashtable-copy (get-hashtable ht) mutable))))

    (define (hash-table-empty-copy ht)
      (make-hash-table (hash-table-equivalence-function ht)
                       (hash-table-hash-function ht)))

    ;; (hash-table->alist hash-table) from (srfi 69)

    (define hash-table-union! hash-table-merge!)

    (define (hash-table-intersection! ht1 ht2)
      (for-each (lambda (key)
                  (if (not (hash-table-contains? ht2 key))
                      (hash-table-delete! ht1 key)))
                (hash-table-keys ht1))
      ht1)

    (define (hash-table-difference! ht1 ht2)
      (for-each (lambda (key)
                  (if (hash-table-contains? ht2 key)
                      (hash-table-delete! ht1 key)))
                (hash-table-keys ht1))
      ht1)

    (define (hash-table-xor! ht1 ht2)
      (let ((ht (get-hashtable ht1)))
        (hash-table-for-each
          (lambda (key2 val2)
            (if (hashtable-contains? ht key2)
                (hashtable-delete! ht key2)
                (hashtable-set! ht key2 val2)))
          ht2)
        ht1))
  )
)
