;;; SRFI 69
;;; Basic hash tables
;;;
;;; This SRFI defines basic hash tables. Hash tables are widely recognised as a
;;; fundamental data structure for a wide variety of applications. A hash table
;;; is a data structure that:
;;;
;;;   1. provides a mapping from some set of keys to some set of values
;;;      associated to those keys
;;;   2. has no intrinsic order for the (key, value) associations it contains
;;;   3. supports in-place modification as the primary means of setting the
;;;      contents of a hash table
;;;   4. provides key lookup and destructive update in amortised constant time,
;;;      provided that a good hash function is used.
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
;;;   Copyright © 2005 Panu Kalliokoski. All rights reserved.
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

(define-library (srfi 69)

  (export make-hash-table
          hash-table?
          alist->hash-table
          hash-table-equivalence-function
          hash-table-hash-function
          hash-table-ref
          hash-table-ref/default
          hash-table-set!
          hash-table-delete!
          hash-table-exists?
          hash-table-update!
          hash-table-update!/default
          hash-table-size
          hash-table-keys
          hash-table-values
          hash-table-walk
          hash-table-fold
          hash-table->alist
          hash-table-copy
          hash-table-merge!
          hash
          string-hash
          string-ci-hash
          symbol-hash
          hash-by-identity)

  (import (lispkit core)
          (lispkit control)
          (lispkit dynamic)
          (lispkit list)
          (lispkit math)
          (lispkit string)
          (lispkit type)
          (lispkit box)
          (except (lispkit hashtable) string-hash string-ci-hash symbol-hash equal-hash eq-hash)
          (prefix (only (lispkit hashtable)
                    string-hash string-ci-hash symbol-hash equal-hash eq-hash) native:))

  (begin

    (define *default-table-size* 64)

    (define *undefined* (box 'undefined))

    (define *default-bound* (greatest-fixnum))

    ;; The new hash-table type mostly defines internal functions. Only the hash-table?
    ;; function gets exported.
    ;;
    ;; (hash-table? obj) -> boolean
    ;;
    ;; A predicate to test whether a given object obj is a hash table. The hash table type
    ;; should be disjoint from all other types, if possible.
    (define-values (new-hash-table hash-table? get-hashtable _htst) (make-type 'hash-table))

    ;; (make-hash-table [ equal? [ hash [ args … ]]]) -> hash-table
    ;;
    ;; Create a new hash table with no associations. equal? is a predicate that should accept
    ;; two keys and return a boolean telling whether they denote the same key value; it
    ;; defaults to equal?.
    ;;
    ;; hash is a hash function, and defaults to an appropriate hash function for the given
    ;; equal? predicate (see section Hashing). However, an acceptable default is not guaranteed
    ;; to be given for any equivalence predicate coarser than equal?, except for string-ci=?.
    ;; The function hash must be acceptable for equal?, so if you use coarser equivalence
    ;; than equal? other than string-ci=?, you must always provide the function hash yourself.
    ;;
    ;; Implementations are allowed to use the rest args for implementation-specific extensions.
    ;; Be warned, though, that using these extensions will make your program less portable.
    (define (make-hash-table . args)
      (let* ((comparison (if (null? args) equal? (car args)))
             (hash       (if (or (null? args) (null? (cdr args)))
    	                       (appropriate-hash-function-for comparison)
    	                       (cadr args)))
             (size       (if (or (null? args) (null? (cdr args)) (null? (cddr args)))
                             *default-table-size*
                             (caddr args))))
        (new-hash-table (make-hashtable hash comparison size))))

    ;; (alist->hash-table alist [ equal? [ hash [ args … ]]]) -> hash-table
    ;;
    ;; Takes an association list alist and creates a hash table hash-table which maps
    ;; the car of every element in alist to the cdr of corresponding elements in alist.
    ;; equal?, hash, and args are interpreted as in make-hash-table. If some key occurs
    ;; multiple times in alist, the value in the first association will take precedence
    ;; over later ones.
    ;;
    ;; The rest args are passed to make-hash-table and can thus be used for
    ;; implementation-specific extensions.
    (define (alist->hash-table alist . args)
      (let* ((comparison (if (null? args) equal? (car args)))
             (hash       (if (or (null? args) (null? (cdr args)))
    	                       (appropriate-hash-function-for comparison)
    	                       (cadr args)))
             (size       (if (or (null? args) (null? (cdr args)) (null? (cddr args)))
                             *default-table-size*
                             (caddr args)))
             (hashtable  (make-hashtable hash comparison size)))
        (for-each (lambda (elem)
                    (hashtable-update! hashtable (car elem) identity (cdr elem)))
                  alist)
        (new-hash-table hashtable)))

    (define (appropriate-hash-function-for comparison)
      (or (and (eq? comparison eq?) hash-by-identity)
          (and (eq? comparison string=?) string-hash)
          (and (eq? comparison string-ci=?) string-ci-hash)
          hash))

    ;; (hash-table-equivalence-function hash-table) -> procedure
    ;;
    ;; Returns the equivalence predicate used for keys of hash-table.
    (define (hash-table-equivalence-function ht)
      (hashtable-equivalence-function (get-hashtable ht)))

    ;; (hash-table-hash-function hash-table) -> procedure
    ;;
    ;; Returns the hash function used for keys of hash-table.
    (define (hash-table-hash-function ht)
      (hashtable-hash-function (get-hashtable ht)))

    ;; (hash-table-ref hash-table key [ thunk ]) -> value
    ;;
    ;; This procedure returns the value associated to key in hash-table. If no value is
    ;; associated to key and thunk is given, it is called with no arguments and its value
    ;; is returned; if thunk is not given, an error is signalled. Given a good hash
    ;; function, this operation should have an (amortised) complexity of O(1) with respect
    ;; to the number of associations in hash-table.
    (define (hash-table-ref ht key . maybe-default)
      (let ((value (hashtable-ref (get-hashtable ht) key *undefined*)))
        (if (eq? value *undefined*)
            (if (null? maybe-default)
                (error "hash-table-ref: no value associated with" key)
                ((car maybe-default)))
            value)))

    ;; (hash-table-ref/default hash-table key default) -> value
    ;;
    ;; Evaluates to the same value as (hash-table-ref hash-table key (lambda () default)).
    ;; Given a good hash function, this operation should have an (amortised) complexity
    ;; of O(1) with respect to the number of associations in hash-table.
    (define (hash-table-ref/default ht key default)
      (hashtable-ref (get-hashtable ht) key default))

    ;; (hash-table-set! hash-table key value)
    ;;
    ;; This procedure sets the value associated to key in hash-table. The previous
    ;; association (if any) is removed. Given a good hash function, this operation
    ;; should have an (amortised) complexity of O(1) with respect to the number of
    ;; associations in hash-table.
    (define (hash-table-set! ht key value)
      (hashtable-set! (get-hashtable ht) key value))

    ;; (hash-table-delete! hash-table key)
    ;;
    ;; This procedure removes any association to key in hash-table. It is not an error
    ;; if no association for that key exists; in this case, nothing is done. Given a
    ;; good hash function, this operation should have an (amortised) complexity of O(1)
    ;; with respect to the number of associations in hash-table.
    (define (hash-table-delete! ht key)
      (hashtable-delete! (get-hashtable ht) key))

    ;; (hash-table-exists? hash-table key) -> boolean
    ;;
    ;; This predicate tells whether there is any association to key in hash-table.
    ;; Given a good hash function, this operation should have an (amortised) complexity
    ;; of O(1) with respect to the number of associations in hash-table.
    (define (hash-table-exists? ht key)
      (hashtable-contains? (get-hashtable ht) key))

    ;; (hash-table-update! hash-table key function [ thunk ])
    ;;
    ;; Semantically equivalent to, but may be implemented more efficiently than,
    ;; the following code:
    ;; `(hash-table-set! hash-table key (function (hash-table-ref hash-table key thunk)))`
    (define (hash-table-update! ht key function . maybe-default)
      (hashtable-update! (get-hashtable ht)
                         key
                         (lambda (x)
                           (if (eq? x *undefined*)
                               (if (null? maybe-default)
                                   (error "hash-table-update!: no value exists for key" key)
                                   (function ((car maybe-default))))
                               (function x)))
                         *undefined*))

    ;; (hash-table-update!/default hash-table key function default)
    ;;
    ;; Behaves as if it evaluates to
    ;; `(hash-table-update! hash-table key function (lambda () default))`.
    (define (hash-table-update!/default ht key function default)
      (hashtable-update! (get-hashtable ht) key function default))

    ;; (hash-table-size hash-table) -> integer
    ;;
    ;; Returns the number of associations in hash-table. This operation must have a
    ;; complexity of O(1) with respect to the number of associations in hash-table.
    (define (hash-table-size ht)
      (hashtable-size (get-hashtable ht)))

    ;; (hash-table-keys hash-table) -> list
    ;;
    ;; Returns a list of keys in hash-table. The order of the keys is unspecified.
    (define (hash-table-keys ht)
      (hashtable-key-list (get-hashtable ht)))

    ;; (hash-table-values hash-table) -> list
    ;;
    ;; Returns a list of values in hash-table. The order of the values is unspecified,
    ;; and is not guaranteed to match the order of keys in the result of hash-table-keys.
    (define (hash-table-values ht)
      (hashtable-value-list (get-hashtable ht)))

    ;; (hash-table-walk hash-table proc)
    ;;
    ;; proc should be a function taking two arguments, a key and a value. This procedure
    ;; calls proc for each association in hash-table, giving the key of the association
    ;; as key and the value of the association as value. The results of proc are discarded.
    ;; The order in which proc is called for the different associations is unspecified.
    (define (hash-table-walk ht proc)
      (hashtable-for-each proc (get-hashtable ht)))

    ;; (hash-table-fold hash-table f init-value) -> final-value
    ;;
    ;; This procedure calls f for every association in hash-table with three arguments:
    ;; the key of the association key, the value of the association value, and an
    ;; accumulated value, val. val is init-value for the first invocation of f, and
    ;; for subsequent invocations of f, the return value of the previous invocation
    ;; of f. The value final-value returned by hash-table-fold is the return value of
    ;; the last invocation of f. The order in which f is called for different associations
    ;; is unspecified.
    (define (hash-table-fold ht f acc)
      (hashtable-for-each (lambda (key value) (set! acc (f key value acc))) (get-hashtable ht))
      acc)

    ;; (hash-table->alist hash-table) -> alist
    ;;
    ;; Returns an association list such that the car of each element in alist is a key in
    ;; hash-table and the corresponding cdr of each element in alist is the value associated
    ;; to the key in hash-table. The order of the elements is unspecified.
    (define (hash-table->alist ht)
      (hashtable->alist (get-hashtable ht)))

    ;; (hash-table-copy hash-table) -> hash-table
    ;;
    ;; Returns a new hash table with the same equivalence predicate, hash function and
    ;; mappings as in hash-table.
    (define (hash-table-copy ht)
      (new-hash-table (hashtable-copy (get-hashtable ht) #t)))

    ;; (hash-table-merge! hash-table1 hash-table2) -> hash-table
    ;;
    ;; Adds all mappings in hash-table2 into hash-table1 and returns the resulting hash
    ;; table. This function may modify hash-table1 destructively.
    (define (hash-table-merge! ht1 ht2)
      (hashtable-union! (get-hashtable ht1) (get-hashtable ht2)))

    ;; (hash object [ bound ]) -> integer
    ;;
    ;; Produces a hash value for object in the range ( 0, bound (. If bound is not given,
    ;; the implementation is free to choose any bound, given that the default bound is
    ;; greater than the size of any imaginable hash table in a normal application.
    (define (hash obj . maybe-bound)
      (modulo (native:equal-hash obj) (if (null? maybe-bound) *default-bound* (car maybe-bound))))

    ;; (string-hash string [ bound ]) -> integer
    ;;
    ;; The same as hash, except that the argument string must be a string.
    (define (string-hash s . maybe-bound)
      (modulo (native:string-hash s) (if (null? maybe-bound) *default-bound* (car maybe-bound))))

    ;; (string-ci-hash string [ bound ]) -> integer
    ;;
    ;; The same as string-hash, except that the case of characters in string does not affect
    ;; the hash value produced.
    (define (string-ci-hash s . maybe-bound)
      (modulo (native:string-ci-hash s) (if (null? maybe-bound) *default-bound* (car maybe-bound))))

    ;; (symbol-hash string [ bound ]) -> integer
    ;;
    ;; The same as hash, except that the argument must be a symbol.
    (define (symbol-hash s . maybe-bound)
      (modulo (native:symbol-hash s) (if (null? maybe-bound) *default-bound* (car maybe-bound))))

    ;; (hash-by-identity object [ bound ]) -> integer
    ;;
    ;; The same as hash, except that this function is only guaranteed to be acceptable
    ;; for eq?. The reason for providing this function is that it might be implemented
    ;; significantly more efficiently than hash.
    (define (hash-by-identity obj . maybe-bound)
      (modulo (native:eq-hash obj) (if (null? maybe-bound) *default-bound* (car maybe-bound))))
  )
)

