;;; SRFI 69 INTERNAL
;;; Basic hash tables
;;;
;;; This is a library defining functions for extending the basic SRFI 69 implementation.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2018 Matthias Zenger. All rights reserved.
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

(define-library (srfi 69 internal)

  (export new-hash-table
          hash-table?
          get-hashtable
          default-table-size
          default-bound
          missing-key
          appropriate-hash-function-for
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
          (lispkit box)
          (lispkit type)
          (prefix (only (lispkit hashtable)
             string-hash string-ci-hash symbol-hash equal-hash eq-hash) native:))

  (begin

    (define default-table-size 64)

    (define default-bound (greatest-fixnum))

    (define missing-key (box 'missing-key))

    ;; Defines a new hash-table datatype
    (define-values (new-hash-table hash-table? get-hashtable _htst) (make-type 'hash-table))

    ;; Returns an appropriate hash function for the given equality function `comparison`
    (define (appropriate-hash-function-for comparison)
      (or (and (eq? comparison eq?) hash-by-identity)
          (and (eq? comparison string=?) string-hash)
          (and (eq? comparison string-ci=?) string-ci-hash)
          hash))

    ;; (hash object [ bound ]) -> integer
    ;;
    ;; Produces a hash value for object in the range ( 0, bound (. If bound is not given,
    ;; the implementation is free to choose any bound, given that the default bound is
    ;; greater than the size of any imaginable hash table in a normal application.
    (define (hash obj . maybe-bound)
      (modulo (native:equal-hash obj)
              (if (null? maybe-bound) default-bound (car maybe-bound))))

    ;; (string-hash string [ bound ]) -> integer
    ;;
    ;; The same as hash, except that the argument string must be a string.
    (define (string-hash s . maybe-bound)
      (modulo (native:string-hash s)
              (if (null? maybe-bound) default-bound (car maybe-bound))))

    ;; (string-ci-hash string [ bound ]) -> integer
    ;;
    ;; The same as string-hash, except that the case of characters in string does not affect
    ;; the hash value produced.
    (define (string-ci-hash s . maybe-bound)
      (modulo (native:string-ci-hash s)
              (if (null? maybe-bound) default-bound (car maybe-bound))))

    ;; (symbol-hash string [ bound ]) -> integer
    ;;
    ;; The same as hash, except that the argument must be a symbol.
    (define (symbol-hash s . maybe-bound)
      (modulo (native:symbol-hash s)
              (if (null? maybe-bound) default-bound (car maybe-bound))))

    ;; (hash-by-identity object [ bound ]) -> integer
    ;;
    ;; The same as hash, except that this function is only guaranteed to be acceptable
    ;; for eq?. The reason for providing this function is that it might be implemented
    ;; significantly more efficiently than hash.
    (define (hash-by-identity obj . maybe-bound)
      (modulo (native:eq-hash obj)
              (if (null? maybe-bound) default-bound (car maybe-bound))))
  )
)
