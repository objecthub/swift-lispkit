;;; SRFI 209
;;; Enums and Enum Sets
;;; 
;;; Many procedures in many libraries accept arguments from a finite set (usually a fairly
;;; small one), or subsets of a finite set to describe one or more modes of operation.
;;; Offering a mechanism for dealing with such values fosters portable and readable code,
;;; much as records do for compound values, or multiple values for procedures computing
;;; several results.
;;;
;;; This SRFI provides abstractions similar to enums of Java version 5 and later. These
;;; are objects of a type disjoint from all others that are grouped into enum types
;;; (called enum classes in Java). In Java, each enum type declares the names and types
;;; of values associated with each object, but in this SRFI an enum object has exactly
;;; one value; this is useful when translating from C to record the numeric value, but
;;; has other uses as well.
;;; 
;;; Each enum has four properties: the enum type to which it belongs, its name (a symbol),
;;; its ordinal (an exact integer), and its value (any object). An enum type provides
;;; access to all the enums that belong to it by name or ordinal.
;;; 
;;; Enum sets are used to represent multiple enums that belong to the same type. They
;;; provide a subset of the operations provided by SRFI 113 general sets.
;;; 
;;; Copyright © 2020 Wolfgang Corcoran-Mathe. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software, and to permit
;;; persons to whom the Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies or
;;; substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;;; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2020 Matthias Zenger. All rights reserved.

(define-library (srfi 209)
  
  (import (lispkit base)
          (srfi 1)
          (srfi 128)
          (except (srfi 151) bit-set? copy-bit)
          (srfi 145)
          (srfi 146)
          (srfi 162))
  
  (export enum-type?
          enum?
          enum-type-contains?
          enum=?
          enum<?
          enum>?
          enum<=?
          enum>=?
          make-enum-type
          enum-type
          enum-name
          enum-ordinal
          enum-value
          enum-name->enum
          enum-ordinal->enum
          enum-name->ordinal
          enum-name->value
          enum-ordinal->name
          enum-ordinal->value
          enum-type-size
          enum-min enum-max
          enum-type-enums
          enum-type-names
          enum-type-values
          enum-next
          enum-prev
          enum-type->enum-set
          enum-set
          list->enum-set
          enum-set-projection
          enum-set-copy
          enum-empty-set
          make-enumeration
          enum-set-universe
          enum-set-constructor
          enum-set-type
          enum-set-indexer
          enum-set?
          enum-set-contains?
          enum-set=?
          enum-set-member?
          enum-set-empty?
          enum-set-disjoint?
          enum-set<?
          enum-set>?
          enum-set<=?
          enum-set>=?
          enum-set-any?
          enum-set-every?
          enum-set-subset?
          enum-set-adjoin!
          enum-set-delete!
          enum-set-delete-all!
          enum-set-adjoin
          enum-set-delete
          enum-set-delete-all
          enum-set-size
          enum-set->list
          enum-set-map->list
          enum-set-for-each
          enum-set-filter
          enum-set-remove
          enum-set-count
          enum-set-fold
          enum-set->enum-list
          enum-set-union
          enum-set-intersection
          enum-set-difference
          enum-set-xor
          enum-set-complement
          enum-set-union!
          enum-set-intersection!
          enum-set-difference!
          enum-set-xor!
          enum-set-complement!
          make-enum-comparator
          define-enum
          define-enumeration)

  (begin
    
    ;;;; Utility

    (define (exact-natural? obj)
      (and (exact-integer? obj) (not (negative? obj))))
    
    ;;;; Types
    
    (define-record-type <enum-type>
      (make-raw-enum-type enum-vector name-table comparator)
      enum-type?
      (enum-vector enum-type-enum-vector set-enum-type-enum-vector!)
      (name-table enum-type-name-table set-enum-type-name-table!)
      (comparator enum-type-comparator set-enum-type-comparator!))
    
    (define-record-type <enum>
      (make-enum type name ordinal value)
      enum?
      (type enum-type)
      (name enum-name)
      (ordinal enum-ordinal)
      (value enum-value))
    
    (define (make-enum-type names+vals)
      (assume (or (pair? names+vals) (null? names+vals)))
      (let* ((type (make-raw-enum-type #f #f #f))
             (enums (generate-enums type names+vals)))
        (set-enum-type-enum-vector! type (list->vector enums))
        (set-enum-type-name-table! type (make-name-table enums))
        (set-enum-type-comparator! type (make-enum-comparator type))
        type))
    
    (define (generate-enums type names+vals)
      (map (lambda (elt ord)
             (cond ((and (pair? elt) (= 2 (length elt)) (symbol? (car elt)))
                    (make-enum type (car elt) ord (cadr elt)))
                   ((symbol? elt) (make-enum type elt ord ord))
                   (else (error "make-enum-type: invalid argument" elt))))
           names+vals
           (iota (length names+vals))))
    
    (define symbol-comparator
      (make-comparator symbol?
                       eqv?
                       (lambda (sym1 sym2)
                         (string<? (symbol->string sym1)
                                   (symbol->string sym2)))
                       symbol-hash))
    
    (define (make-name-table enums)
      (mapping-unfold null?
                      (lambda (enums)
                        (values (enum-name (car enums)) (car enums)))
                      cdr
                      enums
                      symbol-comparator))
    
    (define (%enum-type=? etype1 etype2)
      (eqv? etype1 etype2))
    
    (define (make-enum-comparator type)
      (make-comparator
       (lambda (obj) (and (enum? obj) (eq? (enum-type obj) type)))
       eq?
       (lambda (enum1 enum2) (< (enum-ordinal enum1) (enum-ordinal enum2)))
       (lambda (enum) (symbol-hash (enum-name enum)))))
    
    ;;;; Predicates
    
    (define (enum-type-contains? type enum)
      (assume (enum-type? type))
      (assume (enum? enum))
      ((comparator-type-test-predicate (enum-type-comparator type)) enum))
    
    (define (%enum-type-contains?/no-check type enum)
      ((comparator-type-test-predicate (enum-type-comparator type)) enum))
    
    (define (%well-typed-enum? type obj)
      (and (enum? obj) (%enum-type-contains?/no-check type obj)))
    
    (define (%compare-enums compare enums)
      (assume (and (pair? enums) (pair? (cdr enums)))
              "invalid number of arguments")
      (assume (enum? (car enums)))
      (let ((type (enum-type (car enums))))
        (assume (every (lambda (e) (%well-typed-enum? type e)) (cdr enums))
                "invalid arguments")
        (apply compare (enum-type-comparator type) enums)))
    
    (define (enum=? enum1 enum2 . enums)
      (assume (enum? enum1))
      (let* ((type (enum-type enum1))
             (comp (enum-type-comparator type)))
        (cond ((null? enums)                            ; fast path
               (assume (%well-typed-enum? type enum2) "enum=?: invalid argument")
               ((comparator-equality-predicate comp) enum1 enum2))
              (else                                     ; variadic path
               (assume (every (lambda (e) (%well-typed-enum? type e)) enums)
                       "enum=?: invalid arguments")
               (apply =? comp enum1 enum2 enums)))))
    
    (define (enum<? . enums) (%compare-enums <? enums))
    
    (define (enum>? . enums) (%compare-enums >? enums))
    
    (define (enum<=? . enums) (%compare-enums <=? enums))
    
    (define (enum>=? . enums) (%compare-enums >=? enums))
    
    ;;;; Enum finders
    
    ;;; Core procedures
    
    (define (enum-name->enum type name)
      (assume (enum-type? type))
      (assume (symbol? name))
      (mapping-ref/default (enum-type-name-table type) name #f))
    
    (define (enum-ordinal->enum enum-type ordinal)
      (assume (enum-type? enum-type))
      (assume (exact-natural? ordinal))
      (and (< ordinal (enum-type-size enum-type))
           (vector-ref (enum-type-enum-vector enum-type) ordinal)))
    
    ;;; Derived procedures
    
    (define (%enum-project type finder key proc)
      (assume (enum-type? type))
      (cond ((finder type key) => proc)
            (else (error "no enum found" type key))))
    
    (define (enum-name->ordinal type name)
      (assume (symbol? name))
      (%enum-project type enum-name->enum name enum-ordinal))
    
    (define (enum-name->value type name)
      (assume (symbol? name))
      (%enum-project type enum-name->enum name enum-value))
    
    (define (enum-ordinal->name type ordinal)
      (assume (exact-natural? ordinal))
      (%enum-project type enum-ordinal->enum ordinal enum-name))
    
    (define (enum-ordinal->value type ordinal)
      (assume (exact-natural? ordinal))
      (%enum-project type enum-ordinal->enum ordinal enum-value))
    
    ;;;; Enum type accessors
    
    (define (enum-type-size type)
      (assume (enum-type? type))
      (vector-length (enum-type-enum-vector type)))
    
    (define (enum-min type)
      (assume (enum-type? type))
      (vector-ref (enum-type-enum-vector type) 0))
    
    (define (enum-max type)
      (assume (enum-type? type))
      (let ((vec (enum-type-enum-vector type)))
        (vector-ref vec (- (vector-length vec) 1))))
    
    (define (enum-type-enums type)
      (assume (enum-type? type))
      (vector->list (enum-type-enum-vector type)))
    
    (define (enum-type-names type)
      (assume (enum-type? type))
      (let ((vec (enum-type-enum-vector type)))
        (list-tabulate (vector-length vec)
                       (lambda (n) (enum-name (vector-ref vec n))))))
    
    (define (enum-type-values type)
      (assume (enum-type? type))
      (let ((vec (enum-type-enum-vector type)))
        (list-tabulate (vector-length vec)
                       (lambda (n) (enum-value (vector-ref vec n))))))
    
    ;;;; Enum object procedures
    
    (define (enum-next enum)
      (assume (enum? enum))
      (enum-ordinal->enum (enum-type enum) (+ (enum-ordinal enum) 1)))
    
    (define (enum-prev enum)
      (assume (enum? enum))
      (let ((ord (enum-ordinal enum)))
        (and (> ord 0)
             (enum-ordinal->enum (enum-type enum) (- ord 1)))))
    
    ;;;; Enum set constructors
    
    (define-record-type <enum-set>
      (make-enum-set type bitmap)
      enum-set?
      (type enum-set-type)
      (bitmap enum-set-bitmap set-enum-set-bitmap!))
    
    (define (enum-empty-set type)
      (assume (enum-type? type))
      (make-enum-set type 0))
    
    (define (enum-type->enum-set type)
      (assume (enum-type? type))
      (make-enum-set type (- (expt 2 (enum-type-size type)) 1)))
    
    (define (enum-set type . enums) (list->enum-set type enums))
    
    (define (list->enum-set type enums)
      (assume (or (pair? enums) (null? enums)))
      (make-enum-set
       type
       (fold (lambda (e b)
               (assume (%well-typed-enum? type e) "ill-typed enum")
               (bitwise-ior b (expt 2 (enum-ordinal e))))
             0
             enums)))
    
    ;; Returns a set of enums drawn from the enum-type/-set src with
    ;; the same names as the enums of eset.
    (define (enum-set-projection src eset)
      (assume (or (enum-type? src) (enum-set? src)))
      (assume (enum-set? eset))
      (let ((type (if (enum-type? src) src (enum-set-type src))))
        (list->enum-set
         type
         (enum-set-map->list (lambda (enum)
                               (enum-name->enum type (enum-name enum)))
                             eset))))
    
    (define (enum-set-copy eset)
      (make-enum-set (enum-set-type eset) (enum-set-bitmap eset)))
    
    ;; [Deprecated]
    (define (make-enumeration names)
      (enum-type->enum-set (make-enum-type (zip names names))))
    
    ;; [Deprecated]
    (define (enum-set-universe eset)
      (assume (enum-set? eset))
      (enum-type->enum-set (enum-set-type eset)))
    
    ;; [Deprecated]  Returns a procedure which takes a list of symbols
    ;; and returns an enum set containing the corresponding enums.  This
    ;; extracts the type of eset, but otherwise ignores this argument.
    (define (enum-set-constructor eset)
      (assume (enum-set? eset))
      (let ((type (enum-set-type eset)))
        (lambda (names)
          (list->enum-set type
                          (map (lambda (sym) (enum-name->enum type sym))
                               names)))))
    
    ;; [Deprecated] Returns a procedure which takes a symbol and returns
    ;; the corresponding enum ordinal or #f.  This doesn't make any use
    ;; of eset, beyond pulling out its enum type.
    (define (enum-set-indexer eset)
      (assume (enum-set? eset))
      (let ((type (enum-set-type eset)))
        (lambda (name)
          (cond ((enum-name->enum type name) => enum-ordinal)
                (else #f)))))
    
    ;;;; Enum set predicates
    
    (define (enum-set-contains? eset enum)
      (assume (enum-set? eset))
      (assume (%well-typed-enum? (enum-set-type eset) enum)
              "enum-set-contains?: invalid argument")
      (not (zero? (bitwise-and (enum-set-bitmap eset)
                               (expt 2 (enum-ordinal enum))))))
    
    ;; FIXME: Avoid double (type, then set) lookup.
    (define (enum-set-member? name eset)
      (assume (symbol? name))
      (assume (enum-set? eset))
      (let ((ord (enum-name->ordinal (enum-set-type eset) name)))
        (not (zero? (bitwise-and (enum-set-bitmap eset) (expt 2 ord))))))
    
    (define (%enum-set-type=? eset1 eset2)
      (%enum-type=? (enum-set-type eset1) (enum-set-type eset2)))
    
    (define (enum-set-empty? eset)
      (assume (enum-set? eset))
      (zero? (enum-set-bitmap eset)))
    
    (define (enum-set-disjoint? eset1 eset2)
      (assume (enum-set? eset1))
      (assume (enum-set? eset2))
      (assume (%enum-type=? (enum-set-type eset1) (enum-set-type eset2)))
      (zero? (bitwise-and (enum-set-bitmap eset1) (enum-set-bitmap eset2))))
    
    (define (enum-set=? eset1 eset2)
      (= (enum-set-bitmap eset1) (enum-set-bitmap eset2)))
    
    (define (enum-set<? eset1 eset2)
      (assume (enum-set? eset1))
      (assume (enum-set? eset2))
      (assume (%enum-type=? (enum-set-type eset1) (enum-set-type eset2)))
      (not (zero? (bitwise-andc1 (enum-set-bitmap eset1)
                                 (enum-set-bitmap eset2)))))
    
    (define (enum-set>? eset1 eset2)
      (assume (enum-set? eset1))
      (assume (enum-set? eset2))
      (assume (%enum-type=? (enum-set-type eset1) (enum-set-type eset2)))
      (not (zero? (bitwise-andc2 (enum-set-bitmap eset1)
                                 (enum-set-bitmap eset2)))))
    
    (define (enum-set<=? eset1 eset2)
      (assume (enum-set? eset1))
      (assume (enum-set? eset2))
      (assume (%enum-type=? (enum-set-type eset1) (enum-set-type eset2)))
      (zero? (bitwise-andc2 (enum-set-bitmap eset1)
                            (enum-set-bitmap eset2))))
    
    (define (enum-set>=? eset1 eset2)
      (assume (enum-set? eset1))
      (assume (enum-set? eset2))
      (assume (%enum-type=? (enum-set-type eset1) (enum-set-type eset2)))
      (zero? (bitwise-andc1 (enum-set-bitmap eset1)
                            (enum-set-bitmap eset2))))
    
    (define (%enum-set->name-mapping eset)
      (mapping-unfold null?
                      (lambda (syms) (values (car syms) #t))
                      cdr
                      (enum-set-map->list enum-name eset)
                      symbol-comparator))
    
    (define (enum-set-subset? eset1 eset2)
      (assume (enum-set? eset1))
      (assume (enum-set? eset2))
      (mapping<=? symbol-comparator
                  (%enum-set->name-mapping eset1)
                  (%enum-set->name-mapping eset2)))
    
    (define (enum-set-any? pred eset)
      (assume (procedure? pred))
      (call-with-current-continuation
       (lambda (return)
         (enum-set-fold (lambda (e _) (and (pred e) (return #t)))
                        #f
                        eset))))
    
    (define (enum-set-every? pred eset)
      (assume (procedure? pred))
      (call-with-current-continuation
       (lambda (return)
         (enum-set-fold (lambda (e _) (or (pred e) (return #f)))
                        #t
                        eset))))
    
    ;;;; Enum set mutators
    
    ;; Fold a list of enums into a bitmap of their ordinals.
    (define (%enum-list->bitmap type enums)
      (fold (lambda (enum b)
              (assume (%well-typed-enum? type enum))
              (bitwise-ior b (expt 2 (enum-ordinal enum))))
            0
            enums))
    
    (define enum-set-adjoin
      (case-lambda
        ((eset enum)                 ; fast path
         (assume (enum-set? eset))
         (let ((type (enum-set-type eset)))
           (assume (%well-typed-enum? type enum)
                   "enum-set-adjoin: invalid argument"
                   enum)
           (make-enum-set
            type
            (bitwise-ior (enum-set-bitmap eset)
                         (expt 2 (enum-ordinal enum))))))
        ((eset . enums)              ; variadic path
         (assume (enum-set? eset))
         (let ((type (enum-set-type eset)))
           (make-enum-set
            type
            (bitwise-ior (enum-set-bitmap eset)
                         (%enum-list->bitmap type enums)))))))
    
    (define enum-set-adjoin!
      (case-lambda
        ((eset enum)                 ; fast path
         (assume (enum-set? eset))
         (assume (%well-typed-enum? (enum-set-type eset) enum))
         (set-enum-set-bitmap!
          eset
          (bitwise-ior (enum-set-bitmap eset)
                       (expt 2 (enum-ordinal enum))))
         eset)
        ((eset . enums)              ; variadic path
         (assume (enum-set? eset))
         (set-enum-set-bitmap!
          eset
          (bitwise-ior (enum-set-bitmap eset)
                       (%enum-list->bitmap (enum-set-type eset) enums)))
         eset)))
    
    (define enum-set-delete
      (case-lambda
        ((eset enum)                ; fast path
         (assume (enum-set? eset))
         (let ((type (enum-set-type eset)))
           (assume (%well-typed-enum? type enum) "ill-typed enum" enum type)
           (make-enum-set type
                          (bitwise-andc2 (enum-set-bitmap eset)
                                         (expt 2 (enum-ordinal enum))))))
        ((eset . enums)             ; variadic path
         (enum-set-delete-all eset enums))))
    
    (define enum-set-delete!
      (case-lambda
        ((eset enum)                ; fast path
         (assume (enum-set? eset))
         (let ((type (enum-set-type eset)))
           (assume (%well-typed-enum? type enum) "ill-typed enum" enum type)
           (set-enum-set-bitmap!
            eset
            (bitwise-andc2 (enum-set-bitmap eset)
                           (expt 2 (enum-ordinal enum))))
           eset))
        ((eset . enums)             ; variadic path
         (enum-set-delete-all! eset enums))))
    
    (define (enum-set-delete-all eset enums)
      (assume (enum-set? eset))
      (assume (or (pair? enums) (null? enums)))
      (let ((type (enum-set-type eset)))
        (make-enum-set type
                       (bitwise-andc2 (enum-set-bitmap eset)
                                      (%enum-list->bitmap type enums)))))
    
    (define (enum-set-delete-all! eset enums)
      (assume (enum-set? eset))
      (assume (or (pair? enums) (null? enums)))
      (if (null? enums)
          eset
          (begin
           (set-enum-set-bitmap!
            eset
            (bitwise-andc2 (enum-set-bitmap eset)
                           (%enum-list->bitmap (enum-set-type eset) enums)))
           eset)))
    
    ;;;; Enum set operations
    
    (define (enum-set-size eset)
      (assume (enum-set? eset))
      (bit-count (enum-set-bitmap eset)))
    
    (define (enum-set->enum-list eset)
      (assume (enum-set? eset))
      (enum-set-fold cons '() eset))
    
    (define (enum-set->list eset)
      (enum-set-map->list enum-name eset))
    
    (define (enum-set-map->list proc eset)
      (assume (procedure? proc))
      (enum-set-fold (lambda (e res) (cons (proc e) res)) '() eset))
    
    (define (enum-set-count pred eset)
      (assume (procedure? pred))
      (enum-set-fold (lambda (e n) (if (pred e) (+ n 1) n)) 0 eset))
    
    ;; TODO: Optimize this.
    (define (enum-set-filter pred eset)
      (assume (enum-set? eset))
      (let ((type (enum-set-type eset))
            (bitmap (enum-set-bitmap eset)))
        (make-enum-set
         type
         (fold (lambda (p m)
                 (let ((i (car p)) (b (cadr p)))
                   (if (and b (pred (enum-ordinal->enum type i)))
                       (bitwise-ior m (expt 2 i))
                       m)))
               0
               (zip (iota (integer-length bitmap))
                    (bits->list bitmap))))))
    
    ;; TODO: Optimize this.
    (define (enum-set-remove pred eset)
      (assume (enum-set? eset))
      (let ((type (enum-set-type eset))
            (bitmap (enum-set-bitmap eset)))
        (make-enum-set
         type
         (fold (lambda (p m)
                 (let ((i (car p)) (b (cadr p)))
                   (if (and b (pred (enum-ordinal->enum type i)))
                       m
                       (bitwise-ior m (expt 2 i)))))
               0
               (zip (iota (integer-length bitmap))
                    (bits->list bitmap))))))
    
    (define (enum-set-for-each proc eset)
      (assume (procedure? proc))
      (enum-set-fold (lambda (e _) (proc e)) '() eset))
    
    ;; TODO: Optimize this.
    (define (enum-set-fold proc nil eset)
      (assume (procedure? proc))
      (assume (enum-set? eset))
      (let ((type (enum-set-type eset))
            (bitmap (enum-set-bitmap eset)))
        (cadr
         (fold-right (lambda (b p)
                       (let ((i (car p)) (state (cadr p)))
                         (if b
                             (list (- i 1)
                                   (proc (enum-ordinal->enum type i) state))
                             (list (- i 1) state))))
                     (list (- (integer-length bitmap) 1) nil)
                     (bits->list bitmap)))))
    
    ;;;; Enum set logical operations
    
    (define (%enum-set-logical-op proc eset1 eset2)
      (assume (enum-set? eset1))
      (assume (enum-set? eset2))
      (assume (%enum-set-type=? eset1 eset2) "enum sets have different types")
      (make-enum-set
       (enum-set-type eset1)
       (proc (enum-set-bitmap eset1) (enum-set-bitmap eset2))))
    
    (define (%enum-set-logical-op! proc eset1 eset2)
      (assume (enum-set? eset1))
      (assume (enum-set? eset2))
      (assume (%enum-set-type=? eset1 eset2) "enum sets have different types")
      (set-enum-set-bitmap! eset1
                            (proc (enum-set-bitmap eset1)
                                  (enum-set-bitmap eset2)))
      eset1)
    
    (define (enum-set-union eset1 eset2)
      (%enum-set-logical-op bitwise-ior eset1 eset2))
    
    (define (enum-set-intersection eset1 eset2)
      (%enum-set-logical-op bitwise-and eset1 eset2))
    
    (define (enum-set-difference eset1 eset2)
      (%enum-set-logical-op bitwise-andc2 eset1 eset2))
    
    (define (enum-set-xor eset1 eset2)
      (%enum-set-logical-op bitwise-xor eset1 eset2))
    
    (define (enum-set-union! eset1 eset2)
      (%enum-set-logical-op! bitwise-ior eset1 eset2))
    
    (define (enum-set-intersection! eset1 eset2)
      (%enum-set-logical-op! bitwise-and eset1 eset2))
    
    (define (enum-set-difference! eset1 eset2)
      (%enum-set-logical-op! bitwise-andc2 eset1 eset2))
    
    (define (enum-set-xor! eset1 eset2)
      (%enum-set-logical-op! bitwise-xor eset1 eset2))
    
    (define (enum-set-complement eset)
      (assume (enum-set? eset))
      (%enum-set-logical-op bitwise-andc1 eset (enum-set-universe eset)))
    
    (define (enum-set-complement! eset)
      (assume (enum-set? eset))
      (%enum-set-logical-op! bitwise-andc1 eset (enum-set-universe eset)))
    
    ;;;; Syntax
    
    ;; Defines a new enum-type T, binds type-name to a macro which
    ;; takes a symbol to an enum in T, and binds constructor to a
    ;; macro taking symbols to an enum set of type T.
    (define-syntax define-enum
      (syntax-rules ()
        ((_ type-name (name-val ...) constructor)
         (begin
          (define etype (make-enum-type '(name-val ...)))
          (define-syntax type-name
            (syntax-rules ()
              ((_ name)
               (enum-name->enum etype 'name))))
          (define-syntax constructor
            (syntax-rules ()
              ((_ . names)
               (list->enum-set etype
                               (map (lambda (s)
                                      (enum-name->enum etype s))
                                    'names)))))))))
    
    ;; [Deprecated] As define-enum, except that type-name is bound to
    ;; a macro that returns its symbol argument if the corresponding
    ;; enum is in the new type.
    (define-syntax define-enumeration
      (syntax-rules ()
        ((_ type-name (name-val ...) constructor)
         (begin
          (define etype (make-enum-type '(name-val ...)))
          (define-syntax type-name
            (syntax-rules ()
              ((_ name)
               (and (enum-name->enum etype 'name) 'name))))
          (define-syntax constructor
            (syntax-rules ()
              ((_ . names)
               (list->enum-set etype
                               (map (lambda (s)
                                      (enum-name->enum etype s))
                                    'names)))))))))
  )
)

