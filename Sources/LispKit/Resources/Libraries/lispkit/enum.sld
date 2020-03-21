;;; LISPKIT ENUMERATION
;;; 
;;; Implementation of R6RS-style enumerations (symbol sets).
;;; 
;;; Copyright 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>.
;;;
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright and permission notice in full.
;;;  
;;; I also request that you send me a copy of any improvements that you
;;; make to this software so that they may be incorporated within it to
;;; the benefit of the Scheme community.
;;; 
;;; Commentary:
;;;
;;; The R6RS speaks of universes and enumeration types, but doesn't define these
;;; as Scheme types or objects; it only defines the <enum-set> type.
;;;
;;; The closest thing one can get to a universe object is the <enum-set> object
;;; returned by `enum-set-universe'.  Therefore, in our implementation, a
;;; universe is simply an <enum-set> whose universe field contains itself, thus
;;; `enum-set-universe' is just a record field access.
;;;
;;; The closest thing one can get to the enumeration type of an <enum-set> is
;;; the universe of that <enum-set>, although there can be separate enumeration
;;; types with equal universes.  In our implementation, there is one universe
;;; (<enum-set> object) per enumeration type, as created by `make-enumeration',
;;; that will be in the universe field of any <enum-set> of that enumeration
;;; type, so we can use the object identity of that universe (<enum-set> object)
;;; as the enumeration type of an <enum-set>.
;;;
;;; Adaptation to LispKit
;;;   Copyright © 2017 Matthias Zenger. All rights reserved.

(define-library (lispkit enum)
  
  (export define-enumeration
          make-enumeration
          enum-set-universe
          enum-set-indexer
          enum-set-constructor
          enum-set->list
          enum-set-member?
          enum-set-subset?
          enum-set=?
          enum-set-union
          enum-set-intersection
          enum-set-difference
          enum-set-complement
          enum-set-projection)
  
  (import (lispkit base))
  
  (begin
    
    (define (sort-uniq list order)
      (let loop ((order order)
                 (result '()))
        (cond ((null? order)
                (reverse result))
              ((memq (car order) list)
                (loop (cdr order) (cons (car order) result)))
              (else
                (loop (cdr order) result)))))
    
    ;;; Given a list of symbols, sorts it into some standard order.
    ;;; The standard order doesn't matter, so long as the same standard
    ;;; order is used whenever sets of symbols are compared.
    
    (define (sorted-symbols symbols)
      (sort (lambda (sym1 sym2)
              (string<? (symbol->string sym1) (symbol->string sym2)))
            symbols))
    
    ;;; Given two lists that have been sorted with respect to the same
    ;;; total order, returns true if and only if the elements of the
    ;;; first list are a subset of the elements of the second list.
    
    (define (sorted-subset? list1 list2)
      (let loop ((list1 list1)
                 (list2 list2))
        (or (null? list1)
            (let ((list2-rest (memq (car list1) list2)))
              (and list2-rest (loop (cdr list1) (cdr list2-rest)))))))
    
    (define (sorted-intersection list1 list2)
      (let loop ((list1 list1)
                 (list2 list2)
                 (result '()))
        (if (null? list1)
            (reverse result)
            (let ((list2-rest (memq (car list1) list2)))
              (if list2-rest
                  (loop (cdr list1) (cdr list2-rest) (cons (car list1) result))
                  (loop (cdr list1) list2 result))))))
    
    (define (sorted-difference list1 list2)
      (let loop ((list1 list1)
                 (list2 list2)
                 (result '()))
        (if (null? list1)
            (reverse result)
            (let ((list2-rest (memq (car list1) list2)))
              (if list2-rest
                  (loop (cdr list1) (cdr list2-rest) result)
                  (loop (cdr list1) list2 (cons (car list1) result)))))))
    
    (define-record-type <enum-set>
      (make-enum-set universe indexer constructor elements)
      enum-set?
      (universe enum-set-universe set-enum-set-universe!)
      (indexer enum-set-indexer)
      (constructor enum-set-constructor)
      (elements enum-set->list))
    
    (define (make-enumeration symbol-list)
      (assert (every? symbol? symbol-list))
      (letrec* ((indexer
                  (lambda (symbol)
                    (let loop ((i 0) (p symbol-list))
                      (cond ((null? p) #f)
                            ((eq?      (car p) symbol) i)
                            (else      (loop (+ i 1) (cdr p)))))))
                (constructor
                  (lambda (elements)
                    (assert (every? (lambda (e) (memq e symbol-list)) elements))
                    (let ((elements (sort-uniq elements symbol-list)))
                      (make-enum-set universe indexer constructor elements))))
                (universe 
                  (make-enum-set #f indexer constructor symbol-list)))
        (set-enum-set-universe! universe universe)
        universe))
    
    (define (enum-set-member? symbol set)
      (not (not (memq symbol (enum-set->list set)))))
    
    (define (enum-set-subset? set1 set2)
      (let* ((universe1 (enum-set->list (enum-set-universe set1)))
             (universe2 (enum-set->list (enum-set-universe set2)))
             (elements1 (enum-set->list set1))
             (elements2 (enum-set->list set2))
             (universe3 (sorted-symbols universe1))
             (universe4 (sorted-symbols universe2))
             (elements3 (sorted-symbols elements1))
             (elements4 (sorted-symbols elements2)))
        (and (sorted-subset? universe3 universe4)
             (sorted-subset? elements3 elements4))))
    
    (define (enum-set=? set1 set2)
      (let* ((universe1 (enum-set->list (enum-set-universe set1)))
             (universe2 (enum-set->list (enum-set-universe set2)))
             (elements1 (enum-set->list set1))
             (elements2 (enum-set->list set2))
             (universe3 (sorted-symbols universe1))
             (universe4 (sorted-symbols universe2))
             (elements3 (sorted-symbols elements1))
             (elements4 (sorted-symbols elements2)))
        (and (equal? universe3 universe4)
             (equal? elements3 elements4))))
    
    (define (assert-same-type set1 set2)
      (assert (eq? (enum-set-universe set1)
                   (enum-set-universe set2))))
    
    (define (enum-set-union set1 set2)
      (assert-same-type set1 set2)
      ((enum-set-constructor set1) (append (enum-set->list set1) (enum-set->list set2))))
     
    (define (enum-set-intersection set1 set2)
      (assert-same-type set1 set2)
      ((enum-set-constructor set1)
        (sorted-intersection (enum-set->list set1) (enum-set->list set2))))
    
    (define (enum-set-difference set1 set2)
      (assert-same-type set1 set2)
      ((enum-set-constructor set1)
        (sorted-difference (enum-set->list set1) (enum-set->list set2))))
    
    (define (enum-set-complement set)
      ((enum-set-constructor set)
        (sorted-difference (enum-set->list (enum-set-universe set)) (enum-set->list set))))
    
    (define (enum-set-projection set1 set2)
      ((enum-set-constructor set2)
        (let ((set1-elements (enum-set->list set1))
              (set2-universe (enum-set->list (enum-set-universe set2))))
          (filter (lambda (e) (memq e set2-universe)) set1-elements))))
    
    (define-syntax define-enumeration
     (syntax-rules ()
       ((_ <type-name> (<symbol> ...) <constructor-syntax>)
         (begin
           (define universe (make-enumeration '(<symbol> ...)))
           (define constructor (enum-set-constructor universe))
           (define-syntax <type-name>
             (syntax-rules ()
               ((_ <obj>)
                 (let ((obj '<obj>))
                   (assert (symbol? obj))
                   (assert (memq obj '(<symbol> ...)))
                   obj))))
           (define-syntax <constructor-syntax>
             (syntax-rules ___ ()
               ((_) (constructor '()))
               ((_ <element> ___)
                (begin
                  (<type-name> <element>)
                  ___
                  (constructor '(<element> ___))))))))))
  )
)
