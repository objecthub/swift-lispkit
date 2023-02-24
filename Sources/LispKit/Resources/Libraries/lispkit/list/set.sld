;;; LISPKIT LIST SET
;;;
;;; Simple implementation of list-based sets.
;;; 
;;; Portions of this code: SRFI 1 reference implementation
;;;   Copyright © 1998, 1999 by Olin Shivers.
;;;   You may do as you please with this code as long as you do not remove this copyright
;;;   notice or hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2023 Matthias Zenger. All rights reserved.
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

(define-library (lispkit list set)

  (export lset
          list->lset
          lset<=?
          lset=?
          lset-contains?
          lset-adjoin
          lset-union
          lset-intersection
          lset-difference
          lset-xor
          lset-diff+intersection)
  
  (import (lispkit base))
  
  (begin
    
    (define (list->lset = lst)
      (cond ((eq? = eq?)
              (list->lset/eq lst values))
            ((eq? = eqv?)
              (list->lset/eqv lst values))
            (else
              (let rec ((lst lst)
                        (cont values))
                (if (null? lst)
                    (cont '())
                    (let* ((x (car lst)))
                      (rec (delete x (cdr lst) =) (lambda (y) (cont (cons x y))))))))))
    
    (define (list->lset/eq lst cont)
      (if (null? lst)
          (cont '())
          (let* ((x (car lst)))
            (list->lset/eq (delq x (cdr lst)) (lambda (y) (cont (cons x y)))))))
    
    (define (list->lset/eqv lst cont)
      (if (null? lst)
          (cont '())
          (let* ((x (car lst)))
            (list->lset/eqv (delv x (cdr lst)) (lambda (y) (cont (cons x y)))))))
    
    (define (lset = . elements)
      (list->lset = elements))
    
    (define (lset<=? = . lists)
      (or (null? lists)
          (let rec ((head (car lists))
                    (rest (cdr lists)))
            (or (null? rest)
                (let ((next (car rest)))
                  (and (or (eq? head next) (every? (lambda (x) (member x next =)) head))
                       (rec next (cdr rest))))))))
    
    (define (lset=? = . lists)
      (or (null? lists)
          (let rec ((head (car lists))
                    (rest (cdr lists)))
            (or (null? rest)
                (let ((next (car rest)))
                  (and (or (eq? head next)
                           (and (every? (lambda (x) (member x next =)) head)
                                (every? (lambda (x) (member x head =)) next))
                           (rec next (cdr rest)))))))))
    
    (define (lset-contains? = lst e)
      (cond ((eq? = eq?)
              (and (memq e lst) #t))
            ((eq? = eqv?)
              (and (memv e lst) #t))
            (else
              (and (member e lst =) #t))))
    
    (define (lset-adjoin = list . elts)
      (cond ((eq? = eq?)
              (lset-adjoin/eq list elts))
            ((eq? = eqv?)
              (lset-adjoin/eqv list elts))
            (else
              (let rec ((list list)
                        (elts elts))
                (if (null? elts)
                    list
                    (if (member (car elts) list =)
                        (rec list (cdr elts))
                        (rec (cons (car elts) list) (cdr elts))))))))
    
    (define (lset-adjoin/eq list elts)
      (if (null? elts)
          list
          (if (memq (car elts) list)
              (lset-adjoin/eq list (cdr elts))
              (lset-adjoin/eq (cons (car elts) list) (cdr elts)))))
    
    (define (lset-adjoin/eqv list elts)
      (if (null? elts)
          list
          (if (memv (car elts) list)
              (lset-adjoin/eqv list (cdr elts))
              (lset-adjoin/eqv (cons (car elts) list) (cdr elts)))))

    (define (lset-union = . lists)
      (if (null? lists)
          lists
          (let rec ((head (car lists))
                    (rest (cdr lists)))
            (if (null? rest)
                head
                (let ((next (car rest)))
                  (if (eq? head next)
                      (rec head (cdr rest))
                      (rec (apply lset-adjoin = head next) (cdr rest))))))))

    (define (lset-intersection = . lists)
      (if (null? lists)
          lists
          (cond ((eq? = eq?)
                  (lset-intersection/eq (car lists) (cdr lists)))
                ((eq? = eqv?)
                  (lset-intersection/eqv (car lists) (cdr lists)))
                (else
                  (let rec ((head (car lists))
                            (rest (cdr lists)))
                    (if (null? rest)
                        head
                        (let ((next (car rest)))
                          (if (eq? head next)
                              (rec head (cdr rest))
                              (rec (filter (lambda (x) (member x next =)) head)
                                   (cdr rest))))))))))
    
    (define (lset-intersection/eq head rest)
      (if (null? rest)
          head
          (let ((next (car rest)))
            (if (eq? head next)
                (lset-intersection/eq head (cdr rest))
                (lset-intersection/eq (filter (lambda (x) (memq x next)) head) (cdr rest))))))
    
    (define (lset-intersection/eqv head rest)
      (if (null? rest)
          head
          (let ((next (car rest)))
            (if (eq? head next)
                (lset-intersection/eqv head (cdr rest))
                (lset-intersection/eqv (filter (lambda (x) (memv x next)) head) (cdr rest))))))
    
    (define (lset-difference = list . lists)
      (let rec ((head list)
                (rest lists))
        (if (null? rest)
            head
            (let ((next (car rest)))
              (if (eq? head next)
                  '()
                  (rec (remove (lambda (x) (member x next =)) head) (cdr rest)))))))
    
    (define (lset-xor = . lists)
      (if (null? lists)
          lists
          (let rec ((head (car lists))
                    (rest (cdr lists)))
            (if (null? rest)
                head
                (let ((next (car rest)))
                  (if (eq? head next)
                      '()
                      (rec (append (remove (lambda (x) (member x next =)) head)
                                   (remove (lambda (x) (member x head =)) next))
                           (cdr rest))))))))
    
    (define (lset-diff+intersection = list . lists)
      (values (apply lset-difference = list lists)
              (lset-intersection = list (apply lset-union lists))))
  )
)
