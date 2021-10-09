;;; PAIP Utilities
;;; 
;;; This library provides some utilities for porting the Common Lisp code from
;;; "Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp".
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2021 Matthias Zenger. All rights reserved.
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

(define-library (paip util)
  
  (export not-number?
          ^
          take
          position
          sublis
          subst
          make-proc-map)
  
  (import (lispkit base))
  
  (begin
    
    (define (not-number? x)
      (not (number? x)))
    
    (define (^ b n)
      (expt b n))
      ;  (cond ((zero? n)     1)
      ;        ((negative? n) (/ 1 (^ b (- n))))
      ;        ((even? n)     (square (^ b (/ n 2))))
      ;        (else          (* b (^ b (- n 1))))))
    
    (define (take x i)
      (if (zero? i) '() (cons (car x) (take (cdr x) (- i 1)))))
    
    (define (position item lst start)
      (do ((clist (list-tail lst start) (cdr clist))
           (n start (+ n 1)))
        ((or (not (pair? clist)) (equal? (car clist) item))
         (and (pair? clist) n))))
    
    (define (sublis alist tree)
      (if (pair? tree)
          (cons (sublis alist (car tree)) (sublis alist (cdr tree)))
          (if (assv tree alist)
              (cdr (assv tree alist))
              tree)))
    
    (define (subst new old l)
      (cond ((null? l)
              '())
            ((pair? l)
              (cons (subst new old (car l)) (subst new old (cdr l))))
            ((eq? old l)
              new)
            (else
              l)))
    
    (define (make-proc-map)
      (let ((proc-map (make-eq-hashtable)))
        (case-lambda
          ((key)
            (hashtable-ref proc-map key #f))
          ((key proc)
            (hashtable-set! proc-map key proc)))))
  )
)
