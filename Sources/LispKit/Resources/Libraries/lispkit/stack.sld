;;; LISPKIT STACK
;;;
;;; Simple implementation of mutable stacks.
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

(define-library (lispkit stack)

  (export make-stack
          stack
          stack?
          stack-type-tag
          stack-empty?
          stack-size
          stack=?
          stack-push!
          stack-top
          stack-pop!
          stack-clear!
          stack-copy
          stack->list
          list->stack
          list->stack!)

  (import (lispkit base))

  (begin

    (define-values (stack-type-tag new-stack stack? stack-ref make-stack-subtype)
      (make-type 'stack))

    (define (stack . elements)
      (new-stack (mcons (length elements) elements)))

    (define (make-stack)
      (new-stack (mcons 0 '())))

    (define (stack-empty? s)
      (null? (mcdr (stack-ref s))))

    (define (stack-size s)
      (mcar (stack-ref s)))

    (define (stack=? s1 s2)
      (and (eqv? (mcar (stack-ref s1)) (mcar (stack-ref s2)))
           (eqv? (mcdr (stack-ref s1)) (mcdr (stack-ref s2)))))

    (define (stack-push! s x)
      (let ((ref (stack-ref s)))
        (set-mcar! ref (fx1+ (mcar ref)))
        (set-mcdr! ref (cons x (mcdr ref)))))

    (define (stack-top s)
      (car (mcdr (stack-ref s))))

    (define (stack-pop! s)
      (let* ((ref (stack-ref s))
             (lst (mcdr ref)))
        (if (zero? (mcar ref))
          (error "cannot pop value off empty stack $0" s)
          (begin (set-mcdr! ref (cdr lst))
                 (set-mcar! ref (fx1- (mcar ref)))
                 (car lst)))))

    (define (stack-clear! s)
      (set-mcar! (stack-ref s) 0)
      (set-mcdr! (stack-ref s) '()))

    (define (stack-copy s)
      (new-stack (mcons (mcar (stack-ref s)) (mcdr (stack-ref s)))))

    (define (stack->list s)
      (mcdr (stack-ref s)))

    (define (list->stack l)
      (new-stack (mcons (length l) l)))

    (define (list->stack! s xs)
      (let ((ref (stack-ref s)))
        (set-mcar! ref (fx+ (length xs) (mcar ref)))
        (set-mcdr! ref (append xs (mcdr ref)))))
  )
)
