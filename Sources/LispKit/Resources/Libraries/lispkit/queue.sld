;;; LISPKIT QUEUE
;;;
;;; Simple implementation of mutable queues.
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

(define-library (lispkit queue)

  (export make-queue
          queue
          queue?
          queue-empty?
          queue-size
          queue=?
          enqueue!
          dequeue!
          queue-front
          queue-clear!
          queue-copy
          queue->list
          list->queue
          list->queue!)

  (import (lispkit base))

  (begin

    (define-values (new-queue queue? queue-ref make-queue-subtype) (make-type 'queue))

    (define (queue . elements)
      (new-queue (mcons (length elements) (list elements))))

    (define (make-queue)
      (new-queue (mcons 0 (list '()))))

    (define (queue-normalize! q)
      (let* ((lsts (mcdr (queue-ref q))))
        (if (pair? (cdr lsts))
            (set-mcdr! (queue-ref q) (list (append (car lsts) (reverse (cdr lsts))))))))

    (define (queue-empty? q)
      (fxzero? (mcar (queue-ref q))))

    (define (queue-size q)
      (mcar (queue-ref q)))

    (define (queue=? q1 q2)
      (and (eqv? (mcar (queue-ref q1)) (mcar (queue-ref q2)))
           (eqv? (queue->list q1) (queue->list q2))))

    (define (enqueue! q x)
      (let* ((ref  (queue-ref q))
             (lsts (mcdr ref)))
        (set-mcar! ref (fx1+ (mcar ref)))
        (set-mcdr! ref (cons (car lsts) (cons x (cdr lsts))))))

    (define (dequeue! q)
      (let* ((ref  (queue-ref q))
             (lsts (mcdr ref)))
        (if (fxzero? (mcar ref))
            (error "cannot dequeue element from empty queue $0" q)
            (if (null? (car lsts))
                (let ((elems (reverse (cdr lsts))))
                  (set-mcdr! ref (list (cdr elems)))
                  (set-mcar! ref (fx1- (mcar ref)))
                  (car elems))
                (begin
                  (set-mcdr! ref (cons (cdar lsts) (cdr lsts)))
                  (set-mcar! ref (fx1- (mcar ref)))
                  (caar lsts))))))

    (define (queue-front q)
      (let* ((ref  (queue-ref q))
             (lsts (mcdr ref)))
        (if (zero? (mcar ref))
            (error "cannot return front element from empty queue $0" q)
            (if (null? (car lsts))
                (let ((elems (reverse (cdr lsts))))
                  (set-mcdr! ref (list elems))
                  (car elems))
                (caar lsts)))))

    (define (queue-clear! q)
      (set-mcar! (queue-ref q) 0)
      (set-mcdr! (queue-ref q) (list '())))

    (define (queue-copy q)
      (new-queue (mcons (mcar (queue-ref q)) (mcdr (queue-ref q)))))

    (define (queue->list q)
      (queue-normalize! q)
      (car (mcdr (queue-ref q))))

    (define (list->queue elements)
      (new-queue (mcons (length elements) (list elements))))

    (define (list->queue! q elements)
      (if (pair? elements)
          (if (pair? (cdr elements))
              (let ((ref (queue-ref q))
                    (num (length elements)))
                (queue-normalize! q)
                (set-mcdr! ref (list (append (car (mcdr ref)) elements)))
                (set-mcar! ref (fx+ (mcar ref) num)))
              (enqueue! q (car elements)))))
  )
)
