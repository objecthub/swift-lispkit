;;; Coroutines
;;;
;;; This is a simple example explaining how to implement coroutines in Scheme using
;;; call/cc. The code is based on Matt Might's blog post on "Continuations by example:
;;; Exceptions, time-traveling search, generators, threads, and coroutines" at his
;;; site http://matt.might.net/. The article can be found here: https://goo.gl/pyCHCu .
;;;
;;; The code below introduces the following API for implementing simple coroutine-based
;;; applications:
;;;
;;;   - (spawn thunk) puts a thread for thunk into the thread queue.
;;;   - (quit) kills the current thread and removes it from the thread queue.
;;;   - (yield) hands control from the current thread to another thread.
;;;   - (start-threads) starts executing threads in the thread queue.
;;;   - (halt) exits all threads.
;;;
;;; Author: Matt Might (might@uab.edu)


(import (srfi 31))

; *thread-queue* : list[continuation]
(define *thread-queue* '())

; halt : continuation
(define halt #f)

;; (current-continuation) -> continuation | any value
;; This procedure returns the continuation in which it was evaluated. A conditional
;; pattern can be used to detect whether the continuation was just created, or the
;; continuation has been invoked from some later point:
;; ```
;; (let ((cc (current-continuation)))
;;   (cond ((procedure? cc)    body)
;;         ((future-value? cc) handling-body)
;;         (else               (error "Contract violation!"))))
;; ```
(define (current-continuation)
  (call-with-current-continuation (lambda (cc) (cc cc))))

;; (spawn thunk)
;; Puts a thread for thunk into the thread queue.
(define (spawn thunk)
  (let ((cc (current-continuation)))
    (if (procedure? cc)
        (set! *thread-queue* (append *thread-queue* (list cc)))
        (begin (thunk)
               (quit)))))

;; (yield)
;; Hands control from the current thread to another thread.
(define (yield)
  (let ((cc (current-continuation)))
    (if (and (procedure? cc) (pair? *thread-queue*))
        (let ((next-thread (car *thread-queue*)))
          (set! *thread-queue* (append (cdr *thread-queue*) (list cc)))
          (next-thread 'resume)))))

;; (quit)
;; Kills the current thread and removes it from the thread queue.
(define (quit)
  (if (pair? *thread-queue*)
      (let ((next-thread (car *thread-queue*)))
        (set! *thread-queue* (cdr *thread-queue*))
        (next-thread 'resume))
      (halt)))

;; (start-threads)
;; Starts executing threads in the thread queue. As a side effect, this function
;; initializes `halt`.
(define (start-threads)
  (let ((cc (current-continuation)))
    (if cc
        (begin
          (set! halt (lambda () (cc #f)))
          (if (pair? *thread-queue*)
              (begin
                (let ((next-thread (car *thread-queue*)))
                  (set! *thread-queue* (cdr *thread-queue*))
                  (next-thread 'resume))))))))

;; Example of a simple program using three coroutines

(define counter 10)

(define (make-coroutine name)
  (rec (loop)
    (if (< counter 0)
      (begin (display "quitting thread ")
             (display name)
             (newline)
             (quit)))
    (display "in thread ")
    (display name)
    (display "; counter = ")
    (display counter)
    (newline)
    (set! counter (- counter 1))
    (yield)
    (loop)))

(spawn (make-coroutine 'a))
(spawn (make-coroutine 'b))
(spawn (make-coroutine 'c))

(start-threads)

