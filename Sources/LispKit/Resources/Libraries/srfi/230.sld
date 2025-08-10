;;; SRFI 230
;;; Atomic Operations
;;;
;;; This SRFI defines atomic operations for the Scheme programming language.
;;; An atomic operation is an operation that, even in the presence of
;;; multiple threads, is either executed completely or not at all. Atomic
;;; operations can be used to implement mutexes and other synchronization
;;; primitives, and they can be used to make concurrent algorithms lock-free.
;;; For this, this SRFI defines two data types, atomic flags and atomic
;;; (fixnum) boxes, whose contents can be queried and mutated atomically.
;;; Moreover, each atomic operation comes with a memory order that defines
;;; the level of synchronization with other threads.
;;;
;;; Copyright © 2021 Marc Nieper-Wißkirchen. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.
;;;
;;; Adaptation to LispKit
;;;   Copyright © 2023 Matthias Zenger. All rights reserved.

(define-library (srfi 230)
  
  (export memory-order
          memory-order?
          make-atomic-flag
          atomic-flag?
          atomic-flag-test-and-set!
          atomic-flag-clear!
          make-atomic-box
          atomic-box?
          atomic-box-ref
          atomic-box-set!
          atomic-box-swap!
          atomic-box-compare-and-swap!
          make-atomic-fxbox
          atomic-fxbox?
          atomic-fxbox-ref
          atomic-fxbox-set!
          atomic-fxbox-swap!
          atomic-fxbox-compare-and-swap!
          atomic-fxbox+/fetch!
          atomic-fxbox-/fetch!
          atomic-fxbox-and/fetch!
          atomic-fxbox-ior/fetch!
          atomic-fxbox-xor/fetch!
          make-atomic-pair
          atomic-pair?
          atomic-pair-ref
          atomic-pair-set!
          atomic-pair-swap!
          atomic-pair-compare-and-swap!
          atomic-fence)
  
  (import (except (lispkit base)
            atomic-box? make-atomic-box atomic-box-ref atomic-box-set!
            atomic-box-swap! atomic-box-compare-and-set! atomic-box-compare-and-swap!
            atomic-box-inc+mul!)
          (lispkit thread))
  
  (begin
    
    ;; Internals
    
    (define lock (make-mutex))
    
    (define-syntax lock-guard
      (syntax-rules ()
        ((lock-guard . body)
          (dynamic-wind
            (lambda ()
              (guard
                (c ((abandoned-mutex-exception? c) #f))
                (mutex-lock! lock)))
            (lambda () . body)
            (lambda () (mutex-unlock! lock))))))
    
    ;; Memory orders
    
    (define-syntax memory-order
      (syntax-rules ()
        ((memory-order symbol) 'symbol)))
    
    (define (memory-order? obj)
      (and (memq obj '(relaxed
                       acquire
                       release
                       acquire-release
                       sequentially-consistent)) #t))
    
    ;; Atomic flags
    
    (define-record-type atomic-flag
      (%make-atomic-flag content)
      atomic-flag?
      (content atomic-flag-content atomic-flag-set-content!))
    
    (define (make-atomic-flag)
      (%make-atomic-flag #f))
    
    (define (atomic-flag-test-and-set! flag . o)
      (lock-guard
        (let ((prev (atomic-flag-content flag)))
          (atomic-flag-set-content! flag #t)
          prev)))
    
    (define (atomic-flag-clear! flag . o)
      (lock-guard
        (atomic-flag-set-content! flag #f)))
    
    ;; Atomic boxes
    
    (define-record-type atomic-box
      (make-atomic-box content)
      atomic-box?
      (content atomic-box-content atomic-box-set-content!))
    
    (define (atomic-box-ref box . o)
      (lock-guard
        (atomic-box-content box)))
    
    (define (atomic-box-set! box obj . o)
      (lock-guard
        (atomic-box-set-content! box obj)))
    
    (define (atomic-box-swap! box obj . o)
      (lock-guard
        (let ((prev (atomic-box-content box)))
          (atomic-box-set-content! box obj)
          prev)))
    
    (define (atomic-box-compare-and-swap! box expected desired . o)
      (lock-guard
        (let ((actual (atomic-box-content box)))
          (when (eq? expected actual)
            (atomic-box-set-content! box desired))
          actual)))
    
    ;; Atomic fixnum boxes
    
    (define-record-type atomic-fxbox
      (make-atomic-fxbox content)
      atomic-fxbox?
      (content atomic-fxbox-content atomic-fxbox-set-content!))
    
    (define (atomic-fxbox-ref box . o)
      (lock-guard
        (atomic-fxbox-content box)))
    
    (define (atomic-fxbox-set! box obj . o)
      (lock-guard
        (atomic-fxbox-set-content! box obj)))
    
    (define (atomic-fxbox-swap! box obj . o)
      (lock-guard
        (let ((prev (atomic-fxbox-content box)))
          (atomic-fxbox-set-content! box obj)
          prev)))
    
    (define (atomic-fxbox-compare-and-swap! box expected desired . o)
      (lock-guard
        (let ((actual (atomic-fxbox-content box)))
          (when (fx= expected actual)
            (atomic-fxbox-set-content! box desired))
          actual)))
    
    (define (atomic-fxbox+/fetch! box n . o)
      (lock-guard
        (let ((prev (atomic-fxbox-content box)))
          (atomic-fxbox-set-content! box (fx+ n prev))
          prev)))
    
    (define (atomic-fxbox-/fetch! box n . o)
      (lock-guard
        (let ((prev (atomic-fxbox-content box)))
          (atomic-fxbox-set-content! box (fx- n prev))
          prev)))
    
    (define (atomic-fxbox-and/fetch! box n . o)
      (lock-guard
        (let ((prev (atomic-fxbox-content box)))
          (atomic-fxbox-set-content! box (fxand n prev))
          prev)))
    
    (define (atomic-fxbox-ior/fetch! box n . o)
      (lock-guard
        (let ((prev (atomic-fxbox-content box)))
          (atomic-fxbox-set-content! box (fxior n prev))
          prev)))
    
    (define (atomic-fxbox-xor/fetch! box n . o)
      (lock-guard
        (let ((prev (atomic-fxbox-content box)))
          (atomic-fxbox-set-content! box (fxxor n prev))
          prev)))
    
    ;; Atomic pairs
    
    (define-record-type atomic-pair
      (make-atomic-pair car cdr)
      atomic-pair?
      (car atomic-pair-car atomic-pair-set-car!)
      (cdr atomic-pair-cdr atomic-pair-set-cdr!))
    
    (define (atomic-pair-ref pair . o)
      (lock-guard
        (values
          (atomic-pair-car pair)
          (atomic-pair-cdr pair))))
    
    (define (atomic-pair-set! pair car cdr . o)
      (lock-guard
        (atomic-pair-set-car! pair car)
        (atomic-pair-set-cdr! pair cdr)))
    
    (define (atomic-pair-swap! pair car cdr . o)
      (lock-guard
        (let ((prev-car (atomic-pair-car pair))
              (prev-cdr (atomic-pair-cdr pair)))
          (atomic-pair-set-car! pair car)
          (atomic-pair-set-cdr! pair cdr)
          (values prev-car prev-cdr))))
    
    (define (atomic-pair-compare-and-swap! pair
                                           expected-car expected-cdr
                                           desired-car desired-cdr . o)
      (lock-guard
        (let ((actual-car (atomic-pair-car pair))
              (actual-cdr (atomic-pair-cdr pair)))
          (when (and (eq? expected-car actual-car)
                     (eq? expected-cdr actual-cdr))
            (atomic-pair-set-car! pair desired-car)
            (atomic-pair-set-cdr! pair desired-cdr))
          (values actual-car actual-cdr))))
    
    ;; Memory synchronization
    
    (define (atomic-fence . o)
      (lock-guard (if #f #f)))
  )
)
