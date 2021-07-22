;;; SRFI 216
;;; SICP Prerequisites
;;;
;;; This SRFI provides "out-of-the-box" support for hosting the exercises suggested by
;;; "Structure and Interpretation of Computer Programs". It primarily provides procedures
;;; for working with time data and streams, as well as SICP names for true and false.
;;; Support for multi-threading is omitted due to LispKit currently not supporting threads.
;;; None of the provided procedures are fit for production use. They are only designed for
;;; pedagogical purposes. Students are expected to be able to just write
;;;
;;;   (include (srfi sicp))
;;;
;;; and have the code from the book run without problems (apart from those intended by the
;;; book authors).
;;;
;;; Companion website of SICP: https://mitpress.mit.edu/sites/default/files/sicp/index.html
;;;
;;; Copyright © 2020 Vladimir Nikishkin. All rights reserved.
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
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
;;; PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
;;; FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
;;; OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;
;;; Adaptation to LispKit
;;;   Copyright © 2021 Matthias Zenger. All rights reserved.

(define-library (srfi 216)

  (export runtime
          random
          ; parallel-execute
          ; test-and-set!
          cons-stream
          stream-null?
          the-empty-stream
          true
          false
          nil)

  (import (lispkit base)
          (except (lispkit stream) stream-null?)
          (only (srfi 27) random-integer random-real))
  ; (only (srfi 18) thread-start! make-thread thread-join!
  ;                 make-mutex mutex-lock! mutex-unlock!)

  (begin

    ;;; r4rs booleans

    (define true #t)
    (define false #f) ;; luckily, SICP does not use '() as false

    ;;; Empty list
    (define nil '())

    ;;; Random numbers

    (define (random x) ;; srfi-27
      (if (exact-integer? x)
          (random-integer x)
          (* x (random-real))))

    ;;; Timing

    (define (runtime) ;; r7rs
      (round (* (current-jiffy) (jiffies-per-second) #e1e6))) ;; microseconds

    ;;; Multi-threading

    ; (define (parallel-execute . forms) ;; srfi-18
    ;   (let ((myo (open-output-string)))
    ;     (define (create-threads . forms)
    ;       (if (null? forms)
    ;           (list)
    ;           (let ((ctxi (thread-start!
    ;                         (make-thread
    ;                           (lambda () (parameterize ((current-output-port myo))
    ;                                                    ((car forms))))))))
    ;             (cons ctxi (apply create-threads (cdr forms))))))
    ;     (define (wait-threads thread-list)
    ;       (if (null? thread-list)
    ;           #t
    ;           (begin (thread-join! (car thread-list))
    ;                  (wait-threads (cdr thread-list)))))
    ;     (wait-threads (apply create-threads forms))
    ;     (display (get-output-string myo)))) ;; return value is not specified by SICP
    ;
    ; (define central-old-mutex (make-mutex 'global-srfi-18-mutex)) ;; not exported
    ;
    ; (define (test-and-set! cell) ;; srfi-18
    ;   (mutex-lock! central-old-mutex)
    ;   (let ((output (if (car cell) #t (begin (set-car! cell #t) #f))))
    ;     (mutex-unlock! central-old-mutex)
    ;     output))

    ;;; Streams

    (define-syntax cons-stream ;; r7rs
      (syntax-rules ()
        ((cons-stream a b) (cons a (delay b)))))

    (define stream-null? null?)

    (define the-empty-stream '())
  )
)
