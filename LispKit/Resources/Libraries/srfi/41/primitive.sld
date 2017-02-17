;;; SRFI 41 Primitive
;;; Streams
;;;
;;; Streams, sometimes called lazy lists, are a sequential data structure containing elements
;;; computed only on demand. A stream is either null or is a pair with a stream in its cdr.
;;; Since elements of a stream are computed only when accessed, streams can be infinite.
;;; Once computed, the value of a stream element is cached in case it is needed again.
;;;
;;; Streams without memoization were first described by Peter Landin in 1965. Memoization
;;; became accepted as an essential feature of streams about a decade later. Today, streams
;;; are the signature data type of functional programming languages such as Haskell.
;;;
;;; This Scheme Request for Implementation describes two libraries for operating on streams:
;;; a canonical set of stream primitives and a set of procedures and syntax derived from
;;; those primitives that permits convenient expression of stream operations.
;;;
;;; Copyright © 2007 Philip L. Bewig. All Rights Reserved.
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
;;;   Copyright © 2017 Matthias Zenger. All rights reserved.

(define-library (srfi 41 primitive)

  (export stream?
          stream-car
          stream-cdr
          stream-cons
          stream-lambda
          stream-null
          stream-null?
          stream-pair?)
  
  (import (scheme base))
  
  (begin

    (define _stream-null (box '()))
    (define stream-null (stream-delay _stream-null))

    (define-record-type <stream-pare>
      (make-stream-pare kar kdr)
      stream-pare?
      (kar stream-kar)
      (kdr stream-kdr))

    (define (stream-pair? obj)
      (and (stream? obj) (stream-pare? (force obj))))

    (define (stream-null? obj)
      (and (stream? obj) (eqv? (force obj) _stream-null)))

    (define-syntax stream-cons
      (syntax-rules ()
        ((stream-cons obj strm)
         (stream-eager (make-stream-pare (stream-delay obj) (stream-lazy strm))))))

    (define (stream-car strm)
      (cond ((not (stream? strm))
              (error "stream-car: non-stream" strm))
            ((stream-null? strm)
              (error "stream-car: null stream" strm))
            (else
              (force (stream-kar (force strm))))))

    (define (stream-cdr strm)
      (cond ((not (stream? strm))
              (error "stream-cdr: non-stream" strm))
            ((stream-null? strm)
              (error "stream-cdr: null stream" strm))
            (else
              (stream-kdr (force strm)))))

    (define-syntax stream-lambda
      (syntax-rules ()
        ((stream-lambda formals expr0 expr1 ...)
          (lambda formals (stream-lazy (let () expr0 expr1 ...))))))))
