;;; SRFI 41 Derived
;;; <title>
;;;
;;; <description>
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

(define-library (srfi 41 derived)

  (export define-stream
          list->stream
          port->stream
          stream
          stream->list
          stream-append
          stream-concat
          stream-constant
          stream-drop
          stream-drop-while
          stream-filter
          stream-fold
          stream-for-each
          stream-from
          stream-iterate
          stream-length
          stream-let
          stream-map
          stream-match
          stream-of
          stream-range
          stream-ref
          stream-reverse
          stream-scan
          stream-take
          stream-take-while
          stream-unfold
          stream-zip)

  (import (scheme base)
          (srfi 41 primitive))

  (begin

    (define-syntax define-stream
      (syntax-rules ()
        ((define-stream (name . formal) expr0 expr1 ...)
          (define name (stream-lambda formal expr0 expr1 ...)))))

    (define (list->stream objs)
      (if (not (list? objs))
          (error "list->stream: non-list argument" objs)
          (_list->stream objs)))

    (define _list->stream
      (stream-lambda (objs)
        (if (null? objs)
            stream-null
            (stream-cons (car objs) (_list->stream (cdr objs))))))

    (define (port->stream . port)
      (let ((p (if (null? port) (current-input-port) (car port))))
        (if (not (input-port? p))
            (error "port->stream: non-input-port argument" p)
            (_port->stream p))))

    (define _port->stream
      (stream-lambda (p)
        (let ((c (read-char p)))
          (if (eof-object? c)
              stream-null
              (stream-cons c (_port->stream p))))))

    (define-syntax stream
      (syntax-rules ()
        ((stream) stream-null)
          ((stream x y ...) (stream-cons x (stream y ...)))))

    (define (stream->list . args)
      (let ((n (if (= 1 (length args)) #f (car args)))
            (strm (if (= 1 (length args)) (car args) (cadr args))))
        (cond ((not (stream? strm))
                (error "stream->list: non-stream argument" strm))
              ((and n (not (integer? n)))
                (error "stream->list: non-integer count" n))
              ((and n (negative? n))
                (error "stream->list: negative count" n))
              (else
                (let loop ((n (if n n -1)) (strm strm))
                  (if (or (zero? n) (stream-null? strm))
                      '()
                      (cons (stream-car strm) (loop (- n 1) (stream-cdr strm)))))))))

    (define (assert-all-streams strms)
      (if (pair? strms)
          (if (stream? (car strms))
              (assert-all-streams (cdr strms))
              (error "assert-all-streams: non-stream argument" (car strms)))
          (if (not (null? strms))
              (error "not a proper list" strms))))

    (define (exists proc strms)
      (cond ((null? strms) #f)
            ((pair? strms) (or (proc (car strms)) (exists proc (cdr strms))))
            (else          (error "exists: not a proper list" strms))))

    (define (stream-append . strms)
      (if (null? strms)
          stream-null
          (begin (assert-all-streams strms)
                 (_stream-append strms))))

    (define _stream-append
      (stream-lambda (strms)
        (cond ((null? (cdr strms))
                (car strms))
              ((stream-null? (car strms))
                (_stream-append (cdr strms)))
              (else
                (stream-cons (stream-car (car strms))
                             (_stream-append (cons (stream-cdr (car strms)) (cdr strms))))))))

    (define (stream-concat strms)
      (if (not (stream? strms))
          (error "stream-concat: non-stream argument" strms)
          (_stream-concat strms)))

    (define _stream-concat
      (stream-lambda (strms)
        (cond ((stream-null? strms)
                stream-null)
              ((not (stream? (stream-car strms)))
                (error "stream-concat: non-stream object in input stream" strms))
              ((stream-null? (stream-car strms))
                (_stream-concat (stream-cdr strms)))
              (else
                (stream-cons (stream-car (stream-car strms))
                             (_stream-concat (stream-cons (stream-cdr (stream-car strms))
                                                          (stream-cdr strms))))))))

    (define stream-constant
      (stream-lambda objs
        (cond ((null? objs)
                stream-null)
              ((null? (cdr objs))
                (stream-cons (car objs) (stream-constant (car objs))))
              (else
                (stream-cons (car objs)
                             (apply stream-constant (append (cdr objs) (list (car objs)))))))))

    (define (stream-drop n strm)
      (cond ((not (integer? n))
              (error "stream-drop: non-integer argument" n))
            ((negative? n)
              (error "stream-drop: negative argument" n))
            ((not (stream? strm))
              (error "stream-drop: non-stream argument" strm))
            (else
              (_stream-drop n strm))))

    (define _stream-drop
      (stream-lambda (n strm)
        (if (or (zero? n) (stream-null? strm))
            strm
            (_stream-drop (- n 1) (stream-cdr strm)))))

    (define (stream-drop-while pred? strm)
      (letrec ((_stream-drop-while
                  (stream-lambda (strm)
                    (if (and (stream-pair? strm) (pred? (stream-car strm)))
                        (_stream-drop-while (stream-cdr strm))
                        strm))))
        (cond ((not (procedure? pred?))
                (error "stream-drop-while: non-procedural argument" pred?))
              ((not (stream? strm))
                (error "stream-drop-while: non-stream argument" strm))
              (else
                (_stream-drop-while strm)))))

    (define (stream-filter pred? strm)
      (letrec ((_stream-filter
                  (stream-lambda (strm)
                    (cond ((stream-null? strm)
                            stream-null)
                          ((pred? (stream-car strm))
                            (stream-cons (stream-car strm) (_stream-filter (stream-cdr strm))))
                          (else
                            (_stream-filter (stream-cdr strm)))))))
        (cond ((not (procedure? pred?))
                (error "stream-filter: non-procedural argument" pred?))
              ((not (stream? strm))
                (error "stream-filter: non-stream argument" strm))
              (else
                (_stream-filter strm)))))

    (define (stream-fold proc base strm)
      (cond ((not (procedure? proc))
              (error "stream-fold: non-procedural argument" proc))
            ((not (stream? strm))
              (error "stream-fold: non-stream argument" strm))
            (else
              (let loop ((base base) (strm strm))
                (if (stream-null? strm)
                    base
                    (loop (proc base (stream-car strm)) (stream-cdr strm)))))))

    (define (stream-for-each proc . strms)
      (letrec ((_stream-for-each
                  (lambda (strms)
                    (if (not (exists stream-null? strms))
                        (begin (apply proc (map stream-car strms))
                               (_stream-for-each (map stream-cdr strms)))))))
        (cond ((not (procedure? proc))
                (error "stream-for-each: non-procedural argument" proc))
              ((null? strms)
                (error "stream-for-each: no stream arguments" strms))
              ((exists (lambda (x) (not (stream? x))) strms)
                (error "stream-for-each: non-stream argument" strms))
              (else
                (_stream-for-each strms)))))

    (define (stream-from first . step)
      (let ((delta (if (null? step) 1 (car step))))
        (cond ((not (number? first))
                (error "stream-from: non-numeric starting number" first))
              ((not (number? delta))
                (error "stream-from: non-numeric step size" delta))
              (else
                (_stream-from first delta)))))

    (define _stream-from
      (stream-lambda (first delta)
        (stream-cons first (_stream-from (+ first delta) delta))))

    (define (stream-iterate proc base)
      (letrec ((_stream-iterate
                  (stream-lambda (base) (stream-cons base (_stream-iterate (proc base))))))
        (if (not (procedure? proc))
            (error "stream-iterate: non-procedural argument" proc)
            (_stream-iterate base))))

    (define (stream-length strm)
      (if (not (stream? strm))
          (error "stream-length: non-stream argument")
          (let loop ((len 0) (strm strm))
            (if (stream-null? strm)
                len
                (loop (+ len 1) (stream-cdr strm))))))

    (define-syntax stream-let
      (syntax-rules ()
        ((stream-let tag ((name val) ...) expr1 expr2 ...)
          ((letrec ((tag (stream-lambda (name ...) expr1 expr2 ...))) tag) val ...))))

    (define (stream-map proc . strms)
      (letrec ((_stream-map
                 (stream-lambda (strms)
                   (if (exists stream-null? strms)
                       stream-null
                       (stream-cons (apply proc (map stream-car strms))
                                    (_stream-map (map stream-cdr strms)))))))
        (cond ((not (procedure? proc))
                (error "stream-map: non-procedural argument" proc))
              ((null? strms)
                (error "stream-map: no stream arguments" strms))
              ((exists (lambda (x) (not (stream? x))) strms)
                (error "stream-map: non-stream argument" strms))
              (else
                (_stream-map strms)))))

    (define-syntax stream-match
      (syntax-rules ()
        ((stream-match strm-expr clause ...)
          (let ((strm strm-expr))
            (cond ((not (stream? strm))
                    (error "stream-match: non-stream argument" strm))
                  ((stream-match-test strm clause) => car)
                    ...
                  (else
                    (error "stream-match: pattern failure")))))))

    (define-syntax stream-match-test
      (syntax-rules ()
        ((stream-match-test strm (pattern fender expr))
          (stream-match-pattern strm pattern () (and fender (list expr))))
        ((stream-match-test strm (pattern expr))
          (stream-match-pattern strm pattern () (list expr)))))

    (define-syntax stream-match-pattern
      (syntax-rules (_)
        ((stream-match-pattern strm () (binding ...) body)
          (and (stream-null? strm) (let (binding ...) body)))
        ((stream-match-pattern strm (_ . rest) (binding ...) body)
          (and (stream-pair? strm)
               (let ((strm (stream-cdr strm)))
                 (stream-match-pattern strm rest (binding ...) body))))
        ((stream-match-pattern strm (var . rest) (binding ...) body)
          (and (stream-pair? strm)
               (let ((temp (stream-car strm)) (strm (stream-cdr strm)))
                 (stream-match-pattern strm rest ((var temp) binding ...) body))))
        ((stream-match-pattern strm _ (binding ...) body)
          (let (binding ...) body))
        ((stream-match-pattern strm var (binding ...) body) 
          (let ((var strm) binding ...) body))))

    (define-syntax stream-of
      (syntax-rules ()
        ((_ expr rest ...)
          (stream-of-aux expr stream-null rest ...))))

    (define-syntax stream-of-aux
      (syntax-rules (in is)
        ((stream-of-aux expr base)
          (stream-cons expr base))
        ((stream-of-aux expr base (var in stream) rest ...)
          (stream-let loop ((strm stream))
            (if (stream-null? strm)
                base
                (let ((var (stream-car strm)))
                  (stream-of-aux expr (loop (stream-cdr strm)) rest ...)))))
        ((stream-of-aux expr base (var is exp) rest ...)
          (let ((var exp)) (stream-of-aux expr base rest ...)))
        ((stream-of-aux expr base pred? rest ...)
          (if pred? (stream-of-aux expr base rest ...) base))))

    (define (stream-range first past . step)
      (cond ((not (number? first))
              (error "stream-range: non-numeric starting number" first))
            ((not (number? past))
              (error "stream-range: non-numeric ending number" past))
            (else
              (let ((delta (cond ((pair? step) (car step)) ((< first past) 1) (else -1))))
                (if (not (number? delta))
                    (error "stream-range: non-numeric step size" delta)
                    (let ((lt? (if (< 0 delta) < >)))
                      (_stream-range first past delta lt?)))))))

    (define _stream-range
      (stream-lambda (first past delta lt?)
        (if (lt? first past)
            (stream-cons first (_stream-range (+ first delta) past delta lt?))
            stream-null)))

    (define (stream-ref strm n)
      (cond ((not (stream? strm))
              (error "stream-ref: non-stream argument" strm))
            ((not (integer? n))
              (error "stream-ref: non-integer argument" n))
            ((negative? n)
              (error "stream-ref: negative argument" n))
            (else
              (let loop ((strm strm) (n n))
                (cond ((stream-null? strm)
                        (error "stream-ref: beyond end of stream" strm))
                      ((zero? n)
                        (stream-car strm))
                      (else
                        (loop (stream-cdr strm) (- n 1))))))))

    (define (stream-reverse strm)
      (if (not (stream? strm))
          (error "stream-reverse: non-stream argument" strm)
          (_stream-reverse strm stream-null)))

    (define _stream-reverse
      (stream-lambda (strm rev)
        (if (stream-null? strm)
            rev
            (_stream-reverse (stream-cdr strm) (stream-cons (stream-car strm) rev)))))

    (define (stream-scan proc base strm)
      (letrec ((_stream-scan
                 (stream-lambda (base strm)
                   (if (stream-null? strm)
                       (stream base)
                       (stream-cons base (_stream-scan (proc base (stream-car strm))
                                                       (stream-cdr strm)))))))
        (cond ((not (procedure? proc))
                (error "stream-scan: non-procedural argument" proc))
              ((not (stream? strm))
                (error "stream-scan: non-stream argument" strm))
              (else
                (_stream-scan base strm)))))

    (define (stream-take n strm)
      (cond ((not (stream? strm))
              (error "stream-take: non-stream argument" strm))
            ((not (integer? n))
              (error "stream-take: non-integer argument" n))
            ((negative? n)
              (error "stream-take: negative argument" n))
            (else
              (_stream-take n strm))))

    (define _stream-take
      (stream-lambda (n strm)
        (if (or (stream-null? strm) (zero? n))
            stream-null
            (stream-cons (stream-car strm) (_stream-take (- n 1) (stream-cdr strm))))))

    (define (stream-take-while pred? strm)
      (letrec ((_stream-take-while
                  (stream-lambda (strm)
                    (cond ((stream-null? strm)
                            stream-null)
                          ((pred? (stream-car strm))
                            (stream-cons (stream-car strm) (_stream-take-while (stream-cdr strm))))
                          (else
                            stream-null)))))
        (cond ((not (stream? strm))
                (error "stream-take-while: non-stream argument" strm))
              ((not (procedure? pred?))
                (error "stream-take-while: non-procedural argument" pred?))
              (else
                (_stream-take-while strm)))))

    (define (stream-unfold mapper pred? generator base)
      (letrec ((_stream-unfold
                  (stream-lambda (base)
                    (if (pred? base)
                        (stream-cons (mapper base) (_stream-unfold (generator base)))
                        stream-null))))
        (cond ((not (procedure? mapper))
                (error "stream-unfold: non-procedural mapper" mapper))
              ((not (procedure? pred?))
                (error "stream-unfold: non-procedural pred?" pred?))
              ((not (procedure? generator))
                (error "stream-unfold: non-procedural generator" generator))
              (else
                (_stream-unfold base)))))

    (define (stream-zip . strms)
      (if (null? strms)
          (error "stream-zip: no stream arguments" strms)
          (begin (assert-all-streams strms)
                 (_stream-zip strms))))

    (define _stream-zip
      (stream-lambda (strms)
        (if (exists stream-null? strms)
            stream-null
            (stream-cons (map stream-car strms) (_stream-zip (map stream-cdr strms))))))))
