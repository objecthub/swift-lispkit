;;; SRFI 134
;;; Immutable Deques
;;;
;;; This SRFI defines immutable deques. A deque is a double-ended queue, a sequence
;;; which allows elements to be added or removed efficiently from either end. A deque
;;; is a generalization of both a queue and a stack, and can be used as either by
;;; disregarding the irrelevant procedures. A data structure is immutable when all its
;;; operations leave the structure unchanged. Note that none of the procedures specified
;;; here ends with an exclamation point.
;;;
;;; Specification:
;;;   Copyright © 2015 John Cowan, Kevin Wortman. All Rights Reserved.
;;;
;;;   Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;;   software and associated documentation files (the "Software"), to deal in the Software
;;;   without restriction, including without limitation the rights to use, copy, modify,
;;;   merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
;;;   permit persons to whom the Software is furnished to do so, subject to the following
;;;   conditions:
;;;
;;;   The above copyright notice and this permission notice shall be included in all copies
;;;   or substantial portions of the Software.
;;;
;;; Implementation:
;;;   Copyright © 2015 Shiro Kawai. All Rights Reserved.
;;;
;;;   Redistribution and use in source and binary forms, with or without modification,
;;;   are permitted provided that the following conditions are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;;; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2017 Matthias Zenger. All rights reserved.

(define-library (srfi 134)

  (export ideque
          ideque-tabulate
          ideque-unfold
          ideque-unfold-right
          ideque?
          ideque-empty?
          ideque=
          ideque-any
          ideque-every
          ideque-front
          ideque-add-front
          ideque-remove-front
          ideque-back
          ideque-add-back
          ideque-remove-back
          ideque-ref
          ideque-take
          ideque-take-right
          ideque-drop
          ideque-drop-right
          ideque-split-at
          ideque-length
          ideque-append
          ideque-reverse
          ideque-count
          ideque-zip
          ideque-map
          ideque-filter-map
          ideque-for-each
          ideque-for-each-right
          ideque-fold
          ideque-fold-right
          ideque-append-map
          ideque-filter
          ideque-remove
          ideque-partition
          ideque-find
          ideque-find-right
          ideque-take-while
          ideque-take-while-right
          ideque-drop-while
          ideque-drop-while-right
          ideque-span
          ideque-break
          list->ideque
          ideque->list
          generator->ideque
          ideque->generator)

  (import (scheme base)
          (srfi 1)
          (srfi 8)
          (srfi 121))

  (begin
    ;; This implements banker's deque as described in Chris Okasaki's "Purely Functional
    ;; Data Structures". It provides amortized O(1) basic operations.
    ;; Originally written for Gauche, and ported to R7RS.

    ;;;
    ;;; Record
    ;;;

    (define-record-type <ideque>
      (%make-dq lenf f lenr r) ideque?
      (lenf dq-lenf)  ; length of front chain
      (f    dq-f)     ; front chain
      (lenr dq-lenr)  ; length of rear chain
      (r    dq-r))    ; rear chain

    ;; We use a singleton for empty deque
    (define *empty* (%make-dq 0 '() 0 '()))

    ;; Common type checker
    (define (%check-ideque x)
      (unless (ideque? x) (error "ideque expected, but got:" x)))
    
    ;;;
    ;;; Constructors
    ;;;

    (define (ideque . args) (list->ideque args))

    (define (ideque-tabulate size init)
      (let ((lenf (quotient size 2))
            (lenr (quotient (+ size 1) 2)))
        (%make-dq lenf (list-tabulate lenf init)
                  lenr (unfold (lambda (n) (= n lenr))
                               (lambda (n) (init (- size n 1)))
                               (lambda (n) (+ n 1))
                               0))))

    (define (ideque-unfold p f g seed)
      (list->ideque (unfold p f g seed)))

    (define (ideque-unfold-right p f g seed)
      (list->ideque (unfold-right p f g seed)))
    ;; alternatively:
    ;; (ideque-reverse (list->ideque (unfold p f g seed)))

    ;; Internal constructor.  Returns a new ideque, with balancing 'front' and
    ;; 'rear' chains.  (The name 'check' comes from Okasaki's book.)

    (define C 3)

    (define (check lenf f lenr r)
      (cond ((> lenf (+ (* lenr C) 1))
             (let* ((i (quotient (+ lenf lenr) 2))
                    (j (- (+ lenf lenr) i))
                    (f. (take f i))
                    (r. (append r (reverse (drop f i)))))
               (%make-dq i f. j r.)))
            ((> lenr (+ (* lenf C) 1))
             (let* ((j (quotient (+ lenf lenr) 2))
                    (i (- (+ lenf lenr) j))
                    (r. (take r j))
                    (f. (append f (reverse (drop r j)))))
               (%make-dq i f. j r.)))
            (else (%make-dq lenf f lenr r))))

    ;;;
    ;;; Basic operations
    ;;;

    (define (ideque-empty? dq)
      (%check-ideque dq)
      (and (zero? (dq-lenf dq)) (zero? (dq-lenr dq))))

    (define (ideque-add-front dq x)
      (%check-ideque dq)
      (check (+ (dq-lenf dq) 1) (cons x (dq-f dq)) (dq-lenr dq) (dq-r dq)))

    (define (ideque-front dq)
      (%check-ideque dq)
      (if (zero? (dq-lenf dq))
        (if (zero? (dq-lenr dq))
          (error "Empty deque:" dq)
          (car (dq-r dq)))
        (car (dq-f dq))))

    (define (ideque-remove-front dq)
      (%check-ideque dq)
      (if (zero? (dq-lenf dq))
        (if (zero? (dq-lenr dq))
          (error "Empty deque:" dq)
          *empty*)
        (check (- (dq-lenf dq) 1) (cdr (dq-f dq)) (dq-lenr dq) (dq-r dq))))

    (define (ideque-add-back dq x)
      (%check-ideque dq)
      (check (dq-lenf dq) (dq-f dq) (+ (dq-lenr dq) 1) (cons x (dq-r dq))))

    (define (ideque-back dq)
      (%check-ideque dq)
      (if (zero? (dq-lenr dq))
        (if (zero? (dq-lenf dq))
          (error "Empty deque:" dq)
          (car (dq-f dq)))
        (car (dq-r dq))))

    (define (ideque-remove-back dq)
      (%check-ideque dq)
      (if (zero? (dq-lenr dq))
        (if (zero? (dq-lenf dq))
          (error "Empty deque:" dq)
          *empty*)
        (check (dq-lenf dq) (dq-f dq) (- (dq-lenr dq) 1) (cdr (dq-r dq)))))

    (define (ideque-reverse dq)
      (%check-ideque dq)
      (if (ideque-empty? dq)
        *empty*
        (%make-dq (dq-lenr dq) (dq-r dq) (dq-lenf dq) (dq-f dq))))

    ;;
    ;; Other operations
    ;;

    (define ideque=
      (case-lambda
        ((elt=) #t)
        ((elt= ideque) (%check-ideque ideque) #t)
        ((elt= dq1 dq2)
         ;; we optimize two-arg case
         (%check-ideque dq1)
         (%check-ideque dq2)
         (or (eq? dq1 dq2)
             (let ((len1 (+ (dq-lenf dq1) (dq-lenr dq1)))
                   (len2 (+ (dq-lenf dq2) (dq-lenr dq2))))
               (and (= len1 len2)
                    (receive (x t1 t2) (list-prefix= elt= (dq-f dq1) (dq-f dq2))
                      (and x
                           (receive (y r1 r2) (list-prefix= elt= (dq-r dq1) (dq-r dq2))
                             (and y
                                  (if (null? t1)
                                    (list= elt= t2 (reverse r1))
                                    (list= elt= t1 (reverse r2)))))))))))
        ((elt= . dqs)
         ;; The comparison scheme is the same as srfi-1's list=.
         (apply list= elt= (map ideque->list dqs)))))

    ;; Compare two lists up to whichever shorter one.
    ;; Returns the compare result and the tails of uncompared lists.
    (define (list-prefix= elt= a b)
      (let loop ((a a) (b b))
        (cond ((or (null? a) (null? b)) (values #t a b))
              ((elt= (car a) (car b)) (loop (cdr a) (cdr b)))
              (else (values #f a b)))))

    (define (ideque-ref dq n)
      (%check-ideque dq)
      (let ((len (+ (dq-lenf dq) (dq-lenr dq))))
        (cond ((or (< n 0) (>= n len)) (error "Index out of range:" n))
              ((< n (dq-lenf dq)) (list-ref (dq-f dq) n))
              (else (list-ref (dq-r dq) (- len n 1))))))

    (define (%ideque-take dq n)             ; n is within the range
      (let ((lenf (dq-lenf dq))
            (f    (dq-f dq)))
        (if (<= n lenf)
          (check n (take f n) 0 '())
          (let ((lenr. (- n lenf)))
            (check lenf f lenr. (take-right (dq-r dq) lenr.))))))

    (define (%ideque-drop dq n)             ; n is within the range
      (let ((lenf (dq-lenf dq))
            (f    (dq-f dq))
            (lenr (dq-lenr dq))
            (r    (dq-r dq)))
        (if (<= n lenf)
          (check n (drop f n) lenr r)
          (let ((lenr. (- lenr (- n lenf))))
            (check 0 '() lenr. (take r lenr.))))))

    (define (%check-length dq n)
      (unless (<= 0 n (- (ideque-length dq) 1))
        (error "argument is out of range:" n)))

    (define (ideque-take dq n)
      (%check-ideque dq)
      (%check-length dq n)
      (%ideque-take dq n))

    (define (ideque-take-right dq n)
      (%check-ideque dq)
      (%check-length dq n)
      (%ideque-drop dq (- (ideque-length dq) n)))

    (define (ideque-drop dq n)
      (%check-ideque dq)
      (%check-length dq n)
      (%ideque-drop dq n))

    (define (ideque-drop-right dq n)
      (%check-ideque dq)
      (%check-length dq n)
      (%ideque-take dq (- (ideque-length dq) n)))

    (define (ideque-split-at dq n)
      (%check-ideque dq)
      (%check-length dq n)
      (values (%ideque-take dq n)
              (%ideque-drop dq n)))

    (define (ideque-length dq)
      (%check-ideque dq)
      (+ (dq-lenf dq) (dq-lenr dq)))

    (define (ideque-append . dqs)
      ;; We could save some list copying by carefully split dqs into front and
      ;; rear groups and append separately, but for now we don't bother...
      (list->ideque (concatenate (map ideque->list dqs))))

    (define (ideque-count pred dq)
      (%check-ideque dq)
      (+ (count pred (dq-f dq)) (count pred (dq-r dq))))

    (define (ideque-zip dq . dqs)
      ;; An easy way.
      (let ((elts (apply zip (ideque->list dq) (map ideque->list dqs))))
        (check (length elts) elts 0 '())))

    (define (ideque-map proc dq)
      (%check-ideque dq)
      (%make-dq (dq-lenf dq) (map proc (dq-f dq))
                (dq-lenr dq) (map proc (dq-r dq))))

    (define (ideque-filter-map proc dq)
      (%check-ideque dq)
      (let ((f (filter-map proc (dq-f dq)))
            (r (filter-map proc (dq-r dq))))
        (check (length f) f (length r) r)))

    (define (ideque-for-each proc dq)
      (%check-ideque dq)
      (for-each proc (dq-f dq))
      (for-each proc (reverse (dq-r dq))))

    (define (ideque-for-each-right proc dq)
      (%check-ideque dq)
      (for-each proc (dq-r dq))
      (for-each proc (reverse (dq-f dq))))

    (define (ideque-fold proc knil dq)
      (%check-ideque dq)
      (fold proc (fold proc knil (dq-f dq)) (reverse (dq-r dq))))

    (define (ideque-fold-right proc knil dq)
      (%check-ideque dq)
      (fold-right proc (fold-right proc knil (reverse (dq-r dq))) (dq-f dq)))

    (define (ideque-append-map proc dq)
      ;; can be cleverer, but for now...
      (list->ideque (append-map proc (ideque->list dq))))

    (define (%ideque-filter-remove op pred dq)
      (%check-ideque dq)
      (let ((f (op pred (dq-f dq)))
            (r (op pred (dq-r dq))))
        (check (length f) f (length r) r)))

    (define (ideque-filter pred dq) (%ideque-filter-remove filter pred dq))
    (define (ideque-remove pred dq) (%ideque-filter-remove remove pred dq))

    (define (ideque-partition pred dq)
      (%check-ideque dq)
      (receive (f1 f2) (partition pred (dq-f dq))
        (receive (r1 r2) (partition pred (dq-r dq))
          (values (check (length f1) f1 (length r1) r1)
                  (check (length f2) f2 (length r2) r2)))))

    (define *not-found* (cons #f #f)) ; unique value

    (define (%search pred seq1 seq2 failure)
      ;; We could write seek as CPS, but we employ *not-found* instead to avoid
      ;; closure allocation.
      (define (seek pred s)
        (cond ((null? s) *not-found*)
              ((pred (car s)) (car s))
              (else (seek pred (cdr s)))))
      (let ((r (seek pred seq1)))
        (if (not (eq? r *not-found*))
          r
          (let ((r (seek pred (reverse seq2))))
            (if (not (eq? r *not-found*))
              r
              (failure))))))

    (define (ideque-find pred dq . opts)
      (%check-ideque dq)
      (let ((failure (if (pair? opts) (car opts) (lambda () #f))))
        (%search pred (dq-f dq) (dq-r dq) failure)))

    (define (ideque-find-right pred dq . opts)
      (%check-ideque dq)
      (let ((failure (if (pair? opts) (car opts) (lambda () #f))))
        (%search pred (dq-r dq) (dq-f dq) failure)))

    (define (ideque-take-while pred dq)
      (%check-ideque dq)
      (receive (hd tl) (span pred (dq-f dq))
        (if (null? tl)
          (receive (hd. tl.) (span pred (reverse (dq-r dq)))
            (check (dq-lenf dq) (dq-f dq) (length hd.) (reverse hd.)))
          (check (length hd) hd 0 '()))))

    (define (ideque-take-while-right pred dq)
      (%check-ideque dq)
      (ideque-reverse (ideque-take-while pred (ideque-reverse dq))))

    (define (ideque-drop-while pred dq)
      (%check-ideque dq)
      (receive (hd tl) (span pred (dq-f dq))
        (if (null? tl)
          (receive (hd. tl.) (span pred (reverse (dq-r dq)))
            (check (length tl.) tl. 0 '()))
          (check (length tl) tl (dq-lenr dq) (dq-r dq)))))

    (define (ideque-drop-while-right pred dq)
      (%check-ideque dq)
      (ideque-reverse (ideque-drop-while pred (ideque-reverse dq))))

    (define (%idq-span-break op pred dq)
      (%check-ideque dq)
      (receive (head tail) (op pred (dq-f dq))
        (if (null? tail)
          (receive (head. tail.) (op pred (reverse (dq-r dq)))
            (values (check (length head) head (length head.) (reverse head.))
                    (check (length tail.) tail. 0 '())))
          (values (check (length head) head 0 '())
                  (check (length tail) tail (dq-lenr dq) (dq-r dq))))))

    (define (ideque-span pred dq)
      (%idq-span-break span pred dq))

    (define (ideque-break pred dq)
      (%idq-span-break break pred dq))

    (define (ideque-any pred dq)
      (%check-ideque dq)
      (if (null? (dq-r dq))
        (any pred (dq-f dq))
        (or (any pred (dq-f dq)) (any pred (reverse (dq-r dq))))))

    (define (ideque-every pred dq)
      (%check-ideque dq)
      (if (null? (dq-r dq))
        (every pred (dq-f dq))
        (and (every pred (dq-f dq)) (every pred (reverse (dq-r dq))))))

    (define (ideque->list dq)
      (%check-ideque dq)
      (append (dq-f dq) (reverse (dq-r dq))))

    (define (list->ideque lis)
      (check (length lis) lis 0 '()))

    (define (ideque->generator dq)
      (%check-ideque dq)
      (lambda ()
        (if (ideque-empty? dq)
          (eof-object)
          (let ((v (ideque-front dq)))
            (set! dq (ideque-remove-front dq))
            v))))

    (define (generator->ideque gen)
      (list->ideque (generator->list gen)))))
