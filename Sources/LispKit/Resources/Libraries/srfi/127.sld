;;; SRFI 127
;;; Lazy Sequences
;;;
;;; Lazy sequences (or lseqs, pronounced "ell-seeks") are a generalization of lists. In
;;; particular, an lseq is either a proper list or a dotted list whose last cdr is a SRFI 121
;;; generator. A generator is a procedure that can be invoked with no arguments in order to
;;; lazily supply additional elements of the lseq. When a generator has no more elements to
;;; return, it returns an end-of-file object. Consequently, lazy sequences cannot reliably
;;; contain end-of-file objects.
;;;
;;; This SRFI provides a set of procedures suitable for operating on lazy sequences based
;;; on SRFI 1.
;;;
;;; Copyright © 2015 John Cowan. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify,
;;; merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to the following
;;; conditions:
;;; The above copyright notice and this permission notice shall be included in all copies
;;; or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;;; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2026 Matthias Zenger. All rights reserved.
;;;
;;; Implementation notes:
;;;   The reference implementation memoizes the elements of an lseq by destructively
;;;   updating the last cdr of a lazy pair. Pairs are immutable in LispKit. Therefore, this
;;;   implementation stores the memoized state in the generator at the end of an lseq. This
;;;   generator is a tagged procedure ("lseq tail") which forwards to the underlying
;;;   generator when called for the first time and remembers the result. As a consequence,
;;;   lseqs created by `generator->lseq` and by the procedures of this library still are
;;;   dotted lists ending in a procedure, `lseq-cdr` is idempotent, and `car`, `cdr`,
;;;   `pair?` and `null?` can be used on them.
;;;   Applying `lseq-cdr` to a pair that was created directly via `(cons x generator)` cannot
;;;   be memoized. Each such application invokes `generator` once more. All lseqs derived
;;;   from the result are memoized again.

(define-library (srfi 127)

  (export generator->lseq
          lseq?
          lseq=?
          lseq-car
          lseq-cdr
          lseq-first
          lseq-rest
          lseq-ref
          lseq-take
          lseq-drop
          lseq-realize
          lseq->generator
          lseq-length
          lseq-append
          lseq-zip
          lseq-map
          lseq-for-each
          lseq-filter
          lseq-remove
          lseq-find
          lseq-find-tail
          lseq-any
          lseq-every
          lseq-index
          lseq-take-while
          lseq-drop-while
          lseq-member
          lseq-memq
          lseq-memv)

  (import (lispkit base)
          (srfi 1))

  (begin

    ;; === Lseq tails ===

    ;; An lseq tail is the object stored in the last cdr of the realized part of an lazily
    ;; generated lseq. It is a procedure, which can be used as a generator for the elements
    ;; following the pair it is attached to. Its tag is a `lseq-tail-state` object, which
    ;; remembers the result of invoking the underlying generator.
    ;;
    ;; The field `state` contains the underlying generator (a procedure) as long as the tail
    ;; has not been forced. Afterwards, it contains the lseq following the pair the tail
    ;; belongs to: either the empty list or a pair whose cdr is a new lseq tail.
    (define-record-type <lseq-tail-state>
      (make-lseq-tail-state state)
      lseq-tail-state?
      (state lseq-tail-state lseq-tail-state-set!))

    ;; Creates a new lseq tail for the given generator. Invoking the lseq tail as a
    ;; procedure returns the element that follows the pair of the tail, or an end-of-file
    ;; object if there is no such element.
    (define (make-lseq-tail generator)
      (let ((tail (make-lseq-tail-state generator)))
        (lambda/tag tail ()
          (let ((next (force-lseq-tail tail)))
            (if (pair? next) (car next) (eof-object))))))

    ;; Returns the lseq that follows the pair the given lseq tail belongs to. The underlying
    ;; generator is invoked only the first time this procedure is called.
    (define (force-lseq-tail tail)
      (let ((state (lseq-tail-state tail)))
        (if (procedure? state)
            (let* ((val (state))
                   (next (if (eof-object? val) '() (cons val (make-lseq-tail state)))))
              (lseq-tail-state-set! tail next)
              next)
            state)))

    ;; Returns #t if `obj` is a memoizing lseq tail created by this library.
    (define (lseq-tail? obj)
      (and (procedure/tag? obj) (lseq-tail-state? (procedure-tag obj))))

    ;; === Constructors ===

    ;; Returns an lseq whose elements are the values generated by `generator`.
    (define (generator->lseq generator)
      (unless (procedure? generator)
        (error "generator->lseq: argument is not a generator:" generator))
      (let ((val (generator)))
        (if (eof-object? val)
            '()
            (cons val (make-lseq-tail generator)))))

    ;; === Predicates ===

    ;; Returns #t if `x` is an lseq.
    (define (lseq? x)
      (let loop ((x x))
        (cond ((null? x) #t)
              ((pair? x) (or (procedure? (cdr x)) (loop (cdr x))))
              (else #f))))

    ;; Determines lseq equality, given an element-equality procedure.
    (define (lseq=? elt=? lseq1 lseq2)
      (let loop ((ls1 lseq1) (ls2 lseq2))
        (cond ((null? ls1) (null? ls2))
              ((null? ls2) #f)
              ((elt=? (lseq-car ls1) (lseq-car ls2))
               (loop (lseq-cdr ls1) (lseq-cdr ls2)))
              (else #f))))

    ;; === Selectors ===

    (define (lseq-car lseq)
      (if (pair? lseq)
          (car lseq)
          (error "lseq-car: argument is not a pair:" lseq)))

    (define (lseq-cdr lseq)
      (if (pair? lseq)
          (let ((rest (cdr lseq)))
            (cond ((not (procedure? rest))
                   rest)
                  ((lseq-tail? rest)
                   (force-lseq-tail (procedure-tag rest)))
                  (else
                   (let ((val (rest)))
                     (if (eof-object? val)
                         '()
                         (cons val (make-lseq-tail rest)))))))
          (error "lseq-cdr: argument is not a pair:" lseq)))

    (define lseq-first lseq-car)
    (define lseq-rest lseq-cdr)

    (define (lseq-ref lseq i)
      (check-index "lseq-ref" i)
      ;; Written as a loop in tail position so that the realized prefix of `lseq` can be
      ;; reclaimed while the loop is running
      (let loop ((lseq lseq) (i i))
        (if (> i 0)
            (loop (lseq-cdr lseq) (- i 1))
            (lseq-car lseq))))

    (define (lseq-take lseq i)
      (check-index "lseq-take" i)
      (generator->lseq
        (let ((current lseq)
              (remaining i)
              (started #f))
          (lambda ()
            (if (zero? remaining)
                (eof-object)
                (begin
                  ;; Only advance once the next element is requested; this avoids realizing
                  ;; more elements of `lseq` than necessary
                  (if started
                      (set! current (lseq-cdr current))
                      (set! started #t))
                  (set! remaining (- remaining 1))
                  (lseq-car current)))))))

    (define (lseq-drop lseq i)
      (check-index "lseq-drop" i)
      (let loop ((lseq lseq) (i i))
        (if (> i 0)
            (loop (lseq-cdr lseq) (- i 1))
            lseq)))

    ;; === The whole lazy sequence ===

    ;; Returns a proper list with all elements of `lseq`.
    (define (lseq-realize lseq)
      (let loop ((lseq lseq) (acc '()))
        (cond ((pair? lseq)
               (loop (lseq-cdr lseq) (cons (car lseq) acc)))
              ((null? lseq)
               (reverse acc))
              (else
               (error "lseq-realize: not an lseq:" lseq)))))

    ;; Returns a generator for all elements of `lseq`. The generator does not realize an
    ;; element of `lseq` until it is requested.
    (define (lseq->generator lseq)
      (let ((current lseq)
            (previous #f))
        (lambda ()
          (when previous
            (set! current (lseq-cdr previous))
            (set! previous #f))
          (cond ((pair? current)
                 (set! previous current)
                 (car current))
                ((null? current)
                 (eof-object))
                (else
                 (error "lseq->generator: not an lseq:" current))))))

    (define (lseq-length lseq)
      (let loop ((lseq lseq) (len 0))
        (cond ((pair? lseq) (loop (lseq-cdr lseq) (+ len 1)))
              ((null? lseq) len)
              (else (error "lseq-length: not an lseq:" lseq)))))

    ;; Returns an lseq that lazily contains all the elements of all the given lseqs.
    (define (lseq-append . lseqs)
      (let ((remaining lseqs)
            (current #f))
        (generator->lseq
          (lambda ()
            (let loop ()
              (cond (current
                     (let ((val (current)))
                       (cond ((eof-object? val)
                              (set! current #f)
                              (loop))
                             (else
                              val))))
                    ((pair? remaining)
                     (set! current (lseq->generator (car remaining)))
                     (set! remaining (cdr remaining))
                     (loop))
                    (else
                     (eof-object))))))))

    ;; Returns an lseq of lists, with each list containing the corresponding elements of the
    ;; given lseqs. The result is as long as the shortest lseq.
    (define (lseq-zip lseq . lseqs)
      (generator->lseq (zip-generator (cons lseq lseqs))))

    ;; Returns a generator producing a list of the corresponding elements of the given lseqs
    ;; until one of the lseqs is exhausted.
    (define (zip-generator lseqs)
      (let ((gens (map lseq->generator lseqs)))
        (lambda ()
          (let loop ((gens gens) (vals '()))
            (if (null? gens)
                (reverse vals)
                (let ((val ((car gens))))
                  (if (eof-object? val)
                      val
                      (loop (cdr gens) (cons val vals)))))))))

    ;; === Mapping and filtering ===

    (define (lseq-map proc lseq . lseqs)
      (if (null? lseqs)
          (let ((gen (lseq->generator lseq)))
            (generator->lseq
              (lambda ()
                (let ((val (gen)))
                  (if (eof-object? val)
                      val
                      (proc val))))))
          (let ((gen (zip-generator (cons lseq lseqs))))
            (generator->lseq
              (lambda ()
                (let ((vals (gen)))
                  (if (eof-object? vals)
                      vals
                      (apply proc vals))))))))

    (define (lseq-for-each proc lseq . lseqs)
      (if (null? lseqs)
          (let loop ((lseq lseq))
            (when (pair? lseq)
              (proc (car lseq))
              (loop (lseq-cdr lseq))))
          (let loop ((lseqs (cons lseq lseqs)))
            (when (every pair? lseqs)
              (apply proc (map car lseqs))
              (loop (map lseq-cdr lseqs))))))

    (define (lseq-filter pred lseq)
      (let ((gen (lseq->generator lseq)))
        (generator->lseq
          (lambda ()
            (let loop ()
              (let ((val (gen)))
                (if (or (eof-object? val) (pred val))
                    val
                    (loop))))))))

    (define (lseq-remove pred lseq)
      (lseq-filter (lambda (x) (not (pred x))) lseq))

    ;; === Searching ===

    (define (lseq-find-tail pred lseq)
      (let loop ((lseq lseq))
        (and (pair? lseq)
             (if (pred (car lseq))
                 lseq
                 (loop (lseq-cdr lseq))))))

    (define (lseq-find pred lseq)
      (let loop ((lseq lseq))
        (and (pair? lseq)
             (let ((val (car lseq)))
               (if (pred val)
                   val
                   (loop (lseq-cdr lseq)))))))

    (define (lseq-take-while pred lseq)
      (let ((gen (lseq->generator lseq)))
        (generator->lseq
          (lambda ()
            (let ((val (gen)))
              (if (and (not (eof-object? val)) (pred val))
                  val
                  (eof-object)))))))

    (define (lseq-drop-while pred lseq)
      (let loop ((lseq lseq))
        (if (and (pair? lseq) (pred (car lseq)))
            (loop (lseq-cdr lseq))
            lseq)))

    (define (lseq-any pred lseq . lseqs)
      (if (null? lseqs)
          (let loop ((lseq lseq))
            (and (pair? lseq)
                 (or (pred (car lseq))
                     (loop (lseq-cdr lseq)))))
          (let loop ((lseqs (cons lseq lseqs)))
            (and (every pair? lseqs)
                 (or (apply pred (map car lseqs))
                     (loop (map lseq-cdr lseqs)))))))

    (define (lseq-every pred lseq . lseqs)
      (if (null? lseqs)
          (let loop ((lseq lseq) (last #t))
            (if (pair? lseq)
                (let ((val (pred (car lseq))))
                  (and val (loop (lseq-cdr lseq) val)))
                last))
          (let loop ((lseqs (cons lseq lseqs)) (last #t))
            (if (every pair? lseqs)
                (let ((val (apply pred (map car lseqs))))
                  (and val (loop (map lseq-cdr lseqs) val)))
                last))))

    (define (lseq-index pred lseq . lseqs)
      (if (null? lseqs)
          (let loop ((lseq lseq) (i 0))
            (and (pair? lseq)
                 (if (pred (car lseq))
                     i
                     (loop (lseq-cdr lseq) (+ i 1)))))
          (let loop ((lseqs (cons lseq lseqs)) (i 0))
            (and (every pair? lseqs)
                 (if (apply pred (map car lseqs))
                     i
                     (loop (map lseq-cdr lseqs) (+ i 1)))))))

    (define lseq-member
      (case-lambda
        ((x lseq)
          (lseq-member x lseq equal?))
        ((x lseq pred)
          (let loop ((lseq lseq))
            (and (pair? lseq)
                 (if (pred x (car lseq))
                     lseq
                     (loop (lseq-cdr lseq))))))))

    (define (lseq-memq x lseq)
      (lseq-member x lseq eq?))

    (define (lseq-memv x lseq)
      (lseq-member x lseq eqv?))

    ;; === Auxiliary ===

    (define (check-index who i)
      (unless (and (exact-integer? i) (>= i 0))
        (error (string-append who ": index is not a natural number:") i)))
  )
)
