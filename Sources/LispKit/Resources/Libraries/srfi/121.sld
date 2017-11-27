;;; SRFI 121
;;; Generators
;;;
;;; This SRFI defines utility procedures that create, transform, and consume generators.
;;; A generator is simply a procedure with no arguments that works as a source of a series
;;; of values. Every time it is called, it yields a value. Generators may be finite or
;;; infinite; a finite generator returns an end-of-file object to indicate that it is
;;; exhausted. For example, read-char, read-line, and read are generators that generate
;;; characters, lines, and objects from the current input port. Generators provide
;;; lightweight laziness.
;;;
;;; Copyright © 2015 Shiro Kawai, John Cowan, Thomas Gilray. All Rights Reserved.
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
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;;; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2017 Matthias Zenger. All rights reserved.

(define-library (srfi 121)

  (export generator
          make-iota-generator
          make-range-generator
          make-coroutine-generator
          list->generator
          vector->generator
          reverse-vector->generator
          string->generator
          bytevector->generator
          make-for-each-generator
          make-unfold-generator
          gcons*
          gappend
          gcombine
          gfilter
          gremove
          gtake
          gdrop
          gtake-while
          gdrop-while
          gdelete
          gdelete-neighbor-dups
          gindex
          gselect
          generator->list
          generator->reverse-list
          generator->vector
          generator->vector!
          generator->string
          generator-fold
          generator-for-each
          generator-find
          generator-count
          generator-any
          generator-every
          generator-unfold)

  (import (lispkit base))

  (begin

    ;; generator
    (define (generator . args)
      (lambda () (if (null? args)
                     (eof-object)
                     (let ((next (car args))) (set! args (cdr args)) next))))

    ;; make-iota-generator
    (define make-iota-generator
      (case-lambda ((count) (make-iota-generator count 0 1))
                   ((count start) (make-iota-generator count start 1))
                   ((count start step) (make-iota count start step))))

    ;; make-iota
    (define (make-iota count start step)
      (lambda () (if (<= count 0)
                     (eof-object)
                     (let ((result start))
                       (set! count (- count 1))
                       (set! start (+ start step))
                       result))))

    ;; make-range-generator
    (define make-range-generator
      (case-lambda ((start end)      (make-range-generator start end 1))
                   ((start)          (make-infinite-range-generator start))
                   ((start end step) (set! start (- (+ start step) step))
                                     (lambda () (if (< start end)
                                                    (let ((v start))
                                                      (set! start (+ start step))
                                                      v)
                                                    (eof-object))))))

    (define (make-infinite-range-generator start)
      (lambda () (let ((result start)) (set! start (+ start 1)) result)))

    ;; make-coroutine-generator
    (define (make-coroutine-generator proc)
      (let* ((return #f)
             (resume #f)
             (yield  (lambda (v) (call/cc (lambda (r) (set! resume r) (return v))))))
        (lambda () (call/cc (lambda (cc) (set! return cc)
                              (if resume
                                (resume (void))  ; void? or yield again?
                                (begin (proc yield)
                                       (set! resume (lambda (v) (return (eof-object))))
                                       (return (eof-object)))))))))

    ;; list->generator
    (define (list->generator lst)
      (lambda () (if (null? lst)
                   (eof-object)
                   (let ((next (car lst))) (set! lst (cdr lst)) next))))

    ;; vector->generator
    (define vector->generator
      (case-lambda ((vec)           (vector->generator vec 0 (vector-length vec)))
                   ((vec start)     (vector->generator vec start (vector-length vec)))
                   ((vec start end) (lambda () (if (>= start end)
                                                   (eof-object)
                                                   (let ((next (vector-ref vec start)))
                                                     (set! start (+ start 1))
                                                     next))))))

    ;; reverse-vector->generator
    (define reverse-vector->generator
      (case-lambda ((vec)           (reverse-vector->generator vec 0 (vector-length vec)))
                   ((vec start)     (reverse-vector->generator vec start (vector-length vec)))
                   ((vec start end) (lambda () (if (>= start end)
                                                   (eof-object)
                                                   (let ((next (vector-ref vec (- end 1))))
                                                     (set! end (- end 1))
                                                     next))))))

    ;; string->generator
    (define string->generator
      (case-lambda ((str)           (string->generator str 0 (string-length str)))
                   ((str start)     (string->generator str start (string-length str)))
                   ((str start end) (lambda () (if (>= start end)
                                                   (eof-object)
                                                   (let ((next (string-ref str start)))
                                                     (set! start (+ start 1))
                                                     next))))))

    ;; bytevector->generator
    (define bytevector->generator
      (case-lambda ((str)           (bytevector->generator str 0 (bytevector-length str)))
                   ((str start)     (bytevector->generator str start (bytevector-length str)))
                   ((str start end) (lambda () (if (>= start end)
                                                   (eof-object)
                                                   (let ((next (bytevector-u8-ref str start)))
                                                     (set! start (+ start 1))
                                                     next))))))

    ;; make-for-each-generator
    ;FIXME: seems to fail test
    (define (make-for-each-generator for-each obj)
      (make-coroutine-generator (lambda (yield) (for-each yield obj))))

    ;; make-unfold-generator
    (define (make-unfold-generator stop? mapper successor seed)
      (make-coroutine-generator (lambda (yield) (let loop ((s seed))
                                                  (if (stop? s)
                                                      (void)
                                                      (begin (yield (mapper s))
                                                             (loop (successor s))))))))

    ;; gcons*
    (define (gcons* . args)
      (lambda () (if (null? args)
                     (eof-object)
                     (if (= (length args) 1)
                         ((car args))
                         (let ((v (car args))) (set! args (cdr args)) v)))))

    ;; gappend
    (define (gappend . args)
      (lambda () (if (null? args)
                     (eof-object)
                     (let loop ((v ((car args))))
                      (if (eof-object? v)
                        (begin (set! args (cdr args))
                               (if (null? args)
                                 (eof-object)
                                 (loop ((car args)))))
                        v)))))

    ;; gcombine
    (define (gcombine proc seed . gens)
      (lambda ()
        (let ((items (map (lambda (x) (x)) gens)))
          (if (any? eof-object? items)
              (eof-object)
              (let-values (((value newseed) (apply proc (append items (list seed)))))
                (set! seed newseed)
                value)))))

    ;; gfilter
    (define (gfilter pred gen)
      (lambda () (let loop ()
                   (let ((next (gen)))
                   (if (or (eof-object? next) (pred next)) next (loop))))))

    ;; gremove
    (define (gremove pred gen)
      (gfilter (lambda (v) (not (pred v))) gen))

    ;; gtake
    (define gtake
      (case-lambda ((gen k)
                     (gtake gen k (eof-object)))
                   ((gen k padding)
                     (make-coroutine-generator
                       (lambda (yield) (if (> k 0)
                                           (let loop ((i 0) (v (gen)))
                                             (begin (if (eof-object? v) (yield padding) (yield v))
                                                    (if (< (+ 1 i) k)
                                                        (loop (+ 1 i) (gen))
                                                        (eof-object))))
                                           (eof-object)))))))

    ;; gdrop
    (define (gdrop gen k)
      (lambda () (do () ((<= k 0)) (set! k (- k 1)) (gen)) (gen)))

    ;; gdrop-while
    (define (gdrop-while pred gen)
      (let ((found #f))
        (lambda () (let loop ()
                      (let ((val (gen)))
                        (cond (found val)
                              ((and (not (eof-object? val)) (pred val)) (loop))
                              (else (set! found #t) val)))))))

    ;; gtake-while
    (define (gtake-while pred gen)
      (lambda () (let ((next (gen)))
                   (if (eof-object? next)
                       next
                       (if (pred next) next (begin (set! gen (generator)) (gen)))))))

    ;; gdelete
    (define gdelete
      (case-lambda ((item gen)    (gdelete item gen equal?))
                   ((item gen ==) (lambda ()
                                    (let loop ((v (gen)))
                                      (cond ((eof-object? v) (eof-object))
                                            ((== item v)     (loop (gen)))
                                            (else            v)))))))

    ;; gdelete-neighbor-dups
    (define gdelete-neighbor-dups
      (case-lambda ((gen)
                     (gdelete-neighbor-dups gen equal?))
                   ((gen ==)
                     (let ((firsttime #t) (prev #f))
                       (lambda () (if firsttime
                                      (begin (set! firsttime #f)
                                             (set! prev (gen))
                                             prev)
                                      (let loop ((v (gen)))
                                        (cond ((eof-object? v) v)
                                              ((== prev v)     (loop (gen)))
                                              (else            (set! prev v) v)))))))))

    ;; gindex
    (define (gindex value-gen index-gen)
      (let ((done? #f) (count 0))
        (lambda ()
          (if done?
              (eof-object)
              (let loop ((value (value-gen)) (index (index-gen)))
                (cond ((or (eof-object? value) (eof-object? index))
                         (set! done? #t)
                         (eof-object))
                      ((= index count)
                         (set! count (+ count 1))
                         value)
                      (else
                         (set! count (+ count 1))
                         (loop (value-gen) index))))))))

    ;; gselect
    (define (gselect value-gen truth-gen)
      (let ((done? #f))
        (lambda ()
          (if done?
              (eof-object)
              (let loop ((value (value-gen)) (truth (truth-gen)))
                (cond ((or (eof-object? value) (eof-object? truth))
                         (set! done? #t)
                         (eof-object))
                      (truth
                         value)
                      (else
                         (loop (value-gen) (truth-gen)))))))))

    ;; generator->list
    (define generator->list
      (case-lambda ((gen n) (generator->list (gtake gen n)))
                   ((gen)   (reverse (generator->reverse-list gen)))))

    ;; generator->reverse-list
    (define generator->reverse-list
      (case-lambda ((gen n) (generator->reverse-list (gtake gen n)))
                   ((gen)   (generator-fold cons '() gen))))

    ;; generator->vector
    (define generator->vector
      (case-lambda ((gen)   (list->vector (generator->list gen)))
                   ((gen n) (list->vector (generator->list gen n)))))

    ;; generator->vector!
    (define (generator->vector! vector at gen)
      (let loop ((value (gen)) (count 0) (at at))
        (cond ((eof-object? value)            count)
              ((>= at (vector-length vector)) count)
              (else
                 (vector-set! vector at value)
                 (loop (gen) (+ count 1) (+ at 1))))))

    ;; generator->string
    (define generator->string
      (case-lambda ((gen)   (list->string (generator->list gen)))
                   ((gen n) (list->string (generator->list gen n)))))

    ;; generator-fold
    (define (generator-fold f seed . gs)
      (define (inner-fold seed)
        (let ((vs (map (lambda (g) (g)) gs)))
          (if (any? eof-object? vs)
              seed
              (inner-fold (apply f (append vs (list seed)))))))
      (inner-fold seed))

    ;; generator-for-each
    (define (generator-for-each f . gs)
      (let loop ()
        (let ((vs (map (lambda (g) (g)) gs)))
          (if (any? eof-object? vs)
              (void)
              (begin (apply f vs) (loop))))))

    ;; generator-find
    (define (generator-find pred g)
      (let loop ((v (g)))
        ; A literal interpretation might say it only terminates on #eof if (pred #eof)
        ; but I think this makes more sense...
        (if (or (pred v) (eof-object? v)) v (loop (g)))))

    ;; generator-count
    (define (generator-count pred g)
      (generator-fold (lambda (v n) (if (pred v) (+ 1 n) n)) 0 g))

    ;; generator-any
    (define (generator-any pred g)
      (let loop ((v (g)))
        (if (eof-object? v)
            #f
            (if (pred v) #t (loop (g))))))

    ;; generator-every
    (define (generator-every pred g)
      (let loop ((v (g)))
        (if (eof-object? v)
            #t
            (if (pred v)
                (loop (g))
                #f)))) ; the spec would have me return #f, but I think it must simply be wrong...

    ;; generator-unfold
    (define (generator-unfold g unfold . args)
      (apply unfold eof-object? (lambda (x) x) (lambda (x) (g)) (g) args))))
