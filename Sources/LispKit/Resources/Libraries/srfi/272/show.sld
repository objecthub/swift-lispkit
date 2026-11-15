;;; SRFI 272 SHOW
;;; Pretty Printing: integration with SRFI 166
;;;
;;; Library `(srfi 272 show)` integrates the advanced pretty printer of library
;;; `(srfi 272 advanced)` with the monadic formatting framework of SRFI 166. It
;;; exports two procedures, `pprinted` and `pprinted*`, which return SRFI 166
;;; formatters, as well as all parameters with prefix `pp-` of library
;;; `(srfi 272 advanced)`.
;;;
;;; Copyright © 2026 Sergei Egorov. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;; this software and associated documentation files (the "Software"), to deal in the
;;; Software without restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so, subject to the
;;; following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2026 Matthias Zenger. All rights reserved.

(define-library (srfi 272 show)
  (import
    (scheme base)
    (srfi 166 base)
    (srfi 272 advanced))

  (export
    pprinted pprinted*
    pp-width pp-circle pp-graph pp-radix pp-length
    pp-level pp-lines pp-indent pp-tab pp-max-tab pp-miser-width
    pp-inline-width pp-brackets pp-code pp-pretty pp-newline
    pp-color pp-emit pp-tint pp-decorate pp-styles pp-hooks
    ; LispKit: these parameters are specified, but not exported by the sample
    pp-level-stub pp-length-stub pp-lines-stub)

  (begin

    (define (pprinted obj . kv*)
      ; LispKit: as specified by the SRFI, the width state variable overrides
      ; explicitly specified values of pp-width (keyword arguments are scanned
      ; from left to right)
      (fn (width) ; default width is taken from show env
        (let* ((ppe (cons pp-width (cons width kv*)))
               (g (apply make-pprint-generator obj ppe)))
          (let lp ((s (g)))
            (if (eof-object? s)
                nothing
                (each s (fn () (lp (g)))))))))

    (define (pprinted* obj arg . args)
      (define (cons* arg . args)
        (let loop ((xs (cons arg args)))
          (if (null? (cdr xs))
              (car xs)
              (cons (car xs) (loop (cdr xs))))))
      (apply pprinted obj (apply cons* arg args)))
))
