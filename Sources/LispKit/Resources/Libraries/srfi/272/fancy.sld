;;; SRFI 272 FANCY
;;; Pretty Printing: fancy library
;;;
;;; Library `(srfi 272 fancy)` provides all the functionality of library
;;; `(srfi 272 advanced)`. Procedure `pprint-file` is enhanced such that it
;;; preserves comments within expressions, recognizes Emacs-style "magic comments"
;;; for file-local configurations and "style comments" for defining formatting
;;; styles on a per-file basis. Procedure `pprint-file/html` generates HTML output.
;;; All exported identifiers except for `pprint-file` refer to the same bindings
;;; as the ones exported by `(srfi 272 advanced)`.
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

(define-library (srfi 272 fancy)
  (import (scheme base) (scheme char) (scheme cxr)
    (scheme case-lambda) (scheme inexact) (scheme file)
    (scheme read) (scheme write))

  ; main machinery
  (import
    (rename (srfi 272 advanced)
      (pprint-file advanced-pprint-file)))
  ; color support
  (import (srfi 272 colorize))
  ; LispKit: the implementation of pprint-file and pprint-file/html is provided by
  ; library (srfi 272 fancy support), which works on ports
  (import (srfi 272 fancy support))


  ; procedures
  (export pp pp* pprint pprint-shared pprint-simple pprint-file
    make-pprint-generator pprint-file/html)

  ; configuration
  (export pretty-style lookup-pp-style add-pp-style pretty-hook
    lookup-pp-hook add-pp-hook rmac-pp-hook glst-pp-hook
    bvec-pp-hook atom-pp-hook)

  ; parameters
  (export pp-width pp-circle pp-graph pp-radix pp-length
    pp-level pp-lines pp-indent pp-tab pp-max-tab pp-miser-width
    pp-inline-width pp-brackets pp-code pp-pretty pp-newline
    pp-color pp-emit pp-tint pp-decorate pp-styles pp-hooks
    ; LispKit: these parameters are specified, but not exported by the sample
    pp-level-stub pp-length-stub pp-lines-stub)

  (begin

    ; reads input file, pretty-prints it to output file or current output;
    ; comments are preserved, -*- line and style comments are recognized
    (define (pprint-file ifn . rest)
      (define-values (ofn kv*)
        (if (and (pair? rest) (string? (car rest)))
            (values (car rest) (cdr rest))
            (values #f rest)))
      (call-with-input-file ifn
        (lambda (ip)
          (if (not ofn)
              (pprint-port ip (current-output-port) kv*)
              (call-with-output-file ofn (lambda (op) (pprint-port ip op kv*)))))))

    ; reads input file, pretty-prints it as HTML to output file or current output
    (define (pprint-file/html ifn . rest)
      (define-values (ofn kv*)
        (if (and (pair? rest) (string? (car rest)))
            (values (car rest) (cdr rest))
            (values #f rest)))
      (call-with-input-file ifn
        (lambda (ip)
          (if (not ofn)
              (pprint-port/html ip (current-output-port) kv*)
              (call-with-output-file ofn (lambda (op) (pprint-port/html ip op kv*)))))))
  )
)
