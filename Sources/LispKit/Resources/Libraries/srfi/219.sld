;;; SRFI 219
;;; Define higher-order lambda
;;;
;;; This SRFI codifies the following shorthand syntax, which some Scheme implementations
;;; have had for a long time:
;;;
;;; ```
;;; (define ((outer-name outer-args ...) inner-args ...)
;;;   inner-body ...)
;;; ```
;;;
;;; The syntax generalizes further: lambdas can be nested arbitrarily deep by adding more
;;; nested lists. The key is that each nested list always be at the head position of the
;;; containing list. Each inner list adds one outer lambda; this order is intuitive when
;;; reading the S-expression left-to-right. The identifier at the head position of the
;;; innermost list becomes the name of the definition.
;;;
;;; Apart from helping define higher-order functions, the additional shorthand syntax
;;; partially applies to the task of making partially applied functions in Scheme.
;;;
;;; Copyright © 2021 Lassi Kortela. All rights reserved.
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

(define-library (srfi 219)
  (export define)

  (import (rename (lispkit base) (define define/native)))

  (begin
    (define-syntax define
      (syntax-rules ()
        ((define ((head . outer-args) . args) . body)
          (define (head . outer-args) (lambda args . body)))
        ((define (head . args) . body)
          (define head (lambda args . body)))
        ((define head . body)
          (define/native head . body))))
  )
)
