;;; SRFI 236
;;; Evaluating expressions in an unspecified order
;;;
;;; This SRFI defines the independently syntax, which can be used to
;;; combine side effects into one expression without specifying their
;;; relative order.
;;;
;;; Copyright © 2022 Marc Nieper-Wißkirchen. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; Adaptation to LispKit
;;;   Copyright © 2023 Matthias Zenger. All rights reserved.

(define-library (srfi 236)
  
  (export independently)
  
  (import (lispkit base))
  
  (begin
    (define-syntax independently
      (syntax-rules ()
        ((independently expr ...)
         (independently-aux (expr ...)))))
           (define-syntax independently-aux
             (syntax-rules ()
               ((independently-aux () (expr tmp) ...)
                 (let ((tmp (begin expr #f)) ...) (values)))
               ((independently-aux (expr . exprs) . binds)
                 (independently-aux exprs (expr tmp) . binds))))
  )
)
