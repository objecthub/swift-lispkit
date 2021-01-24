;;; SRFI 154
;;; First-class dynamic extents
;;;
;;; Scheme has the notion of the dynamic extent of a procedure call. A number of standard
;;; Scheme procedures and syntaxes like dynamic-wind, call-with-current-continuation, and
;;; parameterize deal with the dynamic extent indirectly. This SRFI reifies the dynamic
;;; extent into a first-class value together with a well-defined procedural interface and
;;; a syntax to create procedures that remember not only their environment at creation
;;; time but also their dynamic extent, which includes their dynamic environment.
;;; 
;;; Copyright © 2017 Marc Nieper-Wißkirchen. All rights reserved.
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
;;;   Copyright © 2021 Matthias Zenger. All rights reserved.

(define-library (srfi 154)
  
  (export dynamic-extent?
          current-dynamic-extent
          with-dynamic-extent
          dynamic-lambda)
  
  (import (lispkit base))
  
  (begin
    (define-record-type <dynamic-extent>
      (make-dynamic-extent proc)
      dynamic-extent?
      (proc dynamic-extent-proc))
    
    (define (current-dynamic-extent)
      (call-with-current-continuation
        (lambda (return)
          (let-values
            (((k thunk)
              (call-with-current-continuation
                (lambda (c)
                  (return
                    (make-dynamic-extent (lambda (thunk)
                                           (call-with-current-continuation
                                             (lambda (k)
                                               (c k thunk))))))))))
            (call-with-values thunk k)))))
    
    (define (with-dynamic-extent dynamic-extent thunk)
      ((dynamic-extent-proc dynamic-extent) thunk))
    
    (define-syntax dynamic-lambda
      (syntax-rules ()
        ((dynamic-lambda formals body)
          (let ((dynamic-extent (current-dynamic-extent)))
            (lambda formals
              (with-dynamic-extent dynamic-extent (lambda () body)))))))
  )
)
