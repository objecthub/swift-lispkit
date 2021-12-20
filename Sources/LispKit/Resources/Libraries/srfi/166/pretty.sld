;;; SRFI 166 PRETTY
;;; Monadic Formatting
;;; 
;;; A library of procedures for formatting Scheme objects to text in various ways, and
;;; for easily concatenating, composing and extending these formatters efficiently
;;; without resorting to capturing and manipulating intermediate strings. This SRFI is
;;; an updated version of SRFI 159, primarily with the difference that state variables
;;; are hygienic.
;;;
;;; Copyright © 2020 Marc Nieper-Wißkirchen. All rights reserved.
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

(define-library (srfi 166 pretty)
  
  (export pretty
          pretty-shared
          pretty-simply
          pretty-color)
  
  (import (scheme base)
          (srfi 166 base))
  
  (begin
    (define pretty #f)              ;FIXME
    (define pretty-shared #f)       ;FIXME
    (define pretty-simply #f)       ;FIXME
    (define pretty-color #f)        ;FIXME
  )
)
