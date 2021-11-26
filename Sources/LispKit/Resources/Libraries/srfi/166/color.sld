;;; SRFI 166 COLOR
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

(define-library (srfi 166 color)
  
  (export as-red
          as-blue
          as-green
          as-cyan
          as-yellow
          as-magenta
          as-white
          as-black
          as-bold
          as-underline
          as-color
          as-true-color)
  
  (import (scheme base)
          (srfi 166 base)
          (rename (srfi 165)
              (make-computation-environment-variable make-environment-variable)))

  (begin
    (define color (make-environment-variable 'color #f #t))
    (define underline? (make-environment-variable 'underline? #f #t))
    (define bold? (make-environment-variable 'bold? #f #t))
    
    (define ansi-escape
      (fn (color underline? bold?)
        (each #\x1b #\[
          (joined displayed
              (cons "0"
                (append (if color (list color) '())
                    (if bold? (list "1") '())
                    (if underline? (list "4") '())))
              ";")
          #\m)))
    
    (define (colored new-color fmt)
      (each (with ((color new-color))
          (each ansi-escape fmt))
        ansi-escape))
    
    (define (as-red . fmt*) (colored "31" (each-in-list fmt*)))
    (define (as-blue . fmt*) (colored "34" (each-in-list fmt*)))
    (define (as-green . fmt*) (colored "32" (each-in-list fmt*)))
    (define (as-cyan . fmt*) (colored "36" (each-in-list fmt*)))
    (define (as-yellow . fmt*) (colored "33" (each-in-list fmt*)))
    (define (as-magenta . fmt*) (colored "35" (each-in-list fmt*)))
    (define (as-white . fmt*) (colored "37" (each-in-list fmt*)))
    (define (as-black . fmt*) (colored "30" (each-in-list fmt*)))
    
    (define (as-bold . fmt*)
      (each (with ((bold? #t))
          (each-in-list (cons ansi-escape fmt*)))
        ansi-escape))
    
    (define (as-underline . fmt*)
      (each (with ((underline? #t))
          (each-in-list (cons ansi-escape fmt*)))
        ansi-escape))
    
    (define (as-color r g b . fmt*)
      (define color (joined displayed
                (list "38" "5" (+ 16 (* 36 r) (* 6 g) b)) ";"))
      (colored color (each-in-list fmt*)))
    
    (define (as-true-color r g b . fmt*)
      (define color (joined displayed
                (list "38" "2" r g b) ";"))
      (colored color (each-in-list fmt*)))
    
  )
)
