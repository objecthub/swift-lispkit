;;; SRFI 204
;;; Wright-Cartwright-Shinn Pattern Matcher
;;;
;;; Pattern matching decomposes a compound data structure into parts and assigns those
;;; parts to variables. This SRFI describes a pattern-matching library already in use
;;; by several scheme implementations which can match many common compound data structures.
;;;
;;; Copyright © 2020 Felix Thibault. All rights reserved.
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
;;;   Copyright © 2020 Matthias Zenger. All rights reserved.

(define-library (srfi 204)
  
  (export match
          match-lambda
          match-lambda*
          match-let
          match-letrec
          match-let*
          ___
          **1
          =..
          *..
          ***
          ?
          $
          struct
          object
          get!)
  
  (import (except (lispkit base) object)
          (lispkit match))
  
  (begin
    
    (define-syntax define-auxiliary-keyword
      (syntax-rules ()
        ((_ name)
          (define-syntax name
            (syntax-rules ()
              ((xx head . tail)
                (syntax-error "invalid auxiliary syntax" head . tail)))))))
    
    (define-auxiliary-keyword ___)
    (define-auxiliary-keyword **1)
    (define-auxiliary-keyword =..)
    (define-auxiliary-keyword *..)
    (define-auxiliary-keyword ***)
    (define-auxiliary-keyword ?)
    (define-auxiliary-keyword $)
    (define-auxiliary-keyword struct)
    (define-auxiliary-keyword object)
    (define-auxiliary-keyword get!)
  )
)
