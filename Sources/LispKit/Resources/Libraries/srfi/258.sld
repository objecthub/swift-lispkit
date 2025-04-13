;;; SRFI 258
;;; Uninterned Symbols
;;; 
;;; This SRFI provides functionality to deal with uninterend symbols.
;;; An uninterned symbol is not the same as any other symbol, even one
;;; with the same name. These symbols are useful in macro programming
;;; and in other situations where guaranteed-unique names are needed.
;;; 
;;; Author of spec: Wolfgang Corcoran-Mathe.
;;; 
;;; Copyright © 2025 Matthias Zenger. All rights reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(define-library (srfi 258)
  
  (export generate-uninterned-symbol
          string->uninterned-symbol
          symbol-interned?)
  
  (import (lispkit base))
)
