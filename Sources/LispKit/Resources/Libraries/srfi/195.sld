;;; SRFI 195
;;; Multiple-value boxes
;;;
;;; Boxes are objects with a single mutable state. Several Schemes have them, sometimes
;;; called cells. A constructor, predicate, accessor, and mutator are provided.
;;; This SRFI extends the specification of the boxes of SRFI 111 so that they are
;;; multiple-values aware. Whereas a SRFI 111 box is limited in that it can only
;;; box a single value, multiple values can be boxed with this SRFI.
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
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
;;; PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
;;; FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
;;; OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;
;;; Adaptation to LispKit
;;;   Copyright © 2020 Matthias Zenger. All rights reserved.

(define-library (srfi 195)
  (export box?
          box
          unbox
          set-box!)
  (import (lispkit base)))
