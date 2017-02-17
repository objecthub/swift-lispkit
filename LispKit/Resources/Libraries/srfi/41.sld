;;; SRFI 41
;;; Streams
;;;
;;; Streams, sometimes called lazy lists, are a sequential data structure containing elements
;;; computed only on demand. A stream is either null or is a pair with a stream in its cdr.
;;; Since elements of a stream are computed only when accessed, streams can be infinite.
;;; Once computed, the value of a stream element is cached in case it is needed again.
;;;
;;; Streams without memoization were first described by Peter Landin in 1965. Memoization
;;; became accepted as an essential feature of streams about a decade later. Today, streams
;;; are the signature data type of functional programming languages such as Haskell.
;;;
;;; This Scheme Request for Implementation describes two libraries for operating on streams:
;;; a canonical set of stream primitives and a set of procedures and syntax derived from
;;; those primitives that permits convenient expression of stream operations.
;;;
;;; Copyright © 2007 Philip L. Bewig. All Rights Reserved.
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
;;;   Copyright © 2017 Matthias Zenger. All rights reserved.

(define-library (srfi 41)

  (export define-stream
          list->stream
          port->stream
          stream
          stream?
          stream->list
          stream-append
          stream-car  
          stream-cdr
          stream-concat
          stream-cons
          stream-constant
          stream-drop
          stream-drop-while
          stream-filter
          stream-fold
          stream-for-each
          stream-from
          stream-iterate
          stream-lambda
          stream-length
          stream-let
          stream-map
          stream-match
          stream-null
          stream-null?
          stream-of
          stream-pair?
          stream-range
          stream-ref
          stream-reverse
          stream-scan
          stream-take
          stream-take-while
          stream-unfold
          stream-unfolds
          stream-zip)
  
  (import (srfi 41 primitive)
          (srfi 41 derived)))
