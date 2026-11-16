;;; SRFI 275 UTILS
;;; URIs, IRIs, and basic paths: miscellaneous utilities
;;;
;;; Library `(srfi 275 utils)` provides procedures to percent-encode and percent-decode
;;; strings, as well as predicates which determine whether updating a component of a URI
;;; or IRI is legal, i.e. does not make the authority and the path inconsistent.
;;;
;;; Author of spec: Duncan Guthrie
;;;
;;; Copyright © 2026 Duncan Guthrie. All rights reserved.
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

(define-library (srfi 275 utils)

  (import (scheme base)
          (srfi 14)
          (srfi 275 internal chars)
          (srfi 275 internal encoding)
          (srfi 275 internal path)
          (srfi 275 internal ident))

  (export encode-string
          decode-string
          user-settable?
          host-settable?
          port-settable?
          path-settable?)

  (begin

    (define default-encode-set
      (char-set-complement char-set:iri-unreserved))

    (define (check-args who str cset)
      (unless (string? str)
        (fail who "not a string" str))
      (unless (char-set? cset)
        (fail who "not a character set" cset)))

    ;; Escapes the characters of `str` which are contained in `cset` and all percent
    ;; characters
    (define (encode-string str . opt)
      (let ((cset (if (pair? opt) (car opt) default-encode-set)))
        (check-args "encode-string" str cset)
        (percent-encode "encode-string" str cset)))

    ;; Decodes percent-encoded characters of `str` which are contained in `cset`
    (define (decode-string str . opt)
      (let ((cset (if (pair? opt) (car opt) char-set:full)))
        (check-args "decode-string" str cset)
        (percent-decode "decode-string" str cset)))

    (define (check-ident-arg who x)
      (unless (ident? x)
        (fail who "not an IRI or URI" x)))

    (define (user-settable? ident obj)
      (check-ident-arg "user-settable?" ident)
      (compatible-authority? ident obj (ident-host-raw ident) (ident-port-raw ident)))

    (define (host-settable? ident obj)
      (check-ident-arg "host-settable?" ident)
      (compatible-authority? ident (ident-user-raw ident) obj (ident-port-raw ident)))

    (define (port-settable? ident obj)
      (check-ident-arg "port-settable?" ident)
      (compatible-authority? ident (ident-user-raw ident) (ident-host-raw ident) obj))

    (define (path-settable? ident path)
      (check-ident-arg "path-settable?" ident)
      (and (path? path)
           (compatible-path? path
                             (authority-set? (ident-user-raw ident) (ident-host-raw ident)
                                             (ident-port-raw ident))
                             (not (ident-scheme-raw ident)))))
  )
)
