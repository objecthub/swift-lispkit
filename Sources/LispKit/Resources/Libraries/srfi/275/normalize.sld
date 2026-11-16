;;; SRFI 275 NORMALIZE
;;; URIs, IRIs, and basic paths: normalization, resolution, and conversion
;;;
;;; Library `(srfi 275 normalize)` provides case, escape, and path normalization of
;;; URIs and IRIs, the resolution of relative references against a base (RFC 3986,
;;; section 5.2) and its inverse, relativization, as well as the conversion between
;;; URIs and IRIs.
;;;
;;; Case normalization converts ASCII letters in the scheme and the host to lowercase.
;;; Escape normalization decodes percent-encoded characters which are not reserved
;;; and which are permitted in the component (for URIs only ASCII characters are
;;; decoded) and encodes characters which are not permitted. Path normalization removes
;;; dot segments from the path of non-relative URIs and IRIs; relative references are
;;; not changed. The procedures `normalize-iri` and `normalize-uri` combine the three
;;; normalizations.
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

(define-library (srfi 275 normalize)

  (import (scheme base)
          (srfi 275 internal resolve))

  (export normalize-iri-case
          normalize-iri-escape
          normalize-iri-path
          normalize-iri
          normalize-uri-case
          normalize-uri-escape
          normalize-uri-path
          normalize-uri
          resolve-iri
          resolve-uri
          relativize-iri
          relativize-uri
          uri->iri
          iri->uri)

  (begin

    (define (normalize-iri-case iri)
      (normalize-case "normalize-iri-case" #t iri))

    (define (normalize-iri-escape iri)
      (normalize-escape "normalize-iri-escape" #t iri))

    (define (normalize-iri-path iri)
      (normalize-path "normalize-iri-path" #t iri))

    (define (normalize-iri iri)
      (normalize-all "normalize-iri" #t iri))

    (define (normalize-uri-case uri)
      (normalize-case "normalize-uri-case" #f uri))

    (define (normalize-uri-escape uri)
      (normalize-escape "normalize-uri-escape" #f uri))

    (define (normalize-uri-path uri)
      (normalize-path "normalize-uri-path" #f uri))

    (define (normalize-uri uri)
      (normalize-all "normalize-uri" #f uri))

    (define (resolve-iri base ref)
      (resolve-ident "resolve-iri" #t base ref))

    (define (resolve-uri base ref)
      (resolve-ident "resolve-uri" #f base ref))

    (define (relativize-iri base iri)
      (relativize-ident "relativize-iri" #t base iri))

    (define (relativize-uri base uri)
      (relativize-ident "relativize-uri" #f base uri))

    (define (iri->uri iri)
      (convert-ident "iri->uri" #t iri))

    (define (uri->iri uri)
      (convert-ident "uri->iri" #f uri))
  )
)
