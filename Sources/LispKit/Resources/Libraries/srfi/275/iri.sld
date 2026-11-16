;;; SRFI 275 IRI
;;; URIs, IRIs, and basic paths: Internationalized Resource Identifiers
;;;
;;; Library `(srfi 275 iri)` implements Internationalized Resource Identifiers (IRIs)
;;; according to RFC 3987 and relative references thereof. IRIs are immutable objects
;;; which are structured into the components scheme, user, host, port, path, query and
;;; fragment. Components are accessed by getters and are updated by persistent setters,
;;; which return a modified copy. Setters escape characters which are not permitted in a
;;; component, and make sure that the resulting IRI is consistent: the path of an IRI
;;; is never changed so that it clashes with the authority and vice versa.
;;;
;;; IRIs are parsed from strings or ports and serialized into strings. String components
;;; are returned as they appear in the IRI: percent-encoded bytes are not decoded and
;;; use uppercase hexadecimal digits. The path of an IRI is a path object of library
;;; `(srfi 275 path)`. Positions in error messages of parsing procedures are indices
;;; into the parsed text.
;;; Characters are interpreted as UTF-16 code units, i.e. characters outside of the
;;; basic multilingual plane are represented by surrogate pairs. The character sets
;;; exported by this library reflect this by containing all surrogate code units if
;;; the set contains supplementary characters.
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

(define-library (srfi 275 iri)

  (import (scheme base)
          (srfi 275 internal chars)
          (srfi 275 internal path)
          (srfi 275 internal ident))

  ;; types and constructors
  (export iri?
          relative-iri?
          non-relative-iri?
          rfc3987-absolute-iri?
          empty-iri
          iri=?)

  ;; parsing and serialization
  (export string->iri
          string->non-relative-iri
          string->rfc3987-absolute-iri
          get-iri
          get-non-relative-iri
          get-rfc3987-absolute-iri
          iri->string)

  ;; getters
  (export iri-scheme
          iri-user
          iri-host
          iri-port
          iri-authority
          iri-username+password
          iri-path
          iri-query
          iri-fragment
          iri-path-segment-ref)

  ;; setters
  (export update-iri-scheme
          update-iri-user
          update-iri-host
          update-iri-port
          update-iri-authority
          update-iri-path
          update-iri-query
          update-iri-fragment
          iri-path-segment-update)

  ;; character sets
  (export char-set:iri
          char-set:iri-unreserved
          char-set:ucschar
          char-set:iri-private
          char-set:gen-delims
          char-set:sub-delims
          char-set:reserved
          char-set:scheme
          char-set:iri-userinfo
          char-set:iri-reg-name
          char-set:iri-segment
          char-set:iri-query
          char-set:iri-fragment)

  (begin

    (define (iri? obj)
      (ident-of-kind? #t obj))

    (define (relative-iri? obj)
      (relative-ident? obj #t))

    (define (non-relative-iri? obj)
      (non-relative-ident? obj #t))

    (define (rfc3987-absolute-iri? obj)
      (absolute-ident? obj #t))

    (define (empty-iri)
      (empty-ident #t))

    (define (iri=? iri1 iri2 . iris)
      (ident=? "iri=?" #t iri1 iri2 iris))

    ;;; Parsing and serialization

    (define (string->iri str)
      (parse-text "string->iri" #t str 'any))

    (define (string->non-relative-iri str)
      (parse-text "string->non-relative-iri" #t str 'non-relative))

    (define (string->rfc3987-absolute-iri str)
      (parse-text "string->rfc3987-absolute-iri" #t str 'absolute))

    (define (stop-object opt)
      (if (pair? opt) (car opt) (eof-object)))

    (define (get-iri port . opt)
      (parse-text "get-iri" #t (read-text "get-iri" port (stop-object opt)) 'any))

    (define (get-non-relative-iri port . opt)
      (parse-text "get-non-relative-iri" #t
                  (read-text "get-non-relative-iri" port (stop-object opt)) 'non-relative))

    (define (get-rfc3987-absolute-iri port . opt)
      (parse-text "get-rfc3987-absolute-iri" #t
                  (read-text "get-rfc3987-absolute-iri" port (stop-object opt)) 'absolute))

    (define (iri->string iri)
      (check-ident "iri->string" #t iri)
      (ident->text iri))

    ;;; Getters

    (define (iri-scheme iri)
      (get-scheme "iri-scheme" #t iri))

    (define (iri-user iri)
      (get-component "iri-user" #t iri 'user))

    (define (iri-host iri)
      (get-component "iri-host" #t iri 'host))

    (define (iri-port iri)
      (get-port "iri-port" #t iri))

    (define (iri-authority iri)
      (get-authority "iri-authority" #t iri))

    (define (iri-username+password iri)
      (get-username+password "iri-username+password" #t iri))

    (define (iri-path iri)
      (get-path "iri-path" #t iri))

    (define (iri-query iri)
      (get-component "iri-query" #t iri 'query))

    (define (iri-fragment iri)
      (get-component "iri-fragment" #t iri 'fragment))

    (define (iri-path-segment-ref iri k)
      (segment-ref "iri-path-segment-ref" #t iri k))

    ;;; Setters

    (define (update-iri-scheme iri str)
      (set-scheme "update-iri-scheme" #t iri str))

    (define (update-iri-user iri obj)
      (set-user "update-iri-user" #t iri obj))

    (define (update-iri-host iri obj)
      (set-host "update-iri-host" #t iri obj))

    (define (update-iri-port iri obj)
      (set-port "update-iri-port" #t iri obj))

    (define (update-iri-authority iri user host port)
      (set-authority "update-iri-authority" #t iri user host port))

    (define (update-iri-path iri path)
      (set-path "update-iri-path" #t iri path))

    (define (update-iri-query iri obj)
      (set-query "update-iri-query" #t iri obj))

    (define (update-iri-fragment iri obj)
      (set-fragment "update-iri-fragment" #t iri obj))

    (define (iri-path-segment-update iri k str)
      (segment-update "iri-path-segment-update" #t iri k str))
  )
)
