;;; SRFI 275 URI
;;; URIs, IRIs, and basic paths: Uniform Resource Identifiers
;;;
;;; Library `(srfi 275 uri)` implements Uniform Resource Identifiers (URIs)
;;; according to RFC 3986 and relative references thereof. URIs are immutable objects
;;; which are structured into the components scheme, user, host, port, path, query and
;;; fragment. Components are accessed by getters and are updated by persistent setters,
;;; which return a modified copy. Setters escape characters which are not permitted in a
;;; component, and make sure that the resulting URI is consistent: the path of a URI
;;; is never changed so that it clashes with the authority and vice versa.
;;;
;;; URIs are parsed from strings or ports and serialized into strings. String components
;;; are returned as they appear in the URI: percent-encoded bytes are not decoded and
;;; use uppercase hexadecimal digits. The path of a URI is a path object of library
;;; `(srfi 275 path)`. Positions in error messages of parsing procedures are indices
;;; into the parsed text.
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

(define-library (srfi 275 uri)

  (import (scheme base)
          (srfi 275 internal chars)
          (srfi 275 internal path)
          (srfi 275 internal ident))

  ;; types and constructors
  (export uri?
          relative-uri?
          non-relative-uri?
          rfc3986-absolute-uri?
          empty-uri
          uri=?)

  ;; parsing and serialization
  (export string->uri
          string->non-relative-uri
          string->rfc3986-absolute-uri
          get-uri
          get-non-relative-uri
          get-rfc3986-absolute-uri
          uri->string)

  ;; getters
  (export uri-scheme
          uri-user
          uri-host
          uri-port
          uri-authority
          uri-username+password
          uri-path
          uri-query
          uri-fragment
          uri-path-segment-ref)

  ;; setters
  (export update-uri-scheme
          update-uri-user
          update-uri-host
          update-uri-port
          update-uri-authority
          update-uri-path
          update-uri-query
          update-uri-fragment
          uri-path-segment-update)

  ;; character sets
  (export char-set:uri
          char-set:uri-unreserved
          char-set:gen-delims
          char-set:sub-delims
          char-set:reserved
          char-set:scheme
          char-set:uri-userinfo
          char-set:uri-reg-name
          char-set:uri-segment
          char-set:uri-query
          char-set:uri-fragment)

  (begin

    (define (uri? obj)
      (ident-of-kind? #f obj))

    (define (relative-uri? obj)
      (relative-ident? obj #f))

    (define (non-relative-uri? obj)
      (non-relative-ident? obj #f))

    (define (rfc3986-absolute-uri? obj)
      (absolute-ident? obj #f))

    (define (empty-uri)
      (empty-ident #f))

    (define (uri=? uri1 uri2 . uris)
      (ident=? "uri=?" #f uri1 uri2 uris))

    ;;; Parsing and serialization

    (define (string->uri str)
      (parse-text "string->uri" #f str 'any))

    (define (string->non-relative-uri str)
      (parse-text "string->non-relative-uri" #f str 'non-relative))

    (define (string->rfc3986-absolute-uri str)
      (parse-text "string->rfc3986-absolute-uri" #f str 'absolute))

    (define (stop-object opt)
      (if (pair? opt) (car opt) (eof-object)))

    (define (get-uri port . opt)
      (parse-text "get-uri" #f (read-text "get-uri" port (stop-object opt)) 'any))

    (define (get-non-relative-uri port . opt)
      (parse-text "get-non-relative-uri" #f
                  (read-text "get-non-relative-uri" port (stop-object opt)) 'non-relative))

    (define (get-rfc3986-absolute-uri port . opt)
      (parse-text "get-rfc3986-absolute-uri" #f
                  (read-text "get-rfc3986-absolute-uri" port (stop-object opt)) 'absolute))

    (define (uri->string uri)
      (check-ident "uri->string" #f uri)
      (ident->text uri))

    ;;; Getters

    (define (uri-scheme uri)
      (get-scheme "uri-scheme" #f uri))

    (define (uri-user uri)
      (get-component "uri-user" #f uri 'user))

    (define (uri-host uri)
      (get-component "uri-host" #f uri 'host))

    (define (uri-port uri)
      (get-port "uri-port" #f uri))

    (define (uri-authority uri)
      (get-authority "uri-authority" #f uri))

    (define (uri-username+password uri)
      (get-username+password "uri-username+password" #f uri))

    (define (uri-path uri)
      (get-path "uri-path" #f uri))

    (define (uri-query uri)
      (get-component "uri-query" #f uri 'query))

    (define (uri-fragment uri)
      (get-component "uri-fragment" #f uri 'fragment))

    (define (uri-path-segment-ref uri k)
      (segment-ref "uri-path-segment-ref" #f uri k))

    ;;; Setters

    (define (update-uri-scheme uri str)
      (set-scheme "update-uri-scheme" #f uri str))

    (define (update-uri-user uri obj)
      (set-user "update-uri-user" #f uri obj))

    (define (update-uri-host uri obj)
      (set-host "update-uri-host" #f uri obj))

    (define (update-uri-port uri obj)
      (set-port "update-uri-port" #f uri obj))

    (define (update-uri-authority uri user host port)
      (set-authority "update-uri-authority" #f uri user host port))

    (define (update-uri-path uri path)
      (set-path "update-uri-path" #f uri path))

    (define (update-uri-query uri obj)
      (set-query "update-uri-query" #f uri obj))

    (define (update-uri-fragment uri obj)
      (set-fragment "update-uri-fragment" #f uri obj))

    (define (uri-path-segment-update uri k str)
      (segment-update "uri-path-segment-update" #f uri k str))
  )
)
