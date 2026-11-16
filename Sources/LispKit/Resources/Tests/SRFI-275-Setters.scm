;;; SRFI 275 SETTER REGRESSION TEST SUITE
;;;
;;; This is the test suite for the persistent setters of libraries `(srfi 275 iri)` and
;;; `(srfi 275 uri)`, namely with respect to preserving the consistency of the authority
;;; and the path of an identifier, and for the predicates of library `(srfi 275 utils)`
;;; which determine whether a component is settable. It is based on the test suite of the
;;; SRFI 275 sample implementation and on the examples of the SRFI document.
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

(import (lispkit base)
        (lispkit test)
        (srfi 275 iri)
        (srfi 275 uri)
        (srfi 275 path)
        (srfi 275 utils))

;; The three components of the authority of an identifier as a list
(define (authority-list authority x)
  (call-with-values (lambda () (authority x)) list))

;; Tests the setters for the user component
(define (user-tests kind parse authority user update)
  (test-group (string-append "set " kind " user")
    (let ((sample0 (parse "urn:/some/where/place"))
          (sample1 (parse "urn:/some/where/place"))
          (sample2 (parse "urn://user@"))
          (sample3 (parse "urn://user@example.org"))
          (sample4 (parse "urn://user@:80"))
          (sample5 (parse "urn://user@//some/where/place"))
          (sample6 (parse "urn:some/where/place")))
      (test-equal "persistence of update" '("persistent" #f)
                  (list (user (update sample0 "persistent")) (user sample0)))
      (test-equal "nil authority, update user to non-nil" '("user" #f #f)
                  (authority-list authority (update sample0 "user")))
      (test-equal "nil authority, update user to nil" '(#f #f #f)
                  (authority-list authority (update sample1 #f)))
      (test-equal "non-nil user, nil host/port, update user to nil" '(#f #f #f)
                  (authority-list authority (update sample2 #f)))
      (test-equal "non-nil user, non-nil host, nil port, update user to nil" '(#f "example.org" #f)
                  (authority-list authority (update sample3 #f)))
      (test-equal "non-nil user, non-nil port, nil host, update user to nil" '(#f #f 80)
                  (authority-list authority (update sample4 #f)))
      (test-equal "empty user" '("" #f #f)
                  (authority-list authority (update sample0 "")))
      (test-equal "user with colon" "user:pass"
                  (user (update sample0 "user:pass")))
      (test-equal "user with at sign is encoded" "a%40b"
                  (user (update sample0 "a@b")))
      (test-error "update to nil in conflict with the path" (update sample5 #f))
      (test-error "update to non-nil in conflict with the path" (update sample6 "user"))
      (test-error "not a string" (update sample0 'user))
      (test-error "not a string" (update sample0 42))
      (test-error "not an identifier" (update "urn:/a" "user")))))

;; Tests the setters for the host component
(define (host-tests kind parse authority host update)
  (test-group (string-append "set " kind " host")
    (let ((sample0 (parse "urn:/some/where/place"))
          (sample1 (parse "urn:/some/where/place"))
          (sample2 (parse "urn://"))
          (sample3 (parse "urn://user@example.org"))
          (sample4 (parse "urn://example.org:80"))
          (sample5 (parse "urn://example.org//some/where/place"))
          (sample6 (parse "urn:some/where/place")))
      (test-equal "persistence of update" '("persistent" #f)
                  (list (host (update sample0 "persistent")) (host sample0)))
      (test-equal "nil authority, update host to non-nil" '(#f "example.org" #f)
                  (authority-list authority (update sample0 "example.org")))
      (test-equal "nil authority, update host to nil" '(#f #f #f)
                  (authority-list authority (update sample1 #f)))
      (test-equal "non-nil host, nil user/port, update host to nil" '(#f #f #f)
                  (authority-list authority (update sample2 #f)))
      (test-equal "non-nil host, non-nil user, nil port, update host to nil" '("user" #f #f)
                  (authority-list authority (update sample3 #f)))
      (test-equal "non-nil host, non-nil port, nil user, update host to nil" '(#f #f 80)
                  (authority-list authority (update sample4 #f)))
      (test-equal "empty host" '(#f "" #f)
                  (authority-list authority (update sample0 "")))
      (test-equal "IP literal" "[2001:db8::1]"
                  (host (update sample0 "[2001:db8::1]")))
      (test-equal "future IP literal" "[v1.a:b]"
                  (host (update sample0 "[v1.a:b]")))
      (test-equal "colon is encoded" "a%3Ab"
                  (host (update sample0 "a:b")))
      (test-equal "slash is encoded" "a%2Fb"
                  (host (update sample0 "a/b")))
      (test-error "invalid IP literal" (update sample0 "[2001:db8"))
      (test-error "invalid IP literal" (update sample0 "[::g]"))
      (test-error "update to nil in conflict with the path" (update sample5 #f))
      (test-error "update to non-nil in conflict with the path" (update sample6 "example.org"))
      (test-error "not a string" (update sample0 'host))
      (test-error "not a string" (update sample0 80)))))

;; Tests the setters for the port component
(define (port-tests kind parse authority port update)
  (test-group (string-append "set " kind " port")
    (let ((sample0 (parse "urn:/some/where/place"))
          (sample1 (parse "urn:/some/where/place"))
          (sample2 (parse "urn://:80"))
          (sample3 (parse "urn://user@:80"))
          (sample4 (parse "urn://example.org:80"))
          (sample5 (parse "urn://:80//some/where/place"))
          (sample6 (parse "urn:some/where/place")))
      (test-equal "persistence of update" '(8080 #f)
                  (list (port (update sample0 8080)) (port sample0)))
      (test-equal "nil authority, update port to non-nil" '(#f #f 80)
                  (authority-list authority (update sample0 80)))
      (test-equal "nil authority, update port to nil" '(#f #f #f)
                  (authority-list authority (update sample1 #f)))
      (test-equal "non-nil port, nil host/user, update port to nil" '(#f #f #f)
                  (authority-list authority (update sample2 #f)))
      (test-equal "non-nil port, non-nil user, nil host, update port to nil" '("user" #f #f)
                  (authority-list authority (update sample3 #f)))
      (test-equal "non-nil port, nil user, non-nil host, update port to nil" '(#f "example.org" #f)
                  (authority-list authority (update sample4 #f)))
      (test-equal "port 0" 0 (port (update sample0 0)))
      (test-equal "large port" 123456 (port (update sample0 123456)))
      (test-error "update to nil in conflict with the path" (update sample5 #f))
      (test-error "update to non-nil in conflict with the path" (update sample6 19))
      (test-error "negative port" (update sample0 -1))
      (test-error "inexact port" (update sample0 80.0))
      (test-error "rational port" (update sample0 1/2))
      (test-error "port as string" (update sample0 "80")))))

;; Tests the setter for all authority components at once
(define (authority-tests kind parse authority user host port update)
  (test-group (string-append "set " kind " authority")
    (let ((sample0 (parse "urn:/some/where/place"))
          (sample1 (parse "urn://u@h:1/some/where/place"))
          (sample2 (parse "urn://example.org//some/where/place"))
          (sample3 (parse "urn:some/where/place")))
      (test-equal "set all components" '("user" "host" 80)
                  (authority-list authority (update sample0 "user" "host" 80)))
      (test-equal "set no component" '(#f #f #f)
                  (authority-list authority (update sample0 #f #f #f)))
      (test-equal "unset all components" '(#f #f #f)
                  (authority-list authority (update sample1 #f #f #f)))
      (test-equal "replace all components" '("x" "y" 2)
                  (authority-list authority (update sample1 "x" "y" 2)))
      (test-equal "partial authority" '("user" #f #f)
                  (authority-list authority (update sample0 "user" #f #f)))
      (test-equal "partial authority" '(#f #f 80)
                  (authority-list authority (update sample0 #f #f 80)))
      (test-equal "persistence" '("u" "h" 1)
                  (authority-list authority sample1))
      (test-equal "components are encoded" '("a%40b" "c%2Fd" 1)
                  (authority-list authority (update sample1 "a@b" "c/d" 1)))
      (test-equal "path is retained" "/some/where/place"
                  (path->string (let ((x (update sample0 "user" "host" 80)))
                                  (if (string=? kind "IRI") (iri-path x) (uri-path x)))))
      (test-error "empty authority in conflict with the path" (update sample2 #f #f #f))
      (test-error "authority in conflict with the path" (update sample3 "user" "host" 80))
      (test-error "invalid user" (update sample0 'user "host" 80))
      (test-error "invalid host" (update sample0 "user" 1 80))
      (test-error "invalid port" (update sample0 "user" "host" -1))
      (test-error "invalid port" (update sample0 "user" "host" "80")))))

;; Tests the setters for the path component
(define (path-tests kind parse path update to-string)
  (test-group (string-append "set " kind " path")
    (let ((sample0 (parse "urn://example.org//some/where/place"))
          (sample1 (parse "urn://example.org//some/where/place"))
          (sample2 (parse "urn://example.org//some/where/place"))
          (sample3 (parse "urn:/some/where/place"))
          (sample4 (parse "urn:some/where/place"))
          (sample5 (parse "urn:"))
          (path-absolute1 (build-path (empty-absolute-path) "some" "where" "place"))
          (path-empty (empty-path))
          (path-absolute2 (build-path (empty-absolute-path) "some" "where" "else"))
          (path-noscheme (build-path "some" "where" "place"))
          (path-abempty (build-path (empty-absolute-path) "" "some" "where" "place")))
      (test-assert "persistence of update"
                   (and (not (equal? (update sample0 path-absolute2) sample0))
                        (path=? (path sample0) path-abempty)))
      (test-assert "authority set, path abempty: set to absolute"
                   (path=? path-absolute1 (path (update sample1 path-absolute1))))
      (test-assert "authority set, path abempty: set to empty"
                   (path=? path-empty (path (update sample0 path-empty))))
      (test-error "authority set, path abempty: set to noscheme" (update sample2 path-noscheme))
      (test-error "authority nil, path absolute: set to abempty" (update sample3 path-abempty))
      (test-error "authority nil, path noscheme: set to abempty" (update sample4 path-abempty))
      (test-error "authority nil, path empty: set to abempty" (update sample5 path-abempty))
      (test-equal "authority set: absolute path with an empty first segment" "urn://example.org//a"
                  (to-string (update sample0 (string->path "//a"))))
      (test-equal "authority nil: set to empty" "urn:" (to-string (update sample3 path-empty)))
      (test-equal "authority nil: set to the root" "urn:/" (to-string (update sample3 (empty-absolute-path))))
      (test-equal "authority nil: set to rootless path" "urn:a/b"
                  (to-string (update sample3 (string->path "a/b"))))
      (test-equal "authority nil: set to rootless path with colon" "urn:a:b/c"
                  (to-string (update sample3 (string->path "a:b/c"))))
      (test-error "not a path object" (update sample3 "/a/b"))
      (test-error "not a path object" (update sample3 '("a")))
      (test-error "not a path object" (update sample3 #f)))
    (let ((relative (parse "a/b"))
          (reference (parse "//example.org/a")))
      (test-error "relative reference: colon in the first segment" (update relative (string->path "a:b")))
      (test-equal "relative reference: dot prefix for a colon" "./a:b"
                  (to-string (update relative (string->path "./a:b"))))
      (test-error "relative reference: empty segment prefix" (update relative (string->path "//a")))
      (test-equal "relative reference with authority" "//example.org/b/c"
                  (to-string (update reference (string->path "/b/c"))))
      (test-error "relative reference with authority: noscheme path" (update reference (string->path "b/c"))))
    ;; characters which are not permitted in paths are encoded
    (let ((base (parse "urn:/")))
      (test-equal "segments of a relative path"
                  (if (string=? kind "IRI") "urn:crêpes/in/Rhône" "urn:cr%C3%AApes/in/Rh%C3%B4ne")
                  (to-string (update base (string->path "crêpes/in/Rhône"))))
      (test-equal "segments of an absolute path"
                  (if (string=? kind "IRI") "urn:/The/Köln/Concert" "urn:/The/K%C3%B6ln/Concert")
                  (to-string (update base (string->path "/The/Köln/Concert")))))))

;; Tests the setters for path segments
(define (segment-tests kind parse segment-ref segment-update to-string)
  (test-group (string-append "set " kind " path segment")
    (let ((sample6 (parse "urn://example.org//some/where/place"))
          (sample7 (parse "urn:/some/where/place"))
          (sample8 (parse "urn:some:thing/where/place"))
          (sample9 (parse "some/where/place")))
      (test-equal "authority set, path abempty: make it absolute by setting the first segment" "ini"
                  (segment-ref (segment-update sample6 0 "ini") 0))
      (test-error "authority nil, path absolute: set to abempty by unsetting the first segment"
                  (segment-update sample7 0 ""))
      (test-equal "authority nil, path rootless: make it noscheme by eliminating the colon" "something"
                  (segment-ref (segment-update sample8 0 "something") 0))
      (test-error "authority nil, path noscheme: make it rootless by adding a colon"
                  (segment-update sample9 0 "some:thing"))
      (test-equal "update of an inner segment" "urn:/some/there/place"
                  (to-string (segment-update sample7 1 "there")))
      (test-equal "update of the last segment" "urn:/some/where/else"
                  (to-string (segment-update sample7 2 "else")))
      (test-equal "slashes are encoded" "urn:/some/a%2Fb/place"
                  (to-string (segment-update sample7 1 "a/b")))
      (test-equal "segment reference" '("some" "where" "place")
                  (list (segment-ref sample7 0) (segment-ref sample7 1) (segment-ref sample7 2)))
      (test-equal "persistence" "some" (segment-ref sample7 0))
      (test-error "index out of range" (segment-ref sample7 3))
      (test-error "negative index" (segment-ref sample7 -1))
      (test-error "index out of range" (segment-update sample7 3 "x"))
      (test-error "index is not an integer" (segment-ref sample7 'a))
      (test-error "segment is not a string" (segment-update sample7 0 'a))
      (test-error "empty path" (segment-ref (parse "urn:") 0)))))

;; Tests the setters for the query and the fragment component
(define (query-tests kind parse query update to-string)
  (test-group (string-append "set " kind " query")
    (let ((sample0 (parse "urn://example.org/some/where/place"))
          (sample1 (parse "//example.org/some/where/place"))
          (sample2 (parse "http://example.org?query")))
      (test-assert "persistence of update"
                   (and (equal? "query" (query (update sample0 "query"))) (not (query sample0))))
      (test-equal "non-relative" "query" (query (update sample0 "query")))
      (test-equal "relative" "query" (query (update sample1 "query")))
      (test-equal "empty query" "urn://example.org/some/where/place?" (to-string (update sample0 "")))
      (test-equal "unset query" "urn://example.org/some/where/place" (to-string (update sample0 #f)))
      (test-equal "empty query of an identifier without path" "http://example.org?" (to-string (update sample2 "")))
      (test-equal "query with question mark" "a?b" (query (update sample0 "a?b")))
      (test-equal "query with slash" "a/b" (query (update sample0 "a/b")))
      (test-equal "hash is encoded" "a%23b" (query (update sample0 "a#b")))
      (test-equal "space is encoded" "a%20b" (query (update sample0 "a b")))
      (test-error "not a string" (update sample0 'query))
      (test-error "not a string" (update sample0 1)))))

(define (fragment-tests kind parse fragment update to-string)
  (test-group (string-append "set " kind " fragment")
    (let ((sample0 (parse "urn://example.org/some/where/place"))
          (sample1 (parse "//example.org/some/where/place"))
          (sample2 (parse "http://example.org?query")))
      (test-assert "persistence of update"
                   (and (equal? "fragment" (fragment (update sample0 "fragment"))) (not (fragment sample0))))
      (test-equal "non-relative" "fragment" (fragment (update sample0 "fragment")))
      (test-equal "relative" "fragment" (fragment (update sample1 "fragment")))
      (test-equal "empty fragment" "urn://example.org/some/where/place#" (to-string (update sample0 "")))
      (test-equal "unset fragment" "urn://example.org/some/where/place" (to-string (update sample0 #f)))
      (test-equal "empty fragment of an identifier with query" "http://example.org?query#"
                  (to-string (update sample2 "")))
      (test-equal "fragment with question mark" "a?b" (fragment (update sample0 "a?b")))
      (test-equal "hash is encoded" "a%23b" (fragment (update sample0 "a#b")))
      (test-equal "space is encoded" "a%20b" (fragment (update sample0 "a b")))
      (test-error "not a string" (update sample0 'fragment))
      (test-error "not a string" (update sample0 #t)))))

(test-begin "SRFI 275: Setters")

(user-tests "URI" string->uri uri-authority uri-user update-uri-user)
(user-tests "IRI" string->iri iri-authority iri-user update-iri-user)
(host-tests "URI" string->uri uri-authority uri-host update-uri-host)
(host-tests "IRI" string->iri iri-authority iri-host update-iri-host)
(port-tests "URI" string->uri uri-authority uri-port update-uri-port)
(port-tests "IRI" string->iri iri-authority iri-port update-iri-port)
(authority-tests "URI" string->uri uri-authority uri-user uri-host uri-port update-uri-authority)
(authority-tests "IRI" string->iri iri-authority iri-user iri-host iri-port update-iri-authority)
(path-tests "URI" string->uri uri-path update-uri-path uri->string)
(path-tests "IRI" string->iri iri-path update-iri-path iri->string)
(segment-tests "URI" string->uri uri-path-segment-ref uri-path-segment-update uri->string)
(segment-tests "IRI" string->iri iri-path-segment-ref iri-path-segment-update iri->string)
(query-tests "URI" string->uri uri-query update-uri-query uri->string)
(query-tests "IRI" string->iri iri-query update-iri-query iri->string)
(fragment-tests "URI" string->uri uri-fragment update-uri-fragment uri->string)
(fragment-tests "IRI" string->iri iri-fragment update-iri-fragment iri->string)

(test-group "Non-ASCII characters in IRI setters and URI setters"
  (define base-iri (string->iri "urn:/"))
  (define base-uri (string->uri "urn:/"))
  (test-equal "non-escape in IRI relative path" '("crêpes" "in" "Rhône")
              (path-segments (iri-path (update-iri-path base-iri (string->path "crêpes/in/Rhône")))))
  (test-equal "non-escape in IRI absolute path" '("crêpes" "in" "Rhône")
              (path-segments (iri-path (update-iri-path base-iri (string->path "/crêpes/in/Rhône")))))
  (test-equal "escape in URI relative path" '("cr%C3%AApes" "in" "Rh%C3%B4ne")
              (path-segments (uri-path (update-uri-path base-uri (string->path "crêpes/in/Rhône")))))
  (test-equal "escape in URI absolute path" '("The" "K%C3%B6ln" "Concert")
              (path-segments (uri-path (update-uri-path base-uri (string->path "/The/Köln/Concert")))))
  ;; U+202E (right-to-left override) is not permitted within an IRI
  (let ((input (list (string #\a (integer->char #x202E) #\b)
                     (string #\c (integer->char #x202E) #\d)
                     (string #\e (integer->char #x202E) #\f))))
    (test-equal "escape relative path non-IRI segment"
                '("a%E2%80%AEb" "c%E2%80%AEd" "e%E2%80%AEf")
                (path-segments (iri-path (update-iri-path base-iri (apply build-path (empty-path) input)))))
    (test-equal "escape absolute path non-IRI segment"
                '("a%E2%80%AEb" "c%E2%80%AEd" "e%E2%80%AEf")
                (path-segments (iri-path (update-iri-path base-iri
                                                          (apply build-path (empty-absolute-path) input)))))))

(test-group "Scheme setters"
  (define example0 (string->iri "http://example.org:80/ex#IRI"))
  (define example1 (string->iri "/some/where/place"))
  (test-equal "file://example.org:80/ex#IRI" (iri->string (update-iri-scheme example0 "file")))
  (test-equal "http://example.org:80/ex#IRI" (iri->string example0))
  (test-equal "file://example.org:80/ex#IRI"
              (uri->string (update-uri-scheme (string->uri "http://example.org:80/ex#IRI") "file")))
  (test-error (update-iri-scheme example0 #f))
  (test-error (update-iri-scheme example0 ""))
  (test-error (update-iri-scheme example0 "http%40a"))
  (test-error (update-iri-scheme example1 "file"))
  (test-error (update-iri-scheme example0 'file))
  (test-error (update-uri-scheme (string->iri "http://a") "file")))

(test-group "Examples of the specification"
  (define example0 (string->iri "urn:/some/where/place"))
  (define example1 (string->iri "http://example.org?query"))
  (define example2 (string->iri "urn://"))
  (define example3 (string->iri "urn://user@example.org"))
  (define example4 (string->iri "urn://example.org:80"))
  (define example5 (string->iri "urn://example.org//some/where/place"))
  (define example6 (string->iri "urn:some/where/place"))
  (test-equal "urn://example.org/some/where/place"
              (iri->string (update-iri-host example0 "example.org")))
  (test-equal "urn:/some/where/place" (iri->string (update-iri-host example0 #f)))
  (test-equal "urn:" (iri->string (update-iri-host example2 #f)))
  (test-equal "urn://user@" (iri->string (update-iri-host example3 #f)))
  (test-equal "urn://:80" (iri->string (update-iri-host example4 #f)))
  (test-error (update-iri-host example5 #f))
  (test-error (update-iri-host example6 "example.org"))
  (test-equal "urn:/some/where/place?query" (iri->string (update-iri-query example0 "query")))
  (test-equal "urn:/some/where/place?" (iri->string (update-iri-query example0 "")))
  (test-equal "urn:/some/where/place" (iri->string (update-iri-query example0 #f)))
  (test-equal "http://example.org?" (iri->string (update-iri-query example1 "")))
  (test-equal "urn:/some/where/place#nonquery" (iri->string (update-iri-fragment example0 "nonquery")))
  (test-equal "urn:/some/where/place#" (iri->string (update-iri-fragment example0 "")))
  (test-equal "urn:/some/where/place" (iri->string (update-iri-fragment example0 #f)))
  (test-equal "http://example.org?query#" (iri->string (update-iri-fragment example1 "")))
  (define example7 (string->iri "http://example.org/some/where/place"))
  (test-error (update-iri-path example0 (string->path "//some/where/place")))
  (test-equal "urn:" (iri->string (update-iri-path example0 (empty-path))))
  (test-equal "http://example.org//some/where/place"
              (iri->string (update-iri-path example7 (string->path "//some/where/place"))))
  (test-equal "http://example.org" (iri->string (update-iri-path example7 (empty-path))))
  (test-equal "urn:/some/where/else" (iri->string (update-iri-path example0 (string->path "/some/where/else"))))
  (test-equal "equality examples"
              '(#f #t #f #f)
              (let ((e0 (string->iri "/some/where/place"))
                    (e1 (string->iri "urn:/some/where/place"))
                    (e2 (string->iri "http://example.org"))
                    (e3 (string->iri "http://example.org:"))
                    (e4 (string->iri "http://example.org?a"))
                    (e5 (string->iri "http://example.org?")))
                (list (iri=? e0 e1) (iri=? e2 e3) (iri=? e4 e5) (iri=? e0 e1 e2 e3 e4 e5))))
  (test-assert (iri=? (empty-iri) (string->iri "") (empty-iri))))

(test-group "Settable predicates"
  (define (check-predicates parse)
    (let ((absolute (parse "urn:/some/where/place"))
          (authority-abempty (parse "urn://user@example.org//some/where/place"))
          (user-abempty (parse "urn://user@//some/where/place"))
          (host-abempty (parse "urn://example.org//some/where/place"))
          (port-abempty (parse "urn://:80//some/where/place"))
          (user-only (parse "urn://user@"))
          (port-only (parse "urn://:80"))
          (rootless (parse "urn:some/where/place"))
          (empty (parse "urn:"))
          (relative (parse "some/where/place"))
          (reference (parse "//example.org/a")))
      (test-equal "user-settable?" '(#t #t #t #f #t #t #t)
                  (list (user-settable? absolute "user")
                        (user-settable? absolute #f)
                        (user-settable? authority-abempty #f)
                        (user-settable? user-abempty #f)
                        (user-settable? user-only #f)
                        (user-settable? empty "user")
                        (user-settable? reference "user")))
      (test-equal "user-settable? of a rootless path" '(#f #t)
                  (list (user-settable? rootless "user") (user-settable? rootless #f)))
      (test-equal "host-settable?" '(#t #t #t #f #f #t)
                  (list (host-settable? absolute "example.org")
                        (host-settable? absolute #f)
                        (host-settable? authority-abempty #f)
                        (host-settable? host-abempty #f)
                        (host-settable? rootless "example.org")
                        (host-settable? empty "example.org")))
      (test-equal "port-settable?" '(#t #t #t #f #f)
                  (list (port-settable? absolute 80)
                        (port-settable? absolute #f)
                        (port-settable? host-abempty #f)
                        (port-settable? port-abempty #f)
                        (port-settable? rootless 80)))
      (test-equal "path-settable? with authority" '(#t #t #f #t)
                  (list (path-settable? authority-abempty (string->path "/a/b"))
                        (path-settable? authority-abempty (empty-path))
                        (path-settable? authority-abempty (string->path "a/b"))
                        (path-settable? authority-abempty (string->path "//a/b"))))
      (test-equal "path-settable? without authority" '(#t #t #f #t #f)
                  (list (path-settable? absolute (string->path "/a/b"))
                        (path-settable? absolute (empty-path))
                        (path-settable? absolute (string->path "//a/b"))
                        (path-settable? rootless (string->path "a:b"))
                        (path-settable? relative (string->path "a:b"))))
      (test-equal "path-settable? of a relative reference without authority" '(#t #f #t)
                  (list (path-settable? relative (string->path "a/b"))
                        (path-settable? relative (string->path "//e"))
                        (path-settable? relative (empty-path))))))
  (check-predicates string->iri)
  (check-predicates string->uri)
  (test-error "not an identifier" (user-settable? "urn:/a" "user"))
  (test-error "not an identifier" (host-settable? 'a "host"))
  (test-error "not an identifier" (port-settable? #f 80))
  (test-error "not an identifier" (path-settable? (empty-path) (empty-path))))

(test-end)
