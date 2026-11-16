;;; SRFI 275 NORMALIZATION REGRESSION TEST SUITE
;;;
;;; This is the test suite for the normalization and conversion procedures of library
;;; `(srfi 275 normalize)`. It is based on the test suite of the SRFI 275 sample
;;; implementation and on the examples of the SRFI document.
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
        (lispkit combinator)
        (lispkit test)
        (srfi 275 iri)
        (srfi 275 uri)
        (srfi 275 normalize))

;;; Test data of the sample implementation. Each row consists of a description, the text
;;; to normalize, the text of the expected result, and the normalization procedure.

(define uri-case-normalization
  `(;; Mixed-case scheme and hostname are rendered lower-case.
    ("All lower-case preserved"        "http://example.org/ex#test" "http://example.org/ex#test" ,normalize-uri-case)
    ("Mixed-case scheme to lower-case" "HttP://example.org/ex#test" "http://example.org/ex#test" ,normalize-uri-case)
    ("Mixed-case user preserved"       "http://MySelf@example.org/Examp#test" "http://MySelf@example.org/Examp#test" ,normalize-uri-case)
    ("Mixed-case host to lower-case"   "http://Example.ORG/ex#test"           "http://example.org/ex#test"           ,normalize-uri-case)
    ("Mixed case path preserved"       "http://example.org/Examp#test"        "http://example.org/Examp#test"        ,normalize-uri-case)
    ("Mixed case query preserved"      "http://example.org/examp?Qua#test"    "http://example.org/examp?Qua#test"    ,normalize-uri-case)
    ("Mixed case fragment preserved"   "http://example.org/examp#TeSt"        "http://example.org/examp#TeSt"        ,normalize-uri-case)
    ("User percent-encodings to upper-case"     "http://%2e@%2E%2E%2E/some/where/place" "http://%2E@%2E%2E%2E/some/where/place" ,normalize-uri-case)
    ("Host percent-encodings to upper-case"     "http://%2e%2E%2e/some/where/place"     "http://%2E%2E%2E/some/where/place"     ,normalize-uri-case)
    ("Path percent-encodings to upper-case"     "http://myname@example.org/%2e/%2E/%2e"       "http://myname@example.org/%2E/%2E/%2E"       ,normalize-uri-case)
    ("Query percent-encodings to upper-case"    "http://myname@example.org/%2E/%2E/%2E?%2e"   "http://myname@example.org/%2E/%2E/%2E?%2E"   ,normalize-uri-case)
    ("Fragment percent-encodings to upper-case" "http://myname@example.org/%2E/%2E/%2E#%2e"   "http://myname@example.org/%2E/%2E/%2E#%2E"   ,normalize-uri-case)))

(define uri-escaped-normalization
  `(;; Reserved character(s) in the clear are not escaped (!)
    ("User reserved character not escaped"     "http://my!name@example.org/ex#test"   "http://my!name@example.org/ex#test"   ,normalize-uri-escape)
    ("Host reserved character not escaped"     "http://myname@!example.org/ex#test"   "http://myname@!example.org/ex#test"   ,normalize-uri-escape)
    ("Path reserved character not escaped"     "http://myname@example.org/ex!#test"   "http://myname@example.org/ex!#test"   ,normalize-uri-escape)
    ("Query reserved character not escaped"    "http://myname@example.org/ex?!a#test" "http://myname@example.org/ex?!a#test" ,normalize-uri-escape)
    ("Fragment reserved character not escaped" "http://myname@example.org/ex?a#!test" "http://myname@example.org/ex?a#!test" ,normalize-uri-escape)
    ;; Reserved character(s) escaped are not decoded (@)
    ("User reserved escape not decoded"     "http://my%40name@example.org/ex#test"   "http://my%40name@example.org/ex#test"   ,normalize-uri-escape)
    ("Host reserved escape not decoded"     "http://myname@ex%40ample.org/ex#test"   "http://myname@ex%40ample.org/ex#test"   ,normalize-uri-escape)
    ("Path reserved escape not decoded"     "http://myname@example.org/e%40x#test"   "http://myname@example.org/e%40x#test"   ,normalize-uri-escape)
    ("Query reserved escape not decoded"    "http://myname@example.org/ex?a%40#test" "http://myname@example.org/ex?a%40#test" ,normalize-uri-escape)
    ("Fragment reserved escape not decoded" "http://myname@example.org/ex?a#t%40est" "http://myname@example.org/ex?a#t%40est" ,normalize-uri-escape)
    ;; Other permissible character(s) escaped are decoded
    ("User permissible escape decoded"     "http://my%2Ename@example.org/ex?a#test"        "http://my.name@example.org/ex?a#test"      ,normalize-uri-escape)
    ("Host permissible escape decoded"     "http://myname@example%2Eorg/ex?a#test"         "http://myname@example.org/ex?a#test"       ,normalize-uri-escape)
    ("Path permissible escape decoded"     "http://myname@example.org/misc%2Etxt#test"     "http://myname@example.org/misc.txt#test"   ,normalize-uri-escape)
    ("Query permissible escape decoded"    "http://myname@example.org/misc.txt?%2E%2E%2E"  "http://myname@example.org/misc.txt?..."    ,normalize-uri-escape)
    ("Fragment permissible escape decoded" "http://myname@example.org/misc.txt#line%31%30" "http://myname@example.org/misc.txt#line10" ,normalize-uri-escape)))

(define uri-escaped-normalization/ge
  `(;; Illegal characters are escaped
    ("User illegal characters are escaped" "http://dosh£@crepes.example.org"      "http://dosh%C2%A3@crepes.example.org"     ,normalize-uri-escape)
    ("Host illegal characters are escaped" "http://crêpes.example.org"            "http://cr%C3%AApes.example.org"           ,normalize-uri-escape)
    ("Path illegal characters are escaped" "http://crepes.example.org/in/Rhône"  "http://crepes.example.org/in/Rh%C3%B4ne" ,normalize-uri-escape)
    ("Query illegal characters are escaped" "http://crepes.example.org/in/Rennes?Dim.‥Sam."  "http://crepes.example.org/in/Rennes?Dim.%E2%80%A5Sam." ,normalize-uri-escape)
    ("Fragment illegal characters are escaped" "http://crepes.example.org/in/Rennes#L'Étage"  "http://crepes.example.org/in/Rennes#L'%C3%89tage" ,normalize-uri-escape)))

(define uri-path-normalization
  `(;; Path without dotted segments is unchanged
    ("Absolute path without dotted segments unchanged"  "http://example.org/some/where/place" "http://example.org/some/where/place" ,normalize-uri-path)
    ("No authority absolute path without dotted segments unchanged"   "urn:/some/where/place" "urn:/some/where/place" ,normalize-uri-path)
    ("Relative path without dotted segments unchanged"                "urn:some/where/place"  "urn:some/where/place"  ,normalize-uri-path)
    ;; Single-dot segments only eliminates the dotted segments
    ("Absolute path eliminates single dotted segments"  "urn:/some/./where/././place/./"  "urn:/some/where/place/" ,normalize-uri-path)
    ("Relative path eliminates single dotted segments"  "urn:some/./where/././place/./"   "urn:some/where/place/"  ,normalize-uri-path)
    ;; Empty segments are treated identically to non-empty segments
    ("Absolute path empty segments treated like non-empty"  "urn:/some//where//place//" "urn:/some//where//place//" ,normalize-uri-path)
    ("Relative path empty segments treated like non-empty"  "urn:some//where//place//"  "urn:some//where//place//"  ,normalize-uri-path)
    ;; Relative references are not path normalized
    ("Single leading slash not normalized"      "/"  "/"   ,normalize-uri-path)
    ("Multiple leading slashes not normalized"  "//" "//"  ,normalize-uri-path)
    ("Absolute path reference not normalized (double-dot)"    "/a/b/../../c"    "/a/b/../../c"    ,normalize-uri-path)
    ("Absolute path reference not normalized (single-dot)"    "/a/b/././c"      "/a/b/././c"      ,normalize-uri-path)
    ("Absolute path reference not normalized (mixed dotted)"  "/a/b/../c/././d" "/a/b/../c/././d" ,normalize-uri-path)
    ("Relative path reference not normalized (double-dot)"    "a/b/../../c"     "a/b/../../c"     ,normalize-uri-path)
    ("Relative path reference not normalized (single-dot)"    "a/b/././c"       "a/b/././c"       ,normalize-uri-path)
    ("Relative path reference not normalized (mixed dotted)"  "a/b/../c/././d"  "a/b/../c/././d"  ,normalize-uri-path)
    ("1-segment path reference with leading dot not normalized"         "./def"     "./def"     ,normalize-uri-path)
    ("1-segment path reference with leading dot not normalized (colon)" "./abc:def" "./abc:def" ,normalize-uri-path)
    ("Additional relative path case not normalized"  "../../abc/./def" "../../abc/./def" ,normalize-uri-path)
    ;; Normalisation may not preserve relative path, in a non-relative URI
    ("Relative path must not be normalized to an absolute path" "foo:a/b/../.././../../e" "foo:e" ,normalize-uri-path)
    ;; Additional test cases from Webkit
    ("Empty segments eliminated like non-empty (1)"  "http://example.com////../.."       "http://example.com//"        ,normalize-uri-path)
    ("Empty segments eliminated like non-empty (2)"  "http://example.com/foo/bar//../.." "http://example.com/foo/"     ,normalize-uri-path)
    ("Empty segments eliminated like non-empty (3)"  "http://example.com/foo/bar//.."    "http://example.com/foo/bar/" ,normalize-uri-path)
    ;; From Haskell network-uri
    ("General test cases (1)"  "http://example/a/b/../../c"  "http://example/c"      ,normalize-uri-path)
    ("General test cases (2)"  "http://example/a/b/c/../../" "http://example/a/"     ,normalize-uri-path)
    ("General test cases (3)"  "http://example/a/b/c/./"     "http://example/a/b/c/" ,normalize-uri-path)
    ("General test cases (4)"  "http://example/a/b/c/.././"  "http://example/a/b/"   ,normalize-uri-path)
    ("General test cases (5)"  "http://example/a/b/c/d/../../../../e"   "http://example/e" ,normalize-uri-path)
    ("General test cases (6)"  "http://example/a/b/c/d/../.././../../e" "http://example/e" ,normalize-uri-path)
    ("General test cases (7)"  "http://example/a/b/../.././../../e"     "http://example/e" ,normalize-uri-path)))

(define iri-case-normalization/extra
  `(;; Only U.S. ASCII has a canonical lower-case representation
    ("Host non-U.S. ASCII is case-sensitive" "http://CRÊPES.example.org" "http://crÊpes.example.org" ,normalize-iri-case)))

(define iri-escaped-normalization/extra
  `(;; Other permissible character(s) escaped are decoded
    ("User permissible escape decoded"   "http://dosh%C2%A3@crepes.example.org" "http://dosh£@crepes.example.org" ,normalize-iri-escape)
    ("Host permissible escape decoded"    "http://cr%C3%AApes.example.org"      "http://crêpes.example.org"       ,normalize-iri-escape)
    ("Path permissible characters are decoded" "http://crepes.example.org/in/Rennes/Cr%C3%AAperie" "http://crepes.example.org/in/Rennes/Crêperie"   ,normalize-uri-escape)
    ("Query permissible characters are decoded" "http://crepes.example.org/in/Rennes?Dim.%E2%80%A5Sam." "http://crepes.example.org/in/Rennes?Dim.‥Sam."   ,normalize-iri-escape)
    ("Fragment permissible characters are decoded" "http://crepes.example.org/in/Rennes#L'%C3%89tage" "http://crepes.example.org/in/Rennes#L'Étage"   ,normalize-iri-escape)
    ("Wholly escaped wholly decoded"        "https://en.wiktionary.org/wiki/%E1%BF%AC%CF%8C%CE%B4%CE%BF%CF%82" "https://en.wiktionary.org/wiki/Ῥόδος" ,normalize-iri-escape)
    ("Partially normalized wholly decoded"  "https://example.org/ceol/%C3%89irigh'sCuirOrtDoChuid%C3%89adaigh" "https://example.org/ceol/Éirigh'sCuirOrtDoChuidÉadaigh" ,normalize-iri-escape)
    ("Wholly normalized not reencoded"       "https://en.wiktionary.org/wiki/Ῥόδος" "https://en.wiktionary.org/wiki/Ῥόδος" ,normalize-iri-escape)
    ("Repeated normalization is indepodent" "https://en.wiktionary.org/wiki/Ῥόδος"  "https://en.wiktionary.org/wiki/Ῥόδος" ,(compose normalize-iri-escape normalize-iri-escape))))

(define uri-to-iri-normalization
  `(("Wholly escaped path normalized" "https://en.wiktionary.org/wiki/%E1%BF%AC%CF%8C%CE%B4%CE%BF%CF%82" "https://en.wiktionary.org/wiki/Ῥόδος")
    ("Partially escaped path normalized" "https://example.org/ceol/%C3%89irigh'sCuirOrtDoChuid%C3%89adaigh" "https://example.org/ceol/Éirigh'sCuirOrtDoChuidÉadaigh")))

(define iri-to-uri-normalization
  `(("Wholly escapable path escaped" "https://en.wiktionary.org/wiki/Ῥόδος" "https://en.wiktionary.org/wiki/%E1%BF%AC%CF%8C%CE%B4%CE%BF%CF%82")
    ("Partialy escapable path escaped" "https://example.org/ceol/Éirigh'sCuirOrtDoChuidÉadaigh" "https://example.org/ceol/%C3%89irigh'sCuirOrtDoChuid%C3%89adaigh")))


;; The test data for IRIs which is identical to the test data for URIs
(define (uri-rows->iri-rows norm rows)
  (map (lambda (row) (list (string-append (car row) " (IRI)") (cadr row) (caddr row) norm))
       rows))

(define iri-case-normalization
  (uri-rows->iri-rows normalize-iri-case uri-case-normalization))

(define iri-escaped-normalization
  (uri-rows->iri-rows normalize-iri-escape uri-escaped-normalization))

(define iri-path-normalization
  (uri-rows->iri-rows normalize-iri-path uri-path-normalization))

;; One of the IRI rows of the sample implementation uses the procedure for URIs
(define (fix-iri-row row)
  (if (eq? (cadddr row) normalize-uri-escape)
      (list (car row) (cadr row) (caddr row) normalize-iri-escape)
      row))

;; Normalizes the text of each row, and compares the result with the expected result
(define (check-rows rows parse unparse)
  (for-each (lambda (row)
              (let ((norm (cadddr row)))
                (test-equal (car row)
                            (unparse (parse (caddr row)))
                            (unparse (norm (parse (cadr row)))))))
            rows))

(define (check-iri-rows rows) (check-rows rows string->iri iri->string))
(define (check-uri-rows rows) (check-rows rows string->uri uri->string))

(test-begin "SRFI 275: Normalization")

(test-group "URI case normalization"
  (check-uri-rows uri-case-normalization))

(test-group "IRI case normalization"
  (check-iri-rows iri-case-normalization)
  (check-iri-rows iri-case-normalization/extra))

(test-group "URI escape normalization"
  (check-uri-rows uri-escaped-normalization)
  ;; Characters which are not permitted in a URI are escaped by the conversion of an IRI
  (for-each (lambda (row)
              (test-equal (car row)
                          (uri->string (string->uri (caddr row)))
                          (uri->string (iri->uri (string->iri (cadr row))))))
            uri-escaped-normalization/ge))

(test-group "IRI escape normalization"
  (check-iri-rows iri-escaped-normalization)
  (check-iri-rows (map fix-iri-row iri-escaped-normalization/extra)))

(test-group "URI path normalization"
  (check-uri-rows uri-path-normalization))

(test-group "IRI path normalization"
  (check-iri-rows iri-path-normalization))

(test-group "Conversion from URIs to IRIs"
  (for-each (lambda (row)
              (let ((result (uri->iri (string->uri (cadr row)))))
                (test-equal (car row) (iri->string (string->iri (caddr row))) (iri->string result))
                (test-assert (string-append (car row) " (type)") (iri? result))))
            uri-to-iri-normalization))

(test-group "Conversion from IRIs to URIs"
  (for-each (lambda (row)
              (let ((result (iri->uri (string->iri (cadr row)))))
                (test-equal (car row) (uri->string (string->uri (caddr row))) (uri->string result))
                (test-assert (string-append (car row) " (type)") (uri? result))))
            iri-to-uri-normalization))

(test-group "Examples of the specification"
  (define (norm op parse unparse text) (unparse (op (parse text))))
  (define (iri-norm op text) (norm op string->iri iri->string text))
  (define (uri-norm op text) (norm op string->uri uri->string text))
  (test-equal "http://example.org/ex#test" (iri-norm normalize-iri-case "HttP://example.org/ex#test"))
  (test-equal "http://example.org/ex#test" (iri-norm normalize-iri-case "http://Example.ORG/ex#test"))
  (test-equal "http://crÊpes.example.org" (iri-norm normalize-iri-case "http://CRÊPES.example.org"))
  (test-equal "http://USER@example.org/Some/Where/Place?Query#test"
              (iri-norm normalize-iri-case "http://USER@example.org/Some/Where/Place?Query#test"))
  (test-equal "http://crepes.example.org/in/Rennes?Dim.‥Sam."
              (iri-norm normalize-iri-escape "http://crepes.example.org/in/Rennes?Dim.%E2%80%A5Sam."))
  (test-equal "https://en.wiktionary.org/wiki/Ῥόδος"
              (iri-norm normalize-iri-escape
                        "https://en.wiktionary.org/wiki/%E1%BF%AC%CF%8C%CE%B4%CE%BF%CF%82"))
  (test-equal "http://my%40name@example.org/ex#test"
              (iri-norm normalize-iri-escape "http://my%40name@example.org/ex#test"))
  (test-equal "http://dosh%C2%A3@crepes.example.org"
              (uri->string (iri->uri (string->iri "http://dosh£@crepes.example.org"))))
  (define iri0 (string->iri "https://en.wiktionary.org/wiki/Ῥόδος"))
  (test-assert (uri? (iri->uri iri0)))
  (test-equal "https://en.wiktionary.org/wiki/%E1%BF%AC%CF%8C%CE%B4%CE%BF%CF%82"
              (uri->string (iri->uri iri0)))
  (define iri1 (string->iri "https://example.org/ceol/Éirigh'sCuirOrtDoChuidÉadaigh"))
  (test-equal "https://example.org/ceol/%C3%89irigh'sCuirOrtDoChuid%C3%89adaigh"
              (uri->string (iri->uri iri1)))
  (define uri0 (string->uri "https://en.wiktionary.org/wiki/%E1%BF%AC%CF%8C%CE%B4%CE%BF%CF%82"))
  (test-assert (iri? (uri->iri uri0)))
  (test-equal "https://en.wiktionary.org/wiki/Ῥόδος" (iri->string (uri->iri uri0))))

(test-group "Escape normalization"
  (define (iri-escape text) (iri->string (normalize-iri-escape (string->iri text))))
  (define (uri-escape text) (uri->string (normalize-uri-escape (string->uri text))))
  (test-equal "unreserved characters are decoded" "http://a/b-c.d_e~f"
              (uri-escape "http://a/b%2Dc%2Ed%5Fe%7Ef"))
  (test-equal "alphanumeric characters are decoded" "http://a/Ab9" (uri-escape "http://a/%41%62%39"))
  (test-equal "sub-delimiters are not decoded" "http://a/%21%24%26%27%28%29%2A%2B%2C%3B%3D"
              (uri-escape "http://a/%21%24%26%27%28%29%2A%2B%2C%3B%3D"))
  (test-equal "gen-delimiters are not decoded" "http://a/%3A%2F%3F%23%5B%5D%40"
              (uri-escape "http://a/%3A%2F%3F%23%5B%5D%40"))
  (test-equal "lowercase hexadecimal digits are normalized" "http://a/%2F%3A"
              (uri-escape "http://a/%2f%3a"))
  (test-equal "slash in a path segment" "http://a/b%2Fc" (uri-escape "http://a/b%2Fc"))
  (test-equal "a percent-encoded percent sign is retained" "http://a/b%25c" (uri-escape "http://a/b%25c"))
  (test-equal "a percent-encoded space is retained" "http://a/b%20c" (uri-escape "http://a/b%20c"))
  (test-equal "percent-encoded non-ASCII characters are retained in URIs" "http://a/%C3%A9"
              (uri-escape "http://a/%c3%a9"))
  (test-equal "percent-encoded non-ASCII characters are decoded in IRIs" "http://a/é"
              (iri-escape "http://a/%c3%a9"))
  (test-equal "percent-encoded supplementary characters are decoded in IRIs" "http://a/😎"
              (iri-escape "http://a/%F0%9F%98%8E"))
  (test-equal "percent-encoded private use characters in the query" "http://a/?\xE000;"
              (iri-escape "http://a/?%EE%80%80"))
  (test-equal "percent-encoded private use characters in the path are retained" "http://a/%EE%80%80"
              (iri-escape "http://a/%EE%80%80"))
  (test-equal "percent-encoded bidirectional control characters are retained" "http://a/%E2%80%8E"
              (iri-escape "http://a/%E2%80%8E"))
  (test-equal "percent-encoded spaces are retained in IRIs" "http://a/b%20c" (iri-escape "http://a/b%20c"))
  (test-equal "IP literals are not changed" "http://[::1]:80/~foo" (uri-escape "http://[::1]:80/%7Efoo"))
  (test-equal "IP literals are retained" "http://[v1.a:b]/" (uri-escape "http://[v1.a:b]/"))
  (test-equal "idempotence"
              (iri-escape "http://a/%c3%a9%41?%e2%80%a5#%F0%9F%98%8E")
              (iri-escape (iri-escape "http://a/%c3%a9%41?%e2%80%a5#%F0%9F%98%8E"))))

(test-group "Case normalization"
  (define (iri-case text) (iri->string (normalize-iri-case (string->iri text))))
  (test-equal "scheme and host" "http://example.org/Path" (iri-case "HTTP://EXAMPLE.ORG/Path"))
  (test-equal "scheme with symbols" "a+b-c.d:x" (iri-case "A+B-C.D:x"))
  (test-equal "relative references have no scheme" "//example.org/Path" (iri-case "//EXAMPLE.org/Path"))
  (test-equal "IP literals are lowercased as well" "http://[abcd::ef]/" (iri-case "http://[ABCD::EF]/"))
  (test-equal "percent-encoded bytes in the host are not lowercased" "http://%C3%A9.example/"
              (iri-case "http://%c3%a9.EXAMPLE/"))
  (test-equal "only ASCII letters are lowercased" "http://crÊpes.example.org/"
              (iri-case "http://CRÊPES.Example.ORG/"))
  (test-equal "no host" "urn:/Some/Where" (iri-case "URN:/Some/Where"))
  (test-equal "empty host" "file:///Some" (iri-case "FILE:///Some"))
  (test-equal "idempotence" (iri-case "HTTP://A.B/c") (iri-case (iri-case "HTTP://A.B/c"))))

(test-group "Path normalization"
  (define (path text) (iri->string (normalize-iri-path (string->iri text))))
  (test-equal "http://a/b/g" (path "http://a/b/c/./../g"))
  (test-equal "dot segments at the end" "http://a/b/" (path "http://a/b/c/.."))
  (test-equal "single dot at the end" "http://a/b/c/" (path "http://a/b/c/."))
  (test-equal "excess double dots" "http://a/g" (path "http://a/../../g"))
  (test-equal "excess double dots at the end" "http://a/" (path "http://a/../.."))
  (test-equal "empty path" "http://a" (path "http://a"))
  (test-equal "query and fragment are retained" "http://a/b?x/../y#z/./w" (path "http://a/c/../b?x/../y#z/./w"))
  (test-equal "percent-encoded dots are not dot segments" "http://a/b/%2E%2E/c" (path "http://a/b/%2E%2E/c"))
  (test-equal "relative references are not normalized" "../a/./b/../c" (path "../a/./b/../c"))
  (test-equal "the empty relative reference" "" (path ""))
  (test-equal "rootless path" "urn:b" (path "urn:a/../b"))
  (test-equal "rootless path becomes empty" "urn:" (path "urn:a/.."))
  (test-equal "path with authority" "http://a//b" (path "http://a//b"))
  (test-equal "path which looks like an authority" "foo:.///g" (path "foo:/.//.././/g"))
  (test-equal "idempotence" (path (path "http://a/b/c/../../d")) (path "http://a/b/c/../../d")))

(test-group "Full normalization"
  (define (iri-full text) (iri->string (normalize-iri (string->iri text))))
  (define (uri-full text) (uri->string (normalize-uri (string->uri text))))
  (test-equal "http://example.org/a/c?q#f"
              (uri-full "HTTP://Example.ORG/%61/b/../c?q#f"))
  (test-equal "http://example.org/é/c"
              (iri-full "HTTP://Example.ORG/%C3%A9/b/../c"))
  (test-equal "http://example.org/%C3%A9/c"
              (uri-full "HTTP://Example.ORG/%c3%a9/b/../c"))
  (test-equal "http://crÊpes.example/é"
              (iri-full "http://CRÊPES.example/%C3%A9"))
  (test-equal "relative references are not path-normalized" "../a/./b/c"
              (uri-full "../a/./b/%63"))
  (test-equal "composition of the three normalizations"
              (iri->string (normalize-iri-path (normalize-iri-case (normalize-iri-escape
                                                 (string->iri "HTTP://A.B/%7E/c/../d")))))
              (iri-full "HTTP://A.B/%7E/c/../d"))
  (test-equal "normalization is idempotent"
              (iri-full "HTTP://A.B/%7E/c/../d?x=%41#%42")
              (iri-full (iri-full "HTTP://A.B/%7E/c/../d?x=%41#%42")))
  (test-assert "normalized IRIs are equal"
               (iri=? (normalize-iri (string->iri "HTTP://A.B/%7Ec/./d"))
                      (normalize-iri (string->iri "http://a.b/~c/d"))))
  (test-assert "normalized URIs are equal"
               (uri=? (normalize-uri (string->uri "HTTP://A.B/%7Ec/./d"))
                      (normalize-uri (string->uri "http://a.b/~c/d")))))

(test-group "Conversion between IRIs and URIs"
  (define (to-uri text) (uri->string (iri->uri (string->iri text))))
  (define (to-iri text) (iri->string (uri->iri (string->uri text))))
  (test-equal "relative references are converted to relative references"
              "//example.org/%C3%A9?%C3%A9#%C3%A9" (to-uri "//example.org/é?é#é"))
  (test-equal "relative IRI stays relative" #t (relative-uri? (iri->uri (string->iri "/é"))))
  (test-equal "relative URI stays relative" #t (relative-iri? (uri->iri (string->uri "/%C3%A9"))))
  (test-equal "non-relative IRI stays non-relative" #t (non-relative-uri? (iri->uri (string->iri "a:é"))))
  (test-equal "private use characters" "http://a/?%EE%80%80" (to-uri "http://a/?\xE000;"))
  (test-equal "supplementary characters" "http://a/%F0%9F%98%8E" (to-uri "http://a/😎"))
  (test-equal "host" "http://cr%C3%AApes.example/" (to-uri "http://crêpes.example/"))
  (test-equal "user" "http://j%C3%B6rg@example/" (to-uri "http://jörg@example/"))
  (test-equal "the conversion normalizes" "http://example.org/a/" (to-uri "HTTP://Example.ORG/%61/b/.."))
  (test-equal "a URI converted to an IRI is decoded" "http://a/é?é#é" (to-iri "http://a/%C3%A9?%C3%A9#%C3%A9"))
  (test-equal "bidirectional control characters stay encoded" "http://a/%E2%80%8E"
              (to-iri "http://a/%E2%80%8E"))
  (test-equal "reserved characters stay encoded" "http://a/%2F%3F" (to-iri "http://a/%2F%3F"))
  (test-equal "round trip" "http://a/é" (to-iri (to-uri "http://a/é")))
  (test-assert "IRI to URI yields a URI" (uri? (iri->uri (string->iri "http://a/é"))))
  (test-assert "URI to IRI yields an IRI" (iri? (uri->iri (string->uri "http://a/b"))))
  (test-assert "the IRI of a URI is not a URI" (not (uri? (uri->iri (string->uri "http://a/b")))))
  (test-error "not an IRI" (iri->uri (string->uri "http://a/b")))
  (test-error "not a URI" (uri->iri (string->iri "http://a/b")))
  (test-error "not an IRI" (iri->uri "http://a/b"))
  (test-error "not a URI" (uri->iri #f)))

(test-group "Argument checks"
  (test-error (normalize-iri-case (string->uri "http://a/b")))
  (test-error (normalize-iri-escape (string->uri "http://a/b")))
  (test-error (normalize-iri-path (string->uri "http://a/b")))
  (test-error (normalize-iri (string->uri "http://a/b")))
  (test-error (normalize-uri-case (string->iri "http://a/b")))
  (test-error (normalize-uri-escape (string->iri "http://a/b")))
  (test-error (normalize-uri-path (string->iri "http://a/b")))
  (test-error (normalize-uri (string->iri "http://a/b")))
  (test-error (normalize-iri "http://a/b"))
  (test-error (normalize-uri 'a)))

(test-end)
