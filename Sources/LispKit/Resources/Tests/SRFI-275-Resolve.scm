;;; SRFI 275 RESOLUTION REGRESSION TEST SUITE
;;;
;;; This is the test suite for the relative reference resolution procedures
;;; `resolve-iri`, `resolve-uri`, `relativize-iri` and `relativize-uri` of library
;;; `(srfi 275 normalize)`. It is based on the test suite of the SRFI 275 sample
;;; implementation, which in turn collects test cases from the RDF/Turtle test suite, the
;;; Haskell library `network-uri`, and Chicken's `uri-generic`, as well as on the examples
;;; of the SRFI document.
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
        (srfi 275 normalize))

;;; Test data of the sample implementation

(define turtle-common-cases
  '("g:h"
    "g"
    "./g"
    "g/"
    "/g"
    "//g"
    "?y"
    "g?y"
    "#s"
    "g#s"
    "g?y#s"
    ";x"
    "g;x"
    "g;x?y#s"
    ""
    "."
    "./"
    ".."
    "../"
    "../g"
    "../.."
    "../../"
    "../../g"
    "../../../g"
    "../../../../g"
    "/./g"
    "/../g"
    "g."
    ".g"
    "g.."
    "..g"
    "./../g"
    "./g/."
    "g/./h"
    "g/../h"
    "g;x=1/./y"
    "g;x=1/../y"
    "g?y/./x"
    "g?y/../x"
    "g#s/./x"
    "g#s/../x"))

(define turtle-base01 (string->iri"http://a/bb/ccc/d;p?q"))

(define turtle-expected01
  '("g:h"
    "http://a/bb/ccc/g"
    "http://a/bb/ccc/g"
    "http://a/bb/ccc/g/"
    "http://a/g"
    "http://g"
    "http://a/bb/ccc/d;p?y"
    "http://a/bb/ccc/g?y"
    "http://a/bb/ccc/d;p?q#s"
    "http://a/bb/ccc/g#s"
    "http://a/bb/ccc/g?y#s"
    "http://a/bb/ccc/;x"
    "http://a/bb/ccc/g;x"
    "http://a/bb/ccc/g;x?y#s"
    "http://a/bb/ccc/d;p?q"
    "http://a/bb/ccc/"
    "http://a/bb/ccc/"
    "http://a/bb/"
    "http://a/bb/"
    "http://a/bb/g"
    "http://a/"
    "http://a/"
    "http://a/g"
    "http://a/g"
    "http://a/g"
    "http://a/g"
    "http://a/g"
    "http://a/bb/ccc/g."
    "http://a/bb/ccc/.g"
    "http://a/bb/ccc/g.."
    "http://a/bb/ccc/..g"
    "http://a/bb/g"
    "http://a/bb/ccc/g/"
    "http://a/bb/ccc/g/h"
    "http://a/bb/ccc/h"
    "http://a/bb/ccc/g;x=1/y"
    "http://a/bb/ccc/y"
    "http://a/bb/ccc/g?y/./x"
    "http://a/bb/ccc/g?y/../x"
    "http://a/bb/ccc/g#s/./x"
    "http://a/bb/ccc/g#s/../x"))

(define turtle-base02 (string->iri"http://a/bb/ccc/d/"))

(define turtle-expected02
  '("g:h"
    "http://a/bb/ccc/d/g"
    "http://a/bb/ccc/d/g"
    "http://a/bb/ccc/d/g/"
    "http://a/g"
    "http://g"
    "http://a/bb/ccc/d/?y"
    "http://a/bb/ccc/d/g?y"
    "http://a/bb/ccc/d/#s"
    "http://a/bb/ccc/d/g#s"
    "http://a/bb/ccc/d/g?y#s"
    "http://a/bb/ccc/d/;x"
    "http://a/bb/ccc/d/g;x"
    "http://a/bb/ccc/d/g;x?y#s"
    "http://a/bb/ccc/d/"
    "http://a/bb/ccc/d/"
    "http://a/bb/ccc/d/"
    "http://a/bb/ccc/"
    "http://a/bb/ccc/"
    "http://a/bb/ccc/g"
    "http://a/bb/"
    "http://a/bb/"
    "http://a/bb/g"
    "http://a/g"
    "http://a/g"
    "http://a/g"
    "http://a/g"
    "http://a/bb/ccc/d/g."
    "http://a/bb/ccc/d/.g"
    "http://a/bb/ccc/d/g.."
    "http://a/bb/ccc/d/..g"
    "http://a/bb/ccc/g"
    "http://a/bb/ccc/d/g/"
    "http://a/bb/ccc/d/g/h"
    "http://a/bb/ccc/d/h"
    "http://a/bb/ccc/d/g;x=1/y"
    "http://a/bb/ccc/d/y"
    "http://a/bb/ccc/d/g?y/./x"
    "http://a/bb/ccc/d/g?y/../x"
    "http://a/bb/ccc/d/g#s/./x"
    "http://a/bb/ccc/d/g#s/../x"))

(define turtle-cases07 (append turtle-common-cases '("http:g")))

(define turtle-base07 (string->iri"file:///a/bb/ccc/d;p?q"))

(define turtle-expected07
  '("g:h"
    "file:///a/bb/ccc/g"
    "file:///a/bb/ccc/g"
    "file:///a/bb/ccc/g/"
    "file:///g"
    "file://g"
    "file:///a/bb/ccc/d;p?y"
    "file:///a/bb/ccc/g?y"
    "file:///a/bb/ccc/d;p?q#s"
    "file:///a/bb/ccc/g#s"
    "file:///a/bb/ccc/g?y#s"
    "file:///a/bb/ccc/;x"
    "file:///a/bb/ccc/g;x"
    "file:///a/bb/ccc/g;x?y#s"
    "file:///a/bb/ccc/d;p?q"
    "file:///a/bb/ccc/"
    "file:///a/bb/ccc/"
    "file:///a/bb/"
    "file:///a/bb/"
    "file:///a/bb/g"
    "file:///a/"
    "file:///a/"
    "file:///a/g"
    "file:///g"
    "file:///g"
    "file:///g"
    "file:///g"
    "file:///a/bb/ccc/g."
    "file:///a/bb/ccc/.g"
    "file:///a/bb/ccc/g.."
    "file:///a/bb/ccc/..g"
    "file:///a/bb/g"
    "file:///a/bb/ccc/g/"
    "file:///a/bb/ccc/g/h"
    "file:///a/bb/ccc/h"
    "file:///a/bb/ccc/g;x=1/y"
    "file:///a/bb/ccc/y"
    "file:///a/bb/ccc/g?y/./x"
    "file:///a/bb/ccc/g?y/../x"
    "file:///a/bb/ccc/g#s/./x"
    "file:///a/bb/ccc/g#s/../x"
    "http:g"))

(define turtle-base08a (string->iri"http://abc/def/ghi"))

(define turtle-cases08a '("."
                   ".?a=b"
                   ".#a=b"
                   ".."
                   "..?a=b"
                   "..#a=b"))

(define turtle-expected08a '("http://abc/def/"
                      "http://abc/def/?a=b"
                      "http://abc/def/#a=b"
                      "http://abc/"
                      "http://abc/?a=b"
                      "http://abc/#a=b"))

(define turtle-base08b (string->iri"http://ab//de//ghi"))

(define turtle-cases08b '("xyz" "./xyz" "../xyz"))

(define turtle-expected08b '("http://ab//de//xyz"
                      "http://ab//de//xyz"
                      "http://ab//de/xyz"))

(define turtle-base08c (string->iri"http://abc/d:f/ghi"))

(define turtle-cases08c '("xyz"
                   "./xyz"
                   "../xyz"))

(define turtle-expected08c '("http://abc/d:f/xyz"
                      "http://abc/d:f/xyz"
                      "http://abc/xyz"))

(define swap-cases
  '(;; test cases from Python URI implementation
    ("foo:xyz" "bar:abc" "bar:abc")
    ("http://example/x/y/z" "../abc" "http://example/x/abc")
    ("http://example2/x/y/z" "//example/x/abc" "http://example/x/abc")
    ("http://ex/x/y/z" "../r" "http://ex/x/r")

    ("http://ex/x/y" "q/r" "http://ex/x/q/r")
    ("http://ex/x/y" "q/r#s" "http://ex/x/q/r#s")
    ("http://ex/x/y" "q/r#s/t" "http://ex/x/q/r#s/t")
    ("http://ex/x/y" "ftp://ex/x/q/r" "ftp://ex/x/q/r")
    ("http://ex/x/y" "y" "http://ex/x/y")
    ("http://ex/x/y/" "." "http://ex/x/y/")
    ("http://ex/x/y/pdq" "pdq" "http://ex/x/y/pdq")
    ("http://ex/x/y/" "z/" "http://ex/x/y/z/")
    ("file:/swap/test/animal.rdf" "animal.rdf#Animal" "file:/swap/test/animal.rdf#Animal")
    ("file:/e/x/y/z" "../abc" "file:/e/x/abc")
    ("file:/example2/x/y/z" "../../../example/x/abc" "file:/example/x/abc")
    ("file:/ex/x/y/z" "../r" "file:/ex/x/r")
    ("file:/ex/x/y/z" "../../../r" "file:/r")
    ("file:/ex/x/y" "q/r" "file:/ex/x/q/r")
    ("file:/ex/x/y" "q/r#s" "file:/ex/x/q/r#s")
    ("file:/ex/x/y" "q/r#" "file:/ex/x/q/r#")
    ("file:/ex/x/y" "q/r#s/t" "file:/ex/x/q/r#s/t")
    ("file:/ex/x/y" "ftp://ex/x/q/r" "ftp://ex/x/q/r")
    ("file:/ex/x/y" "y" "file:/ex/x/y")
    ("file:/ex/x/y/" "." "file:/ex/x/y/")
    ("file:/ex/x/y/pdq" "pdq" "file:/ex/x/y/pdq")
    ("file:/ex/x/y/" "z/" "file:/ex/x/y/z/")
    ("file:/devel/WWW/2000/10/swap/test/reluri-1.n3" "//meetings.example.com/cal#m1" "file://meetings.example.com/cal#m1")
    ("file:/home/connolly/w3ccvs/WWW/2000/10/swap/test/reluri-1.n3" "//meetings.example.com/cal#m1" "file://meetings.example.com/cal#m1")
    ("file:/some/dir/foo" ".#blort" "file:/some/dir/#blort")
    ("file:/some/dir/foo" ".#" "file:/some/dir/#")
    ;; Graham Klyne https://lists.w3.org/Archives/Public/uri/2003Feb/0016.html
    ("http://example/x/y%2Fz" "abc" "http://example/x/abc")
    ("http://example/x/y/z" "../../x%2Fabc" "http://example/x%2Fabc")
    ("http://example/x/y%2Fz" "../x%2Fabc" "http://example/x%2Fabc")
    ("http://example/x%2Fy/z" "abc" "http://example/x%2Fy/abc")
    #| Ryan Lee |#
    ("http://example/x/abc.efg" "." "http://example/x/")))

(define hs-network-uri-nonrel-cases
  `(;; Check handling of queries and fragments with non-relative paths
    ("mailto:local1@domain1?query1" "local2@domain2" "mailto:local2@domain2")
    ("mailto:local1@domain1" "local2@domain2?query2" "mailto:local2@domain2?query2")
    ("mailto:local1@domain1?query1" "local2@domain2?query2" "mailto:local2@domain2?query2")
    ("mailto:local@domain?query1" "?query2" "mailto:local@domain?query2")
    ("mailto:?query1" "local@domain?query2" "mailto:local@domain?query2")
    ("mailto:local@domain?query1" "?query2" "mailto:local@domain?query2")
    ("foo:bar" "http://example/a/b?c/../d" "http://example/a/b?c/../d")
    ("foo:bar" "http://example/a/b#c/../d" "http://example/a/b#c/../d")))

(define hs-network-uri-invertible-cases
  '(#| I'm quoting a comment in the Haskell network-uri test file verbatim:

       "These (78-81) are some awkward test cases thrown up by a question on the URI list:
        http://lists.w3.org/Archives/Public/uri/2005Jul/0013
        Note that RFC 3986 discards path segments after the final '/' only when merging two
        paths - otherwise the final segment in the base URI is maintained.  This leads to
        difficulty in constructing a reversible relativeTo/relativeFrom pair of functions."
    |#
    ("http://www.example.com/data/limit/.." "test.xml" "http://www.example.com/data/limit/test.xml")
    ("file:/some/dir/foo" "./#blort" "file:/some/dir/#blort")
    ("file:/some/dir/foo" "./#" "file:/some/dir/#")
    ("file:/some/dir/.." "./#blort" "file:/some/dir/#blort")
    ;; testRelative91 = testRelSplit "testRelative91" "http://example.org/base/uri" "http:this" "this"
    ("http://example.org/base/uri" "http:this" "http:this")
    ("http:base" "http:this" "http:this")
    ("f://example.org/base/a" "b/c//d/e" "f://example.org/base/b/c//d/e")
    ("mid:m@example.ord/c@example.org" "m2@example.ord/c2@example.org" "mid:m@example.ord/m2@example.ord/c2@example.org")
    ("file:///C:/DEV/Haskell/lib/HXmlToolbox-3.01/examples/" "mini1.xml" "file:///C:/DEV/Haskell/lib/HXmlToolbox-3.01/examples/mini1.xml")
    ("foo:a/y/z" "../b/c" "foo:a/b/c")))

(define all-hs-network-uri-cases
  (append hs-network-uri-nonrel-cases hs-network-uri-invertible-cases))

(define abnormal-cases
  '(;; Over-deep `..' traversal is clamped at root, not an error:
    ("http://a/b/c/d;p?q" "../../../g"    "http://a/g")
    ("http://a/b/c/d;p?q" "../../../../g" "http://a/g")
    ("http://a/b/c/d;p?q" "../../../.."   "http://a/")
    ("http://a/b/c/d;p?q" "../../../../"  "http://a/")
    ;; Dotted segments in absolute paths are removed:
    ("http://a/b/c/d;p?q" "/./g"  "http://a/g")
    ("http://a/b/c/d;p?q" "/../g" "http://a/g")
    ;; These are NOT dot-segments; literal names, not traversal:
    ("http://a/b/c/d;p?q" "g.." "http://a/b/c/g..")
    ("http://a/b/c/d;p?q" "..g" "http://a/b/c/..g")
    ;; Dots do not affect query or fragment:
    ("http://a/b/c/d;p?q" "g?y/./x"  "http://a/b/c/g?y/./x")
    ("http://a/b/c/d;p?q" "g?y/../x" "http://a/b/c/g?y/../x")
    ("http://a/b/c/d;p?q" "g#s/./x"  "http://a/b/c/g#s/./x")
    ("http://a/b/c/d;p?q" "g#s/../x" "http://a/b/c/g#s/../x")
    #| The following two test cases test what happens if we normalize
       a path to be empty. Specifically an empty path can never be parsed
       as an empty internal representation (zero-length u16vector), so this
       must be normalized to #f in order to parse the equivalent <foo:>
       string representation to the same IRI record. |#
    ("foo:a" "." "foo:")
    ("foo:a" ".." "foo:")
    #|
       The following two normalization cases are numbered testRelative94 and testRelative99
       in Haskell network-uri test file. The string representation <f://g> is given as result
       of both.

       I don't think this is tenable, because this is parsed with `f' as scheme component and
       `g' as host, because `://' always separates a scheme and authority component. I believe
       this is a limitation of a string representation of URIs: there's no way to preserve
       the separation after normalization.

       RFC 3986 section 4.2 explicitly emphasises that `./some/path' is semantically equivalent
       to the bare, undotted path, and to use that for disambiguation of relative references
       with colons. Sufficiently robust normalization should thus make sure not to normalize a
       path which would have a different meaning when represented as a string and then re-parsed.
       That is, prefix such a path with `./'!

       In these two examples, relative reference resolution would elicit the path `//g'. There
       is no authority component defined, hence that path is invalid for the parser (since it
       has an empty segment with a leading slash, only admitted by the `path-abempty' production)
       Although this path /would/ be valid if we wrote e.g. <f:////g> (empty registered name
       component), the empty registered name portion would not be produced by the relative
       reference resolution or normalization procedure as described in RFC 3986.
    |#
    ("f:/a" ".//g" "f:.///g")
    ("f:/a/" "..//g" "f:.///g")))

(define chicken-uri-generic-cases
  `(;; Chicken uri-generic `reverse-extra-cases'
    ("http://a/b/c/d;p?q" "d;p" "http://a/b/c/d;p?q")
    ("http://a/b/c/d;p?q" "e" "http://a/b/c/e")
    ("http://a/b/c/d;p?q" "." "http://a/b/c/")
    ("http://a/b/c/d;p?q" "../e" "http://a/b/e")
    ("http://a/b/c/d;p?q" "../c" "http://a/b/c") ;; Slightly weird: dir in base, file in target
    ("http://a/b/c/d;p?q" ".." "http://a/b/")
    ("http://a/b/c/d;p?q" "../.." "http://a/") ;; or "/", but that's not convenient
    ("http://a/b/c/d;p?q" "//a" "http://a") ; No relative representation possible
    ("http://a/b/c/d;p?q" "//b" "http://b")
    ("http://a/b/c/d;p?q" "//b/" "http://b/")
    ("http://a/b/c/d;p?q" "//b/c" "http://b/c")
    ("http://a/b/c/d;p?q" "ftp://a/b/c/d;p?q" "ftp://a/b/c/d;p?q")
    ("http://a/b/c/d;p?q" "ftp://x/y/z;a?b" "ftp://x/y/z;a?b")))


;; Resolves each reference of `refs` against `base` and compares the result with the
;; corresponding text of `expected`
(define (check-resolution base refs expected)
  (for-each
    (lambda (ref expect)
      (test-equal (string-append "<" (iri->string base) "> + <" ref ">")
                  (iri->string (string->iri expect))
                  (iri->string (resolve-iri base (string->iri ref))))
      ;; Resolution of URIs behaves identically
      (test-equal (string-append "<" (iri->string base) "> + <" ref "> (URI)")
                  (iri->string (string->iri expect))
                  (uri->string (resolve-uri (string->uri (iri->string base)) (string->uri ref)))))
    refs
    expected))

;; Rows of the form (base reference result)
(define (check-resolution-rows rows)
  (for-each
    (lambda (row)
      (let ((base (car row)) (ref (cadr row)) (expect (caddr row)))
        (test-equal (string-append "<" base "> + <" ref "> = <" expect ">")
                    (iri->string (string->iri expect))
                    (iri->string (resolve-iri (string->iri base) (string->iri ref))))
        (test-equal (string-append "<" base "> + <" ref "> = <" expect "> (URI)")
                    (iri->string (string->iri expect))
                    (uri->string (resolve-uri (string->uri base) (string->uri ref))))))
    rows))

;; Rows of the form (base reference resolved)
(define (check-relativization-rows rows)
  (for-each
    (lambda (row)
      (let ((base (car row)) (reference (cadr row)) (resolved (caddr row)))
        (test-equal (string-append "<" base "> > <" resolved "> = <" reference ">")
                    (iri->string (string->iri reference))
                    (iri->string (relativize-iri (string->iri base) (string->iri resolved))))
        (test-equal (string-append "<" base "> > <" resolved "> = <" reference "> (URI)")
                    (iri->string (string->iri reference))
                    (uri->string (relativize-uri (string->uri base) (string->uri resolved))))))
    rows))

(test-begin "SRFI 275: Resolution")

(test-group "Resolution (RDF/Turtle test cases)"
  (check-resolution turtle-base01 turtle-common-cases turtle-expected01)
  (check-resolution turtle-base02 turtle-common-cases turtle-expected02)
  (check-resolution turtle-base07 turtle-cases07 turtle-expected07)
  (check-resolution turtle-base08a turtle-cases08a turtle-expected08a)
  (check-resolution turtle-base08b turtle-cases08b turtle-expected08b)
  (check-resolution turtle-base08c turtle-cases08c turtle-expected08c))

(test-group "Resolution (miscellaneous test cases)"
  (check-resolution-rows (map (lambda (row) (list (car row) (cadr row) (caddr row))) swap-cases))
  (check-resolution-rows (map (lambda (row) (list (car row) (cadr row) (caddr row)))
                              all-hs-network-uri-cases)))

(test-group "Resolution (abnormal test cases)"
  (check-resolution-rows abnormal-cases))

(test-group "Resolution (RFC 3986, section 5.4)"
  (define base "http://a/b/c/d;p?q")
  (define normal
    '(("g:h" "g:h") ("g" "http://a/b/c/g") ("./g" "http://a/b/c/g") ("g/" "http://a/b/c/g/")
      ("/g" "http://a/g") ("//g" "http://g") ("?y" "http://a/b/c/d;p?y") ("g?y" "http://a/b/c/g?y")
      ("#s" "http://a/b/c/d;p?q#s") ("g#s" "http://a/b/c/g#s") ("g?y#s" "http://a/b/c/g?y#s")
      (";x" "http://a/b/c/;x") ("g;x" "http://a/b/c/g;x") ("g;x?y#s" "http://a/b/c/g;x?y#s")
      ("" "http://a/b/c/d;p?q") ("." "http://a/b/c/") ("./" "http://a/b/c/") (".." "http://a/b/")
      ("../" "http://a/b/") ("../g" "http://a/b/g") ("../.." "http://a/") ("../../" "http://a/")
      ("../../g" "http://a/g")))
  (define abnormal
    '(("../../../g" "http://a/g") ("../../../../g" "http://a/g") ("/./g" "http://a/g")
      ("/../g" "http://a/g") ("g." "http://a/b/c/g.") (".g" "http://a/b/c/.g")
      ("g.." "http://a/b/c/g..") ("..g" "http://a/b/c/..g") ("./../g" "http://a/b/g")
      ("./g/." "http://a/b/c/g/") ("g/./h" "http://a/b/c/g/h") ("g/../h" "http://a/b/c/h")
      ("g;x=1/./y" "http://a/b/c/g;x=1/y") ("g;x=1/../y" "http://a/b/c/y")
      ("g?y/./x" "http://a/b/c/g?y/./x") ("g?y/../x" "http://a/b/c/g?y/../x")
      ("g#s/./x" "http://a/b/c/g#s/./x") ("g#s/../x" "http://a/b/c/g#s/../x")))
  (for-each (lambda (case)
              (test-equal (string-append "normal: " (car case))
                          (cadr case)
                          (iri->string (resolve-iri (string->iri base) (string->iri (car case))))))
            normal)
  (for-each (lambda (case)
              (test-equal (string-append "abnormal: " (car case))
                          (cadr case)
                          (iri->string (resolve-iri (string->iri base) (string->iri (car case))))))
            abnormal))

(test-group "Examples of the specification"
  (define string-cases '("g:h" "g" "./g" "g/" "/g" "//g"))
  (define base01 (string->iri "http://a/bb/ccc/d;p?q"))
  (define base02 (string->iri "http://a/bb/ccc/d/"))
  (define base07 (string->iri "file:///a/bb/ccc/d;p?q"))
  (define (resolve-with base-iri)
    (lambda (ref) (iri->string (resolve-iri base-iri (string->iri ref)))))
  (test-equal '("g:h" "http://a/bb/ccc/g" "http://a/bb/ccc/g" "http://a/bb/ccc/g/" "http://a/g" "http://g")
              (map (resolve-with base01) string-cases))
  (test-equal '("g:h" "http://a/bb/ccc/d/g" "http://a/bb/ccc/d/g" "http://a/bb/ccc/d/g/" "http://a/g" "http://g")
              (map (resolve-with base02) string-cases))
  (test-equal '("g:h" "file:///a/bb/ccc/g" "file:///a/bb/ccc/g" "file:///a/bb/ccc/g/" "file:///g" "file://g")
              (map (resolve-with base07) string-cases))
  (define base-iri (string->iri "http://a/bb/ccc/d;p?q"))
  (define reference0 (string->iri "g"))
  (define resolved0 (string->iri "http://a/bb/ccc/g"))
  (test-assert (iri=? (relativize-iri base-iri (resolve-iri base-iri reference0)) reference0))
  (test-assert (iri=? (resolve-iri base-iri (relativize-iri base-iri resolved0)) resolved0)))

(test-group "Resolution: result types"
  (test-assert (iri? (resolve-iri (string->iri "http://a/b") (string->iri "c"))))
  (test-assert (non-relative-iri? (resolve-iri (string->iri "http://a/b") (string->iri "c"))))
  (test-assert (uri? (resolve-uri (string->uri "http://a/b") (string->uri "c"))))
  (test-assert (non-relative-uri? (resolve-uri (string->uri "http://a/b") (string->uri "c"))))
  (test-assert "a non-relative reference is resolved to itself"
               (iri=? (string->iri "g:h") (resolve-iri (string->iri "http://a/b") (string->iri "g:h"))))
  (test-assert "the resolved IRI has the structure of the base"
               (let ((r (resolve-iri (string->iri "http://u@a:80/b/c?q") (string->iri "d"))))
                 (and (equal? (iri-user r) "u") (equal? (iri-host r) "a") (equal? (iri-port r) 80)
                      (equal? (iri-query r) #f))))
  (test-equal "relative path in a base with an empty path" "http://a/c"
              (iri->string (resolve-iri (string->iri "http://a") (string->iri "c"))))
  (test-equal "query of the base is retained for an empty reference path" "http://a/b?q#f"
              (iri->string (resolve-iri (string->iri "http://a/b?q") (string->iri "#f"))))
  (test-equal "empty query of the reference" "http://a/b?"
              (iri->string (resolve-iri (string->iri "http://a/b?q") (string->iri "?"))))
  (test-equal "non-ASCII characters" "http://a/b/é/ü"
              (iri->string (resolve-iri (string->iri "http://a/b/c") (string->iri "é/ü"))))
  (test-equal "percent-encoded bytes are retained" "http://a/b/%C3%A9"
              (iri->string (resolve-iri (string->iri "http://a/b/c") (string->iri "%c3%a9")))))

(test-group "Resolution: errors"
  (test-error "the base has to be non-relative" (resolve-iri (string->iri "/a/b") (string->iri "c")))
  (test-error "the base has to be non-relative" (resolve-uri (string->uri "a/b") (string->uri "c")))
  (test-error "the base has to be an IRI" (resolve-iri (string->uri "http://a/b") (string->iri "c")))
  (test-error "the reference has to be an IRI" (resolve-iri (string->iri "http://a/b") (string->uri "c")))
  (test-error "the base has to be a URI" (resolve-uri (string->iri "http://a/b") (string->uri "c")))
  (test-error "the reference has to be a URI" (resolve-uri (string->uri "http://a/b") (string->iri "c")))
  (test-error "strings are not IRIs" (resolve-iri "http://a/b" "c"))
  (test-error "the reference is not an IRI" (resolve-iri (string->iri "http://a/b") 'c)))

(test-group "Relativization"
  (for-each
    (lambda (row)
      (let ((base (car row)) (resolved (caddr row)) (reference (cadr row)))
        ;; The resolution of the relativization of an IRI against a base produces the IRI again,
        ;; unless the IRI is the base itself: its relative reference is the final segment of
        ;; its path (as in Chicken's `uri-generic`), which does not retain the query
        (unless (equal? base resolved)
          (test-equal (string-append "<" base "> + (<" base "> > <" resolved ">)")
                    (iri->string (string->iri resolved))
                    (iri->string (resolve-iri (string->iri base)
                                              (relativize-iri (string->iri base)
                                                              (string->iri resolved))))))))
    (append swap-cases chicken-uri-generic-cases))
  (check-relativization-rows
    (map (lambda (row) (list (car row) (cadr row) (caddr row))) swap-cases))
  (check-relativization-rows
    (map (lambda (row) (list (car row) (cadr row) (caddr row))) chicken-uri-generic-cases))
  (test-equal "an IRI relativized against itself is the final segment of its path"
              "d;p"
              (iri->string (relativize-iri (string->iri "http://a/b/c/d;p?q")
                                           (string->iri "http://a/b/c/d;p?q"))))
  (test-equal "<f:/a> > <f:.///g> = </g> (equivalent to <.//g>)"
              "/g"
              (iri->string (relativize-iri (string->iri "f:/a") (string->iri "f:.///g"))))
  (test-equal "<f:/a/> > <f:.///g> = <..//g>"
              "..//g"
              (iri->string (relativize-iri (string->iri "f:/a/") (string->iri "f:.///g")))))

(test-group "Relativization: structure"
  (define base (string->iri "http://a/b/c/d;p?q"))
  (define (rel text) (iri->string (relativize-iri base (string->iri text))))
  (test-equal "same path and query refer to the final segment" "b#f"
              (iri->string (relativize-iri (string->iri "http://a/b?q") (string->iri "http://a/b?q#f"))))
  (test-equal "sibling" "e" (rel "http://a/b/c/e"))
  (test-equal "child" "e/f" (rel "http://a/b/c/e/f"))
  (test-equal "parent" "../e" (rel "http://a/b/e"))
  (test-equal "directory of the base" "." (rel "http://a/b/c/"))
  (test-equal "query only" "?z" (rel "http://a/b/c/d;p?z"))
  (test-equal "fragment of another document" "e#f" (rel "http://a/b/c/e#f"))
  (test-equal "different scheme" "https://a/b/c/d" (rel "https://a/b/c/d"))
  (test-equal "different host" "//x/b/c/d" (rel "http://x/b/c/d"))
  (test-equal "different port" "//a:81/b/c/d" (rel "http://a:81/b/c/d"))
  (test-equal "different user" "//u@a/b/c/d" (rel "http://u@a/b/c/d"))
  (test-equal "empty path" "//a" (rel "http://a"))
  (test-equal "dot segments are removed first" "e" (rel "http://a/b/c/x/../e"))
  (test-assert "the result is a relative reference" (relative-iri? (relativize-iri base (string->iri "http://a/x"))))
  (test-assert "the result is a relative reference for URIs"
               (relative-uri? (relativize-uri (string->uri "http://a/b") (string->uri "http://a/c"))))
  (test-assert "the result is not relative if the scheme differs"
               (non-relative-iri? (relativize-iri base (string->iri "ftp://a/b"))))
  (test-error "the base has to be non-relative" (relativize-iri (string->iri "a/b") (string->iri "http://a/b")))
  (test-error "the identifier has to be non-relative" (relativize-iri base (string->iri "a/b")))
  (test-error "wrong type of base" (relativize-iri (string->uri "http://a/b") (string->iri "http://a/c")))
  (test-error "wrong type of identifier" (relativize-iri base (string->uri "http://a/c")))
  (test-error "wrong type of base" (relativize-uri (string->iri "http://a/b") (string->uri "http://a/c")))
  (test-error "wrong type of identifier" (relativize-uri (string->uri "http://a/b") (string->iri "http://a/c"))))

(test-group "Relativization: paths which are not path-abempty"
  (test-error "rootless path of the base"
              (relativize-iri (string->iri "urn:a/b") (string->iri "urn:a/c")))
  (test-error "rootless path of the identifier"
              (relativize-iri (string->iri "urn:/a") (string->iri "urn:b/c")))
  (test-error "rootless path in URIs"
              (relativize-uri (string->uri "mailto:a@b") (string->uri "mailto:c@d")))
  (test-equal "the irritants are the path and the argument"
              (list "urn:a/b")
              (guard (e (#t (map (lambda (x) (if (iri? x) (iri->string x) x))
                                 (cdr (error-object-irritants e)))))
                (relativize-iri (string->iri "urn:a/b") (string->iri "urn:a/c"))))
  (test-equal "different schemes need no relative path"
              "foo:b"
              (iri->string (relativize-iri (string->iri "urn:a/b") (string->iri "foo:b"))))
  (test-equal "the same rootless path"
              "urn:a/b"
              (iri->string (relativize-iri (string->iri "foo:a/b") (string->iri "urn:a/b")))))

(test-end)
