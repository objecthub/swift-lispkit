;;; SRFI 275 PATH REGRESSION TEST SUITE
;;;
;;; This is the test suite for library `(srfi 275 path)`. It is based on the test suite
;;; of the SRFI 275 sample implementation and on the examples of the SRFI document.
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
        (srfi 14)
        (srfi 275 path))

(test-begin "SRFI 275: Basic paths")

(test-group "Predicates"
  (define test-path0 (empty-path))
  (define test-path1 (empty-absolute-path))
  (define test-path2 (string->path "some/where/place"))
  (define test-path3 (string->path "some:thing/where/place"))
  (define test-path4 (string->path "some/where:there/place"))
  (define test-path5 (string->path "/some/where/place"))
  (define test-path6 (string->path "//some/where/place"))
  (define all (list test-path0 test-path1 test-path2 test-path3 test-path4 test-path5 test-path6))
  (test-equal "path?" '(#t #t #t #t #t #t #t) (map path? all))
  (test-assert "path? rejects non-paths" (not (or (path? "a/b") (path? '("a")) (path? #f))))
  (test-equal "absolute-path?" '(#f #t #f #f #f #t #t) (map absolute-path? all))
  (test-equal "relative-path?" '(#t #f #t #t #t #f #f) (map relative-path? all))
  (test-equal "empty-path?" '(#t #f #f #f #f #f #f) (map empty-path? all))
  (test-equal "rfc3987-path-noscheme?" '(#f #f #t #f #t #f #f) (map rfc3987-path-noscheme? all))
  (test-equal "rfc3987-path-rootless?" '(#f #f #t #t #t #f #f) (map rfc3987-path-rootless? all))
  (test-equal "rfc3987-path-abempty?" '(#t #t #f #f #f #t #t) (map rfc3987-path-abempty? all))
  (test-equal "rfc3987-path-absolute?" '(#f #f #f #f #f #t #f) (map rfc3987-path-absolute? all))
  (test-assert "predicates reject non-paths"
    (not (or (absolute-path? 1) (relative-path? "a") (empty-path? '())
             (rfc3987-path-rootless? #f) (rfc3987-path-noscheme? 'a)
             (rfc3987-path-abempty? "") (rfc3987-path-absolute? "/")))))

(test-group "Examples of the specification"
  (define rfc-abempty-example (string->path "//some/where/place"))
  (define rfc-absolute-example (string->path "/some/where/place"))
  (define rfc-rootless-example (string->path "some:thing/where/place"))
  (define rfc-noscheme-example (string->path "some/where/place"))
  (define relative-empty-example (string->path ""))
  (define absolute-empty-example (string->path "/"))
  (define all (list rfc-abempty-example rfc-absolute-example rfc-rootless-example
                    rfc-noscheme-example relative-empty-example absolute-empty-example))
  (test-equal '(#t #t #f #f #f #t) (map absolute-path? all))
  (test-equal '(#f #f #t #t #t #f) (map relative-path? all))
  (test-equal '(#f #f #f #f #t #f) (map empty-path? all))
  (test-equal '(#t #t #f #f #t #t) (map rfc3987-path-abempty? all))
  (test-equal '(#f #t #f #f #f #f) (map rfc3987-path-absolute? all))
  (test-equal '(#f #f #t #t #f #f) (map rfc3987-path-rootless? all))
  (test-equal '(#f #f #f #t #f #f) (map rfc3987-path-noscheme? all)))

(test-group "Empty paths"
  (test-assert (empty-path? (empty-path)))
  (test-assert (not (empty-path? (empty-absolute-path))))
  (test-assert (relative-path? (empty-path)))
  (test-assert (absolute-path? (empty-absolute-path)))
  (test-equal "" (path->string (empty-path)))
  (test-equal "/" (path->string (empty-absolute-path)))
  (test-assert (path=? (empty-absolute-path) (string->path "/")))
  (test-assert (path=? (empty-path) (string->path "")))
  (test-assert (path=? (empty-path) (empty-path) (empty-path)))
  (test-assert (not (path=? (empty-path) (empty-absolute-path)))))

(test-group "Segments"
  (define example0 (empty-path))
  (define example1 (empty-absolute-path))
  (define example2 (string->path "some/where/place"))
  (define example3 (string->path "/some/where/place"))
  (test-equal '(0 0 3 3) (map path-length (list example0 example1 example2 example3)))
  (test-equal '() (path-segments example0))
  (test-equal '() (path-segments example1))
  (test-equal '("some" "where" "place") (path-segments example2))
  (test-equal '("some" "where" "place") (path-segments example3))
  (test-equal "some" (path-ref example2 0))
  (test-equal "place" (path-ref example3 2))
  (test-error (path-ref example0 0))
  (test-error (path-ref example1 9999))
  (test-error (path-ref example2 12))
  (test-error (path-ref example2 -9999))
  (test-error (path-ref example2 1.0))
  (test-error (path-ref "some/where" 0))
  (test-error (path-segments "some/where"))
  (test-error (path-length 'a))
  (test-equal '("" "bar") (path-segments (string->path "//bar")))
  (test-equal '("foo" "" "bar") (path-segments (string->path "/foo//bar")))
  (test-equal '("foo" "" "bar") (path-segments (string->path "foo//bar")))
  (test-equal '("a" "") (path-segments (string->path "a/")))
  (test-equal '("" "") (path-segments (string->path "//"))))

(test-group "Update"
  (define example0 (string->path "some/where/place"))
  (define example1 (string->path "/some/where/place"))
  (test-equal "some/where/else" (path->string (path-update example0 2 "else")))
  (test-equal "/in%2Fany/where/place" (path->string (path-update example1 0 "in/any")))
  (test-equal "persistent" "some/where/place" (path->string example0))
  (test-equal "escapes" "some/a%20b/place" (path->string (path-update example0 1 "a b")))
  (test-equal "keeps escapes" "some/a%2Fb/place" (path->string (path-update example0 1 "a%2fb")))
  (test-equal "unicode" "some/%C3%A9/place" (path->string (path-update example0 1 "%c3%a9")))
  (test-equal "absolute empty segment" "//where/place" (path->string (path-update example1 0 "")))
  (test-error (path-update example0 0 ""))
  (test-error (path-update example0 3 "x"))
  (test-error (path-update example0 -1 "x"))
  (test-error (path-update example0 0 'x))
  (test-error (path-update example0 0 "%C0%A0"))
  (test-error (path-update example0 0 "%zz")))

(test-group "Equality"
  (define example0 (string->path "some/where/place"))
  (define example1 (string->path "/some/where/place"))
  (define example2 (string->path "some/where/place"))
  (test-assert (not (path=? example0 example1)))
  (test-assert (path=? example0 example2))
  (test-assert (not (path=? example0 example1 example2)))
  (test-assert (path=? example0 example2 (string->path "some/where/place")))
  (test-assert (not (path=? example0 (string->path "some/where"))))
  (test-assert (not (path=? example0 (string->path "some/where/Place"))))
  (test-error (path=? example0 "some/where/place"))
  (test-error (path=? "some/where/place" example0)))

(test-group "Serialization"
  (test-equal "" (path->string (string->path "")))
  (test-equal "/" (path->string (string->path "/")))
  (test-equal "some/where/place" (path->string (string->path "some/where/place")))
  (test-equal "/some/where/place" (path->string (string->path "/some/where/place")))
  (test-equal "some:thing/where/place" (path->string (string->path "some:thing/where/place")))
  (test-equal "some/where:there/place" (path->string (string->path "some/where:there/place")))
  (test-equal "//some/where/place" (path->string (string->path "//some/where/place")))
  (test-equal "a/" (path->string (string->path "a/")))
  (test-equal "///" (path->string (string->path "///")))
  (test-equal "uppercase escapes" "a%2Fb/%C3%A9" (path->string (string->path "a%2fb/%c3%a9")))
  (test-equal "characters of paths" "a?b#c[d]e@f:g" (path->string (string->path "a?b#c[d]e@f:g")))
  (test-equal "unicode" "α/β/😎" (path->string (string->path "α/β/😎")))
  (test-assert "no slash in segments"
    (not (memv #\/ (string->list (apply string-append (path-segments (string->path "a%2Fb/c"))))))))

(test-group "Path strings"
  (test-assert (path-string? "some/where/place"))
  (test-assert (path-string? ""))
  (test-assert (path-string? "/"))
  (test-assert (path-string? (string->path "some/where/place")))
  (test-assert (path-string? (empty-path)))
  (test-assert (path-string? "crêpes/in/Rhône"))
  (test-assert (path-string? "a😎b"))
  (test-assert "invalid UTF-8 results in a replacement character"
    (not (path-string? (utf8->string (bytevector #xC0 #xA0)))))
  (test-assert (not (path-string? "a b")))
  (test-assert (not (path-string? "a%20b")))
  (test-assert (not (path-string? 'a)))
  (test-assert (not (path-string? #f)))
  (test-assert (not (path-string? '("a" "b")))))

(test-group "Parsing"
  (test-assert (absolute-path? (string->path "/a")))
  (test-assert (relative-path? (string->path "a")))
  (test-equal "some:where/place/else" (path->string (string->relative-path "some:where/place/else")))
  (test-equal "./some:where/place/else" (path->string (string->relative-path "some:where/place/else" #t)))
  (test-equal "some/where:place/else" (path->string (string->relative-path "some/where:place/else" #t)))
  (test-equal "" (path->string (string->relative-path "")))
  (test-error (string->relative-path "/some/where/place"))
  (test-error (string->relative-path 'a))
  (test-equal "/some/where/place" (path->string (string->absolute-path "/some/where/place")))
  (test-equal "//some/where/place" (path->string (string->absolute-path "//some/where/place")))
  (test-equal "/some/where/place" (path->string (string->absolute-path "/some/where/place" #t)))
  (test-error (string->absolute-path "//some/where/place" #t))
  (test-error (string->absolute-path "some/where/place"))
  (test-error (string->absolute-path ""))
  (test-error (string->path 'a))
  (test-error "illegal character" (string->path "a b"))
  (test-error "illegal character" (string->path "a\x0;b"))
  (test-error "invalid escape" (string->path "a%zzb"))
  (test-error "incomplete escape" (string->path "a%2"))
  (test-error "lone percent sign" (string->path "a%"))
  (test-equal "uppercase hexadecimal digits" "a%C3%A9b" (path->string (string->path "a%c3%a9b")))
  (test-equal "reserved characters" '("a:b" "c;d=e" "f@g") (path-segments (string->path "a:b/c;d=e/f@g")))
  (test-equal "percent-encoded slash" '("a%2Fb") (path-segments (string->path "a%2fb"))))

(test-group "Overlong and invalid UTF-8 sequences"
  (for-each
    (lambda (str)
      (test-error str (string->path (string-append "a/" str)))
      (test-error str (string->relative-path str))
      (test-error str (string->absolute-path (string-append "/" str)))
      (test-error str (list->relative-path (list str)))
      (test-error str (path-update (string->path "a/b") 1 str)))
    '("a%C0%A0b" "a%E0%80%A0b" "a%E0%80%80%A0b" "a%E0%80%A9b" "a%C0b" "a%E2%99"
      "a%F0%9F%98" "a%F0%9F%98x" "a%A9" "%ED%A0%80" "%F4%90%80%80" "%C1%BF" "%FF" "%F5%80%80%80"))
  (test-equal "valid 2-, 3-, and 4-byte sequences"
              '("%C2%A0" "%E2%99%82" "%F0%9F%98%8E" "%F4%8F%BF%BF")
              (path-segments (string->path "%c2%a0/%e2%99%82/%f0%9f%98%8e/%f4%8f%bf%bf"))))

(test-group "Lists of segments"
  (test-equal '("some" "where" "place") (path-segments (list->relative-path '("some" "where" "place"))))
  (test-equal '("some:where" "place" "else")
              (path-segments (list->relative-path '("some:where" "place" "else"))))
  (test-equal '("." "some:where" "place" "else")
              (path-segments (list->relative-path '("some:where" "place" "else") #t)))
  (test-equal '("some" "where:place")
              (path-segments (list->relative-path '("some" "where:place") #t)))
  (test-equal '() (path-segments (list->relative-path '())))
  (test-assert (relative-path? (list->relative-path '("a"))))
  (test-equal "slashes are escaped" '("a%2Fb" "%2Fc") (path-segments (list->relative-path '("a/b" "/c"))))
  (test-equal "illegal characters are escaped" '("a%20b" "é")
              (path-segments (list->relative-path '("a b" "é"))))
  (test-error (list->relative-path '("" "a")))
  (test-error (list->relative-path '("a" 1)))
  (test-error (list->relative-path "a"))
  (test-error (list->relative-path '("a" . "b")))
  (test-equal '("some" "where" "place") (path-segments (list->absolute-path '("some" "where" "place"))))
  (test-equal '("" "some" "where" "place")
              (path-segments (list->absolute-path '("" "some" "where" "place"))))
  (test-equal '("some" "where" "place")
              (path-segments (list->absolute-path '("some" "where" "place") #t)))
  (test-equal "/some/where" (path->string (list->absolute-path '("some" "where"))))
  (test-equal "//some/where" (path->string (list->absolute-path '("" "some" "where"))))
  (test-assert (absolute-path? (list->absolute-path '())))
  (test-error (list->absolute-path '("" "some" "where" "place") #t))
  (test-error (list->absolute-path '(a)))
  (test-error (list->absolute-path 'a)))

(test-group "Building paths"
  (test-assert (path=? (build-path "some" "where" "place") (string->path "some/where/place")))
  (test-equal "some/where/place" (path->string (build-path "some" "where" "place")))
  (test-equal "some/where/place" (path->string (build-path (empty-path) "some" "where" "place")))
  (test-equal "/some/where/place"
              (path->string (build-path (empty-absolute-path) "some" "where" "place")))
  (test-assert (path=? (build-path (empty-absolute-path) "some" "where" "place")
                       (string->path "/some/where/place")))
  (test-assert (path=? (build-path (empty-absolute-path) "" "some" "where" "place")
                       (string->path "//some/where/place")))
  (test-equal "//res" (path->string (build-path (empty-absolute-path) "" "res")))
  (test-equal "/a/x" (path->string (build-path (string->path "/a/b") "x")))
  (test-equal "" (path->string (build-path)))
  (test-assert (path=? (build-path (string->path "/a/b")) (string->path "/a/b")))
  (test-equal "escape slash in relative path segments" '("a%2Fb" "c%2Fd" "e%2Ff")
              (path-segments (build-path "a/b" "c/d" "e/f")))
  (test-equal "escape slash in absolute path segments" '("a%2Fb" "c%2Fd" "e%2Ff")
              (path-segments (build-path (empty-absolute-path) "a/b" "c/d" "e/f")))
  (test-error "empty initial segment" (build-path "" "some" "where" "place"))
  (test-error "empty initial segment, empty base" (build-path (empty-path) "" "some" "where" "place"))
  (test-error (build-path (empty-path) 'a))
  (test-error (build-path "a" 'b))
  (test-error (build-path 'a "b")))

(test-group "Remove dot segments"
  (define (rds str) (path->string (remove-dot-segments (string->path str))))
  (test-equal "/a/x" (rds "/a/b/../x"))
  (test-equal "/" (rds "/a/b/.././.././.."))
  (test-equal "/" (rds "/a/b/.././.././../"))
  (test-equal "/e" (rds "/a/b/../.././../../e"))
  (test-equal "e" (rds "a/b/../.././../../e"))
  (test-equal "/a/b/" (rds "/a/b/."))
  (test-equal "/a/" (rds "/a/b/.."))
  (test-equal "/" (rds "/a/.."))
  (test-equal "/" (rds "/."))
  (test-equal "" (rds ""))
  (test-equal "" (rds "."))
  (test-equal "" (rds ".."))
  (test-equal "g" (rds "./g"))
  (test-equal "g" (rds "../g"))
  (test-equal "a/b" (rds "a/./b"))
  (test-equal "/a//b" (rds "/a//b"))
  (test-equal "/a//b/" (rds "/a//b/"))
  (test-equal "//" (rds "////../.."))
  (test-equal "/foo/" (rds "/foo/bar//../.."))
  (test-equal "/foo/bar/" (rds "/foo/bar//.."))
  (test-equal "g.." (rds "g.."))
  (test-equal "/..g" (rds "/..g"))
  (test-equal "/a/b/c/" (rds "/a/b/c/./"))
  (test-equal "/a/b/" (rds "/a/b/c/.././"))
  (test-assert (relative-path? (remove-dot-segments (string->path "a/b/../../.."))))
  (test-assert (absolute-path? (remove-dot-segments (string->path "/a/b/../../.."))))
  (test-error (remove-dot-segments "a/b")))

(test-group "Merging"
  (define example1 (merge-paths (string->path "/a/b") (string->path "x")))
  (define example2 (merge-paths (string->path "/b/c/d;p") (string->path "../../../g")))
  (define example3 (merge-paths (string->path "/swap/test/animal.rdf") (string->path "animal.rdf")))
  (define example4a (merge-paths (empty-path) (string->path "a/b/c")))
  (define example4b (merge-paths (empty-path) (string->path "a/b/c") #t))
  (test-equal "/a/x" (path->string example1))
  (test-equal "/b/c/../../../g" (path->string example2))
  (test-equal "/swap/test/animal.rdf" (path->string example3))
  (test-equal "a/b/c" (path->string example4a))
  (test-equal "/a/b/c" (path->string example4b))
  (test-equal "/a/b/" (path->string (merge-paths (string->path "/a/b/c") (string->path "") #f)))
  (test-equal "a/" (path->string (merge-paths (string->path "a/b") (string->path ""))))
  (test-equal "" (path->string (merge-paths (string->path "a") (string->path ""))))
  (test-equal "/" (path->string (merge-paths (string->path "/a") (string->path ""))))
  (test-equal "a/x" (path->string (merge-paths (string->path "a/b") (string->path "x"))))
  (test-equal "x" (path->string (merge-paths (string->path "a") (string->path "x"))))
  (test-equal "/x" (path->string (merge-paths (string->path "/") (string->path "x"))))
  (test-equal "/x" (path->string (merge-paths (string->path "/") (string->path "x") #t)))
  (test-equal "/x" (path->string (merge-paths (empty-absolute-path) (string->path "x"))))
  (test-error (merge-paths "/a/b" (string->path "x")))
  (test-error (merge-paths (string->path "/a/b") "x")))

(test-group "Character set"
  (test-assert (char-set? char-set:path))
  (test-assert (char-set-contains? char-set:path #\a))
  (test-assert (char-set-contains? char-set:path #\:))
  (test-assert (char-set-contains? char-set:path #\#))
  (test-assert (char-set-contains? char-set:path #\é))
  (test-assert (not (char-set-contains? char-set:path #\space)))
  (test-assert (not (char-set-contains? char-set:path #\%)))
  (test-assert (not (char-set-contains? char-set:path #\x7F))))

(test-end)
