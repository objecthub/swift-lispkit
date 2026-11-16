;;; SRFI 275 PATH
;;; URIs, IRIs, and basic paths: path objects
;;;
;;; Library `(srfi 275 path)` implements basic paths. A path object is either relative
;;; or absolute, depending on whether it has a leading slash. Both variants consist of
;;; a sequence of segments, which are strings that do not contain slashes. The
;;; characters of a segment are characters of `char-set:path` and percent-encoded bytes,
;;; for which all consecutive sequences are well-formed UTF-8. Path objects are
;;; immutable. They are the type of the path component of URIs and IRIs; the library
;;; offers predicates which exactly follow the definitions of RFC 3986 and RFC 3987.
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

(define-library (srfi 275 path)

  (import (scheme base)
          (srfi 275 internal chars)
          (srfi 275 internal path))

  (export path?
          relative-path?
          absolute-path?
          empty-path?
          rfc3987-path-rootless?
          rfc3987-path-noscheme?
          rfc3987-path-abempty?
          rfc3987-path-absolute?
          empty-path
          empty-absolute-path
          path-length
          path-segments
          path-ref
          path-update
          path=?
          path->string
          path-string?
          string->path
          string->relative-path
          string->absolute-path
          list->relative-path
          list->absolute-path
          build-path
          remove-dot-segments
          merge-paths
          char-set:path)
)
