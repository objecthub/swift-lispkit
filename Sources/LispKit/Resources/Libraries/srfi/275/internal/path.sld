;;; SRFI 275 INTERNAL PATH
;;; URIs, IRIs, and basic paths: path objects
;;;
;;; This library is internal to the implementation of SRFI 275. It implements path
;;; objects and all procedures of library `(srfi 275 path)`, together with some
;;; additional functionality needed by the implementation of URIs and IRIs.
;;;
;;; A path object consists of a flag which determines whether the path is absolute
;;; (i.e. it has a leading slash) and a list of segments. Each segment is a string in
;;; canonical form: it contains characters from `char-set:path` and percent-encoded
;;; bytes with uppercase hexadecimal digits, but no slashes. An absolute path with
;;; a single empty segment is represented by an absolute path without segments, and
;;; both stand for "/". The initial segment of a relative path is never empty.
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

(define-library (srfi 275 internal path)

  (import (scheme base)
          (srfi 275 internal chars)
          (srfi 275 internal encoding))

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
          char-set:path
          ;; internal
          make-path
          path-absolute-flag
          path-segment-list
          segments-have-colon?
          check-path
          split-on-slash
          canonical-segment
          path-char?)

  (begin

    ;;; Path objects

    (define-record-type path
      (new-path absolute? segments)
      path?
      (absolute? path-absolute-flag)
      (segments path-segment-list))

    ;; Creates a path from a list of canonical segments
    (define (make-path absolute? segments)
      (new-path absolute? (if (equal? segments '("")) '() segments)))

    (define (check-path who obj)
      (unless (path? obj)
        (fail who "not a path" obj)))

    (define (relative-path? obj)
      (and (path? obj) (not (path-absolute-flag obj))))

    (define (absolute-path? obj)
      (and (path? obj) (path-absolute-flag obj)))

    (define (empty-path? obj)
      (and (relative-path? obj) (null? (path-segment-list obj))))

    ;; Does the first segment contain a plain colon?
    (define (segments-have-colon? segments)
      (and (pair? segments)
           (let ((seg (car segments)))
             (let loop ((i 0))
               (and (< i (string-length seg))
                    (or (char=? (string-ref seg i) #\:)
                        (loop (+ i 1))))))))

    (define (rfc3987-path-rootless? obj)
      (and (relative-path? obj) (pair? (path-segment-list obj))))

    (define (rfc3987-path-noscheme? obj)
      (and (rfc3987-path-rootless? obj)
           (not (segments-have-colon? (path-segment-list obj)))))

    (define (rfc3987-path-abempty? obj)
      (or (absolute-path? obj) (empty-path? obj)))

    (define (rfc3987-path-absolute? obj)
      (and (absolute-path? obj)
           (pair? (path-segment-list obj))
           (not (string=? (car (path-segment-list obj)) ""))))

    (define (empty-path)
      (make-path #f '()))

    (define (empty-absolute-path)
      (make-path #t '()))

    (define (path-length path)
      (check-path "path-length" path)
      (length (path-segment-list path)))

    (define (path-segments path)
      (check-path "path-segments" path)
      (map string-copy (path-segment-list path)))

    (define (check-index who path k)
      (unless (and (exact-integer? k)
                   (<= 0 k)
                   (< k (length (path-segment-list path))))
        (fail who "invalid segment index" k path)))

    (define (path-ref path k)
      (check-path "path-ref" path)
      (check-index "path-ref" path k)
      (string-copy (list-ref (path-segment-list path) k)))

    (define (path-update path k str)
      (check-path "path-update" path)
      (check-index "path-update" path k)
      (unless (string? str)
        (fail "path-update" "not a string" str))
      (let* ((seg (canonical-segment "path-update" str))
             (segs (path-segment-list path))
             (new-segs (append (list-head segs k) (cons seg (list-tail segs (+ k 1))))))
        (when (and (= k 0)
                   (not (path-absolute-flag path))
                   (string=? seg ""))
          (fail "path-update" "empty initial segment in relative path" k str path))
        (make-path (path-absolute-flag path) new-segs)))

    (define (list-head lst k)
      (if (= k 0)
          '()
          (cons (car lst) (list-head (cdr lst) (- k 1)))))

    (define (path=? path1 path2 . paths)
      (check-path "path=?" path1)
      (check-path "path=?" path2)
      (for-each (lambda (p) (check-path "path=?" p)) paths)
      (let loop ((p path1) (rest (cons path2 paths)))
        (or (null? rest)
            (and (eq? (path-absolute-flag p) (path-absolute-flag (car rest)))
                 (equal? (path-segment-list p) (path-segment-list (car rest)))
                 (loop (car rest) (cdr rest))))))

    (define (join-segments segments)
      (cond ((null? segments) "")
            ((null? (cdr segments)) (car segments))
            (else
              (let ((out (open-output-string)))
                (write-string (car segments) out)
                (for-each (lambda (seg) (write-char #\/ out) (write-string seg out))
                          (cdr segments))
                (get-output-string out)))))

    (define (path->string path)
      (check-path "path->string" path)
      (let ((text (join-segments (path-segment-list path))))
        (if (path-absolute-flag path)
            (string-append "/" text)
            text)))

    ;;; Parsing and construction

    ;; Is `cp` a character which may appear in a segment of a path object?
    (define (path-char? cp)
      (and (not (eqv? cp 47))
           (or (cp-uri-unreserved? cp)
               (cp-reserved? cp)
               (cp-ucschar? cp))))

    (define (path-string? obj)
      (cond ((path? obj) #t)
            ((string? obj)
             (let ((cps (string->codepoints obj)))
               (let loop ((i 0))
                 (or (= i (vector-length cps))
                     (let ((cp (vector-ref cps i)))
                       (and (or (eqv? cp 47) (path-char? cp))
                            (loop (+ i 1))))))))
            (else #f)))

    (define (split-on-slash str)
      (let ((n (string-length str)))
        (let loop ((i 0) (start 0) (acc '()))
          (cond ((= i n)
                 (reverse (cons (substring str start n) acc)))
                ((char=? (string-ref str i) #\/)
                 (loop (+ i 1) (+ i 1) (cons (substring str start i) acc)))
                (else
                 (loop (+ i 1) start acc))))))

    ;; Canonical form of a segment given as text. Illegal characters, including slashes,
    ;; are escaped.
    (define (canonical-segment who str)
      (canonicalize-component who str path-char? #t #f 0))

    ;; Parses the segments of a path given as text. Illegal characters are rejected.
    (define (parse-segments who str offset)
      (if (string=? str "")
          '()
          (let loop ((parts (split-on-slash str)) (offset offset) (acc '()))
            (if (null? parts)
                (reverse acc)
                (loop (cdr parts)
                      (+ offset (string-length (car parts)) 1)
                      (cons (canonicalize-component who (car parts) path-char? #f #f offset)
                            acc))))))

    (define (check-string who obj)
      (unless (string? obj)
        (fail who "not a string" obj)))

    (define (string->absolute-path str . opt)
      (check-string "string->absolute-path" str)
      (unless (and (> (string-length str) 0) (char=? (string-ref str 0) #\/))
        (fail "string->absolute-path" "path does not start with a slash" str))
      (let ((segs (parse-segments "string->absolute-path" (substring str 1 (string-length str)) 1)))
        (when (and (pair? opt) (car opt) (pair? segs) (string=? (car segs) ""))
          (fail "string->absolute-path" "initial segment is empty" str))
        (make-path #t segs)))

    (define (prefix-dot-segment? noscheme? segs)
      (and noscheme? (segments-have-colon? segs)))

    (define (string->relative-path str . opt)
      (check-string "string->relative-path" str)
      (when (and (> (string-length str) 0) (char=? (string-ref str 0) #\/))
        (fail "string->relative-path" "path starts with a slash" str))
      (let ((segs (parse-segments "string->relative-path" str 0)))
        (make-path #f (if (prefix-dot-segment? (and (pair? opt) (car opt)) segs)
                          (cons "." segs)
                          segs))))

    (define (string->path str)
      (check-string "string->path" str)
      (if (and (> (string-length str) 0) (char=? (string-ref str 0) #\/))
          (string->absolute-path str)
          (string->relative-path str)))

    ;; Canonicalizes the elements of a list of segments
    (define (list->segments who lst)
      (unless (list? lst)
        (fail who "not a proper list" lst))
      (let loop ((l lst) (k 0) (acc '()))
        (cond ((null? l) (reverse acc))
              ((string? (car l))
               (loop (cdr l) (+ k 1) (cons (canonical-segment who (car l)) acc)))
              (else (fail who "segment is not a string" (car l) k lst)))))

    (define (list->relative-path lst . opt)
      (let ((segs (list->segments "list->relative-path" lst)))
        (when (and (pair? segs) (string=? (car segs) ""))
          (fail "list->relative-path" "initial segment is empty" (car lst) 0 lst))
        (make-path #f (if (prefix-dot-segment? (and (pair? opt) (car opt)) segs)
                          (cons "." segs)
                          segs))))

    (define (list->absolute-path lst . opt)
      (let ((segs (list->segments "list->absolute-path" lst)))
        (when (and (pair? opt) (car opt) (pair? segs) (string=? (car segs) ""))
          (fail "list->absolute-path" "initial segment is empty" (car lst) 0 lst))
        (make-path #t segs)))

    (define (build-path . args)
      (cond ((null? args)
             (empty-path))
            ((string? (car args))
             (list->relative-path args))
            ((and (path? (car args)) (null? (cdr args)))
             (car args))
            ((path? (car args))
             (let* ((segs (list->segments "build-path" (cdr args)))
                    (merged (merge-paths (car args) (make-path #f segs))))
               (when (and (not (path-absolute-flag merged))
                          (pair? (path-segment-list merged))
                          (string=? (car (path-segment-list merged)) ""))
                 (fail "build-path" "initial segment is empty" (cadr args) 1))
               merged))
            (else
              (fail "build-path" "not a path or string" (car args)))))

    ;;; Dot segments and merging

    (define (remove-dot-segments path)
      (check-path "remove-dot-segments" path)
      (let ((absolute? (path-absolute-flag path)))
        (define (finish rev-segs)
          (let ((segs (reverse rev-segs)))
            ;; As in RFC 3986, a relative path which starts with an empty segment after
            ;; the removal of dot segments is read as an absolute path
            (if (and (not absolute?) (pair? segs) (pair? (cdr segs)) (string=? (car segs) ""))
                (make-path #t (cdr segs))
                (make-path absolute? segs))))
        (let loop ((in (path-segment-list path)) (out '()))
          (if (null? in)
              (finish out)
              (let ((seg (car in)) (last? (null? (cdr in))))
                (cond ((string=? seg ".")
                       (if last?
                           (finish (cons "" out))
                           (loop (cdr in) out)))
                      ((string=? seg "..")
                       (let ((out (if (pair? out) (cdr out) out)))
                         (if last?
                             (finish (cons "" out))
                             (loop (cdr in) out))))
                      (else
                        (loop (cdr in) (cons seg out)))))))))

    (define (drop-last lst)
      (if (or (null? lst) (null? (cdr lst)))
          '()
          (cons (car lst) (drop-last (cdr lst)))))

    (define (merge-paths base path . opt)
      (check-path "merge-paths" base)
      (check-path "merge-paths" path)
      (let ((base-has-authority? (and (pair? opt) (car opt)))
            (base-segs (path-segment-list base)))
        (cond ((path-absolute-flag path)
               path)
              ((and base-has-authority? (empty-path? base))
               (make-path #t (path-segment-list path)))
              (else
                (make-path (path-absolute-flag base)
                           (append (drop-last base-segs)
                                   (if (null? (path-segment-list path))
                                       '("")
                                       (path-segment-list path))))))))
  )
)
