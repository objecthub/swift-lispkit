;;; SRFI 275 INTERNAL IDENT
;;; URIs, IRIs, and basic paths: resource identifiers
;;;
;;; This library is internal to the implementation of SRFI 275. It implements the
;;; data type shared by URIs, IRIs, and relative references as well as parsing,
;;; serialization, getters, and setters. The public libraries `(srfi 275 iri)` and
;;; `(srfi 275 uri)` are thin wrappers around the procedures defined here. Procedures
;;; take the name `who` of the public procedure for error messages and a flag `iri?`
;;; which determines whether an IRI (true) or a URI (false) is expected.
;;;
;;; All components are stored in canonical form: a component is a string containing
;;; the characters permitted in the component and percent-encoded bytes. Percent-encoded
;;; bytes use uppercase hexadecimal digits and each maximal sequence of them is well-formed
;;; UTF-8. A relative reference is represented by a missing (#f) scheme. A component
;;; which is not set is #f. The authority is set if at least one of user, host, and
;;; port is set.
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

(define-library (srfi 275 internal ident)

  (import (scheme base)
          (srfi 275 internal chars)
          (srfi 275 internal encoding)
          (srfi 275 internal path))

  (export ident?
          ident-of-kind?
          ident-iri-flag
          ident-scheme-raw
          ident-user-raw
          ident-host-raw
          ident-port-raw
          ident-path-raw
          ident-query-raw
          ident-fragment-raw
          new-ident
          empty-ident
          non-relative-ident?
          relative-ident?
          absolute-ident?
          check-ident
          parse-text
          read-text
          ident->text
          get-scheme
          get-component
          get-port
          get-authority
          get-username+password
          get-path
          set-scheme
          set-user
          set-host
          set-port
          set-authority
          set-path
          set-query
          set-fragment
          segment-ref
          segment-update
          ident=?
          authority-set?
          compatible-authority?
          compatible-path?
          valid-ip-literal?)

  (begin

    ;;; Data type

    (define-record-type ident
      (new-ident iri? scheme user host port path query fragment)
      ident?
      (iri? ident-iri-flag)
      (scheme ident-scheme-raw)
      (user ident-user-raw)
      (host ident-host-raw)
      (port ident-port-raw)
      (path ident-path-raw)
      (query ident-query-raw)
      (fragment ident-fragment-raw))

    (define (ident-of-kind? iri? obj)
      (and (ident? obj) (eq? (ident-iri-flag obj) iri?)))

    (define (check-ident who iri? obj)
      (unless (ident-of-kind? iri? obj)
        (fail who (if iri? "not an IRI" "not a URI") obj)))

    (define (empty-ident iri?)
      (new-ident iri? #f #f #f #f (empty-path) #f #f))

    (define (non-relative-ident? obj iri?)
      (and (ident-of-kind? iri? obj) (ident-scheme-raw obj) #t))

    (define (relative-ident? obj iri?)
      (and (ident-of-kind? iri? obj) (not (ident-scheme-raw obj))))

    (define (absolute-ident? obj iri?)
      (and (non-relative-ident? obj iri?) (not (ident-fragment-raw obj))))

    (define (profile-for iri?)
      (if iri? iri-profile uri-profile))

    (define (authority-set? user host port)
      (and (or user host port) #t))

    ;;; Compatibility of authority and path

    ;; Is `path` a valid path if the authority is (not) set and the identifier is
    ;; (not) a relative reference? The absolute path "/" is accepted in all cases.
    (define (compatible-path? path authority? relative?)
      (if authority?
          (rfc3987-path-abempty? path)
          (or (rfc3987-path-absolute? path)
              (and (absolute-path? path) (null? (path-segment-list path)))
              (if relative?
                  (rfc3987-path-noscheme? path)
                  (rfc3987-path-rootless? path))
              (empty-path? path))))

    (define (compatible-authority? x user host port)
      (compatible-path? (ident-path-raw x)
                        (authority-set? user host port)
                        (not (ident-scheme-raw x))))

    ;;; String utilities

    (define (index-of str ch start end)
      (let loop ((i start))
        (cond ((>= i end) #f)
              ((char=? (string-ref str i) ch) i)
              (else (loop (+ i 1))))))

    (define (split-char str ch)
      (let ((n (string-length str)))
        (let loop ((i 0) (start 0) (acc '()))
          (cond ((= i n) (reverse (cons (substring str start n) acc)))
                ((char=? (string-ref str i) ch)
                 (loop (+ i 1) (+ i 1) (cons (substring str start i) acc)))
                (else (loop (+ i 1) start acc))))))

    (define (string-every pred str)
      (let ((n (string-length str)))
        (let loop ((i 0))
          (or (= i n)
              (and (pred (string-ref str i)) (loop (+ i 1)))))))

    (define (every pred lst)
      (or (null? lst)
          (and (pred (car lst)) (every pred (cdr lst)))))

    (define (hex-char? c)
      (and (cp-hex-value (char->integer c)) #t))

    (define (digit-char? c)
      (cp-digit? (char->integer c)))

    ;;; IP literals

    (define (valid-h16? s)
      (and (<= 1 (string-length s) 4) (string-every hex-char? s)))

    (define (valid-dec-octet? s)
      (let ((n (string-length s)))
        (and (<= 1 n 3)
             (string-every digit-char? s)
             (or (= n 1) (not (char=? (string-ref s 0) #\0)))
             (<= (string->number s) 255))))

    (define (valid-ipv4? s)
      (let ((parts (split-char s #\.)))
        (and (= (length parts) 4) (every valid-dec-octet? parts))))

    (define (find-double-colon s)
      (let ((n (string-length s)))
        (let loop ((i 0))
          (cond ((>= (+ i 1) n) #f)
                ((and (char=? (string-ref s i) #\:) (char=? (string-ref s (+ i 1)) #\:)) i)
                (else (loop (+ i 1)))))))

    ;; Number of 16-bit groups of a sequence of colon-separated pieces, or #f if the
    ;; pieces are not valid. If `ipv4-ok?` is true, then the last piece may be an IPv4
    ;; address, which counts for two groups.
    (define (count-groups pieces ipv4-ok?)
      (let loop ((p pieces) (count 0))
        (cond ((null? p) count)
              ((and ipv4-ok? (null? (cdr p)) (index-of (car p) #\. 0 (string-length (car p))))
               (and (valid-ipv4? (car p)) (+ count 2)))
              ((valid-h16? (car p)) (loop (cdr p) (+ count 1)))
              (else #f))))

    (define (valid-ipv6? s)
      (let ((dc (find-double-colon s)))
        (if dc
            (let ((left (substring s 0 dc))
                  (right (substring s (+ dc 2) (string-length s))))
              (and (not (find-double-colon right))
                   (let ((lc (count-groups (if (string=? left "") '() (split-char left #\:)) #f))
                         (rc (count-groups (if (string=? right "") '() (split-char right #\:)) #t)))
                     (and lc rc (<= (+ lc rc) 7)))))
            (eqv? (count-groups (split-char s #\:) #t) 8))))

    ;; IPvFuture = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
    (define (valid-ipvfuture? s)
      (let ((n (string-length s)))
        (and (> n 0)
             (memv (string-ref s 0) '(#\v #\V))
             (let ((dot (index-of s #\. 1 n)))
               (and dot
                    (> dot 1)
                    (< (+ dot 1) n)
                    (string-every hex-char? (substring s 1 dot))
                    (string-every (lambda (c)
                                    (let ((cp (char->integer c)))
                                      (or (cp-uri-unreserved? cp) (cp-sub-delim? cp) (eqv? cp 58))))
                                  (substring s (+ dot 1) n)))))))

    ;; Checks the text of an IP literal, including its brackets
    (define (valid-ip-literal? text)
      (let ((n (string-length text)))
        (and (>= n 2)
             (char=? (string-ref text 0) #\[)
             (char=? (string-ref text (- n 1)) #\])
             (let ((inner (substring text 1 (- n 1))))
               (or (valid-ipv6? inner) (valid-ipvfuture? inner))))))

    ;;; Parsing

    (define (canon who profile which str offset escape?)
      (canonicalize-component who str (profile-predicate profile which) escape? #f offset))

    (define (parse-scheme who str end)
      (let ((n end))
        (when (= n 0)
          (fail who "illegal character" #\: 0 str))
        (let loop ((i 0))
          (when (< i n)
            (let ((cp (char->integer (string-ref str i))))
              (unless (if (= i 0) (cp-scheme-start? cp) (cp-scheme-char? cp))
                (fail who "illegal character" (string-ref str i) i str))
              (loop (+ i 1)))))
        (substring str 0 n)))

    (define (parse-port who str start end)
      (if (= start end)
          #f
          (begin
            (do ((i start (+ i 1))) ((= i end))
              (unless (digit-char? (string-ref str i))
                (fail who "illegal character" (string-ref str i) i str)))
            (string->number (substring str start end)))))

    ;; Parses the text of the authority between `start` and `end` in `str`. Returns
    ;; the user, host and port components.
    (define (parse-authority who profile str start end)
      (let* ((at (index-of str #\@ start end))
             (user (and at (canon who profile 'user (substring str start at) start #f)))
             (hstart (if at (+ at 1) start)))
        (cond
          ;; IP literal
          ((and (< hstart end) (char=? (string-ref str hstart) #\[))
           (let ((close (index-of str #\] hstart end)))
             (unless close
               (fail who "unexpected end of string" (eof-object) (string-length str)))
             (let ((host (substring str hstart (+ close 1))))
               (unless (valid-ip-literal? host)
                 (fail who "invalid IP literal" host hstart str))
               (cond ((= (+ close 1) end)
                      (values user host #f))
                     ((char=? (string-ref str (+ close 1)) #\:)
                      (values user host (parse-port who str (+ close 2) end)))
                     (else
                       (fail who "illegal character" (string-ref str (+ close 1)) (+ close 1) str))))))
          (else
            (let* ((colon (index-of str #\: hstart end))
                   (hend (or colon end))
                   (host-text (substring str hstart hend))
                   (port (and colon (parse-port who str (+ colon 1) end)))
                   ;; an empty host is only retained if it is the only component of the
                   ;; authority
                   (host (if (and (string=? host-text "") (or at port))
                             #f
                             (canon who profile 'host host-text hstart #f))))
              (values user host port))))))

    ;; Parses the segments of a path given between `start` and `end` in `str`
    (define (parse-path who profile str start end)
      (let* ((absolute? (and (< start end) (char=? (string-ref str start) #\/)))
             (pstart (if absolute? (+ start 1) start))
             (pred (profile-predicate profile 'segment)))
        (make-path
          absolute?
          (if (= pstart end)
              '()
              (let loop ((i pstart) (seg-start pstart) (acc '()))
                (cond ((= i end)
                       (reverse (cons (canonicalize-component who (substring str seg-start i)
                                                              pred #f #f seg-start)
                                      acc)))
                      ((char=? (string-ref str i) #\/)
                       (loop (+ i 1) (+ i 1)
                             (cons (canonicalize-component who (substring str seg-start i)
                                                           pred #f #f seg-start)
                                   acc)))
                      (else (loop (+ i 1) seg-start acc))))))))

    ;; Parses `str` as URI or IRI. `mode` is one of the symbols `any`, `non-relative`,
    ;; and `absolute`, which determines whether a scheme is required and whether a
    ;; fragment is forbidden.
    (define (parse-text who iri? str mode)
      (unless (string? str)
        (fail who "not a string" str))
      (let* ((profile (profile-for iri?))
             (n (string-length str))
             (hash (index-of str #\# 0 n))
             (end1 (or hash n))
             (qmark (index-of str #\? 0 end1))
             (end2 (or qmark end1)))
        (when (and hash (eq? mode 'absolute))
          (fail who "fragment not permitted" #\# hash str))
        (let* ((colon (index-of str #\: 0 end2))
               (slash (index-of str #\/ 0 end2))
               (scheme-end (and colon (or (not slash) (< colon slash)) colon))
               (scheme (and scheme-end (parse-scheme who str scheme-end)))
               (start (if scheme (+ scheme-end 1) 0)))
          (when (and (not scheme) (not (eq? mode 'any)))
            (fail who "missing scheme" (eof-object) n))
          (let* ((authority? (and (<= (+ start 2) end2)
                                  (char=? (string-ref str start) #\/)
                                  (char=? (string-ref str (+ start 1)) #\/)))
                 (astart (+ start 2))
                 (aend (if authority? (or (index-of str #\/ astart end2) end2) start)))
            (call-with-values
              (lambda ()
                (if authority?
                    (parse-authority who profile str astart aend)
                    (values #f #f #f)))
              (lambda (user host port)
                (new-ident iri? scheme user host port
                           (parse-path who profile str aend end2)
                           (and qmark (canon who profile 'query (substring str (+ qmark 1) end1)
                                             (+ qmark 1) #f))
                           (and hash (canon who profile 'fragment (substring str (+ hash 1) n)
                                            (+ hash 1) #f)))))))))

    ;; Reads the text of a URI or IRI from `port` up to (not including) `stop` or the
    ;; end of the port.
    (define (read-text who port stop)
      (unless (and (input-port? port) (textual-port? port))
        (fail who "not a textual input port" port))
      (let ((out (open-output-string)))
        (let loop ()
          (let ((c (peek-char port)))
            (unless (or (eof-object? c) (eqv? c stop))
              (write-char (read-char port) out)
              (loop))))
        (get-output-string out)))

    ;;; Serialization

    (define (ident->text x)
      (let ((out (open-output-string))
            (scheme (ident-scheme-raw x))
            (user (ident-user-raw x))
            (host (ident-host-raw x))
            (port (ident-port-raw x))
            (query (ident-query-raw x))
            (fragment (ident-fragment-raw x)))
        (when scheme
          (write-string scheme out)
          (write-char #\: out))
        (when (authority-set? user host port)
          (write-string "//" out)
          (when user
            (write-string user out)
            (write-char #\@ out))
          (when host
            (write-string host out))
          (when port
            (write-char #\: out)
            (write-string (number->string port) out)))
        (write-string (path->string (ident-path-raw x)) out)
        (when query
          (write-char #\? out)
          (write-string query out))
        (when fragment
          (write-char #\# out)
          (write-string fragment out))
        (get-output-string out)))

    ;;; Getters

    (define (get-scheme who iri? x)
      (check-ident who iri? x)
      (unless (ident-scheme-raw x)
        (fail who "relative reference has no scheme" x))
      (string-copy (ident-scheme-raw x)))

    ;; `which` is one of `user`, `host`, `query`, and `fragment`
    (define (get-component who iri? x which)
      (check-ident who iri? x)
      (let ((c (case which
                 ((user) (ident-user-raw x))
                 ((host) (ident-host-raw x))
                 ((query) (ident-query-raw x))
                 (else (ident-fragment-raw x)))))
        (and c (string-copy c))))

    (define (get-port who iri? x)
      (check-ident who iri? x)
      (ident-port-raw x))

    (define (get-authority who iri? x)
      (check-ident who iri? x)
      (values (get-component who iri? x 'user)
              (get-component who iri? x 'host)
              (ident-port-raw x)))

    (define (get-username+password who iri? x)
      (check-ident who iri? x)
      (let ((user (ident-user-raw x)))
        (if (not user)
            (values #f #f)
            (let ((colon (index-of user #\: 0 (string-length user))))
              (if colon
                  (values (substring user 0 colon)
                          (substring user (+ colon 1) (string-length user)))
                  (values (string-copy user) #f))))))

    (define (get-path who iri? x)
      (check-ident who iri? x)
      (ident-path-raw x))

    ;;; Setters

    (define (copy-ident x scheme user host port path query fragment)
      (new-ident (ident-iri-flag x) scheme user host port path query fragment))

    (define (set-scheme who iri? x str)
      (check-ident who iri? x)
      (unless (ident-scheme-raw x)
        (fail who "relative reference has no scheme" x str))
      (unless (string? str)
        (fail who "not a string" str))
      (copy-ident x (parse-scheme who str (string-length str))
                  (ident-user-raw x) (ident-host-raw x) (ident-port-raw x)
                  (ident-path-raw x) (ident-query-raw x) (ident-fragment-raw x)))

    ;; Canonical text of a string given for a component of the identifier
    (define (component-text who iri? which obj)
      (cond ((not obj) #f)
            ((string? obj)
             (if (and (eq? which 'host)
                      (> (string-length obj) 0)
                      (char=? (string-ref obj 0) #\[))
                 (if (valid-ip-literal? obj)
                     (string-copy obj)
                     (fail who "invalid IP literal" obj))
                 (canon who (profile-for iri?) which obj 0 #t)))
            (else (fail who "not a string" obj))))

    (define (port-value who obj)
      (cond ((not obj) #f)
            ((and (exact-integer? obj) (>= obj 0)) obj)
            (else (fail who "not a non-negative integer" obj))))

    ;; Sets the three authority components, checking that they do not conflict with the path
    (define (set-authority* who x user host port irritants)
      (unless (compatible-authority? x user host port)
        (apply fail who "authority conflicts with path" irritants))
      (copy-ident x (ident-scheme-raw x) user host port (ident-path-raw x)
                  (ident-query-raw x) (ident-fragment-raw x)))

    (define (set-user who iri? x obj)
      (check-ident who iri? x)
      (set-authority* who x (component-text who iri? 'user obj)
                      (ident-host-raw x) (ident-port-raw x) (list x obj)))

    (define (set-host who iri? x obj)
      (check-ident who iri? x)
      (set-authority* who x (ident-user-raw x) (component-text who iri? 'host obj)
                      (ident-port-raw x) (list x obj)))

    (define (set-port who iri? x obj)
      (check-ident who iri? x)
      (set-authority* who x (ident-user-raw x) (ident-host-raw x) (port-value who obj)
                      (list x obj)))

    (define (set-authority who iri? x user host port)
      (check-ident who iri? x)
      (set-authority* who x (component-text who iri? 'user user)
                      (component-text who iri? 'host host) (port-value who port)
                      (list x user host port)))

    (define (set-query who iri? x obj)
      (check-ident who iri? x)
      (copy-ident x (ident-scheme-raw x) (ident-user-raw x) (ident-host-raw x) (ident-port-raw x)
                  (ident-path-raw x) (component-text who iri? 'query obj) (ident-fragment-raw x)))

    (define (set-fragment who iri? x obj)
      (check-ident who iri? x)
      (copy-ident x (ident-scheme-raw x) (ident-user-raw x) (ident-host-raw x) (ident-port-raw x)
                  (ident-path-raw x) (ident-query-raw x) (component-text who iri? 'fragment obj)))

    ;; Replaces the path of `x` by `path` whose segments are canonical for the profile
    (define (replace-path who x path irritants)
      (unless (compatible-path? path
                                (authority-set? (ident-user-raw x) (ident-host-raw x)
                                                (ident-port-raw x))
                                (not (ident-scheme-raw x)))
        (apply fail who "path conflicts with authority" irritants))
      (copy-ident x (ident-scheme-raw x) (ident-user-raw x) (ident-host-raw x) (ident-port-raw x)
                  path (ident-query-raw x) (ident-fragment-raw x)))

    (define (set-path who iri? x path)
      (check-ident who iri? x)
      (unless (path? path)
        (fail who "not a path" path))
      (let ((pred (profile-predicate (profile-for iri?) 'segment)))
        (replace-path who x
                      (make-path (absolute-path? path)
                                 (map (lambda (seg) (canonicalize-component who seg pred #t #f 0))
                                      (path-segment-list path)))
                      (list x path))))

    (define (segment-ref who iri? x k)
      (check-ident who iri? x)
      (path-ref (ident-path-raw x) k))

    (define (segment-update who iri? x k str)
      (check-ident who iri? x)
      (unless (string? str)
        (fail who "not a string" str))
      (let* ((path (ident-path-raw x))
             (segs (path-segment-list path)))
        (unless (and (exact-integer? k) (<= 0 k) (< k (length segs)))
          (fail who "invalid segment index" k x))
        (let ((seg (canonicalize-component who str (profile-predicate (profile-for iri?) 'segment)
                                           #t #f 0)))
          (when (and (= k 0) (not (absolute-path? path)) (string=? seg ""))
            (fail who "empty initial segment in relative path" x k str))
          (replace-path who x
                        (make-path (absolute-path? path)
                                   (let loop ((l segs) (i 0))
                                     (cond ((null? l) '())
                                           ((= i k) (cons seg (cdr l)))
                                           (else (cons (car l) (loop (cdr l) (+ i 1)))))))
                        (list x k str)))))

    ;;; Equality

    (define (ident-equal? a b)
      (and (equal? (ident-scheme-raw a) (ident-scheme-raw b))
           (equal? (ident-user-raw a) (ident-user-raw b))
           (equal? (ident-host-raw a) (ident-host-raw b))
           (eqv? (ident-port-raw a) (ident-port-raw b))
           (path=? (ident-path-raw a) (ident-path-raw b))
           (equal? (ident-query-raw a) (ident-query-raw b))
           (equal? (ident-fragment-raw a) (ident-fragment-raw b))))

    (define (ident=? who iri? x y rest)
      (check-ident who iri? x)
      (check-ident who iri? y)
      (for-each (lambda (z) (check-ident who iri? z)) rest)
      (let loop ((a x) (l (cons y rest)))
        (or (null? l)
            (and (ident-equal? a (car l))
                 (loop (car l) (cdr l))))))
  )
)
