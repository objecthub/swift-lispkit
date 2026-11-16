;;; SRFI 275 INTERNAL CHARS
;;; URIs, IRIs, and basic paths: code points and character classes
;;;
;;; This library is internal to the implementation of SRFI 275. It provides code point
;;; based character classification as defined by RFC 3986 (URIs) and RFC 3987 (IRIs),
;;; as well as the SRFI 14 character sets exported by `(srfi 275 iri)`, `(srfi 275 uri)`
;;; and `(srfi 275 path)`.
;;;
;;; LispKit strings are sequences of UTF-16 code units. Characters outside of the basic
;;; multilingual plane are therefore represented by surrogate pairs. This library
;;; converts strings into vectors of Unicode code points so that all classification can
;;; be done precisely. The exported SRFI 14 character sets can only describe code
;;; units. Supplementary characters are approximated by including all surrogate code
;;; units in sets which contain supplementary characters.
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

(define-library (srfi 275 internal chars)

  (import (scheme base)
          (srfi 14))

  (export string->codepoints
          codepoint->string
          codepoint-valid?
          cp-alpha?
          cp-digit?
          cp-hex-value
          cp-sub-delim?
          cp-gen-delim?
          cp-reserved?
          cp-uri-unreserved?
          cp-ucschar?
          cp-iprivate?
          cp-iri-unreserved?
          cp-scheme-start?
          cp-scheme-char?
          ;; profiles
          uri-profile
          iri-profile
          profile?
          profile-iri?
          profile-predicate
          ;; character sets
          char-set:uri-unreserved
          char-set:uri
          char-set:gen-delims
          char-set:sub-delims
          char-set:reserved
          char-set:scheme
          char-set:uri-userinfo
          char-set:uri-reg-name
          char-set:uri-segment
          char-set:uri-query
          char-set:uri-fragment
          char-set:iri-private
          char-set:ucschar
          char-set:iri-unreserved
          char-set:iri
          char-set:iri-userinfo
          char-set:iri-reg-name
          char-set:iri-segment
          char-set:iri-query
          char-set:iri-fragment
          char-set:path)

  (begin

    ;;; Code points

    ;; Converts a string into a vector of Unicode code points. Surrogate pairs are
    ;; combined into a single code point; unpaired surrogates are retained as they are.
    (define (string->codepoints str)
      (let* ((chars (string->vector str))   ; string-ref is not constant-time on non-ASCII strings
             (n (vector-length chars))
             (vec (make-vector n 0)))
        (let loop ((i 0) (k 0))
          (if (= i n)
              (if (= k n) vec (vector-copy vec 0 k))
              (let ((u (char->integer (vector-ref chars i))))
                (cond ((and (<= #xD800 u #xDBFF)
                            (< (+ i 1) n)
                            (<= #xDC00 (char->integer (vector-ref chars (+ i 1))) #xDFFF))
                       (vector-set! vec k (+ #x10000
                                             (* (- u #xD800) #x400)
                                             (- (char->integer (vector-ref chars (+ i 1))) #xDC00)))
                       (loop (+ i 2) (+ k 1)))
                      (else
                        (vector-set! vec k u)
                        (loop (+ i 1) (+ k 1)))))))))

    ;; Converts a code point into a string of one or two UTF-16 code units
    (define (codepoint->string cp)
      (if (< cp #x10000)
          (string (integer->char cp))
          (let ((c (- cp #x10000)))
            (string (integer->char (+ #xD800 (quotient c #x400)))
                    (integer->char (+ #xDC00 (remainder c #x400)))))))

    ;; Is `cp` a Unicode scalar value?
    (define (codepoint-valid? cp)
      (and (<= 0 cp #x10FFFF) (not (<= #xD800 cp #xDFFF))))

    ;;; Character classes (on code points)

    (define (cp-alpha? cp)
      (or (<= 65 cp 90) (<= 97 cp 122)))

    (define (cp-digit? cp)
      (<= 48 cp 57))

    ;; Returns the value of a hexadecimal digit or #f for other code points
    (define (cp-hex-value cp)
      (cond ((<= 48 cp 57) (- cp 48))
            ((<= 65 cp 70) (- cp 55))
            ((<= 97 cp 102) (- cp 87))
            (else #f)))

    ;; !$&'()*+,;=
    (define (cp-sub-delim? cp)
      (and (< cp 62)
           (memv cp '(33 36 38 39 40 41 42 43 44 59 61))
           #t))

    ;; :/?#[]@
    (define (cp-gen-delim? cp)
      (and (memv cp '(58 47 63 35 91 93 64)) #t))

    (define (cp-reserved? cp)
      (or (cp-gen-delim? cp) (cp-sub-delim? cp)))

    ;; ALPHA / DIGIT / "-" / "." / "_" / "~"
    (define (cp-uri-unreserved? cp)
      (or (cp-alpha? cp) (cp-digit? cp) (eqv? cp 45) (eqv? cp 46) (eqv? cp 95) (eqv? cp 126)))

    ;; RFC 3987 ucschar
    (define (cp-ucschar? cp)
      (cond ((< cp #xA0) #f)
            ((<= cp #xD7FF) #t)
            ((< cp #xF900) #f)
            ((<= cp #xFDCF) #t)
            ((< cp #xFDF0) #f)
            ((<= cp #xFFEF) #t)
            ((< cp #x10000) #f)
            ((< cp #xE0000) (<= (modulo cp #x10000) #xFFFD))
            ((< cp #xE1000) #f)
            ((<= cp #xEFFFD) #t)
            (else #f)))

    ;; RFC 3987 iprivate
    (define (cp-iprivate? cp)
      (or (<= #xE000 cp #xF8FF)
          (and (<= #xF0000 cp #x10FFFD) (<= (modulo cp #x10000) #xFFFD))))

    ;; Bidirectional formatting characters which are excluded from iunreserved
    (define (cp-iri-illegal-control? cp)
      (or (eqv? cp #x200E) (eqv? cp #x200F) (<= #x202A cp #x202E)))

    ;; RFC 3987 iunreserved
    (define (cp-iri-unreserved? cp)
      (or (cp-uri-unreserved? cp)
          (and (cp-ucschar? cp) (not (cp-iri-illegal-control? cp)))))

    ;; scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
    (define (cp-scheme-start? cp)
      (cp-alpha? cp))

    (define (cp-scheme-char? cp)
      (or (cp-alpha? cp) (cp-digit? cp) (eqv? cp 43) (eqv? cp 45) (eqv? cp 46)))

    ;;; Profiles
    ;;
    ;; A profile bundles the character repertoire of URIs or IRIs. Component predicates
    ;; map code points to booleans; they identify the characters which are permitted
    ;; without percent-encoding in the corresponding component.

    (define-record-type profile
      (make-profile iri? user host segment query fragment)
      profile?
      (iri? profile-iri?)
      (user profile-user)
      (host profile-host)
      (segment profile-segment)
      (query profile-query)
      (fragment profile-fragment))

    (define (make-component-predicates iri? unreserved? private?)
      (let* ((host (lambda (cp) (or (unreserved? cp) (cp-sub-delim? cp))))
             (user (lambda (cp) (or (host cp) (eqv? cp 58))))
             (segment (lambda (cp) (or (user cp) (eqv? cp 64))))
             (fragment (lambda (cp) (or (segment cp) (eqv? cp 47) (eqv? cp 63))))
             (query (if private?
                        (lambda (cp) (or (fragment cp) (private? cp)))
                        fragment)))
        (make-profile iri? user host segment query fragment)))

    (define uri-profile
      (make-component-predicates #f cp-uri-unreserved? #f))

    (define iri-profile
      (make-component-predicates #t cp-iri-unreserved? cp-iprivate?))

    ;; Returns the predicate for component `which`, a symbol out of `user`, `host`,
    ;; `segment`, `query` and `fragment`.
    (define (profile-predicate profile which)
      (case which
        ((user) (profile-user profile))
        ((host) (profile-host profile))
        ((segment) (profile-segment profile))
        ((query) (profile-query profile))
        ((fragment) (profile-fragment profile))
        (else (error "unknown component" which))))

    ;;; Character sets

    ;; Inclusive range of code units
    (define (range lo hi)
      (ucs-range->char-set lo (+ hi 1)))

    ;; Surrogate code units are used as a stand-in for supplementary characters
    (define char-set:surrogates (range #xD800 #xDFFF))

    (define char-set:alpha (char-set-union (range 65 90) (range 97 122)))
    (define char-set:numeric (range 48 57))

    (define char-set:uri-unreserved
      (char-set-union char-set:alpha char-set:numeric (char-set #\- #\. #\_ #\~)))

    (define char-set:gen-delims (char-set #\: #\/ #\? #\# #\[ #\] #\@))
    (define char-set:sub-delims (char-set #\! #\$ #\& #\' #\( #\) #\* #\+ #\, #\; #\=))
    (define char-set:reserved (char-set-union char-set:gen-delims char-set:sub-delims))
    (define char-set:uri (char-set-union char-set:reserved char-set:uri-unreserved))

    (define char-set:ucschar
      (char-set-union (range #xA0 #xD7FF) (range #xF900 #xFDCF) (range #xFDF0 #xFFEF)
                      char-set:surrogates))

    (define char-set:iri-private
      (char-set-union (range #xE000 #xF8FF) char-set:surrogates))

    (define char-set:iri-illegal-controls
      (char-set-union (char-set (integer->char #x200E) (integer->char #x200F))
                      (range #x202A #x202E)))

    (define char-set:iri-unreserved
      (char-set-difference (char-set-union char-set:uri-unreserved char-set:ucschar)
                           char-set:iri-illegal-controls))

    (define char-set:iri
      (char-set-union char-set:reserved char-set:iri-unreserved char-set:iri-private))

    (define char-set:scheme
      (char-set-union char-set:alpha char-set:numeric (char-set #\+ #\- #\.)))

    (define char-set:uri-reg-name
      (char-set-union char-set:uri-unreserved char-set:sub-delims))
    (define char-set:uri-userinfo
      (char-set-union char-set:uri-reg-name (char-set #\:)))
    (define char-set:uri-segment
      (char-set-union char-set:uri-userinfo (char-set #\@)))
    (define char-set:uri-fragment
      (char-set-union char-set:uri-segment (char-set #\/ #\?)))
    (define char-set:uri-query char-set:uri-fragment)

    (define char-set:iri-reg-name
      (char-set-union char-set:iri-unreserved char-set:sub-delims))
    (define char-set:iri-userinfo
      (char-set-union char-set:iri-reg-name (char-set #\:)))
    (define char-set:iri-segment
      (char-set-union char-set:iri-userinfo (char-set #\@)))
    (define char-set:iri-fragment
      (char-set-union char-set:iri-segment (char-set #\/ #\?)))
    (define char-set:iri-query
      (char-set-union char-set:iri-fragment char-set:iri-private))

    ;; Union of `char-set:uri` and `char-set:ucschar`
    (define char-set:path
      (char-set-union char-set:uri char-set:ucschar))
  )
)
