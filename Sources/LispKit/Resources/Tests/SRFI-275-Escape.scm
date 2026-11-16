;;; SRFI 275 PERCENT-ENCODING REGRESSION TEST SUITE
;;;
;;; This is the test suite for percent-encoded data: the procedures `encode-string` and
;;; `decode-string` of library `(srfi 275 utils)`, the rejection of invalid UTF-8
;;; sequences while parsing, and the implicit encoding of the setters of libraries
;;; `(srfi 275 iri)` and `(srfi 275 uri)`. It is based on the test suite of the SRFI 275
;;; sample implementation and on the examples of the SRFI document.
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
        (srfi 275 iri)
        (srfi 275 uri)
        (srfi 275 utils))

(test-begin "SRFI 275: Percent-encoding")

(test-group "Direct encoding and decoding"
  (test-equal "encode all four byte lengths in a single call"
              "a%20b%C3%A9c%E2%99%82d%F0%9F%98%8Ee"
              (encode-string "a béc♂d😎e" (char-set-difference char-set:full char-set:uri)))
  (test-equal "encode with the complement of the URI characters"
              "a%20b%C3%A9c%E2%99%82d%F0%9F%98%8Ee"
              (encode-string "a béc♂d😎e" (char-set-complement char-set:uri)))
  (test-equal "decode everything except ♂ (U+2642, encoded as %E2%99%82)"
              "a béc%E2%99%82d😎e"
              (decode-string "a%20b%C3%A9c%E2%99%82d%F0%9F%98%8Ee"
                             (char-set-complement (char-set #\♂))))
  (test-equal "direct encoding treats percent as a character"
              "foo%2520bar"
              (encode-string "foo%20bar" (char-set-difference char-set:full char-set:uri)))
  (test-equal "escaped percent round trip during direct decoding"
              "foo%20bar"
              (decode-string "foo%2520bar" char-set:full))
  (test-equal "default character set" "user%2540host" (encode-string "user%40host"))
  (test-equal "reserved characters are escaped by default" "user%40host" (encode-string "user@host"))
  (test-equal "unreserved characters are retained by default" "a-b.c_d~e" (encode-string "a-b.c_d~e"))
  (test-equal "non-ASCII characters are unreserved in IRIs" "é" (encode-string "é"))
  (test-equal "bidirectional control characters are escaped by default" "%E2%80%8E" (encode-string "\x200E;"))
  (test-equal "everything can be encoded"
              "%75%73%65%72%40%68%6F%73%74"
              (encode-string "user@host" char-set:full))
  (test-equal "percent is always escaped" "%25%61" (encode-string "%a" (char-set-complement (char-set #\%))))
  (test-equal "percent is escaped even if not in the set" "%25a" (encode-string "%a" (char-set #\b)))
  (test-equal "nothing to encode" "abc" (encode-string "abc" char-set:empty))
  (test-equal "empty string" "" (encode-string ""))
  (test-equal "supplementary characters are encoded as a unit" "%F0%9F%98%8E"
              (encode-string "😎" char-set:full))
  (test-equal "decode the encoded string" "user%40host" (decode-string (encode-string "user%40host")))
  (test-equal "decode everything" "a b" (decode-string "a%20b"))
  (test-equal "decode lowercase hex digits" "é" (decode-string "%c3%a9"))
  (test-equal "undecoded bytes are normalized" "%C3%A9" (decode-string "%c3%a9" char-set:empty))
  (test-equal "decode supplementary characters" "😎" (decode-string "%F0%9F%98%8E"))
  (test-equal "characters which are not in the set are retained" "/%41"
              (decode-string "%2F%41" (char-set #\/)))
  (test-equal "literal characters are retained" "a b/ü" (decode-string "a b/ü"))
  (test-equal "empty string" "" (decode-string ""))
  (test-equal "encoding and decoding with all characters of a string"
              "a b\x00E9;\x2603;😎%"
              (decode-string (encode-string "a b\x00E9;\x2603;😎%" char-set:full) char-set:full))
  (test-error "overlong 4-byte encoding of U+0020" (decode-string "a%E0%80%80%A0b"))
  (test-error "incomplete 4-byte sequence" (decode-string "a%F0%9F%98"))
  (test-error "incomplete percent-encoding" (decode-string "a%"))
  (test-error "incomplete percent-encoding (one digit)" (decode-string "a%4"))
  (test-error "invalid hex digits" (decode-string "%zz"))
  (test-error "not a string" (encode-string 'a))
  (test-error "not a character set" (encode-string "a" "b"))
  (test-error "not a string" (decode-string #\a))
  (test-error "not a character set" (decode-string "a" 'b)))

(test-group "Overlong and invalid percent-encoded sequences in decode-string"
  (define invalid-strings
    '(("overlong 2-byte encoding of U+0020 (space)" "a%C0%A0b")
      ("overlong 3-byte encoding of U+0020 (space)" "a%E0%80%A0b")
      ("overlong 4-byte encoding of U+0020 (space)" "a%E0%80%80%A0b")
      ("overlong 3-byte encoding of U+00E9 (é)" "a%E0%80%A9b")
      ("incomplete 2-byte sequence (missing continuation byte)" "a%C0b")
      ("incomplete 3-byte sequence (missing third byte for ♂ U+2642)" "a%E2%99")
      ("incomplete 4-byte sequence (missing fourth byte for 😎 U+1F60E)" "a%F0%9F%98")
      ("non-encoded character where a continuation byte is expected" "a%F0%9F%98x")
      ("UTF-8 continuation byte without a leading byte" "a%A9")))
  (for-each (lambda (case)
              (test-error (car case) (decode-string (cadr case) char-set:full))
              (test-error (string-append (car case) " (no byte decoded)")
                          (decode-string (cadr case) char-set:empty)))
            invalid-strings)
  (test-error "encoded surrogate" (decode-string "%ED%A0%80"))
  (test-error "encoded surrogate pair" (decode-string "%ED%A0%BD%ED%B8%8E"))
  (test-error "code point above U+10FFFF" (decode-string "%F4%90%80%80"))
  (test-error "five-byte sequence" (decode-string "%F8%88%80%80%80"))
  (test-error "invalid start byte 0xC1" (decode-string "%C1%81"))
  (test-error "invalid start byte 0xF5" (decode-string "%F5%80%80%80"))
  (test-error "invalid start byte 0xFF" (decode-string "%FF"))
  (test-equal "lowest code point of three bytes (U+0800)" "\x0800;" (decode-string "%E0%A0%80"))
  (test-equal "highest code point of two bytes (U+07FF)" "\x07FF;" (decode-string "%DF%BF"))
  (test-equal "lowest code point of four bytes (U+10000)" "𐀀" (decode-string "%F0%90%80%80"))
  (test-equal "highest code point (U+10FFFF)" "􏿿" (decode-string "%F4%8F%BF%BF"))
  (test-equal "U+D7FF" "\xD7FF;" (decode-string "%ED%9F%BF"))
  (test-equal "U+E000" "\xE000;" (decode-string "%EE%80%80")))

(test-group "Invalid percent-encoded sequences in identifiers"
  (define invalid-strings
    '(("overlong 2-byte encoding of U+0020 (space)" "a%C0%A0b")
      ("overlong 3-byte encoding of U+0020 (space)" "a%E0%80%A0b")
      ("overlong 4-byte encoding of U+0020 (space)" "a%E0%80%80%A0b")
      ("overlong 3-byte encoding of U+00E9 (é)" "a%E0%80%A9b")
      ("incomplete 2-byte sequence (missing continuation byte)" "a%C0b")
      ("incomplete 3-byte sequence (missing third byte for ♂ U+2642)" "a%E2%99")
      ("incomplete 4-byte sequence (missing fourth byte for 😎 U+1F60E)" "a%F0%9F%98")
      ("non-encoded character where a continuation byte is expected" "a%F0%9F%98x")
      ("UTF-8 continuation byte without a leading byte" "a%A9")))
  ;; contexts: a label, the text before and the text after the component
  (define contexts
    '(("user component" "foo://" "@")
      ("reference user component" "//" "@")
      ("host component" "foo://" "/")
      ("reference host component" "//" "/")
      ("path component" "foo:" "/")
      ("reference path component" "" "/")
      ("query component" "foo:?" "")
      ("reference query component" "?" "")
      ("fragment component" "foo:#" "")
      ("reference fragment component" "#" "")))
  (define (check-context label parse context text description)
    (test-error (string-append label " " (car context) ": " description)
                (parse (string-append (cadr context) text (caddr context)))))
  (for-each
    (lambda (context)
      (for-each (lambda (case) (check-context "URI" string->uri context (cadr case) (car case)))
                invalid-strings)
      (for-each (lambda (case) (check-context "IRI" string->iri context (cadr case) (car case)))
                invalid-strings))
    contexts)
  ;; valid sequences are accepted in the same contexts
  (for-each
    (lambda (context)
      (for-each
        (lambda (text)
          (test-assert (string-append "valid URI " (car context) ": " text)
                       (string->uri (string-append (cadr context) text (caddr context))))
          (test-assert (string-append "valid IRI " (car context) ": " text)
                       (string->iri (string-append (cadr context) text (caddr context)))))
        '("a%C3%A9b" "a%E2%99%82b" "a%F0%9F%98%8Eb" "a%41b" "%e2%99%82")))
    contexts))

(test-group "Examples of the specification"
  (define example0 (string->iri "http://example.org/some/where/place"))
  (test-equal "http://person!pass@example.org/some/where/place"
              (iri->string (update-iri-user example0 "person!pass")))
  (test-equal "http://person%21pass@example.org/some/where/place"
              (iri->string (update-iri-user example0 (encode-string "person!pass"))))
  (test-equal "user%2540host"
              (iri-user (update-iri-user example0 (encode-string "user%40host")))))

;;; Properties of the implicit encoding of setters

(define sample-iri (string->iri "http://example.org/some/where/place?x#a"))
(define sample-iri-ref (string->iri "//example.org/some/where/place?x#a"))
(define sample-uri (string->uri "http://example.org/some/where/place?x#a"))
(define sample-uri-ref (string->uri "//example.org/some/where/place?x#a"))

;; The characters of `cset` as a string. Only the code units in the vector `indices` (in
;; ascending order) are considered. (Lists with tens of thousands of elements are avoided
;; on purpose.)
(define (cset->string cset indices)
  (let ((out (open-output-string)))
    (vector-for-each
      (lambda (i)
        (let ((ch (integer->char i)))
          (when (char-set-contains? cset ch)
            (write-char ch out))))
      indices)
    (get-output-string out)))

;; A vector with all code units which satisfy `include?`. Surrogate code units cannot be
;; percent-encoded and are therefore excluded.
(define (make-indices include?)
  (let ((vec (make-vector #x10000 0)))
    (let loop ((i 0) (k 0))
      (cond ((= i #x10000)
             (vector-copy vec 0 k))
            ((and (not (<= #xD800 i #xDFFF)) (include? i))
             (vector-set! vec k i)
             (loop (+ i 1) (+ k 1)))
            (else
             (loop (+ i 1) k))))))

;; Boundaries of the ranges of the character sets of RFC 3987, and of the basic multilingual plane
(define edge-points
  '(#xA0 #xD7FF #xE000 #xF8FF #xF900 #xFDCF #xFDF0 #xFFEF #x200E #x200F #x202A #x202E #xFFFF))

(define (near-edge? i)
  (let loop ((l edge-points))
    (and (pair? l)
         (or (<= (abs (- i (car l))) 1)
             (loop (cdr l))))))

(define (all-units? i) #t)
(define (ascii-units? i) (< i 128))

;; All characters below 256, every 11th character above, and the characters around the boundaries
(define (wide-units? i) (or (< i 256) (= (remainder i 11) 0) (near-edge? i)))

;; All characters below 256, every 37th character above, and the characters around the boundaries
(define (sampled-units? i) (or (< i 256) (= (remainder i 37) 0) (near-edge? i)))

(define all-indices (make-indices all-units?))
(define ascii-indices (make-indices ascii-units?))
(define wide-indices (make-indices wide-units?))
(define sampled-indices (make-indices sampled-units?))

(define hex-digits "0123456789ABCDEF")

;; The percent-encoded UTF-8 encoding of a character
(define (escape-char ch)
  (let ((bytes (string->utf8 (string ch)))
        (out (open-output-string)))
    (do ((i 0 (+ i 1)))
        ((= i (bytevector-length bytes)))
      (let ((b (bytevector-u8-ref bytes i)))
        (write-char #\% out)
        (write-char (string-ref hex-digits (quotient b 16)) out)
        (write-char (string-ref hex-digits (remainder b 16)) out)))
    (get-output-string out)))

;; The percent-encoded UTF-8 encoding of all characters of a string
(define (escape-string str)
  (let ((out (open-output-string)))
    (vector-for-each (lambda (ch) (write-string (escape-char ch) out)) (string->vector str))
    (get-output-string out)))

;; Setting a component to the characters of `cset` does not encode any of them
(define (clear-property ident get update cset indices)
  (let ((text (cset->string cset indices)))
    (string=? (get (update ident text)) text)))

;; Setting a component to the percent-encoded characters of `cset` retains the encoding
(define (escaped-property ident get update cset indices)
  (let ((text (escape-string (cset->string cset indices))))
    (string=? (get (update ident text)) text)))

;; Setting a component to the characters of `cset` percent-encodes all of them. The percent
;; sign introduces a percent-encoded byte, and is therefore excluded.
(define (encoding-property ident get update cset indices)
  (let ((text (cset->string (char-set-delete cset #\%) indices)))
    (string=? (get (update ident text)) (escape-string text))))

;; Tests the properties of the setter of a component. The properties are checked for all
;; a sample of the characters of the basic multilingual plane for non-relative identifiers
;; (including all boundaries of the character sets), and for the ASCII characters for relative
;; references. (An exhaustive check of all characters takes about 18 seconds.)
(define (check-component kind label get update allowed)
  (let ((illegal (char-set-complement allowed))
        (non-relative (if (eq? kind 'iri) sample-iri sample-uri))
        (relative (if (eq? kind 'iri) sample-iri-ref sample-uri-ref)))
    (test-group (string-append (if (eq? kind 'iri) "IRI " "URI ") label)
      (test-assert "permissible characters are not encoded in non-relative identifier"
                   (clear-property non-relative get update allowed wide-indices))
      (test-assert "encoded characters are retained in non-relative identifier"
                   (escaped-property non-relative get update illegal sampled-indices))
      (test-assert "illegal characters are encoded in non-relative identifier"
                   (encoding-property non-relative get update illegal wide-indices))
      (test-assert "permissible characters are not encoded in relative reference"
                   (clear-property relative get update allowed ascii-indices))
      (test-assert "encoded characters are retained in relative reference"
                   (escaped-property relative get update illegal ascii-indices))
      (test-assert "illegal characters are encoded in relative reference"
                   (encoding-property relative get update illegal ascii-indices)))))

(check-component 'uri "user setters" uri-user update-uri-user char-set:uri-userinfo)
(check-component 'uri "host setters" uri-host update-uri-host char-set:uri-reg-name)
(check-component 'uri "query setters" uri-query update-uri-query char-set:uri-query)
(check-component 'uri "fragment setters" uri-fragment update-uri-fragment char-set:uri-fragment)
(check-component 'iri "user setters" iri-user update-iri-user char-set:iri-userinfo)
(check-component 'iri "host setters" iri-host update-iri-host char-set:iri-reg-name)
(check-component 'iri "query setters" iri-query update-iri-query char-set:iri-query)
(check-component 'iri "fragment setters" iri-fragment update-iri-fragment char-set:iri-fragment)

(test-group "Implicit encoding of supplementary characters"
  (test-equal "IRI user" "😎" (iri-user (update-iri-user sample-iri "😎")))
  (test-equal "IRI query" "a😎b" (iri-query (update-iri-query sample-iri "a😎b")))
  (test-equal "IRI fragment" "😎" (iri-fragment (update-iri-fragment sample-iri "😎")))
  (test-equal "IRI path segment" "😎" (iri-path-segment-ref (iri-path-segment-update sample-iri 0 "😎") 0))
  (test-equal "URI user" "%F0%9F%98%8E" (uri-user (update-uri-user sample-uri "😎")))
  (test-equal "URI query" "a%F0%9F%98%8Eb" (uri-query (update-uri-query sample-uri "a😎b")))
  (test-equal "URI fragment" "%F0%9F%98%8E" (uri-fragment (update-uri-fragment sample-uri "😎")))
  (test-equal "URI fragment with a character of the basic multilingual plane"
              "%E2%99%82" (uri-fragment (update-uri-fragment sample-uri "♂")))
  (test-equal "IRI private use character in the query" "\xE000;"
              (iri-query (update-iri-query sample-iri "\xE000;")))
  (test-equal "IRI private use character in the fragment" "%EE%80%80"
              (iri-fragment (update-iri-fragment sample-iri "\xE000;"))))

(test-group "Invalid percent-encodings in setters"
  (test-error (update-iri-user sample-iri "a%"))
  (test-error (update-iri-user sample-iri "a%4"))
  (test-error (update-iri-user sample-iri "a%zz"))
  (test-error (update-iri-host sample-iri "a%C0%A0"))
  (test-error (update-iri-query sample-iri "%E2%99"))
  (test-error (update-iri-fragment sample-iri "%A9"))
  (test-error (update-uri-user sample-uri "a%"))
  (test-error (update-uri-host sample-uri "a%C0%A0"))
  (test-error (update-uri-query sample-uri "%E2%99"))
  (test-error (update-uri-fragment sample-uri "%A9"))
  (test-equal "valid escapes are normalized" "%C3%A9" (iri-fragment (update-iri-fragment sample-iri "%c3%a9"))))

(test-group "IRI scheme"
  (test-assert "scheme characters are not encoded"
    (let ((text (string-append "a" (cset->string char-set:scheme all-indices))))
      (string=? (iri-scheme (update-iri-scheme sample-iri text)) text)))
  (test-equal "ordinary scheme" "urn" (iri-scheme (update-iri-scheme sample-iri "urn")))
  (test-equal "scheme with all permitted characters" "a+b-c.d1"
              (iri-scheme (update-iri-scheme sample-iri "a+b-c.d1")))
  (test-error "relative reference cannot have scheme set" (update-iri-scheme sample-iri-ref "urn"))
  (test-error (update-iri-scheme sample-iri "sch%20eme"))
  (test-error (update-iri-scheme sample-iri "%20scheme"))
  (test-error (update-iri-scheme sample-iri "scheme%20"))
  (test-error (update-iri-scheme sample-iri "£dosh"))
  (test-error (update-iri-scheme sample-iri "crêpes"))
  (test-error (update-iri-scheme sample-iri-ref "sch%20eme"))
  (test-error (update-iri-scheme sample-iri-ref "%20scheme"))
  (test-error (update-iri-scheme sample-iri-ref "scheme%20"))
  (test-error (update-iri-scheme sample-iri-ref "£dosh"))
  (test-error (update-iri-scheme sample-iri-ref "crêpes"))
  (test-error "empty scheme" (update-iri-scheme sample-iri ""))
  (test-error "unset scheme" (update-iri-scheme sample-iri #f))
  (test-error "scheme starting with a digit" (update-iri-scheme sample-iri "1a"))
  (test-error "scheme starting with a plus" (update-iri-scheme sample-iri "+a"))
  (test-error "scheme with space" (update-iri-scheme sample-iri "a b"))
  (test-error "scheme with colon" (update-iri-scheme sample-iri "a:b"))
  (test-error "not a string" (update-iri-scheme sample-iri 'urn)))

(test-group "URI scheme"
  (test-assert "scheme characters are not encoded"
    (let ((text (string-append "a" (cset->string char-set:scheme all-indices))))
      (string=? (uri-scheme (update-uri-scheme sample-uri text)) text)))
  (test-equal "ordinary scheme" "urn" (uri-scheme (update-uri-scheme sample-uri "urn")))
  (test-error "relative reference cannot have scheme set" (update-uri-scheme sample-uri-ref "urn"))
  (test-error (update-uri-scheme sample-uri "sch%20eme"))
  (test-error (update-uri-scheme sample-uri "%20scheme"))
  (test-error (update-uri-scheme sample-uri "scheme%20"))
  (test-error (update-uri-scheme sample-uri "£dosh"))
  (test-error (update-uri-scheme sample-uri "crêpes"))
  (test-error (update-uri-scheme sample-uri-ref "sch%20eme"))
  (test-error (update-uri-scheme sample-uri-ref "%20scheme"))
  (test-error (update-uri-scheme sample-uri-ref "scheme%20"))
  (test-error (update-uri-scheme sample-uri-ref "£dosh"))
  (test-error (update-uri-scheme sample-uri-ref "crêpes"))
  (test-error "empty scheme" (update-uri-scheme sample-uri ""))
  (test-error "unset scheme" (update-uri-scheme sample-uri #f)))

(test-end)
