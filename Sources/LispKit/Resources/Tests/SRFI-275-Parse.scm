;;; SRFI 275 PARSING REGRESSION TEST SUITE
;;;
;;; This is the test suite for the parsing, serialization and component access
;;; procedures of libraries `(srfi 275 iri)` and `(srfi 275 uri)`. It is based on the test
;;; suite of the SRFI 275 sample implementation and on the examples of the SRFI document.
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
        (srfi 275 path))

;;; Test data of the sample implementation. Each row consists of a description, the text
;;; to parse, the expected scheme (omitted for relative references), user, host, port,
;;; path, query and fragment.

(define ip-address-segments
  `(;; :: alone
    ("IPv6 address (:: alone)"  "http://[::]" "http" #f "[::]" #f "" #f #f)
    ;; Full address of hex eight segments
    ("IPv6 address (eight segments)" "http://[AAAA:BBBB:CCCC:DDDD:EEEE:FFFF:1111:2222]" "http" #f "[AAAA:BBBB:CCCC:DDDD:EEEE:FFFF:1111:2222]" #f "" #f #f)
    ("IPv6 address (six segments : IPv4)" "http://[AAAA:BBBB:CCCC:DDDD:EEEE:FFFF:10.0.12.255]" "http" #f "[AAAA:BBBB:CCCC:DDDD:EEEE:FFFF:10.0.12.255]" #f "" #f #f)
    ;; :: exactly seven hex segments
    ("IPv6 address (seven segments)" "http://[::BBBB:CCCC:DDDD:EEEE:FFFF:1111:2222]" "http" #f "[::BBBB:CCCC:DDDD:EEEE:FFFF:1111:2222]" #f "" #f #f)
    ("IPv6 address (five segments : IPv4)" "http://[::BBBB:CCCC:DDDD:EEEE:FFFF:10.0.12.255]" "http" #f "[::BBBB:CCCC:DDDD:EEEE:FFFF:10.0.12.255]" #f "" #f #f)
    ;; at most one hex segment :: exactly six segments
    ("IPv6 address (zero :: six segments)" "http://[::CCCC:DDDD:EEEE:FFFF:1111:2222]"     "http" #f "[::CCCC:DDDD:EEEE:FFFF:1111:2222]"     #f "" #f #f)
    ("IPv6 address (one :: six segments)"  "http://[AAAA::CCCC:DDDD:EEEE:FFFF:1111:2222]" "http" #f "[AAAA::CCCC:DDDD:EEEE:FFFF:1111:2222]" #f "" #f #f)
    ("IPv6 address (zero :: four segments : IPv4)" "http://[::CCCC:DDDD:EEEE:FFFF:10.0.12.255]"     "http" #f "[::CCCC:DDDD:EEEE:FFFF:10.0.12.255]"     #f "" #f #f)
    ("IPv6 address (one :: four segments : IPv4)"  "http://[AAAA::CCCC:DDDD:EEEE:FFFF:10.0.12.255]" "http" #f "[AAAA::CCCC:DDDD:EEEE:FFFF:10.0.12.255]" #f "" #f #f)
    ;; at most two hex segments :: exactly five hex segments
    ("IPv6 address (zero :: five segments)" "http://[::DDDD:EEEE:FFFF:1111:2222]"          "http" #f "[::DDDD:EEEE:FFFF:1111:2222]"          #f "" #f #f)
    ("IPv6 address (one :: five segments)"  "http://[AAAA::DDDD:EEEE:FFFF:1111:2222]"      "http" #f "[AAAA::DDDD:EEEE:FFFF:1111:2222]"      #f "" #f #f)
    ("IPv6 address (two :: five segments)"  "http://[AAAA:BBBB::DDDD:EEEE:FFFF:1111:2222]" "http" #f "[AAAA:BBBB::DDDD:EEEE:FFFF:1111:2222]" #f "" #f #f)
    ("IPv6 address (zero :: three segments : IPv4)" "http://[::DDDD:EEEE:FFFF:10.0.12.255]"          "http" #f "[::DDDD:EEEE:FFFF:10.0.12.255]"          #f "" #f #f)
    ("IPv6 address (one :: three segments : IPv4)"  "http://[AAAA::DDDD:EEEE:FFFF:10.0.12.255]"      "http" #f "[AAAA::DDDD:EEEE:FFFF:10.0.12.255]"      #f "" #f #f)
    ("IPv6 address (two :: three segments : IPv4)"  "http://[AAAA:BBBB::DDDD:EEEE:FFFF:10.0.12.255]" "http" #f "[AAAA:BBBB::DDDD:EEEE:FFFF:10.0.12.255]" #f "" #f #f)
    ;; at most three hex segments :: exactly four hex segments
    ("IPv6 address (zero :: four segments)"  "http://[::EEEE:FFFF:1111:2222]"               "http" #f "[::EEEE:FFFF:1111:2222]"               #f "" #f #f)
    ("IPv6 address (one :: four segments)"   "http://[AAAA::EEEE:FFFF:1111:2222]"           "http" #f "[AAAA::EEEE:FFFF:1111:2222]"           #f "" #f #f)
    ("IPv6 address (two :: four segments)"   "http://[AAAA:BBBB::EEEE:FFFF:1111:2222]"      "http" #f "[AAAA:BBBB::EEEE:FFFF:1111:2222]"      #f "" #f #f)
    ("IPv6 address (three :: four segments)" "http://[AAAA:BBBB:CCCC::EEEE:FFFF:1111:2222]" "http" #f "[AAAA:BBBB:CCCC::EEEE:FFFF:1111:2222]" #f "" #f #f)
    ("IPv6 address (zero :: two segments : IPv4)"  "http://[::EEEE:FFFF:10.0.12.255]"               "http" #f "[::EEEE:FFFF:10.0.12.255]"               #f "" #f #f)
    ("IPv6 address (one :: two segments : IPv4)"   "http://[AAAA::EEEE:FFFF:10.0.12.255]"           "http" #f "[AAAA::EEEE:FFFF:10.0.12.255]"           #f "" #f #f)
    ("IPv6 address (two :: two segments : IPv4)"   "http://[AAAA:BBBB::EEEE:FFFF:10.0.12.255]"      "http" #f "[AAAA:BBBB::EEEE:FFFF:10.0.12.255]"      #f "" #f #f)
    ("IPv6 address (three :: two segments : IPv4)" "http://[AAAA:BBBB:CCCC::EEEE:FFFF:10.0.12.255]" "http" #f "[AAAA:BBBB:CCCC::EEEE:FFFF:10.0.12.255]" #f "" #f #f)
    ;; at most four hex segments :: exactly three hex segments
    ("IPv6 address (zero :: three segments)"  "http://[::FFFF:1111:2222]"                    "http" #f "[::FFFF:1111:2222]"                    #f "" #f #f)
    ("IPv6 address (one :: three segments)"   "http://[AAAA::FFFF:1111:2222]"                "http" #f "[AAAA::FFFF:1111:2222]"                #f "" #f #f)
    ("IPv6 address (two :: three segments)"   "http://[AAAA:BBBB::FFFF:1111:2222]"           "http" #f "[AAAA:BBBB::FFFF:1111:2222]"           #f "" #f #f)
    ("IPv6 address (three :: three segments)" "http://[AAAA:BBBB:CCCC::FFFF:1111:2222]"      "http" #f "[AAAA:BBBB:CCCC::FFFF:1111:2222]"      #f "" #f #f)
    ("IPv6 address (four :: three segments)"  "http://[AAAA:BBBB:CCCC:DDDD::FFFF:1111:2222]" "http" #f "[AAAA:BBBB:CCCC:DDDD::FFFF:1111:2222]" #f "" #f #f)
    ("IPv6 address (zero :: one segment : IPv4)"  "http://[::FFFF:10.0.12.255]"                    "http" #f "[::FFFF:10.0.12.255]"                    #f "" #f #f)
    ("IPv6 address (one :: one segment : IPv4)"   "http://[AAAA::FFFF:10.0.12.255]"                "http" #f "[AAAA::FFFF:10.0.12.255]"                #f "" #f #f)
    ("IPv6 address (two :: one segment : IPv4)"   "http://[AAAA:BBBB::FFFF:10.0.12.255]"           "http" #f "[AAAA:BBBB::FFFF:10.0.12.255]"           #f "" #f #f)
    ("IPv6 address (three :: one segment : IPv4)" "http://[AAAA:BBBB:CCCC::FFFF:10.0.12.255]"      "http" #f "[AAAA:BBBB:CCCC::FFFF:10.0.12.255]"      #f "" #f #f)
    ("IPv6 address (four :: one segment : IPv4)"  "http://[AAAA:BBBB:CCCC:DDDD::FFFF:10.0.12.255]" "http" #f "[AAAA:BBBB:CCCC:DDDD::FFFF:10.0.12.255]" #f "" #f #f)
    ;; at most five hex segments :: exactly two hex segments
    ("IPv6 address (zero :: two segments)"  "http://[::1111:2222]"                         "http" #f "[::1111:2222]"                         #f "" #f #f)
    ("IPv6 address (one :: two segments)"   "http://[AAAA::1111:2222]"                     "http" #f "[AAAA::1111:2222]"                     #f "" #f #f)
    ("IPv6 address (two :: two segments)"   "http://[AAAA:BBBB::1111:2222]"                "http" #f "[AAAA:BBBB::1111:2222]"                #f "" #f #f)
    ("IPv6 address (three :: two segments)" "http://[AAAA:BBBB:CCCC::1111:2222]"           "http" #f "[AAAA:BBBB:CCCC::1111:2222]"           #f "" #f #f)
    ("IPv6 address (four :: two segments)"  "http://[AAAA:BBBB:CCCC:DDDD::1111:2222]"      "http" #f "[AAAA:BBBB:CCCC:DDDD::1111:2222]"      #f "" #f #f)
    ("IPv6 address (five :: two segments)"  "http://[AAAA:BBBB:CCCC:DDDD:EEEE::1111:2222]" "http" #f "[AAAA:BBBB:CCCC:DDDD:EEEE::1111:2222]" #f "" #f #f)
    ("IPv6 address (zero :: IPv4)"  "http://[::10.0.12.255]"                         "http" #f "[::10.0.12.255]"                         #f "" #f #f)
    ("IPv6 address (one :: IPv4)"   "http://[AAAA::10.0.12.255]"                     "http" #f "[AAAA::10.0.12.255]"                     #f "" #f #f)
    ("IPv6 address (two :: IPv4)"   "http://[AAAA:BBBB::10.0.12.255]"                "http" #f "[AAAA:BBBB::10.0.12.255]"                #f "" #f #f)
    ("IPv6 address (three :: IPv4)" "http://[AAAA:BBBB:CCCC::10.0.12.255]"           "http" #f "[AAAA:BBBB:CCCC::10.0.12.255]"           #f "" #f #f)
    ("IPv6 address (four :: IPv4)"  "http://[AAAA:BBBB:CCCC:DDDD::10.0.12.255]"      "http" #f "[AAAA:BBBB:CCCC:DDDD::10.0.12.255]"      #f "" #f #f)
    ("IPv6 address (five :: IPv4)"  "http://[AAAA:BBBB:CCCC:DDDD:EEEE::10.0.12.255]" "http" #f "[AAAA:BBBB:CCCC:DDDD:EEEE::10.0.12.255]" #f "" #f #f)
    ;; at most six hex segments :: exactly one hex segment
    ("IPv6 address (zero :: one segment)"  "http://[::2222]"                              "http" #f "[::2222]"                              #f "" #f #f)
    ("IPv6 address (one :: one segment)"   "http://[AAAA::2222]"                          "http" #f "[AAAA::2222]"                          #f "" #f #f)
    ("IPv6 address (two :: one segment)"   "http://[AAAA:BBBB::2222]"                     "http" #f "[AAAA:BBBB::2222]"                     #f "" #f #f)
    ("IPv6 address (three :: one segment)" "http://[AAAA:BBBB:CCCC::2222]"                "http" #f "[AAAA:BBBB:CCCC::2222]"                #f "" #f #f)
    ("IPv6 address (four :: one segment)"  "http://[AAAA:BBBB:CCCC:DDDD::2222]"           "http" #f "[AAAA:BBBB:CCCC:DDDD::2222]"           #f "" #f #f)
    ("IPv6 address (five :: one segment)"  "http://[AAAA:BBBB:CCCC:DDDD:EEEE::2222]"      "http" #f "[AAAA:BBBB:CCCC:DDDD:EEEE::2222]"      #f "" #f #f)
    ("IPv6 address (six :: one segment)"   "http://[AAAA:BBBB:CCCC:DDDD:EEEE:FFFF::2222]" "http" #f "[AAAA:BBBB:CCCC:DDDD:EEEE:FFFF::2222]" #f "" #f #f)
    ;; positive cases suggested by Ivan Raikov
    ("IPv6 address (all-zeros unspecified address)" "http://[::]/" "http" #f "[::]" #f "/" #f #f)
    ("IPv6 address (loopback)"                      "http://[::1]/" "http" #f "[::1]" #f "/" #f #f)
    ("IPv6 address (trailing ::)"                   "http://[1::]/" "http" #f "[1::]" #f "/" #f #f)
    ("IPv6 address (trailing :: with prefix)"       "http://[2001:db8::]/" "http" #f "[2001:db8::]" #f "/" #f #f)
    ("IPv6 address (leading ::)"                    "http://[::2001:db8]/" "http" #f "[::2001:db8]" #f "/" #f #f)
    ("IPv6 address (IPv4-mapped)"                   "http://[::ffff:192.0.2.1]/"  "http" #f "[::ffff:192.0.2.1]" #f "/" #f #f)
    ("IPv6 address (IPv4-translated (RFC 6052))"    "http://[64:ff9b::192.0.2.1]/" "http" #f "[64:ff9b::192.0.2.1]" #f "/" #f #f)
    ;; IPv4 addresses
    ("IPv4 address (host only)"        "http://10.0.0.0/"   "http" #f  "10.0.0.0" #f "/" #f #f)
    ("IPv4 address (user+host)"        "http://a@10.0.0.0/" "http" "a" "10.0.0.0" #f "/" #f #f)
    ("IPv4 address (user+host+port)"   "http://a@10.0.0.0:8080/" "http" "a" "10.0.0.0" 8080 "/" #f #f)
    ("IPv4 address (max octets)"       "http://255.255.255.128/" "http" #f  "255.255.255.128" #f "/" #f #f)
    ("IPv4 address (min octets)"       "http://0.0.0.0/" "http" #f  "0.0.0.0" #f "/" #f #f)
    ("IPv4 address (mixed octets)"     "http://0.1.22.223/" "http" #f  "0.1.22.223" #f "/" #f #f)
    ;;;
    ))

(define relative-regular-segments
  '(;; Relative reference specific tests
    ("Empty URI"       ""     #f #f #f "" #f #f)
    ("Empty authority" "//"   #f "" #f "" #f #f)
    ("Empty user"      "//@"  "" #f #f "" #f #f)
    ("Empty port"      "//:"  #f "" #f "" #f #f)
    ("Empty query"     "?"    #f #f #f "" "" #f)
    ("Empty fragment"  "#"    #f #f #f "" #f "")
    ("Path which looks like a hostname"   "example.org"            #f    #f #f "example.org"     #f #f)
    ("Path which looks like a URN"        "./urn:something"        #f    #f #f "./urn:something" #f #f)
    ("Relative reference, nil host"        "////g"    #f ""  #f "//g"     #f #f)
    ("Relative reference, path, nil host"  ".///g"    #f #f  #f ".///g"   #f #f)
    ("Relative reference, non-empty host"  "//g"      #f "g" #f ""       #f #f)
    ("Path which looks like a query"       "./p=q:r"  #f #f  #f "./p=q:r" #f #f)
    ("Relative reference, all components filled out"                 "//user@example.org:80/some/where/place?qua#ought" "user" "example.org" 80 "/some/where/place" "qua" "ought")
    ("Relative reference, all components except user filled out"     "//example.org:80/some/where/place?qua#ought"      #f "example.org"     80 "/some/where/place" "qua" "ought")
    ("Relative reference, all components except host filled out"     "//user@:80/some/where/place?qua#ought"            "user" #f            80 "/some/where/place" "qua" "ought")
    ("Relative reference, all components except port filled out"     "//user@example.org:80/some/where/place?qua#ought" "user" "example.org" 80 "/some/where/place" "qua" "ought")
    ("Relative reference, all components except path filled out"     "//user@example.org:80?qua#ought"                  "user" "example.org" 80 ""                  "qua" "ought")
    ("Relative reference, all components except query filled out"    "//user@example.org:80/some/where/place#ought"     "user" "example.org" 80 "/some/where/place" #f    "ought")
    ("Relative reference, all components except fragment filled out" "//user@example.org:80/some/where/place?qua"       "user" "example.org" 80 "/some/where/place" "qua" #f)
    ("Relative reference empty host, nil user/port"     "///some/where/place?qua#ought"  #f "" #f "/some/where/place" "qua" "ought")
    ("Relative reference empty user, nil host/port"     "//@/some/where/place?qua#ought" "" #f #f "/some/where/place" "qua" "ought")
    ("Relative reference empty port implies empty host" "//:/some/where/place?qua#ought" #f "" #f "/some/where/place" "qua" "ought")))

(define non-relative-regular-segments
  `(("URN-like"                           "urn:something"   "urn"  #f    #f #f "something"       #f #f)
    ("URN-like, path looks like hostname" "urn:example.org" "urn"  #f    #f #f "example.org"     #f #f)
    ("User with colon segment"            "http://a:b@c:29" "http" "a:b" "c" 29 ""       #f #f)
    ("User-like component appears as path" "http::@c:29"    "http" #f    #f  #f ":@c:29" #f #f)
    ("Host-like component appears as user" "http://example.org:b@d/"   "http" "example.org:b" "d" #f "/" #f #f)
    ("Padded port as numeric value"        "http://example.org:000080" "http" #f    "example.org" 80 ""  #f #f)
    ("Query component with question mark"    "http://example.org/abcd?efgh?ijkl" "http" #f "example.org" #f "/abcd" "efgh?ijkl" #f)
    ("Fragment component with question mark" "http://example.org/abcd#efgh?ijkl" "http" #f "example.org" #f "/abcd" #f "efgh?ijkl")
    ("Path where first segment looks like host" "http:///some/where/place" "http" #f "" #f "/some/where/place" #f #f)
    ;;;
    ("Scheme with nil host"          "foo:"      "foo" #f #f  #f ""      #f #f)
    ("Scheme with path, empty host"  "foo:////g" "foo" #f ""  #f "//g"   #f #f)
    ("Scheme with path, nil host"    "foo:.///g" "foo" #f #f  #f ".///g" #f #f)
    ("Scheme with non-empty host"    "foo://g"   "foo" #f "g" #f ""      #f #f)
    ("All components filled out"                 "http://user@example.org:80/some/where/place?qua#ought"  "user" "example.org" 80 "/some/where/place" "qua" "ought")
    ("All components except user filled out"     "http://example.org:80/some/where/place?qua#ought"       #f "example.org"     80 "/some/where/place" "qua" "ought")
    ("All components except host filled out"     "http://user@:80/some/where/place?qua#ought"             "user" #f            80 "/some/where/place" "qua" "ought")
    ("All components except port filled out"     "http://user@example.org/some/where/place?qua#ought"     "user" "example.org" #f "/some/where/place" "qua" "ought")
    ("All components except path filled out"     "http://user@example.org:80?qua#ought"                   "user" "example.org" 80 ""                  "qua" "ought")
    ("All components except query filled out"    "http://user@example.org:80/some/where/place#ought"      "user" "example.org" 80 "/some/where/place" #f    "ought")
    ("All components except fragment filled out" "http://user@example.org:80/some/where/place?qua"        "user" "example.org" 80 "/some/where/place" "qua" #f)
    ("Empty host, nil user/port"     "http:///some/where/place?qua#ought"   "http" #f "" #f "/some/where/place" "qua" "ought")
    ("Empty user, nil host/port"     "http://@/some/where/place?qua#ought"  "http" "" #f #f "/some/where/place" "qua" "ought" )
    ("Empty port implies empty host" "http://:/some/where/place?qua#ought"  "http" #f "" #f "/some/where/place" "qua" "ought")))

(define absolute-regular-segments
  `(;; Largely duplicates `non-relative-regular-segments'
    ("Absolute, URN-like"                           "urn:something"   "urn"  #f    #f #f "something"       #f #f)
    ("Absolute, URN-like, path looks like hostname" "urn:example.org" "urn"  #f    #f #f "example.org"     #f #f)
    ("Absolute, user with colon segment"            "http://a:b@c:29" "http" "a:b" "c" 29 ""       #f #f)
    ("Absolute, user-like component appears as path" "http::@c:29"    "http" #f    #f  #f ":@c:29" #f #f)
    ("Absolute, host-like component appears as user" "http://example.org:b@d/"   "http" "example.org:b" "d" #f "/" #f #f)
    ("Absolute, padded port as numeric value"        "http://example.org:000080" "http" #f    "example.org" 80 ""  #f #f)
    ("Absolute, query component with question mark"    "http://example.org/abcd?efgh?ijkl" "http" #f "example.org" #f "/abcd" "efgh?ijkl" #f)
    ("Absolute, path where first segment looks like host" "http:///some/where/place" "http" #f "" #f "/some/where/place" #f #f)
    ;;;
    ("Absolute, scheme with nil host"          "foo:"      "foo" #f #f  #f ""      #f #f)
    ("Absolute, scheme with path, empty host"  "foo:////g" "foo" #f ""  #f "//g"   #f #f)
    ("Absolute, scheme with path, nil host"    "foo:.///g" "foo" #f #f  #f ".///g" #f #f)
    ("Absolute, scheme with non-empty host"    "foo://g"   "foo" #f "g" #f ""      #f #f)
    ("Absolute, all components filled out"                 "http://user@example.org:80/some/where/place?qua"  "user" "example.org" 80 "/some/where/place" "qua" #f)
    ("Absolute, all components except user filled out"     "http://example.org:80/some/where/place?qua"       #f "example.org"     80 "/some/where/place" "qua" #f)
    ("Absolute, all components except host filled out"     "http://user@:80/some/where/place?qua"             "user" #f            80 "/some/where/place" "qua" #f)
    ("Absolute, all components except port filled out"     "http://user@example.org/some/where/place?qua"     "user" "example.org" #f "/some/where/place" "qua" #f)
    ("Absolute, all components except path filled out"     "http://user@example.org:80?qua"                   "user" "example.org" 80 ""                  "qua" #f)
    ("Absolute, all components except query filled out"    "http://user@example.org:80/some/where/place"      "user" "example.org" 80 "/some/where/place" #f    #f)
    ("Absolute, empty host, nil user/port"     "http:///some/where/place?qua"   "http" #f "" #f "/some/where/place" "qua" #f)
    ("Absolute, empty user, nil host/port"     "http://@/some/where/place?qua"  "http" "" #f #f "/some/where/place" "qua" #f)
    ("Absolute, empty port implies empty host" "http://:/some/where/place?qua"  "http" #f "" #f "/some/where/place" "qua" #f)))

(define non-relative-escaped-segments
  `(("Completely percent-encoded" "http://%75%73%65%72@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?%71%75%65%72%79#%66%72%61%67%6D%65%6E%74"
     "http" "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "%71%75%65%72%79" "%66%72%61%67%6D%65%6E%74")
    ("Mixed percent-encoded user" "http://a%75%73%65%72:pass@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?%71%75%65%72%79#%66%72%61%67%6D%65%6E%74"
     "http" "a%75%73%65%72:pass" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "%71%75%65%72%79" "%66%72%61%67%6D%65%6E%74")
    ("Mixed percent-encoded path" "http://%75%73%65%72@example.org:8080/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65?%71%75%65%72%79#%66%72%61%67%6D%65%6E%74"
     "http" "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65" "%71%75%65%72%79" "%66%72%61%67%6D%65%6E%74")
    ("Mixed percent-encoded query" "http://%75%73%65%72@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?q%75e%72y#%66%72%61%67%6D%65%6E%74"
     "http" "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "q%75e%72y" "%66%72%61%67%6D%65%6E%74")
    ("Mixed percent-encoded fragment" "http://%75%73%65%72@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?%71%75%65%72%79#fra%67%6D%65en%74"
     "http" "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "%71%75%65%72%79" "fra%67%6D%65en%74")
    ("Percent-encoded user only" "http://a%75%73%65%72:pass@example.org:8080/some/where/place?query#fragment"
     "http" "a%75%73%65%72:pass" "example.org" 8080 "/some/where/place" "query" "fragment")
    ("Percent-encoded path only" "http://auser:pass@example.org:8080/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65?query#fragment"
     "http" "auser:pass" "example.org" 8080 "/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65" "query" "fragment")
    ("Percent-encoded query only" "http://auser:pass@example.org:8080/some/where/place?q%75e%72y#fragment"
     "http" "auser:pass" "example.org" 8080 "/some/where/place" "q%75e%72y" "fragment")
    ("Percent-encoded fragment only" "http://auser:pass@example.org:8080/some/where/place?query#fra%67%6D%65en%74"
     "http" "auser:pass" "example.org" 8080 "/some/where/place" "query" "fra%67%6D%65en%74")))

(define relative-escaped-segments
  `(("Relative reference, Completely percent-encoded" "//%75%73%65%72@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?%71%75%65%72%79#%66%72%61%67%6D%65%6E%74"
     "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "%71%75%65%72%79" "%66%72%61%67%6D%65%6E%74")
    ("Relative reference, Mixed percent-encoded user" "//a%75%73%65%72:pass@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?%71%75%65%72%79#%66%72%61%67%6D%65%6E%74"
     "a%75%73%65%72:pass" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "%71%75%65%72%79" "%66%72%61%67%6D%65%6E%74")
    ("Relative reference, Mixed percent-encoded path" "//%75%73%65%72@example.org:8080/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65?%71%75%65%72%79#%66%72%61%67%6D%65%6E%74"
     "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65" "%71%75%65%72%79" "%66%72%61%67%6D%65%6E%74")
    ("Relative reference, Mixed percent-encoded query" "//%75%73%65%72@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?q%75e%72y#%66%72%61%67%6D%65%6E%74"
     "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "q%75e%72y" "%66%72%61%67%6D%65%6E%74")
    ("Relative reference, Mixed percent-encoded fragment" "//%75%73%65%72@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?%71%75%65%72%79#fra%67%6D%65en%74"
     "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "%71%75%65%72%79" "fra%67%6D%65en%74")
    ("Relative reference, Percent-encoded user only" "//a%75%73%65%72:pass@example.org:8080/some/where/place?query#fragment"
     "a%75%73%65%72:pass" "example.org" 8080 "/some/where/place" "query" "fragment")
    ("Relative reference, Percent-encoded path only" "//auser:pass@example.org:8080/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65?query#fragment"
     "auser:pass" "example.org" 8080 "/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65" "query" "fragment")
    ("Relative reference, Percent-encoded query only" "//auser:pass@example.org:8080/some/where/place?q%75e%72y#fragment"
     "auser:pass" "example.org" 8080 "/some/where/place" "q%75e%72y" "fragment")
    ("Relative reference, Percent-encoded fragment only" "//auser:pass@example.org:8080/some/where/place?query#fra%67%6D%65en%74"
     "auser:pass" "example.org" 8080 "/some/where/place" "query" "fra%67%6D%65en%74")))

(define absolute-escaped-segments
  `(;; Largely duplicates `non-relative-escaped-segments'
    ("Completely percent-encoded" "http://%75%73%65%72@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?%71%75%65%72%79"
     "http" "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "%71%75%65%72%79" #f)
    ("Mixed percent-encoded user" "http://a%75%73%65%72:pass@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?%71%75%65%72%79"
     "http" "a%75%73%65%72:pass" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "%71%75%65%72%79" #f)
    ("Mixed percent-encoded path" "http://%75%73%65%72@example.org:8080/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65?%71%75%65%72%79"
     "http" "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65" "%71%75%65%72%79" #f)
    ("Mixed percent-encoded query" "http://%75%73%65%72@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?q%75e%72y"
     "http" "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "q%75e%72y" #f)
    ("Mixed percent-encoded fragment" "http://%75%73%65%72@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?%71%75%65%72%79"
     "http" "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "%71%75%65%72%79" #f)
    ("Percent-encoded user only" "http://a%75%73%65%72:pass@example.org:8080/some/where/place?query"
     "http" "a%75%73%65%72:pass" "example.org" 8080 "/some/where/place" "query" #f)
    ("Percent-encoded path only" "http://auser:pass@example.org:8080/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65?query"
     "http" "auser:pass" "example.org" 8080 "/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65" "query" #f)
    ("Percent-encoded query only" "http://auser:pass@example.org:8080/some/where/place?q%75e%72y"
     "http" "auser:pass" "example.org" 8080 "/some/where/place" "q%75e%72y" #f)
    ("Percent-encoded fragment only" "http://auser:pass@example.org:8080/some/where/place?query"
     "http" "auser:pass" "example.org" 8080 "/some/where/place" "query" #f)))


(define all-regular-segments
  (append non-relative-regular-segments relative-regular-segments absolute-regular-segments))

(define all-escaped-segments
  (append non-relative-escaped-segments relative-escaped-segments absolute-escaped-segments))

;;; Accessors as lists, so that the components of an identifier can be compared in one go

(define iri-components
  (list iri-scheme iri-user iri-host iri-port (lambda (x) (path->string (iri-path x)))
        iri-query iri-fragment))

(define uri-components
  (list uri-scheme uri-user uri-host uri-port (lambda (x) (path->string (uri-path x)))
        uri-query uri-fragment))

(define (iri-parts x)
  (map (lambda (get) (if (relative-iri? x) (if (eq? get iri-scheme) #f (get x)) (get x)))
       iri-components))

(define (uri-parts x)
  (map (lambda (get) (if (relative-uri? x) (if (eq? get uri-scheme) #f (get x)) (get x)))
       uri-components))

;; Rows without an expected scheme leave the scheme unchecked
(define (row-expected row)
  (cddr row))

(define (row-actual row parts)
  (if (= (length row) 8) (cdr parts) parts))

;; Checks all rows of a table: the text of each row is parsed with `parse`, and its
;; components are compared with the expected components. Serializing the parsed
;; identifier and parsing the result again has to yield the same components.
(define (check-rows rows parse parts serialize)
  (for-each
    (lambda (row)
      (let ((x (parse (cadr row))))
        (test-equal (car row) (row-expected row) (row-actual row (parts x)))
        (test-equal (string-append (car row) " (round trip)")
                    (row-expected row)
                    (row-actual row (parts (parse (serialize x)))))))
    rows))

(define (relative-row? row) (= (length row) 8))

(test-begin "SRFI 275: Parsing")

(test-group "IRIs: regular components"
  (check-rows all-regular-segments string->iri iri-parts iri->string))

(test-group "URIs: regular components"
  (check-rows all-regular-segments string->uri uri-parts uri->string))

(test-group "IRIs: percent-encoded components"
  (check-rows all-escaped-segments string->iri iri-parts iri->string))

(test-group "URIs: percent-encoded components"
  (check-rows all-escaped-segments string->uri uri-parts uri->string))

(test-group "Relative references"
  (for-each (lambda (row)
              (test-assert (car row) (relative-iri? (string->iri (cadr row))))
              (test-assert (car row) (relative-uri? (string->uri (cadr row)))))
            (append relative-regular-segments relative-escaped-segments))
  (for-each (lambda (row)
              (test-assert (car row) (non-relative-iri? (string->iri (cadr row))))
              (test-assert (car row) (non-relative-uri? (string->uri (cadr row)))))
            (append non-relative-regular-segments non-relative-escaped-segments
                    absolute-regular-segments absolute-escaped-segments)))

(test-group "IRIs: IP addresses"
  (test-equal '("http" #f "[2001:db8::1]" 8080 "/path" "query" "fragment")
              (iri-parts (string->iri "http://[2001:db8::1]:8080/path?query#fragment")))
  (check-rows ip-address-segments string->iri iri-parts iri->string)
  (check-rows ip-address-segments string->non-relative-iri iri-parts iri->string)
  (check-rows ip-address-segments string->rfc3987-absolute-iri iri-parts iri->string)
  (test-error "more than one :: marker" (string->iri "http://[2001:db8:::1]/"))
  (test-error "nine groups" (string->iri "http://[2001:db8:a:b:c:d:e:f:1]/"))
  (test-error "seven groups without ::" (string->iri "http://[2001:db8:a:b:c:d:e]/"))
  (test-error "group with five digits" (string->iri "http://[2001:db8::fffff]/"))
  (test-error "leading zeros push group to five digits" (string->iri "http://[2001:00db8::0001]/"))
  (test-error "incomplete IPv4 suffix" (string->iri "http://[2001:db8::192.0.2]/"))
  (test-error "two :: markers" (string->iri "http://[2001:db8:a::b::c]/"))
  (test-error "two groups only" (string->iri "http://[2001:db8]/"))
  (test-error "unclosed bracket" (string->iri "http://[2001:db8"))
  (test-error "garbage after bracket" (string->iri "http://[::1]x/"))
  (test-error "invalid group" (string->iri "http://[::g]/"))
  (test-error "IPv4 octet out of range" (string->iri "http://[::1.2.3.256]/"))
  (test-equal "future IP literal"
              "[v1.a:b]"
              (iri-host (string->iri "http://[v1.a:b]/")))
  (test-equal "IP literal with port"
              '("[::1]" 80)
              (let ((x (string->iri "http://[::1]:80/x")))
                (list (iri-host x) (iri-port x)))))

(test-group "URIs: IP addresses"
  (test-equal '("http" #f "[2001:db8::1]" 8080 "/path" "query" "fragment")
              (uri-parts (string->uri "http://[2001:db8::1]:8080/path?query#fragment")))
  (check-rows ip-address-segments string->uri uri-parts uri->string)
  (check-rows ip-address-segments string->non-relative-uri uri-parts uri->string)
  (check-rows ip-address-segments string->rfc3986-absolute-uri uri-parts uri->string)
  (test-error "more than one :: marker" (string->uri "http://[2001:db8:::1]/"))
  (test-error "nine groups" (string->uri "http://[2001:db8:a:b:c:d:e:f:1]/"))
  (test-error "seven groups without ::" (string->uri "http://[2001:db8:a:b:c:d:e]/"))
  (test-error "group with five digits" (string->uri "http://[2001:db8::fffff]/"))
  (test-error "leading zeros push group to five digits" (string->uri "http://[2001:00db8::0001]/"))
  (test-error "incomplete IPv4 suffix" (string->uri "http://[2001:db8::192.0.2]/"))
  (test-error "two :: markers" (string->uri "http://[2001:db8:a::b::c]/"))
  (test-error "two groups only" (string->uri "http://[2001:db8]/"))
  (test-error "unclosed bracket" (string->uri "http://[2001:db8")))

(test-group "IRIs: absolute-IRI production"
  (check-rows (filter (lambda (r) (not (relative-row? r))) absolute-regular-segments)
              string->rfc3987-absolute-iri iri-parts iri->string)
  (test-assert "accepts empty authority" (string->rfc3987-absolute-iri "urn:/some/where/place"))
  (test-assert "accepts missing query" (string->rfc3987-absolute-iri "http://example.org/some/where/place"))
  (test-assert "accepts query" (string->rfc3987-absolute-iri "http://example.org/some/where/place?a"))
  (test-error "rejects missing scheme" (string->rfc3987-absolute-iri "//example.org/some/where/place?a"))
  (test-error "rejects fragment" (string->rfc3987-absolute-iri "http://example.org/some/where/place?a#a"))
  (test-error "rejects empty fragment" (string->rfc3987-absolute-iri "http://example.org/some/where/place#")))

(test-group "URIs: absolute-URI production"
  (check-rows absolute-regular-segments string->rfc3986-absolute-uri uri-parts uri->string)
  (test-assert "accepts empty authority" (string->rfc3986-absolute-uri "urn:/some/where/place"))
  (test-assert "accepts missing query" (string->rfc3986-absolute-uri "http://example.org/some/where/place"))
  (test-assert "accepts query" (string->rfc3986-absolute-uri "http://example.org/some/where/place?a"))
  (test-error "rejects missing scheme" (string->rfc3986-absolute-uri "//example.org/some/where/place?a"))
  (test-error "rejects fragment" (string->rfc3986-absolute-uri "http://example.org/some/where/place?a#a")))

(test-group "IRIs: non-relative IRIs"
  (check-rows non-relative-regular-segments string->non-relative-iri iri-parts iri->string)
  (test-assert "accepts fragment" (string->non-relative-iri "http://example.org/some/where/place?a#a"))
  (test-error "rejects missing scheme" (string->non-relative-iri "//example.org/some/where/place?a"))
  (test-error "rejects empty text" (string->non-relative-iri ""))
  (test-error "rejects relative path" (string->non-relative-iri "a/b/c")))

(test-group "URIs: non-relative URIs"
  (check-rows non-relative-regular-segments string->non-relative-uri uri-parts uri->string)
  (test-assert "accepts fragment" (string->non-relative-uri "http://example.org/some/where/place?a#a"))
  (test-error "rejects missing scheme" (string->non-relative-uri "//example.org/some/where/place?a"))
  (test-error "rejects empty text" (string->non-relative-uri ""))
  (test-error "rejects relative path" (string->non-relative-uri "a/b/c")))

(test-group "Predicates"
  (define relative (string->iri "/ex#IRI"))
  (define non-relative (string->iri "http://example.org/ex#title"))
  (define urelative (string->uri "/ex#IRI"))
  (define unon-relative (string->uri "http://example.org/ex#title"))
  (test-equal '(#t #f #t) (list (iri? relative) (non-relative-iri? relative) (relative-iri? relative)))
  (test-equal '(#t #t #f) (list (iri? non-relative) (non-relative-iri? non-relative)
                                (relative-iri? non-relative)))
  (test-equal '(#t #f #t) (list (uri? urelative) (non-relative-uri? urelative) (relative-uri? urelative)))
  (test-equal '(#t #t #f) (list (uri? unon-relative) (non-relative-uri? unon-relative)
                                (relative-uri? unon-relative)))
  (test-assert "IRIs and URIs are different types" (not (or (uri? relative) (iri? urelative))))
  (test-assert "predicates reject other objects"
    (not (or (iri? "http://example.org") (uri? 'a) (iri? #f) (relative-iri? 1)
             (non-relative-iri? '()) (relative-uri? "") (non-relative-uri? "a:b")
             (rfc3987-absolute-iri? "a:b") (rfc3986-absolute-uri? #t))))
  (test-equal "absolute-IRI examples"
              '(#t #f #f #f)
              (map rfc3987-absolute-iri?
                   (map string->iri '("http://example.org/ex?cond" "http://example.org/ex#title"
                                      "//example.org/ex?cond" "//example.org/ex#title"))))
  (test-equal "absolute-URI examples"
              '(#t #f #f #f)
              (map rfc3986-absolute-uri?
                   (map string->uri '("http://example.org/ex?cond" "http://example.org/ex#title"
                                      "//example.org/ex?cond" "//example.org/ex#title")))))

(test-group "Empty identifiers"
  (test-assert (iri? (empty-iri)))
  (test-assert (relative-iri? (empty-iri)))
  (test-assert (iri=? (string->iri "") (empty-iri)))
  (test-equal "" (iri->string (empty-iri)))
  (test-assert (uri? (empty-uri)))
  (test-assert (relative-uri? (empty-uri)))
  (test-assert (uri=? (string->uri "") (empty-uri)))
  (test-equal "" (uri->string (empty-uri)))
  (test-equal '(#f #f #f #f "" #f #f) (iri-parts (empty-iri)))
  (test-equal '(#f #f #f #f "" #f #f) (uri-parts (empty-uri))))

(test-group "Component getters"
  (define x (string->iri "http://example.org:80/ex#IRI"))
  (test-equal "http" (iri-scheme x))
  (test-equal #f (iri-user x))
  (test-equal "example.org" (iri-host x))
  (test-equal 80 (iri-port x))
  (test-equal '("ex") (path-segments (iri-path x)))
  (test-equal #f (iri-query x))
  (test-equal "IRI" (iri-fragment x))
  (test-equal "http://example.org:80/ex#IRI" (iri->string x))
  (test-equal "urn" (iri-scheme (string->iri "urn:a/b/c")))
  (test-error (iri-scheme (string->iri "a/b/c")))
  (test-error (uri-scheme (string->uri "a/b/c")))
  (test-error (iri-scheme "http://example.org"))
  (test-error (iri-host #f))
  (test-error (iri-path 'a))
  (test-error (iri-host (string->uri "http://example.org")))
  (test-error (uri-host (string->iri "http://example.org")))
  (test-equal "empty query is not an unset query"
              '("" #f)
              (list (iri-query (string->iri "a:b?")) (iri-query (string->iri "a:b"))))
  (test-equal "empty fragment is not an unset fragment"
              '("" #f)
              (list (iri-fragment (string->iri "a:b#")) (iri-fragment (string->iri "a:b"))))
  (test-equal "the path is never #f" "" (path->string (iri-path (string->iri "a:"))))
  (test-assert "the path is a path object" (path? (iri-path x)))
  (test-assert "the path is a path object" (path? (uri-path (string->uri "a:b"))))
  (test-equal "http://example.org:80/ex#IRI"
              (uri->string (string->uri "http://example.org:80/ex#IRI"))))

(test-group "Authority"
  (define (authority get x) (call-with-values (lambda () (get x)) list))
  (test-equal '(#f "example.org" 8080)
              (authority iri-authority (string->iri "http://example.org:8080/some/where/place")))
  (test-equal '("user" "example.org" #f)
              (authority iri-authority (string->iri "http://user@example.org/some/where/place")))
  (test-equal '(#f #f #f) (authority iri-authority (string->iri "urn:/some/where/place")))
  (test-equal '("" #f #f) (authority iri-authority (string->iri "//@")))
  (test-equal '(#f "" #f) (authority iri-authority (string->iri "//")))
  (test-equal '(#f "" #f) (authority iri-authority (string->iri "//:")))
  (test-equal '(#f "example.org" 8080)
              (authority uri-authority (string->uri "http://example.org:8080/some/where/place")))
  (test-equal '("user" "example.org" #f)
              (authority uri-authority (string->uri "http://user@example.org/some/where/place")))
  (test-equal '(#f #f #f) (authority uri-authority (string->uri "urn:/some/where/place"))))

(test-group "Username and password"
  (define (user+pass get text) (call-with-values (lambda () (get (string->iri text))) list))
  (test-equal '("foo" "bar:qux") (user+pass iri-username+password "//foo:bar:qux@host"))
  (test-equal '("foo%3Abar" "qux") (user+pass iri-username+password "//foo%3Abar:qux@host"))
  (test-equal '("" #f) (user+pass iri-username+password "//@host"))
  (test-equal '(#f #f) (user+pass iri-username+password "//"))
  (test-equal '("foo" #f) (user+pass iri-username+password "//foo@host"))
  (test-equal '("foo" "") (user+pass iri-username+password "//foo:@host"))
  (test-equal '("" "pw") (user+pass iri-username+password "//:pw@host"))
  (test-equal '("foo" "bar:qux")
              (call-with-values (lambda () (uri-username+password (string->uri "//foo:bar:qux@host")))
                                list))
  (test-equal '("foo%3Abar" "qux")
              (call-with-values (lambda () (uri-username+password (string->uri "//foo%3Abar:qux@host")))
                                list)))

(test-group "Ports"
  (test-equal 80 (iri-port (string->iri "http://example.org:80/")))
  (test-equal 80 (iri-port (string->iri "http://example.org:000080/")))
  (test-equal 0 (iri-port (string->iri "http://example.org:0/")))
  (test-equal 65536 (iri-port (string->iri "http://example.org:65536/")))
  (test-equal #f (iri-port (string->iri "http://example.org:/")))
  (test-equal #f (iri-port (string->iri "http://example.org/")))
  (test-equal "http://example.org:80/" (iri->string (string->iri "http://example.org:000080/")))
  (test-equal "http://example.org/" (iri->string (string->iri "http://example.org:/")))
  (test-error (string->iri "http://example.org:80a/"))
  (test-error (string->iri "http://example.org:-1/"))
  (test-error (string->iri "http://example.org:+1/"))
  (test-error (string->uri "http://example.org:8 0/")))

(test-group "Scheme"
  (test-equal "scheme is retained as written" "HtTp" (iri-scheme (string->iri "HtTp://example.org")))
  (test-equal "x+y-z.1" (iri-scheme (string->iri "x+y-z.1:a")))
  (test-error "scheme must start with a letter" (string->iri "1a:b"))
  (test-error "empty scheme" (string->iri ":x"))
  (test-error "illegal scheme character" (string->iri "a_b:c"))
  (test-equal "a colon after a slash is part of the path"
              '(#t "a/b:c")
              (let ((x (string->iri "a/b:c"))) (list (relative-iri? x) (path->string (iri-path x)))))
  (test-equal "a colon in a later segment"
              '(#t "./a/b:c")
              (let ((x (string->iri "./a/b:c"))) (list (relative-iri? x) (path->string (iri-path x)))))
  (test-error "an invalid scheme is not a relative path" (string->iri "a b:c"))
  (test-equal "colon in the first segment of a relative reference with dot prefix"
              "./urn:something"
              (path->string (iri-path (string->iri "./urn:something")))))

(test-group "Query and fragment"
  (test-equal "query ends at the first #"
              '("a?b" "c")
              (let ((x (string->iri "http://example.org/p?a?b#c")))
                (list (iri-query x) (iri-fragment x))))
  (test-error "a second # is not permitted" (string->iri "http://example.org/p?a?b#c#d"))
  (test-equal '("efgh?ijkl" #f)
              (let ((x (string->iri "http://example.org/abcd?efgh?ijkl")))
                (list (iri-query x) (iri-fragment x))))
  (test-equal '(#f "a/b?c")
              (let ((x (string->iri "http://example.org/abcd#a/b?c")))
                (list (iri-query x) (iri-fragment x))))
  (test-error "a space is not permitted in a fragment" (string->uri "a:b#c d")))

(test-group "Percent-encoding"
  (test-equal "hex digits are uppercased" "%C3%A9" (iri-fragment (string->iri "a:b#%c3%a9")))
  (test-equal "hex digits are uppercased in paths" "/%C3%A9/%2F"
              (path->string (iri-path (string->iri "a:/%c3%a9/%2f"))))
  (test-equal "hex digits are uppercased in hosts" "%C3%A9" (iri-host (string->iri "a://%c3%a9")))
  (test-equal "hex digits are uppercased in users" "%C3%A9" (iri-user (string->iri "a://%c3%a9@h")))
  (test-equal "hex digits are uppercased in queries" "%C3%A9" (iri-query (string->iri "a:b?%c3%a9")))
  (test-equal "hex digits are uppercased in URIs" "%C3%A9" (uri-fragment (string->uri "a:b#%c3%a9")))
  (test-equal "percent-encoded bytes are not decoded" "%41%2F%7E" (iri-fragment (string->iri "a:b#%41%2f%7e")))
  (test-equal "reserved percent-encoded bytes are retained" "a%3Ab"
              (iri-user (string->iri "//a%3ab@h")))
  (test-equal "valid UTF-8 byte sequences" "%E2%82%AC%F0%9F%98%80"
              (iri-fragment (string->iri "a:b#%e2%82%ac%f0%9f%98%80")))
  (test-error "incomplete escape" (string->iri "a:b#%"))
  (test-error "incomplete escape (one digit)" (string->iri "a:b#%a"))
  (test-error "no hex digits" (string->iri "a:b#%zz"))
  (test-error "no hex digits in URI" (string->uri "a:b#%gg"))
  (test-error "escape in the scheme" (string->iri "a%41:b"))
  (test-error "escape in the port" (string->iri "http://h:%38/"))
  (test-error "stray continuation byte" (string->iri "a:b#%80"))
  (test-error "truncated sequence" (string->iri "a:b#%C3"))
  (test-error "overlong encoding" (string->iri "a:b#%C0%80"))
  (test-error "encoded surrogate" (string->iri "a:b#%ED%A0%80"))
  (test-error "code point above U+10FFFF" (string->iri "a:b#%F4%90%80%80"))
  (test-error "invalid start byte" (string->iri "a:b#%FF"))
  (test-error "invalid UTF-8 in the path" (string->iri "a:/x%C0%AFy"))
  (test-error "invalid UTF-8 in the host" (string->iri "a://x%C0%AFy/"))
  (test-error "invalid UTF-8 in the user" (string->iri "a://x%C0%AFy@h/"))
  (test-error "invalid UTF-8 in the query" (string->iri "a:b?x%C0%AFy"))
  (test-error "invalid UTF-8 in a URI" (string->uri "a:b#x%C0%AFy")))

(test-group "Non-ASCII characters"
  (test-equal "IRIs permit non-ASCII characters" "caf\xE9;"
              (path->string (iri-path (string->iri "a:caf\xE9;"))))
  (test-equal "in the host" "caf\xE9;.example" (iri-host (string->iri "http://caf\xE9;.example/")))
  (test-equal "in the user" "j\xF6;rg" (iri-user (string->iri "http://j\xF6;rg@h/")))
  (test-equal "in the query" "\x4E2D;\x6587;" (iri-query (string->iri "a:b?\x4E2D;\x6587;")))
  (test-equal "in the fragment" "\x2603;" (iri-fragment (string->iri "a:b#\x2603;")))
  (test-equal "supplementary characters"
              "😀"
              (iri-fragment (string->iri "a:b#😀")))
  (test-equal "private use characters are permitted in queries"
              "\xE000;"
              (iri-query (string->iri "a:b?\xE000;")))
  (test-error "but not in fragments" (string->iri "a:b#\xE000;"))
  (test-error "but not in paths" (string->iri "a:\xE000;"))
  (test-error "URIs reject non-ASCII characters" (string->uri "a:caf\xE9;"))
  (test-error "URIs reject non-ASCII characters in the fragment" (string->uri "a:b#\x2603;"))
  (test-error "URIs reject non-ASCII characters in the host" (string->uri "http://caf\xE9;.example/"))
  (test-error "bidirectional control characters are not permitted" (string->iri "a:b\x200E;c"))
  (test-error "ASCII space" (string->iri "http://exa mple.org"))
  (test-error "ASCII space in the path" (string->iri "a:b c"))
  (test-error "control character" (string->iri "a:b\x01;"))
  (test-error "DEL" (string->iri "a:b\x7F;"))
  (test-error "double quote" (string->iri "a:b\"c"))
  (test-error "angle bracket" (string->iri "a:b<c"))
  (test-error "backslash" (string->iri "a:b\\c"))
  (test-error "curly brace" (string->iri "a:b{c}"))
  (test-error "square brackets in the path" (string->iri "a:b[c]"))
  (test-error "lone high surrogate" (string->iri (string #\a #\: (integer->char #xD800))))
  (test-error "lone low surrogate" (string->iri (string #\a #\: (integer->char #xDC00)))))

(test-group "Slashes and empty segments"
  (test-equal "//foo//bar: authority and path" '("foo" "//bar")
              (let ((x (string->iri "//foo//bar")))
                (list (iri-host x) (path->string (iri-path x)))))
  (test-equal "multiple slashes" "/a//b" (path->string (iri-path (string->iri "/a//b"))))
  (test-equal '("a" "" "b") (path-segments (iri-path (string->iri "/a//b"))))
  (test-equal '() (path-segments (iri-path (string->iri "http://example.org/"))))
  (test-equal "/" (path->string (iri-path (string->iri "http://example.org/"))))
  (test-equal "" (path->string (iri-path (string->iri "http://example.org"))))
  (test-assert (absolute-path? (iri-path (string->iri "http://example.org/"))))
  (test-assert (empty-path? (iri-path (string->iri "http://example.org")))))

(test-group "Serialization"
  (test-equal "http://user:pw@example.org:8080/a/b?x=1&y#top"
              (iri->string (string->iri "http://user:pw@example.org:8080/a/b?x=1&y#top")))
  (test-equal "uppercase in the scheme and host is not normalized"
              "HTTP://Example.ORG/%C3%A9"
              (iri->string (string->iri "HTTP://Example.ORG/%c3%a9")))
  (test-equal "//@" (iri->string (string->iri "//@")))
  (test-equal "//" (iri->string (string->iri "//")))
  (test-equal "?" (iri->string (string->iri "?")))
  (test-equal "#" (iri->string (string->iri "#")))
  (test-equal "?#" (iri->string (string->iri "?#")))
  (test-equal "foo:" (iri->string (string->iri "foo:")))
  (test-equal "foo:.///g" (iri->string (string->iri "foo:.///g")))
  (test-equal "foo:////g" (iri->string (string->iri "foo:////g")))
  (test-equal "////g" (iri->string (string->iri "////g")))
  (test-equal ".///g" (iri->string (string->iri ".///g")))
  (test-equal "http://[::1]:80/x" (iri->string (string->iri "http://[::1]:80/x")))
  (test-equal "./caf\xE9;/😀" (iri->string (string->iri "./caf\xE9;/😀")))
  (test-equal "a:b" (uri->string (string->uri "a:b")))
  (test-equal "a://u@h:1/p?q#f" (uri->string (string->uri "a://u@h:1/p?q#f")))
  (test-error (iri->string (string->uri "a:b")))
  (test-error (uri->string (string->iri "a:b")))
  (test-error (iri->string "a:b"))
  (test-error (uri->string 'a)))

(test-group "Equality"
  (test-assert (iri=? (string->iri "http://example.org/a") (string->iri "http://example.org/a")))
  (test-assert (iri=? (string->iri "http://example.org/%c3%a9") (string->iri "http://example.org/%C3%A9")))
  (test-assert (not (iri=? (string->iri "http://example.org/a") (string->iri "http://example.org/b"))))
  (test-assert "scheme comparison is case-sensitive"
               (not (iri=? (string->iri "http://example.org/") (string->iri "HTTP://example.org/"))))
  (test-assert "host comparison is case-sensitive"
               (not (iri=? (string->iri "http://example.org/") (string->iri "http://Example.org/"))))
  (test-assert "no escape normalization"
               (not (iri=? (string->iri "http://example.org/a") (string->iri "http://example.org/%61"))))
  (test-assert "empty and missing query are different"
               (not (iri=? (string->iri "a:b?") (string->iri "a:b"))))
  (test-assert "empty and missing fragment are different"
               (not (iri=? (string->iri "a:b#") (string->iri "a:b"))))
  (test-assert "empty and missing host are different"
               (not (iri=? (string->iri "a:/b") (string->iri "a:///b"))))
  (test-assert "port 80 and padded port 80 are equal"
               (iri=? (string->iri "a://h:80/") (string->iri "a://h:000080/")))
  (test-assert "missing and empty port are equal"
               (iri=? (string->iri "a://h/") (string->iri "a://h:/")))
  (test-assert "three arguments"
               (iri=? (string->iri "a:b") (string->iri "a:b") (string->iri "a:b")))
  (test-assert "three arguments, the last one is different"
               (not (iri=? (string->iri "a:b") (string->iri "a:b") (string->iri "a:c"))))
  (test-assert (uri=? (string->uri "http://example.org/%c3%a9") (string->uri "http://example.org/%C3%A9")))
  (test-assert (not (uri=? (string->uri "http://example.org/a") (string->uri "http://example.org/b"))))
  (test-error (iri=? (string->iri "a:b") (string->uri "a:b")))
  (test-error (iri=? (string->iri "a:b") "a:b"))
  (test-error (uri=? (string->uri "a:b") 'a)))

(test-group "Parsing from ports"
  (define (iri-from text . opt)
    (let ((port (open-input-string text)))
      (apply get-iri port opt)))
  (test-equal "http://example.org/a" (iri->string (iri-from "http://example.org/a")))
  (test-equal "reads up to the stop object"
              "http://example.org/a"
              (iri->string (iri-from "http://example.org/a b" #\space)))
  (test-equal "the stop object is not consumed"
              '("a:b" #\space)
              (let* ((port (open-input-string "a:b c:d"))
                     (x (get-iri port #\space)))
                (list (iri->string x) (read-char port))))
  (test-equal "parsing a sequence of IRIs"
              '("a:b" "c:d")
              (let ((port (open-input-string "a:b c:d")))
                (let* ((first (get-iri port #\space))
                       (sep (read-char port))
                       (second (get-iri port #\space)))
                  (list (iri->string first) (iri->string second)))))
  (test-equal "specification example: no stop object"
              '("urn:/some/where/place" #t)
              (let* ((port (open-input-string "urn:/some/where/place"))
                     (x (get-iri port)))
                (list (iri->string x) (eof-object? (read-char port)))))
  (test-equal "specification example: delimited IRI"
              '(#\< "http://example.org/some/where/place" #\>)
              (let* ((port (open-input-string "<http://example.org/some/where/place> <b> ."))
                     (open (read-char port))
                     (x (get-iri port #\>)))
                (list open (iri->string x) (read-char port))))
  (test-equal "empty port gives the empty IRI" "" (iri->string (iri-from "")))
  (test-equal "stop at the first character" "" (iri->string (iri-from " a:b" #\space)))
  (test-equal "non-relative IRI"
              "a:b#c"
              (iri->string (get-non-relative-iri (open-input-string "a:b#c"))))
  (test-equal "absolute IRI"
              "a:b?c"
              (iri->string (get-rfc3987-absolute-iri (open-input-string "a:b?c"))))
  (test-equal "absolute IRI with stop object"
              "a:b?c"
              (iri->string (get-rfc3987-absolute-iri (open-input-string "a:b?c#d") #\#)))
  (test-error (get-non-relative-iri (open-input-string "//a")))
  (test-error (get-rfc3987-absolute-iri (open-input-string "a:b#c")))
  (test-error (get-iri (open-input-string "a:%zz")))
  (test-error (get-iri "a:b"))
  (test-error (get-iri (open-output-string)))
  (test-equal "URIs"
              "http://example.org/a?b"
              (uri->string (get-uri (open-input-string "http://example.org/a?b"))))
  (test-equal "URIs with stop object"
              "http://example.org/a"
              (uri->string (get-uri (open-input-string "http://example.org/a\nb") #\newline)))
  (test-equal "non-relative URI"
              "a:b#c"
              (uri->string (get-non-relative-uri (open-input-string "a:b#c"))))
  (test-equal "absolute URI"
              "a:b?c"
              (uri->string (get-rfc3986-absolute-uri (open-input-string "a:b?c"))))
  (test-error (get-uri (open-input-string "caf\xE9;")))
  (test-error (get-non-relative-uri (open-input-string "/a")))
  (test-error (get-rfc3986-absolute-uri (open-input-string "a:b#c"))))

(test-group "Argument checks"
  (test-error (string->iri 'a))
  (test-error (string->iri #f))
  (test-error (string->iri #\a))
  (test-error (string->non-relative-iri 1))
  (test-error (string->rfc3987-absolute-iri '()))
  (test-error (string->uri 'a))
  (test-error (string->non-relative-uri 1))
  (test-error (string->rfc3986-absolute-uri '())))

(test-end)
