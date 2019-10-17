;;; SRFI 174 REGRESSION TEST SUITE
;;;
;;; This is the test suite for SRFI 174.
;;;
;;; Copyright © 2019 John Cowan. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2019 Matthias Zenger. All rights reserved.

(import (lispkit base)
        (lispkit test)
        (lispkit comparator)
        (srfi 174))

(test-begin "SRFI 174: POSIX Timespecs")

(define ts1 (timespec 1 2))
(define ts2 (timespec 1 2))
(define ts3 (timespec 1 3))
(define ts4 (timespec 2 2))
(define ts-odd (timespec 1 (- 1000000001 2)))  ; nano second too big (fixed test)
(define ts-neg (timespec -1 2))

(test-assert "timespec?" (timespec? ts1))
(test-assert "not timespec?" (not (timespec? #f)))
(test "seconds" 1 (timespec-seconds ts1))
(test "nanos" 2 (timespec-nanoseconds ts1))
(test-assert "equal" (=? timespec-comparator ts1 ts2))
(test-assert "less nanos" (<? timespec-comparator ts1 ts3))
(test-assert "less seconds" (<? timespec-comparator ts1 ts3))
(test-assert "odd" (<? timespec-comparator ts1 ts-odd))
(test-assert "integer hash" (exact-integer? (comparator-hash timespec-comparator ts-neg)))
    ; LispKit hash functions might return negative values (replaced positive? with
    ; exact-integer?)

(test-end)
