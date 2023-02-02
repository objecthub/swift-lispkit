;;; SRFI 233 REGRESSION TEST SUITE
;;;
;;; This is the test suite for SRFI 233.
;;;
;;; Copyright © 2022 Arvydas Silanskas. All rights reserved.
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
;;;   Copyright © 2023 Matthias Zenger. All rights reserved.

(import (lispkit base)
        (lispkit test)
        (srfi 233))

(test-begin "SRFI 233: INI Files")

(test-group "make-ini-file-accumulator"

  (define result
    (let* ((port (open-output-string))
           (acc (make-ini-file-accumulator port)))
      ;; write leading section-less data
      (acc '(|| key1 "value1"))
      ;; write comment
      (acc "test comment")
      ;; write new section
      (acc '(section key2 "value2"))
      (get-output-string port)))

  (test "key1=value1\n; test comment\n[section]\nkey2=value2\n" result))

(test-group "make-ini-file-generator"

  (define (read-to-list generator)
    (let loop ((lst '()))
      (define entry-lst (generator))
      (cond
       ((eof-object? entry-lst)
        (reverse lst))
       (else (loop (cons entry-lst lst))))))

  (define source "key1 = value1\n; comment\n\n[section]\n key2 = value2\n[section2]\nkey3\n[key]4\n\n\n")

  (define result (read-to-list (make-ini-file-generator (open-input-string source))))

  (test '((|| key1 "value1")
          (section key2 "value2")
          (section2 #f "key3")
          (section2 #f "[key]4"))
        result))

(test-group "make-ini-file-accumulator custom delimeters"

  (define result
    (let* ((port (open-output-string))
           (acc (make-ini-file-accumulator port #\- #\#)))
      ;; write leading section-less data
      (acc '(|| key1 "value1"))
      ;; write comment
      (acc "test comment")
      ;; write new section
      (acc '(section key2 "value2"))
      (get-output-string port)))

  (test "key1-value1\n# test comment\n[section]\nkey2-value2\n" result))

(test-group "make-ini-file-generator custom delimeters"

  (define (read-to-list generator)
    (let loop ((lst '()))
      (define entry-lst (generator))
      (cond
       ((eof-object? entry-lst)
        (reverse lst))
       (else (loop (cons entry-lst lst))))))

  (define source "key1 - value1\n# comment\n\n[section]\n key2 - value2\n[section2]\nkey3\n[key]4\n\n\n")

  (define result (read-to-list (make-ini-file-generator (open-input-string source) #\- #\#)))

  (test '((|| key1 "value1")
          (section key2 "value2")
          (section2 #f "key3")
          (section2 #f "[key]4"))
        result))

(test-end)
