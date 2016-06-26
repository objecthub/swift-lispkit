;;; Ports.scm
;;; Regression test data
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2016 ObjectHub. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;      http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(
  "Reading/writing characters with string ports"
  "This is a smilie: 😀, and a flag: 🇩🇪."
  (let ((in (open-input-string "This is a smilie: 😀, and a flag: 🇩🇪."))
        (out (open-output-string)))
    (do ((ch (read-char in) (read-char in)))
        ((eof-object? ch) (get-output-string out))
      (write-char ch out)))
)

(
  "Reading/writing lines with string ports"
  5
  (let ((in (open-input-string "line 1\nline 2\n3\n\nline 5")))
    (do ((line (read-line in) (read-line in))
         (i 0 (+ i 1)))
        ((eof-object? line) i)))
)

(
  "Reading/writing bytes with binary ports"
  #u8(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
  (let ((in (open-input-bytevector #u8(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
      (out (open-output-bytevector)))
    (do ((byte (read-u8 in) (read-u8 in)))
        ((eof-object? byte) (get-output-bytevector out))
      (write-u8 byte out)))
)
