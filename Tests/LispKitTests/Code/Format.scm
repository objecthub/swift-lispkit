;;; Format.scm
;;; Regression test data
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2023 ObjectHub. All rights reserved.
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
  "ASCII directive formatting"
  ("|7 |" "|7 |" "|7   |" "| 17|" "| 17|" "| 17|" "|   17|" "270000000000000000000000000000000")
  (import (lispkit format))
  (list (format "|~1,2,1A|" 7)
        (format "|~2,2,1A|" 7)
        (format "|~3,2,1A|" 7)
        (format "|~1,2,1@A|" 17)
        (format "|~2,2,1@A|" 17)
        (format "|~3,2,1@A|" 17)
        (format "|~4,2,1@A|" 17)
        (format "~33,,,'0A" 27))
)

(
  "Character directive formatting"
  ("Ü" "&#xA9;" "&copy;" "\\u{dc}" "\"\\u{dc}\"" "U+00DC" "LATIN CAPITAL LETTER U WITH DIAERESIS")
  (list (format "~C" #\Ü)
        (format "~:C" #\©)
        (format "~:+C" #\©)
        (format "~@C" #\Ü)
        (format "~@+C" #\Ü)
        (format "~@:C" #\Ü)
        (format "~@:+C" #\Ü))
)

(
  "Decimal directive formatting"
  ("<Foo 5> 7" "<Foo 5> 7" "<Foo 5> 7" "<Foo 5> 14")
  (list (format "~? ~D" "<~A ~D>" '("Foo" 5) 7)
        (format "~? ~D" "<~A ~D>" (vector "Foo" 5 14) 7)
        (format "~@? ~D" "<~A ~D>" "Foo" 5 7)
        (format "~@? ~D" "<~A ~D>" "Foo" 5 14 7))
)

(
  "Fixed-float directive formatting"
  ("|123456.78901|" "|123456.79|" "|   123456.78901|" "|      123456.79|" "|     +123456.79|"
   "|__________+0.01|")
  (list (format "|~F|" 123456.78901)
        (format "|~,2F|" 123456.78901)
        (format "|~15F|" 123456.78901)
        (format "|~15,2F|" 123456.78901)
        (format "|~15,2@F|" 123456.78901)
        (format "|~15,2,,,'_@F|" 0.011))
)

(
  "Fixed-float 2 directive formatting"
  ("|123456.789|" "|--------|" "|1234.568|" "Float = <  +12345.67890>" "123.1416" "--------")
  (list (format "|~8,3F|" 123456.78901)
        (format "|~8,3,0,'-F|" 123456.78901)
        (format "|~8,3,-2F|" 123456.78901)
        (format "Float = <~14,5@F>" 12345.6789)
        (format "~8F" 123.1415926)
        (format "~8,,,'-F" 123456789.12))
)

(
  "Fixed-float 3 directive formatting"
  ("123.1416" "0123.142" "+123.14" "+3.14" "1,234,567.89" "1'234'567.89")
  (list (format "~8,,,'-F" 123.1415926)
        (format "~8,3,,,'0F" 123.1415926)
        (format "~,2@F" 123.1415926)
        (format "~,2,-2@F" 314.15926)
        (format "~,2:F" 1234567.891)
        (format "~,2,,,,'',3:F" 1234567.891))
)

(
  "Iteration directive formatting"
  ("Numbers: one two three" "Numbers: one #f three" "Items: none." "Items: FOO."
   "Items: FOO and BAR." "Items: FOO, BAR, and BAZ." "Items: FOO, BAR, BAZ, and GOO.")
  (define choiceHash "Items:~#[ none~; ~A~; ~A and ~A~:;~@{~#[~; and~] ~A~^,~}~].")
  (list (format "Numbers:~{ ~A~}" '("one" "two" "three"))
        (format "Numbers:~{ ~A~}" '("one" #f "three"))
        (format choiceHash)
        (format choiceHash "FOO")
        (format choiceHash "FOO" "BAR")
        (format choiceHash "FOO" "BAR" "BAZ")
        (format choiceHash "FOO" "BAR" "BAZ" "GOO"))
)

(
  "Iteration 2 directive formatting"
  ("Pairs: <A,1> <B,2> <C,3>." "1 and #f and 3" "/hot .../hamburger/ice .../french ..."
   "/hot .../hamburger .../ice .../french" "/hot .../hamburger"
   "hello\n~~~~~~~~~~\n\nWORLD. Number Three. This is True. My name is unknown. ! 1 two 3 ")
  (list (format "Pairs:~:@{ <~A,~A>~}." '("A" 1) '("B" 2) '("C" 3))
        (format "~A and ~A and ~A" 1 #f 3)
        (format "~:{/~A~^ ...~}" '(("hot" "dog") ("hamburger") ("ice" "cream") ("french" "fries")))
        (format "~:{/~A~:^ ...~}" '(("hot" "dog") ("hamburger") ("ice" "cream") ("french" "fries")))
        (format "~:{/~A~#:^ ...~}" '(("hot" "dog") ("hamburger") ("ice" "cream") ("french" "fries")))
        (format (string-append "hello~%~10~~%~2&~:@(world~). Number ~#[Zero~;One~;Two~:;Three~]."
                               " This is ~:[False~;True~]. ~@[My name is ~A. ~]! ~3{~A ~}")
                2 "unknown" '(1 "two" 3 4)))
)
