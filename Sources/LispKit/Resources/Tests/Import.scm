;;; LISPKIT REGRESSION TEST SUITE
;;;
;;; This code imports all libraries coming with LispKit for testing purposes.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2019-2022 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
;;; except in compliance with the License. You may obtain a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed under the
;;; License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
;;; either express or implied. See the License for the specific language governing permissions
;;; and limitations under the License.

;; LispKit

(display "LispKit native")
(newline)

; Native
(import (lispkit base))
(import (lispkit gvector))
(import (lispkit date-time))
(import (lispkit char-set))
(import (lispkit bitset))
(import (lispkit enum))
(import (lispkit regexp))
(import (lispkit draw))
(import (lispkit markdown))
(import (lispkit sqlite))
(import (lispkit archive zip))
(import (lispkit system os))
(import (lispkit debug))

(display "LispKit non-native")
(newline)

; Non-native
(import (lispkit draw turtle))
(import (lispkit log))
(import (lispkit test))
(import (lispkit match))
(import (lispkit datatype))
(import (lispkit object))
(import (lispkit comparator))
(import (lispkit combinator))
(import (lispkit logic))
(import (lispkit prolog))
(import (lispkit iterate))
(import (lispkit math util))
(import (lispkit math stats))
(import (lispkit set))
(import (lispkit stack))
(import (lispkit queue))
(import (lispkit heap))
(import (lispkit graph))
(import (lispkit wt-tree))
(import (lispkit stream))
(import (lispkit thread))
(import (lispkit prettify))
(import (lispkit text-table))
(import (lispkit csv))
(import (lispkit json))
(import (lispkit pdf))
(import (lispkit clos))
(import (lispkit disjoint-set))
(import (lispkit sxml))
(import (lispkit sxml xml))
(import (lispkit sxml html))

;; SRFI

(display "SRFI")
(newline)

(import (srfi 1))
(import (srfi 2))
(import (srfi 6))
(import (srfi 8))
(import (srfi 9))
(import (srfi 11))
(import (srfi 14))
(import (srfi 14 ascii))
(import (srfi 16))
(import (srfi 17))
(import (srfi 18))
(import (srfi 19))
(import (srfi 23))
(import (srfi 26))
(import (srfi 27))
(import (srfi 28))
(import (srfi 31))
(import (srfi 33))
(import (srfi 34))
(import (srfi 35))
(import (srfi 39))
(import (srfi 41))
(import (srfi 46))
(import (srfi 48))
(import (srfi 51))
(import (srfi 54))
(import (srfi 55))
(import (srfi 63))
(import (srfi 64))
(import (srfi 69))
(import (srfi 87))
(import (srfi 95))
(import (srfi 98))
(import (srfi 102))
(import (srfi 111))
(import (srfi 112))
(import (srfi 113))
(import (srfi 121))
(import (srfi 125))
(import (srfi 128))
(import (srfi 129))
(import (srfi 132))
(import (srfi 133))
(import (srfi 134))
(import (srfi 135))
(import (srfi 137))
(import (srfi 142))
(import (srfi 143))
(import (srfi 144))
(import (srfi 145))
(import (srfi 146))
(import (srfi 149))
(import (srfi 151))
(import (srfi 152))
(import (srfi 154))
(import (srfi 155))
(import (srfi 158))
(import (srfi 161))
(import (srfi 162))
(import (srfi 165))
(import (srfi 166))
(import (srfi 167 engine))
(import (srfi 167 pack))
(import (srfi 167 memory))
(import (srfi 173))
(import (srfi 174))
(import (srfi 175))
(import (srfi 177))
(import (srfi 180))
(import (srfi 189))
(import (srfi 194))
(import (srfi 195))
(import (srfi 196))
(import (srfi 204))
(import (srfi 208))
(import (srfi 209))
(import (srfi 210))
(import (srfi 214))
(import (srfi 215))
(import (srfi 216))
(import (srfi sicp))
(import (srfi 214))
(import (srfi 217))
(import (srfi 219))
(import (srfi 221))
(import (srfi 222))
(import (srfi 223))
(import (srfi 224))
(import (srfi 227))
(import (srfi 229))
(import (srfi 230))

; this needs to be last
(import (srfi 101))
