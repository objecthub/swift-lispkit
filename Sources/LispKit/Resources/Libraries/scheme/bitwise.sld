;;; SCHEME BITWISE
;;;
;;; Library implementing bitwise numeric arithmetic. This library is part of the Scheme Tangerine
;;; edition of the R7RS large language.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2021 Matthias Zenger. All rights reserved.
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

(define-library (scheme bitwise)

  (export bitwise-not
          bitwise-and
          bitwise-ior
          bitwise-xor
          bitwise-eqv
          bitwise-nand
          bitwise-nor
          bitwise-andc1
          bitwise-andc2
          bitwise-orc1
          bitwise-orc2
          arithmetic-shift
          bit-count
          integer-length
          bitwise-if
          bit-set?
          copy-bit
          bit-swap
          any-bit-set?
          every-bit-set?
          first-set-bit
          bit-field
          bit-field-any?
          bit-field-every?
          bit-field-clear
          bit-field-set
          bit-field-replace
          bit-field-replace-same
          bit-field-rotate
          bit-field-reverse
          bits->list
          list->bits
          bits->vector
          vector->bits
          bits
          bitwise-fold
          bitwise-for-each
          bitwise-unfold
          make-bitwise-generator)

  (import (srfi 151))
)
