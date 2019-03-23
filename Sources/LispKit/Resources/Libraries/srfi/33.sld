;;; SRFI 33
;;; Integer Bitwise-operation Library
;;;
;;; R5RS Scheme has no utilities for performing bitwise logical operations on integers
;;; or bitstrings, which is a problem for authors of portable code. This SRFI proposes
;;; a coherent and comprehensive set of these functions. The precise semantics of these
;;; operators is almost never an issue. A consistent, portable set of *names* and
;;; parameter conventions, however, is. Hence this SRFI.
;;;
;;; Author of spec: Olin Shivers
;;;
;;; Copyright © 2019 Matthias Zenger. All rights reserved.
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

(define-library (srfi 33)

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
          bitwise-merge
          bit-set?
          any-bits-set?
          all-bits-set?
          first-set-bit
          extract-bit-field
          test-bit-field?
          clear-bit-field
          replace-bit-field
          copy-bit-field)

  (import (except (lispkit base) integer-length
                                 bit-set?
                                 bitwise-not
                                 bitwise-and
                                 bitwise-ior
                                 bitwise-xor
                                 copy-bit
                                 bit-count
                                 arithmetic-shift)

          (rename (srfi 142) (bitwise-if bitwise-merge)
                             (any-bit-set? any-bits-set?)
                             (every-bit-set? all-bits-set?)
                             (bit-field-any? test-bit-field?)
                             (bit-field-clear clear-bit-field)))

  (begin

    (define (mask len)
      (- (arithmetic-shift 1 len) 1))

    (define (extract-bit-field size position n)
      (bitwise-and (arithmetic-shift n (- position)) (mask size)))

    (define (replace-bit-field size position newfield n)
      (bitwise-ior (bitwise-and n (bitwise-not (arithmetic-shift (mask size) position)))
                   (arithmetic-shift newfield position)))

    (define (copy-bit-field size position from to)
      (bitwise-merge (arithmetic-shift (mask size) position) to from))
  )
)

