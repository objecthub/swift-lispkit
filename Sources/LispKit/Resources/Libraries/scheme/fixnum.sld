;;; SCHEME FIXNUM
;;;
;;; Scheme fixnum library. This library is part of the Scheme Tangerine edition of the R7RS
;;; large language.
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

(define-library (scheme fixnum)
  
  (export fx-width
          fx-greatest
          fx-least
          fixnum?
          fx=?
          fx<?
          fx>?
          fx<=?
          fx>=?
          fxzero?
          fxpositive?
          fxnegative?
          fxodd?
          fxeven?
          fxmax
          fxmin
          fxneg
          fx+
          fx-
          fx*
          fxquotient
          fxremainder
          fxabs
          fxsquare
          fxsqrt
          fx+/carry
          fx-/carry
          fx*/carry
          fxnot
          fxand
          fxior
          fxxor
          fxarithmetic-shift
          fxarithmetic-shift-left
          fxarithmetic-shift-right
          fxbit-count
          fxlength
          fxif
          fxbit-set?
          fxcopy-bit
          fxfirst-set-bit
          fxbit-field
          fxbit-field-rotate
          fxbit-field-reverse)
  
  (import (srfi 143))
)

