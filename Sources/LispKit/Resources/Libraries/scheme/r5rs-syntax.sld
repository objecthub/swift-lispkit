;;; SCHEME R5RS-SYNTAX
;;;
;;; Library exporting syntax definitions from R5RS. This library is used to implement
;;; `null-environment`.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2017 Matthias Zenger. All rights reserved.
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

(define-library (scheme r5rs-syntax)
  (export and
          begin
          case
          cond
          define
          define-syntax
          delay
          do
          if
          lambda
          let
          let-syntax
          let*
          letrec
          letrec-syntax
          or
          quasiquote
          quote
          set!
          syntax-rules)
  (import (lispkit base))
)
