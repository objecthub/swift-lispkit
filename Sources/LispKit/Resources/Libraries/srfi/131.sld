;;; SRFI 131
;;; ERR5RS Record Syntax (reduced)
;;;
;;; This SRFI is a reduced version of the SRFI 99 syntactic layer that can be
;;; implemented with syntax-rules without requiring low-level macros. Like
;;; SRFI-99's syntax layer, it is backward compatible with the `define-record-type`
;;; macro from SRFI 9 or R7RS-small. It is forward compatible with SRFI 99.
;;;
;;; Author of spec: John Cowan, Will Clinger
;;;
;;; Copyright © 2023 Matthias Zenger. All rights reserved.
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

(define-library (srfi 131)

  (export define-record-type)

  (import (lispkit record))

  ;; The form is implemented natively in library `(lispkit record)`
)
