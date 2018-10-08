;;; SRFI 16
;;; Syntax for procedures of variable arity
;;;
;;; The SRFI introduces `case-lambda`, a syntax for procedures with a variable number
;;; of arguments. `case-lambda` reduces the clutter of procedures that execute different
;;; code depending on the number of arguments they were passed. It is a pattern-matching
;;; mechanism that matches on the number of arguments. CASE-LAMBDA is available in some
;;; Scheme systems.
;;;
;;; Author of spec: Lars T Hansen
;;;
;;; Copyright © 2018 Matthias Zenger. All rights reserved.
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

(define-library (srfi 16)

  (export case-lambda)

  (import (lispkit core))

  ;; `case-lambda` is implemented natively in library `(lispkit core)`
)

