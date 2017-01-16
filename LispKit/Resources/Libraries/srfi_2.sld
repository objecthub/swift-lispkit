;;; SRFI 2
;;; AND-LET*: an AND with local bindings, a guarded LET* special form
;;;
;;; Like an ordinary AND, an AND-LET* special form evaluates its arguments - expressions - one
;;; after another in order, till the first one that yields #f. Unlike AND, however, a non-#f
;;; result of one expression can be bound to a fresh variable and used in the subsequent
;;; expressions. AND-LET* is a cross-breed between LET* and AND.
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

(define-library (srfi 2)
  (export and-let*)
  (import (scheme base))

  (begin
    (define-syntax and-let*
      (syntax-rules ()
        ; no claws
        ((and-let* ())
          #t)
        ((and-let* () body ...)
          (begin body ...))
        ; claws but no body
        ((and-let* ((var expr)))
          (let ((var expr)) var))
        ((and-let* ((expr)))
          expr)
        ((and-let* (var))
          (begin (let ((var #f)) #f) var)) ; do this to check that `var` is an identifier
        ; both claws and body
        ((and-let* ((var expr) . bindings) . body)
          (let ((var expr)) (and var (and-let* bindings . body))))
        ((and-let* ((expr) . bindings) . body)
          (and expr (and-let* bindings . body)))
        ((and-let* (var . bindings) . body)
          (begin (let ((var #f)) #f) (and var (and-let* bindings . body))))))
  )
)
