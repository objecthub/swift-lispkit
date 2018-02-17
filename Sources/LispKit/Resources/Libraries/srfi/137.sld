;;; SRFI 137
;;; Minimal Unique Types
;;;
;;; This SRFI is intended to standardize a primitive run-time mechanism to create
;;; disjoint types. This mechanism provides a simple hook to create new data types at
;;; run time that are disjoint from all existing types. allowing portable libraries to
;;; implement SRFI 9, SRFI 99, SRFI 131, SRFI 135, R6RS records, Chicken records, CLOS,
;;; persistent databases, remote access to data on servers, and the like on top of it.
;;; It is also portably implementable and usable entirely separately from any of these.
;;;
;;; Note that there is no concept of a type object here: a type is simply a name for a
;;; group of closely linked procedures that allow the creation and manipulation of type
;;; instances (which are objects) and subtypes. This SRFI exposes no ambient authority,
;;; and relies entirely on module exports for access control.
;;;
;;; Author of spec: John Cowan, Marc Nieper-Wißkirchen
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

(define-library (srfi 137)
  (export make-type)
  (import (rename (lispkit base)
                  (make-type native-make-type)))

  (begin
    (define (make-make-type mk-type)
      (lambda (payload)
        (let-values (((constructor predicate accessor mk-subtype) (mk-type payload)))
          (values (lambda () payload)
                  constructor
                  predicate
                  accessor
                  (make-make-type mk-subtype)))))

    (define make-type (make-make-type native-make-type))
  )
)
