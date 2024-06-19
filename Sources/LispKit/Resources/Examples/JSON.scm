;;; Validate JSON data with JSON schema
;;;
;;; TODO
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2024 Matthias Zenger. All rights reserved.
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

(import (lispkit base)
        (lispkit json)
        (lispkit json schema))

;; Make a new JSON schema registry. The first `#t` argument makes the schema definitions
;; in the asset directory `JSON/Schema/2020-12` available via the Schema identifier base
;; `https://json-schema.org/draft/2020-12`. The second `#t` argument makes the schema
;; definitions in the asset directory `JSON/Schema/custom` available via the Schema
;; identifier base `https://lisppad.app/schema`.
(define registry (make-schema-registry json-draft2020-default #t #t))

(define person-schema (schema-registry-ref "https://lisppad.app/schema/person" registry))

(define person0 (json '(
  (name . "John Doe")
  (birthday . "1983-03-19")
  (numChildren . 2)
  (email . #("john@doe.com" "john.doe@gmail.com"))
)))

(display (json->string person0 #t))
(newline)
(display* "valid person: " (json-valid? person0 person-schema #t registry) " [#t]")
(newline)

(define person1 (json '(
  (name . "John Doe")
  (email . #("john@doe.com" "john.doe@gmail.com"))
)))

(display (json->string person1 #t))
(newline)
(display* "valid person: " (json-valid? person1 person-schema #t registry) " [#f]")
(newline)

(define person2 (json '(
  (name . "John Doe")
  (birthday . "1983-03-19")
  (address . "12 Main Street, 17445 Noname")
)))

(display (json->string person2 #t))
(newline)

(define res (json-validate person2 person-schema #t registry))
(display* "valid person: " (validation-result-valid? res) " [#t]")
(newline)

(for-each
  (lambda (x)
    (if (cadr x)
        (display* (car x) " exists; defaults: " (caddr x))
        (display* (car x) " does not exist; defaults: " (caddr x)))
    (newline))
  (validation-result-defaults res))
