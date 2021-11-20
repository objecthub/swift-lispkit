;;; SRFI 229
;;; Tagged Procedures
;;;
;;; This SRFI defines tagged procedures, which are procedures that are tagged with
;;; a Scheme value when created through the syntax `lambda/tag` and `case-lambda/tag`.
;;; The value of the tag of a procedure can be retrieved with `procedure-tag`, and
;;; the predicate `procedure/tag?` discerns whether a procedure is tagged.
;;;
;;; Author of spec: Marc Nieper-Wißkirchen
;;; 
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

(define-library (srfi 229)

  (export case-lambda/tag
          lambda/tag
          procedure/tag?
          procedure-tag)
  
  (import (lispkit base))
)
