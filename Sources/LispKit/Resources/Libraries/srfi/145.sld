;;; SRFI 145
;;; Assumptions
;;;
;;; This SRFI specifies a means to denote the invalidity of certain code paths in a
;;; Scheme program. It allows Scheme code to turn the evaluation into a user-defined
;;; error that need not be signalled by the implementation. Optimizing compilers may
;;; use these denotations to produce better code and to issue better warnings about
;;; dead code.
;;;
;;; Author of spec: Marc Nieper-Wißkirchen
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

(define-library (srfi 145)
  (export assume)
  (import (lispkit base))

  (begin
    (define-syntax assume
      (syntax-rules ()
        ((_ expression message ...)
          (unless expression (error "invalid assumption" (quote expression) message ...)))))
  )
)
