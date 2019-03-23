;;; SRFI 55
;;; require-extension
;;;
;;; This SRFI specifies an extremely simple facility for making an extension or library
;;; available to a Scheme toplevel environment.
;;;
;;; Authors of spec: Felix L. Winkelmann, D.C. Frost
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

(define-library (srfi 55)

  (export require-extension)

  (import (lispkit base))

  (begin
    (define-syntax require-extension
      (syntax-rules ()
        ((require-extension (prefix mod ...))
          (begin (import (prefix mod) ...)))))
  )
)
