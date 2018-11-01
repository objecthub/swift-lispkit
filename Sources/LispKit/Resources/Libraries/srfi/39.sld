;;; SRFI 39
;;; Parameter objects
;;;
;;; This SRFI defines parameter objects, the procedure `make-parameter` to create parameter
;;; objects and the `parameterize` special form to dynamically bind parameter objects.
;;; In the dynamic environment, each parameter object is bound to a cell containing the
;;; value of the parameter. When a procedure is called, the called procedure inherits the
;;; dynamic environment from the caller. The `parameterize` special form allows the binding
;;; of a parameter object to be changed for the dynamic extent of its body.
;;;
;;; Author of spec: Marc Feeley
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

(define-library (srfi 39)

  (export make-parameter
          parameterize)

  (import (lispkit dynamic))

  ;; `make-parameter` and `parameterize` are implemented natively in library
  ;; `(lispkit dynamic)`
)
