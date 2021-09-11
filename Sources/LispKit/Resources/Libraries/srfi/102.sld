;;; SRFI 102
;;; Procedure Arity Inspection
;;;
;;; This SRFI identifies a common, core set of operations that make it possible to
;;; inspect the arity of procedures and determine if a given procedure accepts a
;;; given number of arguments.
;;;
;;; Author of spec: David Van Horn
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

(define-library (srfi 102)

  (export procedure-arity
          arity-at-least?
          arity-at-least-value
          procedure-arity-includes?)

  (import (lispkit core))
)
