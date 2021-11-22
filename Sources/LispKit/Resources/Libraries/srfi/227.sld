;;; SRFI 227
;;; Optional Arguments
;;;
;;; This SRFI specifies the `opt-lambda` syntax, which generalizes `lambda`. An
;;; `opt-lambda` expression evaluates to a procedure that takes a number of
;;; required and a number of optional (positional) arguments whose default values
;;; are determined by evaluating corresponding expressions when the procedure is
;;; called. This SRFI also specifies a variation `opt*-lambda`, which is to
;;; `opt-lambda` as `let*` is to `let` and the related binding constructs
;;; `let-optionals` and `let-optionals*`. Finally, for those who prefer less explicit
;;; procedure definitions, `define-optionals` and `define-optionals*` are provided.
;;; 
;;; Author of spec: Marc Nieper-Wißkirchen
;;; 
;;; Copyright © 2021 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not use
;;; this file except in compliance with the License. You may obtain a copy of the
;;; License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed
;;; under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
;;; CONDITIONS OF ANY KIND, either express or implied. See the License for the
;;; specific language governing permissions and limitations under the License.

(define-library (srfi 227)

  (export (rename let*-optionals let-optionals*)
          let-optionals
          opt-lambda
          opt*-lambda
          define-optionals
          define-optionals*)
  
  (import (lispkit base))
)
