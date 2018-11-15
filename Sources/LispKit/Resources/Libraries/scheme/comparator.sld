;;; SCHEME COMPARATOR
;;;
;;; Library implementing comparators. This library is part of the R7RS standard.
;;;
;;; Author: Matthias Zenger
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

(define-library (scheme comparator)

  (export comparator?
          comparator-ordered?
          comparator-hashable?
          make-comparator
          make-pair-comparator
          make-list-comparator
          make-vector-comparator
          make-eq-comparator
          make-eqv-comparator
          make-equal-comparator
          boolean-hash
          char-hash
          char-ci-hash
          string-hash
          string-ci-hash
          symbol-hash
          number-hash
          hash-bound hash-salt
          make-default-comparator
          default-hash
          comparator-register-default!
          comparator-type-test-predicate
          comparator-equality-predicate
          comparator-ordering-predicate
          comparator-hash-function
          comparator-test-type
          comparator-check-type
          comparator-hash
          =?
          <?
          >?
          <=?
          >=?
          comparator-if<=>)

  (import (srfi 128))
)
