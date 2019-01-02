;;; SCHEME CHARSET
;;;
;;; Library implementing character sets. This library is part of the Scheme Red edition of the
;;; R7RS large language.
;;;
;;; Author: Matthias Zenger
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

(define-library (scheme charset)

  (export char-set?
          char-set=
          char-set<=
          char-set-hash
          char-set-cursor
          char-set-ref
          char-set-cursor-next
          end-of-char-set?
          char-set-fold
          char-set-unfold
          char-set-unfold!
          char-set-for-each
          char-set-map
          char-set-copy
          char-set
          list->char-set
          string->char-set
          list->char-set!
          string->char-set!
          char-set-filter
          ucs-range->char-set
          ->char-set
          char-set-filter!
          ucs-range->char-set!
          char-set->list
          char-set->string
          char-set-size
          char-set-count
          char-set-contains?
          char-set-every
          char-set-any
          char-set-adjoin
          char-set-delete
          char-set-adjoin!
          char-set-delete!
          char-set-complement
          char-set-union
          char-set-intersection
          char-set-complement!
          char-set-union!
          char-set-intersection!
          char-set-difference
          char-set-xor
          char-set-diff+intersection
          char-set-difference!
          char-set-xor!
          char-set-diff+intersection!
          char-set:lower-case
          char-set:upper-case
          char-set:title-case
          char-set:letter
          char-set:digit
          char-set:letter+digit
          char-set:graphic
          char-set:printing
          char-set:whitespace
          char-set:iso-control
          char-set:punctuation
          char-set:symbol
          char-set:hex-digit
          char-set:blank
          char-set:ascii
          char-set:empty
          char-set:full)

  (import (srfi 14))
)
