;;; SCHEME VECTOR
;;;
;;; Scheme vector library. This library is part of the Scheme Red edition of the R7RS
;;; large language.
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

(define-library (scheme vector)

  (export make-vector
          vector
          vector-unfold
          vector-unfold-right
          vector-copy
          vector-reverse-copy
          vector-append
          vector-concatenate
          vector-append-subvectors
          vector?
          vector-empty?
          vector=
          vector-ref
          vector-length
          vector-fold
          vector-fold-right
          vector-map
          vector-map!
          vector-for-each
          vector-count
          vector-cumulate
          vector-index
          vector-index-right
          vector-skip
          vector-skip-right
          vector-binary-search
          vector-any vector-every
          vector-partition
          vector-set!
          vector-swap!
          vector-fill!
          vector-reverse!
          vector-copy!
          vector-reverse-copy!
          vector-unfold!
          vector-unfold-right!
          vector->list
          reverse-vector->list
          list->vector
          reverse-list->vector
          vector->string
          string->vector)

  (import (except (lispkit vector) vector-map!)
          (srfi 133))
)
