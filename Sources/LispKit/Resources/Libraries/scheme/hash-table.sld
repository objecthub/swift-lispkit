;;; SCHEME HASH-TABLE
;;;
;;; Scheme hashtable library. This library is part of the Scheme Red edition of the R7RS
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

(define-library (scheme hash-table)

  (export make-hash-table
          hash-table
          hash-table-unfold
          alist->hash-table
          hash-table?
          hash-table-contains?
          hash-table-exists?
          hash-table-empty?
          hash-table=?
          hash-table-mutable?
          hash-table-ref
          hash-table-ref/default
          hash-table-set!
          hash-table-delete!
          hash-table-intern!
          hash-table-update!
          hash-table-update!/default
          hash-table-pop!
          hash-table-clear!
          hash-table-size
          hash-table-keys
          hash-table-values
          hash-table-entries
          hash-table-find
          hash-table-count
          hash-table-map
          hash-table-for-each
          hash-table-walk
          hash-table-map!
          hash-table-map->list
          hash-table-fold
          hash-table-prune!
          hash-table-copy
          hash-table-empty-copy
          hash-table->alist
          hash-table-union!
          hash-table-merge!
          hash-table-intersection!
          hash-table-difference!
          hash-table-xor!
          hash
          string-hash
          string-ci-hash
          hash-by-identity
          hash-table-equivalence-function
          hash-table-hash-function)

  (import (srfi 125))
)

