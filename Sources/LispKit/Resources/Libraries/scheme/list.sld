;;; SCHEME LIST
;;;
;;; Library providing list functions. This library is part of the R7RS standard but is not
;;; fully compatible due to the lack of immutable lists in LispKit.
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

(define-library (scheme list)

  (export xcons
          cons*
          make-list
          list-tabulate
          iota
          proper-list?
          dotted-list?
          not-pair?
          null-list?
          list=
          first
          second
          third
          fourth
          fifth
          sixth
          seventh
          eighth
          ninth
          tenth
          car+cdr
          take
          drop
          take-right
          drop-right
          split-at
          last
          last-pair
          concatenate
          zip
          unzip1
          unzip2
          unzip3
          unzip4
          unzip5
          count
          fold
          unfold
          pair-fold
          reduce
          fold-right
          unfold-right
          pair-fold-right
          reduce-right
          append-map
          pair-for-each
          filter-map
          map-in-order
          filter
          partition
          remove
          find
          find-tail
          any
          every
          list-index
          take-while
          drop-while
          span
          break
          delete
          delete-duplicates
          alist-cons
          alist-copy
          alist-delete
          lset<=
          lset=
          lset-adjoin
          lset-union
          lset-intersection
          lset-difference
          lset-xor
          lset-diff+intersection)

  (import (srfi 1))
)

