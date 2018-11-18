;;; SCHEME IDEQUE
;;;
;;; Library implementing immutable deques. This library is part of the Scheme Red edition of
;;; the R7RS large language.
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

(define-library (scheme ideque)

  (export ideque
          ideque-tabulate
          ideque-unfold
          ideque-unfold-right
          ideque?
          ideque-empty?
          ideque=
          ideque-any
          ideque-every
          ideque-front
          ideque-add-front
          ideque-remove-front
          ideque-back
          ideque-add-back
          ideque-remove-back
          ideque-ref
          ideque-take
          ideque-take-right
          ideque-drop
          ideque-drop-right
          ideque-split-at
          ideque-length
          ideque-append
          ideque-reverse
          ideque-count
          ideque-zip
          ideque-map
          ideque-filter-map
          ideque-for-each
          ideque-for-each-right
          ideque-fold
          ideque-fold-right
          ideque-append-map
          ideque-filter
          ideque-remove
          ideque-partition
          ideque-find
          ideque-find-right
          ideque-take-while
          ideque-take-while-right
          ideque-drop-while
          ideque-drop-while-right
          ideque-span
          ideque-break
          list->ideque
          ideque->list
          generator->ideque
          ideque->generator)

  (import (srfi 134))
)
