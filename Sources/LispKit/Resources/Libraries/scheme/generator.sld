;;; SCHEME GENERATOR
;;;
;;; Library implementing generators. This library is part of the Scheme Red edition of the
;;; R7RS large language.
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

(define-library (scheme generator)

  (export generator
          make-iota-generator
          make-range-generator
          make-coroutine-generator
          list->generator
          vector->generator
          reverse-vector->generator
          string->generator
          bytevector->generator
          make-for-each-generator
          make-unfold-generator
          gcons*
          gappend
          gcombine
          gfilter
          gremove
          gtake
          gdrop
          gtake-while
          gdrop-while
          gdelete
          gdelete-neighbor-dups
          gindex
          gselect
          generator->list
          generator->reverse-list
          generator->vector
          generator->vector!
          generator->string
          generator-fold
          generator-for-each
          generator-find
          generator-count
          generator-any
          generator-every
          generator-unfold)

  (import (srfi 121))
)

