;;; SCHEME SORT
;;;
;;; Scheme sorting library. This library is part of the Scheme Red edition of the R7RS
;;; large language. It is incomplete in that it does not support mutable list-related
;;; procedures due to LispKit's lists being immutable. The following procedures are
;;; missing: `list-delete-neighbor-dups!`, `list-merge!`, `list-sort!`, `list-stable-sort!`.
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

(define-library (scheme sort)

  (export list-sorted?
          vector-sorted?
          list-sort
          list-stable-sort
          ; list-sort!
          ; list-stable-sort!
          vector-sort
          vector-stable-sort
          vector-sort!
          vector-stable-sort!
          list-merge
          ; list-merge!
          vector-merge
          vector-merge!
          list-delete-neighbor-dups
          ; list-delete-neighbor-dups!
          vector-delete-neighbor-dups
          vector-delete-neighbor-dups!
          vector-find-median
          vector-find-median!
          vector-select!
          vector-separate!)

   (import (srfi 132))
 )

