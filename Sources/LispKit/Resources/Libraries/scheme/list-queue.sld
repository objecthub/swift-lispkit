;;; SCHEME LIST-QUEUE
;;;
;;; List queue library. This library is part of the Scheme Red edition of the R7RS
;;; large language. It forwards all definitions to SRFI 117.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
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

(define-library (scheme list-queue)

  (export make-list-queue
          list-queue
          list-queue-copy
          list-queue-unfold
          list-queue-unfold-right
          list-queue?
          list-queue-empty?
          list-queue-front
          list-queue-back
          list-queue-list
          list-queue-first-last
          list-queue-add-front!
          list-queue-add-back!
          list-queue-remove-front!
          list-queue-remove-back!
          list-queue-remove-all!
          list-queue-set-list!
          list-queue-append
          list-queue-append!
          list-queue-concatenate
          list-queue-map
          list-queue-map!
          list-queue-for-each)

  (import (srfi 117))
)
