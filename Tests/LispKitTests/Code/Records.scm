;;; Records.scm
;;; Regression test data
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2016 ObjectHub. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;      http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(
  "Record types"
  ("recordtype" (one two three) 1)
  (define rtype (make-record-type "recordtype" '(one two three)))
  (list (record-type-name rtype)
        (record-type-field-names rtype)
        (record-type-field-index rtype 'two))
)

(
  "Manual records"
  (1 2 3)
  (define rec (make-record rtype))
  (record-set! rec 0 1)
  (record-set! rec 1 2)
  (record-set! rec 2 3)
  (list (record-ref rec 0)
        (record-ref rec 1)
        (record-ref rec 2))
)

(
  "Record predicates"
  (#t #t #t #f #f)
  (list (record? rtype)
        (record-type? rtype)
        (record? rec)
        (record-type? rec)
        (record? #(1 2 3)))
)

(
  "Define record type"
  (#t #t #t #t #t #t)
  (define-record-type pare (kons x y) pare? (x kar set-kar!) (y kdr))
  (list (record-type? pare)
        (procedure? kons)
        (procedure? pare?)
        (procedure? kar)
        (procedure? set-kar!)
        (procedure? kdr))
)

(
  "Use defined record type"
  (#t #t #t 1 7 3)
  (define k (kons 1 (kons 2 (kons 3 #f))))
  (set-kar! (kdr k) 7)
  (list (record? k)
        (record? (kdr k))
        (and (pare? k) (pare? (kdr k)))
        (kar k)
        (kar (kdr k))
        (kar (kdr (kdr k))))
)
