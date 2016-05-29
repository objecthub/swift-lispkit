;;; Definitions.scm
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
  "Mixed global local definitions"
  111
  (define bar 1)
  (+ (begin (define (foo n) (+ n bar))
            (define bar 100)
            (foo 10))
     bar)
)

(
  "Definitions via syntax rules"
  111
  (define-syntax declare
    (syntax-rules ()
      ((_ f e) (define f e))))
  (declare baz 1)
  (+ (begin (define (foo n) (+ n baz))
            (declare baz 100)
            (foo 10))
     baz)
)
