;;; LISPKIT REGRESSION TEST SUITE
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

(import (lispkit base)
        (lispkit test))

(test-begin "LispKit regression tests")

(load "Tests/R7RS-Tests")
(load "Tests/SRFI-14-Tests")
(load "Tests/SRFI-101-Tests")
(load "Tests/SRFI-125-Tests")

(test-end)

;; Current number of failures: 26 (all from R7RS-Tests)
