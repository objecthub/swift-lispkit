;;; LISPKIT REGRESSION TEST SUITE
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2019-2020 Matthias Zenger. All rights reserved.
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

(load "Tests/R7RS")
(load "Tests/LispKit-Match")
(load "Tests/SRFI-14")
(load "Tests/SRFI-101")
(load "Tests/SRFI-125")
(load "Tests/SRFI-128")
(load "Tests/SRFI-143")
(load "Tests/SRFI-146")
(load "Tests/SRFI-154")
(load "Tests/SRFI-155")
(load "Tests/SRFI-165")
(load "Tests/SRFI-167")
(load "Tests/SRFI-173")
(load "Tests/SRFI-174")
(load "Tests/SRFI-175")
(load "Tests/SRFI-189")
; removed the following test suite as it is slow to execute
; (load "Tests/SRFI-194")
(load "Tests/SRFI-196")
(load "Tests/SRFI-204")
(load "Tests/SRFI-209")
(load "Tests/SRFI-210")
(load "Tests/SRFI-214")
(load "Tests/SRFI-217")
(load "Tests/SRFI-219")
(load "Tests/SRFI-221")
(load "Tests/SRFI-222")
(load "Tests/SRFI-223")
(load "Tests/SRFI-224")

(test-end)

;; Current number of failures: 18 (all from R7RS-Tests)
