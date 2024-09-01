;;; HTTP GET demo
;;;
;;; This is a small example for how to use the LispKit-specific `http-get`
;;; function. `http-get` expects two parameters `url` and `timeout`. `url`
;;; refers to the URL from which content should be fetched. `timeout` is a
;;; floating point number defining the time in seconds it should take at
;;; most for receiving a response. `http-get` returns two values: the HTTP
;;; headers in form of an alist, and the content in form of a bytevector.
;;; The example below assumes that the content is a UTF8 encoded string.
;;;
;;; With the example functions below, it is possible to display this example
;;; code by fetching it from GitHub:
;;; (display (http-get-content "https://raw.githubusercontent.com/objecthub/swift-lispkit/master/Sources/LispKit/Resources/Examples/HTTP.scm"))
;;;
;;; There is also a way to open a web page in a browser by using the
;;; `open-url` function. The following code opens the GitHub page of LispKit
;;; in a browser: (open-url "https://github.com/objecthub/swift-lispkit")
;;;
;;; For more control and an asynchronous API, the library `(lispkit http)`
;;; provides a dedicated API for handling the HTTP protocol.
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2018 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may
;;; not use this file except in compliance with the License. You may obtain
;;; a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(import (lispkit base))

(define (http-get-header url)
  (let-values (((header _) (http-get url)))
    header))

(define (http-get-content url)
  (let-values (((header content) (http-get url)))
    (utf8->string content)))

