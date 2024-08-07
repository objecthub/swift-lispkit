;;; Webserver
;;;
;;; This program implements a simple web server (only the HTTP protocol is supported)
;;; with the framework of library `(lispkit http server)`. HTML gets generated from
;;; SXML, which the various request handlers are generating. Alternatively, plain text
;;; HTML in string form, Markdown, and JSON is supported to generate the content of HTTP
;;; responses. Headers are represented as association lists, where each header is
;;; represented as a pair of strings.
;;;
;;; This code was inspired by the demo for Swifter, written by Damian Kołakowski. See:
;;; https://github.com/httpswift/swifter
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2024 Matthias Zenger. All rights reserved.
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
        (lispkit box)
        (lispkit thread)
        (lispkit http server))

; Create a new HTTP server
(define server (make-http-server))

; Register a default handler
(http-server-register-default! server
  (lambda (request) (srv-response-not-found)))

; Serve a page under path "/" that lists all routes
(http-server-register! server "/"
  (lambda (request)
    (make-srv-response 200 #f
      `(p (b "Available routes:")
          (ul . ,(map (lambda (x) `(li (a (@ (href ,x)) ,x))) (http-server-routes server)))))))

(http-server-register! server "GET" "/magic"
  (lambda (request)
    (make-srv-response 200 #f "let the magic happen")))

(http-server-register! server "GET" "/foo/bar"
  (lambda (request)
    (make-srv-response 200 #f "this triggered /foo/bar")))

(http-server-register! server "GET" "/close"
  (lambda (request)
    (http-server-stop! (srv-request-server request))
    (make-srv-response 200 #f "this closed the server")))

(http-server-register! server "GET" "/error"
  (lambda (request)
    (error "Something really bad has happened ($0)" request server)))

(http-server-register! server "/path"
  (lambda (request)
    (make-srv-response 200
      '(("XXX-Custom-Header" . "value"))
      `(p "You asked for " ,(srv-request-path request)))))

(http-server-register! server "/test/:param1/:param2"
  (lambda (req)
    (make-srv-response 200 #f
      `((h3 "Address: " ,(or (srv-request-address req) "unknown"))
        (h3 "URL: " ,(srv-request-path req))
        (h3 "Method: " ,(srv-request-method req))
        (h3 "Query params:")
        (table
          ,(map (lambda (x) `(tr (td ,(car x)) (td ,(cdr x)))) (srv-request-query-params req)))
        (h3 "Path params:")
        (table
          ,(map (lambda (x) `(tr (td ,(car x)) (td ,(cdr x)))) (srv-request-path-params req)))
        (h3 "Headers:")
        (table
          ,(map (lambda (x) `(tr (td ,(car x)) (td ,(cdr x)))) (srv-request-headers req)))))))

(http-server-register! server "GET" "/upload"
  (lambda (req)
    (make-srv-response 200 #f
      `(form (@ (method "POST")
                 (action "/upload")
                 (enctype "multipart/form-data"))
         (input (@ (name "my_file1")(type "file")))
         (input (@ (name "my_file2")(type "file")))
         (input (@ (name "my_file3")(type "file")))
         (button (@ (type "submit")) "Upload")))))

(http-server-register! server "POST" "/upload"
  (lambda (req)
    (make-srv-response 200 #f
      `(table
         ,(map (lambda (x)
                 `((tr (td (@ (colspan 2)) (b "Part"))) .
                   ,(map (lambda (y)
                           `(tr (td ,(car y)) (td ,(cdr y)))) (srv-multipart-headers x))))
               (srv-request-form-multiparts req))))))

(http-server-register! server "GET" "/login"
  (lambda (req)
    (srv-response-ok
      `(html
         (head
           (script (@ (src "http://cdn.staticfile.org/jquery/2.1.4/jquery.min.js")))
           (stylesheet (@ (href "http://cdn.staticfile.org/twitter-bootstrap/3.3.0/css/bootstrap.min.css"))))
         (body
           (h3 "Sign In")
           (form (@ (method "POST")
                    (action "/login"))
             (fieldset
               (input (@ (name "email")(type "email")(placeholder "E-mail")(autofocus "")))
               (input (@ (name "password")(type "password")(placeholder "Password")(autofocus "")))
               (a (@ (href "/login"))
                  (button (@ (type "submit")) "Login"))))
          (javascript (@ (src "http://cdn.staticfile.org/twitter-bootstrap/3.3.0/js/bootstrap.min.js"))))))))

(http-server-register! server "POST" "/login"
  (lambda (req)
    (srv-response-ok
      '(("XXX-Custom-Header" . "value"))
      `(p ,@(map (lambda (x) `(,(car x) " = " ,(cdr x) (br))) (srv-request-form-attributes req))))))

(http-server-register! server "/redirect/permanently"
  (lambda (req)
    (srv-response-moved-permanently "http://www.google.com")))

(http-server-register! server "/redirect/temporarily"
  (lambda (req)
    (srv-response-moved-temporarily "http://www.google.com")))

(http-server-register! server "/public/::path"
  (share-directory-handler "/Users/zenger/Downloads"))
  
(http-server-register! server "/files/::path"
  (browse-directory-handler "/Users/zenger/Downloads"))

(http-server-register! server "/long"
  (lambda (request)
    (do ((str "")
         (i 0 (+ i 1)))
        ((>= i 1000) (srv-response-ok '(("XXX-Custom-Header" . "value")) str))
      (string-append! str "(" (number->string i) "),->"))))

(http-server-register! server "GET" "/wildcard/*/test/*/:param"
  (lambda (request)
    (make-srv-response 200 '(("XXX-Custom-Header" . "value")) (srv-request-path request))))

(http-server-register! server "GET" "/not/implemented"
  (lambda (request) (srv-response-not-implemented)))

; Counter for processed requests
(define requests-served (make-atomic-box 0))

; Use this middleware processor to count served requests with an atomic box (needed
; because middleware is executed concurrently on potentially many different threads)
(http-server-register-middleware! server
  (lambda (request)
    (atomic-box-inc+mul! requests-served 1)
    #f))

; Print all log messages
; (http-server-log-severity-set! server 0) 

; Display all supported server routes
(display* "server routes: " (http-server-routes server) "\n")

; Start the server. This procedure only terminates when the server is shut down
; (e.g. via a GET request for path "/close")
(http-server-start! server 8090)

; Wait for all threads to be terminated, but not longer than 10s
(wait-threads-terminated 10)

; Display the number of requests served
(display* (atomic-box-ref requests-served) " requests served.\n")
