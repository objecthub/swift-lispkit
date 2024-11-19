;;; Demonstrate usage of OAuth 2.0 authorization
;;; 
;;; This example showcases how to use LispKit's networking libraries to
;;; access web APIs from Google and GitHub. 
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2024 Matthias Zenger. All rights reserved.
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

(import (lispkit base)
        (lispkit http)
        (lispkit http oauth)
        (lispkit http server)
        (lispkit thread)
        (lispkit thread future)
        (lispkit json))

(define min-log-level 2)
(define google-client-id "")
(define google-client-secret "")
(define github-client-id "")
(define github-client-secret "")
(define github-device-client-id "")
(define github-device-client-secret "")

;;; Start a small webserver to receive OAuth 2.0 redirects

(define server (make-http-server))

(http-server-register-default! server
  (lambda (request) (srv-response-not-found)))

(http-server-register! server "/"
  (lambda (request)
    (let ((url (string-append "http://localhost:3000" (srv-request-query request #t))))
      (http-server-log server 2 "handler/redirect" url)
      (if (oauth2-redirect! url)
          (make-srv-response 200 #f "redirect registered")
          (make-srv-response 200 #f "redirect failed")))))

(spawn (thunk (http-server-start! server 3000)))
(thread-yield!)

;;; Access Google's People API

;; Define an OAuth 2.0 authentication client for Google using a code grant flow
(define google
  (make-oauth2 'code-grant
               `((client_id . ,google-client-id)
                 (client_secret . ,google-client-secret)
                 (authorize_uri . "https://accounts.google.com/o/oauth2/auth")
                 (token_uri . "https://www.googleapis.com/oauth2/v3/token")
                 (redirect_uris . #("http://localhost:3000"))
                 (scope . "profile")
                 (auth_embedded . #f)
                 (keychain . #f)
                 (verbose . #t)
                 (log . 4))))

;; Set up an OAuth 2.0 HTTP session that uses the client to authorize requests
(define session (make-oauth2-session google #f #f #t))

;; Define a GET request for retrieving user data
(define request
  (make-http-request "https://people.googleapis.com/v1/people/me?personFields=names" "GET"))

;; Send the request via a OAuth 2.0 session; this returns a future containing
;; the response of the request (or an error if the request failed)
(define result (oauth2-session-send request session))

;; Retrieve the body of the HTTP response from the `result` future; this will
;; block until the response has been received.
(define response (bytevector->json (http-response-content (future-get result))))

(display (json->string response #t))
(newline)

;;; Access GitHub API via a code grant flow

;; Define an OAuth 2.0 authentication client for GitHub using a code grant flow
(define oauth
  (make-oauth2 'code-grant
               `((client_id . ,github-client-id)
                 (client_secret . ,github-client-secret)
                 (authorize_uri . "https://github.com/login/oauth/authorize")
                 (token_uri . "https://github.com/login/oauth/access_token")
                 (redirect_uris . #("http://localhost:3000"))
                 (scope . "user repo:status")
                 (auth_embedded . #f)
                 (keychain . #t)
                 (secret_in_body . #t)
                 (log . ,min-log-level))))

;; Set up an OAuth 2.0 HTTP session that uses the client to authorize requests
(define session (make-oauth2-session oauth))

;; Define a GET request for retrieving user data
(define request (make-http-request "https://api.github.com/user" "GET"))

;; Send the request via a OAuth 2.0 session; this returns a future containing
;; the response of the request (or an error if the request failed)
(define result (oauth2-session-send request session))

;; Retrieve the body of the HTTP response from the `result` future; this will
;; block until the response has been received.
(define response (bytevector->json (http-response-content (future-get result))))

;; Pretty print the response on the terminal
(display (json->string response #t))
(newline)

;;; Access GitHub API via a device grant flow

;; Define an OAuth 2.0 authentication client for GitHub using a code grant flow
(define github
  (make-oauth2 'device-grant
               `((client_id . ,github-device-client-id)
                 (client_secret . ,github-device-client-secret)
                 (device_authorize_uri . "https://github.com/login/device/code")
                 (authorize_uri . "https://github.com/login/oauth/authorize")
                 (token_uri . "https://github.com/login/oauth/access_token")
                 (scope . "user repo:status")
                 (keychain . #t)
                 (secret_in_body . #t)
                 (log . ,min-log-level))))

;; Determine request codes
(define codes (oauth2-request-codes github))
(display* "codes: " (future-get codes))
(newline)

;; Initiate authorization
(define auth (oauth2-authorize! github))
(display* "authorization: " (future-get auth))
(newline)

;; Set up an OAuth 2.0 HTTP session
(define session (make-oauth2-session github))

;; Define a GET request for retrieving user data
(define request (make-http-request "https://api.github.com/user" "GET"))

;; Send the request via a OAuth 2.0 session; this returns a future containing
;; the response of the request (or an error if the request failed)
(define result (oauth2-session-send request session))

;; Retrieve the body of the HTTP response from the `result` future; this will
;; block until the response has been received.
(define response (bytevector->json (http-response-content (future-get result))))

;; Pretty print the response on the terminal
(display* "response:\n" (json->string response #t))
(newline)

;;; Stop the webserver

(http-server-stop! server)
