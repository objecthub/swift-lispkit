;;; Showcases how to manage secrets
;;; 
;;; This is demo code for the libraries `(lispkit serialize)` and
;;; `(lispkit system keychain)`. `(lispkit serialize)` provides an API for
;;; serializing Scheme expressions, i.e. turning them into binary data which
;;; can be deserialized efficiently back into structured data. Library
;;; `(lispkit system keychain)` integrates LispKit into the _Keychain_ of
;;; macOS and iOS. It provides means to store private and sensitive strings
;;; and binary data in the system keychain in a safe way. Thus, passwords,
;;; certificates, etc. do not need to be stored in code, files or databases.
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
        (lispkit date-time)
        (lispkit serialize)
        (lispkit system keychain))

;; Define string constants used for service and key references
(define service "LispKit Example")
(define last-user "last user")
(define last-user-alt "last user alt")
(define complex-data "complex data")
(define complex-data-alt "complex data alt")

;; Create a new keychain accessor for the given service
(define kc (make-keychain service))

;; Check if this program was run before
(if (keychain-exists? kc last-user)
    (let-values (((username time) (keychain-ref kc last-user)))
      (display* "[use] last usage on " (date-time->string time) " by " username))
    (display "[use] this program is run for the first time"))
(newline)

;; Update username and timestamp
(keychain-set! kc last-user (values (current-user-name) (date-time)))

;; Doing the same again using keychain metadata
(let ((attribs (keychain-ref-attributes kc last-user-alt)))
  (if attribs
      (let ((username (cdr (assoc 'value attribs)))
            (time (cdr (assoc 'modification-date attribs))))
        (display* "[alt] last usage on " (date-time->string time) " by " username))
      (display "[alt] this program is run for the first time")))
(newline)

;; Update username. The modification timestamp is set automatically.
(keychain-set-string! kc last-user-alt (current-user-name))

;; Write a complex, recursive data structure to the keychain
(define str "hello")
(define ht (alist->eq-hashtable `(("zero" . (,str ,str ,str))
                                  ("one" . 1)
                                  (two . 2.3456)
                                  (3 . (t h r e e))
                                  ("four" . #(f o u r)))))

;; The following line makes `ht` a recursive data structure
(hashtable-set! ht '(five 5) ht)

;; Write `ht` to the keychain; this will automatically serialize `ht` and
;; write the serialized value into the keychain.
(keychain-set! kc complex-data ht)

;; Read `ht` back
(define ht2 (keychain-ref kc complex-data))

(if (equal? ht ht2)
    (display "complex data written and read correctly")
    (display "complex data not retrieved correctly"))
(newline)

;; Repeat this with manual serialization of the data structure `ht`
(keychain-set-data! kc complex-data-alt (serialize ht))
(define ht3 (deserialize (keychain-ref-data kc complex-data-alt)))
(if (equal? ht ht3)
    (display "complex data written and read correctly again")
    (display "complex data not retrieved correctly again"))
(newline)

;; Display the available keychain keys in service "LispKit Example"
(display "available keys: ")
(write (keychain-keys kc))
(newline)

;; Remove the keychain item "complex data" and "complex data alt"
(keychain-remove! kc complex-data)
(keychain-remove! kc complex-data-alt)

;; Display again the available keychain keys in service "LispKit Example"
(display "remaining available keys: ")
(write (keychain-keys kc))
(newline)
