;;; LISPKIT SYSTEM UTIL
;;;
;;; Library providing system-level utilities starting with a standardized
;;; approach to access and store credentials such as passwords, API keys,
;;; client ids, etc.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not
;;; use this file except in compliance with the License. You may obtain a copy
;;; of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;;; License for the specific language governing permissions and limitations
;;; under the License.

(define-library (lispkit system util)

  (export manage-credential)
  
  (import (lispkit base)
          (lispkit system keychain))
  
  (begin
    
    ;; Returns two values: an accessor to a credential and a setter of a
    ;; credential. `manage-credential` retrieves a credential from three
    ;; different sources in the following order:
    ;;   1. A provided `default` (this overrides sources 2 and 3)
    ;;   2. The value of the environment variable `envvar` (overriding source 3)
    ;;   3. The secret stored in a keychain entry (described by a given service)
    ;; The setter returned by `manage-credential` sets the value of a keychain
    ;; entry and also caches the value locally, so that access to the credential
    ;; is fast.
    (define (manage-credential default . args)
      (let-optionals args ((envvar #f)
                           (kc #f)
                           (key #f)
                           (label "net.objecthub.LispPad.credential")
                           (comment #f))
        (let-optionals (if (pair? kc) kc (list kc))
                          ((service #f)
                           (group #f)
                           (acc #f)
                           (sync '()))
          (define keychain
            (or (and (keychain? service)
                     (string? key)
                     service)
                (and (string? service)
                     (string? key)
                     (make-keychain service group acc sync))))
          (define (accessor)
            (or default
                (and envvar (get-environment-variable envvar))
                (and keychain (keychain-ref-string keychain key))))
          (define (setter value . args)
            (let-optionals args ((label0 #f)
                                 (comment0 #f)
                                 (acc0 #f)
                                 (sync0 '()))
              (set! default value)
              (if keychain
                  (keychain-set-string!
                    keychain
                    key
                    value
                    (or label0 label)
                    (or comment0 comment)
                    acc0
                    sync0))))
          (values accessor setter))))
  )
)
