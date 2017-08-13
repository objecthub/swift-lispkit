;;; SRFI 17
;;; Generalized set!
;;;
;;; This is a proposal to allow procedure calls that evaluate to the "value of a location"
;;; to be used to set the value of the location, when used as the first operand of set!.
;;; For example: (set! (vector-ref x 0) 17)
;;; becomes equivalent to: (vector-set! x 0 17)
;;;
;;; Many programming languages have the concept of an lvalue. that is an "expression" that
;;; "evaluates" to a location, and which can appear on the left-hand-side of an assignment.
;;; Common Lisp has a related concept of "generalized variables" which can be used in `setf`
;;; and some other special forms. However, the Common Lisp concept is based on the idea of
;;; compile-time recognition of special "location-producing" functions; this does not seem
;;; to be in the "spirit of Scheme".
;;;
;;; This SRFI implements an extension of set! so that it provides similar functionality as
;;; Common Lisp's `setf`, except that the updater is associated with a procedure value,
;;; rather than a name.
;;;
;;; Author of spec: Per Bothner
;;;
;;; Copyright © 2017 Matthias Zenger. All rights reserved.
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

(define-library (srfi 17)
  (export set!
          setter
          getter-with-setter)
  (import (rename (scheme base) (set! %set!)))

  (begin
    (define-syntax set!
      (syntax-rules ()
        ((_ (getter arg ...) val) ((setter getter) arg ... val))
        ((_ var val)              (%set! var val))))
    
    (define setter
      (let ((setters (list (cons vector-ref vector-set!)
                           (cons string-ref string-set!)
                           (cons bytevector-u8-ref bytevector-u8-set!)
                           (cons hashtable-ref hashtable-set!)
                           (cons unbox set-box!)
                           (cons mcar set-mcar!)
                           (cons mcdr set-mcdr!))))
        (letrec ((setter      (lambda (proc)
                                (let ((probe (assv proc setters)))
                                  (if probe
                                      (cdr probe)
                                      (error "missing setter for " proc)))))
                 (set-setter! (lambda (proc setter)
                                (set! setters (cons (cons proc setter) setters))
                                (void))))
          (set-setter! setter set-setter!)
          setter)))

    (define (getter-with-setter get set)
      (let ((proc (lambda args (apply get args))))
        (set! (setter proc) set)
        proc))
  )
)
