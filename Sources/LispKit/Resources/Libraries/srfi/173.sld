;;; SRFI 173
;;; Hooks
;;;
;;; This library describes a mechanism known as hooks. Hooks are a certain kind of
;;; extension point in a program that allows interleaving the execution of arbitrary
;;; code with the execution of the program without introducing any coupling between
;;; the two.
;;;
;;; Author of spec: Amirouche Boubekki
;;;
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

(define-library (srfi 173)

  (export make-hook
          hook?
          hook-add!
          hook-delete!
          hook-reset!
          hook->list
          hook-run)

  (import (lispkit base))

  (begin

    (define-values (new-hook hook? hook-ref make-hook-subtype) (make-type 'hook))

    (define (make-hook arity)
      (new-hook (cons arity (box '()))))

    (define (hook-add! hook proc)
      (let ((procs (cdr (hook-ref hook))))
        (set-box! procs (cons proc (unbox procs)))))

    (define (hook-delete! hook proc)
      (let ((procs (cdr (hook-ref hook))))
        (set-box! procs (remove (lambda (x) (eq? x proc)) (unbox procs)))))

    (define (hook-reset! hook)
      (set-box! (cdr (hook-ref hook)) '()))

    (define (hook->list hook)
      (unbox (cdr (hook-ref hook))))

    (define (hook-run hook . args)
      (if (not (= (length args) (car (hook-ref hook))))
          (error "arity of hook ($0) does not match number of arguments ($1)"
                 (car (hook-ref hook))
                 (length args)))
      (for-each (lambda (proc) (apply proc args))
                (hook->list hook)))
  )
)
