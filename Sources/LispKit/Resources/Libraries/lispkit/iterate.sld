;;; LISPKIT ITERATE
;;;
;;; Implementation of a number of special forms for iterations.
;;;
;;; Author: Matthias Zenger
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

(define-library (lispkit iterate)

  (export dotimes
          dolist
          for
          while
          loop
          exit-with)

  (import (lispkit base))

  (begin

    (define-syntax dotimes
      (syntax-rules ()
        ((_ (var count result) body ...)
           (do ((maxvar count)
                (var 0 (fx1+ var)))
               ((fx>= var maxvar) result)
             body ...))
        ((_ (var count) body ...)
           (do ((maxvar count)
                (var 0 (fx1+ var)))
               ((fx>= var maxvar))
             body ...))
        ((_ count body ...)
           (do ((maxvar count)
                (var 0 (fx1+ var)))
               ((fx>= var maxvar))
             body ...))))

    (define-syntax dolist
      (syntax-rules ()
        ((_ (var lst result) body ...)
           (let ((var '())
                 (ys lst))
             (if (null? ys)
                 result
                 (do ((xs ys (cdr xs)))
                     ((null? xs) result)
                   (set! var (car xs))
                   body ...))))
        ((_ (var lst) body ...)
           (let ((var '())
                 (ys lst))
             (if (null? ys)
                 (void)
                 (do ((xs ys (cdr xs)))
                     ((null? xs))
                   (set! var (car xs))
                   body ...))))))
    
    (define-syntax for
      (syntax-rules (in from to step where)
        ((_ element in lst where condition body ...)
           (dolist (element lst) (if condition (begin body ...))))
        ((_ element in lst body ...)
           (dolist (element lst) body ...))
        ((_ element from (x ...) body ...)
           (dolist (element (list x ...)) body ...))
        ((_ var from lo to hi step s body ...)
           (do ((var lo (fx+ var s))
                (maxvar hi))
               ((fx> var maxvar))
             body ...))
        ((_ var from lo to hi body ...)
           (do ((var lo (fx1+ var))
                (maxvar hi))
               ((fx> var maxvar))
             body ...))))

    (define-syntax while
      (syntax-rules (unless)
        ((_ condition unless break body ...)
           (call-with-current-continuation
             (lambda (exit)
               (do ((break (thunk (exit (void))))) ((not condition)) body ...))))
        ((_ condition body ...)
           (do () ((not condition)) body ...))))

    (define-syntax loop
      (syntax-rules ()
        ((_ break body ...)
           (call-with-current-continuation
             (lambda (break)
               (do () (#f) body ...))))))

    (define-syntax exit-with
      (syntax-rules (from)
        ((_ break from body ...)
           (call-with-current-continuation
             (lambda (break) body ...)))
        ((_ break body ...)
          (call-with-current-continuation
            (lambda (break) body ...)))))
  )
)
