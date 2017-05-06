;;; DynamicWind.scm
;;; Regression test data
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2016 ObjectHub. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;      http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(
  "Simple dynamic-wind interactions"
  (connect talk1 disconnect connect talk2 disconnect)
  (let ((path ’()) (c #f))
      (let ((add (lambda (s) (set! path (cons s path)))))
        (dynamic-wind
          (lambda () (add ’connect))
          (lambda ()
            (add (call-with-current-continuation
                   (lambda (c0) (set! c c0) ’talk1))))
          (lambda () (add ’disconnect)))
        (if (< (length path) 4)
            (c ’talk2)
            (reverse path))))
)

(
  "Complex dynamic-wind interactions"
  (8 exit outer end skipped after before post two one pre after before inner begin enter)
  (define cont 0)
  (define logged '())
  (define (log x) (set! logged (cons x logged)))
  (define (foo n)
    (dynamic-wind
      (lambda () (log 'enter))
      (lambda ()
        (log 'begin)
        (dynamic-wind
          (lambda () (log 'inner))
          (lambda ()
            (let ((res
                (dynamic-wind
                  (lambda () (log 'before))
                  (lambda () (+ 1 (call-with-current-continuation (lambda (f) (set! cont f) 2))))
                  (lambda () (log 'after)))))
              (if (eqv? res 3)
                (dynamic-wind
                  (lambda () (log 'pre))
                  (lambda ()
                    (dynamic-wind
                      (lambda () (log 'one))
                      (lambda () (cont n))
                      (lambda () (log 'two))))
                  (lambda () (log 'post)))
                (log 'skipped))
              (log 'end)
              res))
          (lambda () (log 'outer))))
      (lambda () (log 'exit))))
  (cons (foo 7) logged)
)
