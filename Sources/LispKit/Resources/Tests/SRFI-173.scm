;;; SRFI 173 REGRESSION TEST SUITE
;;;
;;; This is the test suite for SRFI 173.
;;;
;;; Copyright © 2000 Amirouche Boubekki. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2019 Matthias Zenger. All rights reserved.

(import (lispkit base)
        (lispkit test)
        (srfi 173))

(test-begin "SRFI 173: Hooks")

;; test hook->list and make-hook
(test 0 (length (hook->list (make-hook 0))))

;; test hook-add!
(test 1 (let ((hook (make-hook 0))
              (counter 0))
          (hook-add! hook (lambda () (set! counter (+ counter 1))))
          (length (hook->list hook))))

;; test hook-delete!
(test 0 (let ((hook (make-hook 0))
              (counter 0))
          (let ((increment (lambda () (set! counter (+ counter 1)))))
            (hook-add! hook increment)
            (hook-delete! hook increment)
            (length (hook->list hook)))))

;; test hook-reset!
(test 0 (let ((hook (make-hook 0))
              (counter 0))
          (let ((increment (lambda () (set! counter (+ counter 1))))
                (decrement (lambda () (set! counter (- counter 1)))))
            (hook-add! hook increment)
            (hook-reset! hook)
            (length (hook->list hook)))))

;; test hook-run
(test '(1 . 1)
  (let ((hook (make-hook 0))
        (counter 0))
    (let ((increment (lambda () (set! counter (+ counter 1))))
          (decrement (lambda () (set! counter (- counter 1)))))
      (hook-add! hook increment)
      (hook-run hook)
      (let ((c counter))
        (hook-add! hook decrement)
        (hook-run hook)
        (cons c counter)))))

(test-end)
