;;; SRFI 18 REGRESSION TEST SUITE
;;;
;;; This is a test suite for SRFI 18.
;;;
;;; Copyright © Alex Shinn. All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the author may not be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;; LispKit Port:
;;;   Copyright © 2022 Matthias Zenger. All rights reserved.

(import (lispkit base)
        (lispkit test)
        (srfi 18))

(test-begin "SRFI 18: Multithreading support")

(test "unstarted thread" 'ok
  (let ((t (make-thread (lambda () (error "oops"))))) 'ok))

(test "ignored thread terminates" 'ok
  (let ((t (make-thread (lambda () 'oops)))) (thread-start! t) 'ok))

(test "ignored thread hangs" 'ok
  (let ((t (make-thread (lambda () (let lp () (lp))))))
    (thread-start! t)
    'ok))

(test "joined thread terminates" 'ok
  (let ((t (make-thread (lambda () 'oops))))
    (thread-start! t)
    (thread-join! t)
    'ok))

(test "joined thread hangs, timeout" 'timeout
  (let ((t (make-thread (lambda () (let lp () (lp))))))
    (thread-start! t)
    (thread-join! t 0.1 'timeout)))

(test "basic mutex" 'ok
  (let ((m (make-mutex)))
    (and (mutex? m) 'ok)))

(test "mutex unlock" 'ok
  (let ((m (make-mutex)))
    (and (mutex-unlock! m) 'ok)))

(test "mutex lock/unlock" 'ok
  (let ((m (make-mutex)))
    (and (mutex-lock! m)
         (mutex-unlock! m)
         'ok)))

(test "mutex lock/lock" 'timeout
  (let ((m (make-mutex)))
    (and (mutex-lock! m)
         (if (mutex-lock! m 0.1) 'fail 'timeout))))

(test "mutex lock timeout" 'timeout
  (let* ((m (make-mutex))
         (t (make-thread (lambda () (mutex-lock! m)))))
    (thread-start! t)
    (thread-yield!)
    (if (mutex-lock! m 0.1) 'fail 'timeout)))

(test "mutex lock/unlock/lock/lock" 'timeout
  (let* ((m (make-mutex))
         (t (make-thread (lambda () (mutex-unlock! m)))))
    (mutex-lock! m)
    (thread-start! t)
    (if (mutex-lock! m 0.1)
        (if (mutex-lock! m 0.1) 'fail-second 'timeout)
        'bad-timeout)))

(test "thread-join! end result" 5
  (let* ((th (make-thread (lambda () (+ 3 2)))))
    (thread-start! th)
    (thread-join! th)))

(test-error "thread-join! exception"
            (let* ((th (make-thread
                        (lambda ()
                          (parameterize ((current-error-port (open-output-string)))
                            (+ 3 "2"))))))
              (thread-start! th)
              (thread-join! th)))

(test-assert "make-condition-variable"
  (condition-variable? (make-condition-variable)))

(test "condition-variable signal" 'ok
  (let* ((mutex (make-mutex))
         (cndvar (make-condition-variable))
         (th (make-thread
              (lambda ()
                (if (mutex-unlock! mutex cndvar 0.1) 'ok 'timeout1)))))
    (thread-start! th)
    (thread-yield!)
    (condition-variable-signal! cndvar)
    (thread-join! th 0.1 'timeout2)))

(test "condition-variable broadcast" '(ok1 ok2)
  (let* ((mutex (make-mutex))
         (cndvar (make-condition-variable))
         (th1 (make-thread
               (lambda ()
                 (mutex-lock! mutex)
                 (if (mutex-unlock! mutex cndvar 1.0) 'ok1 'timeout1))))
         (th2 (make-thread
               (lambda ()
                 (mutex-lock! mutex)
                 (if (mutex-unlock! mutex cndvar 1.0) 'ok2 'timeout2)))))
    (thread-start! th1)
    (thread-start! th2)
    (thread-yield!)
    (mutex-lock! mutex)
    (condition-variable-broadcast! cndvar)
    (mutex-unlock! mutex)
    (list (thread-join! th1 0.1 'timeout3)
          (thread-join! th2 0.1 'timeout4))))

(test-end)
