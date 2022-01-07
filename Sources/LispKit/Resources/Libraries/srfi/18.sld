;;; SRFI 18
;;; Multithreading support
;;;
;;; This SRFI defines the following multithreading datatypes for Scheme;
;;;   - Thread
;;;   - Mutex
;;;   - Condition variable
;;;   - Time
;;; 
;;; It also defines a mechanism to handle exceptions and some multithreading
;;; exception datatypes.
;;; 
;;; Author of spec: Marc Feeley
;;; 
;;; Copyright © 2022 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
;;; file except in compliance with the License. You may obtain a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed
;;; under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
;;; CONDITIONS OF ANY KIND, either express or implied. See the License for the specific
;;; language governing permissions and limitations under the License.

(define-library (srfi 18)
  
  (export current-thread
          thread?
          make-thread
          thread-name
          thread-specific
          thread-specific-set!
          thread-start!
          thread-yield!
          thread-sleep!
          thread-terminate!
          thread-join!
          mutex?
          make-mutex
          mutex-name
          mutex-specific
          mutex-specific-set!
          mutex-state
          mutex-lock!
          mutex-unlock!
          condition-variable?
          make-condition-variable
          condition-variable-name
          condition-variable-specific
          condition-variable-specific-set!
          condition-variable-signal!
          condition-variable-broadcast!
          current-time
          time?
          time->seconds
          seconds->time
          current-exception-handler
          with-exception-handler
          raise
          join-timeout-exception?
          abandoned-mutex-exception?
          terminated-thread-exception?
          uncaught-exception?
          uncaught-exception-reason)
  
  (import (lispkit base)
          (rename (lispkit date-time)
            (date-time current-time)
            (date-time? time?)
            (date-time->seconds time->seconds)
            (seconds->date-time seconds->time))
          (rename (lispkit thread)
            (make-thread make-thread-internal)
            (make-mutex make-mutex-internal)
            (make-condition-variable make-condition-variable-internal)
            (thread-yield! thread-yield-internal!)
            (thread-sleep! thread-sleep-internal!)
            (thread-join! thread-join-internal!)
            (mutex-lock! mutex-lock-internal!)
            (mutex-unlock! mutex-unlock-internal!)))
  
  (begin
    
    (define (timeout->seconds timeout)
      (if (time? timeout) 
          (let ((res (fl- (time->seconds timeout) (current-seconds))))
            (if (fl< res 0.0) 0.0 res))
          timeout))
    
    (define-optionals (make-thread proc (name #f))
      (make-thread-internal proc name (box #f)))
    
    (define (thread-specific thread)
      (unbox (thread-tag thread)))
    
    (define (thread-specific-set! thread obj)
      (set-box! (thread-tag thread) obj))
    
    (define (thread-yield!)
      (thread-yield-internal!)
      (thread-sleep-internal! 0.01)) ; this is needed since LispKit's native yield
                                     ; does not guarantee a thread switch
    
    (define (thread-sleep! timeout)
      (thread-sleep-internal! (timeout->seconds timeout)))
    
    (define-optionals (thread-join! thread (timeout #f) (val #f))
      (thread-join-internal! thread (timeout->seconds timeout) val))
    
    (define-optionals (make-mutex (name #f))
      (make-mutex-internal name (box #f)))
    
    (define (mutex-specific thread)
      (unbox (mutex-tag thread)))
    
    (define (mutex-specific-set! thread obj)
      (set-box! (mutex-tag thread) obj))
    
    (define-optionals (mutex-lock! mutex (timeout #f) (thread #f))
      (mutex-lock-internal! mutex (timeout->seconds timeout) thread))
    
    (define (mutex-unlock! mutex (condvar #f) (timeout #f))
      (mutex-unlock-internal! mutex condvar (timeout->seconds timeout)))
    
    (define-optionals (make-condition-variable (name #f))
      (make-condition-variable-internal name (box #f)))
    
    (define (condition-variable-specific thread)
      (unbox (condition-variable-tag thread)))
    
    (define (condition-variable-specific-set! thread obj)
      (set-box! (condition-variable-tag thread) obj))
  )
)
