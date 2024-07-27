;;; LISPKIT THREAD SHARED-QUEUE REGRESSION TEST SUITE
;;;
;;; This is the test suite for library `(lispkit thread shared-queue)`.
;;; It is based on the tests for the original implementation for `mtqueue`
;;; queues as implemened in Gauche by Shiro Kawai.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2024 Matthias Zenger. All rights reserved.
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

(import (lispkit base)
        (lispkit test)
        (lispkit thread)
        (lispkit thread shared-queue))

(test-begin "LispKit Thread Shared-Queue")

(test-group "basic shared-queue functionality"
  (define q (make-shared-queue))
  (test "shared-queue queue?" #f (shared-queue? (cons 'a 'b)))
  (test "shared-queue queue?" #f (shared-queue? 3))
  (test "shared-queue queue?" #f (shared-queue? '()))
  (test "shared-queue queue?" #t (shared-queue? q))
  (test "shared-queue enqueue!" #t (begin (shared-queue-enqueue! q 'a) (shared-queue? q)))
  (test "shared-queue enqueue!" #t (begin (shared-queue-enqueue! q 'b) (shared-queue? q)))
  (test "shared-queue enqueue!" #t (begin (shared-queue-enqueue! q 'c) (shared-queue? q)))
  (test "shared-queue queue-front" 'a (shared-queue-front q))
  (test "shared-queue queue-rear" 'c (shared-queue-rear q))
  (test "shared-queue enqueue!" '(a f)
         (begin
           (shared-queue-enqueue! q 'd 'e 'f)
           (list (shared-queue-front q) (shared-queue-rear q))))
  (test "shared-queue queue-length" 6 (shared-queue-length q))
  (test "shared-queue dequeue!" 'a (shared-queue-dequeue! q))
  (test "shared-queue dequeue!" 'b (shared-queue-dequeue! q))
  (test "shared-queue queue-empty?" #f (shared-queue-empty? q))
  (test "shared-queue dequeue!" 'c (shared-queue-dequeue! q))
  (test "shared-queue queue-length" 3 (shared-queue-length q))
  (test "shared-queue dequeue!" 'd (shared-queue-dequeue! q))
  (test "shared-queue dequeue!" 'e (shared-queue-dequeue! q))
  (test "shared-queue dequeue!" 'f (shared-queue-dequeue! q))
  (test "shared-queue queue-empty?" #t (shared-queue-empty? q))
  (test-error "shared-queue dequeue! (error)" (shared-queue-dequeue! q))
  (test "shared-queue dequeue! (fallback)" "empty!" (shared-queue-dequeue! q "empty!"))
  (test-error "shared-queue queue-front (error)" (shared-queue-front q))
  (test "shared-queue queue-front (fallback)" "foo" (shared-queue-front q "foo"))
  (test-error "shared-queue queue-rear (error)" (shared-queue-rear q))
  (test "shared-queue queue-rear (fallback)" "foo" (shared-queue-rear q "foo"))
  (test "shared-queue queue-push!" '(c a)
         (begin
           (shared-queue-push! q 'a) (shared-queue-push! q 'b) (shared-queue-push! q 'c)
           (list (shared-queue-front q) (shared-queue-rear q))))
  (test "shared-queue queue-push!" '(f a)
         (begin
           (shared-queue-push! q 'd 'e 'f)
           (list (shared-queue-front q) (shared-queue-rear q))))
  (test "shared-queue queue-pop!" 'f (shared-queue-pop! q))
  (test "shared-queue queue-pop!" 'e (shared-queue-pop! q))
  (test "shared-queue queue-empty?" #f (shared-queue-empty? q))
  (test "shared-queue queue-pop!" 'd (shared-queue-pop! q))
  (test "shared-queue queue-pop!" 'c (shared-queue-pop! q))
  (test "shared-queue queue-pop!" 'b (shared-queue-pop! q))
  (test "shared-queue queue-pop!" 'a (shared-queue-pop! q))
  (test "shared-queue queue-empty?" #t (shared-queue-empty? q))
  (test "shared-queue dequeue-all!" '(a b c d e)
         (begin (shared-queue-enqueue! q 'a 'b 'c 'd 'e) (shared-queue-dequeue-all! q)))
  (test "shared-queue dequeue-all!" '() (shared-queue-dequeue-all! q))
  (test "shared-queue dequeue-all!" #t  (shared-queue-empty? q)))

(test-group "shared-queue with maximum length"
  (define q (make-shared-queue 3))
  (test "mtqueue maxlen" 'c
         (begin (shared-queue-enqueue! q 'a)
                (shared-queue-enqueue! q 'b)
                (shared-queue-enqueue! q 'c)
                (shared-queue-rear q)))
  (test-error "mtqueue maxlen (shared-queue-enqueue! overflow)" (shared-queue-enqueue! q 'd))
  (test "mtqueue maxlen (enqueue! unchanged after overflow)" '(a b c) (shared-queue->list q))
  (test "mtqueue room" 0 (shared-queue-room q))
  (test-error "mtqueue maxlen (enqueue! multiarg overflow)"
         (begin (shared-queue-dequeue! q)
                (shared-queue-enqueue! q 'd 'e 'f)))
  (test "mtqueue maxlen (enqueue! atomicity)" '(b c) (shared-queue->list q))
  (test "mtqueue room" 1 (shared-queue-room q))
  (test-error "mtqueue maxlen (queue-push! overflow)"
         (begin (shared-queue-push! q 'a)
                (shared-queue-push! q 'z)))
  (test "mtqueue maxlen (shared-queue-push! postcheck)" '(a b c) (shared-queue->list q))
  (test-error "mtqueue maxlen (queue-push! multiarg overflow)"
              (begin (shared-queue-dequeue! q)
                     (shared-queue-push! q 'd 'e 'f)))
  (test "mtqueue maxlen (queue-push! atomicity)" '(b c) (shared-queue->list q)))

(test-group "shared-queue with asynchronous operations"
  (define q (make-shared-queue 2))
  (define reached-end #f)
  (define sender (make-thread (thunk
                                (shared-queue-enqueue/wait! q 'a)
                                (shared-queue-enqueue/wait! q 'b)
                                (shared-queue-enqueue/wait! q 'c)
                                (shared-queue-enqueue/wait! q 'd)
                                (shared-queue-enqueue/wait! q 'e)
                                (shared-queue-enqueue/wait! q 'f)
                                (shared-queue-enqueue/wait! q 'g #f #f #t)
                                (set! reached-end #t))))
  (thread-start! sender)
  (test "asynchronously received a" 'a (shared-queue-dequeue/wait! q))
  (test "asynchronously received b" 'b (shared-queue-dequeue/wait! q))
  (test "asynchronously received c" 'c (shared-queue-dequeue/wait! q))
  (test "asynchronously received d" 'd (shared-queue-dequeue/wait! q))
  (test "asynchronously received e" 'e (shared-queue-dequeue/wait! q))
  (test "asynchronously received f" 'f (shared-queue-dequeue/wait! q))
  (test "asynchronously received g" 'g (shared-queue-dequeue/wait! q))
  (test "reached the end" #t reached-end)
  (test "dequeue timeout irrelevant for closed queues" 'h (shared-queue-dequeue/wait! q 1000 'h))
  (test "queue closed" #t (shared-queue-closed? q))
  (test-error "can't insert into closed queues" (shared-queue-enqueue/wait! q 'i)))

(test-end)
