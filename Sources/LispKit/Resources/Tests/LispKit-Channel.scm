;;; LISPKIT THREAD CHANNEL REGRESSION TEST SUITE
;;;
;;; This is the test suite for library `(lispkit thread channel)`. It is based on
;;; the tests for the original library implementation `(gochan)` by 
;;; Kristian Lein-Mathisen.
;;; 
;;; Copyright © 2017 Kristian Lein-Mathisen. All Rights Reserved.
;;; License: BSD
;;; 
;;; Redistribution and use in source and binary forms, with or without modification,
;;; are permitted provided that the following conditions are met:
;;; 
;;;   1. Redistributions of source code must retain the above copyright notice, this
;;;      list of conditions and the following disclaimer.
;;;   2. Redistributions in binary form must reproduce the above copyright notice,
;;;      this list of conditions and the following disclaimer in the documentation
;;;      and/or other materials provided with the distribution.
;;;   3. Neither the name of the copyright holder nor the names of its contributors
;;;      may be used to endorse or promote products derived from this software
;;;      without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
;;; OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(import (lispkit base)
        (lispkit test)
        (lispkit thread)
        (lispkit thread channel)
        (only (srfi 1) list-tabulate make-list count)
        (srfi 8))

;; todo:
;; - unbuffered synchronous
;; - unbuffered multiple receivers
;; - unbuffered multiple senders
;; - unbuffered send&recv on channel

(test-begin "LispKit Thread Channel")

(test-group "unbuffered 1 channel channel-select* meta"
  (define chan (make-channel))
  (go (channel-send! chan 'hello))
  (test '(hello #f meta!) (receive _ (channel-select* `((,chan meta!))) _)))

(test-group "unbuffered 2 channels, 1 channel ready"
  (define chan1 (make-channel))
  (define chan2 (make-channel))
  (go (channel-send! chan1 'one)
      (channel-send! chan1 'two))
  (test "pick from data-ready in order data first"
        'one
        (channel-select ((chan1 -> msg) msg)
                        ((chan2 -> msg) 'wrong!)))
  (test "pick from data-ready in order data last"
        'two
        (channel-select ((chan2 -> msg) 'wrong!)
                       ((chan1 -> msg) msg))))

(test-group "unbuffered 2 channels, 2 channels ready"
  (define chan1 (make-channel))
  (define chan2 (make-channel))
  (go (channel-send! chan1 1)
      (channel-send! chan1 2))
  (go (channel-send! chan2 3)
      (channel-send! chan2 4))
  (test "all messages received exactly once (order unknown by design)"
        '(1 2 3 4)
        (sort
         <
         (list (channel-select ((chan1 -> msg) msg)
                              ((chan2 -> msg) msg))
               (channel-select ((chan1 -> msg) msg)
                              ((chan2 -> msg) msg))
               (channel-select ((chan2 -> msg) msg)
                              ((chan1 -> msg) msg))
               (channel-select ((chan2 -> msg) msg)
                              ((chan1 -> msg) msg))))))

(test-group "unbuffered 1 channel fifo, primordial first"
  (define chan (make-channel))
  (go (thread-yield!)
      (thread-sleep! 0.5)
      (channel-send! chan 1)
      (channel-send! chan 2))
  (test "1 channel fifo priomaridal first" 1 (channel-receive! chan))
  (test "1 channel fifo priomaridal first" 2 (channel-receive! chan))
  (define chan (make-channel))
  (go (channel-send! chan 1)
      (channel-send! chan 2))
  (thread-yield!)
  (thread-sleep! 0.5)
  (test "1 channel fifo goroutine   first" 1 (channel-receive! chan))
  (test "1 channel fifo goroutine   first" 2 (channel-receive! chan)))

(test-group "timers"
  (define to1 (timer 0.1))
  (define to2 (timer 0.2))
  (define reply (make-channel))
  (go (channel-select ((to1 -> x) (channel-send! reply 'to1))
                      ((to2 -> x) (channel-send! reply 'to2))))
  (go (channel-select ((to1 -> x) (channel-send! reply 'to1))
                     ((to2 -> x) (channel-send! reply 'to2))))
  (define start (current-second))
  (test "timeout order 1" 'to1 (channel-receive! reply))
  (test "timeout order 2" 'to2 (channel-receive! reply))
  (define duration (- (current-second) start))
  (test "200ms to timeout took <220ms " #t (< duration 0.22))
  (test "to1 post-timeout closed" #t (channel-select ((to1 -> x closed?) closed?)))
  (test "to2 post-timeout closed" #t (channel-select ((to2 -> x closed?) closed?))))

(test-group "timers: each ticker gets consumed by only one recv"
  (define reply (make-channel 1024))
  (define tick  (ticker 0.01))
  (go (let loop () (channel-select ((tick -> _) (channel-select ((reply <- 1) (loop)))))))
  (go (let loop () (channel-select ((tick -> _) (channel-select ((reply <- 2) (loop)))))))
  (go (let loop () (channel-select ((tick -> _) (channel-select ((reply <- 3) (loop)))))))
  (go (let loop () (channel-select ((tick -> _) (channel-select ((reply <- 4) (loop)))))))
  (thread-sleep! .107)   ; just a little past the last tick
  (channel-close reply) ; allow goroutines to exit (this is an antipattern in golang,
                        ; hopefully ok here!)
  ; So, we've ticked every 100ms in 1 second. That should give us exactly 10 results,
  ; from a random selection of threads above.
  (define results
    (let loop ((res '()))
      (channel-select ((reply -> msg fail)
                      (if fail
                          (reverse res)
                          (loop (cons msg res)))))))
  (test "10ms messages for 105ms means 10 messages" 10 (length results)))

(test-group "closing channels"
  (define chan1 (make-channel 1))
  (channel-send! chan1 'test)
  (channel-close chan1)
  (test "closed, non-empty buffered channel gives us data" 'test (channel-receive! chan1))
  (test "closed, empty     buffered channel fails"         #f (channel-receive! chan1))
  (define chan (make-channel))
  (go (channel-receive! chan)
      (channel-receive! chan))
  (thread-yield!)
  (thread-sleep! 0.5)
  (test "sender fail-flag says no error 1" #f (channel-select ((chan <- 'hello fail) fail)))
  (test "sender fail-flag says no error 2" #f (channel-select ((chan <- 'hi    fail) fail)))
  (define r1 'untouched)
  (define r2 'untouched)
  (define r3 'untouched)
  (define r4 'untouched)
  (define r1-thread (go (channel-select ((chan -> msg)        (set! r1 'touched)))))
  (define r2-thread (go (channel-select ((chan -> msg fail)   (set! r2 fail)))))
  (define r3-thread (go (channel-select ((chan <- 'TEST)      (set! r3 'touched)))))
  (define r4-thread (go (channel-select ((chan <- 'TEST fail) (set! r4 fail)))))
  (channel-close chan 'my-fail-flag)
  (thread-sleep! 0.1) ;; r1/r2 threads should exit quickly
  (test-assert "blocked receiving thread was terminated" (thread-terminated? r1-thread))
  (test "blocked receiving thread with implicit fail flag" 'untouched (values r1))
  (test "blocked receiving thread with explicit fail flag" 'my-fail-flag (values r2))
  (test "blocked sending   thread with implicit fail flag" 'untouched (values r3))
  (test "blocked sending   thread with explicit fail flag" 'my-fail-flag (values r4))
  (test "receiving from closed channel sync"
        '(#f my-fail-flag)
        (channel-select ((chan -> msg fail) (list msg fail))))
  (test "sending to closed channel sync"
        'my-fail-flag
        (channel-select ((chan <- 123 fail) fail)))
  (test "channel-select ignored body of closed chan recv" (void)
        (channel-select ((chan -> msg) (error "chan closed, this should never run!"))))
  (test "channel-select ignores body of closed chan send"(void)
        (channel-select ((chan <- 123) (error "chan closed, this should never run!"))))
  (define chan (make-channel))
  (define go1 (go (channel-receive! chan)))
  (define go2 (go (channel-receive! chan)))
  (define go3 (go (channel-receive! chan)))
  (thread-sleep! 0.1)
  ; ensure goroutines are blocking on chan
  (test-assert "thread waiting 1" (thread-blocked? go1))
  (test-assert "thread waiting 2" (thread-blocked? go2))
  (test-assert "thread waiting 3" (thread-blocked? go3))
  (channel-close chan)
  (test "thread awakened by close 1" #f (thread-join! go1))
  (test "thread awakened by close 2" #f (thread-join! go2))
  (test "thread awakened by close 3" #f (thread-join! go3)))

(test-group "buffered channels"
  (define chan (make-channel 2))
  (define done #f)
  (define go1
    (go (channel-send! chan 1) (set! done 1)
        (channel-send! chan 2) (set! done 2)
        (channel-send! chan 3) (set! done 3)
        (channel-close chan)
        'exited))
  (thread-yield!)
  (thread-sleep! 0.5)
  (test-assert "thread blocked" (thread-blocked? go1))
  (test "thread filled buffer of two items" 2 done)
  (test "buffered data from chan item 1" 1 (channel-receive! chan))
  (test "thread awakened by previous receive (buffer available)"
        'exited (thread-join! go1))
  (test "thread " 3 done)
  (test "buffered leftovers from chan 2" 2 (channel-receive! chan))
  (test "buffered leftovers from chan 3" 3 (channel-receive! chan))
  (test "chan closed" #f (channel-receive! chan)))

(test-group "channel-select else clause"
  (test "else clause gets executed if nobody else ready"
        'my-else
        (channel-select (((make-channel) -> msg) (error "should never happen"))
                        (else 'my-else)))
  (define chan (make-channel 100))
  (list-tabulate 100 (lambda (i) (channel-send! chan i)))
  (test "else clause does not get executed if data ready"
        (make-list 100 'data)
        (list-tabulate 100
                       (lambda (i)
                         (channel-select (( chan -> when) 'data)
                                        (else (error "should never happen!!"))))))
  (test "else clause does not get executed if timeout ready"
        (make-list 100 'data)
        (list-tabulate 100
                       (lambda (i)
                         (channel-select (( (timer 0) -> when) 'data)
                                        (else (error "should never happen!!")))))))

(test-group "load-balancer"
  ; Create some chans with lots of data immediately available
  (define chan1 (make-channel 100))
  (define chan2 (make-channel 100))
  (list-tabulate 100 (lambda (x) (channel-send! chan1 x)))
  (list-tabulate 100 (lambda (x) (channel-send! chan2 x)))
  ; Receive from either
  (define origin
    (list-tabulate
     20 (lambda (x)
          (channel-select
           ((chan1 -> msg) 1)
           ((chan2 -> msg) 2)))))
  ; Check that we got data from both contestants
  (define num-chan1 (count (lambda (x) (eq? x 1)) origin))
  (define num-chan2 (count (lambda (x) (eq? x 2)) origin))
  (test "not just results from chan1" #t (< num-chan1 19))
  (test "not just results from chan2" #t (< num-chan2 19)))

(test-group "multiple channel-close calls"
  (define c (make-channel))
  (channel-close c 1) (test "first channel-close gets 1" 1 (channel-select ((c -> m f) f)))
  (channel-close c 2) (test "first channel-close gets 2" 2 (channel-select ((c -> m f) f)))
  (channel-close c 3) (test "first channel-close gets 3" 3 (channel-select ((c -> m f) f)))
  (channel-close c 4) (test "first channel-close gets 4" 4 (channel-select ((c -> m f) f))))

(test "abort running threads" 0 (abort-running-threads))

(test-end)
