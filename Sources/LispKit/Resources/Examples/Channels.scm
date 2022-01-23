;;; LispKit Channel Tutorial
;;; 
;;; LispKit supports muli-threading via library `(lispkit thread)`.
;;; The library provides access to threads and low-level synchronization
;;; abstractions such as mutexes and condition variables. Library
;;; `(lispkit thread channel)` implements synchronous and asynchronous
;;; channels facilitating communication between threads. The design is
;;; based on channels as provided by the Go programming language.
;;; 
;;; This file introduces LispKit channels. The material is based on
;;; content created by Mark McGranaghan and Eli Bendersky for the Go
;;; programming language. The original content ("Go by Example") can
;;; be found here: https://github.com/mmcgrana/gobyexample
;;; 
;;; This work is copyright by Mark McGranaghan and licensed under a
;;; Creative Commons Attribution 3.0 Unported License. See
;;; https://creativecommons.org/licenses/by/3.0/
;;; 
;;; Adaptation to LispKit
;;;   Copyright © 2022 Matthias Zenger. All rights reserved.

(import (lispkit base)
        (lispkit iterate)
        (lispkit thread)
        (lispkit thread channel)
        (lispkit date-time))

;;; CHANNELS

;; Channels can be seen as the pipes that connect concurrent threads. You can
;; send values into channels from one thread and receive those values on another
;; thread.
;;
;; The following snippet creates a new channel with `(make-channel)`. Unlike
;; channels in the Go programming language, LispKit channels are not typed.
;; A value is sent into a channel using the `channel-send!` procedure. Below,
;; we send "ping" to the messages channel from a new thread created via `go`.
;; The `channel-receive!` procedure receives a value from the channel. Below,
;; we’ll receive the "ping" message we sent above in the other thread and
;; print it out. By default, sends and receives block until both the sender
;; and receiver are ready. This property allowed us to wait at the end of our
;; program for the "ping" message to arrive without having to use any other
;; synchronization.

(display "---- CHANNELS\n")
(let ((messages (make-channel)))
  (go (channel-send! messages "ping"))
  (let ((msg (channel-receive! messages)))
    (display msg)
    (newline)))

;; Output:
;; ping

;;; CHANNEL BUFFERING

;; By default, channels are unbuffered, meaning that they will only accept
;; `channel-send!` if there is a corresponding `channel-receive!` ready to
;; receive the sent value. Buffered channels accept a limited number of values
;; without a corresponding receiver for those values.
;; 
;; In the code below, we make a channel buffering up to 2 values. Because this
;; channel is buffered, we can send values into the channel without a
;; corresponding concurrent receive. Later we can receive these two values as
;; usual asynchronously.

(display "---- CHANNEL BUFFERING\n")
(let ((messages (make-channel 2)))
  (channel-send! messages "buffered")
  (channel-send! messages "channel")
  (display (channel-receive! messages))
  (newline)
  (display (channel-receive! messages))
  (newline))

;; Output:
;; buffered
;; channel

;;; CHANNEL SYNCHRONIZATION

;; We can use channels to synchronize execution across threads. Here is an
;; example of using a blocking receive to wait for a thread to finish. The done
;; channel will be used to notify another thread that the thread's work is done.

(display "---- CHANNEL SYNCHRONIZATION\n")
(begin
  ; This is the procedure we execute in a separate thread.
  (define (worker done)
    (display "working...")
    (thread-sleep! 1)
    (display "done")
    (newline)
    ; Send value to notify we are done.
    (channel-send! done #t))
  (define done (make-channel 1))
  ; Start a new thread, passing the channel to notify on.
  (go (worker done))
  ; Block until we receive a notification from the worker on the channel.
  (channel-receive! done))

;; If you removed the `channel-receive!` call from this program, the program
;; would exit before the worker even started.
;; Output:
;; working...done

;;; CHANNEL SELECT

;; The `channel-select` form lets you wait on multiple channel operations.
;; Combining threads and channels with `channel-select` is a powerful feature
;; of library `(lispkit thread channel)`.
;; 
;; For our example below, we will select across two channels `c1` and `c2`.
;; Each channel will receive a value after some amount of time from two
;; separate threads, to simulate e.g. blocking RPC operations
;; executing in concurrent threads. We will use `channel-select` to await
;; both of these values simultaneously, printing each one as it arrives.

(display "---- CHANNEL SELECT\n")
(let ((c1 (make-channel))
      (c2 (make-channel)))
  (go (thread-sleep! 1)
      (channel-send! c1 "one"))
  (go (thread-sleep! 2)
      (channel-send! c2 "two"))
  (dotimes (i 2)
    (channel-select
      ((c1 -> msg1)
        (display* "received " msg1 "\n"))
      ((c2 -> msg2)
        (display* "received " msg2 "\n")))))

;; Note that the total execution time is only ~2 seconds since both the 1 and
;; 2 second concurrent `thread-sleep!` calls.
;; Output:
;; received one
;; received two

;;; TIMEOUTS

;; Timeouts are important for programs that connect to external resources
;; or that otherwise need to bound execution time. Implementing timeouts
;; via library `(lispkit thread channel)` is easy and elegant thanks to
;; channels and `channel-select`.
;; 
;; For our example, suppose we are executing an external call that returns
;; its result on a channel `c1` after 2 seconds. Note that the channel is
;; buffered, so the send in the thread is non-blocking. This is a common
;; pattern to prevent thread leaks in case the channel is never read.
;; 
;; The first `channel-select` statement below is implementing a timeout.
;; `(c1 -> res)` awaits the result and `(timer 1)` awaits a value to
;; be sent after the timeout of 1 second. Since select proceeds with the
;; first receive that is ready, we will take the timeout case if the
;; operation takes more than the allowed 1s.
;; 
;; In the second `channel-select` we allow a longer timeout of 3 seconds.
;; In this case, the receive from `c2` will succeed and we will print
;; the result.

(display "---- TIMEOUTS\n")
(let ((c1 (make-channel 1))
      (c2 (make-channel 1)))
  (go (thread-sleep! 2)
      (channel-send! c1 "result 1"))
  (channel-select
    ((c1 -> res)
      (display* res "\n"))
    (((timer 1) -> _)
      (display "timeout 1\n")))
  (go (thread-sleep! 2)
      (channel-send! c2 "result 2"))
  (channel-select
    ((c2 -> res)
      (display* res "\n"))
    (((timer 3) -> _)
      (display "timeout 2\n"))))

;; Output:
;; timeout 1
;; result 2

;;; NON-BLOCKING CHANNEL OPERATIONS

;; Basic sends and receives on channels are blocking. However, we can use
;; `channel-select` with a default clause to implement non-blocking sends,
;; receives, and even non-blocking multi-way selects.
;; 
;; The first `channel-select` statement below shows a non-blocking receive.
;; If a value is available on messages then `channel-select` will take the
;; `(messages -> msg)` clause with that value. If not, it will immediately
;; take the default clause.
;; 
;; The second `channel-select` statement explains how a non-blocking send
;; works. Here `msg` cannot be sent to the messages channel because the
;; channel has no buffer and there is no receiver. Therefore, the default
;; clause is selected.
;;
;; Finally, the third `channel-select` statement uses multiple read clauses
;; above the default clause to implement a multi-way, non-blocking select.
;; Here we attempt non-blocking receives on both `messages` and `signals`
;; channels.

(display "---- NON-BLOCKING CHANNEL OPERATIONS\n")
(let ((messages (make-channel))
      (signals (make-channel))
      (msg "hi"))
  (channel-select
    ((messages -> msg)
      (display* "received message " msg "\n"))
    (else
      (display "no message received\n")))
  (channel-select
    ((messages <- msg)
      (display* "sent message " msg "\n"))
    (else
      (display "no message sent\n")))
  (channel-select
    ((messages -> msg)
      (display* "received message " msg "\n"))
    ((signals -> sig)
      (display* "received signal " sig "\n"))
    (else
      (display "no activity\n"))))

;; Output:
;; no message received
;; no message sent
;; no activity

;;; CLOSING CHANNELS

;; Closing a channel indicates that no more values will be sent on it.
;; This can be useful to communicate completion to the channel’s receivers.
;; 
;; In the example code below, we will use a `jobs` channel to communicate
;; work to be done from the main thread to a worker thread started via `go`.
;; When we have no more jobs for the worker, we will close the `jobs`
;; channel.
;;
;; The worker thread repeatedly receives messages from `jobs` via
;; `(jobs -> j fail)`. In this special 2-value form of receive, the `fail`
;; value will be true if `jobs` has been closed and all values in the
;; channel have already been received. We use this to notify on channel
;; `done` when we have worked all our jobs.
;; 
;; The main thread sends 3 jobs to the worker over the `jobs` channel,
;; then closes it and uses the `done` channel to wait for the worker to
;; complete its job.

(display "---- CLOSING CHANNELS\n")
(let ((jobs (make-channel 5))
      (done (make-channel)))
  (go (let loop ()
        (channel-select
          ((jobs -> j fail)
            (cond (fail
                    (display "received all jobs\n")
                    (channel-send! done #t))
                  (else
                    (display* "received job " j "\n")
                    (loop)))))))
  (dotimes (j 3)
    (channel-send! jobs (+ j 1))
    (display* "sent job " (+ j 1) "\n"))
  (channel-close jobs)
  (display "sent all jobs\n")
  (channel-receive! done))

;; Output (the actual output might differ dependent on thread scheduling):
;; sent job 1
;; received job 1
;; sent job 2
;; received job 2
;; sent job 3
;; sent all jobs
;; received job 3
;; received all jobs

;;; TICKERS

;; Timers are for when you want to do something once in the future. Tickers
;; are for when you want to do something repeatedly at regular intervals.
;; Below is an example of a ticker that ticks periodically until we stop it.
;; 
;; Tickers use a similar mechanism to timers: a channel that is sent values.
;; Here we will use the `channel-select` on the channel to await the values
;; as they arrive every 0.5 seconds. Tickers can be stopped. Once a ticker
;; is stopped, it won’t receive any more values on its channel. We will
;; stop ours after 1600ms. When we run the program below, the ticker should
;; tick 3 times before we stop it.

(display "---- TICKERS\n")
(let ((tick (ticker 0.5))
      (done (make-channel)))
  (go (let loop ()
        (channel-select
          ((done -> _)
            (void))
          ((tick -> t)
            (display* "Tick at " (date-time->string (seconds->date-time t)) "\n")
            (loop)))))
  (thread-sleep! 1.6)
  (ticker-stop! tick)
  (channel-send! done #t)
  (display "Ticker stopped\n"))

;; Output:
;; Tick at 23/01/22, 22:05:11
;; Tick at 23/01/22, 22:05:11
;; Tick at 23/01/22, 22:05:12
;; Ticker stopped

;;; WORKER POOLS

;; In the example below, we will look at how to implement a worker pool
;; using threads and channels. 
;; 
;; Procedure `make-worker` spawns a new thread implementing a worker. We
;; will run several concurrent instances of it. These workers will receive
;; work on the `jobs` channel and send the corresponding results on channel
;; `results`. We will sleep a second per job to simulate an expensive task.
;; 
;; In order to use our pool of workers, we need to send them work and
;; collect their results. We make two channels for this: `jobs` and
;; `results`. Channel `results` is used to collect all the results of the
;; workers. This also ensures that the worker threads have finished.

(display "---- WORKER POOLS\n")
(begin
  (define (make-worker id jobs results)
    (go (channel-range jobs -> j
          (display* "worker " id " started job " j "\n")
          (thread-sleep! 1)
          (display* "worker " id " finished job " j "\n")
          (channel-send! results (* j 2)))))
  (let* ((num-jobs 5)
         (jobs (make-channel num-jobs))
         (results (make-channel num-jobs)))
    (dotimes (w 3)
      (make-worker (+ w 1) jobs results))
    (dotimes (j num-jobs)
      (channel-send! jobs (+ j 1)))
    (channel-close jobs)
    (dotimes (a num-jobs)
      (channel-receive! results))))

;; Output (this can look very different when executed, due to differences
;; in thread scheduling):
;; worker 1 started job 1
;; worker 2 started job 2
;; worker 3 started job 3
;; worker 1 finished job 1
;; worker 2 finished job 2
;; worker 3 finished job 3
;; worker 2 started job 4
;; worker 3 started job 5
;; worker 3 finished job 5
;; worker 2 finished job 4

;; We are using `channel-range` above to iterate through available elements
;; in channel `jobs` in procedure `make-worker`. An alternative approach is to
;; use `channel-select` directly:

(define (make-worker id jobs results)
  (go (let loop ()
        (channel-select
          ((jobs -> j fail)
            (unless fail
              (display* "worker " id " started job " j "\n")
              (thread-sleep! 1)
              (display* "worker " id " finished job " j "\n")
              (channel-send! results (* j 2))
              (loop)))))))

;;; RATE LIMITING

;; Rate limiting is an important mechanism for controlling resource
;; utilization and maintaining quality of service. It is possible to
;; support rate limiting elegantly via threads, channels and tickers.
;;
;; First we’ll look at basic rate limiting. Suppose we want to limit our
;; handling of incoming requests. We’ll serve these requests off a channel
;; of the same name. This `limiter` channel will receive a value every
;; 200 milliseconds. This is the regulator in our rate limiting scheme.
;; By blocking on a receive from the `limiter` channel before serving each
;; request, we limit ourselves to 1 request every 200 milliseconds.
;; 
;; We may want to allow short bursts of requests in our rate limiting
;; scheme while preserving the overall rate limit. We can accomplish this
;; by buffering our limiter channel. This `bursty-limiter` channel will
;; allow bursts of up to 3 events. First, we fill up the channel to represent
;; allowed bursting. Then, every 200 milliseconds we’ll try to add a new
;; value to `bursty-limiter`, up to its limit of 3. We then simulate 5 more
;; incoming requests. But only the first 3 of these will benefit from
;; the burst capability of `bursty-limiter`.

(display "---- RATE LIMITING\n")
(let ((start (current-second))
      (requests (make-channel 5)))
  (define (current-time)
    (exact (round (* (- (current-second) start) 1000.0))))
  (dotimes (i 5)
    (channel-send! requests (+ i 1)))
  (channel-close requests)
  (let ((bursty-limiter (make-channel 3))
        (bursty-requests (make-channel 5))
        (limiter (ticker 0.2)))
    (channel-range requests -> req
      (channel-receive! limiter)
      (display* "request " req " " (current-time) "ms\n"))
    (dotimes (i 3)
      (channel-send! bursty-limiter (current-second)))
    (go (channel-range (ticker 0.2) -> t
          (channel-send! bursty-limiter t)))
    (dotimes (i 5)
      (channel-send! bursty-requests (+ i 1)))
    (channel-close bursty-requests)
    (channel-range bursty-requests -> req
      (channel-receive! bursty-limiter)
      (display* "request " req " " (current-time) "ms\n"))))

;; Running our program we see the first batch of requests handled once
;; every ~200 milliseconds as desired. For the second batch of requests we
;; serve the first 3 immediately because of the burstable rate limiting,
;; then serve the remaining 2 with ~200ms delays each.
;; 
;; Output:
;; request 1 216ms
;; request 2 416ms
;; request 3 617ms
;; request 4 816ms
;; request 5 1017ms
;; request 1 1036ms
;; request 2 1039ms
;; request 3 1040ms
;; request 4 1236ms
;; request 5 1436ms
