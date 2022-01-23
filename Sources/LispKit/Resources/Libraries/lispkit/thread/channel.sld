;;; LISPKIT THREAD CHANNEL
;;; 
;;; This library implements thread-safe (FIFO) channels for synchronizing
;;; communication between multiple threads. The design of the channel abstraction
;;; and the API is based on channels provided by the programming language Go.
;;; 
;;; The library was originally designed and implemented for Chicken Scheme by
;;; Kristian Lein-Mathisen. It features:
;;; 
;;;    - multiple receives
;;;    - multiple sends
;;;    - multiple receive/send simultaniously
;;;    - buffering
;;;    - timeouts (i.e. channel timers and channel tickers)
;;; 
;;; Library design and adaptations for LispKit:
;;; Copyright © 2022 Matthias Zenger. All rights reserved.
;;; 
;;; Original implementation and library design:
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

(define-library (lispkit thread channel)
  
  (export make-channel
          channel?
          channel-receive!
          channel-try-receive!
          channel-send!
          channel-close
          channel-select*
          channel-select
          channel-range
          make-timer
          timer?
          timer
          ticker
          ticker-stop!)
  
  (import (lispkit base)
          (lispkit queue)
          (srfi 18))
  
  (begin
    (define-syntax info
      (syntax-rules ()
        ((_ args ...) (void))))
    
    (define-syntax info-disabled
      (syntax-rules ()
        ((_ arg ...)
          (begin
            (display (current-thread))
            (display ": ")
            (display arg) ...
            (newline)))))
  )
  
  ;; SEMAPHORES
  
  (begin
    
    ;; This semaphore abstraction can be thought of as a return value
    ;; that can block. Each channel-select will create a semaphore and wait
    ;; for somebody to signal it (sometimes immediately (without waiting),
    ;; sometimes externally (having to wait)). It's important to understand
    ;; that it's ok for a semaphore to be registered in a channel (as
    ;; sender/receiver) even though it's already been signalled. this is ok
    ;; because already-signalled subscribers just be skipped (you cannot
    ;; re-deliver data to a (not semaphore-open?) semaphore).
    (define-record-type <semaphore>
      (new-semaphore mutex cv data meta fail)
      semaphore?
      (mutex semaphore-mutex)
      (cv    semaphore-cv)
      (data  semaphore-data semaphore-data-set!)
      (meta  semaphore-meta semaphore-meta-set!)
      (fail  semaphore-fail semaphore-fail-set!))
    
    (define (make-semaphore)
      (new-semaphore (make-mutex) (make-condition-variable) #f #f #t))
    
    (define (semaphore-open? sem)
      (eq? #f (semaphore-meta sem)))
    
    ;; returns #t on successful signal, #f if semaphore was already signalled.
    (define (semaphore-signal! sem data meta fail)
      (info "signalling " sem " meta: " meta " data: " data " fail: " fail)
      (mutex-lock! (semaphore-mutex sem))
      (cond ((semaphore-open? sem)  ;; available!
              (semaphore-data-set! sem data)
              (semaphore-meta-set! sem meta)
              (semaphore-fail-set! sem fail)
              (condition-variable-signal! (semaphore-cv sem))
              (mutex-unlock! (semaphore-mutex sem))
              #t)
            (else                   ;; already signalled
              (mutex-unlock! (semaphore-mutex sem))
              #f)))
  )
  
  ;; TIMERS
  
  (begin
    (define-record-type <timer>
      (new-timer mutex receivers when data fail next)
      timer?
      (mutex     timer-mutex)
      (receivers timer-receivers timer-receivers-set!)
      (when      timer-when timer-when-set!) ; when may be #f if never to trigger again
      (data      timer-data timer-data-set!)
      (fail      timer-fail timer-fail-set!)
      (next      timer-next))
    
    ;; Next must be a thunk which returns (values when-next data fail), where
    ;; `when-next` is when to trigger next in (current-second). `next` will
    ;; be called exaclty once on every timeout and once at "startup" and can thus
    ;; mutate its own private state. `next` is called within a timer mutex lock,
    ;; so it shouldn't ever error.
    (define (make-timer next)
      (let-values (((when-next data fail) (next)))
        (new-timer (make-mutex) (list->queue '()) when-next data fail next)))
    
    ;; It's important that when-next is set in lockstep as timer-next is called so
    ;; that timer-next get's called only once per "when" it returns. timer-next
    ;; should never be called if timer-when is #f.
    (define (timer-tick! timer)
      (let-values (((when-next data fail) ((timer-next timer))))
        (timer-when-set! timer when-next)  ; may be #f if never to timout again
        (timer-data-set! timer data)
        (timer-fail-set! timer fail)))
    
    (define (timer duration)
      (let* ((when (+ (current-second) duration)))
        (make-timer (lambda ()
                      (let ((tmp when))
                        (set! when #f)           ; never trigger again
                        (values tmp tmp #f)))))) ; when-next data fail
    
    (define (ticker duration)
      (let ((when (current-second)))
        (make-timer (lambda ()
                      (set! when (+ when duration))
                      (values when when #f)))))
    
    (define (ticker-stop! tmr)
      (timer-when-set! tmr (lambda () (values #f #f #f))))
  )
  
  ;; CHANNELS
  
  (begin
    (define-record-type <channel>
      (new-channel mutex cap buffer receivers senders closed)
      channel?
      (mutex     channel-mutex)
      (cap       channel-cap)
      (buffer    channel-buffer)
      (receivers channel-receivers)
      (senders   channel-send!ers)
      (closed    channel-closed channel-closed-set!))
    
    (define (make-channel . args)
      (let-optionals args ((cap 0))
        (assert (fixnum? cap))
        (new-channel (make-mutex)
                     cap               ; capacity (max buffer size)
                     (make-queue)      ; buffer
                     (make-queue)      ; senders
                     (make-queue)      ; receivers
                     #f)))             ; not closed
    
    (define (make-send-subscription sem data meta)
      (cons sem (cons data meta)))
    
    (define send-subscription-sem car)
    
    (define send-subscription-data cadr)
    
    (define send-subscription-meta cddr)
    
    (define make-recv-subscription cons)
    
    (define recv-subscription-sem car)
    
    (define recv-subscription-meta cdr)
    
    (define (%channel-free-capacity? chan)
      (< (queue-size (channel-buffer chan))
         (channel-cap chan)))
    
    ;; We want to send, so let's notify any receivers that are ready. if this
    ;; succeeds, we close %sem. otherwise we return #f and we'll need to use our
    ;; semaphore. %sem must already be locked and open.
    ;; Returns #t if registered as subscriber, #f otherwise.
    (define (channel-signal-receiver/subscribe chan %sem msg meta)
      ; Because meta is also used to tell if a semaphore has already been
      ; signalled (#f) or not (≠ #f).
      (if (eq? #f meta)
          (error "metadata cannot be #f (in channel-select* alist)"))
      (mutex-lock! (channel-mutex chan))
      (if (channel-closed chan)
          ; Trying to send to a closed channel! the golang channel api
          ; panics in this situation. but we've got the `fail` variable
          ; conveniently available at the results of channel-send!s (just
          ; like on channel-receive), so let's try this instead: sending
          ; to a closed channel will not panic, but instead immediately
          ; unblock and receive a zero-value with `fail` set to #t.
          (begin
            (semaphore-meta-set! %sem meta)                  ; closes semaphore
            (semaphore-fail-set! %sem (channel-closed chan)) ; fail-flag can't be #f here
            (mutex-unlock! (channel-mutex chan))
            #f)
          (let ((q (channel-receivers chan)))
            (let %loop ()
              (if (queue-empty? q)
                  (begin
                    ; No semaphores left, nobody is around to receive our
                    ; data :( try to fill the buffer then
                    (if (%channel-free-capacity? chan)
                        (begin (enqueue! (channel-buffer chan) msg)
                               (semaphore-meta-set! %sem meta)    ; closes semaphore
                               (semaphore-fail-set! %sem #f)
                               (mutex-unlock! (channel-mutex chan))
                               #f)
                        ; Buffer didn't help us either! we are running out of options now.
                        ; We'll need to allow some future receiver to notify us when they
                        ; need data :(
                        (begin (assert (semaphore-open? %sem))
                               (enqueue! (channel-send!ers chan)
                                         (make-send-subscription %sem msg meta))
                               (mutex-unlock! (channel-mutex chan))
                               #t)))
                  (let ((sub (dequeue! q)))
                    (if (semaphore-signal! (recv-subscription-sem sub) msg
                                           (recv-subscription-meta sub) #f)
                        ; Receiver was signalled, signal self
                        (begin (semaphore-meta-set! %sem meta) ; close!
                               (semaphore-fail-set! %sem #f)   ; delivered, sender-fail-flag says ok
                               (mutex-unlock! (channel-mutex chan))
                               #f)
                        ; Receiver was already signalled by somebody else, try next receiver
                        (%loop))))))))
    
    ;; We want to receive stuff, try to signal someone who's ready to send. %sem must be
    ;; locked and open.
    ;; Returns #t if semaphore was registered on channel.
    (define (channel-signal-sender/subscribe chan %sem meta)
      (if (eq? #f meta)
          (error "metadata cannot be #f (in channel-select* alist)"))
      (mutex-lock! (channel-mutex chan))
      (if (> (queue-size (channel-buffer chan)) 0)
          ;; Data already in buffer! pop buffer and put data in our semaphore for
          ;; ourselves (then signal any blocked senders).
          (begin (semaphore-meta-set! %sem meta) ;; close
                 (semaphore-data-set! %sem (dequeue! (channel-buffer chan)))
                 (semaphore-fail-set! %sem #f)
                 ; Here's a trick! since we just freed 1 buffer item, so we might have need
                 ; to unblocking a sender who's waiting for buffer capacity. we can do it
                 ; all in one go here on the sender's behalf, since we've got the channel
                 ; mutex already: signal a sender and put its data in the buffer.
                 (let ((q (channel-send!ers chan)))
                   (let %loop ()
                     (unless (queue-empty? q)   ; nobody to signal? no problem
                       (let ((sub (dequeue! q)))
                         (if (semaphore-signal! (send-subscription-sem sub) #f
                                                (send-subscription-meta sub) #f)
                             ; Sender was signalled/unblocked! add its data to the buffer
                             (begin (enqueue! (channel-buffer chan)
                                              (send-subscription-data sub))
                                    (mutex-unlock! (channel-mutex chan))
                                    #f)
                             ; Sender was already signalled externally, try next
                             (%loop))))))
                 (mutex-unlock! (channel-mutex chan)))
          (if (channel-closed chan)
              ;; Trying to receive from a closed channel...
              (begin (semaphore-meta-set! %sem meta)
                     (semaphore-fail-set! %sem (channel-closed chan)) ; fail-flag from channel-close
                     (mutex-unlock! (channel-mutex chan))
                     #f)
              (let ((q (channel-send!ers chan)))
                (let %loop ()
                  (if (queue-empty? q)
                      (begin
                        ; Nobody had data for us, but we can add ourselves here so when
                        ; they do, they can signal us.
                        (assert (semaphore-open? %sem))
                        (enqueue! (channel-receivers chan)
                                  (make-recv-subscription %sem meta))
                        (mutex-unlock! (channel-mutex chan))
                        #t)
                      (let ((sub (dequeue! q)))
                        ; Signalling a sender-semaphore. they don't care about data, they
                        ; just want to be unblocked.
                        (if (semaphore-signal! (send-subscription-sem sub) #f
                                               (send-subscription-meta sub) #f)
                            ; Receiver was signalled, fill in our semaphore
                            ; (which can return immediately)
                            (begin (semaphore-meta-set! %sem meta) ; close
                                   (semaphore-data-set! %sem (send-subscription-data sub))
                                   (semaphore-fail-set! %sem #f)
                                   (mutex-unlock! (channel-mutex chan))
                                   #f)
                            ; Sender was already signalled externally, try next.
                            (%loop)))))))))
    
    ;; We want to add a timeout to our semaphore. If the chan (aka timer) has already
    ;; timed out, we immediately alert %sem and tick the timer. otherwise, we register
    ;; %sem as a chan/timer subscriber.
    ;; Returns #t is %sem was registered on timer as receiver subscriber.
    (define (channel-signal-timer/subscribe chan %sem meta)
      ; Note that chan here is really a timer.
      (if (eq? #f meta) (error "metadata cannot be #f (in channel-select* alist)"))
      (mutex-lock! (timer-mutex chan))
      (if (timer-when chan)
          (begin
            (if (<= (timer-when chan) (current-second))
                ; Timer already expired
                (begin
                  (semaphore-meta-set! %sem meta) ;; <-- also closes %sem
                  (semaphore-data-set! %sem (timer-data chan))
                  (semaphore-fail-set! %sem (timer-fail chan))
                  (timer-tick! chan)
                  (mutex-unlock! (timer-mutex chan))
                  #f)
                (begin
                  ; Timer in the future: register us
                  (enqueue! (timer-receivers chan)
                            (make-recv-subscription %sem meta))
                  (mutex-unlock! (timer-mutex chan))
                  #t)))
          ; The timer is indicating it will never trigger again. Assuming it's never useful
          ; to forever be waiting for it, we indicate it has closed.
          (begin (semaphore-meta-set! %sem meta) ; <-- also closes %sem
                 (semaphore-data-set! %sem (timer-data chan))
                 (semaphore-fail-set! %sem (or (timer-fail chan) #t))
                 (mutex-unlock! (timer-mutex chan))
                 #f)))
    
    ;; If timer has expired, signal a single receiver that `timer` has triggered and then
    ;; tick the timer for its next timeout value.
    ;; 
    ;; Whichever thread we're in, unless the timer has expired, transfer data from the
    ;; timer to the receiver's semaphore. many threads might be waiting for the same
    ;; timer, but the first to timer's mutex will do the work on behalf of everybody - the
    ;; rest will do nothing.
    (define (timer-signal timer)
      (mutex-lock! (timer-mutex timer))
      (info "signalling timer " timer)
      (if (timer-when timer)
          (if (<= (timer-when timer) (current-second))
              (let ((q (timer-receivers timer)))
                (let loop ()
                  (if (queue-empty? q)
                      (values)
                      (let ((sub (dequeue! q)))
                        (info "trying to signal " sub)
                        (if (semaphore-signal! (recv-subscription-sem sub)
                                               (timer-data timer)
                                               (recv-subscription-meta sub)
                                               (timer-fail timer))
                            ; Receiver was signalled ok, tick timer.
                            (timer-tick! timer)
                            ; Semaphore was already signalled, can't deliver value. try next
                            ; subscriber.
                            (loop))))))
              (info timer " was postponed"))
          ; Somebody else grabbed our timer trigger from us.
          (info timer " is no longer with us"))
      (mutex-unlock! (timer-mutex timer)))
    
    (define (%remove-queue-item q semaphore)
      (let loop ((n (queue-size q)))
        (when (> n 0)
          (let ((sub (dequeue! q)))
            (unless (eq? semaphore (car sub))
              (enqueue! q sub)))
          (loop (- n 1)))))
    
    ;; Run through the channels' semaphores (queue) and remove any instances of `semaphore`.
    (define (channel-unsubscribe-senders chan semaphore)
      (mutex-lock! (channel-mutex chan))
      (%remove-queue-item (channel-send!ers chan) semaphore)
      (mutex-unlock! (channel-mutex chan)))
    
    (define (channel-unsubscribe-receivers chan semaphore)
      (mutex-lock! (channel-mutex chan))
      (info "unsubscribing " chan)
      (%remove-queue-item (channel-receivers chan) semaphore)
      (mutex-unlock! (channel-mutex chan)))
    
    (define (channel-unsubscribe-timers chan semaphore)
      (mutex-lock! (timer-mutex chan))
      (%remove-queue-item (timer-receivers chan) semaphore)
      (mutex-unlock! (timer-mutex chan)))
    
    ;; The heart of it all! takes input that looks like this:
    ;; 
    ;; ```
    ;; (channel-select `((,chan1 meta1)
    ;;                   (,chan2 meta2 message)
    ;;                   (,chan3 meta3) ...))
    ;; ```
    ;; 
    ;; channel-select* returns: (msg fail meta)
    ;;
    ;; Where msg is the message that was send over the channel, fail is (not #f) if
    ;; channel was closed and #f otherwise, meta is the datum supplied in the arguments.
    ;; If a message arrived on chan3 above, for example, meta would be meta3 in that case.
    ;; This allows you to see which channel a message came from (ie if you supply meta
    ;; data that's is the channel itself)
    (define (channel-select* chans)
      (let ((semaphore (make-semaphore))
            ; Sorting trick! this lets us load-balance in case there are multiple channels
            ; with data always available. there's probably much faster ways to do this, though...
            (chans (map cdr
                        (sort (lambda (a b) (< (car a) (car b)))
                              (map (lambda (spec)
                                     (cons (/ (random 256) 256.0) spec))
                                   chans)))))
        ; Keep our semaphore locked while we check channels for data ready, so that we
        ; can't get signalled externally while we do this.
        (mutex-lock! (semaphore-mutex semaphore))
        (let loop ((chans chans)
                   (sendsub '())   ; list of channels we're subscribed on send
                   (recvsub '())   ; list of channels we're subscribed on recv
                   (timesub '())   ; list of timer we're subscribed on recv/trigger
                   (else-thunk #f))
          (if (and (semaphore-open? semaphore)
                   (pair? chans))
              (let ((chanspec  (car chans)))
                (if (and (pair? chanspec) (pair? (cdr chanspec)))
                    (let ((chan (car chanspec))
                          (meta (cadr chanspec)))
                      (cond ((and (channel? chan) (pair? (cddr chanspec))) ; want to send to chan
                              (loop (cdr chans)
                                    (if (channel-signal-receiver/subscribe
                                          chan semaphore (caddr chanspec) meta)
                                        (cons chan sendsub)
                                        sendsub)
                                    recvsub
                                    timesub
                                    else-thunk))
                            ((channel? chan) ; want to recv on chan
                              (loop (cdr chans)
                                    sendsub
                                    (if (channel-signal-sender/subscribe   chan semaphore meta)
                                        (cons chan recvsub)
                                        recvsub)
                                    timesub
                                    else-thunk))
                            ((timer? chan) ; want to "recv" on timeout
                              (loop (cdr chans)
                                    sendsub
                                    recvsub
                                    (if (channel-signal-timer/subscribe    chan semaphore meta)
                                        (cons chan timesub)
                                        timesub)
                                    else-thunk))
                            ((eq? chan 'else) ; want to execute body if nobody available
                              (loop (cdr chans)
                                    sendsub
                                    recvsub
                                    timesub
                                    meta))))))
              (begin
                (let %retry () ; lock semaphore mutex before retrying!
                  (if (semaphore-open? semaphore)
                      (if else-thunk
                          ; No data immediately available, but we have an else clause
                          (begin
                            (semaphore-meta-set! semaphore else-thunk)
                            (semaphore-data-set! semaphore #f)
                            (semaphore-fail-set! semaphore #f)
                            (mutex-unlock! (semaphore-mutex semaphore)))
                          ; No data immediately available on any of the channels, so we need
                          ; to wait for somebody else to signal us.
                          (if (pair? timesub)
                              ; We need to resort timesub here in case the previous timeout
                              ; caused timer-when modifications. Obs: cheeky timer-when peek
                              ; without mutex! since we're mutex-free, timers may trigger at
                              ; any time in here. But as long as we sort *after* we pick out
                              ; timer-when, we're sure to not all of a sudden have our timeout
                              ; become #f and that's the only really critical thing.
                              (let* ((timers* (sort (lambda (a b) ; sort #f's last
                                                      (let ((a (car a))
                                                            (b (car b)))
                                                        (if a
                                                            (if b (< a b) #t)
                                                            #f)))
                                                    (map (lambda (timer)
                                                           (cons (timer-when timer) timer))
                                                         timesub)))
                                     (timer (cdr (car timers*)))
                                     (timeout (let ((when (car (car timers*))))
                                                (and when (max 0 (- when (current-second)))))))
                                (info "wait for data with timer " timer " and timeout " timeout)
                                (if (mutex-unlock! (semaphore-mutex semaphore)
                                                   (semaphore-cv semaphore)
                                                   timeout)
                                    ; No timeout, semaphore might have been signalled, data
                                    ; might be in semaphore.
                                    (unless (semaphore-meta semaphore)
                                      (mutex-lock! (semaphore-mutex semaphore))
                                      (%retry))
                                    ; Timeout! if we're lucky, our semaphore won't be open after
                                    ; timer-signal.
                                    (begin
                                      (timer-signal timer)
                                      ; At this point, we know there was a timeout on timer but
                                      ; we don't know who received its signal.
                                      (mutex-lock! (semaphore-mutex semaphore))
                                      (%retry))))
                              ; We don't have any timers, wait on cv forever.
                              (begin (info "wait for data without timer")
                                     (mutex-unlock! (semaphore-mutex semaphore)
                                                    (semaphore-cv semaphore))
                                     ; mutex-unlock! might be stopped by a signal,
                                     ; retry if data is still unavailable
                                     (unless (semaphore-meta semaphore)
                                       (mutex-lock! (semaphore-mutex semaphore))
                                       (%retry))
                                     )))
                      ; Semaphore has data already!
                      (begin (info "no need to wait, data already there")
                             (mutex-unlock! (semaphore-mutex semaphore)))))
                ;; it's important that we remove our semaphore from
                ;; wherever it may be registered so we don't leak it. it
                ;; wouldn't get cleared out otherwise until someone else
                ;; tries to signal it - which may never happen
                (for-each (lambda (chan)
                            (channel-unsubscribe-senders chan semaphore))
                          sendsub)
                (for-each (lambda (chan)
                            (channel-unsubscribe-receivers chan semaphore))
                          recvsub)
                (for-each (lambda (chan)
                            (channel-unsubscribe-timers chan semaphore))
                          timesub)
                (assert (semaphore-meta semaphore))
                (values (semaphore-data semaphore)
                        (semaphore-fail semaphore)
                        (semaphore-meta semaphore)))))))
    
    (define (channel-send! chan msg)
      (assert (channel? chan))
      (let-values (((m fail meta) (channel-select* (list (list chan #t msg)))))
        fail))
    
    (define (channel-receive! chan . args)
      (let-optionals args ((none #f))
        (assert (or (channel? chan) (timer? chan)))
        (let-values (((msg fail meta)
                        (channel-select* (list (list chan (lambda (msg fail)
                                                            (if fail
                                                                (if (eq? none #t) fail none)
                                                                msg)))))))
          (meta msg fail))))
    
    (define (channel-try-receive! chan . args)
      (let-optionals args ((none #f))
        (assert (or (channel? chan) (timer? chan)))
        (let-values (((msg fail meta)
                       (channel-select* (list (list chan (lambda (msg fail)
                                                           (if fail
                                                               (if (eq? none #t) fail none)
                                                               msg)))
                                              (list 'else (lambda (msg fail) none))))))
          (meta msg fail))))
    
    ; Close channel. Unlike in go, this operation is idempotent (and hopefully that's a good idea).
    (define (channel-close chan . args)
      (define fail-flag (if (pair? args) (car args) #t))
      (if (eq? #f fail-flag)
          (error "fail-flag for closed channel cannot be #f"))
      (mutex-lock! (channel-mutex chan))
      (info "closing " chan " with "
            "receivers: " (queue->list (channel-receivers chan))
            "senders: "   (queue->list (channel-send!ers   chan)))
      (channel-closed-set! chan fail-flag)
      ; Signal *everybody* that we're closing (waking them all up, because now there are
      ; tons of #f-messages available to them)
      (let ((q (channel-receivers chan)))
        (let %loop ()
          (unless (queue-empty? q)
            (let ((sub (dequeue! q)))
              (semaphore-signal! (recv-subscription-sem  sub) #f ; no data
                                 (recv-subscription-meta sub) (channel-closed chan)) ; fail-flag
              (%loop)))))
      (let ((q (channel-send!ers chan)))
        (let %loop ()
          (unless (queue-empty? q)
            (let ((sub (dequeue! q)))
              (semaphore-signal! (send-subscription-sem  sub) #f ; no data
                                 (send-subscription-meta sub) (channel-closed chan)) ; fail-flag
              (%loop)))))
      (mutex-unlock! (channel-mutex chan)))
    
    ;; Turn channel-select form into `((,chan1 . ,proc1) (,chan2 . ,proc2) ...)
    (define-syntax channel-select-alist
      (syntax-rules (-> <- else)
        ; recv without fail flag
        ((_ ((channel -> varname) body ...) rest ...)
          `((,channel ,(lambda (varname fail) (unless fail (begin body ...))))
            ,@(channel-select-alist rest ...)))
        ; recv with fail flag
        ((_ ((channel -> varname fail) body ...) rest ...)
          `((,channel ,(lambda (varname fail) (begin body ...)))
            ,@(channel-select-alist rest ...)))
        ; send without fail flag
        ((_ ((channel <- msg) body ...) rest ...)
          `((,channel ,(lambda (varname fail) (unless fail (begin body ...))) ,msg)
            ,@(channel-select-alist rest ...)))
        ; send with fail flag
        ((_ ((channel <- msg fail) body ...) rest ...)
          `((,channel ,(lambda (varname fail) (begin body ...)) ,msg)
            ,@(channel-select-alist rest ...)))
        ; default (no rest ..., else must be last expression)
        ((_ (else body ...))
          `((else ,(lambda (varname fail) (begin body ...)))))
        ((_) '())))
    
    (define-syntax channel-select
      (syntax-rules ()
        ((_ form ...)
          (let-values (((msg fail meta) (channel-select* (channel-select-alist form ...))))
            (meta msg fail)))))
    
    (define-syntax channel-range
      (syntax-rules (->)
        ((_ c -> x expr ...)
           (let ((chan c))
             (let loop ()
               (channel-select
                 ((chan -> x fail)
                   (unless fail (begin expr ...)(loop)))))))))
  )
)
