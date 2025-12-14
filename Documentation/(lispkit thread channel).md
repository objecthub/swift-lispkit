# LispKit Thread Channel

Library `(lispkit thread channel)` implements _channels_ for communicating, coordinating and synchronizing threads of execution. LispKit channels are based on the channel abstraction provided by the [Go programming language](https://go.dev).

LispKit channels are thread-safe FIFO buffers for synchronizing communication between multiple threads. The current implementation supports multiple simultaneous receives and sends. It allows channels to be either synchronous or asynchronous by providing buffering capabilities. Furthermore, the library supports timeouts via channel timers and channel tickers.

The main differences compared to channels in the Go programming language are:

  - Channels do not have any type information.
  - Sending to a channel that gets closed does not panic, it unblocks all senders immediately with the `fail` flag set to non-`#f`.
  - Closing an already closed channel does not result in an error.
  - There is support for choosing what channels to select on at runtime via `channel-select*`.


## Channels

**(channel? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a channel, otherwise `#f` is returned.

**(make-channel)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-channel _capacity_)**  

Returns a new channel with a buffer size of _capacity_. If  _capacity_ is 0, the channel is synchronous and all its operations will block until a remote client sends/receives messages. Channels with a buffer capacity > 0 are asynchronous, but block if the buffer is exhausted.

**(channel-send! _channel msg_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sends message _msg_ to _channel_. `channel-send!` blocks if the capacity of _channel_ is exhausted. `channel-send!` returns the fail flag of the send operation, i.e. `#f` is returned if the send operation succeeded.

**(channel-receive! _channel_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(channel-receive! _channel none_)**  

Receives a message from _channel_ and returns the message. If there is no message available, `channel-receive!` blocks. If the receive operation fails, _none_ is returned, if provided. The default for _none_ is `#f`.

**(channel-try-receive! _channel_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(channel-try-receive! _channel none_)**  

Receives a message from _channel_ and returns the message. If there is no message available, `channel-try-receive!` returns _none_, if provided. The default for _none_ is `#f`.

**(channel-select\* _channel clauses_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Procedure `channel-select*` allows selecting channels that are chosen programmatically. It takes input that looks like this:

```scheme
(channel-select*
  `((,chan1 meta1)         ; receive
    (,chan2 meta2 message) ; send
    (,chan3 meta3) ...))
```

`channel-select*` returns three values _msg_, _fail_, and _meta_, where _msg_ is the message that was sent over the channel, _fail_ is `#t` if the channel was closed and `#f` otherwise, and _meta_ is the datum supplied in the arguments.  

For example, if a message arrived on _chan3_ above, _meta_ would be `meta3` in that case. This allows one to see which channel a message came from, i.e. if you supply metadata that is the channel itself.

**(channel-select** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  
&nbsp;&nbsp;&nbsp;&nbsp;**\(\(chan -\> msg\) body ...\)**  
&nbsp;&nbsp;&nbsp;&nbsp;**\(\(chan -\> msg fail\) body ...\)**  
  &nbsp;&nbsp;&nbsp;&nbsp;**\(\(chan <- msg\) body ...\)**  
&nbsp;&nbsp;&nbsp;&nbsp;**\(\(chan <- msg fail\) body ...\)**  
&nbsp;&nbsp;&nbsp;&nbsp;**\(else body ...\)\)**  

This is a channel switch that will send or receive on at most one channel, picking whichever clause is able to complete soonest. If no clause is ready, `channel-select` will block until one does, unless `else` is specified which will execute its body instead of blocking. Multiple send and receive clauses can be specified interchangeably, but only one clause will trigger and get executed. Example:

```scheme
(channel-select
  ((chan1 -> msg fail)
     (if fail
         (print "chan1 closed!")
         (print "chan1 says " msg)))
  ((chan2 -> msg fail)
     (if fail
         (print "chan2 closed!")
         (print "chan2 says " msg))))
```

Receive clauses have the form `((chan -> msg [fail]) body ...)`. They execute _body_ with _msg_ bound to the message object and _fail_ bound to a boolean flag indicating failure. Receiving from a closed channel immediately completes with this _fail_ flag set to non-`#f`.

Send clauses have the form `((chan <- msg [fail]) body ...)`. They execute _body_ after _msg_ has been sent to a receiver, successfully buffered onto the channel, or if channel was closed. Sending to a closed channel immediately completes with the _fail_ flag set to `#f`.

A send or receive clause on a closed channel with no _fail_-flag binding specified will immediately return void without executing _body_. This can be combined with recursion like this:  

```scheme
;; loop forever until either chan1 or chan2 closes
(let loop ()
   (channel-select
     ((chan1 -> msg)
        (display* "chan1 says " msg) (loop))
     ((chan2 <- 123)
        (display* "chan2 got  " 123) (loop))))
```

Or like this:

```scheme
;; loop forever until chan1 closes. replacing chan2 is
;; important to avoid busy-wait!
(let loop ((chan2 chan2))
  (channel-select
    ((chan1 -> msg)
       (display* "chan1 says " msg)
       (loop chan2))
    ((chan2 -> msg fail)
       (if fail
           (begin
             (display* "chan2 closed, keep going")
             ;; create new forever-blocking channel
             (loop (make-channel 0)))
           (begin
             (display* "chan2 says " msg)
             (loop chan2))))))
```

`channel-select` returns the return value of the executed clause's body. To do a non-blocking receive, you can do the following:

```scheme
(channel-select
  ((chan1 -> msg fail) (if fail #!eof msg))
  (else 'eagain))
```  

**(channel-range _channel_ -\> _msg body ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

`channel-range` continuously waits for messages to arrive on _channel_. Once a message _msg_ is available, _body ..._ gets executed and `channel-range` waits again for the next message to arrive. `channel-range` does not terminate unless _channel_ is closed. The following statement is equivalent:

```scheme
(let ((chan channel))
  (let loop ()
    (channel-select
      ((chan -> msg fail)
        (unless fail (begin body ...)(loop))))))
```

**(channel-close _channel_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(channel-close _channel fail_)**  

Closes _channel_. This will unblock existing receivers and senders waiting for an operation on _channel_ with thir fail flag set to a non-`\#f` value. All future receivers and senders will also immdiately unblock in this way, so there is a risk to run into busy-loops.

The optional _fail_ flag of `channel-close` can be used to specify an alternative to the default `#t`. As this value is given to all receivers and senders of _channel_, the _fail_ flag can be used as a "broadcast" mechanism. _fail_ flag must not be set to `#f` though, as that would indicate a successful message transaction.

Closing an already closed channel will results in its fail flag being updated.  


## Timers

**(timer? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a channel timer as provided by this library. Otherwise `timer?` returns `#f`.

**(make-timer _next_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

_next_ is a thunk returning three values: _when-next_, _data_, and _fail_. _when-next_ is when to trigger the next time, expressed in seconds since January 1, 1970 TAI (e.g. computed via `(current-second)`), _data_ is the payload returned when the triggers (it's usually the time in seconds when it triggers), and _fail_ refers to a fail flag, which is usually `#f` for timers.

`next` will be called exaclty once on every timeout and once at "startup" and can thus mutate its own private state. `next` is called within a timer mutex lock and thus does not need to be synchronized.

**(timer _duration_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a timer channel that will "send" a single message after _duration_ seconds after its creation. The message is the `current-second` value at the time of the timeout, i.e. not when the message was received. Receiving more than once on an timer channel will block indefinitely or deadlock the second time.  

```scheme
(channel-select
  ((chan1 -> msg)
     (display* "chan1 says " msg))
  (((timer 1) -> when)
     (display* "chan1 took too long")))
```

You cannot send to or close a timer channel. Creating timers is a relatively cheap operation. Timers may be garbage-collected before the timer triggers. Creating a timer does not spawn a new thread.

**(ticker _duration_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a ticker channel that will "send" a message every _duration_ seconds. The message is the `current-second` value at the time of the tick, i.e. not when it was received.

**(ticker-stop! _ticker_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Stops a ticker channel, i.e. the channel will stop sending "tick" messages.

***

Large portions of this documentation:  
Copyright (c) 2017 Kristian Lein-Mathisen.  All rights reserved.  
License: BSD
