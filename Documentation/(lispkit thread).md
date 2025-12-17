# LispKit Thread

Library `(lispkit thread)` provides programming abstractions facilitating multi-threaded programming. LispKit's thread system offers mechanisms for creating new threads of execution and for synchronizing them. The abstractions provided by this library only offer low-level support for multi-threading and access control. Other libraries such as `(lispkit thread channel)` provide higher-level abstractions built on top of `(lispkit thread)`.

Library `(lispkit thread)` defines the following data types:

  - Threads (a virtual processor which shares object space with all other threads)
  - Mutexes (a mutual exclusion device, also known as a lock and binary semaphore)
  - Condition variables (a set of blocked threads)

Some exception datatypes related to multi-threading are also specified, and a general mechanism for handling such exceptions is provided.

The design of this library as well as most of this documentation originates from SRFI 18 by Marc Feeley.

## Threads

A thread in LispKit encapsulates a _thunk_ which it eventually executes, a _name_ identifying the thread, a _tag_ for storing associated (thread-local) data, a list of mutexes it owns, as well as an end-result and end-exception field for eventually capturing the result of the executed thread. A thread is in exactly one of the following states: new, runnable, blocked, and terminated. Each thread comes with its own stack whose maximum size can be individually configured.

### Thread states

A "running" thread is a thread that is currently executing. There can be more than one running thread on a multiprocessor machine. A "runnable" thread is a thread that is ready to execute or running. A thread is "blocked" if it is waiting for a mutex to become unlocked, the end of a "sleep" period, etc. A "new" thread is a thread that has not yet become runnable. A new thread becomes runnable when it is started explicitly. A "terminated" thread is a thread that can no longer become runnable. Deadlocked threads are not considered terminated. The only valid transitions between the thread states are from new to runnable, between runnable and blocked, and from any state to terminated:

```
                         unblock  
       start            <-------  
  NEW -------> RUNNABLE -------> BLOCKED  
    \             |      block   /  
     \            v             /  
      +-----> TERMINATED <-----+  
```

The API of library `(lispkit thread)` provides procedures for triggering thread state transitions and for determining the current state of threads.

### Primordial thread

The execution of a program is initially under the control of a single thread known as the "primordial thread". The primordial thread has name `main` and a tag referring to a mutable box for storing thread-local data. All threads are terminated when the primordial thread terminates.

Expressions entered in the read-eval-print loop of LispKit are executed on the primordial thread. Whenever execution of an expression is finished, all threads (except for the primordial thread) are terminated automatically.

### Memory coherency

Read and write operations on the store, such as reading and writing a variable, an element of a vector or a string, are not necessarily atomic. It is an error for a thread to write a location in the store while some other thread reads or writes that same location. It is the responsibility of the application to avoid write/read and write/write races through appropriate uses of the synchronization primitives. Concurrent reads and writes to ports are allowed, including input and output to the console.

### Dynamic environment

The "dynamic environment" is a structure which allows the system to find the value returned by `current-input-port`, `current-output-port`, etc. The procedures `with-input-from-file`, `with-output-to-file`, etc. extend the dynamic environment to produce a new dynamic environment which is in effect for the duration of the call to the thunk passed as the last argument. LispKit provides procedures and special forms to define new "dynamic variables" and bind them in the dynamic environment via `make-parameter` and `parameterize`.

Each thread has its own dynamic environment. When a thread's dynamic environment is extended this does not affect the dynamic environment of other threads. When a thread creates a continuation, the thread's dynamic environment and the dynamic-wind stack are saved within the continuation. When this continuation is invoked, the required dynamic-wind before and after thunks are called and the saved dynamic environment is reinstated as the dynamic environment of the current thread. During the call to each required dynamic-wind before and after thunk, the dynamic environment and the dynamic-wind stack in effect when the corresponding dynamic-wind was executed are reinstated. Note that this specification clearly defines the semantics of calling `call-with-current-continuation` or invoking a continuation within a before or after thunk. The semantics are well defined even when a continuation created by another thread is invoked.

### Thread-management API

**thread-type-tag** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  

Symbol representing the `thread` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all thread objects.

**(current-thread)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the current thread, i.e. the thread executing the current expression.

**(thread? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a thread object, otherwise `#f` is returned.

```scheme
(thread? (current-thread))  ⇒  #t
(thread? 12)                ⇒  #f
```

**(make-thread _thunk_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-thread _thunk name_)**  
**(make-thread _thunk name tag_)**  

Creates a new thread for executing _thunk_. Each thread has a thunk to execute as well as a _name_ identifying the thread and a _tag_ which can be used to associate arbitrary objects with a thread. Both _name_ and _tag_ can be arbitrary values. The default for _name_ and _tag_ is `#f`.

New threads are not automatically made runnable; the procedure `thread-start!` must be used for that. Besides _name_ and _tag_, a thread encapsulates an end-result, an end-exception, as well as a list of locked/owned mutexes. The thread's execution consists of a call to _thunk_ with the "initial continuation". This continuation causes the (then) current thread to store the result in its end-result field, abandon all mutexes it owns, and finally terminate.

The dynamic-wind stack of the initial continuation is empty. The thread inherits the dynamic environment from the current thread. Moreover, in this dynamic environment the exception handler is bound to the "initial exception handler" which is a unary procedure which causes the (then) current thread to store in its end-exception field an "uncaught exception" object whose "reason" is the argument of the handler, abandon all mutexes it owns, and finally terminate.  

**(thread _stmt ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

Creates a new thread for executing the statements _stmt_ ... . This statement is equivalent to:

```scheme
(make-thread (thunk stmt ...))
```

**(spawn _thunk_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(spawn _thunk name_)**  
**(spawn _thunk name tag_)**  

Creates a new thread for executing _thunk_ and starts it. Each thread has a thunk to execute as well as a _name_ identifying the thread and a _tag_ which can be used to associate arbitrary objects with a thread. Both _name_ and _tag_ can be arbitrary values. This statement is equivalent to:

```scheme
(thread-start! (make-thread thunk name tag))
```

**(go _stmt ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

Creates a new thread for executing the statements _stmt_ ... and starts it. This statement is equivalent to:

```scheme
(thread-start! (make-thread (thunk stmt ...)))
```

**(parallel _thunk0 thunk1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Executes _thunk0_ on the current thread, and spawns new threads for executing _thunk1 ..._ in parallel. `parallel` only terminates when all parallel computations have terminated. It returns _n_ results for _n_ thunks provided as arguments.

**(parallel/timeout _timeout default thunk ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Executes each _thunk_ in parallel on a separate thread and terminates only if all parallel threads have terminated or the _timeout_ has triggered. _timeout_ is a number specifying the maximum time in seconds the computations are allowed to take. `parallel/timeout` returns _n_ results for _n_ thunks provided as arguments or _default_ in case the timeout triggers.

**(thread-name _thread_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the name of the _thread_.

**(thread-tag _thread_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the tag of the _thread_.

**(thread-runnable? _thread_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _thread_ is in runnable state; otherwise `#f` is returned.

**(thread-blocked? _thread_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _thread_ is in runnable state; otherwise `#f` is returned.

**(thread-terminated? _thread_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _thread_ is in terminated state; otherwise `#f` is returned.

**(thread-max-stack)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(thread-max-stack _limit_)**  
**(thread-max-stack _thread_)**  
**(thread-max-stack _thread limit_)**  

Returns the maximum stack size or sets it to a new limit. If no arguments are provided, the maximum stack size of the current thread is returned. If just fixnum _limit_ is provided as an argument, the current thread's maximum stack size is set to _limit_. If just _thread_ is provided as an argument, the maximum stack size of _thread_ is returned. If both _thread_ and _limit_ are provided, then procedure `thread-max-stack` sets the maximum stack size of _thread_ to _limit_.

Changing the stack size while a thread is running is allowed, but it's not always possible to update the limit. The boolean returned by procedure `thread-max-stack` for forms where the maximum stack size is supposed to be updated, indicates whether the update worked. The return value is `#t` in this case.

**(thread-start! _thread_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Makes _thread_ runnable. The _thread_ must be a new thread. `thread-start!` returns the _thread_. Executing the following code either prints `ba` or `ab`.

```scheme
(let ((t (thread-start! (thread (write 'a)))))
  (write 'b)
  (thread-join! t))
```

**(thread-yield!)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The current thread exits the running state as if its quantum had expired. Here is an example how one could use `thread-yield`:

```scheme
; a busy loop that avoids being too wasteful of the CPU
(let loop ()
  ; try to lock m but don't block
  (if (mutex-try-lock! m)
      (begin
        (display "locked mutex m")
        (mutex-unlock! m))
      (begin
        (do-something-else)
        (thread-yield!) ; relinquish rest of quantum
        (loop))))
```

**(thread-sleep! _timeout_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The current thread waits for _timeout_ seconds. This blocks the thread only if _timeout_ is a positive number.

```scheme
; a clock with a gradual drift:
(let loop ((x 1))
  (thread-sleep! 1)
  (write x)
  (loop (+ x 1)))
; a clock with no drift:
(let ((start (current-second)))
  (let loop ((x 1))
    (thread-sleep!
      (- (+ start x) (current-second)))
    (write x)
    (loop (+ x 1))))
```

**(thread-terminate! _thread_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(thread-terminate! _thread wait_)**  

Causes an abnormal termination of the _thread_. If the _thread_ is not already terminated, all mutexes owned by the _thread_ become unlocked/abandoned and a "terminated thread exception" object is stored in the _thread_'s end-exception field. By default, the termination of the _thread_ will occur before `thread-terminate!` returns, unless parameter _wait_ is provided and set to `#f`. If _thread_ is the current thread, `thread-terminate!` does not return.

This operation must be used carefully because it terminates a thread abruptly and it is impossible for that thread to perform any kind of cleanup. This may be a problem if the thread is in the middle of a critical section where some structure has been put in an inconsistent state. However, another thread attempting to enter this critical section will raise an "abandoned mutex exception" because the mutex is unlocked/abandoned. This helps avoid observing an inconsistent state.

**(thread-join! _thread_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(thread-join! _thread timeout_)**  
**(thread-join! _thread timeout default_)**  

The current thread waits until the _thread_ terminates (normally or not) or until the timeout is reached, if _timeout_ is provided. _timeout_ is a number in seconds relative to the time `thread-join!` is called. If the timeout is reached, _thread-join!_ returns _default_ if it is provided, otherwise a "join timeout exception" is raised. If the _thread_ terminated normally, the content of the end-result field of _thread_ is returned, otherwise the content of the end-exception field is raised. Example:

```scheme
(let ((th (go (+ 1 2 3))))
  (* 10 (thread-join! th)))
⇒ 60
(let ((th (go (error "broken thread"))))
  (* 10 (thread-join! th)))
⇒ raises: [uncaught] [error] broken thread
```

## Mutexes

A mutex is a synchronization abstraction, enforcing mutual exclusive access to a resource when there are many threads of execution.

### Mutex states

A mutex can be in one of four states: _locked_ (either _owned_ or not _owned_) and _unlocked_ (either _abandoned_ or _not abandoned_). An attempt to lock a mutex only succeeds if the mutex is in an unlocked state, otherwise the current thread must wait.

A mutex in the _locked/owned_ state has an associated "owner" thread, which by convention is the thread that is responsible for unlocking the mutex. This case is typical of critical sections implemented as "lock mutex, perform operation, unlock mutex". A mutex in the _locked/not-owned_ state is not linked to a particular thread. A mutex becomes locked when a thread locks it using the `mutex-lock!` primitive. A mutex becomes _unlocked/abandoned_ when the owner of a _locked/owned_ mutex terminates. A mutex becomes _unlocked/not-abandoned_ when a thread unlocks it using the `mutex-unlock!` procedure.

The mutexes provided by library `(lispkit thread)` do not implement "recursive" mutex semantics. An attempt to lock a mutex that is locked already implies that the current thread must wait, even if the mutex is owned by the current thread. This can lead to a deadlock if no other thread unlocks the mutex.

### Mutex state

Upon creation, a mutex can be associated with a tag, which is an arbitrary object used in an application-specific way to associate data with the mutex.

### Mutex-management API

**mutex-type-tag** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  

Symbol representing the `mutex` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all mutex objects.

**(mutex? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a mutex, otherwise returns `#f`.

**(make-mutex)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-mutex _name_)**  
**(make-mutex _name tag_)**  

Returns a new mutex in the _unlocked/not-abandoned_ state. The optional _name_ is an arbitrary object which identifies the mutex (for debugging purposes), defaulting to `#f`. It is also possible to provide a tag, which is an arbitrary object used in an application-specific way to associate data with the mutex. `#f` is used as a default if the tag is not provided.

**(mutex-name _mutex_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the name of the _mutex_.

```scheme
(mutex-name (make-mutex 'foo))  ⇒  foo
```

**(mutex-tag _mutex_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the tag of the _mutex_.

```scheme
(mutex-tag (make-mutex 'id '(1 2 3)))  ⇒  (1 2 3)
```

**(mutex-state _mutex_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the state of the _mutex_. The possible results are:

   - _T_: the _mutex_ is in the _locked/owned_ state and thread _T_ is the owner of the _mutex_
   - `not-owned`: the _mutex_ is in the locked/not-owned state
   - `abandoned`: the _mutex_ is in the unlocked/abandoned state
   - `not-abandoned`: the _mutex_ is in the unlocked/not-abandoned state  

```scheme
(mutex-state (make-mutex))
⇒  not-abandoned
(let ((mutex (make-mutex)))
  (mutex-lock! mutex #f (current-thread))
  (let ((state (mutex-state mutex)))
    (mutex-unlock! mutex)
    (list state (mutex-state mutex))))
⇒  (#<thread main: runnable> not-abandoned)
```

**(mutex-lock! _mutex_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(mutex-lock! _mutex timeout_)**  
**(mutex-lock! _mutex timeout thread_)**  

Locks _mutex_. If _mutex_ is already locked, the current thread waits until the _mutex_ is unlocked, or until the timeout is reached if _timeout_ is supplied. If the timeout is reached, `mutex-lock!` returns `#f`. Otherwise, the state of the _mutex_ is changed as follows:

  - if _thread_ is `#f`, the _mutex_ becomes _locked/not-owned_,
  - otherwise, let _T_ be _thread_ (or the current thread if _thread_ is not supplied): if _T_ is terminated the _mutex_ becomes _unlocked/abandoned_, otherwise _mutex_ becomes _locked/owned_ with _T_ as the owner.

After changing the state of the _mutex_, an "abandoned mutex exception" is raised if the _mutex_ was _unlocked/abandoned_ before the state change, otherwise `mutex-lock!` returns `#t`. It is not an error if the _mutex_ is owned by the current thread, but the current thread will have to wait.  

**(mutex-try-lock! _mutex_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(mutex-try-lock! _mutex thread_)**  

Locks _mutex_ with owner _thread_ and returns `#t` if _mutex_ is not already locked. Otherwise, `#f` is returned. This is equivalent to: `(mutex-lock! mutex 0 thread)`

**(mutex-unlock! _mutex_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(mutex-unlock! _mutex cvar_)**  
**(mutex-unlock! _mutex cvar timeout_)**  

Unlocks the _mutex_ by making it _unlocked/not-abandoned_. It is not an error to unlock an unlocked mutex and a mutex that is owned by any thread. If _cvar_ is supplied, the current thread is blocked and added to the condition variable _cvar_ before unlocking _mutex_. The thread can unblock at any time but no later than when an appropriate call to `condition-variable-signal!` or `condition-variable-broadcast!` is performed, and no later than the timeout (if _timeout_ is supplied). If there are threads waiting to lock this _mutex_, the scheduler selects a thread, the mutex becomes _locked/owned_ or _locked/not-owned_, and the thread is unblocked. `mutex-unlock!` returns `#f` when the timeout is reached, otherwise it returns `#t`.
 
`mutex-unlock!` is related to the "wait" operation on condition variables available in other thread systems. The main difference is that "wait" automatically locks _mutex_ just after the thread is unblocked. This operation is not performed by `mutex-unlock!` and so must be done by an explicit call to `mutex-lock!`. This has the advantages that a different timeout and exception handler can be specified on the mutex-lock! and mutex-unlock! and the location of all the mutex operations is clearly apparent. A typical use with a condition variable is this:

```scheme
(let loop ()
  (mutex-lock! m)
  (if (condition-is-true?)
      (begin
        (do-something-when-condition-is-true)
        (mutex-unlock! m))
      (begin
        (mutex-unlock! m cv)
        (loop))))
```

**(with-mutex _mutex stmt0 stmt1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

`with-mutex` locks _mutex_ and then executes statements _stmt0_, _stmt1_, ... After all statements are executed, _mutex_ is being unlocked. `with-mutex` returns the result of evaluating the last statement. For locking and unlocking t _mutex_, `dynamic-wind` is used so that _mutex_ is automatically unlocked if an error or new continuation exits the statements, and it is re-locked, if the statements are re-entered by a captured continuation.

`(with-mutex m stmt0 stmt1 ...)` is equivalent to:

```scheme
(dynamic-wind
  (lambda () (mutex-lock! m))
  (lambda () (begin stmt0 stmt1 ...))
  (lambda () (mutex-unlock! m)))
```

## Condition variables

### Semantics

A condition variable represents a set of blocked threads. These blocked threads are waiting for a certain condition to become true. When a thread modifies some program state that might make the condition true, the thread unblocks some number of threads 9one or all depending on the primitive used) so they can check the value of that condition. This allows complex forms of inter-thread synchronization to be expressed more conveniently than with mutexes alone.

Each condition variable has a tag which can be used in an application specific way to associate data with the condition variable.

### Condition variable management

**condition-type-tag** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>

Symbol representing the `condition` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all condition objects.

**(condition-variable? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a condition variable, otherwise returns `#f`.

**(make-condition-variable)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-condition-variable _name_)**  
**(make-condition-variable _name tag_)**  

Returns a new empty condition variable. The optional _name_ is an arbitrary object which identifies the condition variable for debugging purposes. It defaults to `#f`.  It is also possible to provide a tag, which is an arbitrary object used in an application-specific way to associate data with the condition variable. `#f` is used as a default if the tag is not provided.

**(condition-variable-name _cvar_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the name of the condition variable _cvar_.

**(condition-variable-tag _cvar_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the tag of the condition variable _cvar_.

**(condition-variable-wait! _cvar mutex_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(condition-variable-wait! _cvar mutex timeout_)**  

`condition-variable-wait!` can be used to make the current thread wait on condition variable _cvar_. It is assumed the current thread has locked _mutex_ when `condition-variable-wait!` is called. `condition-variable-wait!` will unlock _mutex_ and wait on _cvar_, either until _cvar_ unblocks the thread again or _timeout_ (in seconds) triggers. When the current thread is woken up again, it reclaims the lock on _mutex_.

`condition-variable-wait!` returns `#f` if the timeout triggers, otherwise `#t` is being returned.

**(condition-variable-signal! _cvar_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

If there are threads blocked on the condition variable _cvar_, the scheduler selects a thread and unblocks it.

**(condition-variable-broadcast! _cvar_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Unblocks all the threads blocked on the condition variable _cvar_.


## Exception handling

**(join-timeout-exception? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a "join timeout exception" object, otherwise returns `#f`. A join timeout exception is raised when `thread-join!` is called, the timeout is reached and no _default_ is supplied.  

**(abandoned-mutex-exception? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an "abandoned mutex exception" object, otherwise returns `#f`. An abandoned mutex exception is raised when the current thread locks a mutex that was owned by a thread which terminated.  

**(terminated-thread-exception? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a "terminated thread exception" object, otherwise returns `#f`. A terminated thread exception is raised when `thread-join!` is called and the target thread has terminated as a result of a call to `thread-terminate!`.

**(uncaught-exception? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an "uncaught exception" object, otherwise returns `#f`. An uncaught exception is raised when `thread-join!` is called and the target thread has terminated because it raised an exception that called the initial exception handler of that thread.

**(uncaught-exception-reason _exc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

_exc_ must be an "uncaught exception" object. `uncaught-exception-reason` returns the object which was passed to the initial exception handler of that thread.

## Utilities 

**(processor-count)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(processor-count _active?_)**

Returns the number of processors provided by the underlying hardware. If _active?_ is set to `#f` (the default), the number of physical processors is returned. If _active?_ is set to `#t`, the number of active processors (i.e. available for executing code) is returned.

**(runnable-thread-count)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of threads that are currently in runnable state.

**(allocated-thread-count)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of allocated threads, i.e. threads in any state that are not garbage collected yet.

**(abort-running-threads)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Aborts all threads except for the primordial thread and waits until all threads are terminated. This procedure must only be invoked from the primordial thread.

**(wait-threads-terminated)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Waits until all threads except for the primordial thread are terminated. This procedure must only be invoked from the primordial thread.

***

Large portions of this documentation:  
Copyright (c) 2001, Marc Feeley. All rights reserved.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
