# LispKit Thread Future

Library `(lispkit thread future)` implements _futures_, an abstraction allowing for fine-grained concurrent computation of values. A _future_ encapsulates an expression whose computation may occur in parallel with the code of the calling thread and all other threads that are currently executed including other futures. Like _promises_, _futures_ are proxies that can be queried to obtain the value of the encapsulated computation. While _promises_ compute values on demand, _futures_ compute values in parallel.

Futures are created either with special form `future` or procedure `make-future`. The creation of a future implicitly kicks off the parallel computation of the future's value. The value can be accessed via `future-get`, which will block if the computation of the value has not finished yet. `future-get` can optionally be given a timeout to control the maximum number of seconds a thread is waiting for the results of a future to become available. Via procedure `future-done?` it is possible to check in a non-blocking manner if the future has finished computing its value already.

**future-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `future` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all future objects.

**(future? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a future object; `#f` otherwise.

**(future _expr_)** <span style="float:right;text-align:rigth;">[syntax]</span>   

Via the `future` syntax it is possible to create a future which evaluates _expr_ in parallel in the same environment in which `future` appears. The `future` syntax returns a future object which eventually will contain the result of evaluating _expr_ concurrently. If an exception is raised within _expr_, it is not caught but delivered later when `future-get` gets invoked.

**(make-future _proc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new future which computes its value by executing thunk _proc_ in parallel in the same environment in which `make-future` is called. If an exception is raised within _expr_, it is not caught but delivered later when `future-get` gets invoked.

**(make-evaluated-future _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new future which encapsulates _obj_ as its value without doing any computation. When `future-get` gets invoked, _obj_ is returned.

**(make-failing-future _exc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new future which encapsulates exception _exc_ as its value without doing any computation. When `future-get` gets invoked, _exc_ is being raised.

**(future-done? _f_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the computation of future _f_ is finished and the value of _f_ available; otherwise `#f` is returned. This procedure does not block.

**(future-get _f_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>   
**(future-get _f timeout_)**  
**(future-get _f timeout default_)**  

Returns the value associated with future _f_. If the value of _f_ is not available yet (because the computation is still ongoing), this procedure will block. _timeout_ defines the maximum number of seconds `future-get` is waiting for receiving the value of _f_. If _timeout_ is not provided, `future-get` will wait indefinitely. When `future-get` times out, _default_ is returned as its result. If _default_ is not provided, then `future-get` will raise an error when timing out. If an exception is raised during the computation, `future-get` will raise this exception every single time it is invoked.

**(touch _f_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`(touch f)` is equivalent to `(future-get f)`, but does not support the optional parameters of `future-get`. It is provided for compatibility with _future_ implementations of other Scheme implementations.
