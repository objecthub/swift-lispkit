# LispKit HTTP

Library `(lispkit http)` provides an API for downloading data from and uploading data to endpoints specified by URLs using the HTTP family of protocols.

## HTTP sessions

HTTP session objects are used to coordinate a group of related data-transfer tasks. Within each session, typically a series of tasks are created, each of which represents a request for a specific URL. The tasks within a given HTTP session share a common session configuration, which defines connection behavior, like the maximum number of simultaneous connections to make to a single host, whether connections can use the cellular network, etc.

**http-session-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `http-session` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all HTTP session objects.

**current-http-session** <span style="float:right;text-align:rigth;">[parameter object]</span>  

This parameter object represents the current default HTTP session. By default, this is set to a shared multi-purpose session object. Procedures requiring an HTTP session typically make the HTTP session argument optional. If it is not provided, the result of `(current-http-session)` is used instead. The value of `current-http-session` can be overridden with `parameterize`.

**(http-session? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a HTTP session object; `#f` otherwise.

**(make-http-session)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-http-session _proto_)**  
**(make-http-session _proto timeout_)**  
**(make-http-session _proto timeout cookies?_)**  
**(make-http-session _proto timeout cookies? cache_)**  
**(make-http-session _proto timeout cookies? cache maxcon_)**  
**(make-http-session _proto timeout cookies? cache maxcon pipe?_)**  
**(make-http-session _proto timeout cookies? cache maxcon pipe? cell?_)**  

Returns a new HTTP session object. The configuration is copied from the prototype HTTP object _proto_. The arguments following _proto_ override individual settings of _proto_. If one of those arguments is set to `()`, then it is ignored.

If _proto_ is `#f` (or not provided at all), system-specific defaults are used. If _proto_ is `#t`, an _ephemeral_ session default is used which is not writing caches, cookies, or credentials to disk. Otherwise, it is assumed _proto_ is an HTTP session object whose configuration will be used for the newly created HTTP session object.

_timeout_ defines the time in seconds to wait for (additional) data. The default is 60. _cookies?_ is a boolean argument determining whether requests should automatically provide cookies from the shared cookie store. The default is `#t`. If set to `#f`, then cookie headers need to be provided manually.

_cache_ defines a cache policy. The following policies, specified as symbols, are supported:
  - `use-protocol-cache-policy`: Use the caching logic defined in the protocol implementation (default).
  - `reload-ignoring-local-cache`: The URL load should be loaded only from the originating source.
  - `reload-ignoring-local-remote-cache`: Ignore local cache data, and instruct proxies and other intermediates to disregard their caches so far as the protocol allows.
  - `return-cache-data-else-load`: Use existing cache data, regardless or age or expiration date, loading from originating source only if there is no cached data.
  - `return-cache-data-dont-load`: Use existing cache data, regardless or age or expiration date, and fail if no cached data is available.
  - `reload-revalidating-cache`: Use cache data if the origin source can validate it; otherwise, load from the origin.

Argument _maxcon_ specifies the maximum number of simultaneous connections made to each host by requests initiated by this session. The default value is 6. _pipe?_ is a boolean argument  determining whether HTTP pipelining should be used. _cell?_ is a boolean argument specifying whether connections should be made over a cellular network.

**(http-session-copy)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(http-session-copy _session_)**  

Returns a copy of _session_. If argument _session_ is not provided, a copy of the current value of parameter object `current-http-session` is returned.

**(http-session-timeout)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(http-session-timeout _session_)**  

Returns the connection timeout for _session_. This is the time in seconds to wait for (additional) data. If argument _session_ is not provided, the current value of parameter object `current-http-session` is used as a default.

**(http-session-send-cookies?)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(http-session-send-cookies? _session_)**  

Returns a boolean value determining whether requests sent via _session_ automatically provide cookies from the shared cookie store. If argument _session_ is not provided, the current value of parameter object `current-http-session` is used as a default.

**(http-session-cache-policy)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(http-session-cache-policy _session_)**  

Returns the cache policy used by _session_. If argument _session_ is not provided, the current value of parameter object `current-http-session` is used as a default.

The following cache policies are supported: `use-protocol-cache-policy`, `reload-ignoring-local-cache`, `reload-ignoring-local-remote-cache`, `return-cache-data-else-load`, `return-cache-data-dont-load`, `reload-revalidating-cache`. Details are provided in the description of procedure `make-http-session`.

**(http-session-max-connections)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(http-session-max-connections _session_)**  

Returns the maximum number of simultaneous connections made to each host by requests initiated by _session_. If _session_ is not provided, the current value of parameter object `current-http-session` is used as a default.

**(http-session-use-pipelining?)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(http-session-use-pipelining? _session_)**  

Returns a boolean value determining whether HTTP pipelining should be used by _session_. If _session_ is not provided, the current value of parameter object `current-http-session` is used.

**(http-session-allow-cellular?)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(http-session-allow-cellular? _session_)**  

Returns a boolean value determining whether connections should be made over a cellular network by _session_. If _session_ is not provided, the current value of parameter object `current-http-session` is used.

**(http-session-tasks)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(http-session-tasks _session_)**  

Returns a future which will eventually hold the number of currently ongoing tasks initiated via _session_. If _session_ is not provided, the current value of parameter object `current-http-session` is used.

**(http-session-flush!)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(http-session-flush! _session_)**  

Flushes cookies and credentials to disk, clears transient caches, and ensures that future requests sent via _session_ occur on a new TCP connection. Returns a future which will eventually be set to `#t` once flush has completed. If _session_ is not provided, the current value of parameter object `current-http-session` is used.

**(http-session-reset!)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(http-session-reset! _session_)**  

Empties all cookies, caches and credential stores, removes disk files, flushes in-progress downloads to disk, and ensures that future requests initiated via _session_ occur on a new socket. Returns a future which will eventually be set to `#t` once reset has completed. If _session_ is not provided, the current value of parameter object `current-http-session` is used.

**(http-session-finish!)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(http-session-finish! _session_)**  

This procedure invalidates the session, allowing any outstanding tasks to finish. It returns immediately without waiting for tasks to finish. Once a session is invalidated, new tasks cannot be created in the session, but existing tasks continue until completion. After invalidation, session objects cannot be reused. To cancel all outstanding tasks immediately, call procedure `http-session-cancel!` instead. If _session_ is not provided, the current value of parameter object `current-http-session` is used.

**(http-session-cancel!)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(http-session-cancel! _session_)**  

Cancels all outstanding tasks and then invalidates the session. Once invalidated, outstanding tasks will not complete (e.g. by initializing a future) and the session object cannot be reused.  
To allow outstanding tasks to run until completion, call `http-session-finish!` instead. If _session_ is not provided, the current value of parameter object `current-http-session` is used.

**(http-session-send _request_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(http-session-send _request session_)**  

Creates a task that retrieves the contents of a URL via the specified HTTP request object _request_, and eventually stores a HTTP result object in the future returned by `http-session-send`. If _session_ is not provided, the current value of parameter object `current-http-session` is used.


## HTTP requests

**http-request-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `http-request` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all HTTP request objects.

**(http-request? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a HTTP request object; `#f` otherwise.

**(make-http-request _url meth_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-http-request _url meth headers_)**  
**(make-http-request _url meth headers timeout_)**  
**(make-http-request _url meth headers timeout cookies?_)**  
**(make-http-request _url meth headers timeout cookies? cache_)**  
**(make-http-request _url meth headers timeout cookies? cache pipe?_)**  
**(make-http-request _url meth headers timeout cookies? cache pipe? cell?_)**  

Returns a new HTTP request for _url_ using the HTTP method _meth_. Supported are the methods `"GET"`, `"HEAD"`, `"POST"`, `"PUT"`, `"DELETE"`, `"CONNECT"`, `"OPTIONS"`, `"TRACE"`, and `"PATCH"`. _headers_ is an association list mapping HTTP header names (strings) into header values (strings). The remaining arguments override settings of the session when the request is sent. `()` denotes that the setting of the session should be used.

_timeout_ defines the time in seconds to wait for (additional) data. _cookies?_ is a boolean argument determining whether requests should automatically provide cookies from the shared cookie store. _cache_ defines a cache policy (see `make-http-request`). _pipe?_ is a boolean argument  determining whether HTTP pipelining should be used. _cell?_ is a boolean argument specifying whether connections should be made over a cellular network.

**(http-request-copy _req_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(http-request-copy _req headers_)**  
**(http-request-copy _req headers timeout_)**  
**(http-request-copy _req headers timeout cookies?_)**  
**(http-request-copy _req headers timeout cookies? cache_)**  
**(http-request-copy _req headers timeout cookies? cache pipe?_)**  
**(http-request-copy _req headers timeout cookies? cache pipe? cell?_)**  

Returns a copy of HTTP request _req_. The arguments following _req_ override existing settings of _req_. If one of those arguments is set to `()` (or `#f` for non-boolean parameters), then it is ignored.

_headers_ is an association list mapping HTTP header names (strings) into header values (strings). _timeout_ defines the time in seconds to wait for (additional) data. _cookies?_ is a boolean argument determining whether requests should automatically provide cookies from the shared cookie store. _cache_ defines a cache policy (see `make-http-request`). _pipe?_ is a boolean argument  determining whether HTTP pipelining should be used. _cell?_ is a boolean argument specifying whether connections should be made over a cellular network.

**(http-request-url _req_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the ULR of HTTP request _req_ as a string.

**(http-request-method _req_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the HTTP method of HTTP request _req_ as a string.

**(http-request-headers _req_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the headers of HTTP request _req_ as an association list mapping HTTP header names (strings) into header values (strings).

**(http-request-header _req key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the value for the HTTP header _key_ from HTTP request _req_ as a string. If header _key_ does not exist, then `#f` is returned.

**(http-request-header-set! _req key value_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the value for the HTTP header _key_ (string) in HTTP request _req_ to _value_. _value_ can either be a fixnum, flonum, boolean, symbol, or string. It is automatically converted into a string and stored as the new header value for _key_. If header _key_ existed before, it is overridden with a new value via `http-request-header-set!`.

**(http-request-header-remove! _req key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes HTTP header _key_ (string) from HTTP request _req_. If _key_ does not exist, then _req_ does not change.

**(http-request-content _req_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the body of HTTP request _req_ as a bytevector. If no body has been defined for _req_, the `#f` is returned.

**(http-request-content-set! _req bvec_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(http-request-content-set! _req bvec start_)**  
**(http-request-content-set! _req bvec start end_)**  

Assigns bytevector _bvec_ between _start_ and _end_ as the body to HTTP request _req_. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.

**(http-request-timeout _req_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the timeout in seconds associated with HTTP request _req_.

**(http-request-timeout-set! _req timeout_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the timeout in seconds to _timeout_ for HTTP request _req_.

**(http-request-send-cookies _req_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if HTTP request _req_ has been configured to automatically provide cookies from the shared cookie store. If the request has been configured to not provide cookies, then `#f` is returned. Without an explicit configuration of this setting, `()` is returned.

**(http-request-cache-policy _req_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the caching policy associated with HTTP request _req_, if one was set explicitly. Otherwise, `()` is returned. The following policies, specified as symbols, are supported: `use-protocol-cache-policy`, `reload-ignoring-local-cache`, `reload-ignoring-local-remote-cache`, `return-cache-data-else-load`, `return-cache-data-dont-load`, and `reload-revalidating-cache`.

**(http-request-use-pipelining _req_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if HTTP request _req_ has been configured to use HTTP pipelining. If the request has been configured to not use pipelining, then `#f` is returned. Without an explicit configuration of this setting, `()` is returned.

**(http-request-allow-cellular _req_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if HTTP request _req_ has been configured to allow connections over cellular networks. If the request has been configured to not allow cellular connections, then `#f` is returned. Without an explicit configuration of this setting, `()` is returned.


## HTTP responses

**http-response-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `http-response` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all HTTP response objects.

**(http-response? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a HTTP response object; `#f` otherwise.

**(http-response-content _resp_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the body of the HTTP response _resp_ as a bytevector.

**(http-response-status-code _resp_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the status code of the HTTP response _resp_ as a fixnum.

**(http-response-mime-type _resp_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the MIME type describing the type of content provided by HTTP response _resp_ as a string. If no specific MIME type was included in _resp_, then `#f` is returned.

**(http-response-encoding _resp_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the text encoding name provided by HTTP response _resp_ as a string. If no specific text encoding name was included in _resp_, then `#f` is returned.

**(http-response-url _resp_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the URL for the HTTP response _resp_ as a string. If the URL is unknown, then `#f` is returned.

**(http-response-headers _resp_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(http-response-headers _resp str?_)**  

Returns the headers of HTTP response _resp_ as an association list mapping HTTP header names (strings) into header values, which are either fixnums, flonums, booleans, or strings. Header values can be forced to be represented as a string if _str?_ is set to `#t` (default is `#f`).

**(http-response-header _resp key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(http-response-header _resp key str?_)**  

Returns the value for the HTTP header _key_ from HTTP request _req_ either as a fixnum, flonum, boolean or string. If header _key_ does not exist, then `#f` is returned. The result can be forced to be a string if _str?_ is set to `#t` (default is `#f`).

## Miscellaneous

**(http-status-code-\>string _sc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a string representation for numeric HTTP status code _sc_.
