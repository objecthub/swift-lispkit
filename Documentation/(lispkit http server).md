# LispKit HTTP Server

Library `(lispkit http server)` implements a simple multi-threaded HTTP server which can be freely configured for different use cases. The HTTP server allows two different types of request processors to be registered: _middleware processors_ which are applied to all incoming requests sequentially and regular _request processors_ which define how a request for a specified route is turned into a response.

For processing an HTTP request, the HTTP server first sends the request through the middleware processors in the order they were registered. As soon as one middleware processor returns a response, this response becomes the response of the request. Only when all middleware processors did not return a response, the request handler matching the route is being invoked and the result of this handler defines the response for the request.

The following script configures and starts a simple web server. By default, a server listens on the loopback address, i.e. it can only be reached from the machine it is running on; see procedure `http-server-start!` for making a server available to other machines. Please note that procedure `http-server-start!` does not terminate until the web server is shut down (e.g. by visiting "/quit" for the server below).

```scheme
(import (lispkit thread)
        (lispkit http server))
;; Make a new HTTP server
(define server (make-http-server))
;; Register a default handler which returns a "not found" response
(http-server-register-default! server
  (lambda (request) (srv-response-not-found)))
;; Register a simple handler for the "/hello" route
(http-server-register! server "GET" "/hello/:name"
  (lambda (request)
    (make-srv-response 200 #f
      (string-append
        "Hello "
        (srv-request-path-param request "name") "!"))))
;; Define a counter for the requests served
(define requests-served (make-atomic-box 0))
;; Increment the counter in a middleware processor
(http-server-register-middleware! server
  (lambda (request)
    (atomic-box-inc+mul! requests-served 1) #f))
;; Register a handler for "/quit" which terminates the server
(http-server-register! server "GET" "/quit"
  (lambda (request)
    ; Terminate the server with a 1 second delay
    (spawn (thunk
             (thread-sleep! 1.0)
             (http-server-stop! (srv-request-server request))))
    (make-srv-response 200 #f
      (string-append
        "terminating the server; "
        (number->string (atomic-box-ref requests-served))
        " requests served"))))
;; Enable debug logging
(http-server-log-severity-set! server 0)
;; Start the server; this call blocks until the server is terminated
(http-server-start! server 3000)
```

After starting the server, three requests are made for `/hello/Matthias`, `/hello/World` and `/quit`. The last request also terminates the server. This is the server log for this interaction:

```
[http/worker] started worker 0/0
[http/worker] started worker 1/1
[http/worker] started worker 2/2
[http/server] server started for port 3000; try connecting at http://127.0.0.1:3000
[http/req] GET /hello/Matthias (127.0.0.1)
[http/worker] worker 1 received request
[http/worker] worker 1: GET /hello/Matthias
[http/worker] worker 1 idle
[http/req] GET /hello/World (127.0.0.1)
[http/worker] worker 0 received request
[http/worker] worker 0: GET /hello/World
[http/worker] worker 0 idle
[http/req] GET /quit (127.0.0.1)
[http/worker] worker 1 received request
[http/worker] worker 1: GET /quit
[http/worker] worker 1 idle
[http/worker] closed worker 0/0
[http/worker] closed worker 2/0
[http/worker] closed worker 1/0
[http/server] server stopped for port 3000
```

The worker threads are picking up requests from a queue; which worker handles a request is not deterministic.

## Server behavior

A server listens on the _loopback address_ by default. It can only be reached from the machine it is running on. Procedure `http-server-start!` has an optional argument for listening on all network interfaces or on a specific address.

Servers support persistent HTTP/1.1 connections: a client can send many requests via the same connection. A connection that does not send a request for 60 seconds is closed. Idle connections do not occupy worker threads. Each request is entered into a queue and answered by the next available worker thread, independent of the connection it was received on.

Accepting connections, reading requests and writing responses is done by the thread calling `http-server-start!`. A client which stalls while sending a request or does not read its response delays all other clients. For this reason, a server gives up on a client which does not make progress for 5 seconds.

The following requests are answered by the server itself, without invoking a request handler:

  - `HEAD` requests are answered by the handler registered for `GET`; the response body is not sent.
  - `OPTIONS` requests for a path with registered handlers are answered with status 204 and an `Allow` header, unless there is a handler for `OPTIONS`.
  - Requests for a path with registered handlers, but not for the method of the request, are answered with status 405 and an `Allow` header.
  - Requests for a path without handler are answered with status 404 unless a default handler is registered via `http-server-register-default!`.
  - Malformed requests are answered with status 400. Requests with a request line or header line of more than 8 KiB are answered with status 414 and 431 respectively. Requests with headers of more than 64 KiB are answered with status 431, and requests with a body of more than 64 MiB with status 413.
  - Requests which are not completed within 5 seconds are answered with status 408.
  - New connections are refused with status 503 while there are 128 open connections. Requests which find the request queue full are refused with status 503 as well (see `http-server-timeout-set!`).

Every response includes a `Date` header.

## Paths and query values

The path and the query of a request are available as sent by the client, i.e. percent-encoded (e.g. `/caf%C3%A9`), and decoded (`/café`). Decoding loses information: after decoding, `%2F` can no longer be distinguished from a path separator. Furthermore, a value that gets decoded twice can turn into something different than the client sent, e.g. `%252e%252e` first turns into `%2e%2e` and then into `..`, which is dangerous for values used as file paths.

For this reason, procedures `srv-request-path`, `srv-request-query`, `srv-request-query-param`, `srv-request-query-params`, `srv-request-path-param` and `srv-request-path-params` return values as sent by the client by default. Procedures `srv-request-path`, `srv-request-query-param`, `srv-request-query-params`, `srv-request-path-param` and `srv-request-path-params` have an optional argument `decode?` for returning decoded values.

A path variable such as `::path` stands for all its path segments. Its value as sent by the client consists of the path segments joined with `/`, whereas its decoded value consists of the decoded path segments joined with `/`. Procedure `srv-request-path-param-set!` expects the new value of a path parameter in its encoded form, because it is not possible to derive an encoded value from a decoded one. The attributes of a form (see `srv-request-form-attributes`) are always decoded exactly once.

## HTTP servers

**http-server-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `http-server` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all HTTP server objects.

**(http-server? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a HTTP server object; `#f` otherwise.

**(make-http-server)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-http-server _queue-size_)**  
**(make-http-server _queue-size log-level_)**  

Creates a new HTTP server. _queue-size_ is the maximum number of requests the server is able to queue up before starting to reject requests (the default is 10). _log-level_ is the minimum log level for the new HTTP server. _log-level_ is an integer between 0 (= `debug`) and 4 (= `fatal`). Only log messages above and including the level will be output. For details on log levels see the documentation of `(lispkit log)`.

**(http-server-running? _server_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the HTTP server _server_ is currently running; `#f` otherwise.

**(http-server-port _server_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the port the HTTP server _server_ is listening on. If _server_ is not running, `#f` is returned.

**(http-server-ipv4 _server_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the HTTP server _server_ is listening on an IPv4 socket, i.e. if it is listening on the loopback address or on an IPv4 address; returns `#f` if _server_ is listening on an IPv6 address or on all network interfaces (which are served via a combined socket for IPv4 and IPv6). An error is signaled if _server_ is not running.

**(http-server-open-connections _server_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of open HTTP connections for HTTP server _server_.

**(http-server-routes _server_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of routes, i.e. URL paths, supported by the HTTP server _server_. An HTTP server supports a route if it explicitly defines a request handler for it.

**(http-server-handlers _server_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an association list mapping routes, i.e. URL paths, to request handler procedures for HTTP server _server_.

**(http-server-log-severity _server_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the minimum log level for HTTP server _server_. _log-level_ is an integer between 0 (= `debug`) and 4 (= `fatal`). Only log messages above and including the level will be output.

**(http-server-log-severity-set! _server log-level_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the minimum log level for HTTP server _server_ to _log-level_. _log-level_ is an integer between 0 and 4. Only log messages above and including the level will be output. It is possible to use the constants defined in library `(lispkit log)`: `debug` (0), `info` (1), `warn` (2), `err` (3), and `fatal` (4).

**(http-server-timeout _server_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the timeout used by HTTP server _server_ in seconds, or `#f` if the server does not use a timeout. Once a request is received by _server_, it is put into a queue from which request processing worker threads pick up their work. If this queue is full, the request waits for room in the queue, but not longer than the timeout. If the request cannot be entered into the queue within that time, the server refuses the request with status 503 (`Service Unavailable`) and a `Retry-After` header. While a request waits, the server is not able to process any other request. The default timeout is 2 seconds.

**(http-server-timeout-set! _server timeout_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the timeout used by HTTP server _server_ to _timeout_ seconds. If _timeout_ is `#f`, the timeout mechanism is switched off: a request which finds the queue full is refused immediately with status 503, so a full queue never delays other requests.

**(http-server-num-workers _server_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of worker threads used by HTTP server _server_ to process incoming requests concurrently.

**(http-server-log _server log-level tag message ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Concatenates string representations of the _message_ values and outputs them via the logger of HTTP server _server_ if _log-level_ is at least as high as the minimum log level supported by _server_. _tag_ is a string defining a logging tag. See `(lispkit log)` for more details.

**(http-server-register! _server route handler_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(http-server-register! _server method route handler_)**  

Registers a request _handler_ in HTTP server _server_ which processes incoming requests for the given HTTP _method_ and a URL path that matches the _route_ pattern. _handler_ is a procedure which accepts an HTTP request of type `srv-request` and returns a corresponding HTTP response of type `srv-response`. _method_ is a string specifying the HTTP method the handler is handling. Supported are: `"GET"`, `"HEAD"`, `"POST"`, `"PUT"`, `"DELETE"`, `"CONNECT"`, `"OPTIONS"`, `"TRACE"`, and `"PATCH"`. Specifying `#f` or leaving our the _method_ argument implies that _handler_ is handling all possible methods. _route_ is a pattern for URL paths supported by the provided handler. _route_ is a string consists of segments separated by `/`. Each segment can either be:

  - `*`: Segment wildcard (any path segment matches)
  - `**`: Path wildcard (any sequence of path segments matches)
  - `:svar`: Path parameter (any path segment matches; the path segment is assigned to path parameter `svar`)
  - `::pvar`: Path variable (any sequence of path segments matches; the sequence of path segments is assigned to path variable `pvar`)
  - Any other string not containing a `/`: Concrete segment (a path segment of the same string matches)

Examples for valid routes are:

  - `"/person/address"`: This route matches only the exact URL path "/person/address"
  - `"/person/:id/:role"`: This route matches URL paths such as "/person/matthias/admin" and during the matching process, the path parameters `id` and `role` are assigned to `matthias` and `admin` respectively
  - `"/person/:id/*"`: This route is equivalent to the former route but there is no variable assignment to `role`
  - `"/person/:id/::roles"`: This route includes all paths matching the previous two examples and, in addition, also handles paths that have segments beyond `id` and `role`. For instance, "/person/matthias/admin/misc" also matches and path parameter `id` gets assigned to `matthias` and path variable `roles` gets assigned to `admin/misc`
  - `"/person/:id/**"`: This route is equivalent to the previous route without the `roles` variable being assigned

**(http-server-register-default! _server handler_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Registers a default _handler_ for HTTP server _server_. Without a default handler, _server_, by default, returns an error whenever a URL path was used that did not have a matching route. By using `http-server-register-default!`, this behavior can be customized and all requests without a matching route are processed by _handler_. _handler_ is a procedure which accepts an HTTP request of type `srv-request` and returns a corresponding HTTP response of type `srv-response`.

**(http-server-register-middleware! _server processor_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Registers a middleware processor for the given HTTP server _server_. A middleware processor is a function that receives an HTTP request of type `srv-request` and either returns `#f` (request not handled) or an HTTP response of type `srv-response` (request handled). For processing an HTTP request, _server_ first sends the request through the middleware processors in the order they were registered. As soon as one middleware processor returns a response, this response becomes the response of the request. Only after all middleware processors returned `#f`, the request handler matching the route is being invoked and the result of this handler defines the response for the request.

**(http-server-start! _server port_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(http-server-start! _server port address_)**  
**(http-server-start! _server port address num_)**  
**(http-server-start! _server port address num name_)**  

Starts the HTTP server _server_ listening on _port_ for requests. _address_ determines which network addresses the server accepts connections on:

  - `#f` (the default) stands for the loopback address: _server_ can only be reached from the machine it is running on.
  - `#t` stands for all network interfaces of the machine, IPv4 and IPv6: _server_ can be reached from all machines that can reach this machine.
  - A string is an IPv4 or IPv6 address of the machine, e.g. `"192.168.1.20"`, `"0.0.0.0"` (all IPv4 interfaces) or `"::1"` (the IPv6 loopback address).

An error is signaled if _server_ cannot start listening, e.g. because _port_ is in use or because _address_ is not an address of the machine. _num_ is the number of worker threads used for processing requests (default is 3). _name_ is the prefix used for naming worker threads. _name_ followed by the worker thread number determines each worker threads name. The default for _name_ is `"worker "`. Procedure `http-server-start!` terminates only when the server is stopped, i.e. it is typically invoked on a thread to not block the execution of a program.

**(http-server-stop! _server_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Stops a running HTTP server _server_. All worker threads are terminated and procedure `http-server-start!`, which was used to start the server, returns. Stopping a server closes all its connections. If a request handler stops the server, the response of that request can therefore get lost. To make sure the response is sent, stop the server from a different thread after a short delay, as done in the example above.


## Server requests

### HTTP requests

**srv-request-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `srv-request` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all HTTP server request objects.

**(srv-request? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a HTTP server request object; `#f` otherwise.

**(srv-request-server _req_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the HTTP server which issued this HTTP server request object _req_. `srv-request-server` returns `#f` if _req_ was persisted and the corresponding HTTP server object was garbage collected. 

**(srv-request-method _req_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the HTTP method for the given HTTP server request _req_ as a string. The supported HTTP methods are: `"GET"`, `"HEAD"`, `"POST"`, `"PUT"`, `"DELETE"`, `"CONNECT"`, `"OPTIONS"`, `"TRACE"`, and `"PATCH"`.

**(srv-request-path _req_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(srv-request-path _req decode?_)**  

Returns the URL path of the given HTTP server request _req_ as a string. By default, the path is returned as sent by the client, i.e. it is percent-encoded (e.g. `/caf%C3%A9`). If _decode?_ is set to `#t`, the decoded path is returned (e.g. `/café`). A decoded path cannot distinguish `%2F` from a path separator, and it can contain arbitrary characters, including line breaks. It should not be decoded again; for instance, `%252e%252e` turns into `%2e%2e` when decoded once, and into `..` when decoded twice.

**(srv-request-query _req_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(srv-request-query _req incl-path?_)**  

Returns the URL query of the given HTTP server request _req_ as a string, exactly as sent by the client, including the leading `?`. If _req_ does not have a query, the empty string is returned. If _incl-path?_ is provided and set to true, then the query is prefixed with the path of _req_ (as returned by `srv-request-path`, i.e. as sent by the client).

**(srv-request-query-param _req name_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(srv-request-query-param _req name decode?_)**  

Returns the values of the query parameter _name_ (a string) for the given HTTP server request _req_ as a list of strings. If the query parameter _name_ was not used in _req_, the empty list is returned. By default, parameter names and values are compared and returned as sent by the client, i.e. percent-encoded and with `+` characters left as they are. If _decode?_ is set to `#t`, names and values are decoded: percent-encoded characters are replaced and `+` stands for a space.

**(srv-request-query-params _req_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(srv-request-query-params _req decode?_)**  

Returns all query parameters for the given HTTP server request _req_ as an association list consisting of string pairs, mapping query parameter names to query parameter values. By default, names and values are returned as sent by the client, i.e. percent-encoded and with `+` characters left as they are. If _decode?_ is set to `#t`, names and values are decoded: percent-encoded characters are replaced and `+` stands for a space.

**(srv-request-path-param _req name_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(srv-request-path-param _req name default_)**  
**(srv-request-path-param _req name default decode?_)**  

Returns the value of the path parameter _name_ (a string) for the given HTTP server request _req_ as a string. If the path parameter _name_ is undefined in _req_, _default_ is returned instead if provided. Otherwise, `#f` is returned.

Path parameters are determined when a request path gets matched with the available routes of an HTTP server. For details, see documentation for procedure `http-server-register!`. By default, i.e. if _decode?_ is not provided or set to `#f`, the value of a path parameter is returned as sent by the client, i.e. percent-encoded; the value of a path variable (`::pvar`) consists of the path segments joined with `/`. If _decode?_ is set to `#t`, the value is decoded. A decoded value can contain a `/` if the path of the request contained `%2F`; the decoded value of a path variable consists of the decoded path segments joined with `/`, so it cannot tell `%2F` from a path separator:

```scheme
;; Route "/files/::path", request path "/files/caf%C3%A9/a%2Fb"
(srv-request-path-param request "path")            ⇒  "caf%C3%A9/a%2Fb"
(srv-request-path-param request "path" #f #f)      ⇒  "caf%C3%A9/a%2Fb"
(srv-request-path-param request "path" #f #t)      ⇒  "café/a/b"
```

**(srv-request-path-params _req_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(srv-request-path-params _req decode?_)**  

Returns all path parameters for the given HTTP server request _req_ as an association list consisting of string pairs, mapping path parameter names to path parameter values. Path parameters are determined when a request path gets matched with the available routes of an HTTP server. For details, see documentation for procedure `http-server-register!`. By default, i.e. if _decode?_ is not provided or set to `#f`, the values are returned as sent by the client, i.e. percent-encoded. If _decode?_ is set to `#t`, the values of path parameters are decoded. See `srv-request-path-param` for details.

**(srv-request-path-param-set! _req name value_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the value of the path parameter _name_ (a string) for the given HTTP server request _req_ to string _value_. _value_ needs to be percent-encoded, like the values returned by `srv-request-path-param` by default. It is not possible to derive an encoded value from a decoded one, because a `/` and `%2F` have the same decoded value. The decoded value of the parameter, returned with _decode?_ set to `#t`, is derived from _value_. An error is signaled if _value_ is not a valid percent-encoded string; for example, a literal `%` needs to be encoded as `%25`.

**(srv-request-path-param-remove! _req name_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes the path parameter _name_ (a string) from the given HTTP server request _req_.

**(srv-request-header _req name_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(srv-request-header _req name default_)**  

Returns the header _name_ (a string) for the given HTTP server request _req_ as a string. If the header _name_ is undefined in _req_, _default_ is returned instead if provided. Otherwise, `#f` is returned.

**(srv-request-headers _req_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns all headers for the given HTTP server request _req_ as an association list consisting of string pairs, mapping header names to header values.

**(srv-request-header-set! _req name value_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the header _name_ (a string) for the given HTTP server request _req_ to string _value_.

**(srv-request-header-remove! _req name_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes the header _name_ (a string) from the given HTTP server request _req_.

**(srv-request-body _req_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the body of the HTTP server request _req_ as a bytevector.

**(srv-request-body-\>string _req_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the body of the HTTP server request _req_ as a UTF8-encoded string. If an interpretation of the body as a UTF8-encoded string fails, `#f` is returned.

**(srv-request-form-attributes _req_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Parses the body if its content type is `application/x-www-form-urlencoded` and returns the query parameters in the body as an association list mapping query names to query values. Names and values are decoded: percent-encoded characters are replaced and `+` stands for a space.

**(srv-request-form-multiparts _req_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Parses the body if its content type is `multipart/form-data` and returns a list of multi-part request objects of type `srv-multipart`. If the body is not a well-formed `multipart/form-data` body, e.g. because it has no closing boundary, then the empty list is returned.

**(srv-request-address _req_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the IP address of the client issueing the HTTP server request `req` as a string. If the IP address cannot be determind, `#f` is returned.


### HTTP multi-part requests

_HTTP multipart_, specifically _multipart/form-data_, is a media type that allows the encoding of information as a series of parts in a single message as defined by RFC 2388. This format is commonly used for forms that are expressed in HTML and where the form values are sent via HTTP.

**srv-multipart-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `srv-multipart` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all HTTP server multipart request objects.

**(srv-multipart? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a HTTP server multipart request object; `#f` otherwise.

**(srv-multipart-header _mp name_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(srv-multipart-header _mp name default_)**  

Returns the header _name_ (a string) for the given HTTP server multipart request _mp_ as a string. If the header _name_ is undefined in _mp_, _default_ is returned instead if provided. Otherwise, `#f` is returned.

**(srv-multipart-headers _mp_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns all headers for the given HTTP server multipart request _mp_ as an association list consisting of string pairs, mapping header names to header values.

**(srv-multipart-body _mp_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the body of the HTTP server multipart request _mp_ as a bytevector.

**(srv-multipart-body-\>string _mp_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the body of the HTTP server multipart request _mp_ as a UTF8-encoded string. If an interpretation of the body as a UTF8-encoded string fails, `#f` is returned.


## Server responses

### Generic API

**srv-response-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `srv-response` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all HTTP server response objects.

**(srv-response? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a HTTP server response object; `#f` otherwise.

**(make-srv-response)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-srv-response _status_)**  
**(make-srv-response _status headers_)**  
**(make-srv-response _status headers body_)**  
**(make-srv-response _status headers body ct_)**  

Returns a new HTTP server response based on the provided arguments. _status_ is a fixnum defining the status code (default: 200). _headers_ is an association list consisting of string pairs, mapping header names to header values. _body_ is a value (default: `#f`) which gets mapped to suitable response content with _ct_ being a string describing the MIME type for the content.

The following mapping rules are deriving the content of the response from value _body_:

  - `#f`: Sets an empty body.
  - String: The body is assigned the textual content of the string. If _last_ is provided, it is interpreted as a string defining the MIME type of the content. Otherwise, the MIME type is assumed to be `text/plain`.
  - Bytevector: The body is assigned the binary data of the bytevector. If _last_ is provided, it is interpreted as a string defining the MIME type of the content. Otherwise, the MIME type is assumed to be `application/octet-stream`.
  - Pair: _body_ is assumed to be in an SXML representation of HTML. The body of _res_ is assigned a textual HTML representation of _body_. If _last_ is provided and set to `#t` or if it is detected that the outermost markup of _body_ is not `html`, then the body is wrapped automatically in missing `html` and `body` markup. The MIME type is set to `text/html`.
  - `markdown`, `markdown-block`, `markdown-inline` object: The body is assigned an HTML representation of the given markdown object provided by library `(lispkit markdown)`. The MIME type is set to `text/html`.
  - `json`, `mutable-json` object: The body is assigned a textual JSON representation of the given JSON object provided by library `(lispkit json)`. The MIME type is set to `application/json`.
  - `styled-text` object: If _last_ is provided and set to true, the body is assigned an RTF representation (MIME type `application/rtf`) of the given styled text object provided by library `(lispkit styled-text)`. If _last_ is not provided or set to `#f`, the body is assigned an HTML representation (MIME type `text/html`) of the given styled text object.
  - `image` object: The body is assigned a binary representation of the bitmap image provided by library `(lispkit draw)`.  If _last_ is provided, it is interpreted as a string defining the MIME type of the content. Supported are `image/tiff`, `image/png`, `image/jpeg`, `image/gif`, and `image/bmp`. If _last_ is not provided, `image/png` is assumed to be the default.
  - All other data types are interpreted as Scheme objects representing JSON values. They are converted via procedure `json` into a JSON object and a textual representation of this JSON object is assigned to the body of _res_ with MIME type `application/json`. If the conversion fails, an error is returned.

**(srv-response-status-code _res_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the HTTP status code of the HTTP server response _res_.

**(srv-response-status-code-set! _res status_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the HTTP status code of the HTTP server response _res_ to _status_.

**(srv-response-header _res name_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(srv-response-header _res name default_)**  

Returns the header _name_ (a string) for the given HTTP server response _res_ as a string. If the header _name_ is undefined in _res_, _default_ is returned instead if provided. Otherwise, `#f` is returned.

**(srv-response-headers _res_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns all headers for the given HTTP server response _res_ as an association list consisting of string pairs, mapping header names to header values.

**(srv-response-header-set! _res name value_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the header _name_ (a string) for the given HTTP server response _res_ to string _value_.

**(srv-response-header-remove! _res name_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes the header _name_ (a string) from the given HTTP server response _res_.

**(srv-response-body-set! _res body_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(srv-response-body-set! _res body last_)**  

Sets the body of the HTTP server response _res_ to content derived from value _body_. The following mapping rules are used for the given value _body_:

  - `#f`: Sets an empty body.
  - String: The body is assigned the textual content of the string. If _last_ is provided, it is interpreted as a string defining the MIME type of the content. Otherwise, the MIME type is assumed to be `text/plain`.
  - Bytevector: The body is assigned the binary data of the bytevector. If _last_ is provided, it is interpreted as a string defining the MIME type of the content. Otherwise, the MIME type is assumed to be `application/octet-stream`.
  - Pair: _body_ is assumed to be in an SXML representation of HTML. The body of _res_ is assigned a textual HTML representation of _body_. If _last_ is provided and set to `#t` or if it is detected that the outermost markup of _body_ is not `html`, then the body is wrapped automatically in missing `html` and `body` markup. The MIME type is set to `text/html`.
  - `markdown`, `markdown-block`, `markdown-inline` object: The body is assigned an HTML representation of the given markdown object provided by library `(lispkit markdown)`. The MIME type is set to `text/html`.
  - `json`, `mutable-json` object: The body is assigned a textual JSON representation of the given JSON object provided by library `(lispkit json)`. The MIME type is set to `application/json`.
  - `styled-text` object: If _last_ is provided and set to true, the body is assigned an RTF representation (MIME type `application/rtf`) of the given styled text object provided by library `(lispkit styled-text)`. If _last_ is not provided or set to `#f`, the body is assigned an HTML representation (MIME type `text/html`) of the given styled text object.
  - `image` object: The body is assigned a binary representation of the bitmap image provided by library `(lispkit draw)`.  If _last_ is provided, it is interpreted as a string defining the MIME type of the content. Supported are `image/tiff`, `image/png`, `image/jpeg`, `image/gif`, and `image/bmp`. If _last_ is not provided, `image/png` is assumed to be the default.
  - All other data types are interpreted as Scheme objects representing JSON values. They are converted via procedure `json` into a JSON object and a textual representation of this JSON object is assigned to the body of _res_ with MIME type `application/json`. If the conversion fails, an error is returned.

**(srv-response-body-html-set! _res str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(srv-response-body-html-set! _res str just-body?_)**  

Sets the body of the HTTP server response _res_ to string _str_ representing HTML content. If _just-body?_ is provided and set to `#t`, it is assumed that _str_ only represents the body of a HTML document and `srv-response-body-html-set!` will automatically decorate _str_ such that it represents a full HTML document.

### Common responses

**(srv-response-ok _body_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>   
**(srv-response-ok _headers body_)**  

Returns a HTTP server response for status code 200 (= `OK`). This is quivalent to `(make-srv-response 200 headers body)`.

**(srv-response-bad-request)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(srv-response-bad-request _body_)**  
**(srv-response-bad-request _headers body_)**  

Returns a HTTP server response for status code 400 (= `Bad Request`). This is quivalent to `(make-srv-response 400 headers body)`.

**(srv-response-unauthorized)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(srv-response-unauthorized _body_)**  
**(srv-response-unauthorized _headers body_)**  

Returns a HTTP server response for status code 401 (= `Unauthorized`). This is quivalent to `(make-srv-response 401 headers body)`.

**(srv-response-forbidden)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(srv-response-forbidden _body_)**  
**(srv-response-forbidden _headers body_)**  

Returns a HTTP server response for status code 403 (= `Forbidden`). This is quivalent to `(make-srv-response 403 headers body)`.

**(srv-response-not-found)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(srv-response-not-found _body_)**  
**(srv-response-not-found _headers body_)**  

Returns a HTTP server response for status code 404 (= `Not Found`). This is quivalent to `(make-srv-response 404 headers body)`.

**(srv-response-method-not-allowed)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(srv-response-method-not-allowed _body_)**  
**(srv-response-method-not-allowed _headers body_)**  

Returns a HTTP server response for status code 405 (= `Not Allowed`). This is quivalent to `(make-srv-response 405 headers body)`.

**(srv-response-not-acceptable)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(srv-response-not-acceptable _body_)**  
**(srv-response-not-acceptable _headers body_)**  

Returns a HTTP server response for status code 406 (= `Not Acceptable`). This is quivalent to `(make-srv-response 406 headers body)`.

**(srv-response-internal-server-error)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(srv-response-internal-server-error _body_)**  
**(srv-response-internal-server-error _headers body_)**  

Returns a HTTP server response for status code 500 (= `Internal Server Error`). This is quivalent to `(make-srv-response 500 headers body)`.

**(srv-response-not-implemented)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(srv-response-not-implemented _body_)**  
**(srv-response-not-implemented _headers body_)**  

Returns a HTTP server response for status code 501 (= `Not Implemented`). This is quivalent to `(make-srv-response 501 headers body)`.

**(srv-response-created)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a HTTP server response for status code 201 (= `Created`). This is quivalent to `(make-srv-response 201 #f #f)`.

**(srv-response-accepted)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a HTTP server response for status code 202 (= `Accepted`). This is quivalent to `(make-srv-response 202 #f #f)`.

**(srv-response-moved-permanently _redirect_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(srv-response-moved-permanently _redirect headers_)**  

Returns a HTTP server response for status code 301 (= `Moved Permanently`). This is quivalent to `(make-srv-response 301 (cons (cons "Location" redirect) headers) #f)`.

**(srv-response-moved-temporarily _redirect_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(srv-response-moved-temporarily _redirect headers_)**  

Returns a HTTP server response for status code 307 (= `Temporary Redirect`). The client is asked to repeat the request, with the same method and body, at the new location. This is equivalent to `(make-srv-response 307 (cons (cons "Location" redirect) headers) #f)`.


## Utilities

**(parse-http-header-value _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Parses the header value string _str_ into a list of strings and string pairs using a universal header parsing algorithm.

```scheme
(parse-http-header-value "a, b, c")
  ⇒  (("a") ("b") ("c"))
(parse-http-header-value "a; b; c")
  ⇒  (("a" "b" "c"))
(parse-http-header-value "a, b1 ; b2, c")
  ⇒  (("a") ("b1" "b2") ("c"))
(parse-http-header-value "a; b=one; c=two")
  ⇒   (("a" ("b" . "one") ("c" . "two")))
(parse-http-header-value
  "foo/bar;p=\"A,B,C\", bob/dole;x=\"apples,oranges\"")
  ⇒  (("foo/bar" ("p" . "A,B,C")) ("bob/dole" ("x" . "apples,oranges")))
```

**(http-header-param _headers name_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Extracts the header value for header _name_ from the association list _headers_, or returns `#f` if _name_ is not contained in _headers_. _headers_ is an association list of string pairs, mapping header names to header values.

```scheme
(http-header-param '(("one" . "1")("Two" . "2")) "TWO")
  ⇒  "2"
(http-header-param '(("one" . "1")("Two" . "2")) "Three")
  ⇒  #f
```

**(share-file-handler _filepath_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a HTTP request handler for downloading a file at the given _filepath_, an absolute file path. The handler sends the file with `ETag` and `Last-Modified` headers, and supports conditional requests and range requests.

**(share-directory-handler _root_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a HTTP request handler for downloading a file at the file path consisting of the first path variable of the request which is considered to be relative to absolute directory path _root_. The handler needs to be registered for a route ending in a path variable (e.g. `"/public/::path"`). Files are sent like by `share-file-handler`. A request for a directory is redirected to the same path with a `/` appended and then answered with the file `index.html` or `default.html` of the directory, if there is one.

Requests which contain `..` segments or NUL characters, or which refer to files outside of _root_ via symbolic links, are refused with status 403. This also applies to encoded forms such as `%2e%2e`. Requests for files which do not exist are answered with status 404.

**(browse-directory-handler _root_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a HTTP request handler for browsing the files in the directory consisting of the first path variable of the request which is considered to be relative to absolute directory path _root_. The handler needs to be registered for a route ending in a path variable (e.g. `"/files/::path"`). Directories are answered with an HTML page listing their entries, files are sent like by `share-file-handler`. The same restrictions as for `share-directory-handler` apply.
