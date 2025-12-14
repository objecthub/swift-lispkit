# LispKit Dynamic

## Dynamic bindings

**(make-parameter _init_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-parameter _init converter_)**  

Returns a newly allocated parameter object, which is a procedure that accepts zero arguments and returns the value associated with the parameter object. Initially, this value is the value of `(`_converter init_`)`, or of _init_ if the conversion procedure _converter_ is not specified. The associated value can be temporarily changed using `parameterize`. The default associated value can be changed by invoking the parameter object as a function with the new value as the only argument.

Parameter objects can be used to specify configurable settings for a computation without the need to pass the value to every procedure in the call chain explicitly.

**(parameterize ((_param value_) ...) _body_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

A `parameterize` expression is used to change the values returned by specified parameter objects _param_ during the evaluation of _body_.
The _param_ and _value_ expressions are evaluated in an unspecified order. The _body_ is evaluated in a dynamic environment in which calls to the parameters return the results of passing the corresponding values to the conversion procedure specified when the parameters were created. Then the previous values of the parameters are restored without passing them to the conversion procedure. The results of the last expression in the _body_ are returned as the results of the entire `parameterize` expression.

```scheme
(define radix
  (make-parameter 10 (lambda (x)
                       (if (and (exact-integer? x) (<= 2 x 16))
                           x
                           (error "invalid radix")))))
(define (f n) (number->string n (radix)))
(f 12)                              ⇒  "12"
(parameterize ((radix 2)) (f 12))   ⇒  "1100"
(f 12)                              ⇒  "12"
(radix 16)
(parameterize ((radix 0)) (f 12))   ⇒  error: invalid radix
```

**(make-dynamic-environment)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

Returns a newly allocated copy of the current dynamic environment. Dynamic environments are represented as mutable hashtables.

**(dynamic-environment)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

Returns the current dynamic environment represented as mutable hashtables.

**(set-dynamic-environment! _hashtable_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

Sets the current dynamic environment to the given dynamic environment object. Dynamic environments are modeled as hashtables.

## Continuations

**(continuation? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a continuation procedure, `#f` otherwise.

**(call-with-current-continuation _proc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(call/cc _proc_)**  

The procedure `call-with-current-continuation` (or its equivalent abbreviation `call/cc`) packages the current continuation as an “escape procedure” and passes it as an argument to _proc_. It is an error if _proc_ does not accept one argument.

The escape procedure is a Scheme procedure that, if it is later called, will abandon whatever continuation is in effect at that later time and will instead use the continuation that was in effect when the escape procedure was created. Calling the escape procedure will cause the invocation of before and after thunks installed using `dynamic-wind`.

The escape procedure accepts the same number of arguments as the continuation to the original call to `call-with-current-continuation`. Most continuations take only one value. Continuations created by the `call-with-values` procedure (including the initialization expressions of `define-values`, `let-values`, and `let*-values` expressions), take the number of values that the consumer expects. The continuations of all non-final expressions within a sequence of expressions, such as in `lambda`, `case-lambda`, `begin`, `let`, `let*`, `letrec`, `letrec*`, `let-values`, `let*-values`, `let-syntax`, `letrec-syntax`, `parameterize`, `guard`, `case`, `cond`, `when`, and `unless` expressions, take an arbitrary number of values because they discard the values passed to them in any event. The effect of passing no val- ues or more than one value to continuations that were not created in one of these ways is unspecified.

The escape procedure that is passed to proc has unlimited extent just like any other procedure in Scheme. It can be stored in variables or data structures and can be called as many times as desired. However, like the `raise` and `error` procedures, it never returns to its caller.

The following examples show only the simplest ways in which `call-with-current-continuation` is used. If all real uses were as simple as these examples, there would be no need for a procedure with the power of `call-with-current-continuation`.

```scheme
(call-with-current-continuation
  (lambda (exit)
    (for-each (lambda (x) (if (negative? x) (exit x)))
              '(54 0 37 -3 245 19)) #t))  ⇒  -3
(define list-length
  (lambda (obj)
    (call-with-current-continuation
      (lambda (return)
        (letrec
          ((r (lambda (obj)
                (cond ((null? obj) 0)
                      ((pair? obj) (+ (r (cdr obj)) 1))
                      (else        (return #f))))))
          (r obj))))))
(list-length '(1 2 3 4))     ⇒  4
(list-length '(a b . c))     ⇒  #f
```

**(dynamic-wind _before thunk after_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Calls _thunk_ without arguments, returning the result(s) of this call. _before_ and _after_ are called, also without arguments, as required by the following rules. Note that, in the absence of calls to continuations captured using `call-with-current-continuation`, the three arguments are called once each, in order. _before_ is called whenever execution enters the dynamic extent of the call to _thunk_ and _after_ is called whenever it exits that dynamic extent. The dynamic extent of a procedure call is the period between when the call is initiated and when it returns. The _before_ and _after_ thunks are called in the same dynamic environment as the call to `dynamic-wind`. In Scheme, because of `call-with-current-continuation`, the dynamic extent of a call is not always a single, connected time period. It is defined as follows:

  - The dynamic extent is entered when execution of the body of the called procedure begins.
  - The dynamic extent is also entered when execution is not within the dynamic extent and a continuation is invoked that was captured (using `call-with-current-continuation`) during the dynamic extent.
  - It is exited when the called procedure returns.
  - It is also exited when execution is within the dynamic extent and a continuation is invoked that was captured while not within the dynamic extent.

If a second call to `dynamic-wind` occurs within the dynamic extent of the call to _thunk_ and then a continuation is invoked in such a way that the _afters_ from these two invocations of `dynamic-wind` are both to be called, then the _after_ associated with the second (inner) call to `dynamic-wind` is called first.

If a second call to `dynamic-wind` occurs within the dynamic extent of the call to _thunk_ and then a continuation is invoked in such a way that the _befores_ from these two invocations of `dynamic-wind` are both to be called, then the before associated with the first (outer) call to `dynamic-wind` is called first.

If invoking a continuation requires calling the _before_ from one call to `dynamic-wind` and the after from another, then the _after_ is called first.

The effect of using a captured continuation to enter or exit the dynamic extent of a call to _before_ or _after_ is unspecified.

```scheme
(let ((path ’())
      (c #f))
  (let ((add (lambda (s)
        (set! path (cons s path)))))
    (dynamic-wind
      (lambda () (add 'connect))
      (lambda () (add (call-with-current-continuation
                   (lambda (c0) (set! c c0) 'talk1))))
      (lambda () (add 'disconnect)))
    (if (< (length path) 4)
        (c 'talk2)
        (reverse path))))
  ⇒  (connect talk1 disconnect connect talk2 disconnect)
```

**(unwind-protect _body cleanup ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

Executes expression _body_ guaranteeing that statements _cleanup ..._ are executed when _body_'s execution is finished or when an exception is thrown during the execution of _body_. `unwind-protect` returns the result of executing _body_.

**(return _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns to the top-level of the read-eval-print loop with _obj_ as the result (or terminates the program with _obj_ as its return value).

## Exceptions

**(with-exception-handler _handler thunk_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `with-exception-handler` procedure returns the results of invoking _thunk_. _handler_ is installed as the current exception handler in the dynamic environment used for the invocation of _thunk_. It is an error if _handler_ does not accept one argument. It is also an error if _thunk_ does not accept zero arguments.

```scheme
(call-with-current-continuation
  (lambda (k)
    (with-exception-handler
      (lambda (x)
        (display "condition: ")(write x)(newline)(k 'exception))
      (lambda ()
        (+ 1 (raise 'an-error))))))  ⇒ exception; prints "condition: an-error"
(with-exception-handler
  (lambda (x) (display "something went wrong\n"))
  (lambda () (+ 1 (raise 'an-error))))   ⇒ prints "something went wrong"
```

After printing, the second example then raises another exception: "exception handler returned".

**(try _thunk_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(try _thunk handler_)**  

`try` executes argument-less procedure _thunk_ and returns the result as the result of `try` if _thunk_'s execution completes normally. If an exception is thrown, procedure _handler_ is called with the exception object as its argument. The result of executing _handler_ is returned by _try_.

**(guard (_var cond-clause ..._) _body_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

The _body_ is evaluated with an exception handler that binds the raised object to _var_ and, within the scope of that binding, evaluates the clauses as if they were the clauses of a `cond` expression. That implicit `cond` expression is evaluated with the continuation and dynamic environment of the `guard` expression. If every _cond-clause_’s _test_ evaluates to `#f` and there is no "else" clause, then `raise-continuable` is invoked on the raised object within the dynamic environment of the original call to `raise` or `raise-continuable`, except that the current exception handler is that of the `guard` expression.

Please note that each _cond-clause_ is as in the specification of `cond`.

```scheme
(guard (condition
         ((assq ’a condition) => cdr)
         ((assq ’b condition)))
  (raise (list (cons ’a 42))))         ⇒  42
(guard (condition
         ((assq ’a condition) => cdr)
         ((assq ’b condition)))
  (raise (list (cons ’b 23))))         ⇒ (b . 23)
```

**(make-error _message irrlist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a newly allocated custom error object consisting of _message_ as its error message and the list of irritants _irrlist_.

**(make-assertion-error _procname expr_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a newly allocated assertion error object referring to a procedure of name _procname_ and an expression _expr_ which triggered the assertion. Assertion errors that were raised should never be caught as they indicate a violation of an invariant.

**(raise _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Raises an exception by invoking the current exception handler on _obj_. The handler is called with the same dynamic environment as that of the call to `raise`, except that the current exception handler is the one that was in place when the handler being called was installed. If the handler returns, a secondary exception is raised in the same dynamic environment as the handler. The relationship between _obj_ and the object raised by the secondary exception is unspecified.

**(raise-continuable _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Raises an exception by invoking the current exception handler on _obj_. The handler is called with the same dynamic environment as the call to `raise-continuable`, except that: (1) the current exception handler is the one that was in place when the handler being called was installed, and (2) if the handler being called returns, then it will again become the current exception handler. If the handler returns, the values it returns become the values returned by the call to `raise-continuable`.

```scheme
(with-exception-handler
  (lambda (con)
    (cond ((string? con) (display con))
          (else          (display "a warning has been issued")))
    42)
  (lambda ()
    (+ (raise-continuable "should be a number") 23)))
prints: should be a number
  ⇒  65
```

**(error _message obj ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Raises an exception as if by calling `raise` on a newly allocated error object which encapsulates the information provided by _message_, as well as any _obj_, known as the irritants. The procedure `error-object?` must return `#t` on such objects. _message_ is required to be a string.

```scheme
(define (null-list? l)
  (cond ((pair? l) #f)
        ((null? l) #t)
        (else (error "null-list?: argument out of domain" l))))
```

**(assertion _expr_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Raises an exception as if by calling `raise` on a newly allocated assertion error object encapsulating _expr_ as the expression which triggered the assertion failure and the current procedure's name. Assertion errors that are raised via `assertion` should never be caught as they indicate a violation of a critical invariant.

```scheme
(define (null-list? l)
  (cond ((pair? l) #f)
        ((null? l) #t)
        (else (assertion '(list? l)))))
```

**(assert _expr0 expr1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

Executes _expr0_, _expr1_, ... in the given order and raises an assertion error as soon as the first expression is evaluating to `#f`. The raised assertion error encapsulates the expression that evaluated to `#f` and the name of the procedure in which the `assert` statement was placed.

```scheme
(define (drop-elements xs n)
  (assert (list? xs) (fixnum? n) (not (negative? n)))
  (if (or (null? xs) (zero? n)) xs (drop-elements (cdr xs) (fx1- n))))
```

**(error-object? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an error object, `#f` otherwise. Error objects are either implicitly created via `error` or they are created explicitly with procedure `make-error`.

**(error-object-message _err_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(error-object-message _err template?_)**  

Returns the message (which is a string) encapsulated by the error object _err_ with placeholders getting expanded. If _template?_ is provided and set to `#t`, placeholders are not getting expanded, i.e. the message that is returned is the error message template string.

**(error-object-irritants _err_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the irritants encapsulated by the error object _err_.

**(error-object-stacktrace _err_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of procedures representing the stack trace encapsulated by the error object _err_. The stack trace reflects the currently active procedures at the time the error object was created (either implicitly via `error` or explicitly via `make-error`).

**(error-object-\>string _err_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(error-object-\>string _err expanded?_)**  

Returns a string representation of _err_ for the purpose of displaying a description of the error. If _expanded?_ is provided and set to `#t`, then a more comprehensive description is returned. If _expanded?_ is set to symbol `printable`, an even more comprehensive description is returned.

**(read-error? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

This error type predicate returns `#t` if _obj_ is an error object raised by the `read` procedure; otherwise, it returns `#f`.

**(file-error? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

This error type predicate returns `#t` if _obj_ is an error object raised by the inability to open an input or output port on a file; otherwise, it returns `#f`.

## Exiting

**(exit)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(exit _obj_)**  

Runs all outstanding `dynamic-wind` after procedures, terminates the running program, and communicates an exit value to the operating system. If no argument is supplied, or if _obj_ is `#t`, the `exit` procedure should communicate to the operating system that the program exited normally. If _obj_ is `#f`, the `exit` procedure will communicate to the operating system that the program exited abnormally. Otherwise, `exit` should translate _obj_ into an appropriate exit value for the operating system, if possible.
The `exit` procedure must not signal an exception or return to its continuation.

**(emergency-exit)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(emergency-exit _obj_)**  

Terminates the program without running any outstanding `dynamic-wind` "after procedures" and communicates an exit value to the operating system in the same manner as `exit`.
