# LispKit Debug

Library `(lispkit debug)` provides utilities for debugging code. Available are procedures for measuring execution latencies, for tracing procedure calls, for expanding macros, for disassembling code, as well as for inspecting the execution environment.

## Timing execution

**(time _expr_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

`time` compiles _expr_ and executes it. The form displays the time it took to execute _expr_ as a side-effect. It returns the result of executing _expr_.

**(time-values _expr_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

`time-values` executes _expr_. If _expr_ evaluates to _n_ values `x1, ..., xn`, `time-values` returns _n + 1_ values `t, x1, ..., xn` where `t` is the time it takes to evaluate _expr_.

## Tracing procedure calls

**(trace-calls)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(trace-calls _level_)**  

This function is used to enable/disable call tracing. When call tracing is enabled, all function calls that are executed by the virtual machine are being printed to the console. Call tracing operates at three levels:

   - `0`: Call tracing is switched off
   - `1`: Call tracing is enabled only for procedures for which it is enabled (via function `set-procedure-trace!`)
   - `2`: Call tracing is switched on for all procedures (independent of procedure-level tracing being enabled or disabled)

`(trace-calls n)` will set call tracing to level `n`. If the level is ommitted, `trace-calls` will return the current call tracing level.

For instance, if call tracing is enabled via `(trace-calls 2)`, executing `(fib 3)` will print the following call trace.

```scheme
> (define (fib n)
    (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
> (trace-calls 2)
> (fib 2)
  ↪︎ (fib 2) in <repl>
    ⟶ (< 2 2) in fib
    ⟵ #f from <
    ⟶ (- 2 1) in fib
    ⟵ 1 from -
    ⟶ (fib 1) in fib
      ⟶ (< 1 2) in fib
      ⟵ #t from < in fib
    ⟵ 1 from fib in fib
    ⟶ (- 2 2) in fib
    ⟵ 0 from -
    ⟶ (fib 0) in fib
      ⟶ (< 0 2) in fib
      ⟵ #t from < in fib
    ⟵ 0 from fib in fib
  ↪︎ (+ 1 0) in fib
  ⟵ 1 from fib
1
```

Function invocations are prefixed with `⟶`,  or `↪` if it's a tail call. The value returned by a function call is prefixed with `⟵`.

**(procedure-trace? _proc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#f` if procedure-level call tracing is disabled for _proc_, `#t` otherwise.

**(set-procedure-trace! _proc trace?_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Enables procedure-level call tracing for procedure _proc_ if _trace?_ is set to `#t`. It disables call tracing for _proc_ if _trace?_ is `#f`.

## Macro expansion

**(quote-expanded _expr_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

`quote-expanded` is syntax for macro-expanding expression _expr_ in the current syntactical environment. Macro-expansion is applied consecutively as long as the top-level can be expanded further.

```scheme
(quote-expanded (assert (+ 1 2)))
⇒  (if (not (+ 1 2))
       (assertion (quote (+ 1 2))))
```

**(quote-expanded-1 _expr_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

`quote-expanded-1` is syntax for macro-expanding expression _expr_ in the current syntactical environment. Macro-expansion is applied at most once, even if the top-level can be expanded further.

```scheme
(quote-expanded-1 (for x in '(1 2 3) (display x)))
⇒  (dolist (x (quote (1 2 3))) (display x))
```

**(macroexpand _expr_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(macroexpand _expr env_)**  

Procedure `macroexpand` applies macro-expansion to the expression _expr_ in the environment _env_ as long as the expression on its top-level can be expanded further. If _env_ is not provided, the current interaction environment is used.

```scheme
(macroexpand
  '(dotimes (x (+ 2 2)) (display x) (newline)))
⇒  (do ((maxvar (+ 2 2))
        (x 0 (fx1+ x)))
       ((fx>= x maxvar))
     (display x)
     (newline))
```

**(macroexpand-1 _expr_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(macroexpand-1 _expr env_)**  

Procedure `macroexpand-1` applies macro-expansion to the expression _expr_ in the environment _env_ at most once. The resulting expression might therefore only be partially expanded at the top-level. If _env_ is not provided, the current interaction environment is used.

```scheme
(macroexpand-1 '(for x in '(1 2 3) (display x)))
⇒  (dolist (x (quote (1 2 3))) (display x))
(macroexpand-1
  (macroexpand-1
    '(for x in '(1 2 3) (display x))))
⇒  (let ((x (quote ()))
         (ys (quote (1 2 3))))
     (if (null? ys)
         (void)
         (do ((xs ys (cdr xs)))
             ((null? xs))
           (set! x (car xs))
           (display x))))
```

## Disassembling code

**(compile _expr_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(compile _expr env_)**  

Compiles expression _expr_ in environment _env_ and displays the disassembled code. If _env_ is not given, the current interaction environment is used. This is what is being printed when executing `(compile '(do ((i 0 (fx1+ i)))((fx> i 10))(display i)(newline)))`:

```
CONSTANTS:
    0: #<procedure display>
    1: #<procedure newline>
INSTRUCTIONS:
    0: alloc 1
    1: push_fixnum 0
    2: make_local_variable 0
    3: push_local_value 0
    4: push_fixnum 10
    5: fx_gt
    6: branch_if 14                    ;; jump to 20
    7: make_frame
    8: push_constant 0                 ;; #<procedure display>
    9: push_local_value 0
   10: call 1
   11: pop
   12: make_frame
   13: push_constant 1                 ;; #<procedure newline>
   14: call 0
   15: pop
   16: push_local_value 0
   17: fx_inc
   18: set_local_value 0
   19: branch -16                      ;; jump to 3
   20: push_void
   21: reset 0, 1
   22: return
```

**(disassemble _proc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Disassembles procedure _proc_ and prints out the code. This is what is being printed when executing `(disassemble caddr)`:

```
CONSTANTS:
INSTRUCTIONS:
    0: assert_arg_count 1
    1: push_global 426
    2: make_frame
    3: push_global 431
    4: push_local 0
    5: call 1
    6: tail_call 1
```

## Execution environment

**(gc)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Force garbage collection to be performed.

**(available-symbols)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of all symbols that have been used so far.

**(loaded-libraries)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of all libraries that have been loaded so far.

```scheme
> (loaded-libraries)
((lispkit draw) (lispkit base) (lispkit port) (lispkit control) (lispkit type) (lispkit list) (lispkit string) (lispkit math) (lispkit date-time) (lispkit dynamic) (lispkit char-set) (lispkit bytevector) (lispkit char) (lispkit vector) (lispkit regexp) (lispkit record) (lispkit hashtable) (lispkit system) (lispkit core) (lispkit gvector) (lispkit box))
```

**(loaded-sources)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of all sources that have been loaded.

**(environment-info)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Prints out debug information about the current execution environment (mostly relevant for developing LispKit).

**(stack-size)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of elements that are currently on the stack.

**(call-stack-procedures)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of procedures that are currently in process of being executed in the current thread.

**(call-stack-trace)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of procedure calls that are currently in process of being executed in the current thread. The result is a list of lists, where each element corresponds to an active procedure call (with the given parameters, where this can be reconstructed).

**(set-max-call-stack! _n_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

When exceptions and errors are created, a call stack trace is attached to them. Since these can be quite large, call stack traces are capped at the top-most _n_ entries. _n_ can be at most 1000, default is _20_.

**(internal-call-stack)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of strings, each representing a native function that is currently being executed internally.
