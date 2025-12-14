# LispKit Iterate

Library `(lispkit iterate)` defines syntactical forms supporting frequently used iteration patterns. Some of the special forms were inspired by Common Lisp.


**(dotimes (_var count_) _body ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>   
**(dotimes (_var count result_) _body ..._)**   

`dotimes` iterates variable _var_ over the integer range _[0, count[_, executing _body_ for every iteration.

`dotimes` first evaluates _count_, which has to evaluate to a fixnum. If _count_ evaluates to zero or a negative number, _body ..._ is not executed. `dotimes` then executes _body ..._ once for each integer from 0 up to, but not including, the value of _count_, with _var_ bound to each integer. Then, _result_ is evaluated and its value is returned as the value of the `dotimes` form. If _result_ is not provided, no value is being returned.

```scheme
(let ((res 0))
  (dotimes (i 10 res)
    (set! res (+ res i))))  ⟹  45
```

**(dolist (_var lst_) _body ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>   
**(dolist (_var lst result_) _body ..._)**   

`dolist` iterates variable _var_ over the elements of list _lst_, executing _body ..._ for every iteration. 

`dolist` first evaluates _lst_, which has to evaluate to a list. It then executes _body ..._ once for each element in the list, with _var_ bound to the current element of the list. Then, _result_ is evaluated and its value is returned as the value of the `dolist` form. If _result_ is not provided, no value is being returned.

```scheme
(let ((res ""))
  (dolist (x '("a" "b" "c") res)
    (set! res (string-append res x))))  ⟹  "abc"
```

**(loop _break body ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>   

`loop` iterates infinitely, executing _body ..._ in each iteration. _break_ is a variable bound to an exit function which can be used to leave the `loop` form. _break_ receives one argument which is the result of the `loop` form.

```scheme
(let ((i 1))
  (loop break
    (if (> i 100)
        (break i)
        (set! i (* i 2)))))  ⟹  128
```

**(while _condition body ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>   
**(while _condition_ unless _break body ..._)**   

`while` iterates as long as _condition_ evaluates to a value other than `#f`, executing _body ..._ in each iteration. `unless` can be used to bind an exit funtion to variable _break_ so that it is possible to leave the loop by calling thunk _break_. `while` forms never return a result.

```scheme
(let ((i 0)(sum 0))
  (while (< sum 100) unless exit
    (if (> i 10) (exit))
    (set! sum (+ sum i))
    (set! i (fx1+ i)))
  (cons i sum))  ⟹  (11 . 55)
```

**(for _var_ from _lo_ to _hi body ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>   
**(for _var_ from _lo_ to _hi_ step _s body ..._)**   

This form of `for` iterates through all the fixnums from _lo_ to _hi_ (both inclusive), executing _body ..._ in each iteration. If step _s_ is provided, _s_ is used as the increment of variable _var_ which iterates through the elements of the given range.

When this `for` form is being executed, first _lo_ and _hi_ are evaluated. Both have to evaluate to a fixnum. Then, _body ..._ is executed once for each integer in the given range, with _var_ bound to the current integer. The form returns no result.

```scheme
(let ((res '()))
  (for x from 1 to 16 step 2
    (set! res (cons x res)))
  res)  ⟹  (15 13 11 9 7 5 3 1)
```

**(for _var_ in _lst body ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>   
**(for _var_ in _lst_ where _condition body ..._)**   
**(for _var_ from _(x ...) body ..._)**   

This form of `for` iterates through all the elements of a list, executing _body ..._ in each iteration. The list is either explicitly given via _lst_ or its elements are enumerated in the form _(x ...)_. If a _where_ predicate is provided, the it acts as a filter on the elements through which variable _var_ is iterated.

When this `for` form is being executed, first _lst_ or _(x ...)_ is evaluated. Then, _body ..._ is executed once for each element in the list, with _var_ bound to the current element of the list. The form returns no result.

```scheme
(let ((res '()))
  (for x in (iota 16) where (odd? x)
    (set! res (cons x res)))
  res)  ⟹  (15 13 11 9 7 5 3 1)
```

**(exit-with _break body ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>   
**(exit-with _break_ from _body ..._)**   

`exit-with` is not an iteration construct by itself. It is often used in combination with iteration constructs to declare an exit function for leaving statements _body ..._. _break_ is a variable which gets bound to the exit function in the scope of statements _body ..._. `exit-with` either returns the result of the last statement of _body ..._ or it returns the value passed to _break_ in case the exit function gets called.

```scheme
(exit-with break
  (display "hello")
  (break #f)
  (display "world"))  ⟹  #f  ; printing "hello"
```
