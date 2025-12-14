# LispKit Control

## Sequencing

**(begin _expr ... exprn_)** <span style="float:right;text-align:rigth;">[syntax]</span>   

`begin` evaluates _expr_, ..., _exprn_ sequentially from left to right. The values of the last expression _exprn_ are returned. This special form is typically used to sequence side effects such as assignments or input and output.


## Conditionals

**(if _test consequent_)** <span style="float:right;text-align:rigth;">[syntax]</span>   
**(if _test consequent alternate_)**

An `if` expression is evaluated as follows: first, expression _test_ is evaluated. If it yields a true value, then expression _consequent_ is evaluated and its values are returned. Otherwise, _alternate_ is evaluated and its values are returned. If expression _test_ yields a false value and no _alternate_ expression is specified, then the result of the expression is _void_.

```scheme
(if (> 3 2) 'yes 'no)         ⇒ yes
(if (> 2 3) 'yes 'no)         ⇒ yes
(if (> 3 2) (- 3 2) (+ 3 2))  ⇒ 1
```

**(when _test consequent ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>   

The _test_ expression is evaluated, and if it evaluates to a true value, the expressions _consequent ..._ are evaluated in order. The result of the `when` expression is the value to which the last _consequent_ expression evaluates or _void_ if _test_ evaluates to false.

```scheme
(when (= 1 1.0)
  (display "1")
  (display "2"))  ⇒ (void), prints: 12
```

**(unless _test alternate ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>   

The _test_ is evaluated, and if it evaluates to false, the expressions _alternate ..._ are evaluated in order. The result of the `unless` expression is the value to which the last _consequent_ expression evaluates or _void_ if _test_ evaluates to a true value.

```scheme
(unless (= 1 1.0)
  (display "1")
  (display "2"))  ⇒ (void), prints nothing
```

**(cond _clause1 clause2 ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  

Clauses like _clause1_ and _clause2_ take one of two forms, either

   - `(_test expr1 ..._)`, or
   - `(_test_ => _expr_)`

The last clause in a `cond` expression can be an "else clause", which has the form

   - `(else _expr1 expr2 ..._)`

A `cond` expression is evaluated by evaluating the _test_ expressions of successive clauses in order until one of them evaluates to a true value. When a _test_ expression evaluates to a true value, the remaining expressions in its clause are evaluated in order, and the results of the last expression are returned as the results of the entire `cond` expression.

If the selected _clause_ contains only the _test_ and no expressions, then the value of the _test_ expression is returned as the result of the `cond` expression. If the selected clause uses the `=>` alternate form, then the expression is evaluated. It is an error if its value is not a procedure that accepts one argument. This procedure is then called on the value of the _test_ and the values returned by this procedure are returned by the `cond` expression.

If all tests evaluate to `#f`, and there is no `else` clause, then the result of the conditional expression is _void_. If there is an `else` clause, then its expressions are evaluated in order, and the values of the last one are returned.

```scheme
(cond ((> 3 2) ’greater)
      ((< 3 2) ’less))     ⇒ greater

(cond ((> 3 3) ’greater)
      ((< 3 3) ’less)
      (else ’equal))       ⇒ equal

(cond ((assv ’b ’((a 1) (b 2))) => cadr)
      (else #f))           ⇒ 2
```

**(case _key clause1 clause2 ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  

_key_ can be any expression. Each clause _clause1, clause2, ..._ has the form:

   - `((_datum1 ..._) _expr1 expr2 ..._)`

where each _datum_ is an external representation of some object. It is an error if any of the datums are the same anywhere in the expression. Alternatively, a clause can be of the form:

   - `((_datum1 ..._) => _expr_)`

The last clause in a `case` expression can be an "else clause", which has one of the following forms:

   - `(else _expr1 expr2 ..._)`, or
   - `(else => _expr_)`

A case expression is evaluated as follows. Expression _key_ is evaluated and its result is compared against each _datum_. If the result of evaluating _key_ is the same, in the sense of `eqv?`, to a _datum_, then the expressions in the corresponding clause are evaluated in order and the results of the last expression in the clause are returned as the results of the `case` expression.

If the result of evaluating _key_ is different from every _datum_, then if there is an `else` clause, its expressions are evaluated and the results of the last are the results of the `case` expression. Otherwise, the result of the `case` expression is _void_.

If the selected _clause_ or `else` clause uses the `=>` alternate form, then the expression is evaluated. It is an error, if its value is not a procedure accepting one argument. This procedure is then called on the value of the _key_ and the values returned by this procedure are returned by the `case` expression.

```scheme
(case (* 2 3)
  ((2 3 5 7) ’prime)
  ((1 4 6 8 9) ’composite))   ⇒ composite

(case (car ’(c d))
  ((a) ’a)
  ((b) ’b))                   ⇒ (void)

(case (car ’(c d))
  ((a e i o u) ’vowel)
  ((w y) ’semivowel)
  (else => (lambda (x) x)))   ⇒ c
```


## Local bindings

The binding constructs `let`, `let*`, `letrec`, `letrec*`, `let-values`, and `let*-values` give Scheme a block structure. The syntax of the first four constructs is identical, but they differ in the regions they establish for their variable bindings. In a `let` expression, the initial values are computed before any of the variables become bound. In a `let*` expression, the bindings and evaluations are performed sequentially. While in `letrec` and `letrec*` expressions, all the bindings are in effect while their initial values are being computed, thus allowing mutually recursive definitions. The `let-values` and `let*-values` constructs are analogous to `let` and `let*` respectively, but are designed to handle multiple-valued expressions, binding different identifiers to the returned values.

**(let _bindings body_)** <span style="float:right;text-align:rigth;">[syntax]</span>   

_bindings_ has the form `((`_variable_ _init_`) ...)`, where each _init_ is an expression, and _body_ is a sequence of zero or more definitions followed by a sequence of one or more expressions. It is an error for _variable_ to appear more than once in the list of variables being bound.

All _init_ expressions are evaluated in the current environment, the variables are bound to fresh locations holding the results, the _body_ is evaluated in the extended environment, and the values of the last expression of _body_ are returned. Each binding of a _variable_ has _body_ as its scope.

```scheme
(let ((x 2) (y 3))
  (* x y))             ⇒  6

(let ((x 2) (y 3))
  (let ((x 7)
        (z (+ x y)))
    (* z x)))          ⇒ 35
```

**(let\* _bindings body_)** <span style="float:right;text-align:rigth;">[syntax]</span>   

_bindings_ has the form `((`_variable_ _init_`) ...)`, where each _init_ is an expression, and _body_ is a sequence of zero or more definitions followed by a sequence of one or more expressions.

The `let*` binding construct is similar to let, but the bindings are performed sequentially from left to right, and the region of a binding indicated by `(`_variable_ _init_`)` is that part of the `let*` expression to the right of the binding. Thus, the second binding is done in an environment in which the first binding is visible, and so on. The variables need not be distinct.

```scheme
(let ((x 2) (y 3))
      (let* ((x 7)
             (z (+ x y)))
        (* z x)))         ⇒  70
```

**(letrec  _bindings body_)** <span style="float:right;text-align:rigth;">[syntax]</span>   

_bindings_ has the form `((`_variable_ _init_`) ...)`, where each _init_ is an expression, and _body_ is a sequence of zero or more definitions followed by a sequence of one or more expressions. It is an error for _variable_ to appear more than once in the list of variables being bound.

The variables are bound to fresh locations holding unspecified values, the _init_ expressions are evaluated in the resulting environment, each _variable_ is assigned to the result of the corresponding _init_ expression, the _body_ is evaluated in the resulting environment, and the values of the last expression in _body_ are returned. Each binding of a _variable_ has the entire `letrec` expression as its scope, making it possible to define mutually recursive procedures.

```scheme
(letrec ((even? (lambda (n)
                  (if (zero? n) #t (odd? (- n 1)))))
         (odd?  (lambda (n)
                  (if (zero? n) #f (even? (- n 1))))))
  (even? 88))  ⇒  #t
```

One restriction of `letrec` is very important: if it is not possible to evaluate each _init_ expression without assigning or referring to the value of any _variable_, it is an error. The restriction is necessary because `letrec` is defined in terms of a procedure call where a `lambda` expression binds the variables to the values of the _init_ expressions. In the most common uses of `letrec`, all the _init_ expressions are lambda expressions and the restriction is satisfied automatically.

**(letrec\*  _bindings body_)** <span style="float:right;text-align:rigth;">[syntax]</span>   

_bindings_ has the form `((⟨`_variable_ _init_`) ...)`, where each _init_ is an expression, and _body_ is a sequence of zero or more definitions followed by a sequence of one or more expressions. It is an error for _variable_ to appear more than once in the list of variables being bound.

The variables are bound to fresh locations, each _variable_ is assigned in left-to-right order to the result of evaluating the corresponding _init_ expression, the _body_ is evaluated in the resulting environment, and the values of the last expression in _body_ are returned. Despite the left-to-right evaluation and assignment order, each binding of a _variable_ has the entire `letrec*` expression as its region, making it possible to define mutually recursive procedures. If it is not possible to evaluate each _init_ expression without assigning or referring to the value of the corresponding _variable_ or the _variable_ of any of the bindings that follow it in _bindings_, it is an error. Another restriction is that it is an error to invoke the continuation of an _init_ expression more than once.

```scheme
(letrec* ((p (lambda (x)
               (+ 1 (q (- x 1)))))
          (q (lambda (y)
               (if (zero? y) 0 (+ 1 (p (- y 1))))))
          (x (p 5))
          (y x))
  y)  ⇒  5
```

**(let-values _bindings body_)** <span style="float:right;text-align:rigth;">[syntax]</span>   

_bindings_ has the form `((⟨`_formals init_`) ...)`, where each _formals_ is a list of variables, _init_ is an expression, and _body_ is zero or more definitions followed by a sequence of one or more expressions. It is an error for a variable to appear more than once in _formals_.

The _init_ expressions are evaluated in the current environment as if by invoking `call-with-values`, and the variables occurring in list _formals_ are bound to fresh locations holding the values returned by the _init_ expressions, where the _formals_ are matched to the return values in the same way that the _formals_ in a lambda expression are matched to the arguments in a procedure call. Then, _body_ is evaluated in the extended environment, and the values of the last expression of _body_ are returned. Each binding of a variable has _body_ as its scope.

It is an error if the variables in list _formals_ do not match the number of values returned by the corresponding _init_ expression.

```scheme
(let-values (((root rem) (exact-integer-sqrt 32)))
  (* root rem))  ⇒  35
```

**(let\*-values _bindings body_)** <span style="float:right;text-align:rigth;">[syntax]</span>   

_bindings_ has the form `((⟨`_formals init_`) ...)`, where each _formals_ is a list of variables, _init_ is an expression, and _body_ is zero or more definitions followed by a sequence of one or more expressions. It is an error for a variable to appear more than once.

The `let*-values` construct is similar to `let-values`, but the _init_ expressions are evaluated and bindings created sequentially from left to right, with the region of the bindings of each variable in _formals_ including the _init_ expressions to its right as well as _body_. Thus the second _init_ expression is evaluated in an environment in which the first set of bindings is visible and initialized, and so on.

```scheme
(let ((a 'a) (b 'b) (x 'x) (y 'y))
  (let*-values (((a b) (values x y))
                ((x y) (values a b)))
    (list a b x y)))  ⇒  (x y x y)
```

**(letrec-values _bindings body_)** <span style="float:right;text-align:rigth;">[syntax]</span>   

_bindings_ has the form `((⟨`_formals init_`) ...)`, where each _formals_ is a list of variables, _init_ is an expression, and _body_ is zero or more definitions followed by a sequence of one or more expressions. It is an error for a variable to appear more than once.

First, the variables of the _formals_ lists are bound to fresh locations holding unspecified values. Then, the _init_ expressions are evaluated in the current environment as if by invoking `call-with-values`, where the _formals_ are matched to the return values in the same way that the _formals_ in a lambda expression are matched to the arguments in a procedure call. Finally, _body_ is evaluated in the resulting environment, and the values of the last expression in _body_ are returned. Each binding of a _variable_ has the entire `letrec-values` expression as its scope, making it possible to define mutually recursive procedures.

```scheme
(letrec-values
  (((a)   (lambda (n)
            (if (zero? n) #t (odd? (- n 1)))))
   ((b c) (values
            (lambda (n)
              (if (zero? n) #f (even? (- n 1))))
            a)))
  (list (a 1972) (b 1972) (c 1972)))
⇒  (#t #f #t)
```

**(let-optionals _args_ (_arg_ ... (_var_ _default_) ...) _body_ ...)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(let-optionals _args_ (_arg_ ... (_var_ _default_) ... . _rest_) _body_ ...)**  

This binding construct can be used to handle optional arguments of procedures. _args_ refers to the rest parameter list of a procedure or lambda expression. `let-optionals` binds the variables _arg ..._ to the arguments available in _args_. It is an error if there are not sufficient elements in _args_. Then, the variables _var_, ... are bound to the remaining elements available in list _args_, or to _default_, ... if there are not enough elements available in _args_. Variables are bound in parallel, i.e. all _default_ expressions are evaluated in the current environment in which the new variables are not bound yet. Then, _body_ is evaluated in the extended environment including all variable definitions of `let-optionals`, and the values of the last expression of _body_ are returned. Each binding of a variable has _body_ as its scope.

```scheme
(let-optionals '("zero" "one" "two")
               (zero (one 1) (two 2) (three 3))
  (list zero one two three))  ⇒  ("zero" "one" "two" 3)
```

**(let\*-optionals _args_ (_arg_ ... (_var_ _default_) ...) _body_ ...)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(let\*-optionals _args_ (_arg_ ... (_var_ _default_) ... . _rest_) _body_ ...)**  

The `let*-optionals` construct is similar to `let-optionals`, but the _default_ expressions are evaluated and bindings created sequentially from left to right, with the scope of the bindings of each variable including the _default_ expressions to its right as well as _body_. Thus, the second _default_ expression is evaluated in an environment in which the first binding is visible and initialized, and so on.

```scheme
(let*-optionals '(0 10 20)
                (zero
                 (one (+ zero 1))
                 (two (+ zero one 1))
                 (three (+ two 1)))
  (list zero one two three))  ⇒  (0 10 20 21)
```

**(let-keywords _args_ (_binding_ ...) _body_ ...)** <span style="float:right;text-align:rigth;">[syntax]</span>  

_binding_ has one of two forms:

   - `(`_var default_`)`, and
   - `(`_var keyword default_`)`

where _var_ is a variable, _keyword_ is a symbol, and _default_ is an expression. It is an error for a variable _var_ to appear more than once.

This binding construct can be used to handle keyword arguments of procedures. _args_ refers to the rest parameter list of a procedure or lambda expression. `let-keywords` binds the variables _var_, ... by name, i.e. by searching in _args_ for the keyword argument. If an optional _keyword_ is provided, it is used as the name of the keyword to search for, otherwise, _var_ is used, appending `:`. If the keyword is not found in _args_, _var_ is bound to _default_.

Variables are bound in parallel, i.e. all _default_ expressions are evaluated in the current environment in which the new variables are not bound yet. Then, _body_ is evaluated in the extended environment including all variable definitions of `let-keywords`, and the values of the last expression of _body_ are returned. Each binding of a variable has _body_ as its scope.

```scheme
(define (make-person . args)
  (let-keywords args ((name "J. Doe")
                      (age 0)
                      (occupation job: 'unknown))
    (list name age occupation)))
(make-person)                      ⇒  ("J. Doe" 0 unknown)
(make-person 'name: "M. Zenger")   ⇒  ("M. Zenger" 0 unknown)
(make-person 'age: 31 'job: 'eng)  ⇒  ("J. Doe" 31 eng)
```

**(let\*-keywords _args_ (_binding_ ...) _body_ ...)** <span style="float:right;text-align:rigth;">[syntax]</span>   

_binding_ has one of two forms:

   - `(`_var default_`)`, and
   - `(`_var keyword default_`)`

where _var_ is a variable, _keyword_ is a symbol, and _default_ is an expression. It is an error for a variable _var_ to appear more than once.

The `let*-keywords` construct is similar to `let-keywords`, but the _default_ expressions are evaluated and bindings created sequentially from left to right, with the scope of the bindings of each variable including the _default_ expressions to its right as well as _body_. Thus the second _default_ expression is evaluated in an environment in which the first binding is visible and initialized, and so on.


## Local syntax bindings

The `let-syntax` and `letrec-syntax` binding constructs are analogous to `let` and `letrec`, but they bind syntactic keywords to macro transformers instead of binding variables to locations that contain values. Syntactic keywords can also be bound globally or locally with `define-syntax`.

**(let-syntax _bindings body_)** <span style="float:right;text-align:rigth;">[syntax]</span>   

_bindings_ has the form `((`_keyword_ _transformer_`) ...)`.
Each _keyword_ is an identifier, each _transformer_ is an instance of `syntax-rules`, and _body_ is a sequence of one or more definitions followed by one or more expressions. It is an error for a _keyword_ to appear more than once in the list of keywords being bound.

_body_ is expanded in the syntactic environment obtained by extending the syntactic environment of the `let-syntax` expression with macros whose keywords are the _keyword_ symbols bound to the specified transformers. Each binding of a _keyword_ has _body_ as its scope.

```scheme
(let-syntax
  ((given-that (syntax-rules ()
                 ((_ test stmt1 stmt2 ...)
                   (if test
                       (begin stmt1 stmt2 ...))))))
  (let ((if #t))
    (given-that if (set! if ’now))
    if))       ⇒  now

(let ((x ’outer))
  (let-syntax ((m (syntax-rules () ((m) x))))
    (let ((x ’inner))
      (m))))   ⇒  outer
```

**(letrec-syntax _bindings body_)** <span style="float:right;text-align:rigth;">[syntax]</span>   

_bindings_ has the form `((`_keyword_ _transformer_`) ...)`.
Each _keyword_ is an identifier, each _transformer_ is an instance of `syntax-rules`, and _body_ is a sequence of one or more definitions followed by one or more expressions. It is an error for a _keyword_ to appear more than once in the list of keywords being bound.

_body_ is expanded in the syntactic environment obtained by extending the syntactic environment of the `letrec-syntax` expression with macros whose keywords are the keywords, bound to the specified transformers. Each binding of a _keyword_ symbol has the _transformer_ as well as the _body_ within its scope, so the transformers can transcribe expressions into uses of the macros introduced by the `letrec-syntax` expression.

```scheme
(letrec-syntax
        ((my-or (syntax-rules ()
                  ((my-or) #f)
                  ((my-or e) e)
                  ((my-or e1 e2 ...)
                    (let ((temp e1))
                      (if temp temp (my-or e2 ...)))))))
  (let ((x #f)
        (y 7)
        (temp 8)
        (let odd?)
        (if even?))
    (my-or x
           (let temp)
           (if y)
           y)))       ⇒  7
```


## Conditional local bindings

**(if-let\* _\(clause ...\) consequent_)** <span style="float:right;text-align:rigth;">[syntax]</span>   
**(if-let\* _\(clause ...\) consequent alternate_)**   

_clause_ is either a variable, an expression in parenthesis, or a binding of the form `(`_variable_ _init_`)` where _variable_ is a symbol and _init_ is an expression. Both _consequent_ and _alternate_ are arbitrary expressions.

An `if-let*` expression is evaluated as follows: first, every clause is evaluated in the given sequence. Variables evaluate to their value, expressions in parenthesis evaluate to the value of the expression, and bindings first evaluate their initializer and then assign the result of this to the given variable. Each clause is evaluated in an environment that includes bindings of prior clauses. Variables of bindings do not need to be distinct. As soon as the first clause evaluates to `#f`, the evaluation of `if-let*` ends with the whole expression returning the value of _alternate_. If _alternate_ is not provided, no value (void) is returned. Once all clauses were evaluated successfully, _consequent_ gets evaluated and its value is returned as the overall result of the `if-let*` expression.

```scheme
(if-let* ((x "8273")(y (string->number x))) y -1)
⇒  8273
(if-let* ((x "foo")(y (string->number x))) y -1)
⇒  -1
(define z #t)
(if-let* ((x "foo") z (y (string-append x x)) z) y "F")
⇒  "foofoo"
```

**(when-let\* _\(clause ...\) consequent ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>   

_clause_ is either a variable, an expression in parenthesis, or a binding of the form `(`_variable_ _init_`)` where _variable_ is a symbol and _init_ is an expression.

A `when-let*` expression is evaluated as follows: first, every clause is evaluated in the given sequence. Variables evaluate to their value, expressions in parenthesis evaluate to the value of the expression, and bindings first evaluate their initializer and then assign the result of this to the given variable. Each clause is evaluated in an environment that includes bindings of prior clauses. Variables of bindings do not need to be distinct. As soon as the first clause evaluates to `#f`, the evaluation of `when-let*` ends with no value (void) getting returned. Once all clauses were evaluated successfully, the expressions _consequent ..._ are evaluated in order. The result of the `when-let*` expression is the value to which the last _consequent_ expression evaluates.

```scheme
(when-let* ((x 2)(y (* x 10))(z (* y 100)))
  (display x)
  (display y)
  (display z))
⇒ (void), prints: 2202000
```


## Conditional local bindings

**(if-let\* _\(clause ...\) consequent_)** <span style="float:right;text-align:rigth;">[syntax]</span>   
**(if-let\* _\(clause ...\) consequent alternate_)**   

_clause_ is either a variable, an expression in parenthesis, or a binding of the form `(`_variable_ _init_`)` where _variable_ is a symbol and _init_ is an expression. Both _consequent_ and _alternate_ are arbitrary expressions.

An `if-let*` expression is evaluated as follows: first, every clause is evaluated in the given sequence. Variables evaluate to their value, expressions in parenthesis evaluate to the value of the expression, and bindings first evaluate their initializer and then assign the result of this to the given variable. Each clause is evaluated in an environment that includes bindings of prior clauses. Variables of bindings do not need to be distinct. As soon as the first clause evaluates to `#f`, the evaluation of `if-let*` ends with the whole expression returning the value of _alternate_. If _alternate_ is not provided, no value (void) is returned. Once all clauses were evaluated successfully, _consequent_ gets evaluated and its value is returned as the overall result of the `if-let*` expression.

```scheme
(if-let* ((x "8273")(y (string->number x))) y -1)
⇒  8273
(if-let* ((x "foo")(y (string->number x))) y -1)
⇒  -1
(define z #t)
(if-let* ((x "foo") z (y (string-append x x)) z) y "F")
⇒  "foofoo"
```

**(when-let\* _\(clause ...\) consequent ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>   

_clause_ is either a variable, an expression in parenthesis, or a binding of the form `(`_variable_ _init_`)` where _variable_ is a symbol and _init_ is an expression.

A `when-let*` expression is evaluated as follows: first, every clause is evaluated in the given sequence. Variables evaluate to their value, expressions in parenthesis evaluate to the value of the expression, and bindings first evaluate their initializer and then assign the result of this to the given variable. Each clause is evaluated in an environment that includes bindings of prior clauses. Variables of bindings do not need to be distinct. As soon as the first clause evaluates to `#f`, the evaluation of `when-let*` ends with no value (void) getting returned. Once all clauses were evaluated successfully, the expressions _consequent ..._ are evaluated in order. The result of the `when-let*` expression is the value to which the last _consequent_ expression evaluates.

```scheme
(when-let* ((x 2)(y (* x 10))(z (* y 100)))
  (display x)
  (display y)
  (display z))
⇒ (void), prints: 2202000
```


## Iteration

**(do \(\(_variable init step_\) ...)** <span style="float:right;text-align:rigth;">[syntax]</span>  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(_test res ..._)**   
&nbsp;&nbsp;&nbsp;&nbsp;**_command ..._)**  

A `do` expression is an iteration construct. It specifies a set of variables to be bound, how they are to be initialized at the start, and how they are to be updated on each iteration. When a termination condition _test_ is met (i.e. it evaluates to `#t`), the loop exits after evaluating the _res_ expressions.

A `do` expression is evaluated as follows: The _init_ expressions are evaluated, the variables are bound to fresh locations, the results of the _init_ expressions are stored in the bindings of the variables, and then the iteration phase begins.

Each iteration begins by evaluating _test_. If the result is false, then the _command_ expressions are evaluated in order, the _step_ expressions are evaluated in some unspecified order, the variables are bound to fresh locations, the results of the _step_ expressions are stored in the bindings of the variables, and the next iteration begins.

If _test_ evaluates to `#t`, then the _res_ expressions are evaluated from left to right and the values of the last _res_ expression are returned. If no _res_ expressions are present, then the `do` expression evaluates to void.

The scope of the binding of a variable consists of the entire `do` expression except for the _init_ expressions. It is an error for a variable to appear more than once in the list of `do` variables. A _step_ can be omitted, in which case the effect is the same as if `(`_variable init variable_`)` had been written instead of `(`_variable init_`)`.

```scheme
(do ((vec (make-vector 5))
         (i 0 (+ i 1)))
        ((= i 5) vec)
      (vector-set! vec i i))  ⇒  #(0 1 2 3 4)

(let ((x '(1 3 5 7 9)))
      (do ((x x (cdr x))
           (sum 0 (+ sum (car x))))
          ((null? x) sum)))   ⇒  25
```
