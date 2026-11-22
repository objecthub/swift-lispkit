# SRFI 253: Data (Type-)Checking

Library `(srfi 253)` implements the data checking primitives of [SRFI 253](https://srfi.schemers.org/srfi-253/srfi-253.html): `check-arg` checks that a value satisfies a predicate, `values-checked` checks values before returning them, and `check-case` dispatches over predicates. `lambda-checked`, `case-lambda-checked` and `define-checked` define procedures whose arguments are checked, and `define-record-type-checked` defines record types whose fields are checked. The library also supports the extensions of [SRFI 273](https://srfi.schemers.org/srfi-273/srfi-273.html) to these forms: return value checks via `=> (predicate ...)` in `lambda-checked`, `case-lambda-checked` and `define-checked`, arrow clauses `(predicate => procedure)` in `check-case`, and the auxiliary syntax `check-impl?`. Library [`(srfi 273)`](<(srfi 273).md>) provides the remaining forms of SRFI 273.

In LispKit, all checks are performed, and a failing check signals an error. The error message names the procedure (or the form) that performed the check, the argument, and the predicate expression.

```scheme
(import (lispkit base) (srfi 253))

(define-checked (vector-average (v vector?)) => (real?)
  (/ (apply + (vector->list v)) (vector-length v)))

(vector-average #(1 2 3 6))   ⇒  3
(vector-average '(1 2 3 6))
  ⇒  error: vector-average: argument v does not satisfy vector?: (1 2 3 6)
```

## Checking values

**(check-arg _pred arg_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(check-arg _pred arg caller_)**  

Checks that _arg_ satisfies the predicate _pred_ and signals an error otherwise. _caller_ is used to refer to the procedure that performed the check in the error message. In LispKit, `check-arg` is a procedure and returns _arg_.

```scheme
(check-arg integer? 3)          ⇒  3
(check-arg integer? "3" 'foo)
  ⇒  error: foo: argument does not satisfy predicate: "3" #<procedure integer?>
```

**(values-checked (_pred ..._) _expr ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

Evaluates the expressions _expr ..._ from left to right, checks that each value satisfies the corresponding predicate _pred_, and returns the values as multiple values. The number of predicates and expressions must be the same. Note that the parentheses around the predicates are required, even if there is only one predicate. Values are not coerced.

```scheme
(values-checked (integer?) 9)                    ⇒  9
(values-checked (integer? string?) 9 "hello")    ⇒  9 "hello"
(values-checked (string?) 9)
  ⇒  error: values-checked: value does not satisfy string?: 9
```

**(check-case _expr clause ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

Evaluates _expr_ and checks which of the predicates of the clauses its value _v_ satisfies. Each _clause_ has one of the following forms:

  - `(pred body ...)`: if `(pred v)` returns true, the expressions _body ..._ are evaluated and the value of the last one is returned. If there are no expressions, the result of `(pred v)` is returned.
  - `(pred => proc)`: if `(pred v)` returns a true value _r_, `(proc r)` is returned.
  - `(else body ...)`: this clause can only be the last clause; its expressions are evaluated if no other clause applies.

The clauses are tried in order. If no clause applies, an error is signaled.

```scheme
(define (inc x)
  (check-case x
    (integer? (+ x 1))
    (string? (+ (string->number x) 1))))
(inc 1)      ⇒  2
(inc "1")    ⇒  2
(inc 1.5)
  ⇒  error: check-case: value does not satisfy any of the predicates: 1.5 (integer? string?)
(check-case '(1 2 3) ((lambda (x) (memv 2 x)) => length))  ⇒  2
```

## Checked procedures

**(lambda-checked _formals body ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  
**(lambda-checked _formals_ => (_pred ..._) _body ..._)**  

Like `lambda`, but each parameter in _formals_ (except a rest parameter) can be given as `(name pred)`. When the procedure is called, the argument for such a parameter is checked against the predicate _pred_ before the body is evaluated. With the `=>` form, the values returned by the body are checked against the predicates _pred ..._; the number of values must match the number of predicates.

```scheme
(define f (lambda-checked ((x integer?) y . rest) (list x y rest)))
(f 1 2 3)    ⇒  (1 2 (3))
(f "1" 2)
  ⇒  error: lambda-checked: argument x does not satisfy integer?: "1"
((lambda-checked (x) => (integer? integer?) (values x (* x x))) 3)  ⇒  3 9
```

**(case-lambda-checked (_formals body ..._) ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  
**(case-lambda-checked (_formals_ => (_pred ..._) _body ..._) ...)**  

Like `case-lambda`, but with checked parameters and return values like `lambda-checked`. The clause is selected based on the number of arguments only; the checks of the selected clause are performed afterwards. They do not select a different clause.

```scheme
(define g (case-lambda-checked
            (((x integer?)) x)
            (((x integer?) (y integer?)) => (integer?) (+ x y))))
(g 1 2)      ⇒  3
(g "1")
  ⇒  error: case-lambda-checked: argument x does not satisfy integer?: "1"
```

**(define-checked (_name . formals_) _body ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  
**(define-checked (_name . formals_) => (_pred ..._) _body ..._)**  
**(define-checked _name pred expr_)**  

The first two forms define a procedure _name_ like `(define name (lambda-checked formals body ...))`; error messages refer to the procedure by _name_. The third form defines a variable _name_ with the value of _expr_ after checking that it satisfies _pred_. Later assignments to the variable are not checked.

```scheme
(define-checked (greet (name string?)) (string-append "Hello, " name "!"))
(greet "Ann")    ⇒  "Hello, Ann!"
(greet 'ann)     ⇒  error: greet: argument name does not satisfy string?: ann
(define-checked limit integer? 100)
```

## Checked record types

**(define-record-type-checked _type_ (_constructor field-name ..._) _pred field ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

Defines a record type like `define-record-type`. Each _field_ has the form `(field-name field-pred accessor)` or `(field-name field-pred accessor modifier)`. The constructor checks that each argument satisfies the predicate of the field with the same name; the modifiers check the new values of the fields. Unlike in `define-record-type`, the constructor arguments must be plain field names.

```scheme
(define-record-type-checked point
  (make-point x y)
  point?
  (x real? point-x point-x-set!)
  (y real? point-y))
(define p (make-point 1 2))
(point-x-set! p 3.5)
(point-x p)            ⇒  3.5
(make-point 1 'two)    ⇒  error: make-point: argument y does not satisfy real?: two
(point-x-set! p "4")   ⇒  error: point-x-set!: argument x does not satisfy real?: "4"
```

## Implementation-specific checks

**(check-impl? _datum_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

An escape hatch of SRFI 273 for checks that are specific to an implementation and have no standard predicate: when used as a predicate in the forms of this library, _datum_ is passed to the implementation. Since LispKit has no implementation-specific types or checks, `(check-impl? datum)` is a predicate that accepts all values.

```scheme
(define-checked (ref (data (check-impl? pointer)) (idx integer?))
  (list data idx))
(ref 'p 1)      ⇒  (p 1)
(ref 'p 'x)     ⇒  error: ref: argument idx does not satisfy integer?: x
```
