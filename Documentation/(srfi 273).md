# SRFI 273: Extensions to Data (Type-)Checking

[SRFI 273](https://srfi.schemers.org/srfi-273/srfi-273.html) extends the data checking primitives of [SRFI 253](https://srfi.schemers.org/srfi-253/srfi-253.html). Some of the extensions apply to the forms of SRFI 253 and are implemented by library [`(srfi 253)`](<(srfi 253).md>): return value checks via `=> (predicate ...)` in `lambda-checked`, `case-lambda-checked` and `define-checked`, arrow clauses `(predicate => procedure)` in `check-case`, and the auxiliary syntax `check-impl?`. Library `(srfi 273)` provides the other extensions: `define-check` for defining checks, `define-values-checked` for defining checked variables via multiple values, and `declare-checked` for declaring the checks of procedures and variables. It also re-exports `check-impl?`.

```scheme
(import (lispkit base) (srfi 253) (srfi 273))

(define-check email? string?)
(define-check positive-integer?
  (lambda (x) (and (integer? x) (positive? x))))

(define-checked (send-reminders (to email?) (count positive-integer?))
  (list to count))
(send-reminders "ann@example.com" 3)   ⇒  ("ann@example.com" 3)
(send-reminders "ann@example.com" 0)
  ⇒  error: send-reminders: argument count does not satisfy positive-integer?: 0

(define-values-checked (q r) (integer? integer?) (floor/ 7 2))
(list q r)                             ⇒  (3 1)
```

## Defining checks

**(define-check _name predicate_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

Defines _name_ as a new check based on _predicate_. In LispKit, this defines _name_ as the value of the expression _predicate_ after checking that it is a procedure.

```scheme
(define-check non-empty-string?
  (lambda (s) (and (string? s) (> (string-length s) 0))))
(check-arg non-empty-string? "")   ⇒  error: argument does not satisfy predicate: "" ...
(define-check invalid 3)           ⇒  error: define-check: 3 is not a predicate: 3
```

## Checked definitions of multiple values

**(define-values-checked (_var ..._) (_pred ..._) _expr_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

Like `(define-values (var ...) expr)`, but checks that the values returned by _expr_ satisfy the corresponding predicates _pred ..._ before the variables are defined. It is an error if _expr_ does not return as many values as there are variables. Later assignments to the variables are not checked.

```scheme
(define-values-checked (q r) (integer? integer?) (floor/ 7 2))
(define-values-checked (n s) (integer? string?) (floor/ 7 2))
  ⇒  error: define-values-checked: variable s does not satisfy string?: 1
```

## Declaring checks

**(declare-checked _name pred_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  
**(declare-checked (_name arg ..._))**  
**(declare-checked (_name arg ..._) => (_pred ..._))**  

Declares the checks of a variable or a procedure that is defined elsewhere, before or after the declaration. The first form declares that variable _name_ satisfies _pred_. The other forms declare the checks of the arguments of procedure _name_; each _arg_ is either a parameter name or a list `(name pred)`, and a rest parameter is allowed. With `=>`, _pred ..._ are the checks of the return values.

Declarations are hints for implementations that infer or enforce types. LispKit does neither, so declarations have no effect on the declared variables and procedures. LispKit only verifies that a declaration is well-formed and that its predicates evaluate to procedures. Use `define-checked` of library `(srfi 253)` for procedures and variables whose checks are enforced.

```scheme
(declare-checked (negative? (x real?)) => (boolean?))
(declare-checked (numbered-string (str string?) (idx integer?)) => (string?))
(define (numbered-string str idx)
  (string-append str (number->string idx)))
(declare-checked (numbered-string (str 3) idx))
  ⇒  error: declare-checked: 3 for str is not a predicate: 3
```

## Implementation-specific checks

**(check-impl? _datum_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

An escape hatch for checks that are specific to an implementation and have no standard predicate. Since LispKit has no implementation-specific types or checks, `(check-impl? datum)` is a predicate that accepts all values. See library [`(srfi 253)`](<(srfi 253).md>).

## Optimizable check patterns

SRFI 273 suggests that implementations optimize common combinations of predicates, e.g. `conjoin`, `disjoin` and `complement` of [SRFI 235](https://srfi.schemers.org/srfi-235/srfi-235.html), or `cut` of [SRFI 26](https://srfi.schemers.org/srfi-26/srfi-26.html) for member and list-of checks. These are ordinary procedures and work as checks in LispKit; they are not treated specially.

```scheme
(import (srfi 235))
(define-check natural? (conjoin exact-integer? (complement negative?)))
(check-arg natural? 0)    ⇒  0
(check-arg natural? -1)   ⇒  error: argument does not satisfy predicate: -1 ...
```
