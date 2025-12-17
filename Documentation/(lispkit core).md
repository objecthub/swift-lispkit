# LispKit Core

Library `(lispkit core)` provides a foundational API for

- [primitive procedures](#primitives),
- [definitions](#definitions) \(including an [import mechanism](#importing-definitions)\),
- [procedures](#procedures) \(including support for [optional arguments](#procedures-with-optional-arguments) and [tagged procedures](#tagged-procedures)\),
- [delayed execution](#delayed-execution),
- [multiple return values](#multiple-values),
- [symbols](#symbols),
- [booleans](#booleans),
- [environments](#environments),
- [syntax errors](#syntax-errors), and
- [loading source files](#loading-source-files) and support for [conditional compilation](#conditional-and-inclusion-compilation).


## Primitives

**(eval _expr_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(eval _expr env_)**  

If _expr_ is an expression, it is evaluated in the specified environment _env_ and its values are returned. If it is a definition, the specified identifiers are defined in the specified environment, provided the environment is not immutable. Should _env_ not be provided, the global interaction environment is used.

**(apply _proc arg1 ... args_)** <span style="float:right;text-align:rigth;">[procedure]</span>

The `apply` procedure calls _proc_ with the elements of the list `(append (list arg1 ...) args)` as the actual arguments.

**(equal? _obj1 obj2_)** <span style="float:right;text-align:rigth;">[procedure]</span>

The `equal?` procedure, when applied to pairs, vectors, strings and bytevectors, recursively compares them, returning `#t` when the unfoldings of its arguments into possibly infinite trees are equal (in the sense of `equal?`) as ordered trees, and `#f` otherwise. It returns the same as `eqv?` when applied to booleans, symbols, numbers, characters, ports, procedures, and the empty list. If two objects are `eqv?`, they must be `equal?` as well. In all other cases, `equal?` may return either `#t` or `#f`. Even if its arguments are circular data structures, `equal?` must always terminate. As a rule of thumb, objects are generally `equal?` if they print the same.

**(eqv? _obj1 obj2_)** <span style="float:right;text-align:rigth;">[procedure]</span>

The `eqv?` procedure defines a useful equivalence relation on objects. It returns `#t` if _obj1_ and _obj2_ are regarded as the same object.

The `eqv?` procedure returns `#t` if:

- _obj1_ and _obj2_ are both `#t` or both `#f`
- _obj1_ and _obj2_ are both symbols and are the same symbol according to the `symbol=?` procedure
- _obj1_ and _obj2_ are both exact numbers and are numerically equal in the sense of `=`
- _obj1_ and _obj2_ are both inexact numbers such that they are numerically equal in the sense of `=`, and they yield the same results in the sense of `eqv?` when passed as arguments to any other procedure that can be defined as a finite composition of Scheme’s standard arithmetic procedures, provided it does not result in a NaN value
- _obj1_ and _obj2_ are both characters and are the same character according to the `char=?` procedure
- _obj1_ and _obj2_ are both the empty list
- _obj1_ and _obj2_ are both a pair and `car` and `cdr` of both pairs are the same in the sense of `eqv?`
- _obj1_ and _obj2_ are ports, vectors, hashtables, bytevectors, records, or strings that denote the same location in the store
- _obj1_ and _obj2_ are procedures whose location tags are equal

The `eqv?` procedure returns `#f` if:

- _obj1_ and _obj2_ are of different types
- one of _obj1_ and _obj2_ is `#t` but the other is `#f`
- _obj1_ and _obj2_ are symbols but are not the same symbol according to the `symbol=?` procedure
- one of _obj1_ and _obj2_ is an exact number but the other is an inexact number
- _obj1_ and _obj2_ are both exact numbers and are numerically unequal in the sense of `=`
- _obj1_ and _obj2_ are both inexact numbers such that either they are numerically unequal in the sense of `=`, or they do not yield the same results in the sense of `eqv?` when passed as arguments to any other procedure that can be defined as a finite composition of Scheme’s standard arithmetic procedures, provided it does not result in a NaN value. As an exception, the behavior of `eqv?` is unspecified when both _obj1_ and _obj2_ are NaN.
- _obj1_ and _obj2_ are characters for which the `char=?` procedure returns `#f`
- one of _obj1_ and _obj2_ is the empty list but the other is not
- _obj1_ and _obj2_ are both a pair but either `car` or `cdr` of both pairs are not the same in the sense of `eqv?`
- _obj1_ and _obj2_ are ports, vectors, hashtables, bytevectors, records, or strings that denote distinct locations
- _obj1_ and _obj2_ are procedures that would behave differently (i.e. return different values or have different side effects) for some arguments

**(eq? _obj1 obj2_)** <span style="float:right;text-align:rigth;">[procedure]</span>

The `eq?` procedure is similar to `eqv?` except that in some cases it is capable of discerning distinctions finer than those detectable by `eqv?`. It always returns `#f` when `eqv?` also would, but returns `#f` in some cases where `eqv?` would return `#t`. On symbols, booleans, the empty list, pairs, and records, and also on non-empty strings, vectors, and bytevectors, `eq?` and `eqv?` are guaranteed to have the same behavior.

**(quote _datum_)** <span style="float:right;text-align:rigth;">[syntax]</span>

`(quote` _datum_`)` evaluates to _datum_. _datum_ can be any external representation of a LispKit object. This notation is used to include literal constants in LispKit code. `(quote` _datum_`)` can be abbreviated as ’_datum_. The two notations are equivalent in all respects. Numerical constants, string constants, character constants, vector constants, bytevector constants, and boolean constants evaluate to themselves. They need not be quoted.

**(quasiquote _template_)** <span style="float:right;text-align:rigth;">[syntax]</span>

Quasiquote expressions are useful for constructing a list or vector structure when some but not all of the desired structure is known in advance. If no commas appear within _template_, the result of evaluating `(quasiquote` _template_`)` is equivalent to the result of evaluating `(quote` _template_`)`. If a comma appears within _template_, however, the expression following the comma is evaluated ("unquoted") and its result is inserted into the structure instead of the comma and the expression. If a comma appears followed without intervening whitespace by `@`, then it is an error if the following expression does not evaluate to a list; the opening and closing parentheses of the list are then "stripped away" and the elements of the list are inserted in place of the `,@` expression sequence. `,@` normally appears only within a list or vector.

Quasiquote expressions can be nested. Substitutions are made only for unquoted components appearing at the same nesting level as the outermost quasiquote. The nesting level increases by one inside each successive quasiquotation, and decreases by one inside each unquotation. Comma corresponds to form `unquote`, `,@` corresponds to form `unquote-splicing`.

## Definitions

**(define _var expr_)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(define _var expr doc_)**  
**(define (_f arg ..._) _expr ..._)**  
**(define (_f arg ..._) _doc expr ..._)**  

`define` is used to define variables. At the outermost level of a program, a definition `(define` _var expr_`)` has essentially the same effect as the assignment expression `(set!` _var expr_`)` if variable _var_ is bound to a non-syntax value. However, if _var_ is not bound, or is a syntactic keyword, then the definition will bind _var_ to a new location before performing the assignment, whereas it would be an error to perform a `set!` on an unbound variable.

The form `(define (`_f arg ..._`)` _expr_`)` defines a function _f_ with arguments _arg ..._ and body _expr_. It is equivalent to `(define` _f_ `(lambda (`_arg ..._`)` _expr_`))`.

The parameter _doc_ is a string literal defining documentation for variable _var_. It can be accessed, for instance, by using the procedure `environment-documentation` on the environment in which the variable was bound.

```scheme
(define pi 3.141 "documentation for `pi`")
(environment-documentation (interaction-environment) 'pi)
⇒ "documentation for `pi`"
```

**(define-values _var expr_)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(define-values (_var ..._) _expr_)**  
**(define-values (_var doc ..._) _expr_)**  

`define-values` creates multiple definitions _var ..._ from a single expression _expr_ returning multiple values. It is allowed wherever `define` is allowed.

_expr_ is evaluated, and the variables _var ..._ are bound to the return values in the same way that the formal arguments in a `lambda` expression are matched to the actual arguments in a procedure call.

It is an error if a variable _var_ appears more than once in _var ..._.

```scheme
(define-values (x y) (integer-sqrt 17))
(list x y)                              ⇒ (4 1)
(define-values vs (values 'a 'b 'c))
vs                                      ⇒ (a b c)
```

The parameter _doc_ is an optional string literal defining documentation for variable _var_. It directly follows the identifier name.

**(define-syntax _keyword transformer_)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(define-syntax _keyword doc transformer_)**  

Syntax definitions have the form `(define-syntax` _keyword transformer_`)`. _keyword_ is an identifier, and _transformer_ is an instance of `syntax-rules`. Like variable definitions, syntax definitions can appear at the outermost level or nested within a body.

If the `define-syntax` occurs at the outermost level, then the global syntactic environment is extended by binding the _keyword_ to the specified _transformer_, but previous expansions of any global binding for _keyword_ remain unchanged. Otherwise, it is an internal syntax definition, and is local to the "body" in which it is defined. Any use of a syntax keyword before its corresponding definition is an error. 

Macros can expand into definitions in any context that permits them. However, it is an error for a definition to define an identifier whose binding has to be known in order to determine the meaning of the definition itself, or of any preceding definition that belongs to the same group of internal definitions. Similarly, it is an error for an internal definition to define an identifier whose binding has to be known in order to determine the boundary between the internal definitions and the expressions of the body it belongs to.

Here is an example defining syntax for `while` loops. `while` evaluates the body of the loop as long as the predicate is true.

```scheme
(define-syntax while
  (syntax-rules ()
    ((_ pred body ...)
      (let loop () (when pred body ... (loop))))))
```

The parameter _doc_ is an optional string literal defining documentation for the keyword _var_:

```scheme
(define-syntax kwote "alternative to quote"
  (syntax-rules ()
    ((kwote exp) (quote exp))))
```

**(syntax-rules (_literal ..._) _rule ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(syntax-rules _ellipsis_ (_literal ..._) _rule ..._)**  

A _transformer spec_ has one of the two forms listed above. It is an error if any of the _literal ..._, or the _ellipsis_ symbol in the second form, is not an identifier. It is also an error if syntax rules _rule_ are not of the form

   - `(`_pattern template_`)`.

The _pattern_ in a _rule_ is a list pattern whose first element is an identifier. In general, a _pattern_ is either an identifier, a constant, or one of the following:

   - `(`_pattern ..._`)`
   - `(`_pattern pattern ... . pattern_`)`
   - `(`_pattern ... pattern ellipsis pattern ..._`) (`_pattern ... pattern ellipsis pattern ... . pattern_`)`
   - `#(`_pattern ..._`)`
   - `#(`_pattern ... pattern ellipsis pattern ..._`)`

A _template_ is either an identifier, a constant, or one of the following:

   - `(`_element ..._`)`
   - `(`_element element ... . template_`) (`_ellipsis template_`)`
   - `#(`_element ..._`)`

where an _element_ is a _template_ optionally followed by an _ellipsis_. An _ellipsis_ is the identifier specified in the second form of `syntax-rules`, or the default identifier `...` (three consecutive periods) otherwise.

Here is an example showcasing how `when` can be defined in terms of `if`:

```scheme
(define-syntax when
  (syntax-rules ()
    ((_ c e ...)
      (if c (begin e ...)))))
```

**(define-library (_name ..._) _declaration ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>

A library definition takes the following form: `(define-library (`_name ..._`)` _declaration ..._`)`. `(`_name ..._`)` is a list whose members are identifiers and exact non-negative integers. It is used to identify the library uniquely when importing from other programs or libraries. It is inadvisable, but not an error, for identifiers in library names to contain any of the characters `|, \, ?, *, <, ", :, >, +, [, ], /`.

A _declaration_ is any of:

   - `(export` _exportspec ..._`)`
   - `(export-mutable` _exportspec ..._`)`
   - `(import` _importset ..._`)`
   - `(begin` _statement ..._`)`
   - `(include` _filename ..._`)`
   - `(include-ci` _filename ..._`)`
   - `(include-library-declarations` _filename ..._`)`
   - `(cond-expand` _clause ..._`)`

An export declaration specifies a list of identifiers which can be made visible to other libraries or programs. An _exportspec_ takes one of the following forms:

   - _ident_
   - `(rename` _ident1 ident2_`)`

In an _exportspec_, an identifier _ident_ names a single binding defined within or imported into the library, where the external name for the export is the same as the name of the binding within the library. A `rename` spec exports the binding defined within or imported into the library and named by _ident1_ in each `(`_ident1 ident2_`)` pairing, using _ident2_ as the external name. A regular `export` statement exports bindings in a immutable fashion, not allowing binding changes outside of the library. `export-mutable` is a LispKit-specific variant which allows library-external mutations of the exported bindings.

An `import` declaration provides a way to import the identifiers exported by another library. It has the same syntax and semantics as an `import` declaration used in a program or at the read-eval-print loop.

The `begin`, `include`, and `include-ci` declarations are used to specify the body of the library. They have the same syntax and semantics as the corresponding expression types.

The `include-library-declarations` declaration is similar to `include` except that the contents of the file are spliced directly into the current library definition. This can be used, for example, to share the same `export` declaration among multiple libraries as a simple form of library interface.

The `cond-expand` declaration has the same syntax and semantics as the `cond-expand` expression type, except that it expands to spliced-in library declarations rather than expressions enclosed in `begin`.

**(set! _var expr_)** <span style="float:right;text-align:rigth;">[syntax]</span>

Procedure `set!` is used to assign values to variables. _expr_ is evaluated, and the resulting value is stored in the location to which variable _var_ is bound. It is an error if _var_ is not bound either in some region enclosing the `set!` expression or else globally. The result of the `set!` expression is unspecified.

## Importing definitions

**(import _importset ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>

An `import` declaration provides a way to import identifiers exported by a library. Each _importset_ names a set of bindings from a library and possibly specifies local names for the imported bindings. It takes one of the following forms:

   - _libraryname_
   - `(only` _importset identifier ..._`)`
   - `(except` _importset identifier ..._`)`
   - `(prefix` _importset identifier_`)`
   - `(rename` _importset_ `(`_ifrom ito_`)` ...`)`

In the first form, all of the identifiers in the named library’s export clauses are imported with the same names (or the exported names if exported with `rename`). The additional _importset_ forms modify this set as follows:

   - `only` produces a subset of the given _importset_ including only the listed identifiers (after any renaming). It is an error if any of the listed identifiers are not found in the original set.
   - `except` produces a subset of the given _importset_, excluding the listed identifiers (after any renaming). It is an error if any of the listed identifiers are not found in the original set.
   - `rename` modifies the given _importset_, replacing each instance of _ifrom_ with _ito_. It is an error if any of the listed identifiers are not found in the original set.
   - `prefix` automatically renames all identifiers in the given _importset_, prefixing each with the specified identifier.

In a program or library declaration, it is an error to import the same identifier more than once with different bindings, or to redefine or mutate an imported binding with a definition or with `set!`, or to refer to an identifier before it is imported. However, a read-eval-print loop will permit these actions.

## Procedures

**(procedure? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>

Returns `#t` if _obj_ is a procedure; otherwise, it returns `#f`.

**(thunk? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>

Returns `#t` if _obj_ is a procedure which accepts zero arguments; otherwise, it returns `#f`.

**(procedure-of-arity? _obj n_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a procedure that accepts _n_ arguments; otherwise, it returns `#f`. This is equivalent to:

```scheme
(and (procedure? obj)
     (procedure-arity-includes? obj n))
```

**(lambda _(arg1 ...) expr ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(lambda _(arg1 ... . rest) expr ..._)**  
**(lambda _rest expr ..._)**  
**(λ _(arg1 ...) expr ..._)**  
**(λ _(arg1 ... . rest) expr ..._)**  
**(λ _rest expr ..._)**  

A lambda expression evaluates to a procedure. The environment in effect when the lambda expression was evaluated is remembered as part of the procedure. When the procedure is later called with some actual arguments, the environment in which the lambda expression was evaluated will be extended by binding the variables in the formal argument list _arg1 ..._ to fresh locations, and the corresponding actual argument values will be stored in those locations. Next, the expressions in the body of the lambda expression will be evaluated sequentially in the extended environment. The results of the last expression in the body will be returned as the results of the procedure call.

**(case-lambda _(formals expr ...) ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(case-λ _(formals expr ...) ..._)**  

A case-lambda expression evaluates to a procedure that accepts a variable number of arguments and is lexically scoped in the same manner as a procedure resulting from a lambda expression. When the procedure is called, the first clause for which the arguments agree with _formals_ is selected, where agreement is specified as for _formals_ of a lambda expression. The variables of _formals_ are bound to fresh locations, the values of the arguments are stored in those locations, the expressions in the body are evaluated in the extended environment, and the results of the last expression in the body is returned as the results of the procedure call. It is an error for the arguments not to agree with _formals_ of any clause.

Here is an example showing how to use `case-lambda` for defining a simple accumulator:

```scheme
(define (make-accumulator n)
  (case-lambda
    (()  n)
    ((m) (set! n (+ n m)) n)))
(define a (make-accumulator 1))
(a)                              ⇒ 1
(a 5)                            ⇒ 6
(a)                              ⇒ 6
```

**(thunk _expr ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  

Returns a procedure accepting no arguments and evaluating _expr_ ..., returning the result of the last expression being evaluated as the result of a procedure call. `(thunk expr ...)` is equivalent to `(lambda () expr ...)`.

**(thunk\* _expr ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  

Returns a procedure accepting an arbitrary amount of arguments and evaluating _expr_ ..., returning the result of the last expression being evaluated as the result of a procedure call. `(thunk* expr ...)` is equivalent to `(lambda args expr ...)`.

**(procedure-name _proc_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the name of procedure _proc_ as a string, or `#f` if _proc_ does not have a name.

**(procedure-rename _proc name_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates a copy of procedure _proc_ with _name_ as name. _name_ is either a string or a symbol. If it is not possible to create a renamed procedure, `procedure-rename` returns `#f`.

**(procedure-arity _proc_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a value representing the arity of procedure _proc_, or returns `#f` if no arity information is available for _proc_.

If `procedure-arity` returns a fixnum _k_, then procedure _proc_ accepts exactly _k_ arguments and applying _proc_ to some number of arguments other than _k_ will result in an arity error.

If `procedure-arity` returns an "arity-at-least" object _a_, then procedure _proc_ accepts `(arity-at-least-value a)` or more arguments and applying _proc_ to some number of arguments less than `(arity-at-least-value a)` will result in an arity error.

If `procedure-arity` returns a list, then procedure _proc_ accepts any of the arities described by the elements of the list. Applying _proc_ to some number of arguments not described by an element of the list will result in an arity error.

**(procedure-arity-range _proc_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the smallest arity range in form of a pair _(min . max)_ such that if _proc_ is provided _n_ arguments with _n < min_ or _n > max_, an arity error gets raised.

```scheme
(procedure-arity-range (lambda () 3))      ⇒  (0 . 0)
(procedure-arity-range (lambda (x) x))     ⇒  (1 . 1)
(procedure-arity-range (lambda x x))       ⇒  (0 . #f)
(procedure-arity-range (lambda (x . y) x)) ⇒  (1 . #f)
```

**(procedure-arity-includes? _proc k_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if procedure _proc_ can accept _k_ arguments and `#f` otherwise. If this procedure returns `#f`, applying _proc_ to _k_ arguments will result in an arity error.

**(arity-at-least? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an "arity-at-least" object and `#f` otherwise.

**(arity-at-least-value _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a fixnum denoting the minimum number of arguments required by the given "arity-at-least" object _obj_.

## Procedures with optional arguments  

**(opt-lambda _(arg1 ... arg1 bind1 ... bindm) expr ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(opt-lambda _(arg1 ... argn bind1 ... bindm . rest) expr ..._)**  
**(opt-lambda _rest expr ..._)**  

An `opt-lambda` expression evaluates to a procedure and is lexically scoped in the same manner as a procedure resulting from a `lambda` expression. When the procedure is later called with actual arguments, the variables are bound to fresh locations, the values of the corresponding arguments are stored in those locations, the body _expr ..._ is evaluated in the extended environment, and the result of the last body expression is returned as the result of the procedure 
call.

Formal arguments _argi_ are required arguments. Arguments _bindi_ are optional. They are of the form `(var init)`, with _var_ being a symbol and _init_ an initialization expression which gets evaluated and assigned to _var_ if the argument is not provided when the procedure is called. It is a syntax violation if the same variable appears more than once among the variables.

A procedure created with the first syntax of `opt-formals` takes at least _n_ arguments and at most _n + m_ arguments. A procedure created with the second syntax of `opt-formals` takes _n_ or more arguments. If the procedure is called with fewer than _n_ + _m_ (but at least _n_ arguments), the missing actual arguments are substituted by the values resulting from evaluating the corresponding _init_s. The corresponding _init_s are evaluated in an unspecified order in the lexical environment of the `opt-lambda` expression when the procedure is called.

**(opt\*-lambda _(arg1 ... arg1 bind1 ... bindm) expr ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(opt\*-lambda _(arg1 ... argn bind1 ... bindm . rest) expr ..._)**  
**(opt\*-lambda _rest expr ..._)**  

Similar to syntax `opt-lambda` except that the initializers of optional arguments _bindi_ corresponding to missing actual arguments are evaluated sequentially from left to right. The region of the binding of a variable is that part of the `opt*-lambda` expression to the right of it or its binding.

**(define-optionals (_f arg ... bind ..._) _expr ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(define-optionals (_f arg ... bind ... . rest_) _expr ..._)**

`define-optionals` is operationally equivalent to `(define f (opt-lambda (arg ... bind ...) expr ...))` or `(define f (opt-lambda (arg ... bind ... . rest) expr ...))` if the second syntactical form is used.

**(define-optionals\* (_f arg ... bind ..._) _expr ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(define-optionals\* (_f arg ... bind ... . rest_) _expr ..._)**

`define-optionals*` is operationally equivalent to `(define f (opt*-lambda (arg ... bind ...) expr ...))` or `(define f (opt*-lambda (arg ... bind ... . rest) expr ...))` if the second syntactical form is used.

## Tagged procedures

LispKit allows a data object to be associated with a procedure. Such data objects are called _tags_, procedures with an associated tag are called _tagged procedures_. The tag of a procedure is immutable. It is defined at procedure creation time and can later be retrieved without calling the procedure.

**(procedure/tag? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a tagged procedure and `#f` otherwise. Procedures are tagged procedures if they were created either via `lambda/tag` or `case-lambda/tag`.

**(procedure-tag _proc_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the tag of the tagged procedure _proc_. It is an error if _proc_ is not a tagged procedure.

**(lambda/tag _tag (arg1 ...) expr ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(lambda/tag _tag (arg1 ... . rest) expr ..._)**  
**(lambda/tag _tag rest expr ..._)**  

A `lambda/tag` expression evaluates to a tagged procedure. First, _tag_ is evaluated to obtain the tag value for the procedure. Then, the tagged procedure itself gets created for the given formal arguments. The procedure is lexically scoped in the same manner as a procedure resulting from a `lambda` expression. When the procedure is called, it behaves as if constructed by a `lambda` expression with the same formal arguments and body.

**(case-lambda/tag _tag (formals expr ...) ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  

A `case-lambda/tag` expression evaluates to a tagged procedure. First, _tag_ is evaluated to obtain the tag value for the procedure. Then, the tagged procedure itself gets created, accepting a variable number of arguments. It is lexically scoped in the same manner as a procedure resulting from a `lambda` expression. When the procedure is called, it behaves as if it was constructed by a `case-lambda` expression with the same formal arguments and bodies.

## Delayed execution

LispKit provides _promises_ to delay the execution of code. Built on top of _promises_ are _streams_. Streams are similar to lists, except that the tail of a stream is not computed until it is de-referenced. This allows streams to be used to represent infinitely long lists. Library `(lispkit core)` only defines procedures for _streams_ equivalent to _promises_. Library `(lispkit stream)` provides all the list-like functionality.

**(promise? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

The `promise?` procedure returns `#t` if argument _obj_ is a promise, and `#f` otherwise.

**(make-promise _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(eager _obj_)**  

The `make-promise` procedure returns a promise which, when forced, will return _obj_. It is similar to `delay`, but does not delay its argument: it is a procedure rather than syntax. If _obj_ is already a promise, it is returned. `eager` represents the same procedure like `make-promise`.

**(delay _expr_)** <span style="float:right;text-align:rigth;">[syntax]</span>   

The `delay` construct is used together with the procedure `force` to implement lazy evaluation or "call by need". `(delay expr)` returns an object called a promise which, at some point in the future, can be asked (by the `force` procedure) to evaluate _expr_, and deliver the resulting value.

**(delay-force _expr_)** <span style="float:right;text-align:rigth;">[syntax]</span>   
**(lazy _expr_)**  

The expression `(delay-force expr)` is conceptually similar to `(delay (force expr))`, with the difference that forcing the result of `delay-force` will in effect result in a tail call to `(force expr)`, while forcing the result of `(delay (force expr))` might not. Thus iterative lazy algorithms that might result in a long series of chains of delay and force can be rewritten using `delay-force` to prevent consuming unbounded space during evaluation. `lazy` represents the same procedure like `delay-force`.

**(force _promise_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

The `force` procedure forces the value of a promise created by `delay`, `delay-force`, or `make-promise`. If no value has been computed for the promise, then a value is computed and returned. The value of the promise must be cached (or "memoized") so that if it is forced a second time, the previously computed value is returned. Consequently, a delayed expression is evaluated using the parameter values and exception handler of the call to `force` which first requested its value. If _promise_ is not a promise, it may be returned unchanged.

```scheme
(force (delay (+ 1 2)))        ⇒  3
(let ((p (delay (+ 1 2))))
  (list (force p) (force p)))  ⇒ (3 3)
(define integers
  (letrec ((next (lambda (n)
                   (delay (cons n (next (+ n 1)))))))
    (next 0)))
(define head
  (lambda (stream) (car (force stream))))
(define tail
  (lambda (stream) (cdr (force stream))))
(head (tail (tail integers)))  ⇒  2
```

The following example is a mechanical transformation of a lazy stream-filtering algorithm into Scheme. Each call to a constructor is wrapped in `delay`, and each argument passed to a deconstructor is wrapped in `force`. The use of `(delay-force ...)` instead of `(delay (force ...))` around the body of the procedure ensures that an ever-growing sequence of pending promises does not exhaust available storage, because `force` will, in effect, force such sequences iteratively.

```scheme
(define (stream-filter p? s)
  (delay-force
    (if (null? (force s))
        (delay ’())
        (let ((h (car (force s)))
              (t (cdr (force s))))
          (if (p? h)
              (delay (cons h (stream-filter p? t)))
              (stream-filter p? t))))))

(head (tail (tail (stream-filter odd? integers))))  ⇒  5
```

The following examples are not intended to illustrate good programming style, as `delay`, `force`, and `delay-force` are mainly intended for programs written in the functional style. However, they do illustrate the property that only one value is computed for a promise, no matter how many times it is forced.

```scheme
(define count 0)
(define p
  (delay (begin (set! count (+ count 1))
                (if (> count x) count (force p)))))
(define x 5)
p                              ⇒  a promise
(force p)                      ⇒  6
p                              ⇒  a promise
(begin (set! x 10) (force p))  ⇒  6
```

**(stream? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

The `stream?` procedure returns `#t` if argument _obj_ is a stream, and `#f` otherwise. If _obj_ is a stream, `stream?` does not force its promise. If `(stream? obj)` is `#t`, then one of `(stream-null? obj)` and `(stream-pair? obj)` will be `#t` and the other will be `#f`; if `(stream? obj)` is `#f`, both `(stream-null? obj)` and `(stream-pair? obj)` will be `#f`.

**(make-stream _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(stream-eager _obj_)**  

The `make-stream` procedure returns a stream which, when forced, will return _obj_. It is similar to `stream-delay`, but does not delay its argument: it is a procedure rather than syntax. If _obj_ is already a stream, it is returned. `stream-eager` represents the same procedure like `make-stream`.

**(stream-delay _expr_)** <span style="float:right;text-align:rigth;">[syntax]</span>   

The `stream-delay` syntax is used together with procedure `stream-force` to implement lazy evaluation or "call by need". `(stream-delay expr)` returns an object called a stream which, at some point in the future, can be asked (by the `stream-force` procedure) to evaluate _expr_, and deliver the resulting value.

**(stream-delay-force _expr_)** <span style="float:right;text-align:rigth;">[syntax]</span>   
**(stream-lazy _expr_)**  

The expression `(stream-delay-force expr)` is conceptually similar to `(stream-delay (stream-force expr))`, with the difference that forcing the result of `stream-delay-force` will in effect result in a tail call to `(stream-force expr)`, while forcing the result of `(stream-delay (stream-force expr))` might not. Thus iterative lazy algorithms that might result in a long series of chains of delay and force can be rewritten using `stream-delay-force` to prevent consuming unbounded space during evaluation. `stream-lazy` represents the same procedure like `stream-delay-force`.

## Symbols

**(symbol? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a symbol, otherwise returns `#f`.

**(symbol-interned? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an interned symbol, otherwise returns `#f`.

**(gensym)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(gensym _str_)**  

Returns a new (fresh) interned symbol whose name consists of prefix _str_ followed by a number. _str_ is either a symbol or a string. If _str_ is not provided or set to `#f`, "g" is used as a prefix.

**(generate-uninterned-symbol)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(generate-uninterned-symbol _str_)**  

Returns a new uninterned symbol whose name consists of prefix _str_ followed by a number. _str_ is either a symbol or a string. If _str_ is not provided or set to `#f`, "g" is used as a prefix. This procedure is similar to `gensym` but always generates uninterned symbols.

**(symbol=? _sym ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if all the arguments are symbols and all have the same names in the sense of `string=?`.

**(symbol\<? _sym ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the identifiers of the symbols _sym ..._ are monotonically increasing in lexicographic order (according to `string<?`), otherwise returns `#f`.

**(string-\>symbol _str_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the symbol whose name is string _str_. This procedure can create symbols with names containing special characters that would require escaping when written, but does not interpret escapes in its input.

**(string-\>uninterned-symbol _str_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new uninterned symbol whose name is _str_. This procedure can create symbols with names containing special characters that would require escaping when written, but does not interpret escapes in its input.

**(symbol-\>string _sym_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the name of symbol _sym_ as a string, but without adding escapes.

## Booleans

The standard boolean objects for true and false are written as `#t` and `#f`. Alternatively, they can be written `#true` and `#false`, respectively. What really matters, though, are the objects that the Scheme conditional expressions (`if`, `cond`, `and`, `or`, `when`, `unless`, `do`) treat as true or false. The phrase a "true value" (or sometimes just "true") means any object treated as true by the conditional expressions, and the phrase "a false value" (or "false") means any object treated as false by the conditional expressions.

Of all the Scheme values, only `#f` counts as false in conditional expressions. All other Scheme values, including `#t`, count as true. Boolean literals evaluate to themselves, so they do not need to be quoted in programs.

**(boolean? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

The `boolean?` predicate returns `#t` if _obj_ is either `#t` or `#f` and returns `#f` otherwise.

```scheme
(boolean? #f)    ⇒  #t
(boolean? 0)     ⇒  #f
(boolean? '())   ⇒  #f
```

**(boolean=? _obj1 obj2 ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if all the arguments are booleans and all are `#t` or all are `#f`.

**(and _test ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  

The _test ..._ expressions are evaluated from left to right, and if any expression evaluates to `#f`, then `#f` is returned. Any remaining expressions are not evaluated. If all the expressions evaluate to true values, the values of the last expression are returned. If there are no expressions, then `#t` is returned.

```scheme
(and (= 2 2) (> 2 1))  ⇒  #t
(and (= 2 2) (< 2 1))  ⇒  #f
(and 12 'c '(f g))     ⇒  (f g)
(and)                  ⇒  #t
```

**(or _test ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  

The _test ..._ expressions are evaluated from left to right, and the value of the first expression that evaluates to a true value is returned. Any remaining expressions are not evaluated. If all expressions evaluate to `#f` or if there are no expressions, then `#f` is returned.

```scheme
(or (= 2 2) (> 2 1))            ⇒  #t
(or (= 2 2) (< 2 1))            ⇒  #t
(or #f #f #f)                   ⇒  #f
(or (memq 'b '(a b c)) (/ 3 0)) ⇒  (b c)
```

**(not _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

The `not` procedure returns `#t` if _obj_ is false, and returns `#f` otherwise.

```scheme
(not #t)        ⇒  #f
(not 3)         ⇒  #f
(not (list 3))  ⇒  #f
(not #f)        ⇒  #t
(not '())       ⇒  #f
(not (list))    ⇒  #f
(not 'nil)      ⇒  #f
```

**(opt _pred obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(opt _pred obj failval_)**   

The `opt` procedure returns _failval_ if _obj_ is `#f`. If _obj_ is not `#f`, `opt` applies predicate _pred_ to _obj_ and returns the result of this function application. If _failval_ is not provided, `#t` is used as a default. This function is useful to verify a given predicate _pred_ for an optional value _obj_.

## Conditional and inclusion compilation

**(cond-expand _ce-clause1 ce-clause2 ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  

The `cond-expand` expression type provides a way to statically expand different expressions depending on the implementation. A _ce-clause_ takes the following form:

(_featurerequirement_ _expression ..._)

The last clause can be an “else clause,” which has the form:

(else _expression ..._)

A _featurerequirement_ takes one of the following forms:
  - _featureidentifier_
  - `(library` _name_`)`
  - `(and` _featurerequirement ..._`)`
  - `(or` _featurerequirement ..._`)`
  - `(not` _featurerequirement_`)`

LispKit maintains a list of feature identifiers which are present, as well as a list of libraries which can be imported. The value of a _featurerequirement_ is determined by replacing each _featureidentifier_ and `(library` _name_`)` with `#t`, and all other feature identifiers and library names with `#f`, then evaluating the resulting expression as a Scheme boolean expression under the normal interpretation of `and`, `or`, and `not`.

A `cond-expand` is then expanded by evaluating the _featurerequirements_ of successive _ce-clause_ in order until one of them returns `#t`. When a true clause is found, the corresponding _expression ..._ are expanded to a `begin`, and the remaining clauses are ignored. If none of the listed _featurerequirement_ evaluates to `#t`, then if there is an "else" clause, its _expression ..._ are included. Otherwise, the behavior of the `cond-expand` is unspecified. Unlike `cond`, `cond-expand` does not depend on the value of any variables. The exact features provided are defined by the implementation, its environment and host platform.

LispKit supports the following _featureidentifier_:

   - `lispkit`
   - `r7rs`
   - `ratios`
   - `complex`
   - `syntax-rules`
   - `little-endian`
   - `big-endian`
   - `dynamic-loading`
   - `modules`
   - `32bit`
   - `64bit`
   - `macos`
   - `macosx`
   - `ios`
   - `linux`
   - `i386`
   - `x86-64`
   - `arm64`
   - `arm`

**(include _str1 str2 ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(include-ci _str1 str2 ..._)**  

Both `include` and `include-ci` take one or more filenames expressed as string literals, apply an implementation-specific algorithm to find corresponding files, read the contents of the files in the specified order as if by repeated applications of read, and effectively replace the `include` or `include-ci` expression with a `begin` expression containing what was read from the files. The difference between the two is that `include-ci` reads each file as if it began with the `#!fold-case` directive, while `include` does not.

## Multiple values

**(values _obj ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Delivers all of its arguments to its continuation. The `values` procedure might be defined as follows:

```scheme
(define (values . things)
  (call-with-current-continuation
    (lambda (cont) (apply cont things))))
```

**(call-with-values _producer consumer_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Calls its _producer_ argument with no arguments and a continuation that, when passed some values, calls the _consumer_ procedure with those values as arguments. The continuation for the call to _consumer_ is the continuation of the call to `call-with-values`.

```scheme
(call-with-values (lambda () (values 4 5))
                  (lambda (a b) b))
  ⇒  5
(call-with-values * -)
  ⇒  -1
```

**(apply-with-values _proc vals_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

`apply-with-values` calls procedure _proc_ with _vals_ as its arguments and returns the corresponding result. _vals_ might refer to multiple values created via procedure `values`. This is a LispKit-specific procedure that relies on multiple return values being represented by a container object.

## Environments

Environments are first-class objects which associate identifiers (symbols) with values. Environments are used implicitly by the LispKit compiler and runtime system, but library `(lispkit core)` also provides an API allowing systems to manipulate and use environments programmatically.

For instance, when a top-level variable gets created with `define`, the name/value association for that variable is added to the "top-level" environment. The LispKit compiler implicitly creates environments other than the top-level environment, for example, when compiling and executing libraries. 

There are several types of bindings that can occur within an environment. A _variable binding_ associates a value with an identifier. This is the most common type of binding. In addition to variable bindings, environments can have _keyword bindings_. A keyword binding associates an identifier with a macro transformer (usually via `syntax-rules`). There are also _unassigned_ bindings referring to bindings without a known value.

**(environment? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an environment. Otherwise, it returns `#f`.

**(interaction-environment? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an interaction environment, i.e. a mutable environment in which expressions entered by the user into a read-eval-print loop are being evaluated. Otherwise, procedure `interaction-environment?` returns `#f`.

**(custom-environment? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a custom environment, i.e. an environment that was programmatically constructed. Otherwise, predicate `custom-environment?` returns `#f`.

**(the-environment)** <span style="float:right;text-align:rigth;">[syntax]</span>  

Special form `the-environment` returns the current top-level environment. If there is none, `the-environment` returns `#f`.

Here is an example how one can print the names bound at compile-time:

```scheme
(define-library (foo)
  (import (only (lispkit core) the-environment environment-bound-names)
          (only (lispkit port) display newline))
  (begin
    (display "bound = ")
    (display (environment-bound-names (the-environment)))
    (newline)))
(import (foo))
⇒
bound = (display the-environment newline environment-bound-names)
```

**(environment _list1 ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

This procedure returns an environment that results by starting with an empty environment and then importing each list, considered as an import set, into it. The bindings of the environment represented by the specifier are immutable, as is the environment itself.

**(environment-bound-names _env_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the symbols that are bound by environment _env_.

**(environment-bindings _env_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the bindings of environment _env_. Each element of this list takes one of two forms: the form _(name)_ indicates that _name_ is bound but unassigned, while _(name obj)_ indicates that _name_ is bound to value _obj_.

**(environment-bound? _env ident_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if symbol _ident_ is bound in environment _env_; otherwise returns `#f`.

**(environment-lookup _env ident_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the value to which symbol _ident_ is bound in environment _env_. This procedure throws an error if _ident_ is not bound to a value in _env_.

**(environment-assignable? _env ident_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Symbol _ident_ must be bound in environment _env_. Procedure `environment-assignable?` returns `#t` if the binding of _ident_ may be modified.

**(environment-assign! _env ident obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Symbol _ident_ must be bound in environment _env_ and must be assignable. Procedure `environment-assign!` modifies the binding to have _obj_ as its value.

**(environment-definable? _env ident_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Predicate `environment-definable?` returns `#t` if symbol _ident_ is definable in environment _env_, and `#f` otherwise. Currently, interaction environments and custom environments allow for identifiers to be defined. For all other types of environments, this predicate returns `#f` independent of _ident_.

**(environment-define _env ident obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Defines _ident_ to be bound to _obj_ in environment _env_. This procedure signals an error if _ident_ is not definable in _env_.

**(environment-define-syntax _env ident transf_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Defines _ident_ to be a keyword bound to macro transformer _transf_ (typically expressed in terms of `syntax-rules`) in environment _env_. This procedure signals an error if _ident_ is not definable in environment _env_.

**(environment-import _env ident importset_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Imports the identifiers exported by a library and specified via an import set _importset_ into the environment _env_. The procedure fails if the type of environment does not allow identifiers to be defined programmatically.

**(environment-documentation _env ident_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the documentation associated with the identifier _ident_ in environment _env_ as a string. This procedure returns `#f` if _ident_ is not associated with any documentation.

**(environment-assign-documentation! _env ident str_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Assigns the documentation string _str_ to identifier _ident_ in environment _env_.

**(scheme-report-environment _version_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

If _version_ is equal to 5, corresponding to R5RS, _scheme-report-environment_ returns an environment that contains only the bindings defined in the R5RS library.

**(null-environment _version_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

If _version_ is equal to 5, corresponding to R5RS, the _null-environment_ procedure returns an environment that contains only the bindings for all syntactic keywords defined in the R5RS library.

**(interaction-environment)** <span style="float:right;text-align:rigth;">[procedure]</span>  

This procedure returns a mutable environment which is the environment in which expressions entered by the user into a read-eval-print loop are evaluated. This is typically a superset of bindings from _(lispkit base)_.

## Source files

**(load _filename_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(load _filename_ _environment_)**  

`load` reads a source file specified by _filename_ and executes it in the given _environment_. If no environment is specified, the current _interaction environment_ is used, which can be accessed via `(interaction-environment)`. Execution of the file consists of reading expressions and definitions from the file, compiling them, and evaluating them sequentially in the environment. `load` returns the result of evaluating the last expression or definition from the file. During compilation, the special form `source-directory` can be used to access the directory in which the executed file is located.

It is an error if _filename_ is not a string. If _filename_ is not an absolute file path, LispKit will try to find the file in a predefined set of directories, such as the default search path. If no file name suffix, also called _path extension_, is provided, the system will try to determine the right suffix. For instance, `(load "Prelude")` will find the prelude file, determine its suffix and load and execute the file.

**(load-program _filename_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`load-program` reads a source file specified by _filename_ and executes it in a new empty _environment_. Execution of the file consists of reading expressions and definitions from the file, compiling them, and evaluating them sequentially. `load-program` returns the evaluation result of the last expression.

It is an error if _filename_ is not a string. If _filename_ is not an absolute file path, LispKit will try to find the file in a predefined set of directories, such as the default search path. If no file name suffix is provided, the system will try to determine the right suffix.


## Syntax errors

**(syntax-error _message args ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  

`syntax-error` behaves similarly to `error` except that implementations with an expansion pass separate from evaluation should signal an error as soon as `syntax-error` is expanded. This can be used as a `syntax-rules` _template_ for a _pattern_ that is an invalid use of the macro, which can provide more descriptive error messages.

_message_ is a string literal, and _args ..._ are arbitrary expressions providing additional information. Applications cannot count on being able to catch syntax errors with exception handlers or guards.

```scheme
(define-syntax simple-let
  (syntax-rules ()
    ((_ (head ... ((x . y) val) . tail) body1 body2 ...)
      (syntax-error "expected an identifier but got" (x . y)))
    ((_ ((name val) ...) body1 body2 ...)
      ((lambda (name ...) body1 body2 ...) val ...))))
```

## Utilities

**(void)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Performs no operation and returns nothing. This is often useful as a placeholder or whenever a no-op statement is needed.

**(void? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is the "void" value (i.e. no value); returns `#f` otherwise.

**(identity _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

The identity function is always returning its argument _obj_.
