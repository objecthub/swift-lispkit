# LispKit Datatype

Library `(lispkit datatype)` implements algebraic datatypes for LispKit. It provides the following functionality:

   - `define-datatype` creates a new algebraic datatype consisting of a type test predicate and a number of variants. Each variant implicitly defines a constructor and a pattern.
   - `define-pattern` introduces a new pattern and constructor for an existing datatype variant.
   - `match` matches a value of an algebraic datatype against patterns, binding pattern variables and executing the code of the first case whose pattern matches the value.


## Usage

Here is an example of a datatype defining a tree for storing and finding elements:

```scheme
(define-datatype tree tree?
  (empty)
  (node left element right) where (and (tree? left) (tree? right)))
```

The datatype `tree` defines a predicate `tree?` for checking whether a value is of type `tree`. In addition, it defines two variants with corresponding constructors `empty` and `node` for creating values of type `tree`. Variant `node` defines an invariant that prevents nodes from being constructed unless `left` and `right` are also trees.

The following line defines a new tree:

```scheme
(define t1 (node (empty) 4 (node (empty) 7 (empty))))
```

Using `match`, values like `t1` can be deconstructed using pattern matching. The following function `elements` shows how to collect all elements from a tree in a list:

```scheme
(define (elements tree)
  (match tree
    ((empty)       ())
    ((node l e r)  (append (elements l) (list e) (elements r)))))
```

`match` is a special form which takes a value of an algebraic datatype and matches it against a list of cases. Each case defines a pattern and a sequence of statements which get executed if the pattern matches the value.

Cases can also optionally define a guard which is a boolean expression that gets executed if the pattern of the case matches a value. Only if the guard evaluates to true, the statements of the case get executed. Otherwise, pattern matching continues. The following function `insert` demonstrates this functionality:

```scheme
(define (insert tree x)
  (match tree
    ((empty)
      (node (empty) x (empty)))
    ((node l e r) where (< x e)
      (node (insert l x) e r))
    ((node l e r)
      (node l e (insert r x)))))
```

A new tree `t2`, with two new elements inserted, can be created like this:

```scheme
(define t2 (insert (insert t1 2) 9))
```

If a pattern is used frequently containing a lot of boilerplate, it is possible to define a shortcut using the `define-pattern` syntax:

```scheme
(define-pattern (single x)
  (node (empty) x (empty)))
```

With this declaration, it is possible to use `single` in patterns. The following example also shows that it is possible to use `else` for defining a fallback case, if no other pattern is matching.

```scheme
(match t
  ((empty) #f)
  ((single x) x)
  (else (error "two many elements")))
```

`single` can also be used as a constructor for creating trees with a single element:

```scheme
(single 6)
```

An advanced feature of `match` is the usage of pattern alternatives in a single case of a `match` construct. This can be achieved using the `or` form on the top level of a pattern:

```scheme
(define (has-many-elements tree)
  (match tree
    ((or (empty) (single _)) #f)
    (else #t)))
```

The underscore in the `(single _)` subpattern is a wildcard that matches every value and that does not bind a new variable.


## API

**(define-datatype _type (constr arg ...) ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  
**(define-datatype _type pred (constr arg ...) ..._)**  
**(define-datatype _type pred (constr arg ...)_ where _condition ..._)**  

Defines a new datatype with a given number of datatype variants. The definition requires the symbol _type_ denoting the new type, an optional symbol _pred_ which gets bound to a type test function for testing whether a value is an instance of this type, and a list of constructors of the form _(constr arg1 arg2 ...)_ where _constr_ is the constructor and _arg1_, _arg2_, ... are parameter names of the constructor. A constructor can be annotated with an invariant for defining requirements the parameters need to meet. This is done via clause `where` _expr_ succeeding the constructor declaration. _condition_ is a boolean expression which gets checked when the constructor gets invoked.

**(define-pattern _(constr arg ...) (impl expr ...)_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

Defines a new pattern _(constr arg ...)_ which specializes an existing pattern _(impl expr ...)_. Such custom patterns can be used in pattern matching expressions as well as constructors for defining values of an algebraic datatype.

**(match _expr case ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>   
**(match _expr case ..._ (else _stat ..._))** 

`match` provides a mechanism for decomposing values of algebraic datatypes via pattern matching. A `match` construct takes a value _expr_ to pattern match on, as well as a sequence of cases. Each case consists of pattern alternatives, an optional guard, and a sequence of statements:

```
case      =  `(` patterns stat ... `)`
          |  `(` patterns `where` condition stat ... `)`
patterns  =  pattern
          |  `(` `or` pattern ... `)`
pattern   =  '_'                                ; wildcard
          |  var                                ; variable
          |  `#t`                               ; literal boolean (true)
          |  `#f`                               ; literal boolean (false)
          |  string                             ; literal string
          |  number                             ; literal number
          |  character                          ; literal character
          |  vector                             ; literal vector
          |  `'` expr                           ; constant expression
          |  `,` expr                           ; value (result of evaluating expr)
          |  pattern `as` var                   ; pattern bound to variable
          |  `(` `list` pattern ... `)`         ; list pattern
          |  `(` `list` pattern ... `.` var `)` ; list pattern with rest
          |  `(` `list` pattern ... `.` `_` `)` ; list pattern with unbound rest
          |  `(` constr pattern ... `)`         ; variant pattern
```

`match` iterates through the cases and executes the sequence of statements _stat ..._ of the first case whose pattern is matching `expr` and whose guard _condition_ evaluates to true. The value returned by this sequence of statements is returned by `match`.
