# LispKit Match

`(lispkit match)` ports Alex Shinn's portable hygienic pattern matcher library to LispKit and adapts it to match LispKit's features. For instance, `(lispkit match)` assumes all pairs are immutable. At this point, the library does not support matching against algebraic datatypes. Procedure `match` of library `(lispkit datatype)` needs to be used for this purpose.

## Simple patterns

Patterns are written to look like the printed representation of the objects they match. The basic usage for matching an expression `expr` against a pattern `pat` via procedure `match` looks like this:

```scheme
(match expr (pat body ...) ...)
```

Here, the result of `expr` is matched against each pattern in turn, and the corresponding body is evaluated for the first to succeed. Thus, a list of three elements matches a list of three elements.

```scheme
(let ((ls (list 1 2 3)))
  (match ls ((1 2 3) #t)))  ⇒  #t
```

If no patterns match, an error is signaled. Identifiers will match anything, and make the corresponding binding available in the body.

```scheme
(match (list 1 2 3) ((a b c) b))  ⇒  2
```

If the same identifier occurs multiple times, the first instance will match anything, but subsequent instances must match a value which is `equal?` to the first.

```scheme
(match (list 1 2 1) ((a a b) 1) ((a b a) 2))  ⇒  2
```

The special identifier `_` matches anything, no matter how many times it is used, and does not bind the result in the body.

```scheme
(match (list 1 2 1) ((_ _ b) 1) ((a b a) 2))  ⇒  1
```

To match a literal identifier (or list or any other literal), use `quote`.

```scheme
(match 'a ('b 1) ('a 2))  ⇒  2
```

Analogous to its normal usage in scheme, `quasiquote` can be used to quote a mostly literally matching object with selected parts unquoted.

```scheme
(match (list 1 2 3) (`(1 ,b ,c) (list b c)))  ⇒  (2 3)
```

Often you want to match any number of a repeated pattern. Inside a list pattern you can append `...` after an element to match zero or more of that pattern, similar to a regular expression _Kleene star_.

```scheme
(match (list 1 2) ((1 2 3 ...) #t))        ⇒  #t
(match (list 1 2 3) ((1 2 3 ...) #t))      ⇒  #t
(match (list 1 2 3 3 3) ((1 2 3 ...) #t))  ⇒  #t
```

Pattern variables matched inside the repeated pattern are bound to a list of each matching instance in the body.

```scheme
(match (list 1 2) ((a b c ...) c))        ⇒  ()
(match (list 1 2 3) ((a b c ...) c))      ⇒  (3)
(match (list 1 2 3 4 5) ((a b c ...) c))  ⇒  (3 4 5)
```

More than one `...` may not be used in the same list, since this would require exponential backtracking in the general case. However, `...` need not be the final element in the list, and may be succeeded by a fixed number of patterns.

```scheme
(match (list 1 2 3 4) ((a b c ... d e) c))        ⇒  ()
(match (list 1 2 3 4 5) ((a b c ... d e) c))      ⇒  (3)
(match (list 1 2 3 4 5 6 7) ((a b c ... d e) c))  ⇒  (3 4 5)
```

`___` is provided as an alias for `...` when it is inconvenient to use the ellipsis (as in a syntax-rules template).

The `..1` syntax is exactly like the `...` except that it matches one or more
repetitions like a `+` in regular expressions.

```scheme
(match (list 1 2) ((a b c ..1) c))    ⇒  [error] no matching pattern
(match (list 1 2 3) ((a b c ..1) c))  ⇒  (3)
```

## Composite patterns

The boolean operators `and`, `or` and `not` can be used to group and negate patterns analogously to their Scheme counterparts.

The `and` operator ensures that all subpatterns match. This operator is often used with the idiom `(and x pat)` to bind `x` to the entire value that matches `pat`, similar to "as-patterns" in ML and Haskell. Another common use is in conjunction with `not` patterns to match a general case with certain exceptions.

```scheme
(match 1 ((and) #t))     ⇒  #t
(match 1 ((and x) x))    ⇒  1
(match 1 ((and x 1) x))  ⇒  1
```

The `or` operator ensures that at least one subpattern matches.  If the same identifier occurs in different subpatterns, it is matched independently.  All identifiers from all subpatterns are bound if the `or` operator matches, but the binding is only defined for identifiers from the subpattern which matched.

```scheme
(match 1 ((or) #t) (else #f))  ⇒  #f
(match 1 ((or x) x))           ⇒  1
(match 1 ((or x 2) x))         ⇒  1
(match 1 ((or 1 x) x))         ⇒  [error] variable not yet initialized: x
```

The `not` operator succeeds if the given pattern does not match. None of the identifiers used are available in the body.

```scheme
(match 1 ((not 2) #t))  ⇒  #t
```

The more general operator `?` can be used to provide a predicate. The usage is
`(? predicate pat ...)` where `predicate` is a Scheme expression evaluating to a
predicate called on the value to match, and any optional patterns after the
predicate are then matched as in an `and` pattern.

```scheme
(match 1 ((? odd? x) x))  ⇒  1
```

## Advanced patterns

The field operator `=` is used to extract an arbitrary field and match against it. It is useful for more complex or conditional destructuring that can't be more directly expressed in the pattern syntax.  The usage is `(= field pat)`, where `field` can be any expression, and should evaluate to a procedure of one argument which gets applied to the value to match to generate a new value to match against `pat`.

Thus the pattern `(and (= car x) (= cdr y))` is equivalent to `(x . y)`, except it will result in an immediate error if the value isn't a pair.

```scheme
(match '(1 . 2) ((= car x) x))          ⇒  1
(match '(1 . 2)
  ((and (= car x) (= cdr y)) (+ x y)))  ⇒  3
(match 4 ((= square x) x))              ⇒  16
```

The record operator `$` is used as a concise way to match records. The usage is
`($ rtd field ...)`, where `rtd` should be the _record type descriptor_ specified as the first argument to `define-record-type`, and each `field` is a subpattern matched against the fields of the record in order. Not all fields must be present. For more information on _record type descriptors_ see library `(lispkit record)`.

```scheme
(define-record-type employee
  (make-employee name title)
  employee?
  (name get-name)
  (title get-title))

(match (make-employee "Bob" "Doctor")
  (($ employee n t) (list t n)))
⇒  ("Doctor" "Bob")
```

For records with more fields it can be helpful to match them by name rather than
position. For this you can use the `@` operator, originally a Gauche extension:

```scheme
(define-record-type employee
  (make-employee name title)
  employee?
  (name get-name)
  (title get-title))

(match (make-employee "Bob" "Doctor")
  ((@ employee (title t) (name n)) (list t n)))
⇒  ("Doctor" "Bob")
```

The `set!` and `get!` operators are used to bind an identifier to the setter and
getter of a field, respectively. The setter is a procedure of one argument, which mutates the field to that argument. The getter is a procedure of no arguments which returns the current value of the field.

```scheme
(let ((x (mcons 1 2)))
  (match x ((1 . (set! s)) (s 3) x)))  ⇒  #<pair 1 3>
(match '(1 . 2) ((1 . (get! g)) (g)))  ⇒  2
```

The new operator `***` can be used to search a tree for subpatterns. A pattern of the form `(x *** y)` represents the subpattern `y` located somewhere in a tree where the path from the current object to \var{y} can be seen as a list of the form `(x ...)`. `y` can immediately match the current object in which case the path is the empty list. In a sense, it is a two-dimensional version of the `...` pattern. As a common case the pattern `(_ *** y)` can be used to search for `y` anywhere in a tree, regardless of the path used.

```scheme
(match '(a (a (a b))) ((x *** 'b) x))  ⇒  (a a a)
(match '(a (b) (c (d e) (f g)))
  ((x *** 'g) x))                      ⇒  (a c f)
```

## Pattern grammar

```scheme
pat = patvar                         ;; anything, and binds pattern var
    | _                              ;; anything
    | ()                             ;; the empty list
    | #t                             ;; #t
    | #f                             ;; #f
    | string                         ;; a string
    | number                         ;; a number
    | character                      ;; a character
    | 'sexp                          ;; an s-expression
    | 'symbol                        ;; a symbol (special case of s-expr)
    | (pat1 ... patN)                ;; list of n elements
    | (pat1 ... patN . patN+1)       ;; list of n or more
    | (pat1 ... patN patN+1 ooo)     ;; list of n or more, each element
                                     ;;   of remainder must match patN+1
    | #(pat1 ... patN)               ;; vector of n elements
    | #(pat1 ... patN patN+1 ooo)    ;; vector of n or more, each element
                                     ;;   of remainder must match patN+1
    | ($ record-type pat1 ... patN)  ;; a record (patK matches in slot order)
    | (struct struct-type pat1 ... patN)    ;; ditto
    | (@ record-type (slot1 pat1) ...)      ;; a record (using slot names)
    | (object struct-type (slot1 pat1) ...) ;; ditto
    | (= proc pat)                   ;; apply proc, match the result to pat
    | (and pat ...)                  ;; if all of pats match
    | (or pat ...)                   ;; if any of pats match
    | (not pat ...)                  ;; if no pats match
    | (? predicate pat ...)          ;; if predicate true and all pats match
    | (set! patvar)                  ;; anything, and binds setter
    | (get! patvar)                  ;; anything, and binds getter
    | (pat1 *** pat2)                ;; tree subpattern (*)
    | `qp                            ;; a quasi-pattern

patvar = a symbol except _, quote, $, struct, @, object, =, and, or,
         not, ?, set!, get!, quasiquote, ..., ___, ..1, ..=, ..*.

ooo = ...                            ;; zero or more
    | ___                            ;; zero or more
    | ..1                            ;; one or more
    | ..= k                          ;; exactly k where k is an integer. (*)
                                     ;;   Example: ..= 1, ..= 2 ...
    | ..* k j                        ;; between k and j, where k and j are (*)
                                     ;;   integers. Example: ..* 3 4, match 3
                                     ;;   or 4 of a pattern ..* 1 5 match from
				                             ;;   1 to 5 of a pattern

qp  = ()                             ;; the empty list
    | #t                             ;; #t
    | #f                             ;; #f
    | string                         ;; a string
    | number                         ;; a number
    | character                      ;; a character
    | identifier                     ;; a symbol
    | (qp_1 ... qp_n)                ;; list of n elements
    | (qp_1 ... qp_n . qp_{n+1})     ;; list of n or more
    | (qp_1 ... qp_n qp_n+1 ooo)     ;; list of n or more, each element
                                     ;;   of remainder must match qp_n+1
    | #(qp_1 ... qp_n)               ;; vector of n elements
    | #(qp_1 ... qp_n qp_n+1 ooo)    ;; vector of n or more, each element
                                     ;;   of remainder must match qp_n+1
    | ,pat                           ;; a pattern
    | ,@pat                          ;; a pattern
```

## Matching API

**(match _expr_ (_pat_ . _body_) _..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(match _expr_ (_pat_ (=> _failure_) . _body_) _..._)**  

The result of _expr_ is matched against each pattern _pat_ in turn until the first pattern matches. When a match is found, the corresponding _body_ statements are evaluated in order, and the result of the last expression is returned as the result of the entire `match` evaluation. If a failure occurs, then it is bound to a procedure of no arguments which continues processing at the next pattern. If no pattern matches, an error is signaled.

**(match-lambda (_pat body ..._) _..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

This is a shortcut for `lambda` in combination with `match`. `match-lambda` returns a procedure of one argument, and matches that argument against each clause.

```scheme
(lambda (expr) (match expr (pat body ...) ...))
```

**(match-lambda\* (_pat body ..._) _..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

`match-lambda*` is similar to `match-lambda`. It returns a procedure of any number of arguments, and matches the argument list against each clause.

```scheme
(lambda expr (match expr (pat body ...) ...))
```

**(match-let ((_var value_) ...) _body ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(match-let _loop_ ((_var value_) ...) _body ..._)**  

`match-let` matches each variable _var_ to the corresponding expression, and evaluates the body with all match variables in scope. It raises an error if any of the expressions fail to match. This syntax is analogous to named `let` and can also be used for recursive functions which match on their arguments as in `match-lambda*`.

**(match-let\* ((_var value_) ...) _body ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Similar to `match-let`, but analogously to `let*`, `match-let*` matches and binds the variables in sequence, with preceding match variables in scope.

**(match-letrec ((_var value_) ...) _body ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Similar to `match-let`, but analogously to `letrec`, `match-letrec` matches and binds the variables with all match variables in scope.

***

This documentation was derived from code and documentation written by Andrew K. Wright, Robert Cartwright, Alex Shinn, Panicz Maciej Godek, Felix Thibault, Shiro Kawai and Ludovic Cortès.
