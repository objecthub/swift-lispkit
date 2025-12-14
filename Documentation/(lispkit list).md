# LispKit List

Lists are heterogeneous data structures constructed out of _pairs_ and an _empty list_ object.

A _pair_ consists of two fields called _car_ and _cdr_ (for historical reasons). Pairs are created by the procedure `cons`. The _car_ and _cdr_ fields are accessed by the procedures `car` and `cdr`. As opposed to most other Scheme implementations, lists are immutable in LispKit. Thus, it is not possible to set the car and cdr fields of an already existing pair.

Pairs are used primarily to represent lists. A list is defined recursively as either the empty list or a pair whose cdr is a list. More precisely, the set of lists is defined as the smallest set _X_ such that

   - The empty list is in _X_
   - If _list_ is in _X_, then any pair whose cdr field contains _list_ is also in _X_.

The objects in the car fields of successive pairs of a list are the _elements_ of the list. For example, a two-element list is a pair whose car is the first element and whose cdr is a pair whose car is the second element and whose cdr is the empty list. The _length_ of a list is the number of elements, which is the same as the number of pairs.

The empty list is a special object of its own type. It is not a pair, it has no elements, and its length is zero.

The most general notation (external representation) for Scheme pairs is the "dotted" notation `(c1 . c2)` where `c1` is the value of the car field and `c2` is the value of the cdr field. For example `(4 . 5)` is a pair whose car is `4` and whose cdr is `5`. Note that `(4 . 5)` is the external representation of a pair, not an expression that evaluates to a pair.

A more streamlined notation can be used for lists: the elements of the list are simply enclosed in parentheses and separated by spaces. The empty list is written `()`. For example,

```scheme
(a b c d e)
```

and

```scheme
(a . (b . (c . (d . (e . ())))))
```

are equivalent notations for a list of symbols.

A chain of pairs not ending in the empty list is called an _improper list_. Note that an improper list is not a list. The list and dotted notations can be combined to represent improper lists:

```scheme
(a b c . d)
```

is equivalent to

```scheme
(a . (b . (c . d)))
```


## Basic constructors and procedures

**(cons _x y_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a pair whose car is _x_ and whose cdr is _y_.

**(car _xs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the contents of the car field of pair _xs_. Note that it is an error to take the car of the empty list.

**(cdr _xs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the contents of the cdr field of pair _xs_. Note that it is an error to take the cdr of the empty list.

**(caar _xs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(cadr _xs_)**  
**(cdar _xs_)**  
**(cddr _xs_)**  

These procedures are compositions of `car` and `cdr` as follows:

```scheme
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
```

**(caaar _xs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(caadr _xs_)**  
**(cadar _xs_)**  
**(caddr _xs_)**  
**(cdaar _xs_)**  
**(cdadr _xs_)**  
**(cddar _xs_)**   
**(cdddr _xs_)**  

These eight procedures are further compositions of `car` and `cdr` on the same principles. For example, `caddr` could be defined by `(define caddr (lambda (x) (car (cdr (cdr x)))))`. Arbitrary compositions up to four deep are provided.

**(caaaar _xs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(caaadr _xs_)**  
**(caadar _xs_)**  
**(caaddr _xs_)**  
**(cadaar _xs_)**  
**(cadadr _xs_)**  
**(caddar _xs_)**  
**(cadddr _xs_)**  
**(cdaaar _xs_)**  
**(cdaadr _xs_)**  
**(cdadar _xs_)**  
**(cdaddr _xs_)**  
**(cddaar _xs_)**  
**(cddadr _xs_)**  
**(cdddar _xs_)**  
**(cddddr _xs_)**  

These sixteen procedures are further compositions of `car` and `cdr` on the same principles. For example, `cadddr` could be defined by `(define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))`. Arbitrary compositions up to four deep are provided.

**(make-list _k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-list _k fill_)**  

Returns a list of _k_ elements. If argument _fill_ is given, then each element is set to _fill_. Otherwise the content of each element is the empty list.

**(list _x ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of its arguments, i.e. (_x ..._).

```scheme
(list ’a (+ 3 4) ’c)   ⇒  (a 7 c)
(list)                 ⇒  ()
```

**(cons\* _e1 e2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Like `list`, but the last argument provides the tail of the constructed list, returning `(cons e1 (cons e2 (cons ... en)))`. This function is called `list*` in Common Lisp.

```scheme
(cons* 1 2 3 4)  ⇒  (1 2 3 . 4)
(cons* 1)        ⇒  1
```

**(length _xs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the length of list _xs_.

```scheme
(length ’(a b c))          ⇒  3
(length ’(a (b) (c d e)))  ⇒  3
(length ’())               ⇒  0
```


## Predicates

**(pair? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a pair, `#f` otherwise.

**(null? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an empty list, `#f` otherwise.

**(list? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a proper list, `#f` otherwise. A chain of pairs ending in the empty list is called a _proper list_.

**(every? _pred xs ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Applies the predicate _pred_ across the lists _xs ..._, returning `#t` if the predicate returns `#t` on every application. If there are _n_ list arguments _xs1 ... xsn_, then _pred_ must be a procedure taking _n_ arguments and returning a single value, interpreted as a boolean. If an application of _pred_ returns _#f_, then `every?` returns `#f` immediately without applying _pred_ further anymore.

**(any? _pred xs ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Applies the predicate _pred_ across the lists _xs ..._, returning `#t` if the predicate returns `#t` for at least one application. If there are _n_ list arguments _xs1 ... xsn_, then _pred_ must be a procedure taking _n_ arguments and returning a single value, interpreted as a boolean. If an application of _pred_ returns _#t_, then `any?` returns `#t` immediately without applying _pred_ further anymore.


## Composing and transforming lists

**(append _xs ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list consisting of the elements of the first list _xs_
followed by the elements of the other lists. If there are no arguments, the empty list is returned. If there is exactly one argument, it is returned. The last argument, if there is one, can be of any type. An improper list results if the last argument is not a proper list.

```scheme
(append ’(x) ’(y))          ⇒  (x y)
(append ’(a) ’(b c d))      ⇒  (a b c d)
(append ’(a (b)) ’((c)))    ⇒  (a (b) (c))
(append ’(a b) ’(c . d))    ⇒  (a b c . d)
(append ’() ’a)             ⇒  a
```

**(concatenate _xss_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

This procedure appends the elements of the list of lists _xss_. That is, `concatenate` returns (`apply` `append` _xss_).

**(reverse _xs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Procedure `reverse` returns a list consisting of the elements of list _xs_ in reverse order.

```scheme
(reverse '(a b c))              ⇒ (c b a)
(reverse '(a (b c) d (e (f))))  ⇒ ((e (f)) d (b c) a)
```

**(filter _pred xs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns all the elements of list _xs_ that satisfy predicate _pred_. Elements in the result list occur in the same order as they occur in the argument list _xs_.

```scheme
(filter even? '(0 7 8 8 43 -4))  ⇒  (0 8 8 -4)
```

**(remove _pred xs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list without the elements of list _xs_ that satisfy predicate _pred_: `(lambda (pred list) (filter (lambda (x) (not (pred x))) list))`. Elements in the result list occur in the same order as they occur in the argument list _xs_.

```scheme
(remove even? '(0 7 8 8 43 -4))  ⇒  (7 43)
```

**(partition _pred xs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Partitions the elements of list _xs_ with predicate _pred_ returning two values: the list of in-elements (i.e. elements from _xs_ satisfying _pred_) and the list of out-elements. Elements occur in the result lists in the same order as they occur in the argument list _xs_.

```scheme
(partition symbol? '(one 2 3 four five 6))  ⇒  (one four five)
                                               (2 3 6)
```

**(map _f xs ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `map` procedure applies procedure _proc_ element-wise to the elements of the lists _xs ..._ and returns a list of the results, in order. If more than one list is given and not all lists have the same length, map terminates when the shortest list runs out. The dynamic order in which _proc_ is applied to the elements of the lists is unspecified.

It is an error if _proc_ does not accept as many arguments as there
are lists _xs ..._ and return a single value.

```scheme
(map cadr '((a b) (d e) (g h)))             ⇒  (b e h)

(map (lambda (n) (expt n n)) '(1 2 3 4 5))  ⇒  (1 4 27 256 3125)

(map + '(1 2 3) '(4 5 6 7))                 ⇒  (5 7 9)

(let ((count 0))
  (map (lambda (ignored)
         (set! count (+ count 1)) count)
       '(a b)))                             ⇒  (1 2)
```

**(append-map _f xs ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Maps _f_ over the elements of the lists _xs ..._, just as in function `map`. However, the results of the applications are appended together to determine the final result. `append-map` uses `append` to append the results together. The dynamic order in which the various applications of _f_ are made is not specified. At least one of the list arguments _xs ..._ must be finite.

This is equivalent to `(apply append (map f xs ...))`.

```scheme
(append-map!
  (lambda (x)
    (list x (- x))) '(1 3 8))
⇒ (1 -1 3 -3 8 -8)
```

**(filter-map _f xs ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

This function works like `map`, but only values differently from `#f` are being included in the resulting list. The dynamic order in which the various applications of _f_ are made is not specified. At least one of the list arguments _xs ..._ must be finite.

```scheme
(filter-map
  (lambda (x)
    (and (number? x) (* x x))) '(a 1 b 3 c 7))
⇒ (1 9 49)
```

**(for-each _f xs ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The arguments to `for-each` _xs ..._ are like the arguments to `map`, but `for-each` calls _proc_ for its side effects rather than for its values. Unlike `map`, `for-each` is guaranteed to call _proc_ on the elements of the lists in order from the first element to the last. If more than one list is given and not all lists have the same length, `for-each` terminates when the shortest list runs out.

**(fold-left _f z xs ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Fundamental list recursion operator applying _f_ to the elements _x1 ... xn_ of list _xs_ in the following way: `(f ... (f (f z x1) x2) ... xn)`. In other words, this function applies _f_ recursively based on the following rules, assuming one list parameter _xs_:

```scheme
(fold-left f z xs)   ⇒  (fold-left f (f z (car xs)) (cdr xs))
(fold-left f z '())  ⇒  z
```

If _n_ list arguments are provided, then function _f_ must take _n + 1_ parameters: one element from each list, and the "seed" or fold state, which is initially _z_ as its very first argument. The `fold-left` operation terminates when the shortest list runs out of values.

```scheme
(fold-left (lambda (x y) (cons y x)) '() '(1 2 3 4))  ⇒  (4 3 2 1)
(define (xcons+ rest a b) (cons (+ a b) rest))
(fold-left xcons+ '() '(1 2 3 4) '(10 20 30 40 50))   ⇒  (44 33 22 11)
```

Please note, compared to function `fold` from library `(srfi 1)`, this function applies the "seed"/fold state always as its first argument to _f_.

**(fold-right _f z xs ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Fundamental list recursion operator applying _f_ to the elements _x1 ... xn_ of list _xs_ in the following way: `(f x1 (f x2 ... (f xn z)))`. In other words, this function applies _f_ recursively based on the following rules, assuming one list parameter _xs_:

```scheme
(fold-right f z xs)               ⇒  (f (car xs) (fold-right f z (cdr xs)))
(fold-right f z '())              ⇒  z
(define (xcons xs x) (cons x xs))
(fold-left xcons '() '(1 2 3 4))  ⇒  (4 3 2 1)
```

If _n_ list arguments _xs ..._ are provided, then function _f_ must take _n + 1_ parameters: one element from each list, and the "seed" or fold state, which is initially _z_. The `fold-right` operation terminates when the shortest list runs out of values.

```scheme
(fold-right (lambda (x l) (if (even? x) (cons x l) l))
            '()
            '(1 2 3 4 5 6))  ⇒  (2 4 6)
```

As opposed to `fold-left`, procedure `fold-right` is not tail-recursive.

**(sort _less xs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a sorted list containing all elements of _xs_ such that for every element _xi_ at position _i_, `(`less _xj xi_`)` returns `#t` for all elements _xj_ at position _j_ where _j < i_.

**(merge _less xs ys_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Merges two lists _xs_ and _ys_ which are both sorted with respect to the total ordering predicate _less_ and returns the result as a list.

**(tabulate _count proc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list with _count_ elements. Element _i_ of the list, where _0 ≤ i < count_, is produced by `(`_proc i_`)`.

```scheme
(tabulate 4 fx1+)  ⇒  (1 2 3 4)
```

**(iota _count_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(iota _count start_)**  
**(iota _count start step_)**  

Returns a list containing the elements `(start start+step ... start+(count-1)*step)`. The _start_ and _step_ parameters default to 0 and 1.

```scheme
(iota 5)         ⇒  (0 1 2 3 4)
(iota 5 0 -0.1)  ⇒  (0 -0.1 -0.2 -0.3 -0.4)
```

## Finding and extracting elements

**(list-tail _xs k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the sublist of list _xs_ obtained by omitting the first _k_ elements. Procedure `list-tail` could be defined by

```scheme
(define (list-tail xs k)
  (if (zero? k) xs (list-tail (cdr xs) (- k 1))))
```

**(list-ref _xs k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the _k_-th element of list _xs_. This is the same as the car of `(list-tail xs k)`.

**(memq _obj xs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(memv _obj xs_)**  
**(member _obj xs_)**  
**(member _obj xs compare_)**  

These procedures return the first sublist of _xs_ whose car is _obj_, where the sublists of _xs_ are the non-empty lists returned by (`list-tail` _xs k_) for _k_ less than the length of _xs_. If _obj_ does not occur in _xs_, then `#f` is returned. The `memq` procedure uses `eq?` to compare _obj_ with the elements of _xs_, while `memv` uses `eqv?` and `member` uses _compare_, if given, and `equal?` otherwise.

**(delq _obj xs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(delv _obj xs_)**   
**(delete _obj xs_)**  
**(delete _obj xs compare_)**  

Returns a copy of list _xs_ with all entries equal to element _obj_ removed. `delq` uses `eq?` to compare `obj` with the elements in list _xs_, `delv` uses `eqv?`, and `delete` uses _compare_ if given, and `equal?` otherwise.

**(assq _obj alist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(assv _obj alist_)**   
**(assoc _obj alist_)**  
**(assoc _obj alist compare_)**  

_alist_ must be an association list, i.e. a list of key/value pairs. This family of procedures finds the first pair in _alist_ whose car field is _obj_, and returns that pair. If no pair in _alist_ has _obj_ as its car, then `#f` is returned. The `assq` procedure uses `eq?` to compare _obj_ with the car fields of the pairs in _alist_, while `assv` uses `eqv?` and `assoc` uses _compare_ if given, and `equal?` otherwise.

```scheme
(define e '((a 1) (b 2) (c 3)))
(assq 'a e)                             ⇒  (a 1)
(assq 'b e)                             ⇒  (b 2)
(assq 'd e)                             ⇒  #f
(assq (list 'a) '(((a)) ((b)) ((c))))   ⇒  #f
(assoc (list 'a) '(((a)) ((b)) ((c))))  ⇒  ((a))
(assq 5 '((2 3) (5 7) (11 13)))	        ⇒  unspecified
(assv 5 '((2 3) (5 7) (11 13)))	        ⇒  (5 7)
```

**(alist-delq _obj alist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(alist-delv _obj alist_)**   
**(alist-delete _obj alist_)**   
**(alist-delete _obj alist compare_)**   

Returns a copy of association list _alist_ with all entries removed whose `car` is equal to element _obj_. `alist-delq` uses `eq?` to compare `obj` with the first elements of all members of list _xs_, `alist-delv` uses `eqv?`, and `alist-delete` uses _compare_ if given, and `equal?` otherwise.

**(key _xs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(key _xs default_)**  

Returns `(car xs)` if _xs_ is a pair, otherwise _default_ is being returned. If _default_ is not provided as an argument, `#f` is used instead.

**(value _xs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(value _xs default_)**  

Returns `(cdr xs)` if _xs_ is a pair, otherwise _default_ is being returned. If _default_ is not provided as an argument, `#f` is used instead.
