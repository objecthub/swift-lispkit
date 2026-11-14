# LispKit CLOS Support

Library `(lispkit clos support)` provides utility procedures which are used by the implementation of library `(lispkit clos)`. Some of them are also useful when writing code that uses the object system, most notably `getl` for processing initialization arguments in methods of `initialize`.

Since procedures `last` and `every` have a slightly different semantics than procedures of the same name in other libraries (e.g. `(lispkit list)`), it is best to import only the procedures that are needed, as in

```scheme
(import (lispkit base)
        (lispkit clos)
        (only (lispkit clos support) getl))
```

**(getl _args name_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(getl _args name default_)**  

Looks up _name_ in the list of alternating names and values _args_ and returns the value following the first occurrence of _name_. Names are compared with `eq?`. If _name_ is not found, _default_ is returned. If no _default_ is provided in this case, an error is signaled. Procedure `getl` is typically used by methods of `initialize` for looking up initialization arguments.

```scheme
(getl '(a 1 b 2) 'b)       ⇒  2
(getl '(a 1 b 2) 'c 0)     ⇒  0
(getl '(a 1 b 2) 'c)       ⇒  [error] could not find named argument c in argument list (a 1 b 2)
```

**(position-of _obj list_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the position of the first element of _list_ that is `eq?` to _obj_. It is an error if _obj_ is not in _list_.

```scheme
(position-of 'c '(a b c))  ⇒  2
```

**(last _list_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the last element of _list_, or `#f` if _list_ is empty.

```scheme
(last '(1 2 3))            ⇒  3
(last '())                 ⇒  #f
```

**(every _test list ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _test_ returns true for all tuples of corresponding elements of the lists, and `#f` otherwise. The iteration stops at the end of the shortest list. If one of the lists is empty, `every` returns `#t`.

```scheme
(every < '(1 2 3) '(2 3 4))  ⇒  #t
(every < '(1 2) '(2 1))      ⇒  #f
(every < '(1 2 3) '(2 3))    ⇒  #t
```

**(compute-std-cpl _class get-direct-superclasses_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Computes the class precedence list for _class_ by a topological sort over the graph defined by the procedure _get-direct-superclasses_, which returns the list of the direct superclasses of a class. The result is a list of classes, starting with _class_. This procedure is used by the default method of `compute-class-precedence-list` of library `(lispkit clos)`.

```scheme
(import (lispkit clos))
(define <a> (make-class '<a> '()))
(define <b> (make-class '<b> (list <a>) '()))
(map class-name (compute-std-cpl <b> class-direct-superclasses))  ⇒  (<b> <a> <object> <top>)
```
