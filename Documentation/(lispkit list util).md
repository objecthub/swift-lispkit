# LispKit List Util

Library `(lispkit list util)` provides list utilities that are neither part of R7RS nor of the SRFIs supported by LispKit, such as `flatten`, `group-by`, and the combinatorial procedures `permutations`, `combinations` and `cartesian-product`. The library also exports `take`, `drop`, and a selection of the most commonly used list procedures of SRFI 1: `take-right`, `drop-right`, `take-while`, `drop-while`, `split-at`, `first`, `second`, `third`, `fourth`, `fifth`, `last`, `find`, `count`, `fold`, `reduce`, `any`, `every`, `delete-duplicates`, `list-index`, and `zip`.

```scheme
(import (lispkit base)
        (lispkit list util))
(flatten '(1 (2 (3 . 4)) () 5)) ⇒  (1 2 3 4 5)
(group-by even? '(1 2 3 4 5))   ⇒  ((1 3 5) (2 4))
(combinations '(a b c) 2)       ⇒  ((a b) (a c) (b c))
```

**(first _lst_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(second _lst_)**  
**(third _lst_)**  
**(fourth _lst_)**  
**(fifth _lst_)**  

Return the first, second, third, fourth, or fifth element of list _lst_. It is an error if _lst_ has fewer elements. Procedures `first`, `second`, `third`, and `fourth` are the procedures `car`, `cadr`, `caddr`, and `cadddr` under a different name.

```scheme
(first '(a b c d e))   ⇒  a
(second '(a b c d e))  ⇒  b
(third '(a b c d e))   ⇒  c
(fourth '(a b c d e))  ⇒  d
(fifth '(a b c d e))   ⇒  e
```

**(last _lst_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the last element of the non-empty list _lst_. It is an error if _lst_ is empty.

```scheme
(last '(a b c d))  ⇒  d
```

**(find _pred lst_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(list-index _pred lst ..._)**  

`find` returns the first element of _lst_ for which _pred_ returns a true value, or `#f` if there is no such element. Since `find` also returns `#f` if the element it found is `#f`, it cannot distinguish between the two cases; `list-index` can be used in such a situation. `list-index` returns the index (starting with 0) of the first element of _lst_ that satisfies _pred_, or `#f` if there is no such element. If _n_ lists _lst1 ... lstn_ are provided to `list-index`, then _pred_ has to take _n_ arguments. It is applied to the elements at the same position of all lists until the shortest list runs out.

```scheme
(find even? '(3 1 4 1 5 9 2))      ⇒  4
(find even? '(3 1 5))              ⇒  #f
(list-index even? '(3 1 4 1 5 9))  ⇒  2
(list-index = '(3 1 4) '(2 7 4))   ⇒  2
(list-index even? '(3 1 5))        ⇒  #f
```

**(count _pred lst ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of elements of _lst_ that satisfy _pred_. If _n_ lists _lst1 ... lstn_ are provided, then _pred_ has to take _n_ arguments. It is applied to the elements at the same position of all lists until the shortest list runs out, and `count` returns the number of applications for which _pred_ returned a true value.

```scheme
(count even? '(3 1 4 1 5 9 2 6))  ⇒  3
(count < '(1 5 3) '(2 4 6 0))     ⇒  2
```

**(any _pred lst ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(every _pred lst ..._)**  

`any` applies _pred_ to the elements of _lst_ from left to right and returns the first true value returned by _pred_. It returns `#f` if _pred_ returns `#f` for all elements. `every` returns `#f` as soon as _pred_ returns `#f` for an element. If _pred_ returns a true value for all elements, `every` returns the value of the last application of _pred_, or `#t` if _lst_ is empty. If _n_ lists _lst1 ... lstn_ are provided, then _pred_ has to take _n_ arguments. It is applied to the elements at the same position of all lists until the shortest list runs out. The last application of _pred_ is a tail call.

In contrast to `any?` and `every?` from library `(lispkit list)`, which always return a boolean, `any` and `every` return the value computed by _pred_.

```scheme
(any (lambda (x) (and (even? x) (* x x))) '(1 3 4 5)) ⇒  16
(any even? '(1 3 5))                                  ⇒  #f
(every (lambda (x) (and (> x 0) (* x 10))) '(1 2 3))  ⇒  30
(every even? '(2 3 4))                                ⇒  #f
(every even? '())                                     ⇒  #t
```

**(fold _kons knil lst ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(reduce _f ridentity lst_)**  

`fold` combines the elements _x1 ... xn_ of list _lst_ from left to right into a single value, starting with the initial value _knil_: `(kons xn ... (kons x2 (kons x1 knil)))`. Procedure _kons_ is called with an element of the list and the value accumulated so far. If _n_ lists _lst1 ... lstn_ are provided, then _kons_ has to take _n + 1_ arguments: one element from each list followed by the accumulated value. `fold` terminates when the shortest list runs out.

`fold` traverses a list in the same order as `fold-left` from library `(lispkit list)`, but it passes the accumulated value as the _last_ argument to _kons_, whereas `fold-left` passes it as the first argument to its procedure. As a consequence, `(fold cons '() lst)` reverses _lst_. For `fold-right`, which is also provided by `(lispkit list)`, procedure _f_ receives the accumulated value as the last argument as well.

`reduce` is a variant of `fold` for associative operations that does not need an initial value if the list is not empty: `(reduce f ridentity lst)` is `(fold f (car lst) (cdr lst))` if _lst_ is not empty, and _ridentity_ otherwise.

```scheme
(fold cons '() '(1 2 3))        ⇒  (3 2 1)
(fold + 0 '(1 2 3 4))           ⇒  10
(fold list 'z '(1 2 3) '(a b))  ⇒  (2 b (1 a z))
(fold-left cons '() '(1 2 3))   ⇒  (((() . 1) . 2) . 3)
(reduce + 0 '(1 2 3 4))         ⇒  10
(reduce max 0 '(3 9 2))         ⇒  9
(reduce + 0 '())                ⇒  0
(reduce - 0 '(1 2 3 4))         ⇒  2
```

**(take _lst k_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(drop _lst k_)**  

`take` returns a new list with the first _k_ elements of _lst_. `drop` returns the list that remains after the first _k_ elements of _lst_ have been removed; the result shares its structure with _lst_. It is an error if _lst_ has fewer than _k_ elements. Both procedures behave like the procedures of the same name in SRFI 1, so they can be used interchangeably with them.

```scheme
(take '(a b c d) 2)  ⇒  (a b)
(drop '(a b c d) 2)  ⇒  (c d)
```

**(take-right _lst k_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(drop-right _lst k_)**  

`take-right` returns the last _k_ elements of _lst_, and `drop-right` returns a new list with all elements of _lst_ except for the last _k_ elements. The result of `take-right` shares its structure with _lst_. Argument _lst_ may be an improper list: `take-right` includes the final cdr in its result, whereas `drop-right` ignores it. It is an error if _lst_ has fewer than _k_ elements.

```scheme
(take-right '(a b c d e) 2)  ⇒  (d e)
(drop-right '(a b c d e) 2)  ⇒  (a b c)
(take-right '(1 2 3 . d) 2)  ⇒  (2 3 . d)
(drop-right '(1 2 3 . d) 2)  ⇒  (1)
```

**(take-while _pred lst_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(drop-while _pred lst_)**  

`take-while` returns a new list with the longest prefix of _lst_ for which all elements satisfy _pred_. `drop-while` returns the list that remains after removing this prefix from _lst_; the result shares its structure with _lst_.

```scheme
(take-while even? '(2 4 5 6))  ⇒  (2 4)
(drop-while even? '(2 4 5 6))  ⇒  (5 6)
(take-while even? '(1 2))      ⇒  ()
```

**(split-at _lst k_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns two values: a new list with the first _k_ elements of _lst_, and the list that remains after removing these elements. The second list shares its structure with _lst_. `(split-at lst k)` is equivalent to `(values (take lst k) (drop lst k))`, but traverses _lst_ only once. It is an error if _lst_ has fewer than _k_ elements.

```scheme
(split-at '(a b c d e) 2)  ⇒  (a b)
                              (c d e)
```

**(delete-duplicates _lst_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(delete-duplicates _lst same?_)**  

Returns a list that is like _lst_ but without duplicates: if several elements of _lst_ are equivalent with respect to equivalence predicate _same?_ (default: `equal?`), only the first of them is kept. The order of the remaining elements is preserved. Procedure _same?_ is always applied as `(same? x y)`, where _x_ is an element that comes before _y_ in _lst_. The running time of `delete-duplicates` is quadratic in the length of _lst_. Procedure `check-duplicates` can be used to only find out whether there is a duplicate.

```scheme
(delete-duplicates '(a b a c a b))
  ⇒  (a b c)
(delete-duplicates '("a" "A" "b") string-ci=?)
  ⇒  ("a" "b")
```

**(zip _lst1 lst2 ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of lists, where the _i_-th list contains the _i_-th elements of all argument lists. The result is as long as the shortest argument list. `(zip lst1 lst2 ...)` is equivalent to `(map list lst1 lst2 ...)`.

```scheme
(zip '(1 2 3) '(a b c))  ⇒  ((1 a) (2 b) (3 c))
(zip '(1 2 3) '(a b))    ⇒  ((1 a) (2 b))
```

**(flatten _x_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list with all the non-list elements of the tree _x_ in the order in which they occur. Empty lists are ignored, and the final cdr of an improper list is treated like an element. If _x_ is not a pair, the result is a list with _x_ as its only element (or the empty list if _x_ is the empty list). `flatten` is iterative and handles lists with many thousands of elements.

```scheme
(flatten '(1 (2 (3 . 4)) () 5))  ⇒  (1 2 3 4 5)
(flatten 7)                      ⇒  (7)
```

**(add-between _lst sep_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list that is like _lst_ but with _sep_ inserted between each pair of consecutive elements.

```scheme
(add-between '(a b c) 'x)  ⇒  (a x b x c)
```

**(shuffle _lst_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list with the elements of _lst_ in random order. The result is based on a Fisher-Yates shuffle, using LispKit's `random` procedure.

**(list-set _lst k obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(list-update _lst k proc_)**  

Returns a new list that is like _lst_ but where the element at index _k_ (starting with 0) is replaced by _obj_ (`list-set`) or by the result of applying _proc_ to the element (`list-update`). The original list is not changed. It is an error if _k_ is not a valid index of _lst_.

```scheme
(list-set '(a b c) 1 'x)                        ⇒  (a x c)
(list-update '(1 2 3) 2 (lambda (x) (* x 10)))  ⇒  (1 2 30)
```

**(argmin _proc lst_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(argmax _proc lst_)**  

Returns the first element of the non-empty list _lst_ for which _proc_ returns the smallest (`argmin`) or largest (`argmax`) real number. It is an error if _lst_ is empty.

```scheme
(argmin car '((3 a) (1 b) (1 c) (2 d)))  ⇒  (1 b)
(argmax car '((3 a) (1 b) (3 c) (2 d)))  ⇒  (3 a)
```

**(group-by _key lst_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(group-by _key lst same?_)**  

Partitions _lst_ into groups of elements for which procedure _key_ returns equivalent values with respect to equivalence predicate _same?_ (default: `equal?`). The groups are ordered by the first appearance of their key in _lst_, and the elements in each group are ordered like in _lst_.

```scheme
(group-by (lambda (x) (modulo x 3)) '(1 2 3 4 5 6 7))
  ⇒  ((1 4 7) (2 5) (3 6))
```

**(check-duplicates _lst_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(check-duplicates _lst same?_)**  

Returns the first element of _lst_ which is equivalent, with respect to equivalence predicate _same?_ (default: `equal?`), to an element before it. Returns `#f` if there is no such element.

```scheme
(check-duplicates '(1 2 3 2 1))                ⇒  2
(check-duplicates '("a" "b" "A") string-ci=?)  ⇒  "A"
```

**(permutations _lst_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of all permutations of _lst_. The permutations are ordered by the positions of their elements in _lst_. Elements are distinguished by position, i.e. a list with _n_ elements has always _n!_ permutations, even if it contains duplicates.

```scheme
(permutations '(1 2 3))
  ⇒  ((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
```

**(combinations _lst_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(combinations _lst k_)**  

Returns a list of all combinations of _k_ elements of _lst_. A combination preserves the order of its elements in _lst_. If _k_ is omitted, `combinations` returns all sublists of _lst_ (the _power set_), ordered by size first.

```scheme
(combinations '(a b c) 2) ⇒  ((a b) (a c) (b c))
(combinations '(a b c))   ⇒  (() (a) (b) (c) (a b) (a c) (b c) (a b c))
```

**(cartesian-product _lst ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the cartesian product of the lists _lst ..._ as a list of lists. Without arguments, the result is a list with the empty list as its only element.

```scheme
(cartesian-product '(1 2) '(a b))  ⇒  ((1 a) (1 b) (2 a) (2 b))
```
