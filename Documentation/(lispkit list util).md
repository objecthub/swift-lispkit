# LispKit List Util

Library `(lispkit list util)` provides list utilities that are neither part of R7RS nor of the SRFIs supported by LispKit (e.g. SRFI 1), such as `flatten`, `group-by`, and the combinatorial procedures `permutations`, `combinations` and `cartesian-product`. The library also exports the basic procedures `take` and `drop`, so that it does not depend on any other list library. The procedures only use the standard list operations; all lists are treated as immutable and new lists are returned.

With the exception of `take` and `drop`, procedures for which SRFI 1 (`(srfi 1)`), SRFI 133 (`(srfi 133)`) or SRFI 152 (`(srfi 152)`) already provide an equivalent, such as `delete-duplicates`, `partition`, `append-map`, `last`, `iota`, `vector-count` or `string-join`, are not duplicated by this library.

```scheme
(import (lispkit base) (lispkit list util))

(flatten '(1 (2 (3 . 4)) () 5))              ⇒  (1 2 3 4 5)
(group-by even? '(1 2 3 4 5))                ⇒  ((1 3 5) (2 4))
(combinations '(a b c) 2)                    ⇒  ((a b) (a c) (b c))
```

**(take _lst k_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(drop _lst k_)**  

`take` returns a new list with the first _k_ elements of _lst_. `drop` returns the list that remains after the first _k_ elements of _lst_ have been removed; the result shares its structure with _lst_. It is an error if _lst_ has fewer than _k_ elements. Both procedures behave like the procedures of the same name in SRFI 1, so they can be used interchangeably with them.

```scheme
(take '(a b c d) 2)                          ⇒  (a b)
(drop '(a b c d) 2)                          ⇒  (c d)
```

**(flatten _x_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list with all the non-list elements of the tree _x_ in the order in which they occur. Empty lists are ignored, and the final cdr of an improper list is treated like an element. If _x_ is not a pair, the result is a list with _x_ as its only element (or the empty list if _x_ is the empty list). `flatten` is iterative and handles lists with many thousands of elements.

```scheme
(flatten '(1 (2 (3 . 4)) () 5))              ⇒  (1 2 3 4 5)
(flatten 7)                                  ⇒  (7)
```

**(add-between _lst sep_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list that is like _lst_ but with _sep_ inserted between each pair of consecutive elements.

```scheme
(add-between '(a b c) 'x)                    ⇒  (a x b x c)
```

**(shuffle _lst_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list with the elements of _lst_ in random order. The result is based on a Fisher-Yates shuffle, using LispKit's `random` procedure.

**(list-set _lst k obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(list-update _lst k proc_)**  

Returns a new list that is like _lst_ but where the element at index _k_ (starting with 0) is replaced by _obj_ (`list-set`) or by the result of applying _proc_ to the element (`list-update`). The original list is not changed. It is an error if _k_ is not a valid index of _lst_.

```scheme
(list-set '(a b c) 1 'x)                     ⇒  (a x c)
(list-update '(1 2 3) 2 (lambda (x) (* x 10)))  ⇒  (1 2 30)
```

**(argmin _proc lst_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(argmax _proc lst_)**  

Returns the first element of the non-empty list _lst_ for which _proc_ returns the smallest (`argmin`) or largest (`argmax`) real number. It is an error if _lst_ is empty.

```scheme
(argmin car '((3 a) (1 b) (1 c) (2 d)))      ⇒  (1 b)
(argmax car '((3 a) (1 b) (3 c) (2 d)))      ⇒  (3 a)
```

**(group-by _key lst_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(group-by _key lst same?_)**  

Partitions _lst_ into groups of elements for which procedure _key_ returns equivalent values with respect to equivalence predicate _same?_ (default: `equal?`). The groups are ordered by the first appearance of their key in _lst_, and the elements in each group are ordered like in _lst_.

```scheme
(group-by (lambda (x) (modulo x 3)) '(1 2 3 4 5 6 7))   ⇒  ((1 4 7) (2 5) (3 6))
```

**(check-duplicates _lst_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(check-duplicates _lst same?_)**  

Returns the first element of _lst_ which is equivalent, with respect to equivalence predicate _same?_ (default: `equal?`), to an element before it. Returns `#f` if there is no such element.

```scheme
(check-duplicates '(1 2 3 2 1))              ⇒  2
(check-duplicates '("a" "b" "A") string-ci=?)   ⇒  "A"
```

**(permutations _lst_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of all permutations of _lst_. The permutations are ordered by the positions of their elements in _lst_. Elements are distinguished by position, i.e. a list with _n_ elements has always _n!_ permutations, even if it contains duplicates.

```scheme
(permutations '(1 2 3))    ⇒  ((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
```

**(combinations _lst_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(combinations _lst k_)**  

Returns a list of all combinations of _k_ elements of _lst_. A combination preserves the order of its elements in _lst_. If _k_ is omitted, `combinations` returns all sublists of _lst_ (the _power set_), ordered by size first.

```scheme
(combinations '(a b c) 2)    ⇒  ((a b) (a c) (b c))
(combinations '(a b c))      ⇒  (() (a) (b) (c) (a b) (a c) (b c) (a b c))
```

**(cartesian-product _lst ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the cartesian product of the lists _lst ..._ as a list of lists. Without arguments, the result is a list with the empty list as its only element.

```scheme
(cartesian-product '(1 2) '(a b))   ⇒  ((1 a) (1 b) (2 a) (2 b))
```
