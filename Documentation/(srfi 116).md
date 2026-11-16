# SRFI 116: Immutable List Library

Library `(srfi 116)` implements the _immutable list library_ defined by [SRFI 116](https://srfi.schemers.org/srfi-116/srfi-116.html). The library is a variant of [SRFI 1](https://srfi.schemers.org/srfi-1/srfi-1.html) for _immutable pairs_ (_ipairs_) and _immutable lists_ (_ilists_). Procedure names are those of SRFI 1 and R7RS prefixed with `i`, e.g. `imap` or `ifold`; there are only a few functional update procedures, such as `replace-icar`. An _ilist_ is either the empty list or an ipair whose cdr is an ilist. Dotted ilists are ipairs whose last cdr is neither an ipair nor the empty list. The R7RS (large) library `(scheme ilist)` re-exports all procedures of this library.

```scheme
(import (lispkit base) (srfi 116))

(define l (ilist 1 2 3 4 5))

(imap (lambda (x) (* x x)) l)            ⇒  (1 4 9 16 25)
(ifold + 0 l)                            ⇒  15
(ifilter odd? l)                         ⇒  (1 3 5)
(itake l 2)                              ⇒  (1 2)
(idrop l 2)                              ⇒  (3 4 5)
(replace-icar l 'x)                      ⇒  (x 2 3 4 5)
l                                        ⇒  (1 2 3 4 5)
```

## Immutable pairs in LispKit

The pairs of LispKit are already immutable: there is no `set-car!` or `set-cdr!` for them. The only mutable pairs are the ones created by `mcons` (see library `(lispkit box)`). Therefore, ipairs are the ordinary pairs of LispKit, and ilists are ordinary lists. Quoted lists and lists created by `list`, `cons`, `append` and other standard procedures can be used wherever an ilist is expected. This has the following consequences:

  - Most procedures of this library are just SRFI 1 procedures (or R7RS procedures) exported under a different name. For example, `ipair` is `cons`, `icar` is `car`, `ifold` is `fold`, and `ilist?` is `list?`. Consequently, the procedures of this library can be freely mixed with the procedures of `(scheme base)` and `(srfi 1)`.
  - `ipair?` is `pair?`. In particular, `(ipair? (cons 1 2))` is `#t`. SRFI 116 mentions `#f` as the result for such an expression, but this only applies to implementations in which ipairs are distinct from the mutable pairs of the base language.
  - Mutable pairs, i.e. the ones created by `mcons`, are _not_ ipairs: `(ipair? (mcons 1 2))` is `#f`. The conversion procedures `pair->ipair`, `list->ilist`, `tree->itree` and `gtree->itree` convert mutable pairs into ipairs. `ipair->pair` and `ilist->list` return their argument.
  - `iq` does not need to convert its argument: quoted data is immutable.
  - `eq?` compares pairs structurally in LispKit. Thus, procedures such as `imemq` and `iassq` also find equal, but not identical ipairs.
  - Procedures that build new ilists, e.g. `imap` or `ifilter`, allocate new ipairs, as usual. Operations like `replace-icar` create new pairs; they never change their argument.

```scheme
(ipair? '(1 . 2))                 ⇒  #t
(ipair? (mcons 1 2))              ⇒  #f
(ilist? '(1 2 3))                 ⇒  #t
(ilist? (mcons 1 '()))            ⇒  #f
(pair->ipair (mcons 1 2))         ⇒  (1 . 2)
(list->ilist (mcons 1 (mcons 2 '())))  ⇒  (1 2)
(tree->itree (mcons (mcons 1 2) (mcons 3 '())))  ⇒  ((1 . 2) 3)
```

## Syntax

**(iq _obj_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

Returns an ilist containing the given literals _obj_ ... without evaluating them. `(iq a b c)` is equivalent to `'(a b c)`.

```scheme
(iq a b c)               ⇒  (a b c)
(iq (a b) (c . d) 1 "x") ⇒  ((a b) (c . d) 1 "x")
(iq)                     ⇒  ()
```

## Constructors

**(ipair _a_ _d_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(xipair _d_ _a_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`ipair` returns a newly allocated ipair whose icar is _a_ and whose icdr is _d_. `xipair` is the same with swapped arguments. They correspond to `cons` and `xcons`.

```scheme
(ipair 1 2)                 ⇒  (1 . 2)
(ipair 'a (ilist 'b 'c))    ⇒  (a b c)
(xipair 1 2)                ⇒  (2 . 1)
```

**(ilist _obj_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(ipair* _obj_ ... _tail_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`ilist` returns a newly allocated ilist of its arguments. `ipair*` is like `ilist`, but the last argument provides the tail of the result, which is not necessarily an ilist. `ipair*` corresponds to `cons*`.

```scheme
(ilist 1 2 3)         ⇒  (1 2 3)
(ilist)               ⇒  ()
(ipair* 1 2 3)        ⇒  (1 2 . 3)
(ipair* 1 (ilist 2))  ⇒  (1 2)
(ipair* 1)            ⇒  1
```

**(make-ilist _n_ [_fill_])** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an ilist of _n_ elements, each of which is _fill_. If _fill_ is not provided, the contents of the ilist are unspecified.

```scheme
(make-ilist 3 'c)     ⇒  (c c c)
(make-ilist 0 'c)     ⇒  ()
```

**(ilist-tabulate _n_ _proc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an ilist of _n_ elements. Element _i_ of the ilist, where 0 ≤ _i_ < _n_, is the result of applying _proc_ to _i_.

```scheme
(ilist-tabulate 4 (lambda (i) (* i i)))   ⇒  (0 1 4 9)
(ilist-tabulate 4 values)                 ⇒  (0 1 2 3)
```

**(ilist-copy _dilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a copy of the (possibly dotted) ilist _dilist_. Since ipairs cannot be changed, the copy is always equal to the original. If _dilist_ is not a pair, it is returned as is.

```scheme
(ilist-copy (ilist 1 2 3))   ⇒  (1 2 3)
(ilist-copy '(1 2 . 3))      ⇒  (1 2 . 3)
(ilist-copy '())             ⇒  ()
```

**(iiota _count_ [_start_ [_step_]])** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an ilist containing the first _count_ numbers of the arithmetic sequence that begins with _start_ (default 0) and has _step_ (default 1) as its difference.

```scheme
(iiota 5)          ⇒  (0 1 2 3 4)
(iiota 3 1)        ⇒  (1 2 3)
(iiota 4 0 2)      ⇒  (0 2 4 6)
(iiota 3 5 -1)     ⇒  (5 4 3)
```

## Predicates

**(ipair? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(not-ipair? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`ipair?` returns `#t` if _obj_ is an ipair, otherwise `#f`. Mutable pairs created by `mcons` are not ipairs. `not-ipair?` is the negation of `ipair?`.

```scheme
(ipair? (ipair 1 2))     ⇒  #t
(ipair? '(1 2 3))        ⇒  #t
(ipair? '())             ⇒  #f
(ipair? (mcons 1 2))     ⇒  #f
(not-ipair? 'a)          ⇒  #t
(not-ipair? '(a))        ⇒  #f
```

**(ilist? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(proper-ilist? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(dotted-ilist? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`ilist?` and `proper-ilist?` return `#t` if _obj_ is a proper ilist, i.e. the empty list or an ipair whose icdr is a proper ilist. `dotted-ilist?` returns `#t` if _obj_ is a finite object that is neither a proper ilist nor circular; this includes all objects other than ipairs and the empty list.

```scheme
(ilist? '(1 2 3))            ⇒  #t
(proper-ilist? '())          ⇒  #t
(ilist? '(1 2 . 3))          ⇒  #f
(dotted-ilist? '(1 2 . 3))   ⇒  #t
(dotted-ilist? 2)            ⇒  #t
(dotted-ilist? '(1 2 3))     ⇒  #f
(dotted-ilist? '())          ⇒  #f
```

**(null-ilist? _ilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _ilist_ is the empty list and `#f` if it is an ipair. It is an error if _ilist_ is neither.

```scheme
(null-ilist? '())       ⇒  #t
(null-ilist? '(1 2))    ⇒  #f
```

**(ilist= _elt=_ _ilist_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Determines ilist equality, given an element-equality procedure _elt=_. It returns `#t` if all _ilist_ arguments have the same length and corresponding elements are equal according to _elt=_. With fewer than two ilists, the result is `#t`.

```scheme
(ilist= = (iq 1 2 3) (iq 1 2 3))        ⇒  #t
(ilist= = (iq 1 2 3) (iq 1 2 3 4))      ⇒  #f
(ilist= eq? (iq a b) (iq a b) (iq a b)) ⇒  #t
(ilist= string-ci=? '("a" "B") '("A" "b"))  ⇒  #t
```

## Selectors

**(icar _ipair_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(icdr _ipair_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return the icar and the icdr of an ipair.

```scheme
(icar '(1 . 2))    ⇒  1
(icdr '(1 . 2))    ⇒  2
(icdr '(1 2 3))    ⇒  (2 3)
```

**(icaar _ipair_)** ... **(icddddr _ipair_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The 28 compositions of `icar` and `icdr` with up to four levels: `icaar`, `icadr`, `icdar`, `icddr`, `icaaar`, ..., `icdddr`, `icaaaar`, ..., `icddddr`. As usual, `icadr` selects the second element of an ilist, `icddr` skips the first two elements, and so on.

```scheme
(icadr '(1 2 3))         ⇒  2
(icddr '(1 2 3))         ⇒  (3)
(icaar '((a b) c))       ⇒  a
(icdar '((a b) c))       ⇒  (b)
(icaddr '(1 2 3))        ⇒  3
(icdddr '(1 2 3 . 4))    ⇒  4
(icadddr '(1 2 3 4 5))   ⇒  4
```

**(ilist-ref _ilist_ _i_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(ifirst _ilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(isecond _ilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
... **(itenth _ilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`ilist-ref` returns the _i_-th element of _ilist_, where the first element has index 0. `ifirst`, `isecond`, `ithird`, `ifourth`, `ififth`, `isixth`, `iseventh`, `ieighth`, `ininth` and `itenth` return the corresponding element of _ilist_. It is an error if _ilist_ is too short.

```scheme
(ilist-ref '(a b c d) 2)   ⇒  c
(ifirst '(1 2 3))          ⇒  1
(isecond '(1 2 3))         ⇒  2
(ithird '(1 2 3))          ⇒  3
```

**(icar+icdr _ipair_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns two values: the icar and the icdr of _ipair_.

```scheme
(call-with-values (lambda () (icar+icdr '(1 . 2))) list)   ⇒  (1 2)
```

**(itake _dilist_ _i_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(idrop _dilist_ _i_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(ilist-tail _dilist_ _i_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`itake` returns the first _i_ elements of _dilist_ as a new ilist. `idrop` returns all but the first _i_ elements of _dilist_; it shares the structure of _dilist_. `ilist-tail` is a synonym for `idrop`. It is an error if _dilist_ has fewer than _i_ elements.

```scheme
(itake '(a b c d e) 2)      ⇒  (a b)
(idrop '(a b c d e) 2)      ⇒  (c d e)
(ilist-tail '(a b c d e) 2) ⇒  (c d e)
(itake '(1 2 3 . d) 2)      ⇒  (1 2)
(idrop '(1 2 3 . d) 3)      ⇒  d
```

**(itake-right _dilist_ _i_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(idrop-right _dilist_ _i_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`itake-right` returns the last _i_ elements of _dilist_. `idrop-right` returns all but the last _i_ elements. Both procedures also accept dotted ilists.

```scheme
(itake-right '(a b c d e) 2)   ⇒  (d e)
(idrop-right '(a b c d e) 2)   ⇒  (a b c)
(itake-right '(1 2 3 . d) 2)   ⇒  (2 3 . d)
(idrop-right '(1 2 3 . d) 2)   ⇒  (1)
```

**(isplit-at _dilist_ _i_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns two values: the first _i_ elements of _dilist_ and the remainder.

```scheme
(call-with-values (lambda () (isplit-at '(a b c d e) 2)) list)   ⇒  ((a b) (c d e))
```

**(ilast _ipair_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(last-ipair _ipair_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`ilast` returns the last element of the non-empty ilist _ipair_. `last-ipair` returns the last ipair of _ipair_.

```scheme
(ilast '(a b c))       ⇒  c
(last-ipair '(a b c))  ⇒  (c)
```

## Miscellaneous

**(ilength _ilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the length of the proper ilist _ilist_.

```scheme
(ilength '())          ⇒  0
(ilength '(a b c))     ⇒  3
```

**(iappend _ilist_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(iconcatenate _ilists_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`iappend` returns an ilist consisting of the elements of the first _ilist_ followed by the elements of the other ilists. The last argument is not copied and may be an arbitrary object. `iconcatenate` is the same as `iappend`, but takes a single ilist of ilists as its argument.

```scheme
(iappend '(a b) '(c d))       ⇒  (a b c d)
(iappend '(a) '() '(b) '(c))  ⇒  (a b c)
(iappend '(a b) 'c)           ⇒  (a b . c)
(iappend)                     ⇒  ()
(iconcatenate '((a b) (c d))) ⇒  (a b c d)
```

**(ireverse _ilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(iappend-reverse _rev-head_ _tail_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`ireverse` returns the elements of _ilist_ in reverse order. `iappend-reverse` returns `(iappend (ireverse rev-head) tail)`, but is more efficient.

```scheme
(ireverse '(a b c))                 ⇒  (c b a)
(ireverse '(a (b c) d (e (f))))     ⇒  ((e (f)) d (b c) a)
(iappend-reverse '(3 2 1) '(4 5))   ⇒  (1 2 3 4 5)
```

**(izip _ilist_ _ilist2_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(iunzip1 _ilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(iunzip2 _ilist_)**, ..., **(iunzip5 _ilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`izip` combines the elements of the ilists into an ilist of ilists: the _i_-th element of the result consists of the _i_-th elements of all arguments. The result is as long as the shortest argument. `iunzip1` to `iunzip5` are the inverse operations. They take an ilist of ilists and return 1 to 5 values, the first, second, ... elements of each element.

```scheme
(izip '(one two three) '(1 2 3) '(odd even odd))
  ⇒  ((one 1 odd) (two 2 even) (three 3 odd))
(izip '(1 2 3))                  ⇒  ((1) (2) (3))
(iunzip1 '((1) (2) (3)))         ⇒  (1 2 3)
(call-with-values (lambda () (iunzip2 '((1 one) (2 two) (3 three)))) list)
  ⇒  ((1 2 3) (one two three))
```

**(icount _pred_ _ilist_ _ilist2_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Applies _pred_ element-wise to the elements of the ilists and returns the number of times it returned true. With multiple ilists, iteration stops at the end of the shortest.

```scheme
(icount even? '(3 1 4 1 5 9 2 5 6))                    ⇒  3
(icount < '(1 2 4 8) '(2 4 6 8 10 12 14 16))           ⇒  3
```

## Fold, unfold and map

**(ifold _kons_ _knil_ _ilist_ _ilist2_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(ifold-right _kons_ _knil_ _ilist_ _ilist2_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`ifold` combines the elements of the ilists from left to right: it calls _kons_ with the element and the result of the previous call; the first call receives _knil_. `ifold-right` does the same from right to left. With multiple ilists, _kons_ receives one element of each ilist followed by the accumulated value, and iteration stops at the end of the shortest ilist.

```scheme
(ifold + 0 '(1 2 3))               ⇒  6
(ifold ipair '() '(1 2 3))         ⇒  (3 2 1)
(ifold ipair* '() '(a b c) '(1 2 3))  ⇒  (c 3 b 2 a 1)
(ifold-right ipair '() '(1 2 3))   ⇒  (1 2 3)
(ifold-right list '() '(a b) '(c d))  ⇒  (a c (b d ()))
```

**(ipair-fold _kons_ _knil_ _ilist_ _ilist2_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(ipair-fold-right _kons_ _knil_ _ilist_ _ilist2_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Like `ifold` and `ifold-right`, but _kons_ is applied to the successive tails of the ilists instead of their elements.

```scheme
(ipair-fold ipair '() '(a b c))        ⇒  ((c) (b c) (a b c))
(ipair-fold-right ipair '() '(a b c))  ⇒  ((a b c) (b c) (c))
```

**(ireduce _f_ _ridentity_ _ilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(ireduce-right _f_ _ridentity_ _ilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Variants of `ifold` and `ifold-right` for associative operations: if _ilist_ is empty, _ridentity_ is returned; if it has a single element, that element is returned. Otherwise, the first element, respectively the last element, is used as the initial accumulator value, and _f_ is applied to the remaining elements.

```scheme
(ireduce max 0 '(1 3 5 4 2 0))   ⇒  5
(ireduce + 0 '())                ⇒  0
(ireduce - 0 '(1 2 3 4))         ⇒  2
(ireduce-right - 0 '(1 2 3 4))   ⇒  -2
(ireduce-right append '() '((1) (2) (3)))  ⇒  (1 2 3)
```

**(iunfold _stop?_ _mapper_ _successor_ _seed_ [_tail-gen_])** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(iunfold-right _stop?_ _mapper_ _successor_ _seed_ [_tail_])** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`iunfold` is the fundamental recursive ilist constructor. It repeatedly applies _stop?_ to the current seed; if the result is false, the icar of the next ipair is `(mapper seed)` and the next seed is `(successor seed)`. If _stop?_ returns true, the optional _tail-gen_ is applied to the final seed to produce the tail of the ilist (default: the empty list). `iunfold-right` constructs the ilist in the opposite order, starting from the optional _tail_.

```scheme
(iunfold (lambda (x) (> x 5)) (lambda (x) (* x x)) (lambda (x) (+ x 1)) 1)
  ⇒  (1 4 9 16 25)
(iunfold-right zero? (lambda (x) (* x x)) (lambda (x) (- x 1)) 5)
  ⇒  (1 4 9 16 25)
(iunfold null-ilist? icar icdr '(1 2 3))        ⇒  (1 2 3)
(iunfold-right null-ilist? icar icdr '(1 2 3))  ⇒  (3 2 1)
```

**(imap _proc_ _ilist_ _ilist2_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(imap-in-order _proc_ _ilist_ _ilist2_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(ifor-each _proc_ _ilist_ _ilist2_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`imap` applies _proc_ element-wise to the elements of the ilists and returns an ilist of the results. With multiple ilists, iteration stops at the end of the shortest. `imap-in-order` is guaranteed to apply _proc_ in order from the first to the last element. `ifor-each` is like `imap-in-order`, but is called for effect only.

```scheme
(imap + '(1 2 3) '(4 5 6))                   ⇒  (5 7 9)
(imap (lambda (x) (icadr x)) '((a b) (d e))) ⇒  (b e)
(imap + '(1 2 3) '(10 20))                   ⇒  (11 22)
(let ((acc '()))
  (ifor-each (lambda (x) (set! acc (ipair x acc))) '(1 2 3))
  acc)                                       ⇒  (3 2 1)
```

**(iappend-map _proc_ _ilist_ _ilist2_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(ifilter-map _proc_ _ilist_ _ilist2_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(ipair-for-each _proc_ _ilist_ _ilist2_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`iappend-map` maps _proc_ over the ilists and appends the resulting ilists. `ifilter-map` maps _proc_ and keeps only the true results. `ipair-for-each` applies _proc_ to the successive tails of the ilists for effect.

```scheme
(iappend-map (lambda (x) (ilist x (- x))) '(1 3 8))   ⇒  (1 -1 3 -3 8 -8)
(ifilter-map (lambda (x) (and (number? x) (* x x))) '(a 1 b 3 c 7))  ⇒  (1 9 49)
(let ((acc '()))
  (ipair-for-each (lambda (tail) (set! acc (ipair tail acc))) '(a b c))
  acc)                                                ⇒  ((c) (b c) (a b c))
```

## Filtering and partitioning

**(ifilter _pred_ _ilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(iremove _pred_ _ilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(ipartition _pred_ _ilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`ifilter` returns the elements of _ilist_ that satisfy _pred_, `iremove` returns those that do not. `ipartition` returns both ilists as two values.

```scheme
(ifilter even? '(0 7 8 8 43 -4))    ⇒  (0 8 8 -4)
(iremove even? '(0 7 8 8 43 -4))    ⇒  (7 43)
(call-with-values (lambda () (ipartition symbol? '(one 2 3 four five 6))) list)
  ⇒  ((one four five) (2 3 6))
```

## Searching

**(ifind _pred_ _ilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(ifind-tail _pred_ _ilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`ifind` returns the first element of _ilist_ that satisfies _pred_, or `#f`. `ifind-tail` returns the first ipair whose icar satisfies _pred_, or `#f`.

```scheme
(ifind even? '(3 1 4 1 5 9))             ⇒  4
(ifind even? '(1 3 5))                   ⇒  #f
(ifind-tail even? '(3 1 37 -8 -5 0 0))   ⇒  (-8 -5 0 0)
```

**(itake-while _pred_ _ilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(idrop-while _pred_ _ilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(ispan _pred_ _ilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(ibreak _pred_ _ilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`itake-while` returns the longest initial prefix of _ilist_ whose elements all satisfy _pred_. `idrop-while` returns the remainder. `ispan` returns both as two values, and `ibreak` is like `ispan` with the negated predicate.

```scheme
(itake-while even? '(2 18 3 10 22 9))   ⇒  (2 18)
(idrop-while even? '(2 18 3 10 22 9))   ⇒  (3 10 22 9)
(call-with-values (lambda () (ispan even? '(2 18 3 10 22 9))) list)
  ⇒  ((2 18) (3 10 22 9))
(call-with-values (lambda () (ibreak even? '(3 1 4 1 5 9))) list)
  ⇒  ((3 1) (4 1 5 9))
```

**(iany _pred_ _ilist_ _ilist2_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(ievery _pred_ _ilist_ _ilist2_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`iany` applies _pred_ to the elements of the ilists and returns the first true value, or `#f` if there is none. `ievery` returns `#f` as soon as _pred_ returns `#f`; otherwise, it returns the last value of _pred_ (or `#t` for empty ilists).

```scheme
(iany integer? '(a 3 b 2.7))              ⇒  #t
(iany < '(3 1 4 1 5) '(2 7 1 8 2))        ⇒  #t
(ievery integer? '(1 2 3 4 5))            ⇒  #t
(ievery integer? '(1 2 3 4.5 5))          ⇒  #f
(ievery (lambda (x) (and (even? x) x)) '(2 4 6))  ⇒  6
```

**(ilist-index _pred_ _ilist_ _ilist2_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the index of the first element (or the first tuple of elements) that satisfies _pred_, or `#f` if there is none.

```scheme
(ilist-index even? '(3 1 4 1 5 9))                         ⇒  2
(ilist-index < '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2))          ⇒  1
(ilist-index = '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2))          ⇒  #f
```

**(imemq _x_ _ilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(imemv _x_ _ilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(imember _x_ _ilist_ [_=_])** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return the first ipair of _ilist_ whose icar is equivalent to _x_ according to `eq?`, `eqv?`, or _=_ (default `equal?`). They return `#f` if there is no such ipair.

```scheme
(imemq 'b '(a b c))                      ⇒  (b c)
(imemq 'a '(b c d))                      ⇒  #f
(imemv 101 '(100 101 102))               ⇒  (101 102)
(imember '(a) '(b (a) c))                ⇒  ((a) c)
(imember 2.0 '(1 2 3) =)                 ⇒  (2 3)
```

## Deleting

**(idelete _x_ _ilist_ [_=_])** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(idelete-duplicates _ilist_ [_=_])** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`idelete` returns _ilist_ without the elements that are equal to _x_ according to _=_ (default `equal?`). The comparison `(= x element)` is applied with _x_ as first argument. `idelete-duplicates` removes duplicate elements and keeps the first occurrence of each.

```scheme
(idelete 3 '(1 2 3 4 5))                 ⇒  (1 2 4 5)
(idelete 5 '(3 4 5 6 7) <)               ⇒  (3 4 5)
(idelete-duplicates '(a b a c a b c z))  ⇒  (a b c z)
```

## Immutable association lists

An _immutable association list_ (_ialist_) is an ilist of ipairs, mapping keys (the icars) to values (the icdrs).

**(iassq _key_ _ialist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(iassv _key_ _ialist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(iassoc _key_ _ialist_ [_=_])** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return the first ipair of _ialist_ whose icar is equivalent to _key_ according to `eq?`, `eqv?`, or _=_ (default `equal?`), and `#f` if there is none.

```scheme
(iassq 'b '((a . 1) (b . 2) (c . 3)))        ⇒  (b . 2)
(iassq 'd '((a . 1) (b . 2) (c . 3)))        ⇒  #f
(iassv 5 '((2 3) (5 7) (11 13)))             ⇒  (5 7)
(iassoc '(a) '(((a)) ((b)) ((c))))           ⇒  ((a))
(iassoc 5 '((2 3) (5 7) (11 13)) <)          ⇒  (11 13)
```

**(ialist-cons _key_ _datum_ _ialist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(ialist-delete _key_ _ialist_ [_=_])** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`ialist-cons` returns `(ipair (ipair key datum) ialist)`. `ialist-delete` returns _ialist_ without the ipairs whose icar is equal to _key_ according to _=_ (default `equal?`).

```scheme
(ialist-cons 'x 0 '((a . 1) (b . 2)))        ⇒  ((x . 0) (a . 1) (b . 2))
(ialist-delete 'a '((a . 1) (b . 2) (a . 3)))  ⇒  ((b . 2))
(ialist-delete 5 '((2 3) (5 7) (11 13)))     ⇒  ((2 3) (11 13))
```

## Replacement

**(replace-icar _ipair_ _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(replace-icdr _ipair_ _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return a new ipair that is like _ipair_, but with the icar, respectively the icdr, replaced by _obj_. _ipair_ is not changed. It is an error if _ipair_ is not an ipair.

```scheme
(replace-icar '(1 . 2) 'x)       ⇒  (x . 2)
(replace-icdr '(1 . 2) 'x)       ⇒  (1 . x)
(replace-icar '(1 2 3) 'x)       ⇒  (x 2 3)
(replace-icdr '(1 2 3) '(a b))   ⇒  (1 a b)
```

## Conversion

**(pair->ipair _pair_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(ipair->pair _ipair_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`pair->ipair` returns an ipair with the same car and cdr as _pair_. _pair_ is either an ordinary pair, which is returned as is, or a mutable pair created by `mcons`. `ipair->pair` returns its argument, since ipairs are pairs in LispKit.

```scheme
(pair->ipair (mcons 1 2))    ⇒  (1 . 2)
(pair->ipair '(1 . 2))       ⇒  (1 . 2)
(ipair->pair '(1 . 2))       ⇒  (1 . 2)
```

**(list->ilist _flist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(ilist->list _dilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`list->ilist` converts a list whose spine consists of pairs and mutable pairs into an ilist. The elements of the list, and the tail of a dotted list, are not converted. `ilist->list` returns its argument, since ilists are lists in LispKit.

```scheme
(list->ilist (list 1 2 3))                   ⇒  (1 2 3)
(list->ilist (mcons 1 (mcons 2 '())))        ⇒  (1 2)
(list->ilist (mcons 1 (mcons 2 3)))          ⇒  (1 2 . 3)
(ilist->list '(1 2 . 3))                     ⇒  (1 2 . 3)
```

**(tree->itree _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(itree->tree _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(gtree->itree _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(gtree->tree _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`tree->itree` walks a tree of mutable pairs and returns a copy in which all mutable pairs are replaced by ipairs. Other objects are not changed. `gtree->itree` does the same for a _generalized tree_ consisting of a mix of ipairs and mutable pairs and always returns a tree consisting of ipairs only. `itree->tree` returns its argument. `gtree->tree` is a synonym for `gtree->itree`, since ipairs are the pairs of LispKit.

```scheme
(tree->itree (mcons (mcons 1 2) (mcons 3 '())))       ⇒  ((1 . 2) 3)
(gtree->itree (cons (mcons 1 2) (mcons 3 '())))       ⇒  ((1 . 2) 3)
(gtree->itree (list 'a (mcons 'b (mcons 'c '()))))    ⇒  (a (b c))
(itree->tree '((1 . 2) . (3 . 4)))                    ⇒  ((1 . 2) 3 . 4)
```

## Procedure application

**(iapply _proc_ _obj_ ... _ilist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

An analogue of `apply` whose last argument is an ilist. It is equivalent to `(apply proc obj ... (ilist->list ilist))`.

```scheme
(iapply + (ilist 1 2 3))        ⇒  6
(iapply + 1 2 (ilist 3 4 5))    ⇒  15
```

## Comparators

The comparators of this library are [SRFI 128](https://srfi.schemers.org/srfi-128/srfi-128.html) comparators. The library `(srfi 128)` provides the procedures for working with them, e.g. `=?`, `<?`, `comparator-hash` and `comparator-test-type`. Since ilists are ordinary lists, `ilist-comparator` behaves like `list-comparator` from `(srfi 128)`, and `ipair-comparator` like the result of `(make-pair-comparator default-comparator default-comparator)`.

```scheme
(import (lispkit base) (srfi 116) (srfi 128))

(<? ilist-comparator '(1 2) '(1 3))                ⇒  #t
(<? ilist-comparator '() '(1))                     ⇒  #t
(=? ilist-comparator '(1 2 3) (ilist 1 2 3))       ⇒  #t
(<? ipair-comparator '(1 . 9) '(2 . 0))            ⇒  #t
```

**ipair-comparator** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[constant]</span>  

A comparator for ipairs. It compares ipairs by comparing their icars with `default-comparator`. If the icars are equal, their icdrs are compared with `default-comparator`. It is not a procedure.

**ilist-comparator** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[constant]</span>  

A comparator for ilists. Ilists are compared lexicographically, using `default-comparator` for the elements. The empty ilist is equal to itself and less than any non-empty ilist. It is not a procedure.

**(make-ipair-comparator _icar-comparator_ _icdr-comparator_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a comparator for ipairs. Two ipairs are compared first by their icars using _icar-comparator_. If the icars are equal, the icdrs are compared with _icdr-comparator_.

```scheme
(define c (make-ipair-comparator string-comparator real-comparator))
(=? c '("a" . 1) '("a" . 1))      ⇒  #t
(<? c '("a" . 2) '("b" . 1))      ⇒  #t
(<? c '("a" . 1) '("a" . 2))      ⇒  #t
```

**(make-ilist-comparator _comparator_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a comparator for proper ilists whose elements are compared with _comparator_. Ilists are compared lexicographically; a proper prefix of an ilist is less than the ilist.

```scheme
(define c (make-ilist-comparator string-comparator))
(comparator-test-type c '("a" "b"))      ⇒  #t
(comparator-test-type c '("a" 1))        ⇒  #f
(<? c '("a" "b") '("a" "c"))             ⇒  #t
(<? c '("a") '("a" "a"))                 ⇒  #t
```

**(make-improper-ilist-comparator _comparator_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a comparator for arbitrary objects: the empty list precedes all ipairs, which precede all other objects. Ipairs are compared lexicographically; their icars, as well as the objects at the end of dotted ilists, are compared with _comparator_. All other objects are compared with _comparator_, too. The type test of the result succeeds for all ilists, dotted ilists and other objects accepted by _comparator_ whose elements satisfy the type test of _comparator_. The result supports an ordering, respectively hashing, only if _comparator_ does.

```scheme
(define c (make-improper-ilist-comparator real-comparator))
(comparator-test-type c '(1 2 . 3))      ⇒  #t
(comparator-test-type c '(1 2 . a))      ⇒  #f
(=? c '(1 2 . 3) '(1 2 . 3))             ⇒  #t
(<? c '(1 2) '(1 2 . 3))                 ⇒  #t
(<? c '(1 2 . 3) '(1 2 . 4))             ⇒  #t
(<? c '() '(1))                          ⇒  #t
(<? c '(100) 0)                          ⇒  #t
```

**(make-icar-comparator _comparator_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-icdr-comparator _comparator_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return a comparator that compares ipairs only by their icars, respectively their icdrs, using _comparator_. The type test of the result accepts ipairs whose icar, respectively icdr, satisfies the type test of _comparator_.

```scheme
(define c (make-icar-comparator real-comparator))
(=? c '(1 . x) '(1 . y))                 ⇒  #t
(<? c '(1 . z) '(2 . a))                 ⇒  #t
(comparator-test-type c '(x . 1))        ⇒  #f
```

## Library (scheme ilist)

The R7RS (large) library `(scheme ilist)` is part of the Scheme Red edition. It exports exactly the same bindings as `(srfi 116)`, and it is also included in the library `(scheme red)`.
