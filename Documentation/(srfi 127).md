# SRFI 127: Lazy Sequences

Library `(srfi 127)` implements _lazy sequences_ (or _lseqs_) as defined by [SRFI 127](https://srfi.schemers.org/srfi-127/srfi-127.html). Lazy sequences are a generalization of lists: an lseq is either a proper list, or a dotted list whose last cdr is a [SRFI 121](https://srfi.schemers.org/srfi-121/srfi-121.html) generator. A generator is a procedure that can be invoked without arguments to supply the next element of the lseq on demand. It returns an end-of-file object when there are no more elements. Consequently, lseqs cannot reliably contain end-of-file objects. Lseqs are lazier than generators, which do not remember what they have produced, and cheaper than the streams of [SRFI 41](https://srfi.schemers.org/srfi-41/srfi-41.html). They are _odd_: if an lseq is not empty, its first element is always realized. Every list is an lseq, so lseq procedures can also be applied to ordinary lists. The library provides the procedures of SRFI 127 with the same names and behavior. The R7RS (large) library `(scheme lseq)` re-exports all of them.

```scheme
(import (lispkit base) (srfi 127) (srfi 158))

(define nats (generator->lseq (make-iota-generator +inf.0 1)))

(lseq-realize (lseq-take nats 5))                        ⇒  (1 2 3 4 5)
(lseq-realize (lseq-take (lseq-filter even? nats) 5))    ⇒  (2 4 6 8 10)
(lseq-realize (lseq-take (lseq-map (lambda (n) (expt n n)) nats) 5))
  ⇒  (1 4 27 256 3125)
(lseq-ref nats 99)                                       ⇒  100
(lseq-find (lambda (n) (> (* n n) 200)) nats)            ⇒  15

;; Lazily read the lines of a port
(define port (open-input-string "alpha\nbeta\ngamma\n"))
(lseq-realize (generator->lseq (lambda () (read-line port))))
  ⇒  ("alpha" "beta" "gamma")
```

## Lazy sequences in LispKit

Lseqs are built from the immutable pairs of LispKit. The reference implementation of SRFI 127 memoizes the elements of an lseq by destructively replacing the generator in the last cdr with a new pair. This is not possible in LispKit. Instead, the generator at the end of an lseq created by `generator->lseq` (or by any other procedure of this library) is a procedure with a hidden memory: the first time it is asked for the element that follows its pair, it calls the underlying generator and remembers the result. Afterwards, it always returns the same result. This has the following consequences:

  - The result of `generator->lseq` is either the empty list or a pair `(x . tail)`, where `tail` is a procedure. `car`, `cdr`, `pair?`, `null?`, `cons`, and `cons*` can be used with lseqs as usual. For example, `(cons 0 lseq)` prepends an element to `lseq`, and `(cons* 'a 'b lseq)` prepends two elements.
  - `lseq-cdr` (and thus all other procedures) is idempotent, and the underlying generator is invoked at most once for each element. Any number of consumers, including generators created by `lseq->generator`, can traverse the same lseq independently from each other and see the same elements.
  - The `cdr` of a pair is the generator procedure, not the next pair. Use `lseq-cdr` (or `lseq-rest`) to get the next element. An lseq never turns into an ordinary list: `(list? (generator->lseq (generator 1 2 3)))` is `#f`, even if all elements have been realized. Use `lseq-realize` to get an ordinary list.
  - If an lseq is created directly via `(cons x generator)` with an arbitrary generator `generator`, `lseq-cdr` cannot memoize the element which follows `x`: each application of `lseq-cdr` to this pair calls `generator` again. However, the pair that is returned is the start of a regular, memoizing lseq. Use `generator->lseq` to create lseqs from generators.
  - Procedures such as `lseq-take`, `lseq-map` or `lseq->generator` realize only the elements that are needed. They do not look ahead further than the first element which an lseq always has.
  - An lseq retains all elements which have been realized as long as its first pair is referenced. Procedures such as `lseq-length`, `lseq-for-each`, `lseq-find` and `lseq-ref` do not retain the elements they pass, so very long or infinite lseqs can be processed in constant space. If a very long realized prefix (more than a few thousand elements) is retained, then releasing it is limited by the depth of the native stack, like for a long list.

```scheme
(define count 0)
(define l (generator->lseq (lambda () (set! count (+ count 1)) count)))
count                         ⇒  1          ; the first element is realized immediately
(lseq-car (lseq-cdr l))       ⇒  2
(lseq-car (lseq-cdr l))       ⇒  2          ; the generator is not invoked again
(lseq-realize (lseq-take l 4))  ⇒  (1 2 3 4)
count                         ⇒  4
```

## Constructors

**(generator->lseq _generator_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an lseq whose elements are the values returned by _generator_. The procedure invokes _generator_ once and returns the empty list if it returns an end-of-file object. Otherwise, it returns a pair whose car is the value and whose cdr is a procedure that supplies the rest of the lseq (see above). It is an error if _generator_ is not a procedure.

```scheme
(lseq-realize (generator->lseq (generator 1 2 3)))                ⇒  (1 2 3)
(generator->lseq (generator))                                     ⇒  ()
(lseq-realize (cons 0 (generator->lseq (generator 1 2 3))))       ⇒  (0 1 2 3)
(lseq-realize (cons* 'a 'b (generator->lseq (generator 1 2 3))))  ⇒  (a b 1 2 3)
```

## Predicates

**(lseq? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an lseq, i.e. if it is the empty list, or a pair that is the start of a proper list or of a dotted list whose last cdr is a procedure. Since it is not possible to examine whether a procedure can be called without arguments, `lseq?` also returns `#t` if the last cdr is a procedure which requires arguments. Otherwise, `#f` is returned.

```scheme
(lseq? '())                                    ⇒  #t
(lseq? '(1 2 3))                               ⇒  #t
(lseq? (generator->lseq (generator 1 2 3)))    ⇒  #t
(lseq? '(1 . 2))                               ⇒  #f
(lseq? 5)                                      ⇒  #f
```

**(lseq=? _elt=? lseq1 lseq2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the lseqs _lseq1_ and _lseq2_ have the same length and if corresponding elements are equal according to the procedure _elt=?_. The first argument of _elt=?_ is always an element of _lseq1_, and the second argument is an element of _lseq2_. The comparison stops as soon as _elt=?_ returns `#f` or one of the lseqs ends. Both lseqs must be finite if they are equal.

```scheme
(lseq=? = '(1 2 3) (generator->lseq (generator 1 2 3)))   ⇒  #t
(lseq=? = '(1 2 3) '(1 2))                                ⇒  #f
(lseq=? < '(1 2 3) '(2 3 4))                              ⇒  #t
```

## Selectors

**(lseq-car _lseq_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(lseq-first _lseq_)**  

Returns the first element of _lseq_. Both procedures are synonyms of `car` for pairs. It is an error if _lseq_ is empty.

**(lseq-cdr _lseq_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(lseq-rest _lseq_)**  

Returns the lseq which contains all elements of _lseq_ except for the first one. If the cdr of _lseq_ is a procedure, then this procedure supplies the next element, which is realized by the first application of `lseq-cdr` only. Each following application returns the same result. If the cdr of _lseq_ is not a procedure, it is returned unchanged. It is an error if _lseq_ is not a pair.

```scheme
(lseq-cdr '(1 2 3))                                   ⇒  (2 3)
(lseq-cdr '(1))                                       ⇒  ()
(lseq-car (lseq-cdr (generator->lseq (generator 1 2 3))))  ⇒  2
(lseq-cdr '())                                        ⇒  ERROR
```

**(lseq-ref _lseq i_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the _i_-th element of _lseq_, where _i_ is a non-negative integer. This is the same as `(lseq-first (lseq-drop lseq i))`. It is an error if _lseq_ has fewer than _i_ + 1 elements.

```scheme
(lseq-ref '(a b c d) 2)    ⇒  c
(lseq-ref '(a b c d) 4)    ⇒  ERROR
```

**(lseq-take _lseq i_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(lseq-drop _lseq i_)**  

`lseq-take` returns a lazy lseq of the first _i_ elements of _lseq_. `lseq-drop` returns all but the first _i_ elements of _lseq_, and is equivalent to applying `lseq-rest` _i_ times to _lseq_. It is an error if _lseq_ has fewer than _i_ elements. For `lseq-take`, this error is signaled when the missing element is requested.

```scheme
(lseq-realize (lseq-take '(a b c d e) 2))   ⇒  (a b)
(lseq-drop '(a b c d e) 2)                  ⇒  (c d e)
(lseq-realize (lseq-take nats 3))           ⇒  (1 2 3)
(lseq-car (lseq-drop nats 10))              ⇒  11
```

## The whole lazy sequence

**(lseq-realize _lseq_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Realizes all elements of _lseq_ by applying `lseq-cdr` until the generator at the end of _lseq_ (if there is one) is exhausted, and returns a new list containing all elements. This procedure can be applied to an arbitrary lseq before passing it to a procedure which only accepts lists. If the generator never returns an end-of-file object, `lseq-realize` does not return. The original lseq is not changed; it keeps its structure, and all its elements are now realized.

```scheme
(lseq-realize '(1 2 3))                              ⇒  (1 2 3)
(lseq-realize (generator->lseq (generator 1 2 3)))   ⇒  (1 2 3)
```

**(lseq->generator _lseq_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a generator which returns all elements of _lseq_ one by one, including those which have not been realized yet, and an end-of-file object once the lseq is exhausted. The elements are realized when the generator requests them.

```scheme
(define g (lseq->generator '(1 2 3)))
(g)   ⇒  1
(g)   ⇒  2
(g)   ⇒  3
(g)   ⇒  #<eof>
```

**(lseq-length _lseq_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of elements of _lseq_, which must be finite. All elements of _lseq_ are realized.

**(lseq-append _lseq ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an lseq that lazily contains all elements of the given lseqs in order. Without arguments, the result is the empty list. Only the elements of the first lseq are realized as long as the elements of the following lseqs are not requested.

```scheme
(lseq-realize (lseq-append '(1 2 3) (generator->lseq (generator 'a 'b))))  ⇒  (1 2 3 a b)
(lseq-realize (lseq-take (lseq-append '(a b c) nats) 5))                  ⇒  (a b c 1 2)
```

**(lseq-zip _lseq1 lseq2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an lseq each element of which is a list with the corresponding elements of the given lseqs. If any of the lseqs is finite, the result is as long as the shortest one.

```scheme
(lseq-realize (lseq-zip '(one two three) nats (generator->lseq (circular-generator 'odd 'even))))
  ⇒  ((one 1 odd) (two 2 even) (three 3 odd))
(lseq-realize (lseq-zip '(1 2 3)))   ⇒  ((1) (2) (3))
```

## Mapping and filtering

**(lseq-map _proc lseq1 lseq2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Lazily applies _proc_ to the corresponding elements of the lseqs and returns an lseq of the results. _proc_ takes as many arguments as there are lseqs. The result is as long as the shortest lseq. The first element of the result is computed immediately, and every further element when it is requested.

```scheme
(lseq-realize (lseq-map + '(1 2 3) '(4 5 6)))                      ⇒  (5 7 9)
(lseq-realize (lseq-take (lseq-map (lambda (n) (expt n n)) nats) 5))  ⇒  (1 4 27 256 3125)
(lseq-realize (lseq-map (lambda (x) (lseq-car (lseq-cdr x))) '((a b) (d e) (g h))))  ⇒  (b e h)
```

**(lseq-for-each _proc lseq1 lseq2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Applies _proc_ to the corresponding elements of the lseqs for its side effects, from the first to the last element. It returns when the shortest lseq is exhausted. If none of the lseqs is finite, `lseq-for-each` never returns.

```scheme
(lseq-for-each (lambda (x) (display x) (newline)) '(a b c))
```

**(lseq-filter _pred lseq_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(lseq-remove _pred lseq_)**  

`lseq-filter` lazily returns an lseq with the elements of _lseq_ that satisfy _pred_. `lseq-remove` returns an lseq with the elements that do not satisfy _pred_. _pred_ is applied to the elements in order, and the first element of the result is searched immediately.

```scheme
(lseq-realize (lseq-filter odd? (generator->lseq (make-range-generator 1 5))))   ⇒  (1 3)
(lseq-realize (lseq-remove odd? (generator->lseq (make-range-generator 1 5))))   ⇒  (2 4)
(lseq-realize (lseq-take (lseq-filter even? nats) 3))                           ⇒  (2 4 6)
```

## Searching

The following procedures search an lseq from the left. They apply their predicate to the elements in order, and only as often as necessary. They realize as many elements as the search requires.

**(lseq-find _pred lseq_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the first element of _lseq_ that satisfies _pred_, or `#f` if there is no such element. Since the procedure cannot tell elements `#f` from a failed search, use `lseq-find-tail` for lseqs containing `#f`. The search does not return if _lseq_ is infinite and no element satisfies _pred_.

```scheme
(lseq-find even? '(3 1 4 1 5 9 2 6))   ⇒  4
(lseq-find even? '(1 3 5))             ⇒  #f
```

**(lseq-find-tail _pred lseq_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the longest tail of _lseq_ whose first element satisfies _pred_, or `#f` if no element does. `lseq-find-tail` can be seen as a generalization of `lseq-member`.

```scheme
(lseq-find-tail even? '(3 1 37 -8 -5 0 0))   ⇒  (-8 -5 0 0)
(lseq-find-tail even? '(3 1 37 -5))          ⇒  #f
```

**(lseq-take-while _pred lseq_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(lseq-drop-while _pred lseq_)**  

`lseq-take-while` lazily returns the longest initial prefix of _lseq_ whose elements all satisfy _pred_. `lseq-drop-while` drops this prefix and returns the rest of _lseq_. `lseq-drop-while` is `lseq-find-tail` with the opposite sense of the predicate.

```scheme
(lseq-realize (lseq-take-while even? '(2 18 3 10 22 9)))   ⇒  (2 18)
(lseq-drop-while even? '(2 18 3 10 22 9))                  ⇒  (3 10 22 9)
```

**(lseq-any _pred lseq1 lseq2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(lseq-every _pred lseq1 lseq2 ..._)**  

`lseq-any` applies _pred_ to the corresponding elements of the lseqs and returns the first true value returned by _pred_, or `#f` if there is none. `lseq-every` returns the first false value returned by _pred_. If _pred_ never returns `#f`, it returns the last value returned by _pred_, or `#t` if _pred_ was never called. The iteration ends when the shortest lseq is exhausted. _pred_ must take as many arguments as there are lseqs. In contrast to `lseq-find`, `lseq-any` returns the value of the predicate and not the element that satisfied it.

```scheme
(lseq-any integer? '(a 3 b 2.7))               ⇒  #t
(lseq-any integer? '(a 3.1 b 2.7))             ⇒  #f
(lseq-any < '(3 1 4 1 5) '(2 7 1 8 2))         ⇒  #t
(define (factorial n)
  (cond ((< n 0) #f)
        ((= n 0) 1)
        (else (* n (factorial (- n 1))))))
(lseq-any factorial '(-1 -2 3 4))              ⇒  6
(lseq-every factorial '(1 2 3 4))              ⇒  24
(lseq-every factorial '(1 -2 3))               ⇒  #f
(lseq-every even? '())                         ⇒  #t
```

**(lseq-index _pred lseq1 lseq2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the index of the leftmost element for which _pred_ returns a true value, or `#f` if there is none before the shortest lseq is exhausted. The index can be used with `lseq-ref`.

```scheme
(lseq-index even? '(3 1 4 1 5 9))                          ⇒  2
(lseq-index < '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2))           ⇒  1
(lseq-index = '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2))           ⇒  #f
```

**(lseq-member _x lseq_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(lseq-member _x lseq pred_)**  
**(lseq-memq _x lseq_)**  
**(lseq-memv _x lseq_)**  

Return the longest tail of _lseq_ whose first element is _x_ or `#f` if _x_ does not occur in _lseq_. `lseq-memq` compares _x_ with the elements via `eq?`, `lseq-memv` via `eqv?`, and `lseq-member` by default via `equal?`. With the optional argument _pred_, `lseq-member` calls `(pred x e)` for the elements _e_ of _lseq_. So the first argument of _pred_ is always _x_ and the second argument is an element of _lseq_. This makes it possible to find the first element that is greater than 5 with `(lseq-member 5 lseq <)`.

```scheme
(lseq-memq 'b '(a b c))                 ⇒  (b c)
(lseq-memq 'a '(b c d))                 ⇒  #f
(lseq-memv 101 '(100 101 102))          ⇒  (101 102)
(lseq-member (list 'a) '(b (a) c))      ⇒  ((a) c)
(lseq-member 5 '(1 2 5 7 9) <)          ⇒  (7 9)
```

## Library `(scheme lseq)`

Library `(scheme lseq)` is part of the _Red_ edition of R7RS (large). It exports the same 29 bindings as `(srfi 127)`, and these bindings are identical to the ones of `(srfi 127)`. `(scheme red)` also exports them.
