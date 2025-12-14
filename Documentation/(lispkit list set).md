# LispKit List Set

Library `(lispkit list set)` provides a simple API for list-based sets, called `lset`. Such sets are simply represented as lists (without duplicate entries) with respect to a given equality relation. Every `lset` procedure is provided as its first argument such an equality predicate. It is up to the client of the API to make sure that equality predicate and the given list-based sets are compatible and are used consistently.

An equality predicate `=` is required to be consistent with `eq?`, i.e. it must satisfy `(eq? x y) ⇒ (= x y)`. This implies, in turn, that two lists that are `eq?` are also set-equal by any compliant comparison procedure. This allows for constant-time determination of set operations on `eq?` lists.


**(lset _= x ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list-based set containing all the elements _x ..._ without duplicates when using equality predicate _=_ for comparing elements.

**(list-\>lset _= xs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list-based set containing all the elements of list _xs_ without duplicates when using equality predicate _=_ for comparing elements.

**(lset\<=? _= xs ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` iff every list _xs<sub>i</sub>_ is a subset of list _xs<sub>i+1</sub>_ using equality predicate _=_ for comparing elements, otherwise `#f` is returned. List _A_ is a subset of list _B_ if every element in _A_ is equal to some element of _B_. When performing an element comparison, the _=_ procedure's first argument is an element of _A_, its second argument is an element of _B_.

```scheme
(lset<=? eq? '(a) '(a b a) '(a b c c)) ⇒ #t
(lset<=? eq?) ⇒ #t
(lset<=? eq? '(a b)) ⇒ #t
```

**(lset=? _= xs ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` iff every list _xs<sub>i</sub>_ is set-equal to _xs<sub>i+1</sub>_ using equality predicate _=_ for comparing elements, otherwise `#f` is returned. "Set-equal" simply means that _xs<sub>i</sub>_ is a subset of _xs<sub>i+1</sub>_, and _xs<sub>i+1</sub>_ is a subset of _xs<sub>i</sub>_. When performing an element comparison, the _=_ procedure's first argument is an element of _xs<sub>i</sub>_, its second argument is an element of _xs<sub>i+1</sub>_.

```scheme
(lset=? eq? '(b e a) '(a e b) '(e e b a)) ⇒ #t
(lset=? eq?) ⇒ #t
(lset=? eq? '(a b)) ⇒ #t
```

**(lset-contains? _= xs x_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if element _x_ is contained in _xs_ using equality predicate _=_ for comparing elements. Otherwise, `#f` is returned.

```scheme
(lset-contains? eq? '(a b c) 'b) ⇒ #t
(lset-contains? eq? '(a b c) 'd) ⇒ #f
(lset-contains? eq? '() 'd) ⇒ #f
```

**(lset-adjoin _= xs x ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Adds the elements _x ..._ not already in the list _xs_ and returns the result as a list. The new elements are added to the front of the list, but no guarantees are made about their order. The _=_ parameter is an equality predicate used to determine if an element _x_ is already a member of _xs_. Its first argument is an element of _xs_; its second is one of the _x ..._ elements. _xs_ is always a suffix of the result returned by `lset-adjoin`, even if _xs_ contains repeated elements, these are not reduced.

```scheme
(lset-adjoin eq? '(a b c d c e) 'a 'e 'i 'o) ⇒ (o i a b c d c e)
```

**(lset-union _= xs ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the union of the lists _xs_ using equality predicate _=_ for comparing elements. The union of lists _A_ and _B_ is constructed as follows:

   - If _A_ is the empty list, the answer is _B_
   - Otherwise, the result is initialised to be list _A_
   - Proceed through the elements of list _B_ in a left-to-right order. If _b_ is such an element of _B_, compare every element _r_ of the current result list to _b_: _(= r b)_. If all comparisons fail, _b_ is consed onto the front of the result.  

In the n-ary case, the two-argument list-union operation is simply folded across the argument lists _xs ..._.

```scheme
(lset-union eq? '(a b c d e) '(a e i o)) ⇒ (o i a b c d e)
(lset-union eq? '(a a c) '(x a x)) ⇒ (x a a c)
(lset-union eq?) ⇒ ()
(lset-union eq? '(a b c)) ⇒ (a b c)
```

**(lset-intersection _= xs ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the intersection of the lists _xs_ using equality predicate _=_ for comparing elements.

The intersection of lists _A_ and _B_ is comprised of every element of _A_ that is _=_ to some element of _B_: _(= a b)_, for _a_ in _A_, and _b_ in _B_. This implies that an element which appears in _B_ and multiple times in list _A_ will also appear multiple times in the result.

The order in which elements appear in the result is the same as they appear in _xs<sub>1</sub>_, i.e. `lset-intersection` essentially filters _xs<sub>1</sub>_, without disarranging element order.

In the n-ary case, the two-argument `lset-intersection` operation is simply folded across the argument lists.

```scheme
(lset-intersection eq? '(a b c d e) '(a e i o u)) ⇒ (a e)
(lset-intersection eq? '(a x y a) '(x a x z)) ⇒ (a x a)
(lset-intersection eq? '(a b c)) ⇒ (a b c)
```

**(lset-difference _= xs ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the difference of the lists _xs ..._ using equality predicate _=_ for comparing elements. The result is a list of all the elements of _xs<sub>1</sub>_ that are not _=_ to any element from one of the other _xs<sub>i</sub>_ lists.

The _=_ procedure's first argument is always an element of _xs<sub>1</sub>_ whereas its second argument is an element of one of the other _xs<sub>i</sub>_. Elements that are repeated multiple times in _xs<sub>1</sub>_ will occur multiple times in the result. The order in which elements appear in the result list is the same as they appear in _xs<sub>1</sub>_, i.e. `lset-difference` essentially filters _xs<sub>1</sub>_, without disarranging element order.

```scheme
(lset-difference eq? '(a b c d e) '(a e i o u)) ⇒ (b c d)
(lset-difference eq? '(a b c)) ⇒ (a b c)
```

**(lset-xor _= xs ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the exclusive-or of the list-based sets _xs ..._ using equality predicate _=_ for comparing elements. If there are exactly two lists, this is all the elements that appear in exactly one of the two lists. The operation is associative, and thus extends to the n-ary case.

More precisely, for two lists _A_ and _B_, _A_ "xor" _B_ is a list of 

   - every element _a_ of _A_ such that there is no element _b_ of _B_ such that _(= a b)_, and
   - every element _b_ of _B_ such that there is no element _a_ of _A_ such that _(= b a)_.  

However, an implementation is allowed to assume that _=_ is symmetric, i.e., that _(= a b) ⇒ (= b a)_. This means, e.g. that if a comparison _(= a b)_ returns `#t` for some _a_ in _A_ and _b_ in _B_, both _a_ and _b_ may be removed from inclusion in the result.

In the n-ary case, the binary-xor operation is simply folded across the lists _xs ..._.

```scheme
(lset-xor eq? '(a b c d e) '(a e i o u)) ⇒ (d c b i o u)
(lset-xor eq?) ⇒ ()
(lset-xor eq? '(a b c d e)) ⇒ (a b c d e)
```

**(lset-diff+intersection _= xs ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns two values: the difference and the intersection of the list-based sets _xs ..._ using equality predicate _=_ for comparing elements. Is equivalent to but can be implemented more efficiently than the code below. The _=_ procedure's first argument is an element of _xs<sub>1</sub>_, its second arguments is an element of one of the other _xs<sub>i</sub>_. `lset-diff+intersection` essentially partitions _xs<sub>1</sub>_ into elements that are unique to _xs<sub>1</sub>_ and elements that are shared with other _xs<sub>i</sub>_.

```scheme
(values
  (lset-difference = xs ...)
  (lset-intersection = xs1 (lset-union = xs2 ...)))
```


***

Some of this documentation is derived from [SRFI 1]([https://srfi.schemers.org/srfi-1/srfi-1.html\#lset%3C%3D](https://srfi.schemers.org/srfi-1/srfi-1.html#lset%3C%3D)) by Olin Shivers.
