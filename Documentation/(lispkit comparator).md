# LispKit Comparator

Comparators bundle a type test predicate, an equality predicate, an optional ordering predicate, and an optional hash function into a single object. By packaging these procedures together, they can be treated as a single item for use in the implementation of data structures that typically rely on a consistent combination of such functions.

Library `(lispkit comparator)` implements a large part of the API of SRFI 128 and thus, can be used as a drop-in replacement for the core functionality of library `(srfi 128)`. A few procedures and objects from SRFI 162 were adopted as well.

## Comparator objects

Comparators are objects of a distinct type which bundle procedures together that are useful for comparing two objects in a total order. It is an error, if any of the procedures have side effects. There are four procedures in the bundle:

- The _type test predicate_ returns `#t` if its argument has the correct type to be passed as an argument to the other three procedures, and `#f` otherwise.
- The _equality predicate_ returns `#t` if the two objects are the same in the sense of the comparator, and `#f` otherwise. It is the programmer's responsibility to ensure that it is reflexive, symmetric, transitive, and can handle any arguments that satisfy the type test predicate.
- The _ordering predicate_ returns `#t` if the first object precedes the second in a total order, and `#f` otherwise. Note that if it is true, the equality predicate must be false. It is the programmer's responsibility to ensure that it is irreflexive, antisymmetric, transitive, and can handle any arguments that satisfy the type test predicate.
- The _hash function_ takes an object and returns an exact non-negative integer. It is the programmer's responsibility to ensure that it can handle any argument that satisfies the type test predicate, and that it returns the same value on two objects if the equality predicate says they are the same (but not necessarily the converse).

It is also the programmer's responsibility to ensure that all four procedures provide the same result whenever they are applied to the same objects in the sense of `eqv?`, unless the objects have been mutated since the last invocation.

Comparator objects are not applicable to circular structure, or to objects containing any of these. Attempts to pass any such objects to any procedure defined here, or to any procedure that is part of a comparator defined here, has undefined behavior.

## Predicates

**(comparator? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if obj is a comparator, and `#f` otherwise.

**(comparator-ordered? _cmp_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if comparator _cmp_ has a supplied ordering predicate, and `#f` otherwise.

**(comparator-hashable? _cmp_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if comparator _cmp_ has a supplied hash function, and `#f` otherwise.

## Constructors

**(make-comparator _test equality ordering hash_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a comparator which bundles the _test_, _equality_, _ordering_, and _hash_ procedures provided as arguments to `make-comparator`. If _ordering_ or _hash_ is `#f`, a procedure is provided that signals an error on application. The predicates `comparator-ordered?` and `comparator-hashable?` will return `#f` in the respective cases.

Here are calls on `make-comparator` that will return useful comparators for standard Scheme types:

- `(make-comparator boolean? boolean=? (lambda (x y) (and (not x) y)) boolean-hash)` will return a comparator for booleans, expressing the ordering `#f` < `#t` and the standard hash function for booleans
- `(make-comparator real? = < (lambda (x) (exact (abs x))))` will return a comparator expressing the natural ordering of real numbers and a plausible hash function
- `(make-comparator string? string=? string<? string-hash)` will return a comparator expressing the implementation's ordering of strings and the standard hash function
- `(make-comparator string? string-ci=? string-ci<? string-ci-hash)` will return a comparator expressing the implementation's case-insensitive ordering of strings and the standard case-insensitive hash function

**(make-pair-comparator _car-comparator cdr-comparator_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

This procedure returns comparators whose functions behave as follows:

- The type test returns `#t` if its argument is a pair, if the car satisfies the type test predicate of _car-comparator_, and the cdr satisfies the type test predicate of _cdr-comparator_
- The equality function returns `#t` if the cars are equal according to _car-comparator_ and the cdrs are equal according to _cdr-comparator_, and `#f` otherwise.
The ordering function first compares the cars of its pairs using the equality predicate of _car-comparator_. If they are not equal, then the ordering predicate of _car-comparator_ is applied to the cars and its value is returned. Otherwise, the predicate compares the cdrs using the equality predicate of _cdr-comparator_. If they are not equal, then the ordering predicate of _cdr-comparator_ is applied to the cdrs and its value is returned
- The hash function computes the hash values of the car and the cdr using the hash functions of _car-comparator_ and _cdr-comparator_ respectively and then hashes them together in an implementation-defined way

**(make-list-comparator _element-comparator type-test empty? head tail_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

This procedure returns comparators whose functions behave as follows:

- The type test returns `#t` if its argument satisfies _type-test_ and the elements satisfy the type test predicate of _element-comparator_
- The total order defined by the equality and ordering functions is _lexicographic_. It is defined as follows:
     - The empty sequence, as determined by calling _empty?_, compares equal to itself
     - The empty sequence compares less than any non-empty sequence
     - Two non-empty sequences are compared by calling the _head_ procedure on each. If the heads are not equal when compared using _element-comparator_, the result is the result of that comparison. Otherwise, the results of calling the _tail_ procedure are compared recursively.
- The hash function computes the hash values of the elements using the hash function of _element-comparator_ and then hashes them together in an implementation-defined way

**(make-vector-comparator _element-comparator type-test length ref_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

This procedure returns comparators whose functions behave as follows:

- The type test returns `#t` if its argument satisfies _type-test_ and the elements satisfy the type test predicate of _element-comparator_.
- The equality predicate returns `#t` if both of the following tests are satisfied in order: the lengths of the vectors are the same in the sense of `=`, and the elements of the vectors are the same in the sense of the equality predicate of _element-comparator_.
- The ordering predicate returns `#t` if the results of applying _length_ to the first vector is less than the result of applying _length_ to the second vector. If the lengths are equal, then the elements are examined pairwise using the ordering predicate of _element-comparator_. If any pair of elements returns `#t`, then that is the result of the list comparator's ordering predicate; otherwise the result is `#f`
- The hash function computes the hash values of the elements using the hash function of _element-comparator_ and then hashes them together in an implementation-defined way

Here is an example, which returns a comparator for byte vectors:

```scheme
(make-vector-comparator
  (make-comparator exact-integer? = < number-hash)
  bytevector?
  bytevector-length
  bytevector-u8-ref)
```

## Default comparators

**eq-comparator** <span style="float:right;text-align:rigth;">[object]</span>  
**eqv-comparator**  
**equal-comparator**  

These objects implement comparators whose functions behave as follows:

- The type test returns `#t` in all cases
- The equality functions are `eq?`, `eqv?`, and `equal?` respectively
- The ordering function is implementation-defined, except that it must conform to the rules for ordering functions. It may signal an error instead.
- The hash functions are `eq-hash`, `eqv-hash`, and `equal-hash` respectively

**boolean-comparator** <span style="float:right;text-align:rigth;">[object]</span>  

`boolean-comparator` is defined as follows:

```scheme
(make-comparator boolean? boolean=? (lambda (x y) (and (not x) y)) boolean-hash))
```

**real-comparator** <span style="float:right;text-align:rigth;">[object]</span>  

`real-comparator` is defined as follows:

```scheme
(make-comparator real? = < number-hash))
```

**char-comparator** <span style="float:right;text-align:rigth;">[object]</span>  

`char-comparator` is defined as follows:

```scheme
(make-comparator char? char=? char<? char-hash))
```

**char-ci-comparator** <span style="float:right;text-align:rigth;">[object]</span>  

`char-ci-comparator` is defined as follows:

```scheme
(make-comparator char? char-ci=? char-ci<? char-ci-hash))
```

**string-comparator** <span style="float:right;text-align:rigth;">[object]</span>  

`string-comparator` is defined as follows:

```scheme
(make-comparator string? string=? string<? string-hash))
```

**string-ci-comparator** <span style="float:right;text-align:rigth;">[object]</span>  

`string-ci-comparator` is defined as follows:

```scheme
(make-comparator string? string-ci=? string-ci<? string-ci-hash))
```

## Accessors and invokers

**(comparator-type-test-predicate _cmp_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the type test predicate of comparator _cmp_.

**(comparator-equality-predicate _cmp_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the equality predicate of comparator _cmp_.

**(comparator-ordering-predicate _cmp_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the ordering predicate of comparator _cmp_.

**(comparator-hash-function _cmp_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the hash function of comparator _cmp_.

**(comparator-test-type _cmp obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Invokes the type test predicate of comparator _cmp_ on _obj_ and returns what it returns. This procedure is convenient than `comparator-type-test-predicate`, but less efficient when the predicate is called repeatedly.

**(comparator-check-type _cmp obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Invokes the type test predicate of comparator _cmp_ on _obj_ and returns `#t` if it returns `#t`, but signals an error otherwise. This procedure is more convenient than `comparator-type-test-predicate`, but less efficient when the predicate is called repeatedly.

**(comparator-hash _cmp obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Invokes the hash function of comparator _cmp_ on _obj_ and returns what it returns. This procedure is more convenient than comparator-hash-function, but less efficient when the function is called repeatedly.

## Comparison predicates

**(=? _cmp object1 object2 object3 ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(\<? _cmp object1 object2 object3 ..._)**  
**(\>? _cmp object1 object2 object3 ..._)**  
**(\<=? _cmp object1 object2 object3 ..._)**  
**(\>=? _cmp object1 object2 object3 ..._)**  

These procedures are analogous to the number, character, and string comparison predicates of Scheme. They allow the convenient use of comparators to handle variable data types.

These procedures apply the equality and ordering predicates of comparator _cmp_ to the _objects_ as follows. If the specified relation returns `#t` for all _objecti_ and _objectj_ where _n_ is the number of objects and _1 <= i < j <= n_, then the procedures return `#t`, but otherwise `#f`. Because the relations are transitive, it suffices to compare each object with its successor. The order in which the values are compared is unspecified.

**(comparator-max _cmp obj1 obj2 ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(comparator-min _cmp obj1 obj2 ..._)**  
**(comparator-max-in-list _cmp list_)**  
**(comparator-min-in-list _cmp list_)**  

These procedures are analogous to `min` and `max` respectively, but may be applied to any orderable objects, not just to real numbers. They apply the ordering procedure of comparator _cmp_ to the objects _obj1 ..._ to find and return a minimal or maximal object. The order in which the values are compared is unspecified. If two objects are equal in the sense of the comparator _cmp_, either may be returned.

The `-in-list` versions of the procedures accept a single list argument.

## Syntax

**(comparator-if\<=\> _obj1 obj2 less-than equal-to greater-than_)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(comparator-if\<=\> _cmp obj1 obj2 less-than equal-to greater-than_)**  

It is an error unless comparator _cmp_ evaluates to a comparator and _obj1_ and _obj2_ evaluate to objects that the comparator can handle. If the ordering predicate returns `#t` when applied to the values of _obj1_ and _obj2_ in that order, then expression _less-than_ is evaluated and its value is returned. If the equality predicate returns `#t` when applied in the same way, then expression _equal-to_ is evaluated and its value is returned. If neither returns `#t`, expression _greater-than_ is evaluated and its value is returned.

If _cmp_ is omitted, `equal-comparator` is used as a default.

**(if\<=\> _obj1 obj2 less-than equal-to greater-than_)** <span style="float:right;text-align:rigth;">[syntax]</span>  

This special form is equivalent to `(comparator-if<=> obj1 obj2 less-than equal-to greater-than)`, i.e. it uses the predicates provided by `equal-comparator` to determine whether expression _less-than_, _equal-to_, or _greater-than_ gets evaluated and its value returned.

***

This documentation was derived from the [SRFI 128](https://srfi.schemers.org/srfi-128/srfi-128.html) and the [SRFI 162](https://srfi.schemers.org/srfi-162/srfi-162.html) specifications by John Cowan.
