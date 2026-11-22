# SRFI 231: Intervals and Generalized Arrays

Library `(srfi 231)` implements the multi-dimensional arrays of [SRFI 231](https://srfi.schemers.org/srfi-231/srfi-231.html). An array is a mapping from _multi-indices_ of exact integers i0, ..., id-1 to Scheme values. The valid multi-indices of an array form its _domain_, an _interval_ [l0, u0) × ... × [ld-1, ud-1) of exact integers. d is the _dimension_ of the domain and of the array.

SRFI 231 distinguishes several kinds of arrays:

  - A _generalized array_ is defined by its domain and a _getter_, a procedure that maps the multi-indices of the domain to values. Generalized arrays don't store their elements; the getter computes them on demand. `make-array` creates generalized arrays, and procedures like `array-map` or `array-outer-product` return them.
  - A _mutable array_ has a _setter_ as well, a procedure that changes the element at a given multi-index.
  - A _specialized array_ stores its elements in a _body_, e.g. a vector or a string, and maps multi-indices to positions in the body via an affine _indexer_. A _storage class_ determines the type of the body and which values can be stored in it. Specialized arrays are created by procedures like `make-specialized-array`, `array-copy` or `list->array`. Transformations like `array-extract`, `array-translate`, `array-permute`, `array-reverse`, `array-sample`, `array-curry` and `specialized-array-reshape` return specialized arrays that share the body of their argument, without copying elements.

Arrays, intervals and storage classes are disjoint from all other types of Scheme objects.

```scheme
(import (lispkit base) (srfi 231))

(define a (list*->array 2 '((1 2 3)
                            (4 5 6))))
(array-ref a 1 2)                          ⇒  6
(array->list* (array-permute a '#(1 0)))   ⇒  ((1 4) (2 5) (3 6))
(define row (array-ref (array-curry a 1) 1))
(array->list row)                          ⇒  (4 5 6)
(array-set! row 50 1)
(array->list* a)                           ⇒  ((1 2 3) (4 50 6))
(array->list* (array-map * a a))           ⇒  ((1 4 9) (16 2500 36))
(array-fold-left + 0 a)                    ⇒  66
```

## Arrays in LispKit

The LispKit implementation of SRFI 231 differs from the sample implementation of the SRFI in the following ways:

  - `array-getter` and `array-setter` return the procedures passed to `make-array`; it is up to them to check their arguments. The getters and setters of all other generalized arrays check that the multi-index is in the domain of the array. The getters and setters of safe specialized arrays check the multi-index and the values stored in the array; unsafe specialized arrays don't check them.
  - `specialized-array-share` calls the index mapping only with multi-indices of the new domain: at the lower corner of the domain and at its neighbors along every axis of width greater than one. It is an error if the shared array refers to elements outside of the domain of the original array.
  - LispKit has no homogeneous vectors besides bytevectors. `u8-storage-class` uses bytevectors and `char-storage-class` uses strings as bodies. All other storage classes for numbers use vectors as bodies; their checkers restrict the values that can be stored. `f32-storage-class`, `f16-storage-class` and `c64-storage-class` round the stored values to the precision of the corresponding floating-point format. `f8-storage-class` is `#f`.
  - `array-freeze!` makes the array it is applied to immutable, but not other arrays that share its body.
  - Arrays and storage classes are printed in a compact form, without their elements. Intervals show their lower and upper bounds. Two intervals are `equal?` if they have the same bounds. Two arrays are `equal?` only if they are `eq?`.
  - All procedures whose names do not end with `!` are call/cc safe.

## Miscellaneous procedures

A _translation_ is a vector of exact integers. A _permutation_ of dimension n is a vector whose entries are the exact integers 0, 1, ..., n-1, each occurring once, in any order.

**(translation? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a translation, and `#f` otherwise.

**(permutation? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a permutation, and `#f` otherwise.

**(index-rotate _n k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a permutation that rotates _n_ indices _k_ places to the left. _n_ is a nonnegative exact integer and _k_ an exact integer between 0 and _n_ (inclusive).

**(index-first _n k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a permutation of length _n_ that moves index _k_ to the front and leaves the other indices in order. _n_ is a positive exact integer and _k_ an exact integer between 0 (inclusive) and _n_ (exclusive).

**(index-last _n k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a permutation of length _n_ that moves index _k_ to the end and leaves the other indices in order.

**(index-swap _n i j_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a permutation of length _n_ that swaps index _i_ and index _j_ and leaves the other indices in order.

```scheme
(index-rotate 5 3)    ⇒  #(3 4 0 1 2)
(index-first 5 3)     ⇒  #(3 0 1 2 4)
(index-last 5 3)      ⇒  #(0 1 2 4 3)
(index-swap 5 3 0)    ⇒  #(3 1 2 0 4)
```

## Intervals

An interval represents the set of all multi-indices i0, ..., id-1 of exact integers with l0 ≤ i0 < u0, ..., ld-1 ≤ id-1 < ud-1, where the lower bounds l0, ..., ld-1 and the upper bounds u0, ..., ud-1 are exact integers with lk ≤ uk. The nonnegative integer d is the dimension of the interval. If lk = uk for some k, the interval is empty. If d = 0, the interval is zero-dimensional; it contains a single multi-index, the empty sequence.

**(make-interval _upper-bounds_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-interval _lower-bounds upper-bounds_)**  

Returns a new interval. _lower-bounds_ and _upper-bounds_ are vectors of exact integers of the same length such that each lower bound is less than or equal to the corresponding upper bound. If _lower-bounds_ is not given, the upper bounds must be nonnegative, and all lower bounds are 0.

```scheme
(make-interval '#(3 4))           ⇒  #<interval #(0 0) #(3 4)>
(make-interval '#(1 0) '#(3 4))   ⇒  #<interval #(1 0) #(3 4)>
```

**(interval? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an interval, and `#f` otherwise.

**(interval-dimension _interval_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the dimension of _interval_.

**(interval-lower-bound _interval k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(interval-upper-bound _interval k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(interval-width _interval k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return the lower bound, the upper bound, and the width (upper bound minus lower bound) of _interval_ along axis _k_, where 0 ≤ _k_ < `(interval-dimension interval)`.

**(interval-lower-bounds->list _interval_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(interval-upper-bounds->list _interval_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(interval-lower-bounds->vector _interval_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(interval-upper-bounds->vector _interval_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return the lower or upper bounds of _interval_ as a newly allocated list or vector.

**(interval-widths _interval_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a vector with the widths of _interval_ along all axes.

**(interval-volume _interval_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of multi-indices in _interval_, i.e. the product of its widths. The volume of a zero-dimensional interval is 1.

**(interval-empty? _interval_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the volume of _interval_ is 0, and `#f` otherwise.

```scheme
(define iv (make-interval '#(1 0) '#(3 4)))
(interval-width iv 0)                    ⇒  2
(interval-widths iv)                     ⇒  #(2 4)
(interval-volume iv)                     ⇒  8
(interval-volume (make-interval '#()))   ⇒  1
(interval-empty? (make-interval '#(1 0) '#(1 4)))  ⇒  #t
```

**(interval= _interval1 interval2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _interval1_ and _interval2_ have the same lower and upper bounds, and `#f` otherwise.

**(interval-subset? _interval1 interval2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _interval1_ is contained in _interval2_, and `#f` otherwise. Both intervals must have the same dimension.

**(interval-contains-multi-index? _interval i ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the multi-index _i ..._ is contained in _interval_, and `#f` otherwise. The number of indices must be the dimension of _interval_.

**(interval-projections _interval right-dimension_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Splits the _d_-dimensional _interval_ into two intervals, one with the first _d_ - _right-dimension_ axes and one with the last _right-dimension_ axes, and returns them as two values.

```scheme
(interval-projections (make-interval '#(2 3 1 5 4)) 2)
  ⇒  #<interval #(0 0 0) #(2 3 1)>  #<interval #(0 0) #(5 4)>
```

**(interval-for-each _f interval_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Calls _f_ with each multi-index of _interval_ as arguments, in lexicographical order. If _interval_ is zero-dimensional, _f_ is called once without arguments; if _interval_ is empty, _f_ is never called.

**(interval-fold-left _f op id interval_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(interval-fold-right _f op id interval_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Combine the results of applying _f_ to the multi-indices of _interval_ in lexicographical order with the binary procedure _op_, starting with _id_. `interval-fold-left` computes `(op ... (op (op id x0) x1) ... xn-1)` and alternates evaluations of _f_ and _op_. `interval-fold-right` computes `(op x0 (op x1 ... (op xn-1 id)))` and evaluates _f_ at all multi-indices before applying _op_. Both return _id_ if _interval_ is empty.

```scheme
(interval-fold-left list cons '() (make-interval '#(2 2)))
  ⇒  ((((() 0 0) 0 1) 1 0) 1 1)
(interval-fold-right list cons '() (make-interval '#(2 2)))
  ⇒  ((0 0) (0 1) (1 0) (1 1))
```

**(interval-dilate _interval lower-diffs upper-diffs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new interval whose lower and upper bounds are the bounds of _interval_ plus the translations _lower-diffs_ and _upper-diffs_. It is an error if the result is not a valid interval.

**(interval-intersect _interval ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the intersection of the given intervals, which must have the same dimension, or `#f` if the intersection is not a valid interval.

**(interval-translate _interval translation_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns _interval_ with _translation_ added to its lower and upper bounds.

**(interval-permute _interval permutation_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new interval whose axes are the axes of _interval_ in the order given by _permutation_: axis _j_ of the result is axis `(vector-ref permutation j)` of _interval_.

**(interval-scale _interval scales_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

_interval_ must have lower bounds 0, and _scales_ is a vector of positive exact integers. Returns the interval with lower bounds 0 and upper bounds ⌈uk / sk⌉.

**(interval-cartesian-product _interval ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the cartesian product of the given intervals.

```scheme
(interval-dilate (make-interval '#(100 100)) '#(-1 -1) '#(1 1))
  ⇒  #<interval #(-1 -1) #(101 101)>
(interval-intersect (make-interval '#(2 5) '#(10 7)) (make-interval '#(0 6) '#(8 11)))
  ⇒  #<interval #(2 6) #(8 7)>
(interval-permute (make-interval '#(4 8 21 16)) '#(3 0 1 2))
  ⇒  #<interval #(0 0 0 0) #(16 4 8 21)>
(interval-scale (make-interval '#(4 7)) '#(3 2))
  ⇒  #<interval #(0 0) #(2 4)>
(interval-cartesian-product (make-interval '#(3 4)) (make-interval '#(1) '#(7)))
  ⇒  #<interval #(0 0 1) #(3 4 7)>
```

## Storage classes

A storage class is a set of procedures for managing the body of a specialized array.

**(make-storage-class _getter setter checker maker copier length default data? data->body_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new storage class. `(maker n value)` returns a body with _n_ elements initialized to _value_. `(getter body i)` returns element _i_ of _body_ and `(setter body i value)` sets it to _value_. `(checker value)` returns `#t` if _value_ can be stored in a body. `(copier to at from start end)` copies elements from one body into another; _copier_ may be `#f`. `(length body)` returns the number of elements of _body_. _default_ is the default value of new elements. `(data? data)` returns `#t` if _data_ can be used as a body without copying, and `(data->body data)` returns this body.

**(storage-class? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a storage class, and `#f` otherwise.

**(storage-class-getter _sc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(storage-class-setter _sc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(storage-class-checker _sc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(storage-class-maker _sc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(storage-class-copier _sc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(storage-class-length _sc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(storage-class-default _sc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(storage-class-data? _sc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(storage-class-data->body _sc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return the components of storage class _sc_.

**generic-storage-class** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  

Storage class for arbitrary values, using vectors as bodies. The default value is `#f`.

**char-storage-class** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  

Storage class for characters, using strings as bodies. The default value is `#\null`.

**u1-storage-class** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  
**u8-storage-class** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  
**u16-storage-class** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  
**u32-storage-class** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  
**u64-storage-class** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  

Storage classes for exact integers between 0 and 2^X - 1, for X = 1, 8, 16, 32, 64. `u8-storage-class` uses bytevectors as bodies; the other storage classes use vectors. The default value is 0.

**s8-storage-class** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  
**s16-storage-class** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  
**s32-storage-class** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  
**s64-storage-class** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  

Storage classes for exact integers between -2^(X-1) and 2^(X-1) - 1, for X = 8, 16, 32, 64, using vectors as bodies. The default value is 0.

**f16-storage-class** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  
**f32-storage-class** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  
**f64-storage-class** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  

Storage classes for flonums, using vectors as bodies. `f16-storage-class` and `f32-storage-class` round the stored values to the nearest value of the 16-bit and 32-bit IEEE 754 floating-point formats. The default value is 0.0.

**f8-storage-class** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  

`#f`, since there is no support for 8-bit floating-point numbers.

**c64-storage-class** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  
**c128-storage-class** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  

Storage classes for inexact complex numbers, using vectors as bodies. `c64-storage-class` rounds the real and imaginary parts to 32-bit floating-point numbers. The default value is 0.0+0.0i.

```scheme
(array->list (list->array (make-interval '#(2)) '(1.1 1e40) f32-storage-class))
  ⇒  (1.100000023841858 +inf.0)
(list->array (make-interval '#(2)) '(1 256) u8-storage-class)  ⇒  error
```

## Arrays

**specialized-array-default-safe?** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  

Determines whether new specialized arrays are safe by default. The initial value is `#f`. It is an error to set it to a value that is not a boolean.

**specialized-array-default-mutable?** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  

Determines whether new specialized arrays are mutable by default. The initial value is `#t`. It is an error to set it to a value that is not a boolean.

**(make-array _interval getter_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-array _interval getter setter_)**  

Returns a generalized array with domain _interval_ and getter _getter_. If _setter_ is given, the array is mutable; `(setter v i ...)` sets the element at multi-index _i ..._ to _v_.

```scheme
(define a (make-array (make-interval '#(1 1) '#(11 11))
                      (lambda (i j) (if (= i j) 1 0))))
(array-ref a 3 3)    ⇒  1
(array-ref a 2 3)    ⇒  0
(define b (let ((contents (box 42)))
            (make-array (make-interval '#())
                        (lambda () (unbox contents))
                        (lambda (v) (set-box! contents v)))))
(array-set! b 23)
(array-ref b)        ⇒  23
```

**(array? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an array, and `#f` otherwise.

**(array-domain _array_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(array-getter _array_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return the domain and the getter of _array_.

**(array-dimension _array_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the dimension of the domain of _array_.

**(mutable-array? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a mutable array, and `#f` otherwise.

**(array-setter _array_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the setter of the mutable array _array_.

**(array-freeze! _array_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Makes _array_ immutable and returns it.

**(array-empty? _array_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the domain of _array_ is empty, and `#f` otherwise.

**(array-ref _array i ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the element of _array_ at multi-index _i ..._.

**(array-set! _array obj i ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Stores _obj_ at multi-index _i ..._ of the mutable array _array_. Note that the value precedes the indices.

### Specialized arrays

**(make-specialized-array _interval_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-specialized-array _interval storage-class_)**  
**(make-specialized-array _interval storage-class initial-value_)**  
**(make-specialized-array _interval storage-class initial-value safe?_)**  

Returns a new mutable specialized array with domain _interval_, whose elements are stored in lexicographical order in a body created by _storage-class_ (default: `generic-storage-class`) and initialized to _initial-value_ (default: the default value of _storage-class_). _safe?_ defaults to `(specialized-array-default-safe?)`.

```scheme
(array->list* (make-specialized-array (make-interval '#(2 3)) u8-storage-class 42))
  ⇒  ((42 42 42) (42 42 42))
```

**(make-specialized-array-from-data _data_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-specialized-array-from-data _data storage-class_)**  
**(make-specialized-array-from-data _data storage-class mutable?_)**  
**(make-specialized-array-from-data _data storage-class mutable? safe?_)**  

Returns a one-dimensional specialized array whose body is _data_, without copying it: modifying the array modifies _data_ and vice versa. _data_ must be accepted by `(storage-class-data? storage-class)`: a vector for `generic-storage-class`, a string for `char-storage-class`, a bytevector for `u8-storage-class`, and a vector of suitable values for the other storage classes.

```scheme
(define v (vector 'dog 'cat 'bird))
(define a (make-specialized-array-from-data v))
(array-set! a 'fish 1)
v    ⇒  #(dog fish bird)
```

**(specialized-array? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a specialized array, and `#f` otherwise.

**(array-storage-class _array_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(array-indexer _array_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(array-body _array_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(array-safe? _array_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return the storage class, the indexer (a procedure mapping multi-indices to positions in the body), the body, and the safety of the specialized array _array_.

**(array-packed? _array_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the elements of the specialized array _array_, in lexicographical order, are stored at consecutive, increasing positions of its body, and `#f` otherwise.

```scheme
(define a (make-specialized-array-from-data '#(0 1 2 3)))
(array-packed? a)                        ⇒  #t
(array-packed? (array-reverse a))        ⇒  #f
(array-packed? (array-sample a '#(2)))   ⇒  #f
```

**(specialized-array-share _array new-domain new-domain->old-domain_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new specialized array with domain _new-domain_ that shares the body of the specialized array _array_. _new-domain->old-domain_ is a one-to-one affine mapping, which maps multi-indices of _new-domain_ to multi-indices of the domain of _array_, returning them as multiple values. The new array inherits the safety and mutability of _array_.

```scheme
(define a (array-copy (make-array (make-interval '#(5 10)) list)))
(define b (specialized-array-share a (make-interval '#(5 5))
                                   (lambda (i j) (values i (+ i j)))))
(array->list (array-ref (array-curry b 1) 2))
  ⇒  ((2 2) (2 3) (2 4) (2 5) (2 6))
```

**(specialized-array-reshape _array interval_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(specialized-array-reshape _array interval copy-on-failure?_)**  

Returns a specialized array with domain _interval_ containing the elements of the specialized array _array_ in the same lexicographical order, sharing the body of _array_. _interval_ must have the same volume as the domain of _array_. If the elements of _array_ cannot be mapped to the new domain with an affine indexer, an error is signaled, unless _copy-on-failure?_ is `#t`, in which case the elements are copied.

```scheme
(define a (array-copy (make-array (make-interval '#(3 4)) list)))
(array->list* (specialized-array-reshape a (make-interval '#(4 3))))
  ⇒  (((0 0) (0 1) (0 2)) ((0 3) (1 0) (1 1)) ((1 2) (1 3) (2 0)) ((2 1) (2 2) (2 3)))
(specialized-array-reshape (array-sample a '#(2 1)) (make-interval '#(8)))  ⇒  error
(array->list (specialized-array-reshape (array-sample a '#(2 1)) (make-interval '#(8)) #t))
  ⇒  ((0 0) (0 1) (0 2) (0 3) (2 0) (2 1) (2 2) (2 3))
```

### Copying and transforming arrays

**(array-copy _array_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(array-copy _array storage-class_)**  
**(array-copy _array storage-class mutable?_)**  
**(array-copy _array storage-class mutable? safe?_)**  
**(array-copy! _array ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return a new specialized array with the same domain and elements as _array_. If _array_ is a specialized array, the omitted arguments default to its storage class, mutability and safety; otherwise, they default to `generic-storage-class`, `(specialized-array-default-mutable?)` and `(specialized-array-default-safe?)`. The getter of _array_ is called once per element, in lexicographical order. `array-copy!` is a variant that is not call/cc safe, but faster.

**(array-curry _array inner-dimension_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an immutable array whose domain consists of the first _d_ - _inner-dimension_ axes of the domain of _array_, and whose elements are arrays with the last _inner-dimension_ axes. The element arrays are specialized, mutable or immutable, like _array_, and share its elements.

```scheme
(define a (make-array (make-interval '#(10 10)) list))
(array-ref (array-ref (array-curry a 1) 3) 4)  ⇒  (3 4)
```

**(array-extract _array interval_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an array with domain _interval_, which must be a subset of the domain of _array_, sharing the elements of _array_.

**(array-tile _array s_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Decomposes _array_ into subarrays by cuts perpendicular to its axes and returns an immutable array of these subarrays with lower bounds 0. Component _k_ of the vector _s_ specifies the cuts along axis _k_: either a positive exact integer, the width of all slices (except possibly the last one), or a vector of nonnegative exact integers, the widths of the slices, which must add up to the width of the axis.

```scheme
(define t (list*->array 2 '((1 2 3 4) (5 6 7 8) (9 10 11 12))))
(array->list* (array-map array->list* (array-tile t '#(#(2 1) 3))))
  ⇒  ((((1 2 3) (5 6 7)) ((4) (8)))
      (((9 10 11)) ((12))))
```

**(array-translate _array translation_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an array sharing the elements of _array_ with its domain translated by _translation_.

**(array-permute _array permutation_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an array sharing the elements of _array_ with its axes permuted by _permutation_, as defined by `interval-permute`.

**(array-reverse _array_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(array-reverse _array flip?_)**  

Returns an array sharing the elements of _array_ in which the order of the indices along axis _k_ is reversed if `(vector-ref flip? k)` is true. By default, all axes are reversed.

**(array-sample _array scales_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

_array_ must have lower bounds 0. Returns an array sharing every _sk_-th element of _array_ along each axis _k_.

```scheme
(define a (make-array (make-interval '#(2 3)) list))
(array->list (array-translate a '#(1 -3)))
  ⇒  ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2))
(array-ref (array-translate a '#(1 -3)) 2 -1)   ⇒  (1 2)
(array->list* (array-permute a '#(1 0)))
  ⇒  (((0 0) (1 0)) ((0 1) (1 1)) ((0 2) (1 2)))
(array->list* (array-reverse a '#(#f #t)))
  ⇒  (((0 2) (0 1) (0 0)) ((1 2) (1 1) (1 0)))
(array->list* (array-sample a '#(1 2)))
  ⇒  (((0 0) (0 2)) ((1 0) (1 2)))
```

### Combining arrays

**(array-map _f array1 array2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an immutable array with the domain of the given arrays, which must have the same domains, whose element at a multi-index is _f_ applied to the elements of the arrays at this multi-index. The elements are computed on demand.

**(array-outer-product _op array1 array2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an immutable array whose domain is the cartesian product of the domains of _array1_ and _array2_, and whose element at multi-index _i ... j ..._ is `(op (array-ref array1 i ...) (array-ref array2 j ...))`.

**(array-inner-product _a f g b_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Computes the generalized inner product of the arrays _a_ and _b_, like APL's inner product: the last axis of _a_ and the first axis of _b_ must have the same bounds. The element at multi-index _i ... j ..._ of the result reduces with _f_ the results of applying _g_ to corresponding elements of the row _i ..._ of _a_ and the column _j ..._ of _b_.

```scheme
(array->list* (array-outer-product + (list*->array 1 '(0 10 20)) (list*->array 1 '(1 2))))
  ⇒  ((1 2) (11 12) (21 22))
(array->list* (array-inner-product (list*->array 2 '((1 2) (3 4))) + *
                                   (list*->array 2 '((5 6) (7 8)))))
  ⇒  ((19 22) (43 50))
```

**(array-for-each _f array1 array2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Applies _f_ to the elements of the given arrays, which must have the same domains, in lexicographical order of the multi-indices.

**(array-fold-left _op id array1 array2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(array-fold-right _op id array1 array2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Fold _op_ over the elements of the given arrays, which must have the same domains, in lexicographical order, like `interval-fold-left` and `interval-fold-right`. With more than one array, _op_ receives the corresponding elements of all arrays (and the accumulated value) as arguments.

**(array-reduce _op array_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Combines the elements of the nonempty _array_ with the associative procedure _op_.

```scheme
(define a (make-array (make-interval '#(10)) (lambda (i) i)))
(array-fold-left - 0 a)      ⇒  -45
(array-fold-right - 0 a)     ⇒  -5
(array-fold-right cons '() a)  ⇒  (0 1 2 3 4 5 6 7 8 9)
(array-reduce + a)           ⇒  45
```

**(array-any _pred array1 array2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Applies _pred_ to the elements of the given arrays in lexicographical order and returns the first true result, or `#f`.

**(array-every _pred array1 array2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Applies _pred_ to the elements of the given arrays in lexicographical order, returning `#f` as soon as _pred_ returns `#f`. Otherwise, it returns the result of the last application of _pred_, or `#t` if the arrays are empty.

**(array-assign! _destination source_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Copies the elements of _source_ into the mutable array _destination_, which must have the same domain, in lexicographical order.

```scheme
(define a (array-copy (make-array (make-interval '#(4 4)) (lambda (i j) (* i j)))))
(array-assign! (array-extract a (make-interval '#(2 2) '#(4 4)))
               (make-array (make-interval '#(2 2) '#(4 4)) (lambda (i j) 0)))
(array->list* a)  ⇒  ((0 0 0 0) (0 1 2 3) (0 2 0 0) (0 3 0 0))
```

**(array-stack _k arrays_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(array-stack _k arrays storage-class mutable? safe?_)**  
**(array-stack! _k arrays ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return a new specialized array that stacks the arrays in the list _arrays_, which must have the same domains, along a new axis _k_ with lower bound 0. The optional arguments default to `generic-storage-class`, `(specialized-array-default-mutable?)` and `(specialized-array-default-safe?)`, like for the following procedures.

**(array-append _k arrays_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(array-append _k arrays storage-class mutable? safe?_)**  
**(array-append! _k arrays ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return a new specialized array that concatenates the arrays in the list _arrays_ along axis _k_, setting the lower bound of this axis to 0. The domains of the arrays may only differ along axis _k_.

**(array-decurry _array_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(array-decurry _array storage-class mutable? safe?_)**  
**(array-decurry! _array ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return a new specialized array containing the elements of the elements of the nonempty array of arrays _array_, which must have the same domains. The domain of the result is the cartesian product of the domain of _array_ and the domain of its elements. This is the inverse of `array-curry`.

**(array-block _array_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(array-block _array storage-class mutable? safe?_)**  
**(array-block! _array ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return a new specialized array with lower bounds 0 that assembles the elements of the nonempty array of arrays _array_ as blocks. The element arrays must have the dimension of _array_, and widths that fit together. This is the inverse of `array-tile`.

The variants ending in `!` of these procedures are not call/cc safe, but faster.

```scheme
(define a (list*->array 2 '((1 2) (3 4))))
(define b (list*->array 2 '((5 6) (7 8))))
(array->list* (array-stack 0 (list a b)))   ⇒  (((1 2) (3 4)) ((5 6) (7 8)))
(array->list* (array-append 1 (list a b)))  ⇒  ((1 2 5 6) (3 4 7 8))
(array->list* (array-decurry (array-curry a 1)))  ⇒  ((1 2) (3 4))
(array->list* (array-block (list*->array 2 (list (list a b)))))
  ⇒  ((1 2 5 6) (3 4 7 8))
```

### Conversions

**(array->list _array_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(array->vector _array_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return a newly allocated list or vector with the elements of _array_ in lexicographical order.

**(list->array _interval list_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(list->array _interval list storage-class mutable? safe?_)**  
**(vector->array _interval vector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(vector->array _interval vector storage-class mutable? safe?_)**  

Return a new specialized array with domain _interval_ whose elements are the elements of _list_ or _vector_ in lexicographical order. The length of _list_ or _vector_ must be the volume of _interval_. It is an error if an element cannot be stored by the storage class.

**(array->list* _array_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(array->vector* _array_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return the elements of _array_ as nested lists or vectors, one level of nesting per axis. For a zero-dimensional array, the result is its only element.

**(list*->array _d nested-list_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(list*->array _d nested-list storage-class mutable? safe?_)**  
**(vector*->array _d nested-vector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(vector*->array _d nested-vector storage-class mutable? safe?_)**  

Return a new specialized array of dimension _d_ with lower bounds 0 whose elements are taken from the nested lists or vectors. The nesting must have the shape of an array of dimension _d_.

```scheme
(array->list* (list->array (make-interval '#(2 3)) '(1 2 3 4 5 6)))  ⇒  ((1 2 3) (4 5 6))
(array->vector* (list*->array 2 '((1 2 3) (4 5 6))))   ⇒  #(#(1 2 3) #(4 5 6))
(array->list* (list*->array 0 'foo))                  ⇒  foo
(array-domain (list*->array 2 '(() ())))              ⇒  #<interval #(0 0) #(2 0)>
```
