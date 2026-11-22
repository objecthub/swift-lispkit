# SRFI 25: Multi-dimensional Array Primitives

Library `(srfi 25)` implements the multi-dimensional arrays of [SRFI 25](https://srfi.schemers.org/srfi-25/srfi-25.html). Arrays are heterogeneous data structures whose elements are indexed by sequences of exact integers of a fixed length. The length of a valid index sequence is the _rank_, or the number of dimensions, of an array. Arrays are disjoint from all other types of Scheme objects; in particular, vectors are not arrays.

The _shape_ of an array consists of bounds for each index. The lower bound _b_ and the upper bound _e_ of a dimension are exact integers with `(<= b e)`. A valid index along the dimension is an exact integer _k_ that satisfies `(<= b k)` and `(< k e)`. Thus, the lower bound is included and the upper bound is excluded, like for `substring`. Every dimension can start at an arbitrary index; the choice between 0 and 1, for instance, is left to the user. The length of an array along a dimension is the difference `(- e b)`. The _size_ of an array is the product of the lengths of its dimensions.

Parts of an array can be shared as other arrays without copying them, via an affine index mapping. This encourages a declarative style of programming, e.g. to access a row, a column, or the diagonal of a matrix, or to transpose a matrix.

```scheme
(import (lispkit base) (srfi 25))

(define m (array (shape 0 2 0 3)
                 'a 'b 'c
                 'd 'e 'f))
(array-rank m)                       ⇒  2
(array-ref m 1 2)                    ⇒  f
(define t (share-array m (shape 0 3 0 2)
                       (lambda (r c) (values c r))))
(array-ref t 2 1)                    ⇒  f
(array-set! t 0 1 'x)
(array-ref m 1 0)                    ⇒  x
```

## Arrays in LispKit

  - Arrays store their elements in a vector, which is shared by all arrays created from it via `share-array`.
  - Indices are always checked. It is an error to access an array with an index that is not valid, or with the wrong number of indices.
  - `share-array` calls the index mapping only with valid indices of the new array: once for its lower corner, i.e. the index consisting of the lower bounds of all dimensions, and once for each dimension of length greater than one, with the index of the lower corner incremented along that dimension. The mapping is never called for an empty array. It is an error if the new array would refer to elements outside of the original array.
  - An array is printed in a compact form, without its elements.
  - SRFI 25 does not specify when arrays are `equal?`. In LispKit, two arrays are `equal?` only if they are `eq?`.

## Shapes

A shape is specified by an even number of exact integers, alternately the lower and upper bounds for the dimensions of an array. A shape is itself an array: the shape of an array of rank _d_ is a _d_ × 2 array where the element at _k_ 0 contains the lower bound for an index along dimension _k_ and the element at _k_ 1 contains the corresponding upper bound. Shapes are zero-based. An array does not depend on the shape from which it was created; mutating the shape later does not affect the array.

**(shape _bound ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a shape. The sequence _bound ..._ must consist of an even number of exact integers that are pairwise not decreasing. Each pair gives the lower and upper bound of a dimension. If the shape is used to specify the dimensions of an array and _bound ..._ is the sequence b0 e0 ... bk ek ... of n pairs of bounds, then a valid index to the array is any sequence j0 ... jk ... of n exact integers where each jk satisfies `(<= bk jk)` and `(< jk ek)`.

```scheme
(define s (shape 1 3 0 4))
(array-rank s)                       ⇒  2
(array-end s 0)                      ⇒  2
(array-end s 1)                      ⇒  2
(array-ref s 0 0)                    ⇒  1
(array-ref s 1 1)                    ⇒  4
(array-rank (make-array (shape)))    ⇒  0
```

## Constructors

**(make-array _shape_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-array _shape obj_)**  

Returns a newly allocated array whose shape is given by _shape_. If _obj_ is provided, then each element is initialized to it. Otherwise the initial contents of each element is unspecified.

```scheme
(define a (make-array (shape 4 5 4 5 4 5)))
(array-set! a 4 4 4 'huuhkaja)
(array-ref a 4 4 4)                  ⇒  huuhkaja
(array-ref (make-array (shape 0 2 0 2) 0) 1 1)  ⇒  0
```

**(array _shape obj ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new array whose shape is given by _shape_ and the initial contents of the elements are _obj ..._ in row major order. It is an error if the number of objects differs from the size of the array.

```scheme
(array-ref (array (shape 0 2 0 3)
                  'uno 'dos 'tres
                  'cuatro 'cinco 'seis)
           1 0)                      ⇒  cuatro
```

**(share-array _array shape proc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new array of shape _shape_ that shares elements of _array_ through _proc_. The procedure _proc_ must implement an affine function that returns indices of _array_ when given indices of the array returned by `share-array`. The affinity requirement for _proc_ means that each value must be a sum of multiples of the arguments passed to _proc_, plus a constant. The new array refers to the same elements as _array_: modifying an element of one array also modifies the element of the other.

```scheme
(define i4
  (let* ((i (make-array (shape 0 4 0 4) 0))
         (d (share-array i (shape 0 4)
                         (lambda (k) (values k k)))))
    (do ((k 0 (+ k 1)))
        ((= k 4))
      (array-set! d k 1))
    i))
(array-ref i4 2 2)                   ⇒  1
(array-ref i4 2 3)                   ⇒  0
```

The following example accesses the rows of matrix `m` in reverse order, using indices starting at 1:

```scheme
(define m (array (shape 0 3 0 2) 1 2 3 4 5 6))
(define r (share-array m (shape 1 4 1 3)
                       (lambda (i j) (values (- 3 i) (- j 1)))))
(array-ref r 1 1)                    ⇒  5
(array-ref r 3 2)                    ⇒  2
```

## Predicates

**(array? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an array, otherwise returns `#f`. Shapes are arrays; vectors are not.

```scheme
(array? (make-array (shape 0 3)))    ⇒  #t
(array? (shape 0 3))                 ⇒  #t
(array? (vector 1 2 3))              ⇒  #f
```

## Accessors

**(array-rank _array_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of dimensions of _array_.

```scheme
(array-rank (make-array (shape 1 2 3 4)))  ⇒  2
```

**(array-start _array k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the lower bound for the index along dimension _k_ of _array_. _k_ must be an exact integer with `(<= 0 k)` and `(< k (array-rank array))`.

**(array-end _array k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the upper bound for the index along dimension _k_ of _array_. _k_ must be an exact integer with `(<= 0 k)` and `(< k (array-rank array))`.

```scheme
(define a (make-array (shape 1 3 -2 2)))
(array-start a 0)                    ⇒  1
(array-end a 0)                      ⇒  3
(array-start a 1)                    ⇒  -2
(array-end a 1)                      ⇒  2
```

**(array-ref _array k ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(array-ref _array index_)**  

Returns the contents of the element of _array_ at index _k ..._. The sequence _k ..._ must be a valid index to _array_. In the second form, _index_ must be either a vector or a 0-based one-dimensional array containing _k ..._.

```scheme
(let ((a (array (shape 4 7 1 2) 3 1 4)))
  (list (array-ref a 4 1)
        (array-ref a (vector 5 1))
        (array-ref a (array (shape 0 2) 6 1))))  ⇒  (3 1 4)
```

**(array-set! _array k ... obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(array-set! _array index obj_)**  

Stores _obj_ in the element of _array_ at index _k ..._. Returns an unspecified value. The sequence _k ..._ must be a valid index to _array_. In the second form, _index_ must be either a vector or a 0-based one-dimensional array containing _k ..._.

```scheme
(define a (make-array (shape 0 2 0 2) 0))
(array-set! a 0 1 'x)
(array-set! a (vector 1 0) 'y)
(list (array-ref a 0 1) (array-ref a 1 0))  ⇒  (x y)
```
