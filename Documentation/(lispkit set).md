# LispKit Set

Library `(lispkit set)` provides a generic implementation for sets of objects. Its API design is compatible to the R6RS-style API of library `(lispkit hashtable)`.

A set is a data structure for representing collections of objects. Any object can be used as element, provided a hash function and a suitable equivalence function is available. A hash function is a procedure that maps elements to exact integer objects. It is the programmer’s responsibility to ensure that the hash function is compatible with the equivalence function, which is a procedure that accepts two objects and returns true if they are equivalent and `#f` otherwise. Standard sets for arbitrary objects based on the `eq?`, `eqv?`, and `equal?` predicates are provided.


## Constructors

**set-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `set` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all set objects.

**(make-eq-set)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Create a new empty set using `eq?` as equivalence function.

**(make-eqv-set)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Create a new empty set using `eqv?` as equivalence function.

**(make-equal-set)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Create a new empty set using `equal?` as equivalence function.

**(make-set _hash equiv_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-set _hash equiv k_)**  

Create a new empty set using the given hash function _hash_ and equivalence function _equiv_. An initial capacity _k_ can be provided optionally.

**(eq-set _element ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Create a new set using `eq?` as equivalence function. Initialize it with the values _element ..._ .

**(eqv-set _element ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Create a new set using `eqv?` as equivalence function. Initialize it with the values _element ..._ .

**(equal-set _element ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Create a new set using `equal?` as equivalence function. Initialize it with the values _element ..._ .


## Inspection

**(set-equivalence-function _s_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the equivalence function used by set _s_.

**(set-hash-function _s_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the hash function used by set _s_.

**(set-mutable? _s_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if set _s_ is mutable.


## Predicates

**(set? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a set.

**(set-empty? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an empty set.

**(set=? _s1 s2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if set _s1_ and set _s2_ are using the same equivalence function and contain the same elements.

**(disjoint? _s1 s2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if set _s1_ and set _s2_ are disjoint sets.

**(subset? _s1 s2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if set _s1_ is a subset of set _s2_.

**(proper-subset? _s1 s2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if set _s1_ is a proper subset of set _s2_, i.e. _s1_ is a subset of _s2_ and _s1_ is not equivalent to _s2_.

**(set-contains? _s element_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#` if set _s_ contains _element_.

**(set-any? _s proc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns true if there is at least one element in set _s_ for which procedure _proc_ returns true (i.e. not `#f`).

**(set-every? _s proc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns true if procedure _proc_ returns true (i.e. not `#f`) for all elements of set _s_.


## Procedures

**(set-size _s_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of elements in set _s_.

**(set-elements _s_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the elements of set _s_ as a vector.

**(set-copy s)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(set-copy _s mutable_)**

Copies set _s_ creating an immutable copy if `mutable` is set to `#f` or if `mutable` is not provided.

**(set-for-each _s proc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Applies procedure _proc_ to all elements of set _s_ in an undefined order.

**(set-filter _s pred_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates a new set containing the elements of set _s_ for which the procedure _pred_ returns true.

**(set-union _s s1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates a new set containing the union of _s_ with _s1 ..._.

**(set-intersection _s s1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates a new set containing the intersection of _s_ with _s1 ..._.

**(set-difference _s s1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates a new set containing the difference of `s` and the sets in _s1 ..._ .

**(set-\>list _s_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the elements of set _s_ as a list.

**(list-\>eq-set _elements_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates a new set using the equivalence function `eq?` from the values in list _elements_.

**(list-\>eqv-set _elements_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates a new set using the equivalence function `eqv?` from the values in list _elements_.

**(list-\>equal-set _elements_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates a new set using the equivalence function `equal?` from the values in list _elements_.


## Mutators

**(set-adjoin! _s element ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Adds _element ..._ to the set _s_.

**(set-delete! _s element ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Deletes _element ..._ from the set _s_.

**(set-clear! _s_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(set-clear! _s k_)**  

Clears set _s_ and reserves a capacity of _k_ elements if _k_ is provided.

**(list-\>set! _s elements_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Adds the values of list _elements_ to set _s_.

**(set-filter! _s pred_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes all elements from set _s_ for which procedure _pred_ returns `#f`.

**(set-union! _s s1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Stores the union of set _s_ and sets _s1 ..._ in _s_.

**(set-intersection! _s s1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Stores the intersection of set _s_ and the sets _s1 ..._ in _s_.

**(set-difference! _s s1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Stores the difference of set _s_ and the sets _s1 ..._ in _s_.
