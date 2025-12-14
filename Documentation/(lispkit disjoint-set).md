# LispKit Disjoint-Set

Library `(lispkit disjoint-set)` implements disjoint sets, a mutable union-find data structure that tracks a set of elements partitioned into disjoint subsets. Disjoint sets are based on hashtables and require the definition of an equality and a hash function.

**disjoint-set-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `disjoint-set` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all disjoint set objects.

**(disjoint-set? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _obj_ is a disjoint set object; otherwise `#f` is returned.

**(make-eq-disjoint-set)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a new empty disjoint set using `eq` as equality and `eq-hash` as hash function.

**(make-eqv-disjoint-set)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a new empty disjoint set using `eqv` as equality and `eqv-hash` as hash function.

**(make-disjoint-set _comparator_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(make-disjoint-set _hash eql_)**

Returns a new empty disjoint set using _eql_ as equality and _hash_ as hash function. Instead of providing two functions, a new disjoint set can also be created based on a _comparator_.

**(disjoint-set-make _dset x_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Adds a new singleton set _x_ to _dset_ if element _x_ does not exist already in disjoint set _dset_.

**(disjoint-set-find _dset x_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(disjoint-set-find _dset x default_)**   

Looks up element _x_ in _dset_ and returns the set in which _x_ is currently contained. Returns _default_ if element _x_ is not found. If _default_ is not provided, `disjoint-set-find` uses `#f` instead.

**(disjoint-set-union _dset x y_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Unifies the sets containing _x_ and _y_ in disjoint set _dset_.

**(disjoint-set-size _dset_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns the number of sets in _dset_.
