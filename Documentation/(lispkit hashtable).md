# LispKit Hashtable

Library `(lispkit hashtable)` provides a native implementation of hashtables based on the API defined by R6RS.

A hashtable is a data structure that associates keys with values. Any object can be used as a key, provided a hash function and a suitable equivalence function is available. A hash function is a procedure that maps keys to exact integer objects. It is the programmer’s responsibility to ensure that the hash function is compatible with the equivalence function, which is a procedure that accepts two keys and returns true if they are equivalent and `#f` otherwise. Standard hashtables for arbitrary objects based on the `eq?`, `eqv?`, and `equal?` predicates are provided. Also, hash functions for arbitrary objects, strings, and symbols are included.

The specification below uses the _hashtable_ parameter name for arguments that must be hashtables, and the _key_ parameter name for arguments that must be hashtable keys.

## Constructors

**(make-eq-hashtable)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-eq-hashtable _k_)**  

Returns a newly allocated mutable hashtable that accepts arbitrary objects as keys and compares those keys with `eq?`. If an argument is given, the initial capacity of the hashtable is set to approximately _k_ elements.

**(make-eqv-hashtable)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-eqv-hashtable _k_)**  

Returns a newly allocated mutable hashtable that accepts arbitrary objects as keys and compares those keys with `eqv?`. If an argument is given, the initial capacity of the hashtable is set to approximately _k_ elements.

**(make-equal-hashtable)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-equal-hashtable _k_)**  

Returns a newly allocated mutable hashtable that accepts arbitrary objects as keys and compares those keys with `equal?`. If an argument is given, the initial capacity of the hashtable is set to approximately _k_ elements.

**(make-hashtable _hash equiv_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-hashtable _hash equiv k_)**  

Returns a newly allocated mutable hashtable using _hash_ as the hash function and _equiv_ as the equivalence function for comparing keys. If a third argument _k_ is given, the initial capacity of the hashtable is set to approximately _k_ elements.

_hash_ and _equiv_ must be procedures. _hash_ should accept a key as an argument and should return a non-negative exact integer object. _equiv_ should accept two keys as arguments and return a single boolean value. Neither procedure should mutate the hashtable returned by `make-hashtable`. Both _hash_ and _equiv_ should behave like pure functions on the domain of keys. For example, the `string-hash` and `string=?` procedures are permissible only if all keys are strings and the contents of those strings are never changed so long as any of them continues to serve as a key in the hashtable. Furthermore, any pair of keys for which _equiv_ returns true should be hashed to the same exact integer objects by _hash_.

**(alist->eq-hashtable _alist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(alist->eq-hashtable _alist k_)**  

Returns a newly allocated mutable hashtable consisting of the mappings contained in the association list _alist_. Keys are compared with `eq?`. If argument _k_ is given, the capacity of the returned hashtable is set to at least _k_ elements.

**(alist->eqv-hashtable _alist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(alist->eqv-hashtable _alist k_)**  

Returns a newly allocated mutable hashtable consisting of the mappings contained in the association list _alist_. Keys are compared with `eqv?`. If argument _k_ is given, the capacity of the returned hashtable is set to at least _k_ elements.

**(alist->equal-hashtable _alist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(alist->equal-hashtable _alist k_)**  

Returns a newly allocated mutable hashtable consisting of the mappings contained in the association list _alist_. Keys are compared with `equal?`. If argument _k_ is given, the capacity of the returned hashtable is set to at least _k_ elements.

**(hashtable-copy _hashtable_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(hashtable-copy _hashtable mutable_)**  

Returns a copy of _hashtable_. If the _mutable_ argument is provided and is true, the returned hashtable is mutable; otherwise it is immutable.

**(hashtable-empty-copy _hashtable_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new mutable hashtable that uses the same hash and equivalence functions like _hashtable_.

## Type tests

**(hashtable? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a hashtable. Otherwise, it returns `#f`.

**(eq-hashtable? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a hashtable which uses `eq?` for comparing keys. Otherwise, it returns `#f`.

**(eqv-hashtable? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a hashtable which uses `eqv?` for comparing keys. Otherwise, it returns `#f`.

**(equal-hashtable? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a hashtable which uses `equal?` for comparing keys. Otherwise, it returns `#f`.

## Inspection

**(hashtable-equivalence-function _hashtable_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the equivalence function used by _hashtable_ to compare keys. For hashtables created with `make-eq-hashtable`, `make-eqv-hashtable`, and `make-equal-hashtable`, returns `eq?`, `eqv?`, and `equal?` respectively.

**(hashtable-hash-function _hashtable_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(hashtable-hash-function _hashtable force?_)**   

Returns the hash function used by _hashtable_. For hashtables created by `make-eq-hashtable` and `make-eqv-hashtable`, `#f` is returned. This behavior can be disabled if boolean parameter _force?_ is being provided and set to `#t`. In this case, `hashtable-hash-function` will also return hash functions for `eq` and `eqv`-based hashtables.

**(hashtable-mutable? _hashtable_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if hashtable is mutable, otherwise `#f`.

## Hash functions

The `equal-hash`, `string-hash`, and `string-ci-hash` procedures are acceptable as the hash functions of a hashtable only, if the keys on which they are called are not mutated while they remain in use as keys in the hashtable.

**(equal-hash _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an integer hash value for _obj_, based on its structure and current contents. This hash function is suitable for use with `equal?` as an equivalence function. Like `equal?`, the `equal-hash` procedure must always terminate, even if its arguments contain cycles.

**(eqv-hash _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an integer hash value for _obj_, based on _obj_'s identity. This hash function is suitable for use with `eqv?` as an equivalence function.

**(eq-hash _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an integer hash value for _obj_, based on _obj_'s identity. This hash function is suitable for use with `eq?` as an equivalence function.

**(boolean-hash _b_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an integer hash value for boolean _b_.

**(char-hash _ch_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an integer hash value for character _ch_. This hash function is suitable for use with `char=?` as an equivalence function.

**(char-ci-hash _ch_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an integer hash value for character _ch_, ignoring case. This hash function is suitable for use with `char-ci=?` as an equivalence function.

**(string-hash _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an integer hash value for string _str_, based on its current characters. This hash function is suitable for use with `string=?` as an equivalence function.

**(string-ci-hash _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an integer hash value for string _str_ based on its current characters, ignoring case. This hash function is suitable for use with `string-ci=?` as an equivalence function.

**(symbol-hash _sym_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an integer hash value for symbol _sym_.

**(number-hash _x_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an integer hash value for numeric value _x_.

**(combine-hash _h ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Combines the integer hash values _h ..._ into a single hash value.


## Procedures

**(hashtable-size _hashtable_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of keys contained in hashtable as an exact integer object.

**(hashtable-load _hashtable_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the load factor of the hashtable. The load factor is defined as the ratio between the number of keys and the number of hash buckets of _hashtable_.

**(hashtable-ref _hashtable key default_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the value in _hashtable_ associated with _key_. If _hashtable_ does not contain an association for _key_, _default_ is returned.

**(hashtable-get _hashtable key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a pair consisting of a key matching _key_ and associated value from _hashtable_. If _hashtable_ does not contain an association for _key_, `hashtable-get` returns `#f`.

For example, for a hashtable `ht` containing the mapping `3` to `"three"`, `(hashtable-get ht 3)` will return `(3 . "three")`.

**(hashtable-set! _hashtable key obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Changes _hashtable_ to associate _key_ with _obj_, adding a new association or replacing any existing association for _key_.

**(hashtable-delete! _hashtable key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes any association for _key_ within _hashtable_.

**(hashtable-add! _hashtable key obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Changes _hashtable_ to associate _key_ with _obj_, adding a new association for _key_. The difference to `hashtable-set!` is that existing associations of _key_ will remain in _hashtable_, whereas `hashtable-set!` replaces an existing association for _key_.

**(hashtable-remove! _hashtable key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes the association for _key_ within _hashtable_ which was added last, and returns it as a pair consisting of the key matching _key_ and its associated value. If there is no association of _key_ in _hashtable_, `hashtable-remove!` will return `#f`.

**(alist->hashtable! _hashtable alist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Adds all the associations from _alist_ to _hashtable_ using `hashtable-add!`.

**(hashtable-contains? _hashtable key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _hashtable_ contains an association for _key_, `#f` otherwise.

**(hashtable-update! _hashtable key proc default_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`hashtable-update!` applies _proc_ to the value in _hashtable_ associated with _key_, or to _default_ if _hashtable_ does not contain an association for _key_. The hashtable is then changed to associate _key_ with the value returned by _proc_. _proc_ is a procedure which should accept one argument, it should return a single value, and should not mutate _hashtable_. The behavior of `hashtable-update!` is equivalent to the following code:

```scheme
(hashtable-set! hashtable
                key
                (proc (hashtable-ref hashtable key default)))
```

**(hashtable-clear! _hashtable_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(hashtable-clear! _hashtable k_)**  

Removes all associations from _hashtable_. If a second argument _k_ is given, the current capacity of the hashtable is reset to approximately _k_ elements.

**(hashtable-keys _hashtable_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an immutable vector of all keys in _hashtable_.

**(hashtable-values _hashtable_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an immutable vector of all values in _hashtable_.

**(hashtable-entries _hashtable_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns two values, an immutable vector of the keys in _hashtable_, and an immutable vector of the corresponding values.

**(hashtable-key-list _hashtable_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of all keys in _hashtable_.

**(hashtable-value-list _hashtable_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of all values in _hashtable_.

**(hashtable->alist _hashtable_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of all associations in _hashtable_ as an association list. Each association is represented as a pair consisting of the key and the corresponding value.

**(hashtable-for-each _proc hashtable_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Applies _proc_ to every association in _hashtable_. _proc_ should be a procedure accepting two values, a key and a corresponding value.

**(hashtable-map! _proc hashtable_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Applies _proc_ to every association in _hashtable_. _proc_ should be a procedure accepting two values, a key and a corresponding value, and returning one value. This value and the key will replace the existing binding.

## Composition

**(hashtable-union! _hashtable1 hashtable2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Includes all associations from _hashtable2_ in _hashtable1_ if the key of the association is not already contained in _hashtable1_.

**(hashtable-intersection! _hashtable1 hashtable2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes all associations from _hashtable1_ for which the key of the association is not contained in _hashtable2_.

**(hashtable-difference! _hashtable1 hashtable2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes all associations from _hashtable1_ for which the key of the association is contained in _hashtable2_.

***

Some of this documentation is derived from the [R6RS specification of hash tables](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-14.html#node_chap_13) by Michael Sperber, Kent Dybvig, Matthew Flatt, Anton van Straaten, Richard Kelsey, William Clinger, and Jonathan Rees.
