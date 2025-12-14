# LispKit Bitset

Library `(lispkit bitset)` implements bit sets of arbitrary size. Bit sets are mutable objects. The API of library `(lispkit bitset)` provides functionality to create, to inspect, to compose, and to mutate bit sets efficiently.


**(bitset? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a bit set, `#f` otherwise.

**(bitset _i ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new bit set with bits _i ..._ set. Each _i_ is a fixnum referring to one bit in the bit set by its ordinality.

**(list-\>bitset _list_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new bit set with bits specified by _list_. Each element in _list_ is a fixnum referring to one bit in the bit set by its ordinality.

**(fixnum-\>bitset _x_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a bit set with all bits set that are set in fixnum _x_.

**(bitset-copy _bs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a copy of bit set _bs_.

**(bitset-size _bs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of bits set in bit set _bs_.

**(bitset-next _bs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(bitset-next _bs i_)**  

Returns the next bit set in _bs_ following bit _i_. If _i_ is not provided, the first bit set in _bs_ is returned.

**(bitset-empty? _bs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if bit set _bs_ is empty, `#f` otherwise.

**(bitset-disjoint? _bs1 bs2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if bit sets _bs1_ and _bs2_ are disjoint, `#f` otherwise.

**(bitset-subset? _bs1 bs2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if bit set _bs2_ is a subset of bit set _bs2_, `#f` otherwise.

**(bitset-contains? _bs i ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if all bits _i ..._ are set in bit set _bs_, `#f` otherwise.

**(bitset-adjoin! _bs i ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Inserts the bits _i ..._ into bit set _bs_. `bitset-adjoin!` returns _bs_.

**(bitset-adjoin-all! _bs list_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Inserts all the bits specified by _list_ into bit set _bs_. Each element in _list_ is a fixnum referring to one bit in the bit set by its ordinality. `bitset-adjoin-all!` returns _bs_.

**(bitset-delete! _bs i ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes the bits _i ..._ from bit set _bs_. `bitset-delete!` returns _bs_.

**(bitset-delete-all! _bs list_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes all the bits specified by _list_ from bit set _bs_. Each element in _list_ is a fixnum referring to one bit in the bit set by its ordinality. `bitset-delete-all!` returns _bs_.

**(bitset-union! _bs bs1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Computes the union of bit sets _bs_, _bs1 ..._ and stores the result in _bs_. `bitset-union!` returns _bs_.

**(bitset-intersection! _bs bs1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Computes the intersection of bit sets _bs_, _bs1 ..._ and stores the result in _bs_. `bitset-intersection!` returns _bs_.

**(bitset-difference! _bs bs1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Computes the difference between bit sets _bs_ and _bs1 ..._ and stores the result in _bs_. `bitset-difference!` returns _bs_.

**(bitset-xor! _bs bs1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Computes the exclusive disjunction of bit sets _bs_, _bs1 ..._ and stores the result in _bs_. `bitset-xor!` returns _bs_.

**(bitset=? _bs bs1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the bit sets _bs_, _bs1 ..._ are all equal, i.e. have the same bits set. Otherwise `#f` is returned.

**(bitset<? _bs bs1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _bs_ is a proper subset of _bs1_, and _bs1_ is a proper subset of _bs2_, etc. Otherwise `#f` is returned.

**(bitset>? _bs bs1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _bs_ is a proper superset of _bs1_, and _bs1_ is a proper superset of _bs2_, etc. Otherwise `#f` is returned.

**(bitset<=? _bs bs1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _bs_ is a subset of _bs1_, and _bs1_ is a subset of _bs2_, etc. Otherwise `#f` is returned.

**(bitset>=? _bs bs1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _bs_ is a superset of _bs1_, and _bs1_ is a superset of _bs2_, etc. Otherwise `#f` is returned.

**(bitset-\>list _bs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of all bits set in _bs_.

**(bitset-\>fixnum _bs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a fixnum with all bits set which are also set in bit set _bs_. If _bs_ includes bits that cannot be represented by a fixnum, then `bitset->fixnum` returns `#f`.

**(bitset-for-each _proc bs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Invokes _proc_ on each bit set in _bs_ in increasing ordinal order.

**(bitset-fold _proc z bs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The current state is initialized to z, and _proc_ is invoked on each bit of _bs_ in increasing ordinal order and the current state, setting the current state to the result. The algorithm is repeated until all the bits of _bs_ have been processed. Then the current state is returned.

**(bitset-any? _pred bs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if any application of _pred_ to the bits of _bs_ returns true, and `#f` otherwise.

**(bitset-every? _pred bs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if every application of _pred_ to the bits of _bs_ returns true, and `#f` otherwise.

**(bitset-filter _pred bs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new bit set containing the bits from _bs_ that satisfy _pred_.

**(bitset-filter! _pred bs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes all bits from _bs_ for which _pred_ returns `#f`.
