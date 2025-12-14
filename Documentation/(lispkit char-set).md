# LispKit Char-Set

Library `(lispkit char-set)` implements efficient means to represent and manipulate sets of characters. Its design is based on SRFI 14 but the implementation is specific to the definition of characters in LispKit; i.e. library `(lispkit char-set)` assumes that characters are UTF-16 code units. 

As opposed to SRFI 14, it can be assumed that the update procedures ending with "!" are mutating the corresponding character set. This means that clients of these procedures may rely on these procedures performing their functionality in terms of side effects.

In the procedure specifications below, the following conventions are used:

   - A _cs_ parameter is a character set.
   - A _s_ parameter is a string.
   - A _char_ parameter is a character.
   - A _char-list_ parameter is a list of characters.
   - A _pred_ parameter is a unary character predicate procedure, returning either `#t` or `#f` when applied to a character.
   - An _obj_ parameter may be any value at all.

Passing values to procedures with these parameters that do not satisfy these types is an error.

Unless otherwise noted in the specification of a procedure, procedures always return character sets that are distinct from the parameter character sets (unless the procedure mutates a character set and its name ends with "!"). For example, `char-set-adjoin` is guaranteed to provide a fresh character set, even if it is not given any character parameters.

Library `(lispkit char-set)` supports both mutable as well as immutable character sets. Character sets are assumed to be mutable unless they are explicitly specified to be immutable.


## Constants

**char-set:lower-case** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[constant]</span>   
**char-set:upper-case**  
**char-set:title-case**  
**char-set:letter**  
**char-set:digit**  
**char-set:letter+digit**  
**char-set:graphic**  
**char-set:printing**  
**char-set:whitespace**  
**char-set:newlines**  
**char-set:iso-control**  
**char-set:punctuation**  
**char-set:symbol**  
**char-set:hex-digit**  
**char-set:blank**  
**char-set:ascii**  
**char-set:empty**  
**char-set:full**  

Library `(lispkit char-set)` predefines these frequently used immutable character sets.

Note that there may be characters in `char-set:letter` that are neither upper or lower case. The `char-set:whitespaces` character set contains whitespace and newline characters. `char-set:blanks` only contains whitespace (i.e. "blank") characters. `char-set:newlines` only contains newline characters.

## Predicates

**(char-set? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a character set, otherwise returns `#f`.

**(char-set-empty? _cs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the character set _cs_ does not contain any characters, otherwise returns `#f`.

**(char-set=? _cs ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if all the provided character sets _cs ..._ contain the exact same characters; returns `#f` otherwise. For both corner cases, `(char-set=?)` and `(char-set=? cs)`, `char-set=?` returns `#t`.

**(char-set\<=? _cs ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if every character set _cs-i_ is a subset of character set _cs-i+1_; returns `#f` otherwise. For both corner cases, `(char-set<=?)` and `(char-set<=? cs)`, `char-set<=?` returns `#t`.

**(char-set-disjoint? _cs1 cs2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if character sets _cs1_ and _cs2_ are disjoint, i.e. they do not share a single character; returns `#f` otherwise.

**(char-set-contains? _cs char_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if character _char_ is contained in character set _cs_; returns `#f` otherwise.

**(char-set-every? _pred cs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(char-set-any? _pred cs_)**  

The `char-set-every?` procedure returns `#t` if predicate _pred_ returns `#t` for every character in the character set _cs_. Likewise, `char-set-any?` applies _pred_ to every character in character set _cs_, and returns `#t` if there is at least one character for which _pred_ returns `#t`. If no character produces a `#t` value, it returns `#f`. The order in which these procedures sequence through the elements of _cs_ is not specified.

## Constructors

**(char-set _char ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return a newly allocated mutable character set containing the given characters.

**(immutable-char-set _char ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return a newly allocated immutable character set containing the given characters.

**(char-set-copy _cs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(char-set-copy _cs mutable?_)** 

Returns a newly allocated copy of the character set _cs_. The copy is mutable by default unless parameter _mutable?_ is provided and set to `#f`.

**(list->char-set _char-list_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(list->char-set _char-list base-cs_)**  

Return a newly allocated mutable character set containing the characters in the list of characters _char-list_. If character set _base-cs_ is provided, the characters from _base-cs_ are added to it as well.

**(string->char-set _s_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string->char-set _s base-cs_)**  

Return a newly allocated mutable character set containing the characters of the string _s_. If character set _base-cs_ is provided, the characters from _base-cs_ are added to it as well.

**(ucs-range->char-set _lower upper_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(ucs-range->char-set _lower upper base-cs_)**  
**(ucs-range->char-set _lower upper limit base-cs_)**  

Returns a newly allocated mutable character set containing every character whose ISO/IEC 10646 UCS-4 code lies in the half-open range _[lower,upper)_. _lower_ and _upper_ are exact non-negative integers where _lower <= upper <= limit_ is required to hold. _limit_ is either an exact non-negative integer specifying the maximum upper limit, or it is `#t` which specifies the maximum UTF-16 code unit value. If _limit_ is not provided, a very large default is assumed (equivalent to _limit_ being `#f`).

This signature is compatible with the SRFI 16 specification which states that if the requested range includes unassigned UCS values, these are silently ignored. If the requested range includes "private" or "user space" codes, these are passed through transparently. If any code from the requested range specifies a valid, assigned UCS character that has no corresponding representative in the implementation's character type, then

   1. an error is raised if _limit_ is `#t`, and
   2. the code is ignored if _limit_ is `#f` (the default).

If character set _base-cs_ is provided, the characters of _base-cs_ are included in the newly allocated mutable character set.

**(char-set-filter _pred cs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(char-set-filter _pred cs base-cs_)**  

Returns a new character set containing every character _c_ in character set _cs_ such that _(pred c)_ returns true. If character set _base-cs_ is provided, the characters specified by _base-cs_ are added to it.

**(->char-set _x_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Coerces object _x_ into a character set. _x_ may be a string, character or character set. A string is converted to the set of its constituent characters; a character is converted to a singleton character set; a character set is returned as is. This procedure is intended for use by other procedures that want to provide "user-friendly", wide-spectrum interfaces to their clients.


## Querying character sets

**(char-set-size _cs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of elements in character set _cs_.

**(char-set-count _pred cs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Apply _pred_ to the characters of character set _cs_, and return the number of characters that caused the predicate to return `#t`.

**(char-set->list _cs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

This procedure returns a list of the characters of character set _cs_. The order in which _cs_'s characters appear in the list is not defined, and may be different from one call to another.

**(char-set->string _cs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

This procedure returns a string containing the characters of character set _cs_. The order in which _cs_'s characters appear in the string is not defined, and may be different from one call to another.

**(char-set-hash _cs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(char-set-hash _cs bound_)**  

Compute a hash value for the character set _cs_. _bound_ is a non-negative exact integer specifying the range of the hash function. A positive value restricts the return value to the range _[0, bound)_. If _bound_ is either zero or not given, a default value is used, chosen to be as large as it is efficiently practical.

## Character set algebra

**(char-set-adjoin _cs char ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return a newly allocated mutable copy of _cs_ into which the characters _char ..._ were inserted.

**(char-set-delete _cs char ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return a newly allocated mutable copy of _cs_ from which the characters _char ..._ were removed.

**(char-set-complement _cs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return a newly allocated character set containing all characters that are not contained in _cs_.

**(char-set-union _cs ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(char-set-intersection _cs ..._)**  
**(char-set-difference _cs ..._)**  
**(char-set-xor _cs ..._)**  
**(char-set-diff+intersection _cs1 cs2 ..._)**  

These procedures implement set complement, union, intersection, difference, and exclusive-or for character sets. The union, intersection and xor operations are n-ary. The difference function is also n-ary, associates to the left (that is, it computes the difference between its first argument and the union of all the other arguments), and requires at least one argument.

Boundary cases:

```scheme
(char-set-union)          ⇒  char-set:empty
(char-set-intersection)   ⇒  char-set:full
(char-set-xor)            ⇒  char-set:empty
(char-set-difference cs)  ⇒  cs
```

`char-set-diff+intersection` returns both the difference and the intersection of the arguments, i.e. it partitions its first parameter. It is equivalent to `(values (char-set-difference cs1 cs2 ...) (char-set-intersection cs1 (char-set-union cs2 ...)))` but can be implemented more efficiently.


## Mutating character sets

**(char-set-adjoin! _cs char ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Insert the characters _char ..._ into the character set _cs_.

**(char-set-delete! _cs char ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Remove the characters _char ..._ from the character set _cs_.

**(char-set-complement! _cs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Complement the character set _cs_ by including all characters that were not contained in _cs_ previously and by removing all previously contained characters.

**(char-set-union! _cs1 cs2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(char-set-intersection! _cs1 cs2 ..._)**  
**(char-set-difference! _cs1 cs2 ..._)**  
**(char-set-xor! _cs1 cs2 ..._)**  
**(char-set-diff+intersection! _cs1 cs2 cs3 ..._)**  

These are update variants of the set-algebra functions mutating the first character set _cs1_ instead of creating a new one. `char-set-diff+intersection!` will perform a side-effect on both of its two required parameters _cs1_ and _cs2_.

**(char-set-filter! _pred cs base-cs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Adds every character _c_ in _cs_ for which _(pred c)_ returns `#t` to the given character set `base-cs`.

**(list->char-set! _char-list base-cs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Add the characters from the character list _char-list_ to character set _base-cs_ and return the mutated character set _base-cs_.

**(string->char-set! _s base-cs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Add the characters of the string _s_ to character set _base-cs_ and return the mutated character set _base-cs_.

**(ucs-range->char-set! _lower upper base-cs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(ucs-range->char-set! _lower upper limit base-cs_)**  

Mutates the mutable character set _base-cs_ including every character whose ISO/IEC 10646 UCS-4 code lies in the half-open range _[lower,upper)_. _lower_ and _upper_ are exact non-negative integers where _lower <= upper <= limit_ is required to hold. _limit_ is either an exact non-negative integer specifying the maximum upper limit, or it is `#t` which specifies the maximum UTF-16 code unit value. If _limit_ is not provided, a very large default is assumed (equivalent to _limit_ being `#f`).

**(char-set-unfold! _f p g seed base-cs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

This is a fundamental constructor for character sets.

  - _g_ is used to generate a series of "seed" values from the initial seed: seed, (g seed), (g2 seed), (g3 seed), ...
  - _p_ tells us when to stop by returning `#t` when applied to one of these seed values.
  - _f_ maps each seed value to a character. These characters are added to the base character set _base-cs_ to form the result. `char-set-unfold!` adds the characters by mutating _base-cs_ as a side effect.

## Iterating over character sets

**(char-set-cursor _cs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(char-set-ref _cs cursor_)**  
**(char-set-cursor-next _cs cursor_)**  
**(end-of-char-set? _cursor_)**  

Cursors are a low-level facility for iterating over the characters in a character set _cs_. A cursor is a value that indexes a character in a character set. `char-set-cursor` produces a new cursor for a given character set. The set element indexed by the cursor is fetched with `char-set-ref`. A cursor index is incremented with `char-set-cursor-next`; in this way, code can step through every character in a character set. Stepping a cursor "past the end" of a character set produces a cursor that answers true to `end-of-char-set?`. It is an error to pass such a cursor to `char-set-ref` or to `char-set-cursor-next`.

A cursor value may not be used in conjunction with a different character set; if it is passed to `char-set-ref` or `char-set-cursor-next` with a character set other than the one used to create it, the results and effects are undefined. These primitives are necessary to export an iteration facility for character sets to loop macros.

```scheme
(define cs (char-set #\G #\a #\T #\e #\c #\h))

;; Collect elts of CS into a list.
(let lp ((cur (char-set-cursor cs)) (ans '()))
  (if (end-of-char-set? cur) ans
      (lp (char-set-cursor-next cs cur)
          (cons (char-set-ref cs cur) ans))))
  ⇒  (#\G #\T #\a #\c #\e #\h)

;; Equivalently, using a list unfold (from SRFI 1):
(unfold-right end-of-char-set? 
              (curry char-set-ref cs)
	          (curry char-set-cursor-next cs)
	          (char-set-cursor cs))
  ⇒  (#\G #\T #\a #\c #\e #\h)
```

**(char-set-fold _kons knil cs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

This is the fundamental iterator for character sets. Applies the function _kons_ across the character set _cs_ using initial state value _knil_. That is, if _cs_ is the empty set, the procedure returns _knil_. Otherwise, some element _c_ of _cs_ is chosen; let _cs'_ be the remaining, unchosen characters. The procedure returns `(char-set-fold kons (kons c knil) cs')`.

```scheme
; CHAR-SET-MEMBERS
(lambda (cs) (char-set-fold cons '() cs))
; CHAR-SET-SIZE
(lambda (cs) (char-set-fold (lambda (c i) (+ i 1)) 0 cs))
; How many vowels in the char set?
(lambda (cs) 
  (char-set-fold (lambda (c i) (if (vowel? c) (+ i 1) i)) 0 cs))
```

**(char-set-unfold _f p g seed_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(char-set-unfold _f p g seed base-cs_)**  

This is a fundamental constructor for character sets.

  - _g_ is used to generate a series of "seed" values from the initial seed: seed, (g seed), (g2 seed), (g3 seed), ...
  - _p_ tells us when to stop, when it returns `#t` when applied to one of these seed values.
  - _f_ maps each seed value to a character. These characters are added to a mutable copy of the base character set _base-cs_ to form the result; _base-cs_ defaults to an empty set.

More precisely, the following definitions hold, ignoring the optional-argument issues:

```scheme
(define (char-set-unfold p f g seed base-cs) 
  (char-set-unfold! p f g seed (char-set-copy base-cs)))

(define (char-set-unfold! p f g seed base-cs)
  (let lp ((seed seed) (cs base-cs))
    (if (p seed) cs                            ; P says we are done
        (lp (g seed)                           ; Loop on (G SEED)
            (char-set-adjoin! cs (f seed)))))) ; Add (F SEED) to set
```

Examples:

```scheme
(port->char-set p) = (char-set-unfold eof-object?
                                      values
                                      (lambda (x) (read-char p))
                                      (read-char p))
(list->char-set lis) = (char-set-unfold null? car cdr lis)
```

**(char-set-for-each _proc cs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Apply procedure _proc_ to each character in the character set _cs_. Note that the order in which proc is applied to the characters in the set is not specified, and may even change from one procedure application to another.

**(char-set-map _proc cs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

_proc_ is a procedure mapping characters to characters. It will be applied to all the characters in the character set _cs_, and the results will be collected in a newly allocated mutable character set which will be returned by `char-set-map`.
